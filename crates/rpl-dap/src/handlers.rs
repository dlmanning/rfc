//! DAP request handlers.
//!
//! Each handler processes a specific DAP request and sends the appropriate response.

use std::{
    io::{Read, Write},
    path::PathBuf,
};

use dap::{
    events::{OutputEventBody, StoppedEventBody},
    prelude::*,
    requests::{
        DisassembleArguments, InitializeArguments, LaunchRequestArguments, ScopesArguments,
        SetBreakpointsArguments, StackTraceArguments, VariablesArguments,
    },
    responses::{
        DisassembleResponse, ResponseBody, ScopesResponse, SetBreakpointsResponse,
        StackTraceResponse, ThreadsResponse, VariablesResponse,
    },
    types::{
        Breakpoint, Capabilities, DisassembledInstruction, OutputEventCategory, Scope, Source,
        StackFrame, StoppedEventReason, Thread, Variable,
    },
};
use rpl::{DebugEvent, ExecuteOutcome, Pos, ReturnEntry, Session, value::Value};

use super::session::DebugSession;

/// Result type for DAP handlers.
pub type HandlerResult = Result<(), dap::errors::ServerError>;

/// Thread ID for the single RPL thread.
const THREAD_ID: i64 = 1;

/// Variable reference IDs for scopes.
const LOCALS_REF: i64 = 1;
const GLOBALS_REF: i64 = 2;
const STACK_REF: i64 = 3;

/// Handle the initialize request.
pub fn handle_initialize<R: Read, W: Write>(
    server: &mut Server<R, W>,
    req: Request,
    _args: &InitializeArguments,
) -> HandlerResult {
    let capabilities = Capabilities {
        supports_configuration_done_request: Some(true),
        supports_set_variable: Some(false),
        supports_step_back: Some(false),
        supports_restart_request: Some(false),
        supports_conditional_breakpoints: Some(false),
        supports_hit_conditional_breakpoints: Some(false),
        supports_function_breakpoints: Some(false),
        supports_data_breakpoints: Some(false),
        supports_instruction_breakpoints: Some(false),
        supports_disassemble_request: Some(true),
        ..Default::default()
    };

    let resp = req.success(ResponseBody::Initialize(capabilities));
    server.respond(resp)?;

    // Send initialized event
    server.send_event(Event::Initialized)?;

    Ok(())
}

/// Handle the launch request.
pub fn handle_launch<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
    args: &LaunchRequestArguments,
) -> HandlerResult {
    // Get the program path from launch arguments
    let program_path = args
        .additional_data
        .as_ref()
        .and_then(|v| v.get("program"))
        .and_then(|v| v.as_str());

    let Some(program_path) = program_path else {
        let resp = req.error("Missing 'program' in launch configuration");
        return server.respond(resp);
    };

    let path = PathBuf::from(program_path);

    // Read the source file
    let source_code = match std::fs::read_to_string(&path) {
        Ok(code) => code,
        Err(e) => {
            let resp = req.error(&format!("Failed to read program: {}", e));
            return server.respond(resp);
        }
    };

    // Compile the program
    let mut rpl_session = Session::new();
    rpl_stdlib::register_interfaces(rpl_session.registry_mut());
    rpl_stdlib::register_impls(rpl_session.registry_mut());
    let source_id = rpl_session.set_source(
        path.file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("program.rpl"),
        &source_code,
    );

    let compiled = match rpl_session.compile(&source_code) {
        Ok(program) => program,
        Err(error) => {
            let resp = req.error(&format!("Compilation failed: {:?}", error));
            return server.respond(resp);
        }
    };

    // Get the source file for line mapping
    let source_file = rpl_session.sources().get(source_id).unwrap().clone();

    // Create the debug session
    *session = Some(DebugSession::new(compiled, source_file, path));

    let resp = req.success(ResponseBody::Launch);
    server.respond(resp)
}

/// Handle the setBreakpoints request.
pub fn handle_set_breakpoints<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
    args: &SetBreakpointsArguments,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    // Clear existing breakpoints for this source
    session.clear_breakpoints();

    let mut breakpoints = Vec::new();

    // Set new breakpoints
    if let Some(source_breakpoints) = &args.breakpoints {
        for bp in source_breakpoints {
            let (id, verified, actual_line) = session.add_breakpoint(bp.line as u32);

            breakpoints.push(Breakpoint {
                id: Some(id),
                verified,
                line: actual_line.map(|l| l as i64),
                source: Some(args.source.clone()),
                ..Default::default()
            });
        }
    }

    let resp = req.success(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
        breakpoints,
    }));
    server.respond(resp)
}

/// Handle the configurationDone request.
pub fn handle_configuration_done<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    let resp = req.success(ResponseBody::ConfigurationDone);
    server.respond(resp)?;

    // Start execution and stop at first breakpoint or start paused
    if let Some(session) = session.as_mut() {
        // Run until first pause point
        match session.run() {
            Ok(outcome) => {
                send_stop_event(server, session, &outcome)?;
            }
            Err(e) => {
                // Runtime error - send terminated event
                server.send_event(Event::Terminated(None))?;
                server.send_event(Event::Output(OutputEventBody {
                    output: format!("Runtime error: {}\n", e),
                    category: Some(OutputEventCategory::Stderr),
                    ..Default::default()
                }))?;
            }
        }
    }

    Ok(())
}

/// Handle the threads request.
pub fn handle_threads<R: Read, W: Write>(server: &mut Server<R, W>, req: Request) -> HandlerResult {
    // RPL is single-threaded
    let threads = vec![Thread {
        id: THREAD_ID,
        name: "main".to_string(),
    }];

    let resp = req.success(ResponseBody::Threads(ThreadsResponse { threads }));
    server.respond(resp)
}

/// Handle the stackTrace request.
pub fn handle_stack_trace<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &Option<DebugSession>,
    req: Request,
    _args: &StackTraceArguments,
) -> HandlerResult {
    let Some(session) = session.as_ref() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let mut frames = Vec::new();

    // Build the call stack from the return stack
    let return_stack = session.vm.return_stack();

    // Collect call frames in reverse order (most recent call last in return stack)
    let call_frames: Vec<_> = return_stack
        .iter()
        .map(|entry| {
            let ReturnEntry::Call {
                return_pc, name, ..
            } = entry;
            (*return_pc, name.clone())
        })
        .collect();

    // Current frame (top of stack) - either in a called function or main
    let current_line = session.current_line().unwrap_or(1) as i64;

    // Current instruction pointer reference for disassembly
    // Encode call depth in high bits to create unique address spaces for nested programs
    // Format: 0xDDPPPPPP where DD = call depth (0-255), PPPPPP = PC within that program
    let call_depth = session.call_depth();
    let current_pc = session.current_pc();
    let current_ipr = format!("0x{:02X}{:06X}", call_depth, current_pc);

    if let Some((_, current_name)) = call_frames.last() {
        // We're inside a called function
        let frame_name = current_name
            .clone()
            .unwrap_or_else(|| "<anonymous>".to_string());

        frames.push(StackFrame {
            id: 0,
            name: frame_name,
            source: Some(Source {
                name: Some(
                    session
                        .source_path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program.rpl")
                        .to_string(),
                ),
                path: Some(session.source_path.to_string_lossy().to_string()),
                ..Default::default()
            }),
            line: current_line,
            column: 1,
            instruction_pointer_reference: Some(current_ipr.clone()),
            ..Default::default()
        });
    } else {
        // We're in the main program
        frames.push(StackFrame {
            id: 0,
            name: "<main>".to_string(),
            source: Some(Source {
                name: Some(
                    session
                        .source_path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program.rpl")
                        .to_string(),
                ),
                path: Some(session.source_path.to_string_lossy().to_string()),
                ..Default::default()
            }),
            line: current_line,
            column: 1,
            instruction_pointer_reference: Some(current_ipr.clone()),
            ..Default::default()
        });
    }

    // Add caller frames (from most recent to oldest)
    // Each caller frame is one level shallower than the current
    for (frame_idx, (return_pc, name)) in call_frames.iter().rev().skip(1).enumerate() {
        // Caller's depth is (current_depth - 1 - frame_idx)
        let caller_depth = call_depth.saturating_sub(1 + frame_idx);
        let line = session.line_for_pc(*return_pc).unwrap_or(1);
        let frame_name = name
            .clone()
            .unwrap_or_else(|| format!("<caller {}>", frame_idx + 1));
        frames.push(StackFrame {
            id: (frame_idx + 1) as i64,
            name: frame_name,
            source: Some(Source {
                name: Some(
                    session
                        .source_path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program.rpl")
                        .to_string(),
                ),
                path: Some(session.source_path.to_string_lossy().to_string()),
                ..Default::default()
            }),
            line: line as i64,
            column: 1,
            instruction_pointer_reference: Some(format!("0x{:02X}{:06X}", caller_depth, return_pc)),
            ..Default::default()
        });
    }

    // If we have call frames, add the main program as the bottom frame
    if !call_frames.is_empty() {
        let (return_pc, _) = &call_frames[0];
        let line = session.line_for_pc(*return_pc).unwrap_or(1);
        frames.push(StackFrame {
            id: call_frames.len() as i64,
            name: "<main>".to_string(),
            source: Some(Source {
                name: Some(
                    session
                        .source_path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program.rpl")
                        .to_string(),
                ),
                path: Some(session.source_path.to_string_lossy().to_string()),
                ..Default::default()
            }),
            line: line as i64,
            column: 1,
            // Main program is always depth 0
            instruction_pointer_reference: Some(format!("0x{:02X}{:06X}", 0, return_pc)),
            ..Default::default()
        });
    }

    let resp = req.success(ResponseBody::StackTrace(StackTraceResponse {
        stack_frames: frames,
        total_frames: None,
    }));
    server.respond(resp)
}

/// Handle the scopes request.
pub fn handle_scopes<R: Read, W: Write>(
    server: &mut Server<R, W>,
    req: Request,
    _args: &ScopesArguments,
) -> HandlerResult {
    let scopes = vec![
        Scope {
            name: "Stack".to_string(),
            presentation_hint: Some(dap::types::ScopePresentationhint::Locals),
            variables_reference: STACK_REF,
            ..Default::default()
        },
        Scope {
            name: "Locals".to_string(),
            presentation_hint: Some(dap::types::ScopePresentationhint::Locals),
            variables_reference: LOCALS_REF,
            ..Default::default()
        },
        Scope {
            name: "Globals".to_string(),
            presentation_hint: Some(dap::types::ScopePresentationhint::Registers),
            variables_reference: GLOBALS_REF,
            ..Default::default()
        },
    ];

    let resp = req.success(ResponseBody::Scopes(ScopesResponse { scopes }));
    server.respond(resp)
}

/// Handle the variables request.
pub fn handle_variables<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &Option<DebugSession>,
    req: Request,
    args: &VariablesArguments,
) -> HandlerResult {
    let Some(session) = session.as_ref() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let variables = match args.variables_reference {
        STACK_REF => {
            // Data stack
            session
                .vm
                .stack_contents()
                .iter()
                .enumerate()
                .map(|(i, v)| Variable {
                    name: format!("[{}]", i),
                    value: format_value(v),
                    variables_reference: 0,
                    ..Default::default()
                })
                .collect()
        }
        LOCALS_REF => {
            // Local variables
            let locals = &session.vm.locals;
            (0..locals.len() as u32)
                .filter_map(|i| {
                    locals.try_get(i).map(|v| {
                        // Try to find a name for this local index
                        let name = session
                            .vm
                            .local_name_for_index(i)
                            .unwrap_or_else(|| format!("local_{}", i));
                        Variable {
                            name,
                            value: format_value(v),
                            variables_reference: 0,
                            ..Default::default()
                        }
                    })
                })
                .collect()
        }
        GLOBALS_REF => {
            // Global variables from directory
            session
                .vm
                .directory
                .current_node()
                .vars_iter()
                .map(|(name, value)| Variable {
                    name: name.clone(),
                    value: format_value(value),
                    variables_reference: 0,
                    ..Default::default()
                })
                .collect()
        }
        _ => Vec::new(),
    };

    let resp = req.success(ResponseBody::Variables(VariablesResponse { variables }));
    server.respond(resp)
}

/// Handle the continue request.
pub fn handle_continue<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::Continue(dap::responses::ContinueResponse {
        all_threads_continued: Some(true),
    }));
    server.respond(resp)?;

    // Continue execution
    match session.continue_running() {
        Ok(outcome) => send_stop_event(server, session, &outcome),
        Err(e) => {
            server.send_event(Event::Output(OutputEventBody {
                output: format!("Runtime error: {}\n", e),
                category: Some(OutputEventCategory::Stderr),
                ..Default::default()
            }))?;
            server.send_event(Event::Terminated(None))
        }
    }
}

/// Handle the next (step over) request.
pub fn handle_next<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::Next);
    server.respond(resp)?;

    match session.step_over() {
        Ok(outcome) => send_stop_event(server, session, &outcome),
        Err(e) => {
            server.send_event(Event::Output(OutputEventBody {
                output: format!("Runtime error: {}\n", e),
                category: Some(OutputEventCategory::Stderr),
                ..Default::default()
            }))?;
            server.send_event(Event::Terminated(None))
        }
    }
}

/// Handle the stepIn request.
pub fn handle_step_in<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::StepIn);
    server.respond(resp)?;

    match session.step_into() {
        Ok(outcome) => send_stop_event(server, session, &outcome),
        Err(e) => {
            server.send_event(Event::Output(OutputEventBody {
                output: format!("Runtime error: {}\n", e),
                category: Some(OutputEventCategory::Stderr),
                ..Default::default()
            }))?;
            server.send_event(Event::Terminated(None))
        }
    }
}

/// Handle the stepOut request.
pub fn handle_step_out<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::StepOut);
    server.respond(resp)?;

    match session.step_out() {
        Ok(outcome) => send_stop_event(server, session, &outcome),
        Err(e) => {
            server.send_event(Event::Output(OutputEventBody {
                output: format!("Runtime error: {}\n", e),
                category: Some(OutputEventCategory::Stderr),
                ..Default::default()
            }))?;
            server.send_event(Event::Terminated(None))
        }
    }
}

/// Handle the disassemble request.
pub fn handle_disassemble<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &Option<DebugSession>,
    req: Request,
    args: &DisassembleArguments,
) -> HandlerResult {
    let Some(session) = session.as_ref() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    // Parse memory_reference as hex address
    // Format: 0xDDPPPPPP where DD = call depth, PPPPPP = PC within that program
    let full_addr = parse_memory_reference(&args.memory_reference).unwrap_or(0);
    let requested_depth = (full_addr >> 24) as usize;
    let base_pc = (full_addr & 0x00FFFFFF) as usize;

    // Apply byte offset to get starting address
    let start_pc = (base_pc as i64 + args.offset.unwrap_or(0)).max(0) as usize;

    // Get the bytecode for the requested call depth
    let current_depth = session.call_depth();
    let code = if requested_depth == current_depth {
        // Requesting disassembly for the current execution context
        session.current_bytecode()
    } else if requested_depth == 0 {
        // Requesting main program disassembly
        &session.program.code
    } else {
        // Requesting a specific nested call level
        // Try to get it from the return stack
        let return_stack = session.vm.return_stack();
        if requested_depth <= return_stack.len() {
            let rpl::ReturnEntry::Call { program, .. } = &return_stack[requested_depth - 1];
            &program.code[..]
        } else {
            // Fall back to current bytecode
            session.current_bytecode()
        }
    };

    // Handle instruction_offset (offset in INSTRUCTIONS, not bytes)
    // We need to skip forward or backward by instruction_offset instructions
    let instruction_offset = args.instruction_offset.unwrap_or(0);
    let adjusted_pc = if instruction_offset >= 0 {
        // Skip forward by instruction_offset instructions
        let mut pc = start_pc;
        for _ in 0..instruction_offset {
            if let Some((_, next_pc)) = rpl::vm::disasm::disassemble_one(code, pc) {
                pc = next_pc;
            } else {
                pc += 1; // Can't decode, skip 1 byte
            }
        }
        pc
    } else {
        // For negative offsets, we need to scan from the beginning
        // to find the correct starting point
        let target_offset = (-instruction_offset) as usize;
        let mut pc = 0;
        let mut instructions = Vec::new();

        // Disassemble from start to find instruction boundaries
        while pc < start_pc && pc < code.len() {
            if let Some((_, next_pc)) = rpl::vm::disasm::disassemble_one(code, pc) {
                instructions.push(pc);
                pc = next_pc;
            } else {
                instructions.push(pc);
                pc += 1;
            }
        }

        // Go back by target_offset instructions
        if instructions.len() >= target_offset {
            instructions[instructions.len() - target_offset]
        } else {
            0
        }
    };

    // Disassemble requested number of instructions
    let instructions =
        rpl::vm::disasm::disassemble(code, adjusted_pc, args.instruction_count as usize);

    // Build source reference for line mapping
    let source = Source {
        name: Some(
            session
                .source_path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("program.rpl")
                .to_string(),
        ),
        path: Some(session.source_path.to_string_lossy().to_string()),
        ..Default::default()
    };

    // Convert to DAP format
    // Include call depth in addresses to create unique address spaces
    let dap_instructions: Vec<DisassembledInstruction> = instructions
        .iter()
        .map(|instr| {
            // Get line number - for nested programs, use source map from that program
            let line = if requested_depth == 0 {
                session.line_for_pc(instr.pc).map(|l| l as i64)
            } else if requested_depth <= session.vm.return_stack().len() {
                // Get source offset from nested program's source map
                let return_stack = session.vm.return_stack();
                let rpl::ReturnEntry::Call { program, .. } = &return_stack[requested_depth - 1];
                program
                    .source_offset_for_pc(instr.pc)
                    .map(|offset| session.source.line_col(Pos::new(offset)).line as i64)
            } else {
                None
            };

            DisassembledInstruction {
                address: format!("0x{:02X}{:06X}", requested_depth, instr.pc),
                instruction: instr.text.clone(),
                instruction_bytes: Some(instr.bytes.clone()),
                line,
                location: Some(source.clone()),
                ..Default::default()
            }
        })
        .collect();

    let resp = req.success(ResponseBody::Disassemble(DisassembleResponse {
        instructions: dap_instructions,
    }));
    server.respond(resp)
}

/// Parse a memory reference string (hex or decimal) to a PC value.
fn parse_memory_reference(s: &str) -> Option<usize> {
    let s = s.trim();
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        usize::from_str_radix(hex, 16).ok()
    } else {
        s.parse().ok()
    }
}

/// Handle the disconnect request.
pub fn handle_disconnect<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &mut Option<DebugSession>,
    req: Request,
) -> HandlerResult {
    *session = None;
    let resp = req.success(ResponseBody::Disconnect);
    server.respond(resp)
}

/// Send a stopped or terminated event based on execution outcome.
fn send_stop_event<R: Read, W: Write>(
    server: &mut Server<R, W>,
    session: &DebugSession,
    outcome: &ExecuteOutcome,
) -> HandlerResult {
    match outcome {
        ExecuteOutcome::Debug(event) => {
            let reason = match event {
                DebugEvent::Breakpoint { .. } => StoppedEventReason::Breakpoint,
                DebugEvent::Step { .. } => StoppedEventReason::Step,
                DebugEvent::Entry => StoppedEventReason::Entry,
            };

            server.send_event(Event::Stopped(StoppedEventBody {
                reason,
                description: None,
                thread_id: Some(THREAD_ID),
                preserve_focus_hint: None,
                text: None,
                all_threads_stopped: Some(true),
                hit_breakpoint_ids: None,
            }))
        }
        ExecuteOutcome::Completed => {
            // Print final stack to output
            let stack = session.vm.stack_contents();
            if !stack.is_empty() {
                let output = stack.iter().map(format_value).collect::<Vec<_>>().join(" ");
                server.send_event(Event::Output(OutputEventBody {
                    output: format!("Result: {}\n", output),
                    category: Some(OutputEventCategory::Stdout),
                    ..Default::default()
                }))?;
            }
            server.send_event(Event::Terminated(None))
        }
    }
}

/// Format a value for display.
fn format_value(value: &Value) -> String {
    match value {
        Value::Real(r) => format!("{}", r),
        Value::Integer(i) => format!("{}", i),
        Value::String(s) => format!("\"{}\"", s),
        Value::Symbolic(sym) => format!("'{}", sym),
        Value::List(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("{{ {} }}", inner.join(" "))
        }
        Value::Program(prog) => {
            format!("<program size={}>", prog.code.len())
        }
        Value::Library(lib) => {
            format!("<library {} commands={}>", lib.id, lib.commands.len())
        }
        Value::Bytes(bytes) => {
            format!("<bytes len={}>", bytes.len())
        }
    }
}
