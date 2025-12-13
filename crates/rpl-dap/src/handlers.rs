//! DAP request handlers.
//!
//! Each handler processes a specific DAP request and sends the appropriate response.

use std::io::{Read, Write};
use std::path::PathBuf;

use dap::events::{OutputEventBody, StoppedEventBody};
use dap::prelude::*;
use dap::requests::{
    InitializeArguments, LaunchRequestArguments, ScopesArguments, SetBreakpointsArguments,
    StackTraceArguments, VariablesArguments,
};
use dap::responses::{
    ResponseBody, ScopesResponse, SetBreakpointsResponse, StackTraceResponse, ThreadsResponse,
    VariablesResponse,
};
use dap::types::{
    Breakpoint, Capabilities, OutputEventCategory, Scope, Source, StackFrame, StoppedEventReason,
    Thread, Variable,
};

use rpl_lang::{DebugEvent, ExecuteOutcome, Value};
use rpl_session::{LibraryRegistry, OperatorRegistry, Session};

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
    let source_id = rpl_session.set_source(
        path.file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("program.rpl"),
        &source_code,
    );

    let compiled = match rpl_session.compile(source_id) {
        Ok(program) => program,
        Err(errors) => {
            let msg = errors
                .iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<_>>()
                .join("\n");
            let resp = req.error(&format!("Compilation failed:\n{}", msg));
            return server.respond(resp);
        }
    };

    // Get the source file for line mapping
    let source_file = rpl_session.get_source(source_id).unwrap();

    // Create the debug session
    *session = Some(DebugSession::new(compiled, source_file.clone(), path));

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
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    req: Request,
) -> HandlerResult {
    let resp = req.success(ResponseBody::ConfigurationDone);
    server.respond(resp)?;

    // Start execution and stop at first breakpoint or start paused
    if let Some(session) = session.as_mut() {
        // Run until first pause point
        match session.run(registry, operators) {
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
pub fn handle_threads<R: Read, W: Write>(
    server: &mut Server<R, W>,
    req: Request,
) -> HandlerResult {
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
    use rpl_lang::ReturnEntry;

    // Build the call stack from the return stack
    // The return stack has Call entries for each function call, with the most recent at the end
    let return_stack = session.vm.return_stack_snapshot();

    // Collect call frames in reverse order (most recent call last in return stack)
    let call_frames: Vec<_> = return_stack
        .iter()
        .filter_map(|entry| {
            if let ReturnEntry::Call {
                return_pc,
                name,
                debug_info,
                ..
            } = entry
            {
                Some((*return_pc, name.clone(), debug_info.clone()))
            } else {
                None
            }
        })
        .collect();

    // Current frame (top of stack) - either in a called function or main
    if let Some((_, current_name, current_debug_info)) = call_frames.last() {
        // We're inside a called function
        let frame_name = current_name.clone().unwrap_or_else(|| "<anonymous>".to_string());

        // Try to get the line number from debug info
        let line = current_debug_info
            .as_ref()
            .and_then(|dbg| {
                let pc = session.vm.pc;
                dbg.spans.get(pc).map(|span| {
                    // Convert byte offset to line number using source
                    dbg.source.as_ref().map_or(0, |src| {
                        src[..span.start().offset() as usize]
                            .chars()
                            .filter(|&c| c == '\n')
                            .count() as i64
                            + 1
                    })
                })
            })
            .unwrap_or(0);

        // Use the debug info's source name and path if available
        let (source_name, source_path) = current_debug_info
            .as_ref()
            .and_then(|dbg| {
                dbg.source_name
                    .as_ref()
                    .map(|name| (name.clone(), name.clone()))
            })
            .unwrap_or_else(|| {
                (
                    session
                        .source_path
                        .file_name()
                        .and_then(|s| s.to_str())
                        .unwrap_or("program.rpl")
                        .to_string(),
                    session.source_path.to_string_lossy().to_string(),
                )
            });

        frames.push(StackFrame {
            id: 0,
            name: frame_name,
            source: Some(Source {
                name: Some(source_name),
                path: Some(source_path),
                ..Default::default()
            }),
            line,
            column: if line > 0 { 1 } else { 0 },
            ..Default::default()
        });
    } else {
        // We're in the main program
        let line = session.current_line().unwrap_or(1);
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
            line: line as i64,
            column: 1,
            ..Default::default()
        });
    }

    // Add caller frames (from most recent to oldest)
    // Skip the last entry since that's the current function (handled above)
    for (frame_idx, (return_pc, name, _debug_info)) in call_frames.iter().rev().skip(1).enumerate()
    {
        let line = session.line_for_pc(*return_pc).unwrap_or(1);
        let frame_name = name.clone().unwrap_or_else(|| format!("<caller {}>", frame_idx + 1));
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
            ..Default::default()
        });
    }

    // If we have call frames, add the main program as the bottom frame
    if !call_frames.is_empty() {
        let (return_pc, _, _) = &call_frames[0];
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
                .stack_snapshot()
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
            session
                .vm
                .locals_snapshot()
                .map(|locals| {
                    locals
                        .into_iter()
                        .map(|(name, v)| Variable {
                            name,
                            value: format_value(&v),
                            variables_reference: 0,
                            ..Default::default()
                        })
                        .collect()
                })
                .unwrap_or_default()
        }
        GLOBALS_REF => {
            // Global variables
            session
                .vm
                .globals_snapshot()
                .into_iter()
                .map(|(name, v)| Variable {
                    name,
                    value: format_value(&v),
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
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
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
    match session.continue_running(registry, operators) {
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
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::Next);
    server.respond(resp)?;

    match session.step_over(registry, operators) {
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
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::StepIn);
    server.respond(resp)?;

    match session.step_into(registry, operators) {
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
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    req: Request,
) -> HandlerResult {
    let Some(session) = session.as_mut() else {
        let resp = req.error("No active debug session");
        return server.respond(resp);
    };

    let resp = req.success(ResponseBody::StepOut);
    server.respond(resp)?;

    match session.step_out(registry, operators) {
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
                DebugEvent::Breakpoint(_) => StoppedEventReason::Breakpoint,
                DebugEvent::Step => StoppedEventReason::Step,
                DebugEvent::Paused => StoppedEventReason::Entry,
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
            let stack = session.vm.stack_snapshot();
            if !stack.is_empty() {
                let output = stack
                    .iter()
                    .map(format_value)
                    .collect::<Vec<_>>()
                    .join(" ");
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
        Value::Int(i) => format!("{}", i),
        Value::Bool(b) => format!("{}", b),
        Value::String(s) => format!("\"{}\"", s),
        Value::Symbol(sym) => format!("'{:?}", sym),
        Value::List(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("{{ {} }}", inner.join(" "))
        }
        Value::Program { code, debug_info } => {
            let debug_marker = if debug_info.is_some() { " [debug]" } else { "" };
            format!("<program size={}{}>", code.len(), debug_marker)
        }
        Value::Object { type_id, data } => {
            format!("<object type={:?} size={}>", type_id, data.len())
        }
    }
}
