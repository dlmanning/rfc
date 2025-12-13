//! Library for flow control constructs.
//!
//! This library provides:
//! - `IF` / `THEN` / `ELSE` / `END` - Conditional execution
//! - `CASE` / `THENCASE` / `ENDTHEN` / `ENDCASE` - Multiple-choice structure
//! - `IFERR` / `THENERR` / `ELSEERR` / `ENDERR` - Error handling
//! - `DO` / `UNTIL` / `END` - Indefinite loop (test at end)
//! - `WHILE` / `REPEAT` / `END` - Indefinite loop (test at start)
//! - `START` / `NEXT` - Definite loop (count-controlled)

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, ConstructKind, DecompileContext, DecompileResult,
    ExecuteContext, ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

/// Library for flow control.
pub struct FlowControlLib;

/// Check if a value is falsy (0, 0.0, or false).
fn is_falsy(value: &Value) -> bool {
    match value {
        Value::Real(r) => *r == 0.0,
        Value::Int(i) => *i == 0,
        Value::Bool(b) => !b,
        _ => false, // Non-numeric values are truthy
    }
}

impl FlowControlLib {
    /// Library ID for flow control.
    pub const ID: LibraryId = LibraryId::new(9);

    // Command IDs
    /// Jump if top of stack is false/zero.
    const CMD_JUMP_IF_FALSE: u16 = 0;
    /// Unconditional jump.
    const CMD_JUMP: u16 = 1;
    /// Jump if top of stack is true/nonzero.
    const CMD_JUMP_IF_TRUE: u16 = 2;
    /// Decrement loop counter and jump if not zero (for START/NEXT).
    const CMD_LOOP_NEXT: u16 = 3;
    /// Setup loop counter from stack (for START/NEXT).
    const CMD_LOOP_SETUP: u16 = 4;
    /// Marker for IF (no-op, for decompilation).
    const CMD_IF_MARKER: u16 = 5;
    /// Marker for END (no-op, for decompilation).
    const CMD_END_MARKER: u16 = 6;
    /// Marker for DO (no-op, for decompilation).
    const CMD_DO_MARKER: u16 = 7;
    /// Marker for WHILE (no-op, for decompilation).
    const CMD_WHILE_MARKER: u16 = 8;
    /// Marker for REPEAT (used after JUMP_IF_FALSE in WHILE/REPEAT).
    const CMD_REPEAT_MARKER: u16 = 9;
    /// Jump back if false (for UNTIL - loops while false).
    const CMD_UNTIL_JUMP: u16 = 10;
    /// Setup loop with index variable (for FOR/NEXT).
    const CMD_FOR_SETUP: u16 = 11;
    /// Increment loop counter and jump if not done (for FOR/NEXT).
    const CMD_FOR_NEXT: u16 = 12;
    /// Pop loop index from return stack (cleanup after FOR/NEXT).
    const CMD_FOR_END: u16 = 13;
    /// STEP for START/STEP loop - custom increment from stack.
    const CMD_LOOP_STEP: u16 = 14;
    /// STEP for FOR/STEP loop - custom increment from stack.
    const CMD_FOR_STEP: u16 = 15;
    /// Setup ascending FOR loop with skip check (for FORUP).
    const CMD_FORUP_SETUP: u16 = 16;
    /// Setup descending FOR loop with skip check (for FORDN).
    const CMD_FORDN_SETUP: u16 = 17;
    /// Marker for CASE (no-op, for decompilation).
    const CMD_CASE_MARKER: u16 = 18;
    /// Marker for ENDCASE (no-op, for decompilation).
    const CMD_ENDCASE_MARKER: u16 = 19;
    /// Marker for ENDTHEN (no-op, for decompilation).
    const CMD_ENDTHEN_MARKER: u16 = 20;
    /// Setup error handler (for IFERR).
    const CMD_IFERR_SETUP: u16 = 21;
    /// Marker for THENERR (no-op, for decompilation).
    const CMD_THENERR_MARKER: u16 = 22;
    /// Marker for ELSEERR (no-op, for decompilation).
    const CMD_ELSEERR_MARKER: u16 = 23;
    /// Marker for ENDERR (no-op, for decompilation).
    const CMD_ENDERR_MARKER: u16 = 24;
    /// Pop error handler (for successful IFERR completion).
    const CMD_IFERR_SUCCESS: u16 = 25;
}

impl Library for FlowControlLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "FlowControl"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        match text.to_ascii_uppercase().as_str() {
            // Conditionals
            "IF" | "THEN" | "ELSE" | "END" => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // CASE structure
            "CASE" | "THENCASE" | "ENDTHEN" | "ENDCASE" => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Error handling
            "IFERR" | "THENERR" | "ELSEERR" | "ENDERR" => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Indefinite loops
            "DO" | "UNTIL" | "WHILE" | "REPEAT" => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Definite loops
            "START" | "NEXT" | "STEP" | "FOR" | "FORUP" | "FORDN" => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            _ => ProbeResult::NoMatch,
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        match text.to_ascii_uppercase().as_str() {
            // === Conditionals ===
            "IF" => {
                // Emit IF marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_IF_MARKER);
                CompileResult::StartConstruct {
                    kind: ConstructKind::If,
                }
            }
            "THEN" => {
                // THEN consumes the test result and emits conditional jump
                // If false, jump to ELSE or END
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0); // Placeholder for jump target
                CompileResult::NeedMore
            }
            "ELSE" => {
                // ELSE: emit unconditional jump to skip false-clause
                // Compiler will patch THEN's jump to point here
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0); // Placeholder for jump target
                CompileResult::NeedMore
            }

            // === DO/UNTIL/END loop ===
            // Syntax: DO <loop-body> <test> UNTIL END
            // Semantics: Execute body, test condition, repeat if false
            "DO" => {
                // Emit DO marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_DO_MARKER);
                CompileResult::StartConstruct {
                    kind: ConstructKind::DoUntil,
                }
            }
            "UNTIL" => {
                // UNTIL: test is on stack, jump back to DO if false (continue looping)
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_UNTIL_JUMP);
                ctx.emit(0); // Placeholder - compiler will fill with loop_start
                CompileResult::NeedMore
            }

            // === WHILE/REPEAT/END loop ===
            // Syntax: WHILE <test> REPEAT <loop-body> END
            // Semantics: Test first, if true execute body, repeat
            "WHILE" => {
                // Emit WHILE marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_WHILE_MARKER);
                CompileResult::StartConstruct {
                    kind: ConstructKind::While,
                }
            }
            "REPEAT" => {
                // REPEAT: test is on stack, jump to END if false (exit loop)
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0); // Placeholder for exit jump target
                // Emit REPEAT marker after the jump for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_REPEAT_MARKER);
                CompileResult::NeedMore
            }

            // === START/NEXT loop ===
            // Syntax: start finish START <loop-body> NEXT
            // Semantics: Execute body (finish - start + 1) times
            "START" => {
                // START: compute iteration count from stack, start loop
                // The compiler will emit loop setup and record position
                CompileResult::StartConstruct {
                    kind: ConstructKind::Start,
                }
            }
            "NEXT" => {
                // NEXT: decrement counter, jump back if not done
                // Also closes the START or FOR/FORUP/FORDN construct
                // Use different command depending on whether this is START/NEXT or FOR/NEXT
                let cmd = match ctx.current_construct() {
                    Some(ConstructKind::For)
                    | Some(ConstructKind::ForUp)
                    | Some(ConstructKind::ForDn) => Self::CMD_FOR_NEXT,
                    _ => Self::CMD_LOOP_NEXT,
                };
                ctx.emit_opcode(Self::ID.as_u16(), cmd);
                ctx.emit(0); // Placeholder for loop start
                CompileResult::EndConstruct
            }
            "STEP" => {
                // STEP: like NEXT but with custom increment from stack
                // Use different command depending on whether this is START/STEP or FOR/STEP
                let cmd = match ctx.current_construct() {
                    Some(ConstructKind::For)
                    | Some(ConstructKind::ForUp)
                    | Some(ConstructKind::ForDn) => Self::CMD_FOR_STEP,
                    _ => Self::CMD_LOOP_STEP,
                };
                ctx.emit_opcode(Self::ID.as_u16(), cmd);
                ctx.emit(0); // Placeholder for loop start
                CompileResult::EndConstruct
            }

            // === FOR/NEXT loop ===
            // Syntax: start finish FOR variable <loop-body> NEXT
            // Semantics: Execute body with variable taking values from start to finish
            "FOR" => {
                // FOR: like START but binds a local variable to the loop index
                // The compiler will read the variable name as the next token
                // and create a local binding
                CompileResult::StartConstruct {
                    kind: ConstructKind::For,
                }
            }

            // === FORUP/FORDN loops ===
            // Like FOR but with forced direction and zero-iteration skip check
            "FORUP" => {
                // FORUP: ascending FOR with skip if start > end
                CompileResult::StartConstruct {
                    kind: ConstructKind::ForUp,
                }
            }
            "FORDN" => {
                // FORDN: descending FOR with skip if start < end
                CompileResult::StartConstruct {
                    kind: ConstructKind::ForDn,
                }
            }

            // === CASE/THENCASE/ENDTHEN/ENDCASE ===
            // Syntax: CASE test1 THENCASE action1 ENDTHEN test2 THENCASE action2 ENDTHEN default ENDCASE
            // Semantics: Evaluate tests in order; when one is true, execute its action and exit.
            // If no test is true, execute default action before ENDCASE.
            "CASE" => {
                // Emit CASE marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_CASE_MARKER);
                CompileResult::StartConstruct {
                    kind: ConstructKind::Case,
                }
            }
            "THENCASE" => {
                // THENCASE: pop condition, if false jump to next ENDTHEN
                // Emit JUMP_IF_FALSE with placeholder
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0); // Placeholder for jump to next case
                CompileResult::NeedMore
            }
            "ENDTHEN" => {
                // ENDTHEN: emit jump to ENDCASE, then patch THENCASE's conditional jump
                // First emit unconditional jump to ENDCASE (placeholder)
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0); // Placeholder - will be patched by ENDCASE
                // Emit ENDTHEN marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDTHEN_MARKER);
                CompileResult::NeedMore
            }
            "ENDCASE" => {
                // Emit ENDCASE marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDCASE_MARKER);
                // ENDCASE closes the CASE construct
                // Patching of exit jumps is handled by the compiler
                CompileResult::EndConstruct
            }

            // === IFERR/THENERR/ELSEERR/ENDERR ===
            // Syntax: IFERR protected-code THENERR error-handler ENDERR
            // Or:     IFERR protected-code THENERR error-handler ELSEERR no-error-code ENDERR
            // Semantics: Execute protected-code; if error, run error-handler; else run no-error-code
            "IFERR" => {
                // IFERR_SETUP + placeholder will be emitted by the compiler
                // so that start_pos correctly tracks the placeholder position
                CompileResult::StartConstruct {
                    kind: ConstructKind::ErrorHandler,
                }
            }
            "THENERR" => {
                // THENERR: End of protected code
                // Emit IFERR_SUCCESS to pop handler if no error
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_IFERR_SUCCESS);
                // Emit jump to skip error handler (to ELSEERR or ENDERR)
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0); // Placeholder for jump past handler
                // Emit THENERR marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_THENERR_MARKER);
                CompileResult::NeedMore
            }
            "ELSEERR" => {
                // ELSEERR: End of error handler, start of no-error code
                // Emit jump to ENDERR (skip no-error code if we came from handler)
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0); // Placeholder for jump to ENDERR
                // Emit ELSEERR marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ELSEERR_MARKER);
                CompileResult::NeedMore
            }
            "ENDERR" => {
                // Emit ENDERR marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDERR_MARKER);
                // ENDERR closes the ErrorHandler construct
                CompileResult::EndConstruct
            }

            // === END (closes multiple construct types) ===
            "END" => {
                // Emit END marker for decompilation
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_END_MARKER);
                // END closes IF, DO/UNTIL, or WHILE/REPEAT constructs
                // Patching is handled by the compiler's construct tracking
                CompileResult::EndConstruct
            }
            _ => CompileResult::NoMatch,
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_JUMP_IF_FALSE => {
                // Read the jump target from the next word
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop condition and check if falsy
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if is_falsy(&condition) {
                    ExecuteResult::Jump(target)
                } else {
                    ExecuteResult::Ok
                }
            }
            Self::CMD_JUMP => {
                // Read the jump target from the next word
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                ExecuteResult::Jump(target)
            }
            Self::CMD_JUMP_IF_TRUE => {
                // Read the jump target from the next word
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop condition and check if truthy
                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if !is_falsy(&condition) {
                    ExecuteResult::Jump(target)
                } else {
                    ExecuteResult::Ok
                }
            }
            Self::CMD_LOOP_NEXT => {
                // START/NEXT: increment counter by 1, check termination
                // NEXT ignores direction - always increments by 1 and uses <= comparison
                // This matches HP behavior where NEXT only works for ascending loops
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop loop state from return stack
                let (start, end, direction, counter) = match ctx.pop_start_loop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Return stack underflow".to_string()),
                };

                // Increment counter by 1
                let new_counter = counter + 1;

                // Check if we should continue (always uses <= for NEXT)
                if new_counter <= end {
                    // More iterations: push state back and jump
                    if ctx
                        .push_start_loop(start, end, direction, new_counter)
                        .is_err()
                    {
                        return ExecuteResult::Error("Return stack overflow".to_string());
                    }
                    ExecuteResult::Jump(target)
                } else {
                    // Done - exit loop
                    ExecuteResult::Ok
                }
            }
            Self::CMD_LOOP_SETUP => {
                // START loop setup - matches HP behavior
                // Pop start and finish from data stack
                // Compute direction from comparing start and finish
                // Push 4 values to return stack: start, end, direction, counter
                // Body always runs at least once (no skip check)
                //
                // Return stack layout (top to bottom):
                //   counter (= start initially)
                //   direction (-1 ascending, 0 equal, +1 descending)
                //   end
                //   start

                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };

                // Compute direction: -1 if ascending, 0 if equal, +1 if descending
                let direction = if start < finish {
                    -1i64
                } else if start > finish {
                    1i64
                } else {
                    0i64
                };

                // Push loop state onto return stack
                if ctx
                    .push_start_loop(start, finish, direction, start)
                    .is_err()
                {
                    return ExecuteResult::Error("Return stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_UNTIL_JUMP => {
                // Like JUMP_IF_FALSE but for UNTIL (loops while condition is false)
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                let condition = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if is_falsy(&condition) {
                    ExecuteResult::Jump(target)
                } else {
                    ExecuteResult::Ok
                }
            }
            // Markers are no-ops at runtime
            Self::CMD_IF_MARKER
            | Self::CMD_END_MARKER
            | Self::CMD_DO_MARKER
            | Self::CMD_WHILE_MARKER
            | Self::CMD_REPEAT_MARKER
            | Self::CMD_CASE_MARKER
            | Self::CMD_ENDCASE_MARKER
            | Self::CMD_ENDTHEN_MARKER
            | Self::CMD_THENERR_MARKER
            | Self::CMD_ELSEERR_MARKER
            | Self::CMD_ENDERR_MARKER => ExecuteResult::Ok,
            Self::CMD_IFERR_SETUP => {
                // Read handler PC from operand
                let handler_pc = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                // Push error handler onto return stack
                let code = ctx.current_code();
                if ctx.push_error_handler(handler_pc, code).is_err() {
                    return ExecuteResult::Error("Return stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_IFERR_SUCCESS => {
                // Protected code completed without error - pop the handler
                if ctx.pop_error_handler().is_err() {
                    return ExecuteResult::Error("No error handler to pop".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_FOR_SETUP => {
                // FOR loop setup - matches HP behavior
                // Read symbol ID operand (the loop variable)
                // Pop start and finish from data stack
                // Compute direction from comparing start and finish
                // Push 4 values to return stack: start, end, direction, counter
                // Body always runs at least once (no skip check)
                //
                // Return stack layout (top to bottom):
                //   counter (= start initially)
                //   direction (-1 ascending, 0 equal, +1 descending)
                //   end
                //   start
                // Current value is also in the loop variable (local frame)

                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop finish and start from data stack
                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };

                // Compute direction: -1 if ascending, 0 if equal, +1 if descending
                let direction = if start < finish {
                    -1i64
                } else if start > finish {
                    1i64
                } else {
                    0i64
                };

                // Create local frame with the loop variable set to start value
                let symbol = rpl_core::Symbol::from_raw(sym_id);
                if ctx.create_local_frame_for(symbol, start).is_err() {
                    return ExecuteResult::Error("Failed to create local frame".to_string());
                }

                // Push loop state onto return stack
                if ctx.push_for_loop(start, finish, direction, start).is_err() {
                    return ExecuteResult::Error("Return stack overflow".to_string());
                }

                ExecuteResult::Ok
            }
            Self::CMD_FOR_NEXT => {
                // FOR/NEXT: increment loop variable by 1, check termination
                // NEXT ignores direction - always increments by 1 and uses <= comparison
                // This matches HP behavior where NEXT only works for ascending loops
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop loop state from return stack
                let (start, end, direction, counter) = match ctx.pop_for_loop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Return stack underflow".to_string()),
                };

                // Increment counter by 1
                let new_counter = counter + 1;

                // Check if we should continue (always uses <= for NEXT)
                if new_counter <= end {
                    // More iterations: update variable, push state back, jump
                    ctx.set_for_variable(new_counter);
                    if ctx
                        .push_for_loop(start, end, direction, new_counter)
                        .is_err()
                    {
                        return ExecuteResult::Error("Return stack overflow".to_string());
                    }
                    ExecuteResult::Jump(target)
                } else {
                    // Done - pop local frame and exit
                    ctx.pop_local_frame();
                    ExecuteResult::Ok
                }
            }
            Self::CMD_FOR_END => {
                // Pop the local frame created by FOR
                ctx.pop_local_frame();
                ExecuteResult::Ok
            }
            Self::CMD_LOOP_STEP => {
                // START/STEP: add increment to counter, check termination based on direction
                // Direction was computed at LOOP_SETUP time from comparing start and end
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop increment from data stack
                let increment = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => {
                            return ExecuteResult::Error(
                                "Stack underflow: STEP requires increment".to_string(),
                            );
                        }
                    },
                };

                // Pop loop state from return stack
                let (start, end, direction, counter) = match ctx.pop_start_loop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Return stack underflow".to_string()),
                };

                // Add increment to counter
                let new_counter = counter + increment;

                // Check continuation based on direction (computed at setup from start vs end)
                // direction < 0: ascending (start < end), continue if counter <= end
                // direction > 0: descending (start > end), continue if counter >= end
                // direction == 0: equal (start == end), continue if counter == end
                let should_continue = if direction < 0 {
                    new_counter <= end
                } else if direction > 0 {
                    new_counter >= end
                } else {
                    new_counter == end
                };

                if should_continue {
                    // More iterations: push state back and jump
                    if ctx
                        .push_start_loop(start, end, direction, new_counter)
                        .is_err()
                    {
                        return ExecuteResult::Error("Return stack overflow".to_string());
                    }
                    ExecuteResult::Jump(target)
                } else {
                    ExecuteResult::Ok
                }
            }
            Self::CMD_FOR_STEP => {
                // FOR/STEP: add increment to counter, check termination based on direction
                // Direction was computed at FOR_SETUP time from comparing start and end
                // Current value is also in the loop variable (local frame)
                let target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop increment from data stack
                let increment = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => {
                            return ExecuteResult::Error(
                                "Stack underflow: STEP requires increment".to_string(),
                            );
                        }
                    },
                };

                // Pop loop state from return stack
                let (start, end, direction, counter) = match ctx.pop_for_loop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Return stack underflow".to_string()),
                };

                // Add increment to counter
                let new_counter = counter + increment;

                // Check continuation based on direction (computed at setup from start vs end)
                // direction < 0: ascending (start < end), continue if counter <= end
                // direction > 0: descending (start > end), continue if counter >= end
                // direction == 0: equal (start == end), continue if counter == end
                let should_continue = if direction < 0 {
                    new_counter <= end
                } else if direction > 0 {
                    new_counter >= end
                } else {
                    new_counter == end
                };

                if should_continue {
                    // More iterations: update variable, push state back, jump
                    ctx.set_for_variable(new_counter);
                    if ctx
                        .push_for_loop(start, end, direction, new_counter)
                        .is_err()
                    {
                        return ExecuteResult::Error("Return stack overflow".to_string());
                    }
                    ExecuteResult::Jump(target)
                } else {
                    // Done - pop local frame and exit
                    ctx.pop_local_frame();
                    ExecuteResult::Ok
                }
            }
            Self::CMD_FORUP_SETUP => {
                // FORUP: ascending FOR with skip check if start > end
                // Read symbol ID and skip target operands
                // Pop start and finish from data stack
                // If start > end, skip to skip_target (zero iterations)
                // Force direction = -1 (ascending)
                // Push 4 values to return stack: start, end, direction, counter
                //
                // Return stack layout (top to bottom):
                //   counter (= start initially)
                //   direction (-1 for ascending)
                //   end
                //   start

                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                let skip_target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop finish and start from data stack
                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };

                // Skip if start > finish (zero iterations for ascending loop)
                if start > finish {
                    return ExecuteResult::Jump(skip_target);
                }

                // Force direction = -1 (ascending)
                let direction = -1i64;

                // Create local frame with the loop variable set to start value
                let symbol = rpl_core::Symbol::from_raw(sym_id);
                if ctx.create_local_frame_for(symbol, start).is_err() {
                    return ExecuteResult::Error("Failed to create local frame".to_string());
                }

                // Push loop state onto return stack
                if ctx.push_for_loop(start, finish, direction, start).is_err() {
                    return ExecuteResult::Error("Return stack overflow".to_string());
                }

                ExecuteResult::Ok
            }
            Self::CMD_FORDN_SETUP => {
                // FORDN: descending FOR with skip check if start < end
                // Read symbol ID and skip target operands
                // Pop start and finish from data stack
                // If start < end, skip to skip_target (zero iterations)
                // Force direction = +1 (descending)
                // Push 4 values to return stack: start, end, direction, counter
                //
                // Return stack layout (top to bottom):
                //   counter (= start initially)
                //   direction (+1 for descending)
                //   end
                //   start

                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                let skip_target = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };

                // Pop finish and start from data stack
                let finish = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };
                let start = match ctx.pop_int() {
                    Ok(v) => v,
                    Err(_) => match ctx.pop_real() {
                        Ok(v) => v as i64,
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    },
                };

                // Skip if start < finish (zero iterations for descending loop)
                if start < finish {
                    return ExecuteResult::Jump(skip_target);
                }

                // Force direction = +1 (descending)
                let direction = 1i64;

                // Create local frame with the loop variable set to start value
                let symbol = rpl_core::Symbol::from_raw(sym_id);
                if ctx.create_local_frame_for(symbol, start).is_err() {
                    return ExecuteResult::Error("Failed to create local frame".to_string());
                }

                // Push loop state onto return stack
                if ctx.push_for_loop(start, finish, direction, start).is_err() {
                    return ExecuteResult::Error("Return stack overflow".to_string());
                }

                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown flow control command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => DecompileResult::Unknown,
            DecompileMode::Call(cmd) => match cmd {
                Self::CMD_IF_MARKER => {
                    ctx.write("IF");
                    DecompileResult::Ok
                }
                Self::CMD_JUMP_IF_FALSE => {
                    let _target = ctx.read(); // consume target
                    // Check if this is followed by REPEAT_MARKER (WHILE/REPEAT pattern)
                    // If so, don't output "THEN" - let REPEAT_MARKER handle it
                    if let Some(next_word) = ctx.peek()
                        && !rpl_core::is_prolog(next_word)
                    {
                        let next_lib = rpl_core::extract_lib(next_word);
                        let next_cmd = rpl_core::extract_cmd(next_word);
                        if next_lib == Self::ID.as_u16() && next_cmd == Self::CMD_REPEAT_MARKER {
                            // Don't output THEN, let REPEAT_MARKER output REPEAT
                            return DecompileResult::Ok;
                        }
                    }
                    // Could be THEN (in IF context) or THENCASE (in CASE context)
                    // We can't easily distinguish, so use THEN as default
                    // A smarter decompiler would track context
                    ctx.write("THEN");
                    DecompileResult::Ok
                }
                Self::CMD_JUMP => {
                    let target = ctx.read().unwrap_or(0); // consume target
                    // Get current position to determine if this is a backward jump
                    let current_pos = ctx.position();
                    // If target < current position, this is a backward jump (loop)
                    // Don't output anything for backward jumps (end of WHILE loop)
                    if (target as usize) < current_pos {
                        return DecompileResult::Ok;
                    }
                    // Forward jump is ELSE (in IF/THEN/ELSE)
                    ctx.write("ELSE");
                    DecompileResult::Ok
                }
                Self::CMD_END_MARKER => {
                    ctx.write("END");
                    DecompileResult::Ok
                }
                Self::CMD_DO_MARKER => {
                    ctx.write("DO");
                    DecompileResult::Ok
                }
                Self::CMD_WHILE_MARKER => {
                    ctx.write("WHILE");
                    DecompileResult::Ok
                }
                Self::CMD_REPEAT_MARKER => {
                    ctx.write("REPEAT");
                    DecompileResult::Ok
                }
                Self::CMD_JUMP_IF_TRUE => {
                    let _target = ctx.read(); // consume target
                    ctx.write("UNTIL");
                    DecompileResult::Ok
                }
                Self::CMD_UNTIL_JUMP => {
                    let _target = ctx.read(); // consume target
                    ctx.write("UNTIL");
                    DecompileResult::Ok
                }
                Self::CMD_LOOP_NEXT => {
                    let _target = ctx.read(); // consume target
                    ctx.write("NEXT");
                    DecompileResult::Ok
                }
                Self::CMD_LOOP_SETUP => {
                    // LOOP_SETUP has no operands - just sets up loop state
                    ctx.write("START");
                    DecompileResult::Ok
                }
                Self::CMD_FOR_SETUP => {
                    let _symbol = ctx.read(); // consume symbol operand
                    ctx.write("FOR");
                    DecompileResult::Ok
                }
                Self::CMD_FOR_NEXT => {
                    let _target = ctx.read(); // consume target
                    ctx.write("NEXT");
                    DecompileResult::Ok
                }
                Self::CMD_LOOP_STEP => {
                    let _target = ctx.read(); // consume target
                    ctx.write("STEP");
                    DecompileResult::Ok
                }
                Self::CMD_FOR_STEP => {
                    let _target = ctx.read(); // consume target
                    ctx.write("STEP");
                    DecompileResult::Ok
                }
                Self::CMD_FORUP_SETUP => {
                    let _symbol = ctx.read(); // consume symbol operand
                    let _skip_target = ctx.read(); // consume skip_target operand
                    ctx.write("FORUP");
                    DecompileResult::Ok
                }
                Self::CMD_FORDN_SETUP => {
                    let _symbol = ctx.read(); // consume symbol operand
                    let _skip_target = ctx.read(); // consume skip_target operand
                    ctx.write("FORDN");
                    DecompileResult::Ok
                }
                Self::CMD_CASE_MARKER => {
                    ctx.write("CASE");
                    DecompileResult::Ok
                }
                Self::CMD_ENDCASE_MARKER => {
                    ctx.write("ENDCASE");
                    DecompileResult::Ok
                }
                Self::CMD_ENDTHEN_MARKER => {
                    ctx.write("ENDTHEN");
                    DecompileResult::Ok
                }
                Self::CMD_IFERR_SETUP => {
                    let _handler_pc = ctx.read(); // consume operand
                    ctx.write("IFERR");
                    DecompileResult::Ok
                }
                Self::CMD_IFERR_SUCCESS => {
                    // No-op at decompile time - it's implicit before THENERR
                    DecompileResult::Ok
                }
                Self::CMD_THENERR_MARKER => {
                    ctx.write("THENERR");
                    DecompileResult::Ok
                }
                Self::CMD_ELSEERR_MARKER => {
                    ctx.write("ELSEERR");
                    DecompileResult::Ok
                }
                Self::CMD_ENDERR_MARKER => {
                    ctx.write("ENDERR");
                    DecompileResult::Ok
                }
                _ => DecompileResult::Unknown,
            },
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token.to_ascii_uppercase().as_str() {
            // Conditionals
            "IF" => StackEffect::StartConstruct,
            "THEN" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            }, // consumes condition
            "ELSE" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "END" => StackEffect::EndConstruct,
            // CASE structure
            "CASE" => StackEffect::StartConstruct,
            "THENCASE" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            }, // consumes condition
            "ENDTHEN" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "ENDCASE" => StackEffect::EndConstruct,
            // Error handling
            "IFERR" => StackEffect::StartConstruct,
            "THENERR" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "ELSEERR" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "ENDERR" => StackEffect::EndConstruct,
            // DO/UNTIL loop
            "DO" => StackEffect::StartConstruct,
            "UNTIL" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            }, // consumes test result
            // WHILE/REPEAT loop
            "WHILE" => StackEffect::StartConstruct,
            "REPEAT" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            }, // consumes test result
            // START/NEXT/STEP loop
            "START" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            }, // consumes start, finish
            "NEXT" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "STEP" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            }, // consumes increment
            // FOR also consumes start, finish (handled separately)
            "FOR" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            // FORUP/FORDN: ascending/descending FOR with zero-iteration skip
            "FORUP" | "FORDN" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_if() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("IF", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Keyword);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_then() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("THEN", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_else() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("ELSE", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_end() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("END", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = FlowControlLib;

        for name in &["if", "If", "IF", "then", "Then", "THEN"] {
            let ctx = make_probe_ctx(name, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                name
            );
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_keywords() {
        let lib = FlowControlLib;
        assert!(matches!(
            lib.stack_effect("IF"),
            StackEffect::StartConstruct
        ));
        assert!(matches!(
            lib.stack_effect("THEN"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 0
            }
        ));
        assert!(matches!(
            lib.stack_effect("ELSE"),
            StackEffect::Fixed {
                consumes: 0,
                produces: 0
            }
        ));
        assert!(matches!(lib.stack_effect("END"), StackEffect::EndConstruct));
    }
}
