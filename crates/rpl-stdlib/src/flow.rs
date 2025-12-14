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
use rpl_lang::library::{ConstructKind, ExecuteOk, StackEffect};
use rpl_lang::Value;

/// Check if a value is falsy (0, 0.0, or false).
fn is_falsy(value: &Value) -> bool {
    match value {
        Value::Real(r) => *r == 0.0,
        Value::Int(i) => *i == 0,
        Value::Bool(b) => !b,
        _ => false, // Non-numeric values are truthy
    }
}

rpl_macros::define_library! {
    pub library FlowControlLib(9, "FlowControl");

    commands {
        @JUMP_IF_FALSE (0 -> 0) "Jump if top of stack is false/zero" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let condition = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if is_falsy(&condition) {
                Ok(ExecuteOk::Jump(target))
            } else {
                Ok(ExecuteOk::Ok)
            }
        }

        @JUMP (0 -> 0) "Unconditional jump" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };
            Ok(ExecuteOk::Jump(target))
        }

        @JUMP_IF_TRUE (0 -> 0) "Jump if top of stack is true/nonzero" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let condition = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if !is_falsy(&condition) {
                Ok(ExecuteOk::Jump(target))
            } else {
                Ok(ExecuteOk::Ok)
            }
        }

        @LOOP_NEXT (0 -> 0) "Decrement loop counter and jump if not zero (for START/NEXT)" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let (start, end, direction, counter) = match ctx.pop_start_loop() {
                Ok(v) => v,
                Err(_) => return Err("Return stack underflow".to_string()),
            };

            let new_counter = counter + 1;

            if new_counter <= end {
                if ctx.push_start_loop(start, end, direction, new_counter).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(ExecuteOk::Jump(target))
            } else {
                Ok(ExecuteOk::Ok)
            }
        }

        @LOOP_SETUP (0 -> 0) "Setup loop counter from stack (for START/NEXT)" {
            let finish = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };
            let start = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };

            let direction = if start < finish {
                -1i64
            } else if start > finish {
                1i64
            } else {
                0i64
            };

            if ctx.push_start_loop(start, finish, direction, start).is_err() {
                return Err("Return stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @IF_MARKER (0 -> 0) "Marker for IF (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @END_MARKER (0 -> 0) "Marker for END (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @DO_MARKER (0 -> 0) "Marker for DO (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @WHILE_MARKER (0 -> 0) "Marker for WHILE (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @REPEAT_MARKER (0 -> 0) "Marker for REPEAT (used after JUMP_IF_FALSE in WHILE/REPEAT)" {
            Ok(ExecuteOk::Ok)
        }

        @UNTIL_JUMP (0 -> 0) "Jump back if false (for UNTIL - loops while false)" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let condition = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if is_falsy(&condition) {
                Ok(ExecuteOk::Jump(target))
            } else {
                Ok(ExecuteOk::Ok)
            }
        }

        @FOR_SETUP (0 -> 0) "Setup loop with index variable (for FOR/NEXT)" {
            let sym_id = match ctx.read_operand() {
                Ok(w) => w,
                Err(e) => return Err(e.to_string()),
            };

            let finish = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };
            let start = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };

            let direction = if start < finish {
                -1i64
            } else if start > finish {
                1i64
            } else {
                0i64
            };

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            if ctx.create_local_frame_for(symbol, start).is_err() {
                return Err("Failed to create local frame".to_string());
            }

            if ctx.push_for_loop(start, finish, direction, start).is_err() {
                return Err("Return stack overflow".to_string());
            }

            Ok(ExecuteOk::Ok)
        }

        @FOR_NEXT (0 -> 0) "Increment loop counter and jump if not done (for FOR/NEXT)" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let (start, end, direction, counter) = match ctx.pop_for_loop() {
                Ok(v) => v,
                Err(_) => return Err("Return stack underflow".to_string()),
            };

            let new_counter = counter + 1;

            if new_counter <= end {
                ctx.set_for_variable(new_counter);
                if ctx.push_for_loop(start, end, direction, new_counter).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(ExecuteOk::Jump(target))
            } else {
                ctx.pop_local_frame();
                Ok(ExecuteOk::Ok)
            }
        }

        @FOR_END (0 -> 0) "Pop loop index from return stack (cleanup after FOR/NEXT)" {
            ctx.pop_local_frame();
            Ok(ExecuteOk::Ok)
        }

        @LOOP_STEP (0 -> 0) "STEP for START/STEP loop - custom increment from stack" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let increment = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow: STEP requires increment".to_string()),
                },
            };

            let (start, end, direction, counter) = match ctx.pop_start_loop() {
                Ok(v) => v,
                Err(_) => return Err("Return stack underflow".to_string()),
            };

            let new_counter = counter + increment;

            let should_continue = if direction < 0 {
                new_counter <= end
            } else if direction > 0 {
                new_counter >= end
            } else {
                new_counter == end
            };

            if should_continue {
                if ctx.push_start_loop(start, end, direction, new_counter).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(ExecuteOk::Jump(target))
            } else {
                Ok(ExecuteOk::Ok)
            }
        }

        @FOR_STEP (0 -> 0) "STEP for FOR/STEP loop - custom increment from stack" {
            let target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let increment = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow: STEP requires increment".to_string()),
                },
            };

            let (start, end, direction, counter) = match ctx.pop_for_loop() {
                Ok(v) => v,
                Err(_) => return Err("Return stack underflow".to_string()),
            };

            let new_counter = counter + increment;

            let should_continue = if direction < 0 {
                new_counter <= end
            } else if direction > 0 {
                new_counter >= end
            } else {
                new_counter == end
            };

            if should_continue {
                ctx.set_for_variable(new_counter);
                if ctx.push_for_loop(start, end, direction, new_counter).is_err() {
                    return Err("Return stack overflow".to_string());
                }
                Ok(ExecuteOk::Jump(target))
            } else {
                ctx.pop_local_frame();
                Ok(ExecuteOk::Ok)
            }
        }

        @FORUP_SETUP (0 -> 0) "Setup ascending FOR loop with skip check (for FORUP)" {
            let sym_id = match ctx.read_operand() {
                Ok(w) => w,
                Err(e) => return Err(e.to_string()),
            };

            let skip_target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let finish = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };
            let start = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };

            if start > finish {
                return Ok(ExecuteOk::Jump(skip_target));
            }

            let direction = -1i64;

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            if ctx.create_local_frame_for(symbol, start).is_err() {
                return Err("Failed to create local frame".to_string());
            }

            if ctx.push_for_loop(start, finish, direction, start).is_err() {
                return Err("Return stack overflow".to_string());
            }

            Ok(ExecuteOk::Ok)
        }

        @FORDN_SETUP (0 -> 0) "Setup descending FOR loop with skip check (for FORDN)" {
            let sym_id = match ctx.read_operand() {
                Ok(w) => w,
                Err(e) => return Err(e.to_string()),
            };

            let skip_target = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };

            let finish = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };
            let start = match ctx.pop_int() {
                Ok(v) => v,
                Err(_) => match ctx.pop_real() {
                    Ok(v) => v as i64,
                    Err(_) => return Err("Stack underflow".to_string()),
                },
            };

            if start < finish {
                return Ok(ExecuteOk::Jump(skip_target));
            }

            let direction = 1i64;

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            if ctx.create_local_frame_for(symbol, start).is_err() {
                return Err("Failed to create local frame".to_string());
            }

            if ctx.push_for_loop(start, finish, direction, start).is_err() {
                return Err("Return stack overflow".to_string());
            }

            Ok(ExecuteOk::Ok)
        }

        @CASE_MARKER (0 -> 0) "Marker for CASE (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @ENDCASE_MARKER (0 -> 0) "Marker for ENDCASE (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @ENDTHEN_MARKER (0 -> 0) "Marker for ENDTHEN (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @IFERR_SETUP (0 -> 0) "Setup error handler (for IFERR)" {
            let handler_pc = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.to_string()),
            };
            let code = ctx.current_code();
            if ctx.push_error_handler(handler_pc, code).is_err() {
                return Err("Return stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @THENERR_MARKER (0 -> 0) "Marker for THENERR (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @ELSEERR_MARKER (0 -> 0) "Marker for ELSEERR (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @ENDERR_MARKER (0 -> 0) "Marker for ENDERR (no-op, for decompilation)" {
            Ok(ExecuteOk::Ok)
        }

        @IFERR_SUCCESS (0 -> 0) "Pop error handler (for successful IFERR completion)" {
            if ctx.pop_error_handler().is_err() {
                return Err("No error handler to pop".to_string());
            }
            Ok(ExecuteOk::Ok)
        }
    }

    custom probe {
        let text = ctx.text();

        match text.to_ascii_uppercase().as_str() {
            // Conditionals
            "IF" | "THEN" | "ELSE" | "END" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // CASE structure
            "CASE" | "THENCASE" | "ENDTHEN" | "ENDCASE" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Error handling
            "IFERR" | "THENERR" | "ELSEERR" | "ENDERR" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Indefinite loops
            "DO" | "UNTIL" | "WHILE" | "REPEAT" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Definite loops
            "START" | "NEXT" | "STEP" | "FOR" | "FORUP" | "FORDN" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Keyword,
            },
            _ => rpl_lang::library::ProbeResult::NoMatch,
        }
    }

    custom compile {
        let text = ctx.text();

        match text.to_ascii_uppercase().as_str() {
            // === Conditionals ===
            "IF" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_IF_MARKER);
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::If,
                }
            }
            "THEN" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0);
                rpl_lang::library::CompileResult::NeedMore
            }
            "ELSE" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0);
                rpl_lang::library::CompileResult::NeedMore
            }

            // === DO/UNTIL/END loop ===
            "DO" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_DO_MARKER);
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::DoUntil,
                }
            }
            "UNTIL" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_UNTIL_JUMP);
                ctx.emit(0);
                rpl_lang::library::CompileResult::NeedMore
            }

            // === WHILE/REPEAT/END loop ===
            "WHILE" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_WHILE_MARKER);
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::While,
                }
            }
            "REPEAT" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0);
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_REPEAT_MARKER);
                rpl_lang::library::CompileResult::NeedMore
            }

            // === START/NEXT loop ===
            "START" => {
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::Start,
                }
            }
            "NEXT" => {
                let cmd = match ctx.current_construct() {
                    Some(ConstructKind::For)
                    | Some(ConstructKind::ForUp)
                    | Some(ConstructKind::ForDn) => Self::CMD_FOR_NEXT,
                    _ => Self::CMD_LOOP_NEXT,
                };
                ctx.emit_opcode(Self::ID.as_u16(), cmd);
                ctx.emit(0);
                rpl_lang::library::CompileResult::EndConstruct
            }
            "STEP" => {
                let cmd = match ctx.current_construct() {
                    Some(ConstructKind::For)
                    | Some(ConstructKind::ForUp)
                    | Some(ConstructKind::ForDn) => Self::CMD_FOR_STEP,
                    _ => Self::CMD_LOOP_STEP,
                };
                ctx.emit_opcode(Self::ID.as_u16(), cmd);
                ctx.emit(0);
                rpl_lang::library::CompileResult::EndConstruct
            }

            // === FOR/NEXT loop ===
            "FOR" => {
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::For,
                }
            }
            "FORUP" => {
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::ForUp,
                }
            }
            "FORDN" => {
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::ForDn,
                }
            }

            // === CASE/THENCASE/ENDTHEN/ENDCASE ===
            "CASE" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_CASE_MARKER);
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::Case,
                }
            }
            "THENCASE" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP_IF_FALSE);
                ctx.emit(0);
                rpl_lang::library::CompileResult::NeedMore
            }
            "ENDTHEN" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0);
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDTHEN_MARKER);
                rpl_lang::library::CompileResult::NeedMore
            }
            "ENDCASE" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDCASE_MARKER);
                rpl_lang::library::CompileResult::EndConstruct
            }

            // === IFERR/THENERR/ELSEERR/ENDERR ===
            "IFERR" => {
                rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::ErrorHandler,
                }
            }
            "THENERR" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_IFERR_SUCCESS);
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0);
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_THENERR_MARKER);
                rpl_lang::library::CompileResult::NeedMore
            }
            "ELSEERR" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_JUMP);
                ctx.emit(0);
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ELSEERR_MARKER);
                rpl_lang::library::CompileResult::NeedMore
            }
            "ENDERR" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_ENDERR_MARKER);
                rpl_lang::library::CompileResult::EndConstruct
            }

            // === END ===
            "END" => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_END_MARKER);
                rpl_lang::library::CompileResult::EndConstruct
            }
            _ => rpl_lang::library::CompileResult::NoMatch,
        }
    }

    custom decompile {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => rpl_lang::library::DecompileResult::Unknown,
            DecompileMode::Call(cmd) => match cmd {
                Self::CMD_IF_MARKER => {
                    ctx.write("IF");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_JUMP_IF_FALSE => {
                    let _target = ctx.read();
                    if let Some(next_word) = ctx.peek()
                        && !rpl_core::is_prolog(next_word)
                    {
                        let next_lib = rpl_core::extract_lib(next_word);
                        let next_cmd = rpl_core::extract_cmd(next_word);
                        if next_lib == Self::ID.as_u16() && next_cmd == Self::CMD_REPEAT_MARKER {
                            return rpl_lang::library::DecompileResult::Ok;
                        }
                    }
                    ctx.write("THEN");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_JUMP => {
                    let target = ctx.read().unwrap_or(0);
                    let current_pos = ctx.position();
                    if (target as usize) < current_pos {
                        return rpl_lang::library::DecompileResult::Ok;
                    }
                    ctx.write("ELSE");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_END_MARKER => {
                    ctx.write("END");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_DO_MARKER => {
                    ctx.write("DO");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_WHILE_MARKER => {
                    ctx.write("WHILE");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_REPEAT_MARKER => {
                    ctx.write("REPEAT");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_JUMP_IF_TRUE => {
                    let _target = ctx.read();
                    ctx.write("UNTIL");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_UNTIL_JUMP => {
                    let _target = ctx.read();
                    ctx.write("UNTIL");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_LOOP_NEXT => {
                    let _target = ctx.read();
                    ctx.write("NEXT");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_LOOP_SETUP => {
                    ctx.write("START");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_FOR_SETUP => {
                    let _symbol = ctx.read();
                    ctx.write("FOR");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_FOR_NEXT => {
                    let _target = ctx.read();
                    ctx.write("NEXT");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_LOOP_STEP => {
                    let _target = ctx.read();
                    ctx.write("STEP");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_FOR_STEP => {
                    let _target = ctx.read();
                    ctx.write("STEP");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_FORUP_SETUP => {
                    let _symbol = ctx.read();
                    let _skip_target = ctx.read();
                    ctx.write("FORUP");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_FORDN_SETUP => {
                    let _symbol = ctx.read();
                    let _skip_target = ctx.read();
                    ctx.write("FORDN");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_CASE_MARKER => {
                    ctx.write("CASE");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_ENDCASE_MARKER => {
                    ctx.write("ENDCASE");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_ENDTHEN_MARKER => {
                    ctx.write("ENDTHEN");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_IFERR_SETUP => {
                    let _handler_pc = ctx.read();
                    ctx.write("IFERR");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_IFERR_SUCCESS => {
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_THENERR_MARKER => {
                    ctx.write("THENERR");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_ELSEERR_MARKER => {
                    ctx.write("ELSEERR");
                    rpl_lang::library::DecompileResult::Ok
                }
                Self::CMD_ENDERR_MARKER => {
                    ctx.write("ENDERR");
                    rpl_lang::library::DecompileResult::Ok
                }
                _ => rpl_lang::library::DecompileResult::Unknown,
            },
        }
    }

    custom stack_effect {
        match token.to_ascii_uppercase().as_str() {
            // Conditionals
            "IF" => StackEffect::StartConstruct,
            "THEN" => StackEffect::Fixed { consumes: 1, produces: 0 },
            "ELSE" => StackEffect::Fixed { consumes: 0, produces: 0 },
            "END" => StackEffect::EndConstruct,
            // CASE structure
            "CASE" => StackEffect::StartConstruct,
            "THENCASE" => StackEffect::Fixed { consumes: 1, produces: 0 },
            "ENDTHEN" => StackEffect::Fixed { consumes: 0, produces: 0 },
            "ENDCASE" => StackEffect::EndConstruct,
            // Error handling
            "IFERR" => StackEffect::StartConstruct,
            "THENERR" => StackEffect::Fixed { consumes: 0, produces: 0 },
            "ELSEERR" => StackEffect::Fixed { consumes: 0, produces: 0 },
            "ENDERR" => StackEffect::EndConstruct,
            // DO/UNTIL loop
            "DO" => StackEffect::StartConstruct,
            "UNTIL" => StackEffect::Fixed { consumes: 1, produces: 0 },
            // WHILE/REPEAT loop
            "WHILE" => StackEffect::StartConstruct,
            "REPEAT" => StackEffect::Fixed { consumes: 1, produces: 0 },
            // START/NEXT/STEP loop
            "START" => StackEffect::Fixed { consumes: 2, produces: 0 },
            "NEXT" => StackEffect::Fixed { consumes: 0, produces: 0 },
            "STEP" => StackEffect::Fixed { consumes: 1, produces: 0 },
            // FOR
            "FOR" | "FORUP" | "FORDN" => StackEffect::Fixed { consumes: 2, produces: 0 },
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult};

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
