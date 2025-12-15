//! Library for flow control constructs.
//!
//! This library provides:
//! - `IF` / `THEN` / `ELSE` / `END` - Conditional execution
//! - `CASE` / `THENCASE` / `ENDTHEN` / `ENDCASE` - Multiple-choice structure
//! - `IFERR` / `THENERR` / `ELSEERR` / `ENDERR` - Error handling
//! - `DO` / `UNTIL` - Indefinite loop (test at end)
//! - `WHILE` / `REPEAT` / `END` - Indefinite loop (test at start)
//! - `START` / `NEXT` / `STEP` - Definite loop (count-controlled)
//! - `FOR` / `FORUP` / `FORDN` - FOR loops with index variable
//! - `IFT` / `IFTE` - Inline conditionals
//!
//! ## Bytecode Primitives
//!
//! All control flow compiles down to these primitives:
//! - `JUMP <offset>` - Unconditional jump
//! - `JUMP_IF_FALSE <offset>` - Jump if top of stack is falsy
//! - `LOOP_SETUP` / `LOOP_NEXT` / `LOOP_STEP` - START loop management
//! - `FOR_SETUP` / `FOR_NEXT` / `FOR_STEP` - FOR loop management (with variable)
//! - `IFERR_SETUP` / `IFERR_SUCCESS` - Error handler management
//!
//! The high-level keywords (IF, THEN, WHILE, etc.) are compile-time constructs
//! that generate these primitives with computed offsets.

use rpl_lang::library::{EXEC_OK, ExecuteOk};

rpl_macros::define_library! {
    pub library FlowControlLib(9, "FlowControl");

    commands {
        // ================================================================
        // Jump primitives - the core of all control flow
        // ================================================================

        @JUMP_IF_FALSE (1 -> 0) "Jump if top of stack is false/zero" {
            let target = ctx.read_operand()? as usize;
            let condition = ctx.pop().map_err(|_| "Stack underflow")?;
            if Self::is_falsy(&condition) {
                Ok(ExecuteOk::Jump(target))
            } else {
                EXEC_OK
            }
        }

        @JUMP (0 -> 0) "Unconditional jump" {
            let target = ctx.read_operand()? as usize;
            Ok(ExecuteOk::Jump(target))
        }

        // ================================================================
        // START/NEXT/STEP loop primitives
        // ================================================================

        @LOOP_SETUP (2 -> 0) "Setup loop counter from stack (for START)" {
            let finish = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;
            let start = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;

            ctx.push_start_loop(finish, start)
                .map_err(|_| "Return stack overflow")?;
            EXEC_OK
        }

        @LOOP_NEXT (0 -> 0) "Increment counter and loop if not done (for NEXT)" {
            let target = ctx.read_operand()? as usize;
            let (end, counter) = ctx.pop_start_loop()
                .map_err(|_| "Return stack underflow")?;

            let new_counter = counter + 1;
            if new_counter <= end {
                ctx.push_start_loop(end, new_counter)
                    .map_err(|_| "Return stack overflow")?;
                Ok(ExecuteOk::Jump(target))
            } else {
                EXEC_OK
            }
        }

        @LOOP_STEP (1 -> 0) "Custom increment and loop if not done (for STEP)" {
            let target = ctx.read_operand()? as usize;
            let increment = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow: STEP requires increment")?;

            let (end, counter) = ctx.pop_start_loop()
                .map_err(|_| "Return stack underflow")?;

            let new_counter = counter + increment;
            // Use increment direction to determine bound check
            let should_continue = if increment > 0 {
                new_counter <= end
            } else {
                new_counter >= end
            };

            if should_continue {
                ctx.push_start_loop(end, new_counter)
                    .map_err(|_| "Return stack overflow")?;
                Ok(ExecuteOk::Jump(target))
            } else {
                EXEC_OK
            }
        }

        // ================================================================
        // FOR/NEXT/STEP loop primitives (with variable binding)
        // ================================================================

        @FOR_SETUP (2 -> 0) "Setup FOR loop with index variable" {
            let sym_id = ctx.read_operand()?;
            let finish = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;
            let start = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            ctx.create_local_frame_for(symbol, start)
                .map_err(|_| "Failed to create local frame")?;
            ctx.push_for_loop(finish, start)
                .map_err(|_| "Return stack overflow")?;
            EXEC_OK
        }

        @FOR_NEXT (0 -> 0) "Increment FOR counter and loop if not done" {
            let target = ctx.read_operand()? as usize;
            let (end, counter) = ctx.pop_for_loop()
                .map_err(|_| "Return stack underflow")?;

            let new_counter = counter + 1;
            if new_counter <= end {
                ctx.set_for_variable(new_counter);
                ctx.push_for_loop(end, new_counter)
                    .map_err(|_| "Return stack overflow")?;
                Ok(ExecuteOk::Jump(target))
            } else {
                ctx.pop_local_frame();
                EXEC_OK
            }
        }

        @FOR_STEP (1 -> 0) "Custom increment FOR counter and loop if not done" {
            let target = ctx.read_operand()? as usize;
            let increment = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow: STEP requires increment")?;

            let (end, counter) = ctx.pop_for_loop()
                .map_err(|_| "Return stack underflow")?;

            let new_counter = counter + increment;
            // Use increment direction to determine bound check
            let should_continue = if increment > 0 {
                new_counter <= end
            } else {
                new_counter >= end
            };

            if should_continue {
                ctx.set_for_variable(new_counter);
                ctx.push_for_loop(end, new_counter)
                    .map_err(|_| "Return stack overflow")?;
                Ok(ExecuteOk::Jump(target))
            } else {
                ctx.pop_local_frame();
                EXEC_OK
            }
        }

        @FORUP_SETUP (2 -> 0) "Setup ascending FOR loop (skip if start > end)" {
            let sym_id = ctx.read_operand()?;
            let skip_target = ctx.read_operand()? as usize;
            let finish = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;
            let start = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;

            if start > finish {
                return Ok(ExecuteOk::Jump(skip_target));
            }

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            ctx.create_local_frame_for(symbol, start)
                .map_err(|_| "Failed to create local frame")?;
            ctx.push_for_loop(finish, start)
                .map_err(|_| "Return stack overflow")?;
            EXEC_OK
        }

        @FORDN_SETUP (2 -> 0) "Setup descending FOR loop (skip if start < end)" {
            let sym_id = ctx.read_operand()?;
            let skip_target = ctx.read_operand()? as usize;
            let finish = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;
            let start = ctx.pop_int()
                .or_else(|_| ctx.pop_real().map(|v| v as i64))
                .map_err(|_| "Stack underflow")?;

            if start < finish {
                return Ok(ExecuteOk::Jump(skip_target));
            }

            let symbol = rpl_core::Symbol::from_raw(sym_id);
            ctx.create_local_frame_for(symbol, start)
                .map_err(|_| "Failed to create local frame")?;
            ctx.push_for_loop(finish, start)
                .map_err(|_| "Return stack overflow")?;
            EXEC_OK
        }

        // ================================================================
        // Error handling primitives
        // ================================================================

        @IFERR_SETUP (0 -> 0) "Push error handler for IFERR" {
            let handler_pc = ctx.read_operand()? as usize;
            let code = ctx.current_code();
            ctx.push_error_handler(handler_pc, code)
                .map_err(|_| "Return stack overflow")?;
            EXEC_OK
        }

        @IFERR_SUCCESS (0 -> 0) "Pop error handler (IFERR completed without error)" {
            ctx.pop_error_handler()
                .map_err(|_| "No error handler to pop")?;
            EXEC_OK
        }

    }

    // ================================================================
    // Control flow keywords - explicit role-based declarations
    // ================================================================
    control_flow {
        // --------------------------------------------------------
        // Openers - start new constructs
        // Note: START, FOR variants, and IFERR have their setup bytecode
        // emitted by the compiler, not the library. They just return
        // StartConstruct and the compiler handles the rest.
        // --------------------------------------------------------
        opener IF => If;
        opener DO => DoUntil;
        opener WHILE => While;
        opener START (2 -> 0) => Start;
        opener FOR (2 -> 0) => For;
        opener FORUP (2 -> 0) => ForUp;
        opener FORDN (2 -> 0) => ForDn;
        opener CASE => Case;
        opener IFERR => ErrorHandler;

        // --------------------------------------------------------
        // Transitions - emit bytecode, stay in construct (NeedMore)
        // --------------------------------------------------------
        transition THEN (1 -> 0) => emit JUMP_IF_FALSE;
        transition ELSE (0 -> 0) in [If] => emit JUMP;
        transition REPEAT (1 -> 0) in [While] => emit JUMP_IF_FALSE;
        transition UNTIL (1 -> 0) in [DoUntil] => emit JUMP_IF_FALSE;
        transition THENCASE (1 -> 0) in [Case] => emit JUMP_IF_FALSE;
        transition ENDTHEN (0 -> 0) in [Case] => emit JUMP;
        transition THENERR (0 -> 0) in [ErrorHandler] => emit IFERR_SUCCESS, JUMP;
        transition ELSEERR (0 -> 0) in [ErrorHandler] => emit JUMP;

        // --------------------------------------------------------
        // Closers - end constructs
        // --------------------------------------------------------
        closer END in [If, While, DoUntil];
        closer NEXT in [Start] => emit LOOP_NEXT;
        closer NEXT in [For, ForUp, ForDn] => emit FOR_NEXT;
        closer STEP (1 -> 0) in [Start] => emit LOOP_STEP;
        closer STEP (1 -> 0) in [For, ForUp, ForDn] => emit FOR_STEP;
        closer ENDERR in [ErrorHandler];
        closer ENDCASE in [Case];

        // --------------------------------------------------------
        // Inline conditionals - stack-based, no construct
        // --------------------------------------------------------
        inline IFT (2 -> 1);
        inline IFTE (3 -> 1);
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span, token::SemanticKind};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult, StackEffect};

    use super::*;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_if() {
        let interner = Interner::new();
        let lib = FlowControlLib;
        let ctx = make_probe_ctx("IF", &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Keyword);
            }
            _ => panic!("expected match"),
        }
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
        assert!(matches!(
            lib.stack_effect("IFT"),
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("IFTE"),
            StackEffect::Fixed {
                consumes: 3,
                produces: 1
            }
        ));
    }
}
