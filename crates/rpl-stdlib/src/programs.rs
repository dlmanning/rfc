//! Library for program constructs and evaluation.
//!
//! This library provides:
//! - `::` - Start a program construct
//! - `;` - End a program construct
//! - `EVAL` - Execute a program from the stack

use rpl_core::TypeId;
use rpl_lang::Value;

rpl_macros::define_library! {
    pub library ProgramsLib(8, "Programs");

    constructs {
        "::": open(Program);
        "<<": open(Program);
        ";": close;
        ">>": close;
    }

    commands {
        EVAL (*) "Execute a program from the stack" {
            // Pop a program object from the stack
            let program = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            // Check it's a program or symbolic expression
            match program {
                Value::Program { code, debug_info } => {
                    // New Value::Program variant - check for debug info
                    if let Some(dbg) = debug_info {
                        Ok(rpl_lang::library::ExecuteOk::EvalProgramWithDebug {
                            code,
                            name: "<eval>".to_string(),
                            debug_info: dbg,
                        })
                    } else {
                        // No debug info but still provide a name for stack traces
                        Ok(rpl_lang::library::ExecuteOk::EvalProgramNamed(code, "<eval>".to_string()))
                    }
                }
                Value::Object { type_id, data }
                    if type_id == TypeId::PROGRAM || type_id == TypeId::SYMBOLIC =>
                {
                    // Object variant (no debug info available)
                    Ok(rpl_lang::library::ExecuteOk::EvalProgramNamed(data, "<eval>".to_string()))
                }
                _ => Err(
                    "Expected program or symbolic object for EVAL".to_string(),
                ),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::TokenType;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{
        CompileContext, CompileResult, ConstructKind, Library, ProbeContext, ProbeResult,
        StackEffect,
    };

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_program_start() {
        use rpl_core::token::SemanticKind;
        let interner = Interner::new();
        let lib = ProgramsLib;
        let ctx = make_probe_ctx("::", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::OpenBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_program_end() {
        use rpl_core::token::SemanticKind;
        let interner = Interner::new();
        let lib = ProgramsLib;
        let ctx = make_probe_ctx(";", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::CloseBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_eval() {
        use rpl_core::token::SemanticKind;
        let interner = Interner::new();
        let lib = ProgramsLib;
        let ctx = make_probe_ctx("EVAL", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Command);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_eval_case_insensitive() {
        let interner = Interner::new();
        let lib = ProgramsLib;

        for name in &["EVAL", "eval", "Eval"] {
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
        let lib = ProgramsLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_program_delimiters() {
        let lib = ProgramsLib;
        assert!(matches!(
            lib.stack_effect("::"),
            StackEffect::StartConstruct
        ));
        assert!(matches!(lib.stack_effect(";"), StackEffect::EndConstruct));
    }

    #[test]
    fn probe_chevron_aliases() {
        use rpl_core::token::SemanticKind;
        let interner = Interner::new();
        let lib = ProgramsLib;

        // << is alias for ::
        let ctx = make_probe_ctx("<<", &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::OpenBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected << to match"),
        }

        // >> is alias for ;
        let ctx = make_probe_ctx(">>", &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::CloseBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected >> to match"),
        }
    }

    #[test]
    fn stack_effect_chevron_aliases() {
        let lib = ProgramsLib;
        assert!(matches!(
            lib.stack_effect("<<"),
            StackEffect::StartConstruct
        ));
        assert!(matches!(lib.stack_effect(">>"), StackEffect::EndConstruct));
    }

    #[test]
    fn stack_effect_eval_is_dynamic() {
        let lib = ProgramsLib;
        assert!(matches!(lib.stack_effect("EVAL"), StackEffect::Dynamic));
    }

    #[test]
    fn compile_program_start() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ProgramsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(2));

        let mut ctx = CompileContext::new(span, "::", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(
            result,
            CompileResult::StartConstruct {
                kind: ConstructKind::Program
            }
        ));
        // Library doesn't emit prolog - the compiler handles that when it sees StartConstruct
        assert_eq!(ctx.position(), 0);
    }

    #[test]
    fn compile_program_end() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ProgramsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        let mut ctx = CompileContext::new(span, ";", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::EndConstruct));
    }

    #[test]
    fn compile_eval() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ProgramsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(4));

        let mut ctx = CompileContext::new(span, "EVAL", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
        assert_eq!(ctx.position(), 1);
    }
}
