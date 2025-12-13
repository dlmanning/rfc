//! Library for program constructs and evaluation.
//!
//! This library provides:
//! - `::` - Start a program construct
//! - `;` - End a program construct
//! - `EVAL` - Execute a program from the stack

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{
    CompileContext, CompileResult, ConstructKind, DecompileContext, DecompileResult,
    ExecuteContext, ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};

/// Library for program constructs.
pub struct ProgramsLib;

impl ProgramsLib {
    /// Library ID for programs.
    pub const ID: LibraryId = LibraryId::new(8);

    /// Command ID for EVAL.
    const CMD_EVAL: u16 = 0;
}

impl Library for ProgramsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Programs"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        match text {
            "::" | "<<" => ProbeResult::Match {
                info: TokenInfo::open_bracket(text.len() as u8),
                semantic: SemanticKind::Bracket,
            },
            ";" | ">>" => ProbeResult::Match {
                info: TokenInfo::close_bracket(text.len() as u8),
                semantic: SemanticKind::Bracket,
            },
            _ if text.eq_ignore_ascii_case("EVAL") => ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            },
            _ => ProbeResult::NoMatch,
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        match text {
            "::" | "<<" => {
                // Don't emit prolog here - the compiler will emit it when handling StartConstruct
                // This ensures the compiler knows the correct start position for patching
                CompileResult::StartConstruct {
                    kind: ConstructKind::Program,
                }
            }
            ";" | ">>" => {
                // EndConstruct tells compiler to patch the prolog size
                CompileResult::EndConstruct
            }
            _ if text.eq_ignore_ascii_case("EVAL") => {
                ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_EVAL);
                CompileResult::Ok
            }
            _ => CompileResult::NoMatch,
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_EVAL => {
                // Pop a program object from the stack
                let program = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Check it's a program or symbolic expression
                match program {
                    rpl_lang::Value::Program { code, debug_info } => {
                        // New Value::Program variant - check for debug info
                        if let Some(dbg) = debug_info {
                            ExecuteResult::EvalProgramWithDebug {
                                code,
                                name: "<eval>".to_string(),
                                debug_info: dbg,
                            }
                        } else {
                            // No debug info but still provide a name for stack traces
                            ExecuteResult::EvalProgramNamed(code, "<eval>".to_string())
                        }
                    }
                    rpl_lang::Value::Object { type_id, data }
                        if type_id == TypeId::PROGRAM || type_id == TypeId::SYMBOLIC =>
                    {
                        // Object variant (no debug info available)
                        ExecuteResult::EvalProgramNamed(data, "<eval>".to_string())
                    }
                    _ => ExecuteResult::Error(
                        "Expected program or symbolic object for EVAL".to_string(),
                    ),
                }
            }
            _ => ExecuteResult::Error(format!("Unknown programs command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for PROGRAM prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::PROGRAM.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read(); // consume prolog
                    ctx.write("::");

                    // Recursively decompile the program body
                    if size > 0 {
                        ctx.write(" ");
                        ctx.decompile_inner(size);
                    }

                    ctx.write(" ;");
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
            DecompileMode::Call(cmd) => {
                if cmd == Self::CMD_EVAL {
                    ctx.write("EVAL");
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token {
            "::" | "<<" => StackEffect::StartConstruct,
            ";" | ">>" => StackEffect::EndConstruct,
            _ if token.eq_ignore_ascii_case("EVAL") => StackEffect::Dynamic,
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_core::token::TokenType;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_program_start() {
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
        assert!(matches!(lib.stack_effect("<<"), StackEffect::StartConstruct));
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
