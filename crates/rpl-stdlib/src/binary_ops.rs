//! Library for binary integer operations.
//!
//! Explicit bitwise commands operating on binary integers:
//! - BAND, BOR, BXOR, BNOT - bitwise operations
//! - BLSL, BLSR - logical shifts
//! - BADD, BSUB, BMUL, BDIV - arithmetic on binary integers
//! - STWS, RCWS - word size control

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};

/// Library for binary integer operations.
pub struct BinaryOpsLib;

impl BinaryOpsLib {
    /// Library ID for binary operations.
    pub const ID: LibraryId = LibraryId::new(70);

    // Command IDs - Bitwise operations
    const CMD_BAND: u16 = 0;
    const CMD_BOR: u16 = 1;
    const CMD_BXOR: u16 = 2;
    const CMD_BNOT: u16 = 3;
    const CMD_BLSL: u16 = 4;
    const CMD_BLSR: u16 = 5;
    // Arithmetic on binary integers
    const CMD_BADD: u16 = 10;
    const CMD_BSUB: u16 = 11;
    const CMD_BMUL: u16 = 12;
    const CMD_BDIV: u16 = 13;
    // Word size control
    const CMD_STWS: u16 = 20;
    const CMD_RCWS: u16 = 21;

    /// Get command ID from name (case-insensitive).
    fn command_id(name: &str) -> Option<u16> {
        match name.to_ascii_uppercase().as_str() {
            "BAND" => Some(Self::CMD_BAND),
            "BOR" => Some(Self::CMD_BOR),
            "BXOR" => Some(Self::CMD_BXOR),
            "BNOT" => Some(Self::CMD_BNOT),
            "BLSL" => Some(Self::CMD_BLSL),
            "BLSR" => Some(Self::CMD_BLSR),
            "BADD" => Some(Self::CMD_BADD),
            "BSUB" => Some(Self::CMD_BSUB),
            "BMUL" => Some(Self::CMD_BMUL),
            "BDIV" => Some(Self::CMD_BDIV),
            "STWS" => Some(Self::CMD_STWS),
            "RCWS" => Some(Self::CMD_RCWS),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_BAND => Some("BAND"),
            Self::CMD_BOR => Some("BOR"),
            Self::CMD_BXOR => Some("BXOR"),
            Self::CMD_BNOT => Some("BNOT"),
            Self::CMD_BLSL => Some("BLSL"),
            Self::CMD_BLSR => Some("BLSR"),
            Self::CMD_BADD => Some("BADD"),
            Self::CMD_BSUB => Some("BSUB"),
            Self::CMD_BMUL => Some("BMUL"),
            Self::CMD_BDIV => Some("BDIV"),
            Self::CMD_STWS => Some("STWS"),
            Self::CMD_RCWS => Some("RCWS"),
            _ => None,
        }
    }

    /// Get the word size mask for current word size.
    fn word_mask(ws: u8) -> i64 {
        if ws >= 64 { !0i64 } else { (1i64 << ws) - 1 }
    }
}

impl Library for BinaryOpsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "BinaryOps"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if Self::command_id(text).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        if let Some(cmd) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_BAND => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if let Err(e) = ctx.push_bint(a & b) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BOR => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if let Err(e) = ctx.push_bint(a | b) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BXOR => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if let Err(e) = ctx.push_bint(a ^ b) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BNOT => {
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let ws = ctx.word_size();
                let mask = Self::word_mask(ws);
                if let Err(e) = ctx.push_bint(!a & mask) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BLSL => {
                let shift = match ctx.pop_bint() {
                    Ok(v) => v as u32,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let ws = ctx.word_size();
                let mask = Self::word_mask(ws);
                if let Err(e) = ctx.push_bint((a << shift) & mask) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BLSR => {
                let shift = match ctx.pop_bint() {
                    Ok(v) => v as u32,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v as u64, // Logical shift treats as unsigned
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if let Err(e) = ctx.push_bint((a >> shift) as i64) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BADD => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let ws = ctx.word_size();
                let mask = Self::word_mask(ws);
                if let Err(e) = ctx.push_bint(a.wrapping_add(b) & mask) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BSUB => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let ws = ctx.word_size();
                let mask = Self::word_mask(ws);
                if let Err(e) = ctx.push_bint(a.wrapping_sub(b) & mask) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BMUL => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                let ws = ctx.word_size();
                let mask = Self::word_mask(ws);
                if let Err(e) = ctx.push_bint(a.wrapping_mul(b) & mask) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BDIV => {
                let b = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if b == 0 {
                    return ExecuteResult::Error("Division by zero".to_string());
                }
                let a = match ctx.pop_bint() {
                    Ok(v) => v,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if let Err(e) = ctx.push_bint(a / b) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_STWS => {
                let ws = match ctx.pop_bint() {
                    Ok(v) => v as u8,
                    Err(e) => return ExecuteResult::Error(e.to_string()),
                };
                if ws == 0 || ws > 64 {
                    return ExecuteResult::Error("Word size must be 1-64".to_string());
                }
                ctx.set_word_size(ws);
                ExecuteResult::Ok
            }
            Self::CMD_RCWS => {
                let ws = ctx.word_size() as i64;
                if let Err(e) = ctx.push_bint(ws) {
                    return ExecuteResult::Error(e.to_string());
                }
                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => DecompileResult::Unknown,
            DecompileMode::Call(cmd) => {
                if let Some(token) = Self::command_name(cmd) {
                    ctx.write(token);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            // Binary operations: 2 in, 1 out
            "BAND" | "BOR" | "BXOR" | "BLSL" | "BLSR" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            // Binary arithmetic: 2 in, 1 out
            "BADD" | "BSUB" | "BMUL" | "BDIV" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            // Unary: 1 in, 1 out
            "BNOT" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            // Word size
            "STWS" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "RCWS" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::VM;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_bitwise_ops() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;

        for text in &["BAND", "BOR", "BXOR", "BNOT", "BLSL", "BLSR"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_arithmetic_ops() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;

        for text in &["BADD", "BSUB", "BMUL", "BDIV"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_word_size_ops() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;

        for text in &["STWS", "RCWS"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;

        for text in &["band", "Band", "BAND", "bor", "Bor", "BOR"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_returns_command_semantic() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;
        let ctx = make_probe_ctx("BAND", &interner);

        if let ProbeResult::Match { semantic, .. } = lib.probe(&ctx) {
            assert_eq!(semantic, SemanticKind::Command);
        } else {
            panic!("expected match");
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = BinaryOpsLib;

        for text in &["AND", "OR", "XOR", "NOT", "ADD", "hello", "#FF"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::NoMatch),
                "Should not match: {}",
                text
            );
        }
    }

    #[test]
    fn execute_band() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0xF0).unwrap();
        vm.push_int(0x0F).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BAND);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0);
    }

    #[test]
    fn execute_bor() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0xF0).unwrap();
        vm.push_int(0x0F).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BOR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0xFF);
    }

    #[test]
    fn execute_bxor() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0xFF).unwrap();
        vm.push_int(0xF0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BXOR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0x0F);
    }

    #[test]
    fn execute_bnot_64bit() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BNOT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), -1); // All 1s in 64-bit
    }

    #[test]
    fn execute_bnot_8bit() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.set_word_size(8);
        vm.push_int(0xFF).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BNOT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0);
    }

    #[test]
    fn execute_blsl() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(1).unwrap();
        vm.push_int(4).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BLSL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 16);
    }

    #[test]
    fn execute_blsr() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(16).unwrap();
        vm.push_int(4).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BLSR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 1);
    }

    #[test]
    fn execute_badd() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0x10).unwrap();
        vm.push_int(0x05).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BADD);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0x15);
    }

    #[test]
    fn execute_bsub() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0x10).unwrap();
        vm.push_int(0x05).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BSUB);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0x0B);
    }

    #[test]
    fn execute_bmul() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0x10).unwrap();
        vm.push_int(0x05).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BMUL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 0x50);
    }

    #[test]
    fn execute_bdiv() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0x10).unwrap();
        vm.push_int(0x05).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BDIV);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 3);
    }

    #[test]
    fn execute_bdiv_by_zero() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0x10).unwrap();
        vm.push_int(0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BDIV);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_rcws_default() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_RCWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 64);
    }

    #[test]
    fn execute_stws_rcws() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(16).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_RCWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 16);
    }

    #[test]
    fn execute_stws_zero_error() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_stws_65_error() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(65).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn stack_effect_binary() {
        let lib = BinaryOpsLib;

        for token in &[
            "BAND", "BOR", "BXOR", "BLSL", "BLSR", "BADD", "BSUB", "BMUL", "BDIV",
        ] {
            let effect = lib.stack_effect(token);
            assert!(
                matches!(
                    effect,
                    StackEffect::Fixed {
                        consumes: 2,
                        produces: 1
                    }
                ),
                "Expected binary effect for {}",
                token
            );
        }
    }

    #[test]
    fn stack_effect_unary() {
        let lib = BinaryOpsLib;
        let effect = lib.stack_effect("BNOT");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
    }

    #[test]
    fn stack_effect_word_size() {
        let lib = BinaryOpsLib;

        let effect = lib.stack_effect("STWS");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 1,
                produces: 0
            }
        ));

        let effect = lib.stack_effect("RCWS");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 0,
                produces: 1
            }
        ));
    }
}
