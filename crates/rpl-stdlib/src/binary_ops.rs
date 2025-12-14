//! Library for binary integer operations.
//!
//! Explicit bitwise commands operating on binary integers:
//! - BAND, BOR, BXOR, BNOT - bitwise operations
//! - BLSL, BLSR - logical shifts
//! - BADD, BSUB, BMUL, BDIV - arithmetic on binary integers
//! - STWS, RCWS - word size control

use rpl_lang::library::EXEC_OK;

rpl_macros::define_library! {
    pub library BinaryOpsLib(70, "BinaryOps");

    commands {
        BAND (2 -> 1) [
            brief: "Bitwise AND",
            stack: "( #a #b -- #result )",
            example: "#FF #0F BAND",
            see_also: ["BOR", "BXOR"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            if let Err(e) = ctx.push_bint(a & b) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BOR (2 -> 1) [
            brief: "Bitwise OR",
            stack: "( #a #b -- #result )",
            example: "#F0 #0F BOR",
            see_also: ["BAND", "BXOR"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            if let Err(e) = ctx.push_bint(a | b) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BXOR (2 -> 1) [
            brief: "Bitwise XOR",
            stack: "( #a #b -- #result )",
            example: "#FF #F0 BXOR",
            see_also: ["BAND", "BOR"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            if let Err(e) = ctx.push_bint(a ^ b) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BNOT (1 -> 1) [
            brief: "Bitwise NOT (respects word size)",
            stack: "( #a -- #result )",
            example: "#FF BNOT",
            see_also: ["BAND", "STWS"],
        ] {
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let ws = ctx.word_size();
            let mask = word_mask(ws);
            if let Err(e) = ctx.push_bint(!a & mask) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BLSL (2 -> 1) [
            brief: "Bitwise logical shift left",
            stack: "( #a #shift -- #result )",
            example: "#1 #4 BLSL",
            see_also: ["BLSR"],
        ] {
            let shift = match ctx.pop_bint() {
                Ok(v) => v as u32,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let ws = ctx.word_size();
            let mask = word_mask(ws);
            if let Err(e) = ctx.push_bint((a << shift) & mask) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BLSR (2 -> 1) [
            brief: "Bitwise logical shift right",
            stack: "( #a #shift -- #result )",
            example: "#10 #4 BLSR",
            see_also: ["BLSL"],
        ] {
            let shift = match ctx.pop_bint() {
                Ok(v) => v as u32,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v as u64,
                Err(e) => return Err(e.to_string()),
            };
            if let Err(e) = ctx.push_bint((a >> shift) as i64) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BADD (2 -> 1) [
            brief: "Binary integer addition (wraps at word size)",
            stack: "( #a #b -- #result )",
            example: "#10 #5 BADD",
            see_also: ["BSUB", "BMUL"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let ws = ctx.word_size();
            let mask = word_mask(ws);
            if let Err(e) = ctx.push_bint(a.wrapping_add(b) & mask) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BSUB (2 -> 1) [
            brief: "Binary integer subtraction (wraps at word size)",
            stack: "( #a #b -- #result )",
            example: "#10 #5 BSUB",
            see_also: ["BADD", "BMUL"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let ws = ctx.word_size();
            let mask = word_mask(ws);
            if let Err(e) = ctx.push_bint(a.wrapping_sub(b) & mask) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BMUL (2 -> 1) [
            brief: "Binary integer multiplication (wraps at word size)",
            stack: "( #a #b -- #result )",
            example: "#10 #5 BMUL",
            see_also: ["BADD", "BDIV"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            let ws = ctx.word_size();
            let mask = word_mask(ws);
            if let Err(e) = ctx.push_bint(a.wrapping_mul(b) & mask) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        BDIV (2 -> 1) [
            brief: "Binary integer division",
            stack: "( #a #b -- #result )",
            example: "#10 #5 BDIV",
            see_also: ["BMUL", "BSUB"],
        ] {
            let b = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            if b == 0 {
                return Err("Division by zero".to_string());
            }
            let a = match ctx.pop_bint() {
                Ok(v) => v,
                Err(e) => return Err(e.to_string()),
            };
            if let Err(e) = ctx.push_bint(a / b) {
                return Err(e.to_string());
            }
            EXEC_OK
        }

        STWS (1 -> 0) [
            brief: "Set word size (1-64 bits)",
            stack: "( n -- )",
            example: "16 STWS",
            see_also: ["RCWS"],
        ] {
            let ws = match ctx.pop_bint() {
                Ok(v) => v as u8,
                Err(e) => return Err(e.to_string()),
            };
            if ws == 0 || ws > 64 {
                return Err("Word size must be 1-64".to_string());
            }
            ctx.set_word_size(ws);
            EXEC_OK
        }

        RCWS (0 -> 1) [
            brief: "Recall current word size",
            stack: "( -- n )",
            example: "RCWS",
            see_also: ["STWS"],
        ] {
            let ws = ctx.word_size() as i64;
            if let Err(e) = ctx.push_bint(ws) {
                return Err(e.to_string());
            }
            EXEC_OK
        }
    }
}

/// Get the word size mask for current word size.
fn word_mask(ws: u8) -> i64 {
    if ws >= 64 { !0i64 } else { (1i64 << ws) - 1 }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span, token::SemanticKind};
    use rpl_lang::{
        VM,
        library::{ExecuteContext, Library, ProbeContext, ProbeResult, StackEffect},
    };

    use super::*;

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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
        assert_eq!(vm.pop_int().unwrap(), 0x0F);
    }

    #[test]
    fn execute_bnot_64bit() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_BNOT);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_err());
    }

    #[test]
    fn execute_rcws_default() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_RCWS);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_int().unwrap(), 64);
    }

    #[test]
    fn execute_stws_rcws() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(16).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_RCWS);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_int().unwrap(), 16);
    }

    #[test]
    fn execute_stws_zero_error() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(0).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_stws_65_error() {
        let lib = BinaryOpsLib;
        let mut vm = VM::new();
        vm.push_int(65).unwrap();

        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, BinaryOpsLib::CMD_STWS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
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
