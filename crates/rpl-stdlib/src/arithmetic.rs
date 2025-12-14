//! Syntax library for arithmetic and comparison operators.
//!
//! This library recognizes arithmetic and comparison operator tokens and declares their stack effects.
//! It does NOT execute operators - that's done by type-owning libraries (RealNumbersLib, etc.)
//! which register implementations in the OperatorRegistry.

rpl_macros::define_library! {
    pub library ArithmeticLib(64, "Arithmetic");

    operator_syntax {
        // Arithmetic operators
        "+"(Add, binary) { infix: 10 };
        "-"(Sub, binary) { infix: 10 };
        "*"(Mul, binary) { infix: 20 };
        "/"(Div, binary) { infix: 20 };
        "^"(Pow, binary) { infix: 30, right_assoc };

        // Word operators (case-insensitive)
        "MOD"(Mod, binary);
        "NEG"(Neg, unary);
        "ABS"(Abs, unary);
        "INV"(Inv, unary);
        "SAME"(Same, binary);

        // Comparison operators
        "<"(Lt, binary) { infix: 5 };
        ">"(Gt, binary) { infix: 5 };
        "<=" | "≤"(Le, binary) { infix: 5 };
        ">=" | "≥"(Ge, binary) { infix: 5 };
        "=="(Eq, binary) { infix: 5 };
        "!=" | "≠" | "<>"(Ne, binary) { infix: 5 };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::TokenType;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult, StackEffect};
    use rpl_lang::operator::OperatorKind;

    fn make_probe_ctx<'a>(
        text: &'a str,
        interner: &'a Interner,
        in_infix: bool,
    ) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, in_infix, None, None, interner)
    }

    #[test]
    fn probe_basic_operators() {
        let interner = Interner::new();
        let lib = ArithmeticLib;

        for op in &["+", "-", "*", "/", "^"] {
            let ctx = make_probe_ctx(op, &interner, false);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                op
            );
        }
    }

    #[test]
    fn probe_comparison_operators() {
        let interner = Interner::new();
        let lib = ArithmeticLib;

        for op in &["<", ">", "<=", ">=", "==", "!=", "<>", "SAME"] {
            let ctx = make_probe_ctx(op, &interner, false);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                op
            );
        }
    }

    #[test]
    fn probe_word_operators() {
        let interner = Interner::new();
        let lib = ArithmeticLib;

        for op in &["NEG", "ABS", "INV", "MOD", "neg", "abs", "inv", "mod", "SAME", "same"] {
            let ctx = make_probe_ctx(op, &interner, false);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                op
            );
        }
    }

    #[test]
    fn probe_unknown() {
        let interner = Interner::new();
        let lib = ArithmeticLib;
        let ctx = make_probe_ctx("SQRT", &interner, false);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn probe_returns_operator_semantic() {
        use rpl_core::token::SemanticKind;
        let interner = Interner::new();
        let lib = ArithmeticLib;
        let ctx = make_probe_ctx("+", &interner, false);

        if let ProbeResult::Match { semantic, .. } = lib.probe(&ctx) {
            assert_eq!(semantic, SemanticKind::Operator);
        } else {
            panic!("expected match");
        }
    }

    #[test]
    fn probe_infix_returns_binary_info() {
        let interner = Interner::new();
        let lib = ArithmeticLib;
        let ctx = make_probe_ctx("+", &interner, true);

        if let ProbeResult::Match { info, .. } = lib.probe(&ctx) {
            assert_eq!(info.ty(), TokenType::BinaryLeft);
        } else {
            panic!("expected match");
        }
    }

    #[test]
    fn probe_power_is_right_associative() {
        let interner = Interner::new();
        let lib = ArithmeticLib;
        let ctx = make_probe_ctx("^", &interner, true);

        if let ProbeResult::Match { info, .. } = lib.probe(&ctx) {
            assert_eq!(info.ty(), TokenType::BinaryRight);
        } else {
            panic!("expected match");
        }
    }

    #[test]
    fn stack_effect_binary() {
        let lib = ArithmeticLib;

        for token in &["+", "-", "*", "/", "^", "MOD"] {
            let effect = lib.stack_effect(token);
            match effect {
                StackEffect::Operator { arity: 2, .. } => {}
                _ => panic!("Expected binary operator for {}", token),
            }
        }
    }

    #[test]
    fn stack_effect_unary() {
        let lib = ArithmeticLib;

        for token in &["NEG", "ABS", "INV"] {
            let effect = lib.stack_effect(token);
            match effect {
                StackEffect::Operator { arity: 1, .. } => {}
                _ => panic!("Expected unary operator for {}", token),
            }
        }
    }

    #[test]
    fn stack_effect_add() {
        let lib = ArithmeticLib;
        let effect = lib.stack_effect("+");
        assert!(matches!(
            effect,
            StackEffect::Operator {
                kind: OperatorKind::Add,
                arity: 2
            }
        ));
    }

    #[test]
    fn stack_effect_neg() {
        let lib = ArithmeticLib;
        let effect = lib.stack_effect("NEG");
        assert!(matches!(
            effect,
            StackEffect::Operator {
                kind: OperatorKind::Neg,
                arity: 1
            }
        ));
    }

    #[test]
    fn execute_returns_error() {
        // ArithmeticLib should not execute anything
        use rpl_lang::library::ExecuteContext;
        use rpl_lang::VM;

        let lib = ArithmeticLib;
        let mut vm = VM::new();
        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, 0);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }
}
