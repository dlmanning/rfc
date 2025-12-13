//! Syntax library for arithmetic and comparison operators.
//!
//! This library recognizes arithmetic and comparison operator tokens and declares their stack effects.
//! It does NOT execute operators - that's done by type-owning libraries (RealNumbersLib, etc.)
//! which register implementations in the OperatorRegistry.

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::operator::OperatorKind;

/// Syntax library for arithmetic and comparison operators.
///
/// Probes: `+`, `-`, `*`, `/`, `^`, `MOD`, `ABS`, `NEG`, `INV`
///         `<`, `>`, `<=`, `>=`, `==`, `!=`, `SAME`
/// Does not execute - operators are dispatched to type-owning libraries.
pub struct ArithmeticLib;

impl ArithmeticLib {
    /// Library ID for arithmetic/comparison syntax (HP RPL Library 64).
    pub const ID: LibraryId = LibraryId::new(64);

    /// Get operator info from text.
    /// Returns (OperatorKind, arity, is_binary_infix) if recognized.
    fn operator_info(text: &str) -> Option<(OperatorKind, u8, bool)> {
        match text {
            // Arithmetic operators
            "+" => Some((OperatorKind::Add, 2, true)),
            "-" => Some((OperatorKind::Sub, 2, true)),
            "*" => Some((OperatorKind::Mul, 2, true)),
            "/" => Some((OperatorKind::Div, 2, true)),
            "^" => Some((OperatorKind::Pow, 2, true)),
            // Comparison operators
            "<" => Some((OperatorKind::Lt, 2, true)),
            ">" => Some((OperatorKind::Gt, 2, true)),
            "<=" | "≤" => Some((OperatorKind::Le, 2, true)),
            ">=" | "≥" => Some((OperatorKind::Ge, 2, true)),
            "==" => Some((OperatorKind::Eq, 2, true)),
            "!=" | "≠" | "<>" => Some((OperatorKind::Ne, 2, true)),
            _ => {
                let upper = text.to_ascii_uppercase();
                match upper.as_str() {
                    "MOD" => Some((OperatorKind::Mod, 2, false)),
                    "NEG" => Some((OperatorKind::Neg, 1, false)),
                    "ABS" => Some((OperatorKind::Abs, 1, false)),
                    "INV" => Some((OperatorKind::Inv, 1, false)),
                    "SAME" => Some((OperatorKind::Same, 2, false)),
                    _ => None,
                }
            }
        }
    }

    /// Get the precedence for an operator (higher = binds tighter).
    fn precedence(text: &str) -> u8 {
        match text {
            // Comparison (lowest)
            "<" | ">" | "<=" | ">=" | "==" | "!=" | "<>" | "≤" | "≥" | "≠" => 5,
            // Addition/subtraction
            "+" | "-" => 10,
            // Multiplication/division
            "*" | "/" | "MOD" => 20,
            // Power (highest)
            "^" => 30,
            _ => 0,
        }
    }
}

impl Library for ArithmeticLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Arithmetic"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if let Some((_, _, is_binary_infix)) = Self::operator_info(text) {
            let info = if is_binary_infix && ctx.in_infix() {
                // In infix mode, return binary operator info with precedence
                let prec = Self::precedence(text);
                if text == "^" {
                    // Power is right-associative
                    TokenInfo::binary_right(text.len() as u8, prec)
                } else {
                    TokenInfo::binary_left(text.len() as u8, prec)
                }
            } else {
                // In RPN mode, operators are just atoms
                TokenInfo::atom(text.len() as u8)
            };

            ProbeResult::Match {
                info,
                semantic: SemanticKind::Operator,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
        // The compiler handles Operator stack effects via the registry.
        // ArithmeticLib just recognizes the syntax; actual bytecode emission
        // is done by the compiler based on stack_effect().
        CompileResult::Ok
    }

    fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
        // ArithmeticLib doesn't execute anything.
        // Operators are dispatched to type-owning libraries (RealNumbersLib, etc.)
        ExecuteResult::Error("ArithmeticLib has no executable commands".into())
    }

    fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
        // ArithmeticLib doesn't emit bytecode, so nothing to decompile.
        // Operator bytecode comes from type-owning libraries or dispatch.
        DecompileResult::Unknown
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        if let Some((kind, arity, _)) = Self::operator_info(token) {
            StackEffect::Operator { kind, arity }
        } else {
            StackEffect::Dynamic
        }
    }

    // NOTE: No register_operators() - ArithmeticLib doesn't own any types
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::TokenType;
    use rpl_core::{Interner, Pos, Span};

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
        use rpl_lang::VM;

        let lib = ArithmeticLib;
        let mut vm = VM::new();
        let mut ctx = ExecuteContext::new(&mut vm, &[], 0, 0);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }
}
