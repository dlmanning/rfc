//! Library for real numbers - literals and operator implementations.
//!
//! This library:
//! - Probes and compiles real number literals
//! - Registers and executes arithmetic operators for Real type

use crate::codecs::RealCodec;
use rpl_lang::library::EXEC_OK;

rpl_macros::define_library! {
    pub library RealNumbersLib(10, "RealNumbers");

    literals {
        real: RealCodec;
    }

    prologs {
        REAL: format(RealCodec);
    }

    commands {
        // Internal operator implementations (called via operator dispatch)
        @ADD (2 -> 1) "Add two reals" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a + b).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @SUB (2 -> 1) "Subtract two reals" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a - b).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @MUL (2 -> 1) "Multiply two reals" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a * b).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @DIV (2 -> 1) "Divide two reals" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            if b == 0.0 {
                return Err("Division by zero".to_string());
            }
            ctx.push_real(a / b).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @MOD (2 -> 1) "Modulo of two reals" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            if b == 0.0 {
                return Err("Division by zero".to_string());
            }
            ctx.push_real(a % b).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @NEG (1 -> 1) "Negate a real" {
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(-a).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @ABS (1 -> 1) "Absolute value" {
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a.abs()).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @INV (1 -> 1) "Inverse (1/x)" {
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            if a == 0.0 {
                return Err("Division by zero".to_string());
            }
            ctx.push_real(1.0 / a).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @POW (2 -> 1) "Power" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a.powf(b)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        // Comparison operators (return 1.0 for true, 0.0 for false)
        @LT (2 -> 1) "Less than" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if a < b { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @GT (2 -> 1) "Greater than" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if a > b { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @LE (2 -> 1) "Less than or equal" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if a <= b { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @GE (2 -> 1) "Greater than or equal" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if a >= b { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @EQ (2 -> 1) "Equal" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if (a - b).abs() < f64::EPSILON { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @NE (2 -> 1) "Not equal" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if (a - b).abs() >= f64::EPSILON { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        @SAME (2 -> 1) "Same (identical)" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(if (a - b).abs() < f64::EPSILON { 1.0 } else { 0.0 }).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        // User-visible real number functions
        SQ (1 -> 1) "Square (x^2)" {
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a * a).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        MIN (2 -> 1) "Minimum of two values" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a.min(b)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        MAX (2 -> 1) "Maximum of two values" {
            let b = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            ctx.push_real(a.max(b)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        SIGN (1 -> 1) "Sign (-1, 0, or 1)" {
            let a = ctx.pop_real().map_err(|_| "Stack underflow")?;
            let sign = if a > 0.0 { 1.0 } else if a < 0.0 { -1.0 } else { 0.0 };
            ctx.push_real(sign).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }
    }

    operators {
        // Binary arithmetic operators
        Add(Symmetric REAL) -> CMD_ADD { commutative };
        Sub(Symmetric REAL) -> CMD_SUB;
        Mul(Symmetric REAL) -> CMD_MUL { commutative };
        Div(Symmetric REAL) -> CMD_DIV;
        Mod(Symmetric REAL) -> CMD_MOD;
        Pow(Symmetric REAL) -> CMD_POW;

        // Unary operators
        Neg(Unary REAL) -> CMD_NEG;
        Abs(Unary REAL) -> CMD_ABS;
        Inv(Unary REAL) -> CMD_INV;

        // Comparison operators (result is REAL: 1.0 or 0.0)
        Lt(Symmetric REAL) -> CMD_LT;
        Gt(Symmetric REAL) -> CMD_GT;
        Le(Symmetric REAL) -> CMD_LE;
        Ge(Symmetric REAL) -> CMD_GE;
        Eq(Symmetric REAL) -> CMD_EQ { commutative };
        Ne(Symmetric REAL) -> CMD_NE { commutative };
        Same(Symmetric REAL) -> CMD_SAME { commutative };
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{ExecuteContext, Library, ProbeContext, ProbeResult, StackEffect};
    use rpl_lang::VM;

    use super::*;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn probe_integer() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("123", &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_negative_integer() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("-5", &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_float() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("3.14", &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_scientific() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("1.5e10", &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_not_a_number() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("abc", &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_produces_one() {
        let lib = RealNumbersLib;
        let effect = lib.stack_effect("123");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 0,
                produces: 1
            }
        ));
    }

    #[test]
    fn compile_number() {
        use rpl_lang::compile::OutputBuffer;
        use rpl_lang::library::CompileContext;

        let lib = RealNumbersLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(4));

        let mut ctx = CompileContext::new(span, "3.14", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, rpl_lang::library::CompileResult::Ok));
        // Should have emitted: prolog + 2 data words = 3 words
        assert_eq!(ctx.position(), 3);
    }

    // Operator execution tests
    #[test]
    fn execute_add() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(3.0).unwrap();
        vm.push_real(4.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_ADD);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 7.0);
    }

    #[test]
    fn execute_sub() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(10.0).unwrap();
        vm.push_real(3.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_SUB);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 7.0);
    }

    #[test]
    fn execute_mul() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(3.0).unwrap();
        vm.push_real(4.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_MUL);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 12.0);
    }

    #[test]
    fn execute_div() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(10.0).unwrap();
        vm.push_real(4.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_DIV);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 2.5);
    }

    #[test]
    fn execute_neg() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(5.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_NEG);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), -5.0);
    }

    #[test]
    fn execute_abs() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(-5.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_ABS);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 5.0);
    }

    #[test]
    fn execute_inv() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(4.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_INV);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 0.25);
    }

    #[test]
    fn execute_pow() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(2.0).unwrap();
        vm.push_real(3.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_POW);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 8.0);
    }

    #[test]
    fn execute_mod() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(10.0).unwrap();
        vm.push_real(3.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_MOD);
        let result = lib.execute(&mut ctx);
        assert!(result.is_ok());
        assert_eq!(vm.pop_real().unwrap(), 1.0);
    }

    #[test]
    fn execute_underflow() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_ADD);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_div_by_zero() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(10.0).unwrap();
        vm.push_real(0.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_DIV);
        let result = lib.execute(&mut ctx);
        match result {
            Err(msg) => {
                assert!(
                    msg.contains("zero"),
                    "Expected division by zero error, got: {}",
                    msg
                );
            }
            _ => panic!("Expected error for division by zero"),
        }
    }
}
