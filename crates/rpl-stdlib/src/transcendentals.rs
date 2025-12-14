//! Library for transcendental functions.
//!
//! This library provides:
//! - Trigonometric: SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2
//! - Hyperbolic: SINH, COSH, TANH, ASINH, ACOSH, ATANH
//! - Logarithmic: LN, LOG, LNP1
//! - Exponential: EXP, ALOG, EXPM
//! - Other: SQRT, PI
//!
//! Note: All trigonometric functions currently use radians.
//! Angle mode (DEG/RAD/GRAD) will be added when FlagsLib is implemented.

use rpl_lang::{Value, library::EXEC_OK};

/// Helper for unary operations on real numbers.
fn unary_real_op<F>(
    ctx: &mut rpl_lang::library::ExecuteContext,
    op: F,
) -> rpl_lang::library::ExecuteResult
where
    F: FnOnce(f64) -> Result<f64, &'static str>,
{
    use rpl_lang::library::ExecuteOk;

    let arg = match ctx.pop() {
        Ok(v) => v,
        Err(_) => return Err("Stack underflow".into()),
    };

    let n = match arg {
        Value::Real(r) => r,
        Value::Int(i) => i as f64,
        _ => return Err("Expected number".into()),
    };

    match op(n) {
        Ok(result) => {
            if ctx.push(Value::Real(result)).is_err() {
                return Err("Stack overflow".into());
            }
            Ok(ExecuteOk::Ok)
        }
        Err(msg) => Err(msg.into()),
    }
}

rpl_macros::define_library! {
    pub library TranscendentalsLib(66, "Transcendentals");

    custom probe {
        use rpl_core::token::{SemanticKind, TokenInfo};

        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        let len = match upper.as_str() {
            "SIN" | "COS" | "TAN" | "EXP" | "LOG" => 3,
            "ASIN" | "ACOS" | "ATAN" | "SINH" | "COSH" | "TANH" | "SQRT" | "EXPM" | "LNP1"
            | "ALOG" => 4,
            "ASINH" | "ACOSH" | "ATANH" | "ATAN2" => 5,
            "LN" | "PI" => 2,
            _ if text == "π" => {
                return rpl_lang::library::ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Command,
                };
            }
            _ => return rpl_lang::library::ProbeResult::NoMatch,
        };

        rpl_lang::library::ProbeResult::Match {
            info: TokenInfo::atom(len),
            semantic: SemanticKind::Command,
        }
    }

    // Hello
    custom compile {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        let cmd = match upper.as_str() {
            "SIN" => Self::CMD_SIN,
            "COS" => Self::CMD_COS,
            "TAN" => Self::CMD_TAN,
            "ASIN" => Self::CMD_ASIN,
            "ACOS" => Self::CMD_ACOS,
            "ATAN" => Self::CMD_ATAN,
            "ATAN2" => Self::CMD_ATAN2,
            "LN" => Self::CMD_LN,
            "EXP" => Self::CMD_EXP,
            "SINH" => Self::CMD_SINH,
            "COSH" => Self::CMD_COSH,
            "TANH" => Self::CMD_TANH,
            "ASINH" => Self::CMD_ASINH,
            "ACOSH" => Self::CMD_ACOSH,
            "ATANH" => Self::CMD_ATANH,
            "LOG" => Self::CMD_LOG,
            "ALOG" => Self::CMD_ALOG,
            "SQRT" => Self::CMD_SQRT,
            "EXPM" => Self::CMD_EXPM,
            "LNP1" => Self::CMD_LNP1,
            "PI" => Self::CMD_PI,
            _ if text == "π" => Self::CMD_PI,
            _ => return rpl_lang::library::CompileResult::NoMatch,
        };

        ctx.emit_opcode(Self::ID.as_u16(), cmd);
        rpl_lang::library::CompileResult::Ok
    }

    commands {
        // Trigonometric
        SIN (1 -> 1) "Sine" {
            unary_real_op(ctx, |n| Ok(n.sin()))
        }

        COS (1 -> 1) "Cosine" {
            unary_real_op(ctx, |n| Ok(n.cos()))
        }

        TAN (1 -> 1) "Tangent" {
            unary_real_op(ctx, |n| {
                let result = n.tan();
                if result.is_infinite() {
                    Err("Infinite result")
                } else {
                    Ok(result)
                }
            })
        }

        // Inverse trigonometric
        ASIN (1 -> 1) "Arc sine" {
            unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ASIN argument must be in [-1, 1]")
                } else {
                    Ok(n.asin())
                }
            })
        }

        ACOS (1 -> 1) "Arc cosine" {
            unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ACOS argument must be in [-1, 1]")
                } else {
                    Ok(n.acos())
                }
            })
        }

        ATAN (1 -> 1) "Arc tangent" {
            unary_real_op(ctx, |n| Ok(n.atan()))
        }

        ATAN2 (2 -> 1) "Arc tangent of y/x" {
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("Expected number".into()),
                Err(_) => return Err("Stack underflow".into()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("Expected number".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            if x == 0.0 && y == 0.0 {
                return Err("ATAN2 undefined for (0, 0)".into());
            }

            let result = y.atan2(x);
            if ctx.push(Value::Real(result)).is_err() {
                return Err("Stack overflow".into());
            }
            EXEC_OK
        }

        // Logarithmic
        LN (1 -> 1) "Natural logarithm" {
            unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LN argument must be positive")
                } else {
                    Ok(n.ln())
                }
            })
        }

        LOG (1 -> 1) "Base-10 logarithm" {
            unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LOG argument must be positive")
                } else {
                    Ok(n.log10())
                }
            })
        }

        LNP1 (1 -> 1) "Natural log of 1+x" {
            unary_real_op(ctx, |n| {
                if n <= -1.0 {
                    Err("Domain error: LNP1 argument must be > -1")
                } else {
                    Ok(n.ln_1p())
                }
            })
        }

        // Exponential
        EXP (1 -> 1) "Exponential (e^x)" {
            unary_real_op(ctx, |n| Ok(n.exp()))
        }

        ALOG (1 -> 1) "Base-10 antilog (10^x)" {
            unary_real_op(ctx, |n| Ok(10.0_f64.powf(n)))
        }

        EXPM (1 -> 1) "e^x - 1" {
            unary_real_op(ctx, |n| Ok(n.exp_m1()))
        }

        // Hyperbolic
        SINH (1 -> 1) "Hyperbolic sine" {
            unary_real_op(ctx, |n| Ok(n.sinh()))
        }

        COSH (1 -> 1) "Hyperbolic cosine" {
            unary_real_op(ctx, |n| Ok(n.cosh()))
        }

        TANH (1 -> 1) "Hyperbolic tangent" {
            unary_real_op(ctx, |n| Ok(n.tanh()))
        }

        ASINH (1 -> 1) "Inverse hyperbolic sine" {
            unary_real_op(ctx, |n| Ok(n.asinh()))
        }

        ACOSH (1 -> 1) "Inverse hyperbolic cosine" {
            unary_real_op(ctx, |n| {
                if n < 1.0 {
                    Err("Domain error: ACOSH argument must be >= 1")
                } else {
                    Ok(n.acosh())
                }
            })
        }

        ATANH (1 -> 1) "Inverse hyperbolic tangent" {
            unary_real_op(ctx, |n| {
                if n <= -1.0 || n >= 1.0 {
                    Err("Domain error: ATANH argument must be in (-1, 1)")
                } else {
                    Ok(n.atanh())
                }
            })
        }

        // Square root
        SQRT (1 -> 1) "Square root" {
            unary_real_op(ctx, |n| {
                if n < 0.0 {
                    Err("Domain error: SQRT argument must be non-negative")
                } else {
                    Ok(n.sqrt())
                }
            })
        }

        // Pi constant
        PI (0 -> 1) "Pi constant" {
            if ctx.push(Value::Real(std::f64::consts::PI)).is_err() {
                return Err("Stack overflow".into());
            }
            EXEC_OK
        }
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::{
        VM,
        library::{ExecuteOk, Library, ProbeContext, ProbeResult, StackEffect},
    };

    use super::*;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &mut VM, cmd: u16) -> rpl_lang::library::ExecuteContext<'_> {
        rpl_lang::library::ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn probe_trig_functions() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        for text in &["SIN", "COS", "TAN", "sin", "cos", "tan"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_inverse_trig() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        for text in &["ASIN", "ACOS", "ATAN", "ATAN2"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_hyperbolic() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        for text in &["SINH", "COSH", "TANH", "ASINH", "ACOSH", "ATANH"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_log_exp() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        for text in &["LN", "LOG", "EXP", "ALOG", "EXPM", "LNP1"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_sqrt_pi() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        for text in &["SQRT", "PI", "π"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = TranscendentalsLib;

        let ctx = make_probe_ctx("FOO", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn execute_sin_zero() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_SIN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn execute_cos_zero() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_COS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn execute_sqrt() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(16.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_SQRT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 4.0).abs() < 1e-10);
    }

    #[test]
    fn execute_sqrt_negative_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(-1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_SQRT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_ln() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(std::f64::consts::E)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_LN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn execute_ln_zero_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_LN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_exp() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_EXP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - std::f64::consts::E).abs() < 1e-10);
    }

    #[test]
    fn execute_log10() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(100.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_LOG);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn execute_alog() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(2.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ALOG);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - 100.0).abs() < 1e-10);
    }

    #[test]
    fn execute_pi() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_PI);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert!((vm.pop_real().unwrap() - std::f64::consts::PI).abs() < 1e-15);
    }

    #[test]
    fn execute_asin_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(2.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ASIN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_atan2() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap(); // y
        vm.push(Value::Real(1.0)).unwrap(); // x

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ATAN2);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        // atan2(1, 1) = π/4
        assert!((vm.pop_real().unwrap() - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
    }

    #[test]
    fn execute_atan2_zero_zero_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ATAN2);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_sinh_cosh_tanh() {
        let lib = TranscendentalsLib;

        // sinh(0) = 0
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_SINH);
        let _ = lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);

        // cosh(0) = 1
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_COSH);
        let _ = lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);

        // tanh(0) = 0
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_TANH);
        let _ = lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn execute_acosh_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.5)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ACOSH);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn execute_atanh_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, TranscendentalsLib::CMD_ATANH);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
    }

    #[test]
    fn stack_effect_unary() {
        let lib = TranscendentalsLib;

        for token in &[
            "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN", "LN", "EXP", "SINH", "COSH", "TANH",
            "ASINH", "ACOSH", "ATANH", "LOG", "ALOG", "SQRT", "EXPM", "LNP1",
        ] {
            let effect = lib.stack_effect(token);
            assert!(
                matches!(
                    effect,
                    StackEffect::Fixed {
                        consumes: 1,
                        produces: 1
                    }
                ),
                "Expected unary effect for {}",
                token
            );
        }
    }

    #[test]
    fn stack_effect_atan2() {
        let lib = TranscendentalsLib;
        let effect = lib.stack_effect("ATAN2");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        ));
    }

    #[test]
    fn stack_effect_pi() {
        let lib = TranscendentalsLib;
        let effect = lib.stack_effect("PI");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 0,
                produces: 1
            }
        ));
    }
}
