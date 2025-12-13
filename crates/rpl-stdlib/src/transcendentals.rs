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

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::{
    library::{
        CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
        ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
    },
    Value,
};

/// Library for transcendental functions.
pub struct TranscendentalsLib;

// Command IDs
const CMD_SIN: u16 = 0;
const CMD_COS: u16 = 1;
const CMD_TAN: u16 = 2;
const CMD_ASIN: u16 = 3;
const CMD_ACOS: u16 = 4;
const CMD_ATAN: u16 = 5;
const CMD_ATAN2: u16 = 6;
const CMD_LN: u16 = 7;
const CMD_EXP: u16 = 8;
const CMD_SINH: u16 = 9;
const CMD_COSH: u16 = 10;
const CMD_TANH: u16 = 11;
const CMD_ASINH: u16 = 12;
const CMD_ACOSH: u16 = 13;
const CMD_ATANH: u16 = 14;
const CMD_LOG: u16 = 15;
const CMD_ALOG: u16 = 16;
const CMD_SQRT: u16 = 17;
const CMD_EXPM: u16 = 18;
const CMD_LNP1: u16 = 19;
const CMD_PI: u16 = 20;

impl TranscendentalsLib {
    /// Library ID for transcendentals (HP RPL Library 66).
    pub const ID: LibraryId = LibraryId::new(66);
}

/// Helper for unary operations on real numbers.
fn unary_real_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(f64) -> Result<f64, &'static str>,
{
    let arg = match ctx.pop() {
        Ok(v) => v,
        Err(_) => return ExecuteResult::Error("Stack underflow".into()),
    };

    let n = match arg {
        Value::Real(r) => r,
        Value::Int(i) => i as f64,
        _ => return ExecuteResult::Error("Expected number".into()),
    };

    match op(n) {
        Ok(result) => {
            if ctx.push(Value::Real(result)).is_err() {
                return ExecuteResult::Error("Stack overflow".into());
            }
            ExecuteResult::Ok
        }
        Err(msg) => ExecuteResult::Error(msg.into()),
    }
}

impl Library for TranscendentalsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Transcendentals"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        let len = match upper.as_str() {
            "SIN" | "COS" | "TAN" | "EXP" | "LOG" => 3,
            "ASIN" | "ACOS" | "ATAN" | "SINH" | "COSH" | "TANH" | "SQRT" | "EXPM" | "LNP1"
            | "ALOG" => 4,
            "ASINH" | "ACOSH" | "ATANH" | "ATAN2" => 5,
            "LN" | "PI" => 2,
            _ if text == "π" => {
                return ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Command,
                };
            }
            _ => return ProbeResult::NoMatch,
        };

        ProbeResult::Match {
            info: TokenInfo::atom(len),
            semantic: SemanticKind::Command,
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        let cmd = match upper.as_str() {
            "SIN" => CMD_SIN,
            "COS" => CMD_COS,
            "TAN" => CMD_TAN,
            "ASIN" => CMD_ASIN,
            "ACOS" => CMD_ACOS,
            "ATAN" => CMD_ATAN,
            "ATAN2" => CMD_ATAN2,
            "LN" => CMD_LN,
            "EXP" => CMD_EXP,
            "SINH" => CMD_SINH,
            "COSH" => CMD_COSH,
            "TANH" => CMD_TANH,
            "ASINH" => CMD_ASINH,
            "ACOSH" => CMD_ACOSH,
            "ATANH" => CMD_ATANH,
            "LOG" => CMD_LOG,
            "ALOG" => CMD_ALOG,
            "SQRT" => CMD_SQRT,
            "EXPM" => CMD_EXPM,
            "LNP1" => CMD_LNP1,
            "PI" => CMD_PI,
            _ if text == "π" => CMD_PI,
            _ => return CompileResult::NoMatch,
        };

        ctx.emit_opcode(Self::ID.as_u16(), cmd);
        CompileResult::Ok
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            // Trigonometric
            CMD_SIN => unary_real_op(ctx, |n| Ok(n.sin())),
            CMD_COS => unary_real_op(ctx, |n| Ok(n.cos())),
            CMD_TAN => unary_real_op(ctx, |n| {
                let result = n.tan();
                if result.is_infinite() {
                    Err("Infinite result")
                } else {
                    Ok(result)
                }
            }),

            // Inverse trigonometric
            CMD_ASIN => unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ASIN argument must be in [-1, 1]")
                } else {
                    Ok(n.asin())
                }
            }),
            CMD_ACOS => unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ACOS argument must be in [-1, 1]")
                } else {
                    Ok(n.acos())
                }
            }),
            CMD_ATAN => unary_real_op(ctx, |n| Ok(n.atan())),

            CMD_ATAN2 => {
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("Expected number".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("Expected number".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                if x == 0.0 && y == 0.0 {
                    return ExecuteResult::Error("ATAN2 undefined for (0, 0)".into());
                }

                let result = y.atan2(x);
                if ctx.push(Value::Real(result)).is_err() {
                    return ExecuteResult::Error("Stack overflow".into());
                }
                ExecuteResult::Ok
            }

            // Logarithmic
            CMD_LN => unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LN argument must be positive")
                } else {
                    Ok(n.ln())
                }
            }),
            CMD_LOG => unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LOG argument must be positive")
                } else {
                    Ok(n.log10())
                }
            }),
            CMD_LNP1 => unary_real_op(ctx, |n| {
                if n <= -1.0 {
                    Err("Domain error: LNP1 argument must be > -1")
                } else {
                    Ok(n.ln_1p())
                }
            }),

            // Exponential
            CMD_EXP => unary_real_op(ctx, |n| Ok(n.exp())),
            CMD_ALOG => unary_real_op(ctx, |n| Ok(10.0_f64.powf(n))),
            CMD_EXPM => unary_real_op(ctx, |n| Ok(n.exp_m1())),

            // Hyperbolic
            CMD_SINH => unary_real_op(ctx, |n| Ok(n.sinh())),
            CMD_COSH => unary_real_op(ctx, |n| Ok(n.cosh())),
            CMD_TANH => unary_real_op(ctx, |n| Ok(n.tanh())),
            CMD_ASINH => unary_real_op(ctx, |n| Ok(n.asinh())),
            CMD_ACOSH => unary_real_op(ctx, |n| {
                if n < 1.0 {
                    Err("Domain error: ACOSH argument must be >= 1")
                } else {
                    Ok(n.acosh())
                }
            }),
            CMD_ATANH => unary_real_op(ctx, |n| {
                if n <= -1.0 || n >= 1.0 {
                    Err("Domain error: ATANH argument must be in (-1, 1)")
                } else {
                    Ok(n.atanh())
                }
            }),

            // Square root
            CMD_SQRT => unary_real_op(ctx, |n| {
                if n < 0.0 {
                    Err("Domain error: SQRT argument must be non-negative")
                } else {
                    Ok(n.sqrt())
                }
            }),

            // Pi constant
            CMD_PI => {
                if ctx.push(Value::Real(std::f64::consts::PI)).is_err() {
                    return ExecuteResult::Error("Stack overflow".into());
                }
                ExecuteResult::Ok
            }

            _ => ExecuteResult::Error(format!("Unknown transcendental command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        let cmd = match ctx.mode() {
            DecompileMode::Prolog => return DecompileResult::Unknown,
            DecompileMode::Call(cmd) => cmd,
        };

        let name = match cmd {
            CMD_SIN => "SIN",
            CMD_COS => "COS",
            CMD_TAN => "TAN",
            CMD_ASIN => "ASIN",
            CMD_ACOS => "ACOS",
            CMD_ATAN => "ATAN",
            CMD_ATAN2 => "ATAN2",
            CMD_LN => "LN",
            CMD_EXP => "EXP",
            CMD_SINH => "SINH",
            CMD_COSH => "COSH",
            CMD_TANH => "TANH",
            CMD_ASINH => "ASINH",
            CMD_ACOSH => "ACOSH",
            CMD_ATANH => "ATANH",
            CMD_LOG => "LOG",
            CMD_ALOG => "ALOG",
            CMD_SQRT => "SQRT",
            CMD_EXPM => "EXPM",
            CMD_LNP1 => "LNP1",
            CMD_PI => "PI",
            _ => return DecompileResult::Unknown,
        };

        ctx.write(name);
        DecompileResult::Ok
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            // Unary functions: consume 1, produce 1
            "SIN" | "COS" | "TAN" | "ASIN" | "ACOS" | "ATAN" | "LN" | "EXP" | "SINH" | "COSH"
            | "TANH" | "ASINH" | "ACOSH" | "ATANH" | "LOG" | "ALOG" | "SQRT" | "EXPM" | "LNP1" => {
                StackEffect::Fixed {
                    consumes: 1,
                    produces: 1,
                }
            }

            // Binary function: consume 2, produce 1
            "ATAN2" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },

            // Constant: consume 0, produce 1
            "PI" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },

            _ if token == "π" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },

            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span};
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

        let mut ctx = make_exec_ctx(&mut vm, CMD_SIN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn execute_cos_zero() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_COS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn execute_sqrt() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(16.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_SQRT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 4.0).abs() < 1e-10);
    }

    #[test]
    fn execute_sqrt_negative_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(-1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_SQRT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_ln() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(std::f64::consts::E)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_LN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn execute_ln_zero_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_LN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_exp() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_EXP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - std::f64::consts::E).abs() < 1e-10);
    }

    #[test]
    fn execute_log10() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(100.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_LOG);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn execute_alog() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(2.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_ALOG);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - 100.0).abs() < 1e-10);
    }

    #[test]
    fn execute_pi() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, CMD_PI);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert!((vm.pop_real().unwrap() - std::f64::consts::PI).abs() < 1e-15);
    }

    #[test]
    fn execute_asin_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(2.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_ASIN);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_atan2() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap(); // y
        vm.push(Value::Real(1.0)).unwrap(); // x

        let mut ctx = make_exec_ctx(&mut vm, CMD_ATAN2);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        // atan2(1, 1) = π/4
        assert!((vm.pop_real().unwrap() - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
    }

    #[test]
    fn execute_atan2_zero_zero_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        vm.push(Value::Real(0.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_ATAN2);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_sinh_cosh_tanh() {
        let lib = TranscendentalsLib;

        // sinh(0) = 0
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, CMD_SINH);
        lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);

        // cosh(0) = 1
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, CMD_COSH);
        lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 1.0).abs() < 1e-10);

        // tanh(0) = 0
        let mut vm = VM::new();
        vm.push(Value::Real(0.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, CMD_TANH);
        lib.execute(&mut ctx);
        assert!((vm.pop_real().unwrap() - 0.0).abs() < 1e-10);
    }

    #[test]
    fn execute_acosh_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(0.5)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_ACOSH);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_atanh_domain_error() {
        let lib = TranscendentalsLib;
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, CMD_ATANH);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
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
