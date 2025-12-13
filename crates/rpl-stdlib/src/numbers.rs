//! Library for real numbers - literals and operator implementations.
//!
//! This library:
//! - Probes and compiles real number literals
//! - Registers and executes arithmetic operators for Real type

use rpl_core::{
    TypeId,
    token::{SemanticKind, TokenInfo},
};
use rpl_lang::{
    library::{
        CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
        ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
    },
    operator::{OpSignature, OperatorKind, OperatorRegistration, OperatorRegistry},
};

/// Library for real numbers.
pub struct RealNumbersLib;

impl RealNumbersLib {
    /// Library ID for real numbers (HP RPL Library 10).
    pub const ID: LibraryId = LibraryId::new(10);

    // Command IDs for arithmetic operator implementations
    const CMD_ADD: u16 = 0;
    const CMD_SUB: u16 = 1;
    const CMD_MUL: u16 = 2;
    const CMD_DIV: u16 = 3;
    const CMD_MOD: u16 = 4;
    const CMD_NEG: u16 = 5;
    const CMD_ABS: u16 = 6;
    const CMD_INV: u16 = 7;
    const CMD_POW: u16 = 8;

    // Command IDs for comparison operator implementations
    const CMD_LT: u16 = 10;
    const CMD_GT: u16 = 11;
    const CMD_LE: u16 = 12;
    const CMD_GE: u16 = 13;
    const CMD_EQ: u16 = 14;
    const CMD_NE: u16 = 15;
    const CMD_SAME: u16 = 16;

    // Command IDs for real number functions
    const CMD_SQ: u16 = 20; // Square: x^2
    const CMD_MIN: u16 = 21; // Minimum of two values
    const CMD_MAX: u16 = 22; // Maximum of two values
    const CMD_SIGN: u16 = 23; // Sign: -1, 0, or 1

    /// Get command ID from text (for real number functions).
    fn command_id(text: &str) -> Option<u16> {
        let upper = text.to_ascii_uppercase();
        match upper.as_str() {
            "SQ" => Some(Self::CMD_SQ),
            "MIN" => Some(Self::CMD_MIN),
            "MAX" => Some(Self::CMD_MAX),
            "SIGN" => Some(Self::CMD_SIGN),
            _ => None,
        }
    }
}

impl Library for RealNumbersLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "RealNumbers"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        // Try to parse as a real number
        if parse_real(text).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Number,
            }
        } else if Self::command_id(text).is_some() {
            // Recognize real number functions: SQ, MIN, MAX, SIGN
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

        if let Some(value) = parse_real(text) {
            // Emit prolog: type=REAL, size=2 (for f64 as 2 words)
            ctx.emit_prolog(TypeId::REAL.as_u16(), 2);
            // Emit the f64 as two 32-bit words (high word first, then low)
            let bits = value.to_bits();
            ctx.emit((bits >> 32) as u32);
            ctx.emit((bits & 0xFFFF_FFFF) as u32);
            CompileResult::Ok
        } else if let Some(cmd) = Self::command_id(text) {
            // Emit opcode for real number functions
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_ADD => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a + b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_SUB => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a - b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_MUL => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a * b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_DIV => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if b == 0.0 {
                    return ExecuteResult::Error("Division by zero".to_string());
                }
                if ctx.push_real(a / b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_MOD => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if b == 0.0 {
                    return ExecuteResult::Error("Division by zero".to_string());
                }
                if ctx.push_real(a % b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_NEG => {
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(-a).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_ABS => {
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a.abs()).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_INV => {
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if a == 0.0 {
                    return ExecuteResult::Error("Division by zero".to_string());
                }
                if ctx.push_real(1.0 / a).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_POW => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a.powf(b)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            // Comparison operators
            Self::CMD_LT => binary_compare(ctx, |a, b| a < b),
            Self::CMD_GT => binary_compare(ctx, |a, b| a > b),
            Self::CMD_LE => binary_compare(ctx, |a, b| a <= b),
            Self::CMD_GE => binary_compare(ctx, |a, b| a >= b),
            Self::CMD_EQ => binary_compare(ctx, |a, b| (a - b).abs() < f64::EPSILON),
            Self::CMD_NE => binary_compare(ctx, |a, b| (a - b).abs() >= f64::EPSILON),
            Self::CMD_SAME => binary_compare(ctx, |a, b| (a - b).abs() < f64::EPSILON),
            // Real number functions
            Self::CMD_SQ => {
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a * a).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_MIN => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a.min(b)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_MAX => {
                let b = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push_real(a.max(b)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_SIGN => {
                let a = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let sign = if a > 0.0 {
                    1.0
                } else if a < 0.0 {
                    -1.0
                } else {
                    0.0
                };
                if ctx.push_real(sign).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            _ => {
                // Number literals are handled by the VM's prolog decoder,
                // not via library execute calls
                ExecuteResult::Ok
            }
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check if it's a REAL literal
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::REAL.as_u16()
                {
                    ctx.read(); // consume prolog
                    // Read the two data words (high word first, then low)
                    let high = ctx.read().unwrap_or(0) as u64;
                    let low = ctx.read().unwrap_or(0) as u64;
                    let bits = (high << 32) | low;
                    let value = f64::from_bits(bits);
                    ctx.write(&format!("{}", value));
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
            DecompileMode::Call(cmd) => {
                // Decompile operator calls
                let symbol = match cmd {
                    Self::CMD_ADD => "+",
                    Self::CMD_SUB => "-",
                    Self::CMD_MUL => "*",
                    Self::CMD_DIV => "/",
                    Self::CMD_MOD => "MOD",
                    Self::CMD_NEG => "NEG",
                    Self::CMD_ABS => "ABS",
                    Self::CMD_INV => "INV",
                    Self::CMD_POW => "^",
                    // Comparison operators
                    Self::CMD_LT => "<",
                    Self::CMD_GT => ">",
                    Self::CMD_LE => "<=",
                    Self::CMD_GE => ">=",
                    Self::CMD_EQ => "==",
                    Self::CMD_NE => "!=",
                    Self::CMD_SAME => "SAME",
                    // Real number functions
                    Self::CMD_SQ => "SQ",
                    Self::CMD_MIN => "MIN",
                    Self::CMD_MAX => "MAX",
                    Self::CMD_SIGN => "SIGN",
                    _ => return DecompileResult::Unknown,
                };
                ctx.write(symbol);
                DecompileResult::Ok
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        // Check if it's a real number function
        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            "SQ" | "SIGN" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "MIN" | "MAX" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            _ => {
                // Numbers always push one value onto the stack
                StackEffect::Fixed {
                    consumes: 0,
                    produces: 1,
                }
            }
        }
    }

    fn register_operators(&self, registry: &mut OperatorRegistry) {
        // Register binary operators for Real type
        let binary_ops = [
            (OperatorKind::Add, Self::CMD_ADD, true), // commutative
            (OperatorKind::Sub, Self::CMD_SUB, false),
            (OperatorKind::Mul, Self::CMD_MUL, true), // commutative
            (OperatorKind::Div, Self::CMD_DIV, false),
            (OperatorKind::Mod, Self::CMD_MOD, false),
            (OperatorKind::Pow, Self::CMD_POW, false),
        ];

        for (kind, cmd, commutative) in binary_ops {
            registry.register_operator(OperatorRegistration {
                op: kind,
                signature: OpSignature::Symmetric(TypeId::REAL),
                result: TypeId::REAL,
                lib: Self::ID,
                command: cmd,
                priority: 100,
                commutative,
            });
        }

        // Register unary operators for Real type
        let unary_ops = [
            (OperatorKind::Neg, Self::CMD_NEG),
            (OperatorKind::Abs, Self::CMD_ABS),
            (OperatorKind::Inv, Self::CMD_INV),
        ];

        for (kind, cmd) in unary_ops {
            registry.register_operator(OperatorRegistration {
                op: kind,
                signature: OpSignature::Unary(TypeId::REAL),
                result: TypeId::REAL,
                lib: Self::ID,
                command: cmd,
                priority: 100,
                commutative: false,
            });
        }

        // Register comparison operators for Real type (return Real: 1.0 or 0.0)
        let comparison_ops = [
            (OperatorKind::Lt, Self::CMD_LT, false),
            (OperatorKind::Gt, Self::CMD_GT, false),
            (OperatorKind::Le, Self::CMD_LE, false),
            (OperatorKind::Ge, Self::CMD_GE, false),
            (OperatorKind::Eq, Self::CMD_EQ, true), // commutative
            (OperatorKind::Ne, Self::CMD_NE, true), // commutative
            (OperatorKind::Same, Self::CMD_SAME, true), // commutative
        ];

        for (kind, cmd, commutative) in comparison_ops {
            registry.register_operator(OperatorRegistration {
                op: kind,
                signature: OpSignature::Symmetric(TypeId::REAL),
                result: TypeId::REAL, // Returns 1.0 or 0.0
                lib: Self::ID,
                command: cmd,
                priority: 100,
                commutative,
            });
        }
    }
}

/// Helper function for binary comparison operations.
fn binary_compare<F>(ctx: &mut ExecuteContext, compare: F) -> ExecuteResult
where
    F: Fn(f64, f64) -> bool,
{
    match (ctx.pop_real(), ctx.pop_real()) {
        (Ok(b), Ok(a)) => {
            let result = if compare(a, b) { 1.0 } else { 0.0 };
            if ctx.push_real(result).is_err() {
                return ExecuteResult::Error("Stack overflow".to_string());
            }
            ExecuteResult::Ok
        }
        _ => ExecuteResult::Error("Stack underflow".to_string()),
    }
}

/// Parse a string as a real number.
fn parse_real(text: &str) -> Option<f64> {
    // Handle optional sign
    let text = text.trim();

    // Try standard float parsing
    if let Ok(value) = text.parse::<f64>() {
        // Check it's a valid number (not infinity or NaN from weird input)
        if value.is_finite()
            || text.eq_ignore_ascii_case("inf")
            || text.eq_ignore_ascii_case("-inf")
        {
            return Some(value);
        }
    }

    // Handle integers
    if let Ok(value) = text.parse::<i64>() {
        return Some(value as f64);
    }

    None
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
    fn probe_returns_number_semantic() {
        let interner = Interner::new();
        let lib = RealNumbersLib;

        let ctx = make_probe_ctx("42", &interner);
        if let ProbeResult::Match { semantic, .. } = lib.probe(&ctx) {
            assert_eq!(semantic, SemanticKind::Number);
        } else {
            panic!("expected match");
        }
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

        let lib = RealNumbersLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(4));

        let mut ctx = CompileContext::new(span, "3.14", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
        // Should have emitted: prolog + 2 data words = 3 words
        assert_eq!(ctx.position(), 3);
    }

    #[test]
    fn parse_real_integers() {
        assert_eq!(parse_real("0"), Some(0.0));
        assert_eq!(parse_real("123"), Some(123.0));
        assert_eq!(parse_real("-456"), Some(-456.0));
    }

    #[test]
    fn parse_real_floats() {
        assert_eq!(parse_real("3.15"), Some(3.15));
        assert_eq!(parse_real("-2.5"), Some(-2.5));
        assert_eq!(parse_real("0.001"), Some(0.001));
    }

    #[test]
    fn parse_real_scientific() {
        assert_eq!(parse_real("1e10"), Some(1e10));
        assert_eq!(parse_real("1.5e-3"), Some(1.5e-3));
    }

    #[test]
    fn parse_real_invalid() {
        assert_eq!(parse_real("abc"), None);
        assert_eq!(parse_real(""), None);
        assert_eq!(parse_real("1.2.3"), None);
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
        assert!(matches!(result, ExecuteResult::Ok));
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
        assert!(matches!(result, ExecuteResult::Ok));
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
        assert!(matches!(result, ExecuteResult::Ok));
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
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_real().unwrap(), 2.5);
    }

    #[test]
    fn execute_neg() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(5.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_NEG);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_real().unwrap(), -5.0);
    }

    #[test]
    fn execute_abs() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(-5.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_ABS);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_real().unwrap(), 5.0);
    }

    #[test]
    fn execute_inv() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();
        vm.push_real(4.0).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_INV);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
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
        assert!(matches!(result, ExecuteResult::Ok));
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
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_real().unwrap(), 1.0);
    }

    #[test]
    fn execute_underflow() {
        let lib = RealNumbersLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, RealNumbersLib::CMD_ADD);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
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
            ExecuteResult::Error(msg) => {
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
