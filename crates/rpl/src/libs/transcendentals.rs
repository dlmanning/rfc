//! Transcendental functions library.
//!
//! Provides mathematical functions:
//! - Trigonometric: SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2
//! - Hyperbolic: SINH, COSH, TANH, ASINH, ACOSH, ATANH
//! - Logarithmic: LN, LOG, LNP1
//! - Exponential: EXP, ALOG, EXPM
//! - Other: SQRT, PI
//!
//! Note: All trigonometric functions use radians.

use crate::core::Span;

use crate::{
    core::TypeId,
    ir::{Branch, LibId},
    libs::{ExecuteContext, ExecuteResult, Library, StackEffect},
    lower::{LowerContext, LowerError},
    types::CStack,
    value::Value,
    vm::bytecode::Opcode,
};

/// Transcendentals library ID (matches rpl-stdlib).
pub const TRANSCENDENTALS_LIB: LibId = 66;

/// Transcendentals library command IDs.
pub mod cmd {
    // Trigonometric
    pub const SIN: u16 = 0;
    pub const COS: u16 = 1;
    pub const TAN: u16 = 2;
    pub const ASIN: u16 = 3;
    pub const ACOS: u16 = 4;
    pub const ATAN: u16 = 5;
    pub const ATAN2: u16 = 6;

    // Hyperbolic
    pub const SINH: u16 = 10;
    pub const COSH: u16 = 11;
    pub const TANH: u16 = 12;
    pub const ASINH: u16 = 13;
    pub const ACOSH: u16 = 14;
    pub const ATANH: u16 = 15;

    // Logarithmic
    pub const LN: u16 = 20;
    pub const LOG: u16 = 21;
    pub const LNP1: u16 = 22;

    // Exponential
    pub const EXP: u16 = 30;
    pub const ALOG: u16 = 31;
    pub const EXPM: u16 = 32;

    // Other
    pub const SQRT: u16 = 40;
    pub const PI: u16 = 41;

    // Rounding
    pub const CEIL: u16 = 50;
    pub const FLOOR: u16 = 51;
    pub const IP: u16 = 52;  // Integer part (truncate toward zero)
    pub const FP: u16 = 53;  // Fractional part
}

/// Transcendentals library.
#[derive(Clone, Copy)]
pub struct TranscendentalsLib;

impl Library for TranscendentalsLib {
    fn id(&self) -> LibId {
        TRANSCENDENTALS_LIB
    }

    fn name(&self) -> &'static str {
        "Transcendentals"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            // Trigonometric (1 -> 1)
            CommandInfo::with_effect("SIN", TRANSCENDENTALS_LIB, cmd::SIN, 1, 1),
            CommandInfo::with_effect("COS", TRANSCENDENTALS_LIB, cmd::COS, 1, 1),
            CommandInfo::with_effect("TAN", TRANSCENDENTALS_LIB, cmd::TAN, 1, 1),
            CommandInfo::with_effect("ASIN", TRANSCENDENTALS_LIB, cmd::ASIN, 1, 1),
            CommandInfo::with_effect("ACOS", TRANSCENDENTALS_LIB, cmd::ACOS, 1, 1),
            CommandInfo::with_effect("ATAN", TRANSCENDENTALS_LIB, cmd::ATAN, 1, 1),
            CommandInfo::with_effect("ATAN2", TRANSCENDENTALS_LIB, cmd::ATAN2, 2, 1),
            // Hyperbolic (1 -> 1)
            CommandInfo::with_effect("SINH", TRANSCENDENTALS_LIB, cmd::SINH, 1, 1),
            CommandInfo::with_effect("COSH", TRANSCENDENTALS_LIB, cmd::COSH, 1, 1),
            CommandInfo::with_effect("TANH", TRANSCENDENTALS_LIB, cmd::TANH, 1, 1),
            CommandInfo::with_effect("ASINH", TRANSCENDENTALS_LIB, cmd::ASINH, 1, 1),
            CommandInfo::with_effect("ACOSH", TRANSCENDENTALS_LIB, cmd::ACOSH, 1, 1),
            CommandInfo::with_effect("ATANH", TRANSCENDENTALS_LIB, cmd::ATANH, 1, 1),
            // Logarithmic (1 -> 1)
            CommandInfo::with_effect("LN", TRANSCENDENTALS_LIB, cmd::LN, 1, 1),
            CommandInfo::with_effect("LOG", TRANSCENDENTALS_LIB, cmd::LOG, 1, 1),
            CommandInfo::with_effect("LNP1", TRANSCENDENTALS_LIB, cmd::LNP1, 1, 1),
            // Exponential (1 -> 1)
            CommandInfo::with_effect("EXP", TRANSCENDENTALS_LIB, cmd::EXP, 1, 1),
            CommandInfo::with_effect("ALOG", TRANSCENDENTALS_LIB, cmd::ALOG, 1, 1),
            CommandInfo::with_effect("EXPM", TRANSCENDENTALS_LIB, cmd::EXPM, 1, 1),
            // Other
            CommandInfo::with_effect("SQRT", TRANSCENDENTALS_LIB, cmd::SQRT, 1, 1),
            CommandInfo::with_effect("PI", TRANSCENDENTALS_LIB, cmd::PI, 0, 1),
            CommandInfo::with_effect("π", TRANSCENDENTALS_LIB, cmd::PI, 0, 1),
            // Rounding
            CommandInfo::with_effect("CEIL", TRANSCENDENTALS_LIB, cmd::CEIL, 1, 1),
            CommandInfo::with_effect("FLOOR", TRANSCENDENTALS_LIB, cmd::FLOOR, 1, 1),
            CommandInfo::with_effect("IP", TRANSCENDENTALS_LIB, cmd::IP, 1, 1),
            CommandInfo::with_effect("FP", TRANSCENDENTALS_LIB, cmd::FP, 1, 1),
        ]
    }

    fn lower_composite(
        &self,
        _id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError { span: None,
            message: "Transcendentals library has no composites".into(),
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // Note: While WASM has F64Sqrt as a native opcode, we use the library call
        // for domain checking (negative numbers should error, not return NaN).
        match cmd {
            cmd::SQRT => {
                // Use library call for domain checking
                ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
            }
            cmd::PI => {
                // Push pi constant
                ctx.output.emit_f64_const(std::f64::consts::PI);
            }
            cmd::ATAN2 => {
                // Binary operation (y x -- result)
                ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
            }
            // Rounding operations - use native opcodes for reals
            cmd::CEIL => {
                let tos = ctx.types.top();
                if tos.is_real() {
                    ctx.output.emit_opcode(Opcode::F64Ceil);
                } else if !tos.is_integer() {
                    ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
                }
                // Integer is already "ceiled" - no-op
            }
            cmd::FLOOR => {
                let tos = ctx.types.top();
                if tos.is_real() {
                    ctx.output.emit_opcode(Opcode::F64Floor);
                } else if !tos.is_integer() {
                    ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
                }
                // Integer is already "floored" - no-op
            }
            cmd::IP => {
                // Integer part (truncate toward zero)
                let tos = ctx.types.top();
                if tos.is_real() {
                    ctx.output.emit_opcode(Opcode::F64Trunc);
                } else if !tos.is_integer() {
                    ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
                }
                // Integer is its own integer part - no-op
            }
            cmd::FP => {
                // Fractional part: x - trunc(x)
                // For integers, always 0
                let tos = ctx.types.top();
                if tos.is_integer() {
                    // Drop the integer, push 0
                    ctx.output.emit_opcode(Opcode::Drop);
                    ctx.output.emit_i64_const(0);
                } else {
                    ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
                }
            }
            // All other transcendentals use library call
            _ => {
                ctx.output.emit_call_lib(TRANSCENDENTALS_LIB, cmd);
            }
        }
        Ok(())
    }

    fn command_effect(&self, cmd: u16, types: &CStack) -> StackEffect {
        match cmd {
            cmd::PI => StackEffect::fixed(0, &[Some(TypeId::REAL)]),
            cmd::ATAN2 => StackEffect::fixed(2, &[Some(TypeId::REAL)]),
            // Rounding operations preserve integer type
            cmd::CEIL | cmd::FLOOR | cmd::IP => {
                let tos = types.top();
                if tos.is_integer() {
                    StackEffect::fixed(1, &[Some(TypeId::BINT)])
                } else {
                    StackEffect::fixed(1, &[Some(TypeId::REAL)])
                }
            }
            cmd::FP => {
                let tos = types.top();
                if tos.is_integer() {
                    StackEffect::fixed(1, &[Some(TypeId::BINT)])
                } else {
                    StackEffect::fixed(1, &[Some(TypeId::REAL)])
                }
            }
            // All other transcendentals are unary (1 -> 1) and produce Real
            _ => StackEffect::fixed(1, &[Some(TypeId::REAL)]),
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            // Trigonometric
            cmd::SIN => unary_real_op(ctx, |n| Ok(n.sin())),
            cmd::COS => unary_real_op(ctx, |n| Ok(n.cos())),
            cmd::TAN => unary_real_op(ctx, |n| {
                let result = n.tan();
                if result.is_infinite() {
                    Err("Infinite result")
                } else {
                    Ok(result)
                }
            }),
            cmd::ASIN => unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ASIN argument must be in [-1, 1]")
                } else {
                    Ok(n.asin())
                }
            }),
            cmd::ACOS => unary_real_op(ctx, |n| {
                if !(-1.0..=1.0).contains(&n) {
                    Err("Domain error: ACOS argument must be in [-1, 1]")
                } else {
                    Ok(n.acos())
                }
            }),
            cmd::ATAN => unary_real_op(ctx, |n| Ok(n.atan())),
            cmd::ATAN2 => {
                let x = pop_real(ctx)?;
                let y = pop_real(ctx)?;
                if x == 0.0 && y == 0.0 {
                    return Err("ATAN2 undefined for (0, 0)".into());
                }
                ctx.push(Value::Real(y.atan2(x)))?;
                Ok(())
            }

            // Hyperbolic
            cmd::SINH => unary_real_op(ctx, |n| Ok(n.sinh())),
            cmd::COSH => unary_real_op(ctx, |n| Ok(n.cosh())),
            cmd::TANH => unary_real_op(ctx, |n| Ok(n.tanh())),
            cmd::ASINH => unary_real_op(ctx, |n| Ok(n.asinh())),
            cmd::ACOSH => unary_real_op(ctx, |n| {
                if n < 1.0 {
                    Err("Domain error: ACOSH argument must be >= 1")
                } else {
                    Ok(n.acosh())
                }
            }),
            cmd::ATANH => unary_real_op(ctx, |n| {
                if n <= -1.0 || n >= 1.0 {
                    Err("Domain error: ATANH argument must be in (-1, 1)")
                } else {
                    Ok(n.atanh())
                }
            }),

            // Logarithmic
            cmd::LN => unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LN argument must be positive")
                } else {
                    Ok(n.ln())
                }
            }),
            cmd::LOG => unary_real_op(ctx, |n| {
                if n <= 0.0 {
                    Err("Domain error: LOG argument must be positive")
                } else {
                    Ok(n.log10())
                }
            }),
            cmd::LNP1 => unary_real_op(ctx, |n| {
                if n <= -1.0 {
                    Err("Domain error: LNP1 argument must be > -1")
                } else {
                    Ok(n.ln_1p())
                }
            }),

            // Exponential
            cmd::EXP => unary_real_op(ctx, |n| Ok(n.exp())),
            cmd::ALOG => unary_real_op(ctx, |n| Ok(10.0_f64.powf(n))),
            cmd::EXPM => unary_real_op(ctx, |n| Ok(n.exp_m1())),

            // Other
            cmd::SQRT => unary_real_op(ctx, |n| {
                if n < 0.0 {
                    Err("Domain error: SQRT argument must be non-negative")
                } else {
                    Ok(n.sqrt())
                }
            }),
            cmd::PI => {
                ctx.push(Value::Real(std::f64::consts::PI))?;
                Ok(())
            }

            // Rounding
            cmd::CEIL => unary_real_op(ctx, |n| Ok(n.ceil())),
            cmd::FLOOR => unary_real_op(ctx, |n| Ok(n.floor())),
            cmd::IP => unary_real_op(ctx, |n| Ok(n.trunc())),
            cmd::FP => unary_real_op(ctx, |n| Ok(n.fract())),

            _ => Err(format!("Unknown transcendentals command: {}", ctx.cmd)),
        }
    }
}

// Execution helpers

fn pop_real(ctx: &mut ExecuteContext) -> Result<f64, String> {
    match ctx.pop()? {
        Value::Real(r) => Ok(r),
        Value::Integer(i) => Ok(i as f64),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

fn unary_real_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(f64) -> Result<f64, &'static str>,
{
    let n = pop_real(ctx)?;
    match op(n) {
        Ok(result) => {
            ctx.push(Value::Real(result))?;
            Ok(())
        }
        Err(msg) => Err(msg.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::Library;

    #[test]
    fn transcendentals_lib_id() {
        assert_eq!(TranscendentalsLib.id(), 66);
    }

    #[test]
    fn transcendentals_lib_name() {
        assert_eq!(TranscendentalsLib.name(), "Transcendentals");
    }

    #[test]
    fn commands_registered() {
        let lib = TranscendentalsLib;
        let commands = lib.commands();
        let names: Vec<_> = commands.iter().map(|c| c.name).collect();

        // Check key commands are registered
        assert!(names.contains(&"SIN"));
        assert!(names.contains(&"COS"));
        assert!(names.contains(&"TAN"));
        assert!(names.contains(&"SQRT"));
        assert!(names.contains(&"PI"));
        assert!(names.contains(&"π"));
        assert!(names.contains(&"LN"));
        assert!(names.contains(&"EXP"));
    }
}
