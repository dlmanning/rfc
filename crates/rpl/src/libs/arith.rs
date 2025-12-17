//! Arithmetic and comparison operations library.
//!
//! Provides arithmetic commands:
//! - +, -, *, / (binary)
//! - NEG, INV (unary)
//! - MOD (modulo)
//!
//! Provides comparison commands:
//! - ==, != (equality)
//! - <, <=, >, >= (ordering)

use std::cmp::Ordering;

use crate::core::Span;

use crate::{
    core::TypeId,
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, Library, StackEffect},
    lower::{LowerContext, LowerError},
    types::CStack,
    value::Value,
};

/// Arithmetic library ID (matches rpl-stdlib).
pub const ARITH_LIB: LibId = 64;

/// Arithmetic library command IDs.
pub mod cmd {
    // Arithmetic operations
    pub const ADD: u16 = 0;
    pub const SUB: u16 = 1;
    pub const MUL: u16 = 2;
    pub const DIV: u16 = 3;
    pub const NEG: u16 = 4;
    pub const INV: u16 = 5;
    pub const MOD: u16 = 6;
    pub const ABS: u16 = 7;

    // Comparison operations
    pub const EQ: u16 = 10;
    pub const NE: u16 = 11;
    pub const LT: u16 = 12;
    pub const LE: u16 = 13;
    pub const GT: u16 = 14;
    pub const GE: u16 = 15;

    // Additional arithmetic operations
    pub const POW: u16 = 16;
    pub const MIN: u16 = 17;
    pub const MAX: u16 = 18;
    pub const SIGN: u16 = 19;
    pub const SQ: u16 = 20;
}

/// Arithmetic operations library.
#[derive(Clone, Copy)]
pub struct ArithLib;

impl Library for ArithLib {
    fn id(&self) -> LibId {
        ARITH_LIB
    }

    fn name(&self) -> &'static str {
        "Arithmetic"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            // Arithmetic operations (2 -> 1)
            CommandInfo::with_effect("+", ARITH_LIB, cmd::ADD, 2, 1),
            CommandInfo::with_effect("-", ARITH_LIB, cmd::SUB, 2, 1),
            CommandInfo::with_effect("*", ARITH_LIB, cmd::MUL, 2, 1),
            CommandInfo::with_effect("/", ARITH_LIB, cmd::DIV, 2, 1),
            CommandInfo::with_effect("NEG", ARITH_LIB, cmd::NEG, 1, 1),
            CommandInfo::with_effect("INV", ARITH_LIB, cmd::INV, 1, 1),
            CommandInfo::with_effect("MOD", ARITH_LIB, cmd::MOD, 2, 1),
            CommandInfo::with_effect("ABS", ARITH_LIB, cmd::ABS, 1, 1),
            // Comparison operations (2 -> 1)
            CommandInfo::with_effect("==", ARITH_LIB, cmd::EQ, 2, 1),
            CommandInfo::with_effect("!=", ARITH_LIB, cmd::NE, 2, 1),
            CommandInfo::with_effect("<", ARITH_LIB, cmd::LT, 2, 1),
            CommandInfo::with_effect("<=", ARITH_LIB, cmd::LE, 2, 1),
            CommandInfo::with_effect(">", ARITH_LIB, cmd::GT, 2, 1),
            CommandInfo::with_effect(">=", ARITH_LIB, cmd::GE, 2, 1),
            CommandInfo::with_effect("SAME", ARITH_LIB, cmd::EQ, 2, 1), // HP-style alias
            CommandInfo::with_effect("<>", ARITH_LIB, cmd::NE, 2, 1),   // HP-style not equal
            // Additional arithmetic
            CommandInfo::with_effect("^", ARITH_LIB, cmd::POW, 2, 1),
            CommandInfo::with_effect("MIN", ARITH_LIB, cmd::MIN, 2, 1),
            CommandInfo::with_effect("MAX", ARITH_LIB, cmd::MAX, 2, 1),
            CommandInfo::with_effect("SIGN", ARITH_LIB, cmd::SIGN, 1, 1),
            CommandInfo::with_effect("SQ", ARITH_LIB, cmd::SQ, 1, 1),
        ]
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        use crate::vm::bytecode::Opcode;

        // Emit bytecode based on command - effect is determined by command_effect
        match cmd {
            cmd::ADD => ctx.emit_binary_numeric(Opcode::I64Add, Opcode::F64Add, ARITH_LIB, cmd),
            cmd::SUB => ctx.emit_binary_numeric(Opcode::I64Sub, Opcode::F64Sub, ARITH_LIB, cmd),
            cmd::MUL => ctx.emit_binary_numeric(Opcode::I64Mul, Opcode::F64Mul, ARITH_LIB, cmd),
            cmd::DIV => ctx.emit_binary_numeric(Opcode::I64DivS, Opcode::F64Div, ARITH_LIB, cmd),
            cmd::NEG => ctx.emit_unary_numeric(None, Opcode::F64Neg, ARITH_LIB, cmd),
            cmd::ABS => {
                // For reals, use native F64Abs; for others, use library call
                let tos = ctx.types.top();
                if tos.is_real() {
                    ctx.output.emit_opcode(Opcode::F64Abs);
                } else {
                    ctx.output.emit_call_lib(ARITH_LIB, cmd);
                }
            }
            cmd::EQ => ctx.emit_binary_comparison(Opcode::I64Eq, Opcode::F64Eq, ARITH_LIB, cmd),
            cmd::NE => ctx.emit_binary_comparison(Opcode::I64Ne, Opcode::F64Ne, ARITH_LIB, cmd),
            cmd::LT => ctx.emit_binary_comparison(Opcode::I64LtS, Opcode::F64Lt, ARITH_LIB, cmd),
            cmd::LE => ctx.emit_binary_comparison(Opcode::I64LeS, Opcode::F64Le, ARITH_LIB, cmd),
            cmd::GT => ctx.emit_binary_comparison(Opcode::I64GtS, Opcode::F64Gt, ARITH_LIB, cmd),
            cmd::GE => ctx.emit_binary_comparison(Opcode::I64GeS, Opcode::F64Ge, ARITH_LIB, cmd),
            _ => {
                // All other commands use library call
                ctx.output.emit_call_lib(ARITH_LIB, cmd);
            }
        }

        Ok(())
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::ADD => add_op(ctx),
            cmd::SUB => binary_numeric_op(ctx, |a, b| a - b, |a, b| a - b),
            cmd::MUL => binary_numeric_op(ctx, |a, b| a * b, |a, b| a * b),
            cmd::DIV => binary_real_op(ctx, |a, b| a / b),
            cmd::NEG => unary_numeric_op(ctx, |a| -a, |a| -a),
            cmd::ABS => unary_numeric_op(ctx, |a| a.abs(), |a| a.abs()),
            cmd::INV => unary_real_op(ctx, |a| 1.0 / a),
            cmd::MOD => mod_op(ctx),
            cmd::EQ => compare_op(ctx, |ord| ord == Ordering::Equal),
            cmd::NE => compare_op(ctx, |ord| ord != Ordering::Equal),
            cmd::LT => compare_op(ctx, |ord| ord == Ordering::Less),
            cmd::LE => compare_op(ctx, |ord| ord != Ordering::Greater),
            cmd::GT => compare_op(ctx, |ord| ord == Ordering::Greater),
            cmd::GE => compare_op(ctx, |ord| ord != Ordering::Less),
            cmd::POW => binary_real_op(ctx, |a, b| a.powf(b)),
            cmd::MIN => binary_numeric_op(ctx, |a, b| a.min(b), |a, b| a.min(b)),
            cmd::MAX => binary_numeric_op(ctx, |a, b| a.max(b), |a, b| a.max(b)),
            cmd::SIGN => sign_op(ctx),
            cmd::SQ => unary_numeric_op(ctx, |a| a * a, |a| a * a),
            _ => Err(format!("Unknown arith command: {}", ctx.cmd)),
        }
    }

    fn command_effect(&self, cmd: u16, types: &CStack) -> StackEffect {
        // Use shared effect computation helpers from libs/mod.rs
        use super::{binary_numeric_effect, unary_preserving_effect};

        match cmd {
            // Binary numeric: int+int→int, otherwise→real
            cmd::ADD | cmd::SUB | cmd::MUL | cmd::DIV => {
                binary_numeric_effect(types)
            }
            // Comparisons always produce integer (0 or 1)
            cmd::EQ | cmd::NE | cmd::LT | cmd::LE | cmd::GT | cmd::GE => {
                StackEffect::fixed(2, &[Some(TypeId::BINT)])
            }
            // Unary ops that preserve type
            cmd::NEG | cmd::ABS | cmd::SQ => {
                unary_preserving_effect(types)
            }
            // INV always produces real
            cmd::INV => StackEffect::fixed(1, &[Some(TypeId::REAL)]),
            // MOD preserves numeric type
            cmd::MOD => binary_numeric_effect(types),
            // POW always produces real
            cmd::POW => StackEffect::fixed(2, &[Some(TypeId::REAL)]),
            // MIN/MAX preserve type
            cmd::MIN | cmd::MAX => binary_numeric_effect(types),
            // SIGN always returns integer (-1, 0, 1)
            cmd::SIGN => StackEffect::fixed(1, &[Some(TypeId::BINT)]),
            _ => StackEffect::Dynamic,
        }
    }
}

// Execution helpers

/// Addition with support for lists and strings.
fn add_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let result = match (a, b) {
        // Numeric addition
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
        (Value::Real(a), Value::Real(b)) => Value::Real(a + b),
        (Value::Integer(a), Value::Real(b)) => Value::Real(a as f64 + b),
        (Value::Real(a), Value::Integer(b)) => Value::Real(a + b as f64),
        // List concatenation
        (Value::List(a), Value::List(b)) => {
            let mut result: Vec<Value> = a.iter().cloned().collect();
            result.extend(b.iter().cloned());
            Value::list(result)
        }
        // List append (list + element)
        (Value::List(a), elem) => {
            let mut result: Vec<Value> = a.iter().cloned().collect();
            result.push(elem);
            Value::list(result)
        }
        // List prepend (element + list)
        (elem, Value::List(b)) => {
            let mut result = vec![elem];
            result.extend(b.iter().cloned());
            Value::list(result)
        }
        // String concatenation
        (Value::String(a), Value::String(b)) => {
            Value::string(format!("{}{}", a.as_ref(), b.as_ref()))
        }
        _ => return Err("Type error: expected numbers, lists, or strings".into()),
    };
    ctx.push(result)?;
    Ok(())
}

fn binary_numeric_op<Fi, Fr>(ctx: &mut ExecuteContext, int_op: Fi, real_op: Fr) -> ExecuteResult
where
    Fi: FnOnce(i64, i64) -> i64,
    Fr: FnOnce(f64, f64) -> f64,
{
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let result = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(int_op(a, b)),
        (Value::Real(a), Value::Real(b)) => Value::Real(real_op(a, b)),
        (Value::Integer(a), Value::Real(b)) => Value::Real(real_op(a as f64, b)),
        (Value::Real(a), Value::Integer(b)) => Value::Real(real_op(a, b as f64)),
        _ => return Err("Type error: expected numbers".into()),
    };
    ctx.push(result)?;
    Ok(())
}

/// Modulo operation with division-by-zero check.
fn mod_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;

    // Check for division by zero
    let is_zero = match &b {
        Value::Integer(0) => true,
        Value::Real(r) if *r == 0.0 => true,
        _ => false,
    };
    if is_zero {
        return Err("Infinite result: division by zero".into());
    }

    let result = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
        (Value::Real(a), Value::Real(b)) => Value::Real(a % b),
        (Value::Integer(a), Value::Real(b)) => Value::Real((a as f64) % b),
        (Value::Real(a), Value::Integer(b)) => Value::Real(a % (b as f64)),
        _ => return Err("Type error: expected numbers".into()),
    };
    ctx.push(result)?;
    Ok(())
}

fn binary_real_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(f64, f64) -> f64,
{
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let (a, b) = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => (a as f64, b as f64),
        (Value::Real(a), Value::Real(b)) => (a, b),
        (Value::Integer(a), Value::Real(b)) => (a as f64, b),
        (Value::Real(a), Value::Integer(b)) => (a, b as f64),
        _ => return Err("Type error: expected numbers".into()),
    };
    ctx.push(Value::Real(op(a, b)))?;
    Ok(())
}

fn unary_numeric_op<Fi, Fr>(ctx: &mut ExecuteContext, int_op: Fi, real_op: Fr) -> ExecuteResult
where
    Fi: FnOnce(i64) -> i64,
    Fr: FnOnce(f64) -> f64,
{
    let a = ctx.pop()?;
    let result = match a {
        Value::Integer(a) => Value::Integer(int_op(a)),
        Value::Real(a) => Value::Real(real_op(a)),
        _ => return Err("Type error: expected number".into()),
    };
    ctx.push(result)?;
    Ok(())
}

fn unary_real_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(f64) -> f64,
{
    let a = ctx.pop()?;
    let a = match a {
        Value::Integer(a) => a as f64,
        Value::Real(a) => a,
        _ => return Err("Type error: expected number".into()),
    };
    ctx.push(Value::Real(op(a)))?;
    Ok(())
}

fn compare_op<F>(ctx: &mut ExecuteContext, check: F) -> ExecuteResult
where
    F: FnOnce(Ordering) -> bool,
{
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let ord = match (&a, &b) {
        (Value::Integer(a), Value::Integer(b)) => a.cmp(b),
        (Value::Real(a), Value::Real(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
        (Value::Integer(a), Value::Real(b)) => {
            (*a as f64).partial_cmp(b).unwrap_or(Ordering::Equal)
        }
        (Value::Real(a), Value::Integer(b)) => {
            a.partial_cmp(&(*b as f64)).unwrap_or(Ordering::Equal)
        }
        _ => return Err("Type error: expected numbers".into()),
    };
    let result = if check(ord) { 1 } else { 0 };
    ctx.push(Value::Integer(result))?;
    Ok(())
}

fn sign_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let a = ctx.pop()?;
    let sign = match a {
        Value::Integer(n) => {
            if n > 0 {
                1
            } else if n < 0 {
                -1
            } else {
                0
            }
        }
        Value::Real(n) => {
            if n > 0.0 {
                1
            } else if n < 0.0 {
                -1
            } else {
                0
            }
        }
        _ => return Err("Type error: expected number".into()),
    };
    ctx.push(Value::Integer(sign))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arith_lib_id() {
        assert_eq!(ArithLib.id(), 64);
    }

    #[test]
    fn arith_lib_name() {
        assert_eq!(ArithLib.name(), "Arithmetic");
    }
}
