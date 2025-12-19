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

use std::{cmp::Ordering, sync::OnceLock};

use rpl::{
    core::Span,
    interface::InterfaceSpec,
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Arithmetic library.
const INTERFACE: &str = include_str!("interfaces/arith.rpli");

/// Get the interface spec (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid arith interface"))
}

/// Arithmetic library ID (matches rpl-stdlib).
pub const ARITH_LIB: LibId = 64;

/// Arithmetic library command IDs (order matches INTERFACE declaration).
pub mod cmd {
    pub const ADD: u16 = 0;
    pub const SUB: u16 = 1;
    pub const MUL: u16 = 2;
    pub const DIV: u16 = 3;
    pub const NEG: u16 = 4;
    pub const INV: u16 = 5;
    pub const MOD: u16 = 6;
    pub const ABS: u16 = 7;
    pub const EQ: u16 = 8;
    pub const NE: u16 = 9;
    pub const LT: u16 = 10;
    pub const LE: u16 = 11;
    pub const GT: u16 = 12;
    pub const GE: u16 = 13;
    pub const POW: u16 = 14;
    pub const MIN: u16 = 15;
    pub const MAX: u16 = 16;
    pub const SIGN: u16 = 17;
    pub const SQ: u16 = 18;
}

/// Arithmetic operations library (implementation only).
#[derive(Clone, Copy)]
pub struct ArithLib;

impl LibraryLowerer for ArithLib {
    fn id(&self) -> LibId {
        ARITH_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        use rpl::vm::bytecode::Opcode;

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
}

impl LibraryExecutor for ArithLib {
    fn id(&self) -> LibId {
        ARITH_LIB
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
        assert_eq!(interface().id(), 64);
    }

    #[test]
    fn arith_lib_name() {
        assert_eq!(interface().name(), "Arithmetic");
    }
}
