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
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
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
        use rpl_vm::Opcode;

        // Emit bytecode based on command - effect is determined by command_effect
        match cmd {
            cmd::ADD => ctx.emit_binary_numeric(Opcode::I64Add, Opcode::F64Add, ARITH_LIB, cmd),
            cmd::SUB => ctx.emit_binary_numeric(Opcode::I64Sub, Opcode::F64Sub, ARITH_LIB, cmd),
            cmd::MUL => ctx.emit_binary_numeric(Opcode::I64Mul, Opcode::F64Mul, ARITH_LIB, cmd),
            // Division always produces Real (RPL convention)
            cmd::DIV => ctx.emit_binary_real_only(Opcode::F64Div, ARITH_LIB, cmd),
            cmd::NEG => ctx.emit_unary_numeric(None, Opcode::F64Neg, ARITH_LIB, cmd),
            cmd::ABS => {
                // For reals, use native F64Abs; for others, use library call
                let tos = ctx.tos();
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
            cmd::SUB => sub_op(ctx),
            cmd::MUL => mul_op(ctx),
            cmd::DIV => div_op(ctx),
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

/// Numeric addition, string concatenation, list operations, and matrix element-wise addition.
fn add_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let result = match (a, b) {
        // Numeric addition
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
        (Value::Real(a), Value::Real(b)) => Value::Real(a + b),
        (Value::Integer(a), Value::Real(b)) => Value::Real(a as f64 + b),
        (Value::Real(a), Value::Integer(b)) => Value::Real(a + b as f64),
        // String concatenation
        (Value::String(a), Value::String(b)) => {
            Value::string(format!("{}{}", a.as_ref(), b.as_ref()))
        }
        // Matrix element-wise addition
        (Value::Matrix(a), Value::Matrix(b)) => {
            matrix_elementwise(&a, &b, "+", |x, y| add_values(x, y))?
        }
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
        _ => return Err("Type error: expected numbers, strings, lists, or matrices".into()),
    };
    ctx.push(result)?;
    Ok(ExecuteAction::ok())
}

/// Numeric subtraction and matrix element-wise subtraction.
fn sub_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let result = match (a, b) {
        // Numeric subtraction
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
        (Value::Real(a), Value::Real(b)) => Value::Real(a - b),
        (Value::Integer(a), Value::Real(b)) => Value::Real(a as f64 - b),
        (Value::Real(a), Value::Integer(b)) => Value::Real(a - b as f64),
        // Matrix element-wise subtraction
        (Value::Matrix(a), Value::Matrix(b)) => {
            matrix_elementwise(&a, &b, "-", |x, y| sub_values(x, y))?
        }
        _ => return Err("Type error: expected numbers or matrices".into()),
    };
    ctx.push(result)?;
    Ok(ExecuteAction::ok())
}

/// Numeric multiplication, matrix multiplication, and matrix-scalar multiplication.
fn mul_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let result = match (&a, &b) {
        // Numeric multiplication
        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
        (Value::Real(a), Value::Real(b)) => Value::Real(a * b),
        (Value::Integer(a), Value::Real(b)) => Value::Real(*a as f64 * b),
        (Value::Real(a), Value::Integer(b)) => Value::Real(a * *b as f64),
        // Matrix * Matrix = matrix multiplication
        (Value::Matrix(a), Value::Matrix(b)) => {
            matrix_multiply(a, b)?
        }
        // Matrix * scalar
        (Value::Matrix(m), scalar) if scalar.is_numeric() => {
            matrix_scale(m, scalar)?
        }
        // scalar * Matrix
        (scalar, Value::Matrix(m)) if scalar.is_numeric() => {
            matrix_scale(m, scalar)?
        }
        _ => return Err("Type error: expected numbers or matrices".into()),
    };
    ctx.push(result)?;
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
}

/// Division with zero check.
fn div_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = ctx.pop()?;
    let a = ctx.pop()?;
    let (a, b) = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => (a as f64, b as f64),
        (Value::Real(a), Value::Real(b)) => (a, b),
        (Value::Integer(a), Value::Real(b)) => (a as f64, b),
        (Value::Real(a), Value::Integer(b)) => (a, b as f64),
        _ => return Err("Type error: expected numbers".into()),
    };
    if b == 0.0 {
        return Err("Division by zero".into());
    }
    ctx.push(Value::Real(a / b))?;
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
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
        // String comparison
        (Value::String(a), Value::String(b)) => a.as_ref().cmp(b.as_ref()),
        _ => return Err("Type error: expected numbers or strings".into()),
    };
    let result = if check(ord) { 1 } else { 0 };
    ctx.push(Value::Integer(result))?;
    Ok(ExecuteAction::ok())
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
    Ok(ExecuteAction::ok())
}

// Matrix helper functions

use std::sync::Arc;
use rpl::value::MatrixData;

/// Add two values (for matrix element-wise operations).
fn add_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
        _ => Err(format!("cannot add {} and {}", a.type_name(), b.type_name())),
    }
}

/// Subtract two values (for matrix element-wise operations).
fn sub_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
        _ => Err(format!("cannot subtract {} and {}", a.type_name(), b.type_name())),
    }
}

/// Multiply two values (for matrix element-wise operations).
fn mul_values(a: &Value, b: &Value) -> Result<Value, String> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        _ => Err(format!("cannot multiply {} and {}", a.type_name(), b.type_name())),
    }
}

/// Element-wise operation on two matrices.
fn matrix_elementwise<F>(
    a: &MatrixData,
    b: &MatrixData,
    op_name: &str,
    op: F,
) -> Result<Value, String>
where
    F: Fn(&Value, &Value) -> Result<Value, String>,
{
    // Check dimensions match
    if a.rows != b.rows || a.cols != b.cols {
        return Err(format!(
            "{}: dimension mismatch ({}x{} vs {}x{})",
            op_name, a.rows, a.cols, b.rows, b.cols
        ));
    }

    let mut result = Vec::with_capacity(a.data.len());
    for (x, y) in a.data.iter().zip(b.data.iter()) {
        result.push(op(x, y)?);
    }

    Ok(Value::Matrix(Arc::new(MatrixData {
        rows: a.rows,
        cols: a.cols,
        data: result.into(),
    })))
}

/// Matrix multiplication (not element-wise).
fn matrix_multiply(a: &MatrixData, b: &MatrixData) -> Result<Value, String> {
    // For vectors, treat as row/column vectors
    let (a_rows, a_cols) = if a.is_vector() {
        (1, a.cols as usize)
    } else {
        (a.rows as usize, a.cols as usize)
    };

    let (b_rows, b_cols) = if b.is_vector() {
        (b.cols as usize, 1)
    } else {
        (b.rows as usize, b.cols as usize)
    };

    // Check inner dimensions match
    if a_cols != b_rows {
        return Err(format!(
            "*: inner dimensions mismatch ({}x{} * {}x{})",
            a_rows, a_cols, b_rows, b_cols
        ));
    }

    // Result is a_rows x b_cols
    let mut result = Vec::with_capacity(a_rows * b_cols);

    for i in 0..a_rows {
        for j in 0..b_cols {
            let mut sum = Value::Integer(0);
            for k in 0..a_cols {
                let a_val = if a.is_vector() {
                    &a.data[k]
                } else {
                    &a.data[i * a_cols + k]
                };
                let b_val = if b.is_vector() {
                    &b.data[k]
                } else {
                    &b.data[k * b_cols + j]
                };
                let product = mul_values(a_val, b_val)?;
                sum = add_values(&sum, &product)?;
            }
            result.push(sum);
        }
    }

    // Determine output shape
    if a_rows == 1 && b_cols == 1 {
        // Scalar result (dot product)
        Ok(result.into_iter().next().unwrap())
    } else if a_rows == 1 || b_cols == 1 {
        // Vector result
        Ok(Value::vector(result))
    } else {
        // Matrix result
        Ok(Value::matrix(a_rows as u16, b_cols as u16, result))
    }
}

/// Scale matrix by scalar.
fn matrix_scale(m: &MatrixData, scalar: &Value) -> Result<Value, String> {
    let mut result = Vec::with_capacity(m.data.len());
    for x in m.data.iter() {
        result.push(mul_values(x, scalar)?);
    }

    Ok(Value::Matrix(Arc::new(MatrixData {
        rows: m.rows,
        cols: m.cols,
        data: result.into(),
    })))
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

    /// Regression test: division with unknown type operand and real literal.
    /// Previously, when one operand was from a variable (unknown type at compile
    /// time) and the other was a real literal, the lowerer would incorrectly try
    /// to use native F64Div without proper type coercion, causing "expected real,
    /// got integer" runtime errors.
    #[test]
    fn division_unknown_type_with_real() {
        // Variable recall produces unknown type at compile time
        let result = crate::eval("263 'x' STO x 500. /");
        assert_eq!(result, Ok(vec![Value::Real(0.526)]));
    }

    /// Test that mixed int/real division still works when types are known.
    #[test]
    fn division_known_int_with_real() {
        let result = crate::eval("263 500. /");
        assert_eq!(result, Ok(vec![Value::Real(0.526)]));
    }

    /// Test that NEG uses FromInput(0) to preserve input type.
    #[test]
    fn neg_uses_from_input() {
        use rpl::libs::{ResultType, StackEffect};

        let spec = interface();
        let neg = spec.find_command("NEG").expect("NEG should exist");

        // The effect should be Static with FromInput(0)
        match &neg.effect_kind {
            rpl::interface::EffectKind::Static(effect) => {
                assert_eq!(
                    *effect,
                    StackEffect::fixed_result(1, &[ResultType::from_input(0)]),
                    "NEG should use FromInput(0)"
                );
            }
            _ => panic!("NEG should have Static effect, got {:?}", neg.effect_kind),
        }
    }
}
