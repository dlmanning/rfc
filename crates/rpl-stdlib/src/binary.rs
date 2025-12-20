//! Binary integer operations library.
//!
//! Provides bitwise operations:
//! - BAND, BOR, BXOR, BNOT (bitwise logic)
//! - BLSL, BLSR (bit shifts)
//!
//! Provides binary arithmetic:
//! - BADD, BSUB, BMUL, BDIV (64-bit integer arithmetic)

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};
use rpl_vm::Opcode;

/// Interface declaration for the Binary library.
const INTERFACE: &str = include_str!("interfaces/binary.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid binary interface"))
}

/// Binary operations library ID.
pub const BINARY_LIB: LibId = 96;

/// Binary library command IDs (order matches INTERFACE declaration).
pub mod cmd {
    pub const BAND: u16 = 0;
    pub const BOR: u16 = 1;
    pub const BXOR: u16 = 2;
    pub const BNOT: u16 = 3;
    pub const BLSL: u16 = 4;
    pub const BLSR: u16 = 5;
    pub const BADD: u16 = 6;
    pub const BSUB: u16 = 7;
    pub const BMUL: u16 = 8;
    pub const BDIV: u16 = 9;
}

/// Binary operations library (implementation only).
#[derive(Clone, Copy)]
pub struct BinaryLib;

impl LibraryLowerer for BinaryLib {
    fn id(&self) -> LibId {
        BINARY_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // Check if we can use optimized integer ops
        let tos = ctx.types.top();
        let nos = ctx.types.nos();
        let tos_is_int = tos.is_integer();
        let both_int = tos_is_int && nos.is_integer();

        match cmd {
            // Binary bitwise operations (2 -> 1)
            cmd::BAND => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64And);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BOR => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Or);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BXOR => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Xor);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BNOT => {
                if tos_is_int {
                    // !x == x ^ -1
                    ctx.output.emit_i64_const(-1);
                    ctx.output.emit_opcode(Opcode::I64Xor);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BLSL => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Shl);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BLSR => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64ShrU);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }

            // Binary arithmetic (2 -> 1)
            cmd::BADD => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Add);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BSUB => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Sub);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            cmd::BMUL => {
                if both_int {
                    ctx.output.emit_opcode(Opcode::I64Mul);
                } else {
                    ctx.output.emit_call_lib(BINARY_LIB, cmd);
                }
            }
            // BDIV: always use CallLib (needs division by zero check)
            cmd::BDIV => {
                ctx.output.emit_call_lib(BINARY_LIB, cmd);
            }

            _ => {
                ctx.output.emit_call_lib(BINARY_LIB, cmd);
            }
        }
        Ok(())
    }
}

impl LibraryExecutor for BinaryLib {
    fn id(&self) -> LibId {
        BINARY_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            // Bitwise operations (fallback if called via CallLib)
            cmd::BAND => binary_op(ctx, |a, b| a & b),
            cmd::BOR => binary_op(ctx, |a, b| a | b),
            cmd::BXOR => binary_op(ctx, |a, b| a ^ b),
            cmd::BNOT => unary_op(ctx, |a| !a),
            cmd::BLSL => shift_op(ctx, true),
            cmd::BLSR => shift_op(ctx, false),

            // Binary arithmetic
            cmd::BADD => binary_op(ctx, |a, b| a.wrapping_add(b)),
            cmd::BSUB => binary_op(ctx, |a, b| a.wrapping_sub(b)),
            cmd::BMUL => binary_op(ctx, |a, b| a.wrapping_mul(b)),
            cmd::BDIV => bdiv_op(ctx),

            _ => Err(format!("Unknown binary command: {}", ctx.cmd)),
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract integer value, coercing real to integer.
fn to_integer(value: &Value) -> Result<i64, String> {
    match value {
        Value::Integer(n) => Ok(*n),
        Value::Real(r) => Ok(*r as i64),
        _ => Err(format!("Expected integer, got {}", value.type_name())),
    }
}

/// Binary operation.
fn binary_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(i64, i64) -> i64,
{
    let b = to_integer(&ctx.pop()?)?;
    let a = to_integer(&ctx.pop()?)?;
    ctx.push(Value::Integer(op(a, b)))?;
    Ok(())
}

/// Unary operation.
fn unary_op<F>(ctx: &mut ExecuteContext, op: F) -> ExecuteResult
where
    F: FnOnce(i64) -> i64,
{
    let a = to_integer(&ctx.pop()?)?;
    ctx.push(Value::Integer(op(a)))?;
    Ok(())
}

/// Shift operation (left or right).
fn shift_op(ctx: &mut ExecuteContext, left: bool) -> ExecuteResult {
    let count = to_integer(&ctx.pop()?)?;
    let value = to_integer(&ctx.pop()?)?;

    if count < 0 {
        return Err("Shift count must be non-negative".into());
    }

    let result = if left {
        (value as u64).wrapping_shl(count as u32) as i64
    } else {
        (value as u64).wrapping_shr(count as u32) as i64
    };

    ctx.push(Value::Integer(result))?;
    Ok(())
}

/// BDIV operation with division by zero check.
fn bdiv_op(ctx: &mut ExecuteContext) -> ExecuteResult {
    let b = to_integer(&ctx.pop()?)?;
    let a = to_integer(&ctx.pop()?)?;

    if b == 0 {
        return Err("Infinite result: division by zero".into());
    }

    ctx.push(Value::Integer(a / b))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_integer() {
        assert_eq!(to_integer(&Value::Integer(42)).unwrap(), 42);
        assert_eq!(to_integer(&Value::Real(3.14)).unwrap(), 3);
        assert!(to_integer(&Value::string("hello")).is_err());
    }
}
