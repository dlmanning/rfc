//! Symbolic expression operations library.
//!
//! Provides operations on symbolic expressions:
//! - →NUM: Convert symbolic to numeric (if no free variables)
//! - SYMEVAL: Evaluate symbolic expression to a value

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryImpl},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Symbolic library.
const INTERFACE: &str = include_str!("interfaces/symbolic.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid symbolic interface"))
}

/// Symbolic library ID.
pub const SYMBOLIC_LIB: LibId = 80;

/// Symbolic library command IDs.
pub mod cmd {
    /// Convert symbolic to numeric (→NUM).
    pub const TO_NUM: u16 = 0;
    /// Evaluate symbolic expression (SYMEVAL).
    pub const SYM_EVAL: u16 = 1;
}

/// Symbolic operations library (implementation only).
#[derive(Clone, Copy)]
pub struct SymbolicLib;

impl LibraryImpl for SymbolicLib {
    fn id(&self) -> LibId {
        SYMBOLIC_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SYMBOLIC_LIB, cmd);
        Ok(())
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::TO_NUM => {
                let val = ctx.pop()?;
                match val {
                    Value::Symbolic(expr) => {
                        // Try to evaluate to a number
                        match expr.try_eval() {
                            Some(n) => {
                                // Check if it's an integer
                                if n.fract() == 0.0 && n.abs() < 9007199254740992.0 {
                                    ctx.push(Value::Integer(n as i64))?;
                                } else {
                                    ctx.push(Value::Real(n))?;
                                }
                                Ok(())
                            }
                            None => Err("→NUM: symbolic expression contains variables".into()),
                        }
                    }
                    // For numeric types, just return as-is
                    Value::Integer(n) => {
                        ctx.push(Value::Integer(n))?;
                        Ok(())
                    }
                    Value::Real(n) => {
                        ctx.push(Value::Real(n))?;
                        Ok(())
                    }
                    _ => Err(format!("→NUM: expected symbolic or number, got {}", val.type_name())),
                }
            }
            cmd::SYM_EVAL => {
                let val = ctx.pop()?;
                match val {
                    Value::Symbolic(expr) => {
                        // Try to evaluate to a number
                        match expr.try_eval() {
                            Some(n) => {
                                // Check if it's an integer
                                if n.fract() == 0.0 && n.abs() < 9007199254740992.0 {
                                    ctx.push(Value::Integer(n as i64))?;
                                } else {
                                    ctx.push(Value::Real(n))?;
                                }
                                Ok(())
                            }
                            None => {
                                // Can't evaluate, return as symbolic
                                ctx.push(Value::Symbolic(expr))?;
                                Ok(())
                            }
                        }
                    }
                    // For other types, just return as-is
                    other => {
                        ctx.push(other)?;
                        Ok(())
                    }
                }
            }
            _ => Err(format!("Unknown symbolic command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbolic_lib_id() {
        assert_eq!(interface().id(), 80);
    }

    #[test]
    fn symbolic_lib_name() {
        assert_eq!(interface().name(), "Symbolic");
    }
}
