//! Symbolic expression operations library.
//!
//! Provides operations on symbolic expressions:
//! - →NUM: Convert symbolic to numeric (if no free variables)
//! - SYMEVAL: Evaluate symbolic expression to a value

use crate::core::Span;

use crate::{
    ir::LibId,
    libs::{
        ExecuteContext, ExecuteResult, Library, StackEffect,
    },
    lower::{LowerContext, LowerError},
    types::CStack,
    value::Value,
};

/// Symbolic library ID.
pub const SYMBOLIC_LIB: LibId = 80;

/// Symbolic library command IDs.
pub mod cmd {
    /// Convert symbolic to numeric (→NUM).
    pub const TO_NUM: u16 = 0;
    /// Evaluate symbolic expression (SYMEVAL).
    pub const SYM_EVAL: u16 = 1;
}

/// Symbolic operations library.
#[derive(Clone, Copy)]
pub struct SymbolicLib;

impl Library for SymbolicLib {
    fn id(&self) -> LibId {
        SYMBOLIC_LIB
    }

    fn name(&self) -> &'static str {
        "Symbolic"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            // →NUM: (symbolic -- number)
            CommandInfo::with_effect("→NUM", SYMBOLIC_LIB, cmd::TO_NUM, 1, 1),
            CommandInfo::with_effect("->NUM", SYMBOLIC_LIB, cmd::TO_NUM, 1, 1), // ASCII arrow
            CommandInfo::with_effect("TONUM", SYMBOLIC_LIB, cmd::TO_NUM, 1, 1), // Alternative name
            // SYMEVAL: (symbolic -- value)
            CommandInfo::with_effect("SYMEVAL", SYMBOLIC_LIB, cmd::SYM_EVAL, 1, 1),
        ]
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

    fn command_effect(&self, cmd: u16, _types: &CStack) -> StackEffect {
        match cmd {
            // Result could be Integer or Real
            cmd::TO_NUM => StackEffect::fixed(1, &[None]),
            // Result could be any type
            cmd::SYM_EVAL => StackEffect::fixed(1, &[None]),
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::Library;

    #[test]
    fn symbolic_lib_id() {
        assert_eq!(SymbolicLib.id(), 80);
    }

    #[test]
    fn symbolic_lib_name() {
        assert_eq!(SymbolicLib.name(), "Symbolic");
    }
}
