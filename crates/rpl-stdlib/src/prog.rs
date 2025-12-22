//! Program operations library.
//!
//! Provides program execution commands:
//! - EVAL - execute a program from the stack

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;
use rpl::ir::{Branch, LibId};
use rpl::libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer};
use rpl::lower::{LowerContext, LowerError};
use rpl::value::Value;

// Re-export from rpl-vm (the authority on these constants)
pub use rpl_vm::{PROG_LIB, prog_cmd as cmd};

/// Interface declaration for the Program library.
const INTERFACE: &str = include_str!("interfaces/prog.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid prog interface"))
}

/// Program operations library (implementation only).
#[derive(Clone, Copy)]
pub struct ProgLib;

impl LibraryLowerer for ProgLib {
    fn id(&self) -> LibId {
        PROG_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError { span: None,
            message: "Program library has no composites".into(),
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // EVAL has dynamic effects - we don't know what the program will do
        ctx.output.emit_call_lib(PROG_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for ProgLib {
    fn id(&self) -> LibId {
        PROG_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::EVAL => {
                let value = ctx.pop()?;
                match value {
                    Value::Program(prog) => {
                        // Return action for VM to execute the program
                        Ok(ExecuteAction::call(prog, None))
                    }
                    Value::Symbolic(expr) => {
                        // Return action for VM to evaluate the symbolic expression
                        Ok(ExecuteAction::eval_symbolic(expr))
                    }
                    _ => Err(format!(
                        "EVAL expected program or symbolic, got {}",
                        value.type_name()
                    )),
                }
            }
            _ => Err(format!("Unknown prog command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prog_lib_id() {
        assert_eq!(interface().id(), 8);
    }

    #[test]
    fn prog_lib_name() {
        assert_eq!(interface().name(), "Programs");
    }
}
