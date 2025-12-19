//! Program operations library.
//!
//! Provides program execution commands:
//! - EVAL - execute a program from the stack

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;
use rpl::ir::{Branch, LibId};
use rpl::libs::{ExecuteContext, ExecuteResult, LibraryImpl};
use rpl::lower::{LowerContext, LowerError};

/// Interface declaration for the Program library.
const INTERFACE: &str = include_str!("interfaces/prog.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid prog interface"))
}

/// Program library ID (matches rpl-stdlib).
pub const PROG_LIB: LibId = 8;

/// Program library command IDs.
pub mod cmd {
    pub const EVAL: u16 = 0;
}

/// Program operations library (implementation only).
#[derive(Clone, Copy)]
pub struct ProgLib;

impl LibraryImpl for ProgLib {
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

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::EVAL => {
                // EVAL is handled specially by VM::call_library() which intercepts it
                // and calls eval_program() directly. This path should never be reached.
                Err("EVAL must be executed via VM, not LibraryExecutor".into())
            }
            _ => Err(format!("Unknown program command: {}", ctx.cmd)),
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
