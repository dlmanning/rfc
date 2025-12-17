//! Program operations library.
//!
//! Provides program execution commands:
//! - EVAL - execute a program from the stack

use crate::ir::{Branch, LibId};
use crate::libs::{ExecuteContext, ExecuteResult, Library};
use crate::lower::{LowerContext, LowerError};
use crate::core::Span;

/// Program library ID (matches rpl-stdlib).
pub const PROG_LIB: LibId = 8;

/// Program library command IDs.
pub mod cmd {
    pub const EVAL: u16 = 0;
}

/// Program operations library.
#[derive(Clone, Copy)]
pub struct ProgLib;

impl Library for ProgLib {
    fn id(&self) -> LibId {
        PROG_LIB
    }

    fn name(&self) -> &'static str {
        "Programs"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            // EVAL is dynamic - consumes 1 program, produces unknown
            CommandInfo::new("EVAL", PROG_LIB, cmd::EVAL),
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
        assert_eq!(ProgLib.id(), 8);
    }

    #[test]
    fn prog_lib_name() {
        assert_eq!(ProgLib.name(), "Programs");
    }
}
