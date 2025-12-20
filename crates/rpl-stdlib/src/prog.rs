//! Program operations library.
//!
//! Provides program execution commands:
//! - EVAL - execute a program from the stack

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;
use rpl::ir::{Branch, LibId};
use rpl::libs::LibraryLowerer;
use rpl::lower::{LowerContext, LowerError};

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

// Note: ProgLib does not implement LibraryExecutor.
// EVAL is handled specially by the VM which intercepts CallLib(PROG_LIB, EVAL)
// and executes the program directly.

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
