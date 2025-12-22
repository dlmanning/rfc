//! Comments library.
//!
//! Provides comment handling:
//! - `@ text` - Single-line comment (ends at newline)
//! - `@@ text` - Permanent single-line comment
//! - `@@@ text @@@` - Multi-line comment
//!
//! Comments are skipped during tokenization and not stored in bytecode.
//! Therefore, STRIPCOMMENTS is a no-op (returns program unchanged).

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Comments library.
const INTERFACE: &str = include_str!("interfaces/comments.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid comments interface"))
}

/// Comments library ID (matches rpl-stdlib).
pub const COMMENTS_LIB: LibId = 20;

/// Comments library command IDs.
pub mod cmd {
    pub const STRIPCOMMENTS: u16 = 0;
}

/// Comments library (implementation only).
#[derive(Clone, Copy)]
pub struct CommentsLib;

impl LibraryLowerer for CommentsLib {
    fn id(&self) -> LibId {
        COMMENTS_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(COMMENTS_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for CommentsLib {
    fn id(&self) -> LibId {
        COMMENTS_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::STRIPCOMMENTS => {
                // Comments are already stripped during parsing, so just return
                // the program unchanged.
                let val = ctx.pop()?;
                match &val {
                    Value::Program(_) => {
                        ctx.push(val)?;
                        Ok(ExecuteAction::ok())
                    }
                    _ => Err(format!(
                        "STRIPCOMMENTS: expected program, got {}",
                        val.type_name()
                    )),
                }
            }
            _ => Err(format!("Unknown comments command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comments_lib_id() {
        assert_eq!(interface().id(), 20);
    }

    #[test]
    fn comments_lib_name() {
        assert_eq!(interface().name(), "Comments");
    }
}
