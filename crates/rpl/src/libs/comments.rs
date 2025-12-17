//! Comments library.
//!
//! Provides comment handling:
//! - `@ text` - Single-line comment (ends at newline)
//! - `@@ text` - Permanent single-line comment
//! - `@@@ text @@@` - Multi-line comment
//!
//! Comments are skipped during tokenization and not stored in bytecode.
//! Therefore, STRIPCOMMENTS is a no-op (returns program unchanged).

use crate::core::Span;

use crate::{
    core::TypeId,
    ir::LibId,
    libs::{
        ExecuteContext, ExecuteResult, Library, StackEffect,
    },
    lower::{LowerContext, LowerError},
    types::CStack,
    value::Value,
};

/// Comments library ID (matches rpl-stdlib).
pub const COMMENTS_LIB: LibId = 20;

/// Comments library command IDs.
pub mod cmd {
    pub const STRIPCOMMENTS: u16 = 0;
}

/// Comments library.
#[derive(Clone, Copy)]
pub struct CommentsLib;

impl Library for CommentsLib {
    fn id(&self) -> LibId {
        COMMENTS_LIB
    }

    fn name(&self) -> &'static str {
        "Comments"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            // STRIPCOMMENTS: (program -- program)
            // Since comments are stripped during parsing, this is a no-op
            CommandInfo::with_effect("STRIPCOMMENTS", COMMENTS_LIB, cmd::STRIPCOMMENTS, 1, 1),
        ]
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

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::STRIPCOMMENTS => {
                // Comments are already stripped during parsing, so just return
                // the program unchanged.
                let val = ctx.pop()?;
                match &val {
                    Value::Program(_) => {
                        ctx.push(val)?;
                        Ok(())
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

    fn command_effect(&self, cmd: u16, _types: &CStack) -> StackEffect {
        match cmd {
            cmd::STRIPCOMMENTS => StackEffect::fixed(1, &[Some(TypeId::PROGRAM)]),
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::Library;

    #[test]
    fn comments_lib_id() {
        assert_eq!(CommentsLib.id(), 20);
    }

    #[test]
    fn comments_lib_name() {
        assert_eq!(CommentsLib.name(), "Comments");
    }
}
