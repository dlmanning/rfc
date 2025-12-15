use super::id::LibraryId;
use crate::operator::OperatorRegistry;
use rpl_core::token::{SemanticKind, TokenInfo};

use super::context::{CompileContext, ExecuteContext, ProbeContext};
use super::effect::StackEffect;

/// Result of probing a token.
#[derive(Clone, Debug)]
pub enum ProbeResult {
    /// Token not recognized by this library.
    NoMatch,
    /// Token recognized.
    Match {
        info: TokenInfo,
        semantic: SemanticKind,
    },
    /// Token recognized but not allowed in current context.
    NotAllowed { reason: &'static str },
}

/// Result of compiling a token.
#[derive(Clone, Debug)]
pub enum CompileResult {
    /// Compilation successful.
    Ok,
    /// Token not handled by this library.
    NoMatch,
    /// Start a new construct.
    StartConstruct { kind: ConstructKind },
    /// End the current construct.
    EndConstruct,
    /// Need more tokens to complete.
    NeedMore,
    /// Start infix mode.
    StartInfix,
    /// End infix mode.
    EndInfix,
    /// Split token at given character index.
    Split { at_char: usize },
    /// Compilation error.
    Error { message: String },
}

use std::sync::Arc;

use rpl_vm::ProgramDebugInfo;

/// Successful execution outcomes.
#[derive(Clone, Debug)]
pub enum ExecuteOk {
    /// Execution successful, continue to next instruction.
    Ok,
    /// Jump to address.
    Jump(usize),
    /// Call subroutine at address.
    Call(usize),
    /// Return from subroutine.
    Return,
    /// Yield execution.
    Yield,
    /// Halt execution.
    Halt,
    /// Execute a program (bytecode from stack, anonymous).
    EvalProgram(Vec<u32>),
    /// Execute a named program (bytecode with function name for stack traces).
    EvalProgramNamed(Vec<u32>, String),
    /// Execute a named program with full debug info (bytecode, name, spans).
    EvalProgramWithDebug {
        code: Vec<u32>,
        name: String,
        debug_info: Arc<ProgramDebugInfo>,
    },
}

/// Result of executing a command.
pub type ExecuteResult = Result<ExecuteOk, String>;

/// Convenience constant for successful execution.
pub const EXEC_OK: ExecuteResult = Ok(ExecuteOk::Ok);

/// Kind of syntactic construct.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ConstructKind {
    Program,
    List,
    Symbolic,
    Matrix,
    Vector,
    Complex,
    LocalBinding,
    If,
    Case,
    For,
    ForUp,
    ForDn,
    Start,
    While,
    DoUntil,
    ErrorHandler,
}

/// Documentation for a token.
#[derive(Clone, Debug)]
pub struct TokenDoc {
    pub name: &'static str,
    pub brief: &'static str,
    pub stack: &'static str,
    pub example: &'static str,
    pub see_also: &'static [&'static str],
}

/// Trait for RPL libraries.
pub trait Library: Send + Sync + 'static {
    /// Get the library ID.
    fn id(&self) -> LibraryId;

    /// Get the library name.
    fn name(&self) -> &'static str;

    /// Probe a token to see if this library handles it.
    fn probe(&self, ctx: &ProbeContext) -> ProbeResult;

    /// Compile a token.
    fn compile(&self, ctx: &mut CompileContext) -> CompileResult;

    /// Execute a command.
    ///
    /// The context provides access to the VM, the command ID, and any operand
    /// words following the opcode in the code stream.
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult;

    /// Get the stack effect of a token.
    fn stack_effect(&self, _token: &str) -> StackEffect {
        StackEffect::Dynamic
    }

    /// Register operators provided by this library.
    fn register_operators(&self, _registry: &mut OperatorRegistry) {}

    /// Register coercions provided by this library.
    fn register_coercions(&self, _registry: &mut OperatorRegistry) {}

    /// Get documentation for all tokens.
    fn tokens(&self) -> &'static [TokenDoc] {
        &[]
    }

    /// Get help text for a specific token.
    fn help(&self, _token: &str) -> Option<&'static str> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn probe_result_variants() {
        let no_match = ProbeResult::NoMatch;
        assert!(matches!(no_match, ProbeResult::NoMatch));

        let matched = ProbeResult::Match {
            info: TokenInfo::atom(3),
            semantic: SemanticKind::Command,
        };
        assert!(matches!(matched, ProbeResult::Match { .. }));

        let not_allowed = ProbeResult::NotAllowed {
            reason: "not in program",
        };
        assert!(matches!(not_allowed, ProbeResult::NotAllowed { .. }));
    }

    #[test]
    fn compile_result_variants() {
        assert!(matches!(CompileResult::Ok, CompileResult::Ok));
        assert!(matches!(CompileResult::NoMatch, CompileResult::NoMatch));
        assert!(matches!(
            CompileResult::StartConstruct {
                kind: ConstructKind::Program
            },
            CompileResult::StartConstruct { .. }
        ));
    }

    #[test]
    fn execute_result_variants() {
        assert!(matches!(Ok::<_, String>(ExecuteOk::Ok), Ok(ExecuteOk::Ok)));
        assert!(matches!(Ok::<_, String>(ExecuteOk::Jump(10)), Ok(ExecuteOk::Jump(10))));
        assert!(matches!(Ok::<_, String>(ExecuteOk::Halt), Ok(ExecuteOk::Halt)));
        assert!(matches!(Err::<ExecuteOk, _>("error".to_string()), Err(_)));
    }

    #[test]
    fn construct_kind_copy() {
        let kind = ConstructKind::Program;
        let kind2 = kind;
        assert_eq!(kind, kind2);
    }

    #[test]
    fn token_doc_fields() {
        let doc = TokenDoc {
            name: "DUP",
            brief: "Duplicate top of stack",
            stack: "( a -- a a )",
            example: "3 DUP",
            see_also: &["DROP", "SWAP"],
        };
        assert_eq!(doc.name, "DUP");
        assert_eq!(doc.see_also.len(), 2);
    }
}
