//! Library infrastructure for extending RPL.
//!
//! Libraries provide:
//! - Token claims for custom syntax (IF, FOR, etc.)
//! - Commands (stack operations, arithmetic, etc.)
//! - Lowering for extended composites
//! - Runtime execution
//!
//! Library IDs follow the rpl-stdlib convention:
//! - Programs: PROG_LIB (8)
//! - Flow control: FLOW_LIB (9)
//! - Locals: LOCALS_LIB (32)
//! - Arithmetic: ARITH_LIB (64)
//! - Stack operations: STACK_LIB (72)

// Library modules
pub mod arith;
pub mod binary;
pub mod comments;
pub mod directory;
pub mod flow;
pub mod list;
pub mod locals;
pub mod prog;
pub mod stack;
pub mod strings;
pub mod symbolic;
pub mod transcendentals;
pub mod userlib;

// Re-export library structs
pub use arith::ArithLib;
pub use binary::BinaryLib;
pub use comments::CommentsLib;
pub use directory::DirectoryLib;
pub use flow::FlowLib;
pub use list::ListLib;
pub use locals::LocalsLib;
pub use prog::ProgLib;
pub use stack::StackLib;
pub use strings::StringsLib;
pub use symbolic::SymbolicLib;
pub use transcendentals::TranscendentalsLib;

// Re-export library IDs
pub use arith::ARITH_LIB;
pub use binary::BINARY_LIB;
pub use comments::COMMENTS_LIB;
pub use directory::DIRECTORY_LIB;
pub use list::LIST_LIB;
pub use prog::PROG_LIB;
pub use stack::STACK_LIB;
pub use strings::STRINGS_LIB;
pub use symbolic::SYMBOLIC_LIB;
pub use transcendentals::TRANSCENDENTALS_LIB;

use crate::ir::{Branch, LibId};
use crate::lower::{LowerContext, LowerError};
use crate::value::Value;
use crate::vm::directory::Directory;
use crate::vm::stack::Stack;
use crate::vm::RplException;
use crate::core::{Span, TypeId};
use smallvec::SmallVec;

// ============================================================================
// Stack Effects
// ============================================================================

/// Stack effect of a command.
///
/// Used for type inference during lowering and validation.
/// Returned by `LibraryLowerer::lower_command()` after emitting code,
/// so the effect can reflect the actual types based on input types.
///
/// # Type Permutations
///
/// For stack manipulation commands, we track how input types flow to outputs.
/// The `Permutation` variant uses a pattern array where each element indicates
/// which input position (0 = deepest consumed) maps to that output position.
///
/// Examples:
/// - `SWAP (a b -- b a)`: inputs=2, pattern=[1, 0]
/// - `DUP (a -- a a)`: inputs=1, pattern=[0, 0]
/// - `DROP (a --)`: inputs=1, pattern=[]
/// - `ROT (a b c -- b c a)`: inputs=3, pattern=[1, 2, 0]
/// - `OVER (a b -- a b a)`: inputs=2, pattern=[0, 1, 0]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackEffect {
    /// Type-preserving permutation of stack items.
    ///
    /// - `inputs`: number of items consumed from stack
    /// - `pattern`: for each output slot (bottom to top), which input slot it comes from
    ///
    /// Pattern indices are 0-based where 0 is the deepest consumed item.
    Permutation {
        inputs: u8,
        pattern: SmallVec<[u8; 4]>,
    },
    /// Fixed number of inputs with known result types.
    ///
    /// - `consumes`: number of items consumed from stack
    /// - `results`: types of produced items (None = unknown type)
    Fixed {
        consumes: u8,
        results: SmallVec<[Option<TypeId>; 4]>,
    },
    /// Dynamic effect (depends on runtime values, e.g., PICK n).
    Dynamic,
}

impl StackEffect {
    /// Create a fixed effect with known result types.
    ///
    /// Use `Some(TypeId::XXX)` for known types, `None` for unknown.
    pub fn fixed(consumes: u8, results: &[Option<TypeId>]) -> Self {
        Self::Fixed {
            consumes,
            results: SmallVec::from_slice(results),
        }
    }

    /// Create a permutation effect.
    ///
    /// # Arguments
    /// - `inputs`: number of stack items consumed
    /// - `pattern`: output mapping, where `pattern[i]` is the input index for output `i`
    pub fn permutation(inputs: u8, pattern: &[u8]) -> Self {
        Self::Permutation {
            inputs,
            pattern: SmallVec::from_slice(pattern),
        }
    }

    /// Get the number of items consumed.
    pub fn consumes(&self) -> Option<u8> {
        match self {
            Self::Permutation { inputs, .. } => Some(*inputs),
            Self::Fixed { consumes, .. } => Some(*consumes),
            Self::Dynamic => None,
        }
    }

    /// Get the number of items produced.
    pub fn produces(&self) -> Option<u8> {
        match self {
            Self::Permutation { pattern, .. } => Some(pattern.len() as u8),
            Self::Fixed { results, .. } => Some(results.len() as u8),
            Self::Dynamic => None,
        }
    }

    /// Get the result types (for Fixed effects).
    pub fn results(&self) -> Option<&[Option<TypeId>]> {
        match self {
            Self::Fixed { results, .. } => Some(results),
            _ => None,
        }
    }

    // Common permutation patterns as functions
    pub fn dup() -> Self { Self::permutation(1, &[0, 0]) }
    pub fn drop() -> Self { Self::permutation(1, &[]) }
    pub fn swap() -> Self { Self::permutation(2, &[1, 0]) }
    pub fn rot() -> Self { Self::permutation(3, &[1, 2, 0]) }
    pub fn over() -> Self { Self::permutation(2, &[0, 1, 0]) }
    pub fn nip() -> Self { Self::permutation(2, &[1]) }  // (a b -- b)
    pub fn tuck() -> Self { Self::permutation(2, &[1, 0, 1]) }  // (a b -- b a b)

    /// DEPTH: (-- n) produces integer, doesn't consume anything
    pub fn depth() -> Self { Self::fixed(0, &[Some(TypeId::BINT)]) }

    /// Format the stack effect in traditional notation: `(inputs -- outputs)`
    pub fn to_notation(&self) -> String {
        match self {
            Self::Permutation { inputs, pattern } => {
                // Use letters for the input slots (a, b, c, ...)
                let input_names: Vec<char> = (0..*inputs)
                    .map(|i| (b'a' + i) as char)
                    .collect();
                let inputs_str: Vec<String> = input_names.iter().map(|c| c.to_string()).collect();
                let outputs_str: Vec<String> = pattern
                    .iter()
                    .map(|&i| input_names.get(i as usize).copied().unwrap_or('?').to_string())
                    .collect();
                format!("({} -- {})", inputs_str.join(" "), outputs_str.join(" "))
            }
            Self::Fixed { consumes, results } => {
                // Use letters for inputs, type names for outputs
                let inputs_str: Vec<String> = (0..*consumes)
                    .map(|i| ((b'a' + i) as char).to_string())
                    .collect();
                let outputs_str: Vec<&str> = results
                    .iter()
                    .map(|ty| match ty {
                        Some(TypeId::BINT) => "int",
                        Some(TypeId::REAL) => "real",
                        Some(TypeId::STRING) => "str",
                        Some(TypeId::LIST) => "list",
                        Some(TypeId::PROGRAM) => "prog",
                        Some(TypeId::SYMBOLIC) => "sym",
                        _ => "?",
                    })
                    .collect();
                format!("({} -- {})", inputs_str.join(" "), outputs_str.join(" "))
            }
            Self::Dynamic => "(dynamic)".to_string(),
        }
    }
}

// ============================================================================
// Effect Computation Helpers
// ============================================================================

use crate::types::CStack;

/// Compute the stack effect for a binary numeric operation.
///
/// This is the single source of truth for determining result types
/// of binary numeric operations like +, -, *, /, etc.
///
/// Rules:
/// - If both operands are integers → integer result
/// - If either operand is real (or mixed int/real) → real result
/// - Otherwise → unknown result type
pub fn binary_numeric_effect(types: &CStack) -> StackEffect {
    let (tos, nos) = (types.top(), types.nos());
    if tos.is_integer() && nos.is_integer() {
        StackEffect::fixed(2, &[Some(TypeId::BINT)])
    } else if tos.is_real() || nos.is_real() {
        StackEffect::fixed(2, &[Some(TypeId::REAL)])
    } else {
        StackEffect::fixed(2, &[None])
    }
}

/// Compute the stack effect for a unary operation that preserves type.
///
/// Used for operations like NEG, ABS, SQ that return the same type
/// as their input.
///
/// Rules:
/// - If input is integer → integer result
/// - If input is real → real result
/// - Otherwise → unknown result type
pub fn unary_preserving_effect(types: &CStack) -> StackEffect {
    let tos = types.top();
    if tos.is_integer() {
        StackEffect::fixed(1, &[Some(TypeId::BINT)])
    } else if tos.is_real() {
        StackEffect::fixed(1, &[Some(TypeId::REAL)])
    } else {
        StackEffect::fixed(1, &[None])
    }
}

// ============================================================================
// Token Claims
// ============================================================================

/// Context for token claims.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClaimContext {
    /// Claim applies in any context.
    Any,
    /// Claim only applies outside infix expressions.
    NotInfix,
    /// Claim only applies inside infix expressions.
    InfixOnly,
}

/// A token claim by a library.
///
/// Libraries claim tokens to handle custom syntax. When the parser
/// encounters a claimed token, it delegates to the library's parser.
#[derive(Clone, Debug)]
pub struct TokenClaim {
    /// The token text to claim (case-insensitive matching).
    pub token: &'static str,
    /// Priority (higher wins conflicts between libraries).
    pub priority: u8,
    /// Context in which this claim applies.
    pub context: ClaimContext,
    /// The library making the claim.
    pub lib_id: LibId,
}

// ============================================================================
// Command Info
// ============================================================================

/// Command metadata for registration.
#[derive(Clone, Debug)]
pub struct CommandInfo {
    /// Command name (case-insensitive).
    pub name: &'static str,
    /// Library ID.
    pub lib_id: LibId,
    /// Command ID within the library.
    pub cmd_id: u16,
    /// Stack effect of this command.
    pub effect: StackEffect,
}

impl CommandInfo {
    /// Create command info with dynamic stack effect.
    pub fn new(name: &'static str, lib_id: LibId, cmd_id: u16) -> Self {
        Self {
            name,
            lib_id,
            cmd_id,
            effect: StackEffect::Dynamic,
        }
    }

    /// Create command info with a specific stack effect (counts only).
    ///
    /// This creates a stack effect with unknown result types (None). The actual
    /// type inference happens in `LibraryLowerer::lower_command()`.
    pub fn with_effect(
        name: &'static str,
        lib_id: LibId,
        cmd_id: u16,
        consumes: u8,
        produces: u8,
    ) -> Self {
        // Create a slice of None (unknown) types for the produces count
        let results: SmallVec<[Option<TypeId>; 4]> = (0..produces).map(|_| None).collect();
        Self {
            name,
            lib_id,
            cmd_id,
            effect: StackEffect::Fixed { consumes, results },
        }
    }
}

// ============================================================================
// Execution Context
// ============================================================================

/// Context for executing library commands at runtime.
pub struct ExecuteContext<'a> {
    /// The data stack.
    pub stack: &'a mut Stack,
    /// The global directory.
    pub directory: &'a mut Directory,
    /// The command ID being executed.
    pub cmd: u16,
    /// The last error that was caught (for ERRN/ERRM).
    last_error: Option<RplException>,
}

impl<'a> ExecuteContext<'a> {
    /// Create a new execute context.
    pub fn new(
        stack: &'a mut Stack,
        directory: &'a mut Directory,
        cmd: u16,
        last_error: Option<RplException>,
    ) -> Self {
        Self {
            stack,
            directory,
            cmd,
            last_error,
        }
    }

    /// Get the last error (for ERRN/ERRM).
    pub fn last_error(&self) -> Option<&RplException> {
        self.last_error.as_ref()
    }

    /// Pop a value from the stack.
    pub fn pop(&mut self) -> Result<Value, String> {
        self.stack.pop().map_err(|e| e.to_string())
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: Value) -> Result<(), String> {
        self.stack.push(value).map_err(|e| e.to_string())
    }

    /// Peek at a value on the stack (0 = top).
    pub fn peek(&self, index: usize) -> Result<&Value, String> {
        self.stack.peek(index).map_err(|e| e.to_string())
    }

    /// Get the stack depth.
    pub fn depth(&self) -> usize {
        self.stack.depth()
    }

    // === Directory operations ===

    /// Store a value in the directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.directory.store(name, value);
    }

    /// Look up a value in the directory.
    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.directory.lookup(name)
    }

    /// Remove a variable from the directory.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.directory.purge(name)
    }

    /// Check if a variable exists.
    pub fn has_var(&self, name: &str) -> bool {
        self.directory.has_var(name)
    }

    /// Get an iterator over variable names.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.directory.vars()
    }

    /// Clear all variables.
    pub fn clear_vars(&mut self) {
        self.directory.clear();
    }

    /// Rename a variable.
    pub fn rename_var(&mut self, old_name: &str, new_name: &str) -> bool {
        self.directory.rename(old_name, new_name)
    }

    // === Directory navigation ===

    /// Create a subdirectory in the current directory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.directory.create_subdir(name)
    }

    /// Remove an empty subdirectory from the current directory.
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), String> {
        self.directory.remove_subdir(name)
    }

    /// Enter a subdirectory. Returns false if it doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        self.directory.enter_subdir(name)
    }

    /// Move up one directory level.
    pub fn updir(&mut self) {
        self.directory.updir();
    }

    /// Move to root directory.
    pub fn home(&mut self) {
        self.directory.home();
    }

    /// Get the current directory path.
    pub fn dir_path(&self) -> &[String] {
        self.directory.dir_path()
    }

    // === Path-based operations (for library data) ===

    /// Store a value at an absolute path (creates intermediate directories).
    pub fn store_at_path(&mut self, path: &[&str], name: &str, value: Value) {
        self.directory.store_at_path(path, name, value);
    }

    /// Look up a value at an absolute path.
    pub fn lookup_at_path(&self, path: &[&str], name: &str) -> Option<&Value> {
        self.directory.lookup_at_path(path, name)
    }

    /// Purge a value at an absolute path.
    pub fn purge_at_path(&mut self, path: &[&str], name: &str) -> Option<Value> {
        self.directory.purge_at_path(path, name)
    }

    /// Clear all variables at an absolute path.
    pub fn clear_at_path(&mut self, path: &[&str]) {
        self.directory.clear_at_path(path);
    }
}

/// Result of executing a command.
pub type ExecuteResult = Result<(), String>;

// ============================================================================
// Library Trait
// ============================================================================

/// Unified trait for RPL libraries.
///
/// Libraries can provide any combination of:
/// - Commands (arithmetic, stack ops, etc.)
/// - Custom syntax parsing (IF/THEN/ELSE, FOR loops)
/// - Bytecode lowering
/// - Runtime execution
/// - Type analysis
///
/// All methods have default implementations, so libraries only override
/// what they need.
pub trait Library: Send + Sync {
    /// Get the library ID.
    fn id(&self) -> LibId;

    /// Get the library name.
    fn name(&self) -> &'static str;

    /// Get the commands provided by this library.
    fn commands(&self) -> Vec<CommandInfo> {
        Vec::new()
    }

    // ---- Parser methods (override for custom syntax) ----

    /// Get token claims for custom syntax (e.g., "IF", "FOR").
    ///
    /// Default: no claims (not a parser).
    fn claims(&self) -> &[TokenClaim] {
        &[]
    }

    /// Parse starting from a claimed token.
    ///
    /// Called when the parser encounters a token claimed by this library.
    /// Default: returns an error (not a parser).
    fn parse(
        &self,
        _token: &str,
        _ctx: &mut crate::parse::ParseContext,
    ) -> Result<crate::ir::Node, crate::parse::ParseError> {
        Err(crate::parse::ParseError::new(
            "library does not support parsing",
            Span::default(),
        ))
    }

    // ---- Lowerer methods (override for custom bytecode) ----

    /// Lower an extended composite to bytecode.
    ///
    /// Called for `CompositeKind::Extended(lib_id, id)` nodes.
    /// Default: returns an error (no composites).
    fn lower_composite(
        &self,
        _id: u16,
        _branches: &[Branch],
        span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "library does not support composites".into(),
            span: Some(span),
        })
    }

    /// Lower a command by emitting bytecode.
    ///
    /// Default: emits a CallLib instruction.
    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(self.id(), cmd);
        Ok(())
    }

    // ---- Executor methods (override for runtime behavior) ----

    /// Execute a command at runtime.
    ///
    /// Called when the VM encounters a CallLib instruction.
    /// Default: returns an error (no runtime support).
    fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
        Err("library does not support execution".into())
    }

    // ---- Analyzer methods (override for type tracking) ----

    /// Compute the stack effect for a command.
    ///
    /// Used for type inference during compilation and analysis.
    /// Default: returns Dynamic (unknown effect).
    fn command_effect(&self, _cmd: u16, _types: &crate::types::CStack) -> StackEffect {
        StackEffect::Dynamic
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn claim_context_equality() {
        assert_eq!(ClaimContext::Any, ClaimContext::Any);
        assert_ne!(ClaimContext::Any, ClaimContext::NotInfix);
    }

    #[test]
    fn token_claim_creation() {
        let claim = TokenClaim {
            token: "IF",
            priority: 100,
            context: ClaimContext::NotInfix,
            lib_id: 9,
        };
        assert_eq!(claim.token, "IF");
        assert_eq!(claim.priority, 100);
    }

    #[test]
    fn stack_effect_notation_permutation() {
        assert_eq!(StackEffect::dup().to_notation(), "(a -- a a)");
        assert_eq!(StackEffect::drop().to_notation(), "(a -- )");
        assert_eq!(StackEffect::swap().to_notation(), "(a b -- b a)");
        assert_eq!(StackEffect::rot().to_notation(), "(a b c -- b c a)");
        assert_eq!(StackEffect::over().to_notation(), "(a b -- a b a)");
    }

    #[test]
    fn stack_effect_notation_fixed() {
        use crate::core::TypeId;
        // Depth: produces integer, consumes nothing
        assert_eq!(StackEffect::depth().to_notation(), "( -- int)");
        // Comparison: consumes 2, produces int
        let cmp = StackEffect::fixed(2, &[Some(TypeId::BINT)]);
        assert_eq!(cmp.to_notation(), "(a b -- int)");
        // Unknown result type
        let unknown = StackEffect::fixed(1, &[None]);
        assert_eq!(unknown.to_notation(), "(a -- ?)");
    }

    #[test]
    fn stack_effect_notation_dynamic() {
        assert_eq!(StackEffect::Dynamic.to_notation(), "(dynamic)");
    }
}
