//! Library infrastructure for extending RPL.
//!
//! This module defines the traits and types that make up the library system.
//! Libraries provide:
//! - Token claims for custom syntax (IF, FOR, etc.)
//! - Commands (stack operations, arithmetic, etc.)
//! - Lowering for extended composites
//! - Runtime execution
//!
//! The standard library implementations are in the `rpl-stdlib` crate.

use crate::interface::BindingKind;
use crate::ir::{Branch, LibId};

// ============================================================================
// Well-known Library IDs
// ============================================================================
// These are defined here so the core can reference them without depending
// on rpl-stdlib. The stdlib implementations must use these same IDs.

/// Program library ID (EVAL command).
pub const PROG_LIB: LibId = 8;

/// Directory library ID (STO, RCL, etc.).
pub const DIRECTORY_LIB: LibId = 28;

/// Arithmetic library ID (+, -, *, /, comparisons).
pub const ARITH_LIB: LibId = 64;

/// Stack library ID (DUP, DROP, SWAP, etc.).
pub const STACK_LIB: LibId = 72;

/// Well-known command IDs for PROG_LIB.
pub mod prog_cmd {
    pub const EVAL: u16 = 0;
}

/// Well-known command IDs for DIRECTORY_LIB.
pub mod dir_cmd {
    pub const STO: u16 = 0;
    pub const RCL: u16 = 1;
    pub const PURGE: u16 = 2;
    pub const INCR: u16 = 10;
    pub const DECR: u16 = 11;
}

/// Well-known command IDs for ARITH_LIB.
pub mod arith_cmd {
    pub const ADD: u16 = 0;
    pub const SUB: u16 = 1;
    pub const MUL: u16 = 2;
    pub const DIV: u16 = 3;
    pub const NEG: u16 = 4;
    pub const GT: u16 = 12;
}
use crate::lower::{LowerContext, LowerError};
use crate::value::Value;
use crate::vm::directory::Directory;
use crate::vm::stack::Stack;
use crate::vm::RplException;
use crate::core::{Span, TypeId};
use smallvec::{SmallVec, smallvec};

// ============================================================================
// Stack Effects
// ============================================================================

/// Stack effect of a command.
///
/// Used for type inference during lowering and validation.
/// Returned by `LibraryLowerer::lower_command()` after emitting code,
/// so the effect can reflect the actual types based on input types.
///
/// # Type Preservation with FromInput
///
/// For stack manipulation commands, we track how input types flow to outputs.
/// The `FromInput(i)` result type indicates that an output copies its type
/// from input position `i` (0 = deepest consumed).
///
/// Result type for stack effects.
///
/// Represents what type a command produces on the stack:
/// - `Unknown`: type cannot be determined statically
/// - `Known(TypeId)`: exact type is known
/// - `OneOf(types)`: type is one of the specified types (e.g., Numeric = Int|Real)
/// - `FromInput(i)`: type is copied from input at position i (0 = deepest consumed)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultType {
    Unknown,
    Known(TypeId),
    OneOf(SmallVec<[TypeId; 2]>),
    /// Output type is copied from input at position i (0 = deepest consumed).
    /// Used for type-preserving operations like DUP, SWAP, NEG.
    FromInput(u8),
}

impl ResultType {
    /// Create a Numeric result type (Int | Real).
    pub fn numeric() -> Self {
        Self::OneOf(smallvec![TypeId::BINT, TypeId::REAL])
    }

    /// Create a Known result type.
    pub fn known(ty: TypeId) -> Self {
        Self::Known(ty)
    }

    /// Create a FromInput result type (output copies input at position i).
    pub fn from_input(i: u8) -> Self {
        Self::FromInput(i)
    }
}

impl From<Option<TypeId>> for ResultType {
    fn from(opt: Option<TypeId>) -> Self {
        match opt {
            Some(ty) => ResultType::Known(ty),
            None => ResultType::Unknown,
        }
    }
}

/// Stack effect representation.
///
/// Examples using FromInput:
/// - `SWAP (a b -- b a)`: Fixed { consumes: 2, results: [FromInput(1), FromInput(0)] }
/// - `DUP (a -- a a)`: Fixed { consumes: 1, results: [FromInput(0), FromInput(0)] }
/// - `DROP (a --)`: Fixed { consumes: 1, results: [] }
/// - `SIZE (list -- int)`: Fixed { consumes: 1, results: [Known(BINT)] }
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackEffect {
    /// Fixed number of inputs with known result types.
    ///
    /// - `consumes`: number of items consumed from stack
    /// - `results`: types of produced items (can reference inputs via FromInput)
    Fixed {
        consumes: u8,
        results: SmallVec<[ResultType; 4]>,
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
            results: results.iter().map(|&opt| ResultType::from(opt)).collect(),
        }
    }

    /// Create a fixed effect with ResultType results.
    ///
    /// Use this for effects that need OneOf types (e.g., Numeric).
    pub fn fixed_result(consumes: u8, results: &[ResultType]) -> Self {
        Self::Fixed {
            consumes,
            results: results.iter().cloned().collect(),
        }
    }

    /// Get the number of items consumed.
    pub fn consumes(&self) -> Option<u8> {
        match self {
            Self::Fixed { consumes, .. } => Some(*consumes),
            Self::Dynamic => None,
        }
    }

    /// Get the number of items produced.
    pub fn produces(&self) -> Option<u8> {
        match self {
            Self::Fixed { results, .. } => Some(results.len() as u8),
            Self::Dynamic => None,
        }
    }

    /// Get the result types (for Fixed effects).
    pub fn results(&self) -> Option<&[ResultType]> {
        match self {
            Self::Fixed { results, .. } => Some(results),
            _ => None,
        }
    }

    // Common stack manipulation effects using FromInput
    pub fn dup() -> Self { Self::fixed_result(1, &[ResultType::from_input(0), ResultType::from_input(0)]) }
    pub fn drop() -> Self { Self::fixed_result(1, &[]) }
    pub fn swap() -> Self { Self::fixed_result(2, &[ResultType::from_input(1), ResultType::from_input(0)]) }
    pub fn rot() -> Self { Self::fixed_result(3, &[ResultType::from_input(1), ResultType::from_input(2), ResultType::from_input(0)]) }
    pub fn over() -> Self { Self::fixed_result(2, &[ResultType::from_input(0), ResultType::from_input(1), ResultType::from_input(0)]) }
    pub fn nip() -> Self { Self::fixed_result(2, &[ResultType::from_input(1)]) }  // (a b -- b)
    pub fn tuck() -> Self { Self::fixed_result(2, &[ResultType::from_input(1), ResultType::from_input(0), ResultType::from_input(1)]) }  // (a b -- b a b)

    /// DEPTH: (-- n) produces integer, doesn't consume anything
    pub fn depth() -> Self { Self::fixed(0, &[Some(TypeId::BINT)]) }

    /// Format the stack effect in traditional notation: `(inputs -- outputs)`
    pub fn to_notation(&self) -> String {
        match self {
            Self::Fixed { consumes, results } => {
                // Use letters for inputs, type names or input refs for outputs
                let inputs_str: Vec<String> = (0..*consumes)
                    .map(|i| ((b'a' + i) as char).to_string())
                    .collect();
                let outputs_str: Vec<String> = results
                    .iter()
                    .map(|ty| match ty {
                        ResultType::Known(TypeId::BINT) => "int".to_string(),
                        ResultType::Known(TypeId::REAL) => "real".to_string(),
                        ResultType::Known(TypeId::STRING) => "str".to_string(),
                        ResultType::Known(TypeId::LIST) => "list".to_string(),
                        ResultType::Known(TypeId::PROGRAM) => "prog".to_string(),
                        ResultType::Known(TypeId::SYMBOLIC) => "sym".to_string(),
                        ResultType::Known(_) => "?".to_string(),
                        ResultType::OneOf(types) if types.contains(&TypeId::BINT) && types.contains(&TypeId::REAL) => "num".to_string(),
                        ResultType::OneOf(_) => "union".to_string(),
                        ResultType::Unknown => "?".to_string(),
                        ResultType::FromInput(i) => ((b'a' + i) as char).to_string(),
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

/// Compute the stack effect for a binary numeric operation.
///
/// This is the single source of truth for determining result types
/// of binary numeric operations like +, -, *, /, etc.
///
/// Rules:
/// - If both operands are integers → integer result
/// - If either operand is real (or mixed int/real) → real result
/// - Otherwise → numeric result (Int | Real), since the operation is known to be numeric
pub fn binary_numeric_effect(tos: Option<TypeId>, nos: Option<TypeId>) -> StackEffect {
    let tos_is_int = tos == Some(TypeId::BINT);
    let nos_is_int = nos == Some(TypeId::BINT);
    let tos_is_real = tos == Some(TypeId::REAL);
    let nos_is_real = nos == Some(TypeId::REAL);

    if tos_is_int && nos_is_int {
        StackEffect::fixed_result(2, &[ResultType::known(TypeId::BINT)])
    } else if tos_is_real || nos_is_real {
        StackEffect::fixed_result(2, &[ResultType::known(TypeId::REAL)])
    } else {
        // Even when we can't determine the exact type, we know it's numeric
        StackEffect::fixed_result(2, &[ResultType::numeric()])
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
/// - Otherwise → numeric result (preserves numeric nature even when exact type is unknown)
pub fn unary_preserving_effect(tos: Option<TypeId>) -> StackEffect {
    if tos == Some(TypeId::BINT) {
        StackEffect::fixed_result(1, &[ResultType::known(TypeId::BINT)])
    } else if tos == Some(TypeId::REAL) {
        StackEffect::fixed_result(1, &[ResultType::known(TypeId::REAL)])
    } else {
        // Even when we can't determine the exact type, we know it's numeric
        StackEffect::fixed_result(1, &[ResultType::numeric()])
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
    pub token: String,
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
        // Create a slice of Unknown types for the produces count
        let results: SmallVec<[ResultType; 4]> = (0..produces).map(|_| ResultType::Unknown).collect();
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

use std::sync::Arc;
use crate::symbolic::SymExpr;
use crate::value::ProgramData;

/// Action for VM to take after executor runs.
#[derive(Debug)]
pub enum ExecuteAction {
    /// Continue normal execution.
    Continue,
    /// Request VM to call a program.
    CallProgram {
        program: Arc<ProgramData>,
        /// Name for error messages and intercept (e.g., "MAIN").
        name: Option<String>,
        /// If true, preserve current locals (for EVAL semantics).
        /// If false, create a new local scope (for function call semantics).
        preserve_locals: bool,
    },
    /// Request VM to evaluate a symbolic expression.
    /// This needs access to locals which executors don't have.
    EvalSymbolic {
        expr: Arc<SymExpr>,
    },
}

impl ExecuteAction {
    /// Create a Continue action.
    pub fn ok() -> Self {
        Self::Continue
    }

    /// Create a CallProgram action for function calls (new local scope).
    pub fn call(program: Arc<ProgramData>, name: Option<String>) -> Self {
        Self::CallProgram { program, name, preserve_locals: false }
    }

    /// Create a CallProgram action for EVAL (preserve current locals).
    pub fn eval(program: Arc<ProgramData>) -> Self {
        Self::CallProgram { program, name: None, preserve_locals: true }
    }

    /// Create an EvalSymbolic action.
    pub fn eval_symbolic(expr: Arc<SymExpr>) -> Self {
        Self::EvalSymbolic { expr }
    }
}

/// Result of executing a command.
pub type ExecuteResult = Result<ExecuteAction, String>;

// ============================================================================
// Library Traits
// ============================================================================

/// Interface trait: declares what commands exist, their names, effects, and syntax claims.
///
/// This trait describes the "what" of a library - its identity and the commands it provides.
/// Implement this separately from `LibraryImpl` to cleanly separate interface from implementation.
pub trait LibraryInterface: Send + Sync {
    /// Get the library ID.
    fn id(&self) -> LibId;

    /// Get the library name.
    fn name(&self) -> &'static str;

    /// Get the commands provided by this library.
    fn commands(&self) -> Vec<CommandInfo> {
        Vec::new()
    }

    /// Get token claims for custom syntax (e.g., "IF", "FOR").
    fn claims(&self) -> Vec<TokenClaim> {
        Vec::new()
    }

    /// Compute the stack effect for a command.
    ///
    /// Used for type inference during compilation and analysis.
    /// `tos` and `nos` are the top-of-stack and next-on-stack types if known.
    fn command_effect(&self, _cmd: u16, _tos: Option<TypeId>, _nos: Option<TypeId>) -> StackEffect {
        StackEffect::Dynamic
    }

    /// Parse starting from a claimed token.
    ///
    /// Called when the parser encounters a token claimed by this library.
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

    /// Get the indices of binding branches for a construct.
    ///
    /// Given a construct ID and the actual number of branches, returns
    /// which branch indices contain local variable bindings. Used by the
    /// analyzer to determine scope structure without hardcoding library IDs.
    ///
    /// Returns empty vec if the construct has no bindings or is unknown.
    fn binding_branches(&self, _construct_id: u16, _num_branches: usize) -> Vec<usize> {
        Vec::new()
    }

    /// Get the binding effect for a command, if any.
    ///
    /// Returns the binding effect (Define, Read, Delete, Modify) for commands
    /// that create, read, or modify global definitions. Used by the analyzer
    /// to track variable definitions and references without hardcoding library IDs.
    fn binding_effect(&self, _cmd: u16) -> Option<BindingKind> {
        None
    }

    /// Get the input type constraints for a command.
    ///
    /// Returns a vector of TypeConstraint, one per input. Used by the analyzer
    /// for constraint-based type inference on local variables.
    fn input_constraints(&self, _cmd: u16) -> Vec<crate::types::TypeConstraint> {
        Vec::new()
    }

    /// Get alternative branch groups for a construct.
    ///
    /// Returns groups of branch indices that are mutually exclusive at runtime.
    /// The analyzer will save the type stack before visiting these branches,
    /// visit each one, and then merge the resulting stacks.
    ///
    /// For example, IF/THEN/ELSE returns [[1, 2]] meaning branches 1 (then)
    /// and 2 (else) are alternatives. The analyzer will:
    /// 1. Visit branch 0 (condition) normally
    /// 2. Save stack state
    /// 3. Visit branch 1, record resulting stack
    /// 4. Restore stack state
    /// 5. Visit branch 2, record resulting stack
    /// 6. Merge the two resulting stacks
    ///
    /// Returns empty vec if all branches are sequential.
    fn alternative_branches(&self, _construct_id: u16, _num_branches: usize) -> Vec<Vec<usize>> {
        Vec::new()
    }

    /// Check if a construct is a loop (FOR, START, etc.).
    ///
    /// Loop constructs have special handling in the analyzer:
    /// - Loop variables use DefinitionKind::LoopVar instead of Local
    /// - Loop body scope is ScopeKind::Loop
    fn is_loop_construct(&self, _construct_id: u16) -> bool {
        false
    }

    /// Get the stack effect for a syntax construct.
    ///
    /// Returns the number of inputs consumed and outputs produced by a construct.
    /// For example, FOR consumes 2 values (start, end) and produces 0 values.
    fn construct_effect(&self, _construct_id: u16) -> StackEffect {
        StackEffect::Dynamic
    }
}

/// Lowering trait: compiles commands and composites to bytecode.
///
/// Implement this for libraries that need custom lowering behavior.
/// Libraries that only use default lowering (emit CallLib) don't need this.
pub trait LibraryLowerer: Send + Sync {
    /// Get the library ID.
    fn id(&self) -> LibId;

    /// Lower a command by emitting bytecode.
    ///
    /// Default implementation emits a CallLib instruction for runtime execution.
    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(self.id(), cmd);
        Ok(())
    }

    /// Lower an extended composite to bytecode.
    ///
    /// Called for `CompositeKind::Extended(lib_id, construct_id)` nodes.
    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "library does not support composites".into(),
            span: Some(span),
        })
    }
}

/// Executor trait: executes commands at runtime.
///
/// Implement this for libraries that need runtime execution.
/// Libraries that are entirely compile-time (like FlowLib) don't need this.
pub trait LibraryExecutor: Send + Sync {
    /// Get the library ID.
    fn id(&self) -> LibId;

    /// Execute a command at runtime.
    ///
    /// Called when the VM encounters a CallLib instruction.
    /// Return `ExecuteAction::Continue` for normal flow, or
    /// `ExecuteAction::CallProgram` to request program execution.
    fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
        Err("library does not support execution".into())
    }
}

/// Combined trait for libraries that need both lowering and execution.
///
/// This is a convenience trait for the common case where a library
/// provides both custom lowering and runtime execution. Any type
/// implementing both `LibraryLowerer` and `LibraryExecutor` automatically
/// implements `LibraryImpl`.
pub trait LibraryImpl: LibraryLowerer + LibraryExecutor {}

// Blanket impl: any type implementing both traits gets LibraryImpl
impl<T: LibraryLowerer + LibraryExecutor> LibraryImpl for T {}


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
            token: "IF".to_string(),
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
