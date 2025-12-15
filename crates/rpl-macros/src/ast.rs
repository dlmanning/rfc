//! AST types for the define_library! macro.

use proc_macro2::TokenStream;
use syn::{Ident, LitInt, LitStr, Path};

/// A complete library definition.
#[derive(Debug)]
pub struct LibraryDef {
    /// Visibility (e.g., `pub`)
    pub vis: syn::Visibility,
    /// Struct name (e.g., `StackLib`)
    pub name: Ident,
    /// Library ID (e.g., `72`)
    pub id: LitInt,
    /// Display name (e.g., `"Stack"`)
    pub display_name: LitStr,
    /// Literal definitions (e.g., `real: RealCodec;`)
    pub literals: Vec<LiteralDef>,
    /// Command definitions
    pub commands: Vec<CommandDef>,
    /// Construct definitions (e.g., `{` and `}` for lists)
    pub constructs: Vec<ConstructDef>,
    /// Operator registrations
    pub operators: Vec<OperatorDef>,
    /// Operator syntax definitions (for syntax libraries like ArithmeticLib)
    pub operator_syntax: Vec<OperatorSyntaxDef>,
    /// Control flow declarations (openers, transitions, closers, inlines)
    pub control_flow: ControlFlowDef,
    /// Coercion registrations
    pub coercions: Vec<CoercionDef>,
    /// Custom probe implementation (optional)
    pub custom_probe: Option<TokenStream>,
    /// Custom compile implementation (optional)
    pub custom_compile: Option<TokenStream>,
    /// Custom stack_effect handler (optional, replaces entire stack_effect implementation)
    pub custom_stack_effect: Option<TokenStream>,
}

/// A literal codec definition.
///
/// Maps a name to a codec type that implements `LiteralCodec`.
#[derive(Debug)]
pub struct LiteralDef {
    /// Name for this literal (e.g., `real`, `string`)
    pub name: Ident,
    /// Path to the codec type (e.g., `RealCodec`, `crate::codecs::StringCodec`)
    pub codec: Path,
}

/// A construct delimiter definition (e.g., `{` for list open).
#[derive(Debug)]
pub struct ConstructDef {
    /// The token string (e.g., `{`, `}`, `«`, `»`)
    pub token: String,
    /// The action this construct performs
    pub action: ConstructAction,
}

/// The action a construct delimiter performs.
#[derive(Debug)]
pub enum ConstructAction {
    /// Open a construct of the given kind (e.g., `List`, `Program`)
    Open(String),
    /// Close the current construct
    Close,
}

/// A command definition within a library.
#[derive(Debug)]
pub struct CommandDef {
    /// Command name (e.g., `DUP`)
    pub name: Ident,
    /// Alternative names for the command (e.g., `["BLOB→LIST", "BLOB->LIST"]`)
    pub aliases: Vec<String>,
    /// Whether this is an internal command (prefixed with @)
    pub internal: bool,
    /// Stack effect
    pub effect: StackEffectDef,
    /// Documentation
    pub doc: DocDef,
    /// Execute body
    pub body: TokenStream,
}

/// Stack effect definition.
#[derive(Debug, Clone)]
pub enum StackEffectDef {
    /// Fixed stack effect: (consumes -> produces)
    Fixed { consumes: u8, produces: u8 },
    /// Dynamic stack effect
    Dynamic,
}

/// Documentation for a command.
#[derive(Debug, Default)]
pub struct DocDef {
    /// Brief description
    pub brief: String,
    /// Stack notation (e.g., "( a -- a a )")
    pub stack: Option<String>,
    /// Example code
    pub example: Option<String>,
    /// Related commands
    pub see_also: Vec<String>,
    /// Single alias from options block (will be merged with command aliases)
    pub alias: Option<String>,
}

/// An operator registration definition.
#[derive(Debug)]
pub struct OperatorDef {
    /// Operator kind (Add, Sub, Neg, etc.)
    pub kind: String,
    /// Command constant name (e.g., CMD_ADD or just ADD)
    pub command: String,
    /// Operator signature
    pub signature: OpSigDef,
    /// Result type (defaults to same as signature type)
    pub result_type: Option<String>,
    /// Priority (defaults to 100)
    pub priority: u8,
    /// Is this operator commutative?
    pub commutative: bool,
}

/// Operator signature definition.
#[derive(Debug)]
pub enum OpSigDef {
    /// Symmetric binary: same type for both operands
    Symmetric(String),
    /// Unary operator
    Unary(String),
    /// Asymmetric binary: different types
    Binary { left: String, right: String },
}

// =============================================================================
// Control Flow DSL Types
// =============================================================================
// The control_flow block uses an explicit, role-based DSL for defining
// control flow keywords. Each keyword has one of four roles:
//
// - opener: Starts a construct (IF, DO, START, FOR, etc.)
// - transition: Changes state within a construct (THEN, ELSE, REPEAT, etc.)
// - closer: Ends a construct (END, UNTIL, NEXT, STEP, etc.)
// - inline: Stack-based conditionals (IFT, IFTE)
//
// This explicit structure makes the parser enforce correct syntax and
// makes the generated code obvious.

/// A control flow opener declaration.
///
/// Openers start new constructs. They may optionally emit setup bytecode.
/// Example: `opener START (2 -> 0) => Start { emit LOOP_SETUP };`
#[derive(Debug)]
pub struct ControlOpener {
    /// The keyword (e.g., IF, START, FOR)
    pub keyword: Ident,
    /// Stack effect (defaults to 0 -> 0)
    pub effect: Option<StackEffectDef>,
    /// The construct kind to start (e.g., If, Start, For)
    pub construct: Ident,
    /// Optional setup commands to emit (e.g., LOOP_SETUP)
    pub emit: Vec<Ident>,
}

/// A control flow transition declaration.
///
/// Transitions emit bytecode and return NeedMore to stay in the construct.
/// Example: `transition THEN (1 -> 0) => emit JUMP_IF_FALSE;`
/// Example: `transition ELSE (0 -> 0) in [If] => emit JUMP;`
#[derive(Debug)]
pub struct ControlTransition {
    /// The keyword (e.g., THEN, ELSE, REPEAT)
    pub keyword: Ident,
    /// Stack effect (required for transitions that consume values)
    pub effect: Option<StackEffectDef>,
    /// Which construct kinds this transition is valid in (empty = all)
    pub valid_in: Vec<Ident>,
    /// Commands to emit (e.g., JUMP_IF_FALSE, JUMP)
    pub emit: Vec<Ident>,
}

/// A control flow closer declaration.
///
/// Closers end constructs. They may emit bytecode (like NEXT emitting LOOP_NEXT).
/// Shared closers (END, NEXT, STEP) have multiple entries for different constructs.
/// Example: `closer END in [If, While];`
/// Example: `closer NEXT in [Start] => emit LOOP_NEXT;`
/// Example: `closer NEXT in [For, ForUp, ForDn] => emit FOR_NEXT;`
#[derive(Debug)]
pub struct ControlCloser {
    /// The keyword (e.g., END, NEXT, STEP)
    pub keyword: Ident,
    /// Stack effect (defaults to 0 -> 0)
    pub effect: Option<StackEffectDef>,
    /// Which construct kinds this closer handles (required)
    pub valid_in: Vec<Ident>,
    /// Commands to emit (e.g., LOOP_NEXT, FOR_NEXT)
    pub emit: Vec<Ident>,
}

/// A control flow inline declaration.
///
/// Inline commands are regular commands with fixed stack effects.
/// They don't involve construct management.
/// Example: `inline IFT (2 -> 1);`
#[derive(Debug)]
pub struct ControlInline {
    /// The keyword (e.g., IFT, IFTE)
    pub keyword: Ident,
    /// Stack effect (required)
    pub effect: StackEffectDef,
}

/// Container for all control flow declarations in a library.
#[derive(Debug, Default)]
pub struct ControlFlowDef {
    /// Opener declarations
    pub openers: Vec<ControlOpener>,
    /// Transition declarations
    pub transitions: Vec<ControlTransition>,
    /// Closer declarations
    pub closers: Vec<ControlCloser>,
    /// Inline command declarations
    pub inlines: Vec<ControlInline>,
}

/// A coercion registration definition.
///
/// Defines a type coercion from one type to another.
#[derive(Debug)]
pub struct CoercionDef {
    /// Source type (e.g., `BINT`)
    pub from: Ident,
    /// Target type (e.g., `REAL`)
    pub to: Ident,
    /// Command constant name (e.g., `CMD_BINT_TO_REAL`)
    pub command: Ident,
    /// Priority (lower = preferred, defaults to 100)
    pub priority: u8,
    /// Is this coercion implicit (used automatically by operator dispatch)?
    pub implicit: bool,
}

/// An operator syntax definition for syntax libraries.
///
/// Used by libraries like ArithmeticLib that recognize operators
/// but don't execute them (execution is handled by type-owning libraries).
#[derive(Debug)]
pub struct OperatorSyntaxDef {
    /// The token(s) to match (e.g., "+", "<=", "NEG")
    pub tokens: Vec<String>,
    /// Operator kind (Add, Sub, Neg, etc.)
    pub kind: String,
    /// Arity (unary or binary)
    pub arity: Arity,
    /// Infix precedence - None = not infix, Some(n) = infix with precedence
    pub infix_precedence: Option<u8>,
    /// Whether the operator is right-associative (like ^)
    pub right_assoc: bool,
}

/// Operator arity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    Unary,
    Binary,
}
