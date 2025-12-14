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
    /// Prolog decompilation definitions
    pub prologs: Vec<PrologDef>,
    /// Control flow pattern definitions
    pub control_patterns: Vec<ControlPatternDef>,
    /// Coercion registrations
    pub coercions: Vec<CoercionDef>,
    /// Custom probe implementation (optional)
    pub custom_probe: Option<TokenStream>,
    /// Custom compile implementation (optional)
    pub custom_compile: Option<TokenStream>,
    /// Custom decompile prolog handler (optional)
    pub custom_decompile_prolog: Option<TokenStream>,
    /// Custom decompile handler (optional, replaces entire decompile implementation)
    pub custom_decompile: Option<TokenStream>,
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

/// A prolog decompilation definition.
///
/// Defines how to decompile object prologs (the first word that identifies type and size).
#[derive(Debug)]
pub struct PrologDef {
    /// The type ID this prolog handles (e.g., `LIST`, `REAL`)
    pub type_id: Ident,
    /// How to decompile this prolog
    pub kind: PrologKind,
}

/// The kind of prolog decompilation.
#[derive(Debug)]
pub enum PrologKind {
    /// Delimited construct: emit open, decompile body, emit close
    /// Example: `TypeId::LIST => delimited("{", "}")`
    Delimited { open: String, close: String },
    /// Format using a codec: decode data words and format to string
    /// Example: `TypeId::REAL => format(RealCodec)`
    Format(Path),
    /// Custom decompilation logic
    /// Example: `TypeId::SYMBOLIC => custom { ... }`
    Custom(TokenStream),
}

/// A control pattern definition.
///
/// Defines a high-level flow control pattern that generates keywords,
/// internal commands, and all necessary handlers.
#[derive(Debug)]
pub struct ControlPatternDef {
    /// Pattern type
    pub pattern: ControlPattern,
    /// Positional keywords (e.g., [IF, THEN, END] for if_then_else)
    pub keywords: Vec<Ident>,
    /// Optional modifiers
    pub options: ControlPatternOptions,
}

/// The type of control pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlPattern {
    /// IF/THEN/END conditional pattern (with optional ELSE via options.alt)
    IfThenElse,
    /// DO/UNTIL loop (test at end, loops while false)
    DoUntil,
    /// WHILE/REPEAT loop (test at start, loops while true)
    WhileRepeat,
    /// START/NEXT definite loop (with optional STEP via options.step)
    StartNext,
    /// FOR/NEXT loop with named counter variable
    ForNext,
    /// Inline conditional (IFT/IFTE)
    InlineConditional,
    /// CASE/THENCASE/ENDTHEN/ENDCASE multi-way branch
    Case,
    /// IFERR/THENERR/ENDERR error handling (with optional ELSEERR via options.no_error)
    ErrorHandler,
}

impl ControlPattern {
    /// Expected number of positional keyword arguments.
    pub fn arity(&self) -> usize {
        match self {
            ControlPattern::IfThenElse => 3,    // IF, THEN, END
            ControlPattern::DoUntil => 2,       // DO, UNTIL
            ControlPattern::WhileRepeat => 2,   // WHILE, REPEAT
            ControlPattern::StartNext => 2,     // START, NEXT
            ControlPattern::ForNext => 2,       // FOR, NEXT
            ControlPattern::InlineConditional => 2, // IFT, IFTE
            ControlPattern::Case => 4,          // CASE, THEN, END_BRANCH, END
            ControlPattern::ErrorHandler => 3,  // IFERR, THEN, END
        }
    }
}

/// Optional modifiers for control patterns.
#[derive(Debug, Default)]
pub struct ControlPatternOptions {
    /// Alternative branch keyword for if_then_else (ELSE)
    pub alt: Option<Ident>,
    /// Step variant keyword for start_next (STEP)
    pub step: Option<Ident>,
    /// No-error branch keyword for error_handler (ELSEERR)
    pub no_error: Option<Ident>,
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
