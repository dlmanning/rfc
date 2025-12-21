//! AST for library interface declarations.
//!
//! The unified syntax is: `inputs -> PATTERN -> outputs`

/// A complete library interface.
#[derive(Debug, Clone, PartialEq)]
pub struct Library {
    pub name: String,
    pub id: u16,
    pub declarations: Vec<Declaration>,
}

/// Binding effect for commands that create/read/modify global definitions.
///
/// Used to annotate input types that represent variable names:
/// - `$Sym` = defines a binding (STO)
/// - `@Sym` = reads a binding (RCL)
/// - `~Sym` = deletes a binding (PURGE)
/// - `!Sym` = modifies a binding (INCR, DECR)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// Creates a new definition ($Type).
    Define,
    /// Reads an existing definition (@Type).
    Read,
    /// Deletes a definition (~Type).
    Delete,
    /// Modifies a definition in place (!Type).
    Modify,
}

/// A single declaration: `id: inputs -> pattern -> outputs`
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    /// Explicit ID for this declaration (command or construct).
    pub id: u16,
    /// Stack inputs (left of first ->).
    pub inputs: Vec<Type>,
    /// The pattern (command name or syntax construct).
    pub pattern: Pattern,
    /// Stack outputs (right of second ->).
    pub outputs: Vec<Type>,
}

/// A type in the signature.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Concrete type: Int, Real, Str, List, Prog, Sym, Plot.
    Concrete(ConcreteType),
    /// Type variable: a, b, c, ...
    Var(char),
    /// Constrained type variable: a:(Int | Real), b:(List | Str), etc.
    ConstrainedVar(char, Box<Type>),
    /// Dynamic/unknown: ...
    Dynamic,
    /// Numeric computed type: Numeric a b.
    Numeric(char, char),
    /// Union: a | b.
    Union(Box<Type>, Box<Type>),
    /// A type with a binding effect annotation ($Type, @Type, ~Type, !Type).
    Binding(BindingKind, Box<Type>),
}

/// Concrete types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConcreteType {
    Int,
    Real,
    Str,
    List,
    Prog,
    Sym,
    Blob,
    Any,
}

/// The pattern part of a declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    /// Command names (e.g., ["DUP"] or ["+", "ADD"]).
    pub names: Vec<String>,
    /// Slots for syntax constructs (empty for simple commands).
    pub slots: Vec<PatternElement>,
}

/// An element in a syntax pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum PatternElement {
    /// Keyword token (e.g., THEN, END, NEXT).
    Keyword(String),
    /// Optional keyword token (e.g., ELSE?).
    OptionalKeyword(String),
    /// Required keyword that captures from previous branch (e.g., STEP!).
    KeywordWithCapture(String),
    /// Optional keyword that captures from previous branch (e.g., STEP?!).
    OptionalKeywordWithCapture(String),
    /// Typed slot: `name:Type`.
    Slot { name: String, typ: ConcreteType },
    /// Binding slot: `$name:Type`.
    Binding { name: String, typ: ConcreteType },
    /// Repeating group: `(elements)*` - repeat until next keyword seen.
    Repeat(Vec<PatternElement>),
    /// Alternation: `( A | B )` - one of the alternatives is required.
    Alternation(Vec<Vec<PatternElement>>),
}

impl ConcreteType {
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "Int" => Some(ConcreteType::Int),
            "Real" => Some(ConcreteType::Real),
            "Str" => Some(ConcreteType::Str),
            "List" => Some(ConcreteType::List),
            "Prog" => Some(ConcreteType::Prog),
            "Sym" => Some(ConcreteType::Sym),
            "Blob" => Some(ConcreteType::Blob),
            "Any" => Some(ConcreteType::Any),
            _ => None,
        }
    }
}

impl std::fmt::Display for ConcreteType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConcreteType::Int => write!(f, "ℤ"),
            ConcreteType::Real => write!(f, "ℝ"),
            ConcreteType::Str => write!(f, "Str"),
            ConcreteType::List => write!(f, "List"),
            ConcreteType::Prog => write!(f, "Prog"),
            ConcreteType::Sym => write!(f, "Sym"),
            ConcreteType::Blob => write!(f, "Blob"),
            ConcreteType::Any => write!(f, "∀"),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Concrete(c) => write!(f, "{}", c),
            Type::Var(v) => write!(f, "{}", v),
            Type::ConstrainedVar(v, constraint) => {
                write!(f, "{}:({})", v, constraint)
            }
            Type::Dynamic => write!(f, "..."),
            Type::Numeric(a, b) => write!(f, "Numeric {} {}", a, b),
            Type::Union(a, b) => write!(f, "{} ∪ {}", a, b),
            Type::Binding(kind, inner) => {
                let prefix = match kind {
                    BindingKind::Define => '$',
                    BindingKind::Read => '@',
                    BindingKind::Delete => '~',
                    BindingKind::Modify => '!',
                };
                write!(f, "{}{}", prefix, inner)
            }
        }
    }
}
