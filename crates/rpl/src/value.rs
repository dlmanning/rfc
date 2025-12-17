//! Calculator value types.
//!
//! RPL supports several value types on the stack:
//! - Numeric: Integer, Real
//! - Containers: String, List
//! - Code: Program (compiled bytecode)
//! - Symbolic: Unevaluated algebraic expressions

use std::fmt;
use std::sync::Arc;

use crate::serialize::SourceMap;
use crate::symbolic::SymExpr;

/// Compiled program data with bytecode, string table, and optional debug info.
#[derive(Clone, Debug)]
pub struct ProgramData {
    /// The bytecode.
    pub code: Arc<[u8]>,
    /// String table (data section) for local variable names, etc.
    pub strings: Arc<[String]>,
    /// Optional source map for debugging (maps bytecode offsets to source spans).
    pub source_map: Option<SourceMap>,
}

impl ProgramData {
    /// Create a new program with just bytecode (empty string table, no debug info).
    pub fn new(code: impl Into<Arc<[u8]>>) -> Self {
        Self {
            code: code.into(),
            strings: Arc::new([]),
            source_map: None,
        }
    }

    /// Create a new program with bytecode and string table (no debug info).
    pub fn with_strings(code: impl Into<Arc<[u8]>>, strings: impl Into<Arc<[String]>>) -> Self {
        Self {
            code: code.into(),
            strings: strings.into(),
            source_map: None,
        }
    }

    /// Create a new program with bytecode, string table, and source map.
    pub fn with_source_map(
        code: impl Into<Arc<[u8]>>,
        strings: impl Into<Arc<[String]>>,
        source_map: SourceMap,
    ) -> Self {
        Self {
            code: code.into(),
            strings: strings.into(),
            source_map: Some(source_map),
        }
    }

    /// Check if this program has debug info (source map).
    pub fn has_debug_info(&self) -> bool {
        self.source_map.is_some()
    }

    /// Get the source offset for a bytecode PC.
    ///
    /// Returns the start offset of the span containing this PC,
    /// or None if no source map or no span covers this PC.
    pub fn source_offset_for_pc(&self, pc: usize) -> Option<u32> {
        let source_map = self.source_map.as_ref()?;
        // Binary search to find the span containing this PC
        let idx = source_map.offsets.partition_point(|&o| (o as usize) <= pc);
        if idx > 0 {
            Some(source_map.spans[idx - 1].start().offset())
        } else {
            None
        }
    }
}

/// A command within a user library.
#[derive(Clone, Debug)]
pub struct LibraryCommand {
    /// Command name (uppercase, e.g., "DBL").
    pub name: String,
    /// Compiled bytecode.
    pub code: Arc<[u8]>,
    /// String table for local variables, etc.
    pub strings: Arc<[String]>,
}

impl LibraryCommand {
    /// Create a new library command.
    pub fn new(
        name: impl Into<String>,
        code: impl Into<Arc<[u8]>>,
        strings: impl Into<Arc<[String]>>,
    ) -> Self {
        Self {
            name: name.into(),
            code: code.into(),
            strings: strings.into(),
        }
    }
}

/// User library data: a collection of named commands.
#[derive(Clone, Debug)]
pub struct LibraryData {
    /// Library ID (1-4 uppercase alphanumeric chars, e.g., "MATH").
    pub id: String,
    /// Commands in this library.
    pub commands: Vec<LibraryCommand>,
}

impl LibraryData {
    /// Create a new library with the given ID.
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            commands: Vec::new(),
        }
    }

    /// Create a library with commands.
    pub fn with_commands(id: impl Into<String>, commands: Vec<LibraryCommand>) -> Self {
        Self {
            id: id.into(),
            commands,
        }
    }

    /// Find a command by name (case-insensitive).
    pub fn find_command(&self, name: &str) -> Option<(usize, &LibraryCommand)> {
        let name_upper = name.to_uppercase();
        self.commands
            .iter()
            .enumerate()
            .find(|(_, cmd)| cmd.name == name_upper)
    }
}

/// A value on the calculator stack.
#[derive(Clone, Debug)]
pub enum Value {
    /// 64-bit signed integer.
    Integer(i64),
    /// 64-bit floating point.
    Real(f64),
    /// Immutable string.
    String(Arc<str>),
    /// A list of values.
    List(Arc<[Value]>),
    /// A compiled program (bytecode with string table).
    Program(Arc<ProgramData>),
    /// A symbolic expression (unevaluated algebraic expression).
    Symbolic(Arc<SymExpr>),
    /// A user library (collection of named commands).
    Library(Arc<LibraryData>),
    /// Raw bytes (for PACKDIR, sprites, binary data, etc.).
    Bytes(Arc<[u8]>),
}

impl Value {
    /// Create an integer value.
    pub fn integer(n: i64) -> Self {
        Value::Integer(n)
    }

    /// Create a real value.
    pub fn real(n: f64) -> Self {
        Value::Real(n)
    }

    /// Create a string value.
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Value::String(s.into())
    }

    /// Create a list value.
    pub fn list(items: impl Into<Arc<[Value]>>) -> Self {
        Value::List(items.into())
    }

    /// Create a program value with just bytecode (empty string table).
    pub fn program(code: impl Into<Arc<[u8]>>) -> Self {
        Value::Program(Arc::new(ProgramData::new(code)))
    }

    /// Create a program value with bytecode and string table.
    pub fn program_with_strings(
        code: impl Into<Arc<[u8]>>,
        strings: impl Into<Arc<[String]>>,
    ) -> Self {
        Value::Program(Arc::new(ProgramData::with_strings(code, strings)))
    }

    /// Create a program value with bytecode, string table, and source map.
    pub fn program_with_source_map(
        code: impl Into<Arc<[u8]>>,
        strings: impl Into<Arc<[String]>>,
        source_map: SourceMap,
    ) -> Self {
        Value::Program(Arc::new(ProgramData::with_source_map(code, strings, source_map)))
    }

    /// Create a symbolic expression value.
    pub fn symbolic(expr: SymExpr) -> Self {
        Value::Symbolic(Arc::new(expr))
    }

    /// Create a symbolic expression value from an Arc.
    pub fn symbolic_arc(expr: Arc<SymExpr>) -> Self {
        Value::Symbolic(expr)
    }

    /// Create a library value.
    pub fn library(data: LibraryData) -> Self {
        Value::Library(Arc::new(data))
    }

    /// Create a library value from an Arc.
    pub fn library_arc(data: Arc<LibraryData>) -> Self {
        Value::Library(data)
    }

    /// Create a bytes value.
    pub fn bytes(data: impl Into<Arc<[u8]>>) -> Self {
        Value::Bytes(data.into())
    }

    /// Try to get as integer.
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Value::Integer(n) => Some(*n),
            _ => None,
        }
    }

    /// Try to get as real.
    pub fn as_real(&self) -> Option<f64> {
        match self {
            Value::Real(n) => Some(*n),
            Value::Integer(n) => Some(*n as f64),
            _ => None,
        }
    }

    /// Try to get as string.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Check if value is numeric (integer or real).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Real(_))
    }

    /// Type name for error messages.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Real(_) => "real",
            Value::String(_) => "string",
            Value::List(_) => "list",
            Value::Program(_) => "program",
            Value::Symbolic(_) => "symbolic",
            Value::Library(_) => "library",
            Value::Bytes(_) => "bytes",
        }
    }

    /// Try to get as symbolic expression.
    pub fn as_symbolic(&self) -> Option<&SymExpr> {
        match self {
            Value::Symbolic(expr) => Some(expr),
            _ => None,
        }
    }

    /// Try to get as list.
    pub fn as_list(&self) -> Option<&Arc<[Value]>> {
        match self {
            Value::List(items) => Some(items),
            _ => None,
        }
    }

    /// Try to get as library.
    pub fn as_library(&self) -> Option<&Arc<LibraryData>> {
        match self {
            Value::Library(lib) => Some(lib),
            _ => None,
        }
    }

    /// Try to get as program.
    pub fn as_program(&self) -> Option<&Arc<ProgramData>> {
        match self {
            Value::Program(prog) => Some(prog),
            _ => None,
        }
    }

    /// Try to get as bytes.
    pub fn as_bytes(&self) -> Option<&Arc<[u8]>> {
        match self {
            Value::Bytes(data) => Some(data),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Real(n) => {
                if n.fract() == 0.0 && n.abs() < 1e15 {
                    write!(f, "{}.", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(items) => {
                write!(f, "{{ ")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, " }}")
            }
            Value::Program(_) => write!(f, "<< ... >>"),
            Value::Symbolic(expr) => write!(f, "'{}'", expr),
            Value::Library(lib) => write!(f, "Library:{}", lib.id),
            Value::Bytes(data) => write!(f, "Bytes:{}", data.len()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a == b,
            (Value::Integer(a), Value::Real(b)) => (*a as f64) == *b,
            (Value::Real(a), Value::Integer(b)) => *a == (*b as f64),
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Program(a), Value::Program(b)) => Arc::ptr_eq(a, b),
            (Value::Symbolic(a), Value::Symbolic(b)) => a == b,
            (Value::Library(a), Value::Library(b)) => Arc::ptr_eq(a, b),
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_basics() {
        let v = Value::integer(42);
        assert_eq!(v.as_integer(), Some(42));
        assert_eq!(v.as_real(), Some(42.0));
        assert!(v.is_numeric());
        assert_eq!(v.type_name(), "integer");
        assert_eq!(format!("{}", v), "42");
    }

    #[test]
    fn real_basics() {
        let v = Value::real(3.14);
        assert_eq!(v.as_integer(), None);
        assert_eq!(v.as_real(), Some(3.14));
        assert!(v.is_numeric());
        assert_eq!(v.type_name(), "real");
    }

    #[test]
    fn string_basics() {
        let v = Value::string("hello");
        assert_eq!(v.as_string(), Some("hello"));
        assert!(!v.is_numeric());
        assert_eq!(v.type_name(), "string");
        assert_eq!(format!("{}", v), "\"hello\"");
    }

    #[test]
    fn equality() {
        assert_eq!(Value::integer(5), Value::integer(5));
        assert_eq!(Value::integer(5), Value::real(5.0));
        assert_ne!(Value::integer(5), Value::integer(6));
        assert_eq!(Value::string("hi"), Value::string("hi"));
    }
}
