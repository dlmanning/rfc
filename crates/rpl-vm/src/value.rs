use std::sync::Arc;

use rpl_core::{Span, Symbol, TypeId, Word};

/// Debug information for a compiled program.
#[derive(Clone, Debug, PartialEq)]
pub struct ProgramDebugInfo {
    /// Source spans for each bytecode word.
    pub spans: Vec<Span>,
    /// Original source code (for display).
    pub source: Option<String>,
    /// Source file name.
    pub source_name: Option<String>,
}

/// Runtime value on the stack.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// Real number (64-bit float).
    Real(f64),
    /// Integer (64-bit signed).
    Int(i64),
    /// Boolean.
    Bool(bool),
    /// Interned symbol/name.
    Symbol(Symbol),
    /// String value.
    String(String),
    /// List of values.
    List(Vec<Value>),
    /// Compiled program with optional debug info.
    /// The Arc allows sharing debug info between cloned values without duplication.
    Program {
        code: Vec<Word>,
        debug_info: Option<Arc<ProgramDebugInfo>>,
    },
    /// Object with type and data words (for other object types).
    Object { type_id: TypeId, data: Vec<Word> },
}

impl Value {
    /// Get the type ID of this value.
    pub fn type_id(&self) -> TypeId {
        match self {
            Value::Real(_) => TypeId::REAL,
            Value::Int(_) => TypeId::BINT,
            Value::Bool(_) => TypeId::BINT, // Booleans are integers in RPL
            Value::Symbol(_) => TypeId::SYMBOLIC,
            Value::String(_) => TypeId::STRING,
            Value::List(_) => TypeId::LIST,
            Value::Program { .. } => TypeId::PROGRAM,
            Value::Object { type_id, .. } => *type_id,
        }
    }

    /// Create a program value without debug info.
    pub fn program(code: Vec<Word>) -> Self {
        Value::Program {
            code,
            debug_info: None,
        }
    }

    /// Create a program value with debug info.
    pub fn program_with_debug(code: Vec<Word>, debug_info: ProgramDebugInfo) -> Self {
        Value::Program {
            code,
            debug_info: Some(Arc::new(debug_info)),
        }
    }

    /// Try to get as a program (code and optional debug info).
    pub fn as_program(&self) -> Option<(&[Word], Option<&ProgramDebugInfo>)> {
        match self {
            Value::Program { code, debug_info } => {
                Some((code.as_slice(), debug_info.as_ref().map(|d| d.as_ref())))
            }
            Value::Object { type_id, data } if *type_id == TypeId::PROGRAM => {
                Some((data.as_slice(), None))
            }
            _ => None,
        }
    }

    /// Try to get as a real number.
    pub fn as_real(&self) -> Option<f64> {
        match self {
            Value::Real(v) => Some(*v),
            Value::Int(v) => Some(*v as f64),
            Value::Bool(v) => Some(if *v { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    /// Try to get as an integer.
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(v) => Some(*v),
            Value::Real(v) => Some(*v as i64),
            Value::Bool(v) => Some(if *v { 1 } else { 0 }),
            _ => None,
        }
    }

    /// Try to get as a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(v) => Some(*v),
            Value::Int(v) => Some(*v != 0),
            Value::Real(v) => Some(*v != 0.0),
            _ => None,
        }
    }

    /// Check if this value is truthy.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Real(v) => *v != 0.0,
            Value::Int(v) => *v != 0,
            Value::Bool(v) => *v,
            Value::Symbol(_) => true,
            Value::String(_) => true,
            Value::List(_) => true,
            Value::Program { .. } => true,
            Value::Object { .. } => true,
        }
    }

    /// Try to get as a list.
    pub fn as_list(&self) -> Option<&Vec<Value>> {
        match self {
            Value::List(v) => Some(v),
            _ => None,
        }
    }

    /// Try to get as a string.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Create a list value.
    pub fn list(elements: Vec<Value>) -> Self {
        Value::List(elements)
    }

    /// Create a string value.
    pub fn string(s: impl Into<String>) -> Self {
        Value::String(s.into())
    }

    /// Create a real value.
    pub fn real(v: f64) -> Self {
        Value::Real(v)
    }

    /// Create an integer value.
    pub fn int(v: i64) -> Self {
        Value::Int(v)
    }

    /// Create a boolean value.
    pub fn bool(v: bool) -> Self {
        Value::Bool(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Real(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Int(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_type_id() {
        assert_eq!(Value::Real(3.15).type_id(), TypeId::REAL);
        assert_eq!(Value::Int(42).type_id(), TypeId::BINT);
        assert_eq!(Value::Bool(true).type_id(), TypeId::BINT);
        assert_eq!(Value::String("hello".to_string()).type_id(), TypeId::STRING);
        assert_eq!(Value::List(vec![]).type_id(), TypeId::LIST);
    }

    #[test]
    fn value_as_real() {
        assert_eq!(Value::Real(3.15).as_real(), Some(3.15));
        assert_eq!(Value::Int(42).as_real(), Some(42.0));
        assert_eq!(Value::Bool(true).as_real(), Some(1.0));
        assert_eq!(Value::Bool(false).as_real(), Some(0.0));
    }

    #[test]
    fn value_as_int() {
        assert_eq!(Value::Int(42).as_int(), Some(42));
        assert_eq!(Value::Real(3.7).as_int(), Some(3));
        assert_eq!(Value::Bool(true).as_int(), Some(1));
    }

    #[test]
    fn value_is_truthy() {
        assert!(Value::Real(1.0).is_truthy());
        assert!(!Value::Real(0.0).is_truthy());
        assert!(Value::Int(1).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
    }

    #[test]
    fn value_from_primitives() {
        let v: Value = 3.15.into();
        assert_eq!(v, Value::Real(3.15));

        let v: Value = 42i64.into();
        assert_eq!(v, Value::Int(42));

        let v: Value = true.into();
        assert_eq!(v, Value::Bool(true));
    }

    #[test]
    fn value_constructors() {
        assert_eq!(Value::real(3.15), Value::Real(3.15));
        assert_eq!(Value::int(42), Value::Int(42));
        assert_eq!(Value::bool(true), Value::Bool(true));
        assert_eq!(
            Value::list(vec![Value::Int(1), Value::Int(2)]),
            Value::List(vec![Value::Int(1), Value::Int(2)])
        );
    }

    #[test]
    fn value_as_list() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(list.as_list(), Some(&vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(Value::Int(42).as_list(), None);
    }

    #[test]
    fn value_list_is_truthy() {
        assert!(Value::list(vec![]).is_truthy());
        assert!(Value::list(vec![Value::Int(1)]).is_truthy());
    }

    #[test]
    fn value_as_string() {
        let s = Value::string("hello");
        assert_eq!(s.as_string(), Some("hello"));
        assert_eq!(Value::Int(42).as_string(), None);
    }

    #[test]
    fn value_string_is_truthy() {
        assert!(Value::string("").is_truthy());
        assert!(Value::string("hello").is_truthy());
    }

    #[test]
    fn value_string_constructor() {
        assert_eq!(Value::string("hello"), Value::String("hello".to_string()));
        // Also works with String
        assert_eq!(
            Value::string("world".to_string()),
            Value::String("world".to_string())
        );
    }
}
