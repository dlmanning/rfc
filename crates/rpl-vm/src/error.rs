use rpl_core::TypeId;

/// Stack operation errors.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackError {
    /// Stack underflow - tried to pop from empty stack.
    Underflow,
    /// Stack overflow - exceeded maximum depth.
    Overflow,
    /// Return stack underflow.
    ReturnUnderflow,
    /// Return stack type mismatch - expected Call but got Loop or vice versa.
    ReturnTypeMismatch,
    /// Type error - expected a different type.
    TypeError {
        expected: &'static str,
        got: TypeId,
    },
}

impl std::fmt::Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackError::Underflow => write!(f, "Stack underflow"),
            StackError::Overflow => write!(f, "Stack overflow"),
            StackError::ReturnUnderflow => write!(f, "Return stack underflow"),
            StackError::ReturnTypeMismatch => write!(f, "Return stack type mismatch"),
            StackError::TypeError { expected, got } => {
                write!(f, "Type error: expected {}, got {:?}", expected, got)
            }
        }
    }
}

impl std::error::Error for StackError {}

/// Runtime error during execution.
#[derive(Clone, Debug)]
pub enum RuntimeError {
    /// Stack error (underflow, overflow, type mismatch).
    Stack(StackError),
    /// Unknown library.
    UnknownLibrary(u16),
    /// Unknown command in library.
    UnknownCommand { lib: u16, cmd: u16 },
    /// Unknown type in prolog.
    UnknownType(TypeId),
    /// Library-reported error.
    LibraryError(String),
    /// Division by zero.
    DivisionByZero,
    /// Invalid bytecode.
    InvalidBytecode(String),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Stack(e) => write!(f, "{}", e),
            RuntimeError::UnknownLibrary(id) => write!(f, "Unknown library: {}", id),
            RuntimeError::UnknownCommand { lib, cmd } => {
                write!(f, "Unknown command {} in library {}", cmd, lib)
            }
            RuntimeError::UnknownType(id) => write!(f, "Unknown type: {:?}", id),
            RuntimeError::LibraryError(msg) => write!(f, "{}", msg),
            RuntimeError::DivisionByZero => write!(f, "Division by zero"),
            RuntimeError::InvalidBytecode(msg) => write!(f, "Invalid bytecode: {}", msg),
        }
    }
}

impl std::error::Error for RuntimeError {}

impl From<StackError> for RuntimeError {
    fn from(e: StackError) -> Self {
        RuntimeError::Stack(e)
    }
}
