use thiserror::Error;

/// Error codes for diagnostics.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Error)]
pub enum ErrorCode {
    // Lexical errors (E001-E099)
    #[error("unrecognized token")]
    E001,
    #[error("unterminated string")]
    E002,

    // Parse errors (E100-E199)
    #[error("unexpected token")]
    E100,
    #[error("unclosed construct")]
    E101,
    #[error("mismatched bracket")]
    E102,
}

impl ErrorCode {
    /// Get the error code as a string (e.g., "E001").
    pub fn as_str(self) -> &'static str {
        match self {
            ErrorCode::E001 => "E001",
            ErrorCode::E002 => "E002",
            ErrorCode::E100 => "E100",
            ErrorCode::E101 => "E101",
            ErrorCode::E102 => "E102",
        }
    }

    /// Get a short description of the error.
    pub fn message(self) -> &'static str {
        match self {
            ErrorCode::E001 => "unrecognized token",
            ErrorCode::E002 => "unterminated string",
            ErrorCode::E100 => "unexpected token",
            ErrorCode::E101 => "unclosed construct",
            ErrorCode::E102 => "mismatched bracket",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_code_as_str() {
        assert_eq!(ErrorCode::E001.as_str(), "E001");
        assert_eq!(ErrorCode::E002.as_str(), "E002");
        assert_eq!(ErrorCode::E100.as_str(), "E100");
        assert_eq!(ErrorCode::E101.as_str(), "E101");
        assert_eq!(ErrorCode::E102.as_str(), "E102");
    }

    #[test]
    fn error_code_message() {
        assert_eq!(ErrorCode::E001.message(), "unrecognized token");
        assert_eq!(ErrorCode::E002.message(), "unterminated string");
        assert_eq!(ErrorCode::E100.message(), "unexpected token");
    }

    #[test]
    fn error_code_display() {
        assert_eq!(format!("{}", ErrorCode::E001), "unrecognized token");
        assert_eq!(format!("{}", ErrorCode::E002), "unterminated string");
    }
}
