//! Core types for the RPL language.
//!
//! This crate provides foundational types used throughout the RPL compiler and runtime:
//! - Symbols and interning
//! - Source spans and positions
//! - Type IDs and word encoding
//! - Token information
//! - Diagnostics and error codes

pub mod core;
pub mod error;
pub mod token;

// Re-export commonly used types at crate root
pub use core::{
    Interner, Pos, Span, Spanned, Symbol, TypeId, Word,
    extract_cmd, extract_lib, extract_size, extract_type, is_prolog, make_call, make_prolog,
};
pub use error::{Diagnostic, DiagnosticBuilder, ErrorCode, Severity, Suggestion};
pub use token::{SemanticKind, TokenInfo, TokenType};
