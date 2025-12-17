//! Static analysis for RPL2.
//!
//! This module provides static analysis capabilities for RPL2 programs,
//! working on the IR (intermediate representation) rather than raw tokens.
//!
//! # Features
//!
//! - **Symbol tracking**: Track variable definitions and references
//! - **Scope analysis**: Build scope trees for local bindings
//! - **Diagnostics**: Report undefined variables, unused definitions, etc.
//!
//! # Architecture
//!
//! The analysis works on parsed IR nodes:
//!
//! ```text
//! Source → Parse → IR → Analysis → AnalysisResult
//!                       ↓
//!                  SymbolTable + ScopeTree + Diagnostics
//! ```
//!
//! This is cleaner than token-level analysis because:
//! - Scopes are explicit in the IR structure (programs, local bindings)
//! - Variable definitions are structured, not pattern-matched
//! - The tree structure enables proper scope resolution
//!
//! # Example
//!
//! ```ignore
//! use rpl::analysis::{analyze, AnalysisResult};
//! use rpl::parse::parse;
//! use rpl::registry::Registry;
//!
//! let registry = Registry::with_core();
//! let mut interner = Interner::new();
//! let nodes = parse("42 \"x\" STO x", &registry, &mut interner).unwrap();
//! let result = analyze(&nodes, &interner);
//!
//! // Result contains symbol table, scopes, and diagnostics
//! assert_eq!(result.symbols.definition_count(), 1);
//! ```

mod analyzer;
mod incremental;
mod result;
mod scopes;
mod symbols;
mod visitor;

pub use analyzer::analyze;
pub use incremental::{IncrementalAnalysis, SpanEdit, line_edit_to_span_edit};
pub use result::{AnalysisResult, Diagnostic, DiagnosticKind, Severity};
pub use scopes::{Scope, ScopeId, ScopeKind, ScopeTree};
pub use symbols::{
    Definition, DefinitionId, DefinitionKind, Reference, ReferenceId, ReferenceKind, SymbolTable,
};
pub use visitor::{Visitor, walk_node, walk_nodes};
