//! Analysis result types.
//!
//! This module contains the result of running static analysis on RPL code,
//! including the symbol table, scope tree, and any diagnostics.

use crate::core::Span;

use super::scopes::ScopeTree;
use super::symbols::SymbolTable;

/// Severity level of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Severity {
    /// An error that likely indicates incorrect code.
    Error,
    /// A warning about potential issues.
    Warning,
    /// Informational hint.
    Hint,
}

/// The kind of diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DiagnosticKind {
    /// Reference to an undefined variable.
    UndefinedVariable,
    /// Variable defined but never used.
    UnusedVariable,
    /// Variable shadowing another variable.
    ShadowedVariable,
    /// Write to a variable that's never read.
    WriteOnlyVariable,
    /// Duplicate definition in same scope.
    DuplicateDefinition,
    /// Stack underflow - operation requires more items than available.
    StackUnderflow,
}

impl DiagnosticKind {
    /// Get the default severity for this diagnostic kind.
    pub fn default_severity(self) -> Severity {
        match self {
            DiagnosticKind::UndefinedVariable => Severity::Error,
            DiagnosticKind::DuplicateDefinition => Severity::Error,
            DiagnosticKind::StackUnderflow => Severity::Error,
            DiagnosticKind::UnusedVariable => Severity::Warning,
            DiagnosticKind::ShadowedVariable => Severity::Warning,
            DiagnosticKind::WriteOnlyVariable => Severity::Warning,
        }
    }
}

/// A diagnostic message from analysis.
#[derive(Clone, Debug)]
pub struct Diagnostic {
    /// The kind of diagnostic.
    pub kind: DiagnosticKind,
    /// Severity level.
    pub severity: Severity,
    /// The source span.
    pub span: Span,
    /// The diagnostic message.
    pub message: String,
    /// Optional related location (e.g., the original definition for shadowing).
    pub related: Option<Span>,
}

impl Diagnostic {
    /// Create a new diagnostic.
    pub fn new(kind: DiagnosticKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            severity: kind.default_severity(),
            span,
            message: message.into(),
            related: None,
        }
    }

    /// Create a diagnostic with a related span.
    pub fn with_related(
        kind: DiagnosticKind,
        span: Span,
        message: impl Into<String>,
        related: Span,
    ) -> Self {
        Self {
            kind,
            severity: kind.default_severity(),
            span,
            message: message.into(),
            related: Some(related),
        }
    }

    /// Create an undefined variable error.
    pub fn undefined_variable(name: &str, span: Span) -> Self {
        Self::new(
            DiagnosticKind::UndefinedVariable,
            span,
            format!("Undefined variable: {}", name),
        )
    }

    /// Create an unused variable warning.
    pub fn unused_variable(name: &str, span: Span) -> Self {
        Self::new(
            DiagnosticKind::UnusedVariable,
            span,
            format!("Variable '{}' is defined but never used", name),
        )
    }

    /// Create a shadowed variable warning.
    pub fn shadowed_variable(name: &str, span: Span, original: Span) -> Self {
        Self::with_related(
            DiagnosticKind::ShadowedVariable,
            span,
            format!("Variable '{}' shadows an outer variable", name),
            original,
        )
    }

    /// Create a write-only variable warning.
    pub fn write_only_variable(name: &str, span: Span) -> Self {
        Self::new(
            DiagnosticKind::WriteOnlyVariable,
            span,
            format!("Variable '{}' is written but never read", name),
        )
    }

    /// Create a duplicate definition error.
    pub fn duplicate_definition(name: &str, span: Span, original: Span) -> Self {
        Self::with_related(
            DiagnosticKind::DuplicateDefinition,
            span,
            format!("Variable '{}' is already defined in this scope", name),
            original,
        )
    }

    /// Create a stack underflow error.
    pub fn stack_underflow(span: Span, needed: usize, available: usize) -> Self {
        Self::new(
            DiagnosticKind::StackUnderflow,
            span,
            format!(
                "stack underflow: operation requires {} item(s) but stack has {}",
                needed, available
            ),
        )
    }

    /// Check if this is an error.
    pub fn is_error(&self) -> bool {
        matches!(self.severity, Severity::Error)
    }

    /// Check if this is a warning.
    pub fn is_warning(&self) -> bool {
        matches!(self.severity, Severity::Warning)
    }
}

/// The result of analyzing RPL code.
#[derive(Clone, Debug)]
pub struct AnalysisResult {
    /// Symbol table with all definitions and references.
    pub symbols: SymbolTable,
    /// Scope tree.
    pub scopes: ScopeTree,
    /// Diagnostics produced during analysis.
    pub diagnostics: Vec<Diagnostic>,
}

impl Default for AnalysisResult {
    fn default() -> Self {
        Self::new()
    }
}

impl AnalysisResult {
    /// Create a new empty analysis result.
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            scopes: ScopeTree::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Add a diagnostic.
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_error())
    }

    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_warning())
    }

    /// Get all errors.
    pub fn errors(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter().filter(|d| d.is_error())
    }

    /// Get all warnings.
    pub fn warnings(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter().filter(|d| d.is_warning())
    }

    /// Get the number of definitions.
    pub fn definition_count(&self) -> usize {
        self.symbols.definition_count()
    }

    /// Get the number of references.
    pub fn reference_count(&self) -> usize {
        self.symbols.reference_count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn diagnostic_severity() {
        assert_eq!(
            DiagnosticKind::UndefinedVariable.default_severity(),
            Severity::Error
        );
        assert_eq!(
            DiagnosticKind::UnusedVariable.default_severity(),
            Severity::Warning
        );
    }

    #[test]
    fn diagnostic_undefined_variable() {
        let diag = Diagnostic::undefined_variable("x", make_span(0, 5));
        assert!(diag.is_error());
        assert!(!diag.is_warning());
        assert!(diag.message.contains("Undefined"));
        assert!(diag.message.contains("x"));
    }

    #[test]
    fn diagnostic_with_related() {
        let diag = Diagnostic::shadowed_variable("x", make_span(10, 15), make_span(0, 5));
        assert!(diag.related.is_some());
        assert_eq!(diag.related.unwrap(), make_span(0, 5));
    }

    #[test]
    fn analysis_result_empty() {
        let result = AnalysisResult::new();
        assert!(!result.has_errors());
        assert!(!result.has_warnings());
        assert_eq!(result.definition_count(), 0);
        assert_eq!(result.reference_count(), 0);
    }

    #[test]
    fn analysis_result_with_diagnostics() {
        let mut result = AnalysisResult::new();
        result.add_diagnostic(Diagnostic::undefined_variable("x", make_span(0, 5)));
        result.add_diagnostic(Diagnostic::unused_variable("y", make_span(10, 15)));

        assert!(result.has_errors());
        assert!(result.has_warnings());
        assert_eq!(result.errors().count(), 1);
        assert_eq!(result.warnings().count(), 1);
    }
}
