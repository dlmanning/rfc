use super::code::ErrorCode;
use crate::core::Span;

/// Severity level of a diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

/// A suggested fix for a diagnostic.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Suggestion {
    message: String,
    span: Span,
    replacement: String,
}

impl Suggestion {
    pub fn new(message: String, span: Span, replacement: String) -> Self {
        Self {
            message,
            span,
            replacement,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn replacement(&self) -> &str {
        &self.replacement
    }
}

/// A diagnostic message with location and context.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Diagnostic {
    severity: Severity,
    code: ErrorCode,
    message: String,
    span: Span,
    label: Option<String>,
    secondary: Vec<(Span, String)>,
    suggestions: Vec<Suggestion>,
    notes: Vec<String>,
}

impl Diagnostic {
    /// Start building an error diagnostic.
    pub fn error(code: ErrorCode, span: Span) -> DiagnosticBuilder {
        DiagnosticBuilder::new(Severity::Error, code, span)
    }

    /// Start building a warning diagnostic.
    pub fn warning(code: ErrorCode, span: Span) -> DiagnosticBuilder {
        DiagnosticBuilder::new(Severity::Warning, code, span)
    }

    /// Start building a note diagnostic.
    pub fn note(code: ErrorCode, span: Span) -> DiagnosticBuilder {
        DiagnosticBuilder::new(Severity::Note, code, span)
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn code(&self) -> ErrorCode {
        self.code
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn label(&self) -> Option<&str> {
        self.label.as_deref()
    }

    pub fn secondary(&self) -> &[(Span, String)] {
        &self.secondary
    }

    pub fn suggestions(&self) -> &[Suggestion] {
        &self.suggestions
    }

    pub fn notes(&self) -> &[String] {
        &self.notes
    }
}

/// Builder for constructing diagnostics.
pub struct DiagnosticBuilder {
    severity: Severity,
    code: ErrorCode,
    span: Span,
    message: Option<String>,
    label: Option<String>,
    secondary: Vec<(Span, String)>,
    suggestions: Vec<Suggestion>,
    notes: Vec<String>,
}

impl DiagnosticBuilder {
    fn new(severity: Severity, code: ErrorCode, span: Span) -> Self {
        Self {
            severity,
            code,
            span,
            message: None,
            label: None,
            secondary: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Set the main message.
    pub fn message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    /// Set the primary label.
    pub fn label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    /// Add a secondary label.
    pub fn secondary(mut self, span: Span, label: impl Into<String>) -> Self {
        self.secondary.push((span, label.into()));
        self
    }

    /// Add a suggestion.
    pub fn suggestion(
        mut self,
        message: impl Into<String>,
        span: Span,
        replacement: impl Into<String>,
    ) -> Self {
        self.suggestions
            .push(Suggestion::new(message.into(), span, replacement.into()));
        self
    }

    /// Add a note.
    pub fn note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Build the diagnostic.
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            severity: self.severity,
            code: self.code,
            message: self
                .message
                .unwrap_or_else(|| self.code.message().to_string()),
            span: self.span,
            label: self.label,
            secondary: self.secondary,
            suggestions: self.suggestions,
            notes: self.notes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    #[test]
    fn builder_basic() {
        let span = Span::new(Pos::new(10), Pos::new(15));
        let diag = Diagnostic::error(ErrorCode::E001, span)
            .message("custom message")
            .build();

        assert_eq!(diag.severity(), Severity::Error);
        assert_eq!(diag.code(), ErrorCode::E001);
        assert_eq!(diag.message(), "custom message");
        assert_eq!(diag.span(), span);
    }

    #[test]
    fn builder_default_message() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::error(ErrorCode::E001, span).build();

        assert_eq!(diag.message(), "unrecognized token");
    }

    #[test]
    fn builder_with_label() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::error(ErrorCode::E001, span)
            .label("this token is not recognized")
            .build();

        assert_eq!(diag.label(), Some("this token is not recognized"));
    }

    #[test]
    fn builder_with_secondary() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let span2 = Span::new(Pos::new(10), Pos::new(15));
        let diag = Diagnostic::error(ErrorCode::E102, span)
            .secondary(span2, "opened here")
            .build();

        assert_eq!(diag.secondary().len(), 1);
        assert_eq!(diag.secondary()[0].0, span2);
        assert_eq!(diag.secondary()[0].1, "opened here");
    }

    #[test]
    fn builder_with_suggestion() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::error(ErrorCode::E001, span)
            .suggestion("try this instead", span, "DUP")
            .build();

        assert_eq!(diag.suggestions().len(), 1);
        assert_eq!(diag.suggestions()[0].message(), "try this instead");
        assert_eq!(diag.suggestions()[0].replacement(), "DUP");
    }

    #[test]
    fn builder_with_notes() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::error(ErrorCode::E001, span)
            .note("first note")
            .note("second note")
            .build();

        assert_eq!(diag.notes().len(), 2);
        assert_eq!(diag.notes()[0], "first note");
        assert_eq!(diag.notes()[1], "second note");
    }

    #[test]
    fn warning_severity() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::warning(ErrorCode::E001, span).build();
        assert_eq!(diag.severity(), Severity::Warning);
    }

    #[test]
    fn note_severity() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let diag = Diagnostic::note(ErrorCode::E001, span).build();
        assert_eq!(diag.severity(), Severity::Note);
    }
}
