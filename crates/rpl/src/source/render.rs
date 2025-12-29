//! Diagnostic rendering using codespan-reporting.

use std::io::{self, Write};

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{self, termcolor::NoColor};

use super::SourceFile;
use crate::error::Diagnostic;
use crate::source::SourceId;

/// Renders diagnostics to a writer using codespan-reporting.
pub struct DiagnosticRenderer<'a> {
    source: &'a SourceFile,
}

impl<'a> DiagnosticRenderer<'a> {
    pub fn new(source: &'a SourceFile) -> Self {
        Self { source }
    }

    /// Render a diagnostic to the given writer.
    ///
    /// Note: This uses a non-colored output. For colored output, use
    /// `emit_diagnostic` with a `termcolor::StandardStream`.
    pub fn render<W: Write>(&self, diag: &Diagnostic, out: &mut W) -> io::Result<()> {
        let file = SimpleFile::new(self.source.name(), self.source.source());
        let cs_diag = diag.to_codespan(self.source.id());

        // Convert codespan diagnostic with SourceId to one with () for SimpleFile
        let cs_diag = convert_diagnostic(&cs_diag);

        let config = term::Config::default();
        let mut writer = NoColor::new(out);

        term::emit(&mut writer, &config, &file, &cs_diag)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
    }

    /// Render a diagnostic to a string.
    pub fn render_to_string(&self, diag: &Diagnostic) -> String {
        let mut buf = Vec::new();
        self.render(diag, &mut buf)
            .expect("writing to Vec cannot fail");
        String::from_utf8(buf).expect("output is valid UTF-8")
    }
}

/// Convert a diagnostic with SourceId to one with () for use with SimpleFile.
fn convert_diagnostic(
    diag: &codespan_reporting::diagnostic::Diagnostic<SourceId>,
) -> codespan_reporting::diagnostic::Diagnostic<()> {
    use codespan_reporting::diagnostic::{Diagnostic as CsDiag, Label};

    let mut new_diag = CsDiag::new(diag.severity);

    if let Some(ref code) = diag.code {
        new_diag = new_diag.with_code(code.clone());
    }

    new_diag = new_diag.with_message(&diag.message);

    let labels: Vec<_> = diag
        .labels
        .iter()
        .map(|label| {
            let mut new_label = if label.style == codespan_reporting::diagnostic::LabelStyle::Primary
            {
                Label::primary((), label.range.clone())
            } else {
                Label::secondary((), label.range.clone())
            };
            new_label.message = label.message.clone();
            new_label
        })
        .collect();

    new_diag = new_diag.with_labels(labels);
    new_diag = new_diag.with_notes(diag.notes.clone());

    new_diag
}

/// Emit a diagnostic using the full codespan-reporting API with color support.
///
/// This is the preferred way to render diagnostics when you have access to
/// a `SourceCache` and want colored output.
pub fn emit_diagnostic<W: codespan_reporting::term::termcolor::WriteColor>(
    files: &super::SourceCache,
    file_id: SourceId,
    diag: &Diagnostic,
    writer: &mut W,
) -> io::Result<()> {
    let cs_diag = diag.to_codespan(file_id);
    let config = term::Config::default();

    term::emit(writer, &config, files, &cs_diag)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Pos, Span};
    use crate::error::ErrorCode;

    fn make_source(content: &str) -> SourceFile {
        SourceFile::new(SourceId::new(0), "test.rpl".into(), content.into())
    }

    #[test]
    fn render_basic_error() {
        let source = make_source("3 4 + foo");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E001, Span::new(Pos::new(6), Pos::new(9)))
            .message("unrecognized token")
            .label("not recognized")
            .build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("error[E001]: unrecognized token"));
        assert!(output.contains("test.rpl"));
        assert!(output.contains("3 4 + foo"));
        assert!(output.contains("^^^"));
        assert!(output.contains("not recognized"));
    }

    #[test]
    fn render_multiline_source() {
        let source = make_source("line1\nline2\nline3");
        let renderer = DiagnosticRenderer::new(&source);

        // Error on line 2, column 1
        let diag = Diagnostic::error(ErrorCode::E001, Span::new(Pos::new(6), Pos::new(11))).build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("test.rpl"));
        assert!(output.contains("line2"));
    }

    #[test]
    fn render_with_secondary() {
        let source = make_source("( foo");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E102, Span::new(Pos::new(0), Pos::new(1)))
            .message("unclosed parenthesis")
            .secondary(Span::new(Pos::new(2), Pos::new(5)), "expected ')' after this")
            .build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("error[E102]: unclosed parenthesis"));
        assert!(output.contains("expected ')' after this"));
    }

    #[test]
    fn render_with_notes() {
        let source = make_source("test");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E001, Span::new(Pos::new(0), Pos::new(4)))
            .note("help: did you mean 'TEST'?")
            .build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("help: did you mean 'TEST'?"));
    }

    #[test]
    fn render_warning() {
        let source = make_source("test");
        let renderer = DiagnosticRenderer::new(&source);

        let diag =
            Diagnostic::warning(ErrorCode::E001, Span::new(Pos::new(0), Pos::new(4))).build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("warning[E001]"));
    }

    #[test]
    fn render_single_char_span() {
        let source = make_source("abc");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E001, Span::new(Pos::new(1), Pos::new(2))).build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("test.rpl"));
        // Should have a caret
        assert!(output.contains("^"));
    }
}
