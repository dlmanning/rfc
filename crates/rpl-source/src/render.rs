use std::io::{self, Write};

use rpl_core::{Diagnostic, Severity};
use super::SourceFile;

/// Renders diagnostics to a writer.
pub struct DiagnosticRenderer<'a> {
    source: &'a SourceFile,
}

impl<'a> DiagnosticRenderer<'a> {
    pub fn new(source: &'a SourceFile) -> Self {
        Self { source }
    }

    /// Render a diagnostic to the given writer.
    pub fn render<W: Write>(&self, diag: &Diagnostic, out: &mut W) -> io::Result<()> {
        let severity_str = match diag.severity() {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        };

        // Header: error[E001]: message
        writeln!(
            out,
            "{}[{}]: {}",
            severity_str,
            diag.code().as_str(),
            diag.message()
        )?;

        // Location: --> file.rpl:3:10
        let lc = self.source.line_col(diag.span().start());
        writeln!(out, "  --> {}:{}:{}", self.source.name(), lc.line, lc.col)?;

        // Source line with underline
        if let Some(line_text) = self.source.line_text(lc.line) {
            let line_num_width = lc.line.to_string().len();

            // Empty line prefix
            writeln!(out, "{:width$} |", "", width = line_num_width)?;

            // Source line
            writeln!(out, "{} | {}", lc.line, line_text)?;

            // Underline with label
            let underline_start = (lc.col - 1) as usize;
            let span_len = diag.span().len() as usize;
            // Clamp span to line length
            let underline_len = span_len
                .min(line_text.len().saturating_sub(underline_start))
                .max(1);

            write!(out, "{:width$} | ", "", width = line_num_width)?;
            write!(out, "{:spaces$}", "", spaces = underline_start)?;
            write!(out, "{}", "^".repeat(underline_len))?;

            if let Some(label) = diag.label() {
                write!(out, " {}", label)?;
            }
            writeln!(out)?;
        }

        // Secondary labels
        for (span, label) in diag.secondary() {
            let sec_lc = self.source.line_col(span.start());
            if let Some(line_text) = self.source.line_text(sec_lc.line) {
                let line_num_width = sec_lc.line.to_string().len();

                writeln!(out, "{:width$} |", "", width = line_num_width)?;
                writeln!(out, "{} | {}", sec_lc.line, line_text)?;

                let underline_start = (sec_lc.col - 1) as usize;
                let span_len = span.len() as usize;
                let underline_len = span_len
                    .min(line_text.len().saturating_sub(underline_start))
                    .max(1);

                write!(out, "{:width$} | ", "", width = line_num_width)?;
                write!(out, "{:spaces$}", "", spaces = underline_start)?;
                writeln!(out, "{} {}", "-".repeat(underline_len), label)?;
            }
        }

        // Notes
        for note in diag.notes() {
            writeln!(out, "  = note: {}", note)?;
        }

        Ok(())
    }

    /// Render a diagnostic to a string.
    pub fn render_to_string(&self, diag: &Diagnostic) -> String {
        let mut buf = Vec::new();
        self.render(diag, &mut buf)
            .expect("writing to Vec cannot fail");
        String::from_utf8(buf).expect("output is valid UTF-8")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Pos, Span, ErrorCode};
    use crate::SourceId;

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
        assert!(output.contains("--> test.rpl:1:7"));
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

        assert!(output.contains("--> test.rpl:2:1"));
        assert!(output.contains("2 | line2"));
        assert!(output.contains("^^^^^"));
    }

    #[test]
    fn render_with_secondary() {
        let source = make_source("( foo");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E102, Span::new(Pos::new(0), Pos::new(1)))
            .message("unclosed parenthesis")
            .secondary(
                Span::new(Pos::new(2), Pos::new(5)),
                "expected ')' after this",
            )
            .build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("error[E102]: unclosed parenthesis"));
        assert!(output.contains("--- expected ')' after this"));
    }

    #[test]
    fn render_with_notes() {
        let source = make_source("test");
        let renderer = DiagnosticRenderer::new(&source);

        let diag = Diagnostic::error(ErrorCode::E001, Span::new(Pos::new(0), Pos::new(4)))
            .note("help: did you mean 'TEST'?")
            .build();

        let output = renderer.render_to_string(&diag);

        assert!(output.contains("= note: help: did you mean 'TEST'?"));
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

        assert!(output.contains("--> test.rpl:1:2"));
        // Should have exactly one caret
        assert!(output.contains(" ^"));
    }
}
