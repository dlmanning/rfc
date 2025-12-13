use rpl_core::error::Diagnostic;
use rpl_source::SourceId;

use super::state::LineState;
use super::token::ResolvedToken;

/// Result of analyzing a source file.
#[derive(Clone, Debug)]
pub struct AnalysisResult {
    /// ID of the source file.
    pub source_id: SourceId,
    /// Version number for change tracking.
    pub version: u64,
    /// All tokens in the source.
    pub tokens: Vec<ResolvedToken>,
    /// Line states for incremental analysis.
    pub lines: Vec<LineState>,
    /// Diagnostics produced during analysis.
    pub diagnostics: Vec<Diagnostic>,
}

impl AnalysisResult {
    pub fn new(source_id: SourceId) -> Self {
        Self {
            source_id,
            version: 0,
            tokens: Vec::new(),
            lines: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.severity(), rpl_core::error::Severity::Error))
    }

    /// Get the number of tokens.
    pub fn token_count(&self) -> usize {
        self.tokens.len()
    }

    /// Get a token by index.
    pub fn get_token(&self, index: usize) -> Option<&ResolvedToken> {
        self.tokens.get(index)
    }

    /// Find the token at a given position.
    pub fn token_at(&self, pos: rpl_core::Pos) -> Option<&ResolvedToken> {
        self.tokens.iter().find(|t| t.span.contains(pos))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analysis_result_new() {
        let result = AnalysisResult::new(SourceId::new(0));
        assert_eq!(result.source_id, SourceId::new(0));
        assert_eq!(result.version, 0);
        assert!(result.tokens.is_empty());
        assert!(result.lines.is_empty());
        assert!(result.diagnostics.is_empty());
        assert!(!result.has_errors());
    }

    #[test]
    fn analysis_result_token_count() {
        let mut result = AnalysisResult::new(SourceId::new(0));
        assert_eq!(result.token_count(), 0);

        result.tokens.push(crate::analysis::ResolvedToken::new(
            rpl_core::Span::DUMMY,
            crate::library::LibraryId::new(0),
            rpl_core::token::TokenInfo::atom(1),
            rpl_core::token::SemanticKind::Number,
        ));
        assert_eq!(result.token_count(), 1);
    }
}
