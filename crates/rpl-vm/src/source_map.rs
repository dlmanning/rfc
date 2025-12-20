//! Source mapping for debug info.
//!
//! Maps bytecode offsets to source locations, with the source code included.

use crate::span::Span;

/// Source map: maps bytecode offsets to source spans, with the source code included.
///
/// Including the source allows error messages to show the actual code even when
/// deserializing standalone library files.
#[derive(Clone, Debug, Default)]
pub struct SourceMap {
    /// The source code that spans reference into.
    pub source: String,
    /// Bytecode offsets where spans start.
    pub offsets: Vec<u32>,
    /// Source spans (parallel with offsets).
    pub spans: Vec<Span>,
}

impl SourceMap {
    /// Create a new empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a source map with source code.
    pub fn with_source(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            offsets: Vec::new(),
            spans: Vec::new(),
        }
    }

    /// Check if the source map is empty.
    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    /// Get a slice of source code for a span.
    pub fn get_source_slice(&self, span: &Span) -> Option<&str> {
        let start = span.start().offset() as usize;
        let end = span.end().offset() as usize;
        if end <= self.source.len() && start <= end {
            Some(&self.source[start..end])
        } else {
            None
        }
    }
}
