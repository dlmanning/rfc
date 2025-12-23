//! Incremental analysis for RPL2.
//!
//! This module provides incremental analysis capabilities, allowing
//! efficient re-analysis when source code changes.
//!
//! # Design
//!
//! The incremental system works by:
//! 1. Tracking which spans of source correspond to which scopes
//! 2. On edit, finding the smallest affected scope
//! 3. Invalidating only the affected definitions and references
//! 4. Re-analyzing just the affected region
//!
//! # Usage
//!
//! ```ignore
//! use rpl::analysis::incremental::{IncrementalAnalysis, SpanEdit};
//!
//! let mut state = IncrementalAnalysis::new(source, &registry, &interner);
//!
//! // Apply an edit
//! state.apply_edit(SpanEdit::new(10, 15, "new_value".into()), &registry, &mut interner);
//!
//! // Get the updated result
//! let result = state.result();
//! ```

use crate::core::{Interner, Pos, Span};

use super::context::Context;
use super::result::AnalysisResult;
use super::scopes::ScopeId;
use super::analyze;
use crate::ir::Node;
use crate::parse::parse;
use crate::registry::InterfaceRegistry;

/// A text edit to apply to source code.
///
/// Edits are specified using byte offsets into the source.
#[derive(Clone, Debug)]
pub struct SpanEdit {
    /// The span to replace (byte offsets).
    pub span: Span,
    /// The new text to insert.
    pub new_text: String,
}

impl SpanEdit {
    /// Create a new span edit.
    pub fn new(start: u32, end: u32, new_text: String) -> Self {
        Self {
            span: Span::new(Pos::new(start), Pos::new(end)),
            new_text,
        }
    }

    /// Create an edit from a span.
    pub fn from_span(span: Span, new_text: String) -> Self {
        Self { span, new_text }
    }

    /// Create an insertion edit at a position.
    pub fn insert(pos: u32, text: String) -> Self {
        Self::new(pos, pos, text)
    }

    /// Create a deletion edit.
    pub fn delete(start: u32, end: u32) -> Self {
        Self::new(start, end, String::new())
    }

    /// Create a replacement edit at a position.
    pub fn replace(start: u32, end: u32, text: String) -> Self {
        Self::new(start, end, text)
    }

    /// Get the byte offset change caused by this edit.
    pub fn offset_delta(&self) -> i32 {
        self.new_text.len() as i32 - self.span.len() as i32
    }
}

/// Cached information about a scope for incremental updates.
#[derive(Clone, Debug)]
struct ScopeCache {
    /// The scope ID.
    id: ScopeId,
    /// The span of this scope in the source.
    span: Span,
}

/// State for incremental analysis.
///
/// This tracks the current source, parsed IR, and analysis results,
/// allowing efficient updates when the source changes.
pub struct IncrementalAnalysis {
    /// Current source code.
    source: String,
    /// Parsed IR nodes.
    nodes: Vec<Node>,
    /// Analysis result.
    result: AnalysisResult,
    /// Scope cache for quick invalidation decisions.
    scope_cache: Vec<ScopeCache>,
    /// Version counter (incremented on each edit).
    version: u32,
}

impl IncrementalAnalysis {
    /// Create a new incremental analysis state from source.
    pub fn new(
        source: &str,
        registry: &InterfaceRegistry,
        interner: &mut Interner,
        context: &Context,
    ) -> Self {
        let nodes = parse(source, registry, interner).unwrap_or_default();

        let result = analyze(&nodes, registry, interner, context);
        let scope_cache = build_scope_cache(&result);

        Self {
            source: source.to_string(),
            nodes,
            result,
            scope_cache,
            version: 0,
        }
    }

    /// Get the current source.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the current analysis result.
    pub fn result(&self) -> &AnalysisResult {
        &self.result
    }

    /// Get the current version number.
    pub fn version(&self) -> u32 {
        self.version
    }

    /// Get the parsed IR nodes.
    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    /// Apply an edit to the source and update the analysis.
    ///
    /// This is the main entry point for incremental updates.
    pub fn apply_edit(
        &mut self,
        edit: SpanEdit,
        registry: &InterfaceRegistry,
        interner: &mut Interner,
        context: &Context,
    ) {
        // 1. Update source text
        self.apply_source_edit(&edit);

        // 2. Find the stable scope (smallest scope containing the edit)
        let stable_scope = self.find_stable_scope(&edit);

        // 3. Determine if we can do incremental or need full re-parse
        let can_do_incremental = self.can_do_incremental(&edit, stable_scope);

        if can_do_incremental {
            // Incremental update
            self.incremental_update(&edit, stable_scope, registry, interner, context);
        } else {
            // Full re-parse and re-analyze
            self.full_update(registry, interner, context);
        }

        // 4. Increment version
        self.version += 1;
    }

    /// Apply multiple edits at once.
    ///
    /// Edits should be sorted by position (earliest first) and non-overlapping.
    /// They will be applied from last to first to maintain position validity.
    pub fn apply_edits(
        &mut self,
        mut edits: Vec<SpanEdit>,
        registry: &InterfaceRegistry,
        interner: &mut Interner,
        context: &Context,
    ) {
        if edits.is_empty() {
            return;
        }

        // Sort by position (descending) so we can apply from end to start
        edits.sort_by(|a, b| b.span.start().offset().cmp(&a.span.start().offset()));

        // For multiple edits, do a full re-parse for simplicity
        // (incremental multi-edit is complex and not worth it for most cases)
        for edit in edits {
            self.apply_source_edit(&edit);
        }

        self.full_update(registry, interner, context);
        self.version += 1;
    }

    /// Apply the edit to the source string.
    fn apply_source_edit(&mut self, edit: &SpanEdit) {
        let start = edit.span.start().offset() as usize;
        let end = edit.span.end().offset() as usize;

        // Clamp to source bounds
        let start = start.min(self.source.len());
        let end = end.min(self.source.len());

        // Replace the range with new text
        let mut new_source = String::with_capacity(
            self.source.len() - (end - start) + edit.new_text.len(),
        );
        new_source.push_str(&self.source[..start]);
        new_source.push_str(&edit.new_text);
        new_source.push_str(&self.source[end..]);

        self.source = new_source;
    }

    /// Find the smallest scope that contains the edit.
    fn find_stable_scope(&self, edit: &SpanEdit) -> Option<ScopeId> {
        // Find the innermost scope that contains the edit span
        let edit_start = edit.span.start().offset();
        let edit_end = edit.span.end().offset();

        let mut best_scope: Option<&ScopeCache> = None;

        for cache in &self.scope_cache {
            let scope_start = cache.span.start().offset();
            let scope_end = cache.span.end().offset();

            // Check if scope contains the edit
            if scope_start <= edit_start && edit_end <= scope_end {
                // Prefer smaller (more nested) scopes
                if let Some(best) = best_scope {
                    if cache.span.len() < best.span.len() {
                        best_scope = Some(cache);
                    }
                } else {
                    best_scope = Some(cache);
                }
            }
        }

        best_scope.map(|c| c.id)
    }

    /// Check if we can do an incremental update.
    fn can_do_incremental(&self, edit: &SpanEdit, stable_scope: Option<ScopeId>) -> bool {
        // For now, we only do incremental if:
        // 1. The edit is small (< 100 chars)
        // 2. The edit is contained within a single scope
        // 3. The edit doesn't cross construct boundaries (« », { }, etc.)

        let edit_small = edit.new_text.len() < 100 && edit.span.len() < 100;
        let has_scope = stable_scope.is_some();
        let no_construct_boundary = !contains_construct_boundary(&edit.new_text);

        edit_small && has_scope && no_construct_boundary
    }

    /// Perform an incremental update.
    fn incremental_update(
        &mut self,
        _edit: &SpanEdit,
        _stable_scope: Option<ScopeId>,
        registry: &InterfaceRegistry,
        interner: &mut Interner,
        context: &Context,
    ) {
        // For the initial implementation, fall back to full update.
        // A more sophisticated implementation would:
        // 1. Invalidate only definitions/references in the affected scope
        // 2. Re-parse only the affected region
        // 3. Splice the new nodes into the IR
        // 4. Re-analyze only the affected scope
        //
        // This is complex because:
        // - Span offsets need to be adjusted for all nodes after the edit
        // - Scope boundaries might change
        // - Symbol resolution might be affected by changes in outer scopes
        //
        // For now, do a full update which is correct if not optimal.
        self.full_update(registry, interner, context);
    }

    /// Perform a full re-parse and re-analyze.
    fn full_update(&mut self, registry: &InterfaceRegistry, interner: &mut Interner, context: &Context) {
        self.nodes = parse(&self.source, registry, interner).unwrap_or_default();

        self.result = analyze(&self.nodes, registry, interner, context);
        self.scope_cache = build_scope_cache(&self.result);
    }
}

/// Build the scope cache from an analysis result.
fn build_scope_cache(result: &AnalysisResult) -> Vec<ScopeCache> {
    result
        .scopes
        .iter()
        .map(|scope| ScopeCache {
            id: scope.id,
            span: scope.span,
        })
        .collect()
}

/// Check if text contains construct boundaries.
fn contains_construct_boundary(text: &str) -> bool {
    text.contains("«")
        || text.contains("»")
        || text.contains("<<")
        || text.contains(">>")
        || text.contains('{')
        || text.contains('}')
        || text.contains('\'')
}

/// Helper for converting line-based edits to span edits.
pub fn line_edit_to_span_edit(
    source: &str,
    start_line: u32,
    end_line: u32,
    new_text: &str,
) -> SpanEdit {
    let mut line_starts: Vec<usize> = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            line_starts.push(i + 1);
        }
    }
    line_starts.push(source.len());

    let start_line_idx = (start_line.saturating_sub(1) as usize).min(line_starts.len() - 1);
    let end_line_idx = (end_line.saturating_sub(1) as usize).min(line_starts.len() - 1);

    let start_offset = line_starts[start_line_idx] as u32;
    let end_offset = line_starts[end_line_idx] as u32;

    SpanEdit::new(start_offset, end_offset, new_text.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interface::BindingKind;
    use crate::ir::LibId;
    use crate::libs::{CommandInfo, StackEffect};
    use crate::registry::InterfaceRegistry;

    // Test constants matching directory lib
    const DIRECTORY_LIB: LibId = 28;
    mod dir_cmd {
        pub const STO: u16 = 0;
        pub const RCL: u16 = 1;
    }

    /// Mock directory interface that implements binding_effect for tests.
    struct MockDirectoryInterface;

    impl crate::libs::LibraryInterface for MockDirectoryInterface {
        fn id(&self) -> LibId {
            DIRECTORY_LIB
        }
        fn name(&self) -> &'static str {
            "Directory"
        }
        fn commands(&self) -> Vec<CommandInfo> {
            vec![
                CommandInfo {
                    name: "STO",
                    lib_id: DIRECTORY_LIB,
                    cmd_id: dir_cmd::STO,
                    effect: StackEffect::fixed(2, &[]),
                },
                CommandInfo {
                    name: "RCL",
                    lib_id: DIRECTORY_LIB,
                    cmd_id: dir_cmd::RCL,
                    effect: StackEffect::fixed(1, &[None]),
                },
            ]
        }
        fn binding_effect(&self, cmd: u16) -> Option<BindingKind> {
            match cmd {
                0 => Some(BindingKind::Define), // STO
                1 => Some(BindingKind::Read),   // RCL
                _ => None,
            }
        }
    }

    fn make_registry() -> InterfaceRegistry {
        let mut reg = InterfaceRegistry::new();
        reg.add(MockDirectoryInterface);
        reg
    }

    #[test]
    fn span_edit_new() {
        let edit = SpanEdit::new(10, 20, "hello".into());
        assert_eq!(edit.span.start().offset(), 10);
        assert_eq!(edit.span.end().offset(), 20);
        assert_eq!(edit.new_text, "hello");
    }

    #[test]
    fn span_edit_insert() {
        let edit = SpanEdit::insert(10, "hello".into());
        assert_eq!(edit.span.start().offset(), 10);
        assert_eq!(edit.span.end().offset(), 10);
        assert_eq!(edit.new_text, "hello");
    }

    #[test]
    fn span_edit_delete() {
        let edit = SpanEdit::delete(10, 20);
        assert_eq!(edit.span.start().offset(), 10);
        assert_eq!(edit.span.end().offset(), 20);
        assert!(edit.new_text.is_empty());
    }

    #[test]
    fn span_edit_offset_delta() {
        // Replace 10 chars with 5 chars: delta = -5
        let edit = SpanEdit::new(0, 10, "hello".into());
        assert_eq!(edit.offset_delta(), -5);

        // Replace 5 chars with 10 chars: delta = +5
        let edit = SpanEdit::new(0, 5, "hellohello".into());
        assert_eq!(edit.offset_delta(), 5);

        // Same length: delta = 0
        let edit = SpanEdit::new(0, 5, "hello".into());
        assert_eq!(edit.offset_delta(), 0);
    }

    #[test]
    fn incremental_analysis_new() {
        let registry = make_registry();
        let mut interner = Interner::new();

        let state = IncrementalAnalysis::new("1 2 +", &registry, &mut interner, &Context::empty());

        assert_eq!(state.source(), "1 2 +");
        assert_eq!(state.version(), 0);
        assert!(!state.nodes().is_empty());
    }

    #[test]
    fn incremental_analysis_apply_edit() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("1 2 +", &registry, &mut interner, &context);
        assert_eq!(state.version(), 0);

        // Replace "2" with "3"
        state.apply_edit(SpanEdit::new(2, 3, "3".into()), &registry, &mut interner, &context);

        assert_eq!(state.source(), "1 3 +");
        assert_eq!(state.version(), 1);
    }

    #[test]
    fn incremental_analysis_insert() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("1 2", &registry, &mut interner, &context);

        // Insert " +" at end
        state.apply_edit(SpanEdit::insert(3, " +".into()), &registry, &mut interner, &context);

        assert_eq!(state.source(), "1 2 +");
    }

    #[test]
    fn incremental_analysis_delete() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("1 2 3 +", &registry, &mut interner, &context);

        // Delete " 2"
        state.apply_edit(SpanEdit::delete(1, 3), &registry, &mut interner, &context);

        assert_eq!(state.source(), "1 3 +");
    }

    #[test]
    fn incremental_analysis_sto_pattern() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("42 \"x\" STO", &registry, &mut interner, &context);

        // Should have one definition
        assert_eq!(state.result().definition_count(), 1);

        // Change value from 42 to 100
        state.apply_edit(SpanEdit::new(0, 2, "100".into()), &registry, &mut interner, &context);

        assert_eq!(state.source(), "100 \"x\" STO");
        // Should still have one definition
        assert_eq!(state.result().definition_count(), 1);
    }

    #[test]
    fn incremental_analysis_add_definition() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("42 \"x\" STO", &registry, &mut interner, &context);
        assert_eq!(state.result().definition_count(), 1);

        // Add another definition
        let source_len = state.source().len() as u32;
        state.apply_edit(
            SpanEdit::insert(source_len, " 10 \"y\" STO".into()),
            &registry,
            &mut interner,
            &context,
        );

        assert_eq!(state.source(), "42 \"x\" STO 10 \"y\" STO");
        // Should now have two definitions
        assert_eq!(state.result().definition_count(), 2);
    }

    #[test]
    fn incremental_analysis_multiple_edits() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("1 2 3", &registry, &mut interner, &context);

        // Apply multiple edits at once
        let edits = vec![
            SpanEdit::new(0, 1, "10".into()),  // 1 -> 10
            SpanEdit::new(4, 5, "30".into()),  // 3 -> 30
        ];
        state.apply_edits(edits, &registry, &mut interner, &context);

        // Note: edits are applied from end to start
        assert_eq!(state.source(), "10 2 30");
    }

    #[test]
    fn line_edit_conversion() {
        let source = "line1\nline2\nline3\n";

        // Replace line 2
        let edit = line_edit_to_span_edit(source, 2, 3, "new_line2\n");

        assert_eq!(edit.span.start().offset(), 6); // Start of line 2
        assert_eq!(edit.span.end().offset(), 12); // Start of line 3
        assert_eq!(edit.new_text, "new_line2\n");
    }

    #[test]
    fn construct_boundary_detection() {
        assert!(contains_construct_boundary("«"));
        assert!(contains_construct_boundary("»"));
        assert!(contains_construct_boundary("<<"));
        assert!(contains_construct_boundary(">>"));
        assert!(contains_construct_boundary("{"));
        assert!(contains_construct_boundary("}"));
        assert!(contains_construct_boundary("'x'"));

        assert!(!contains_construct_boundary("hello"));
        assert!(!contains_construct_boundary("1 2 +"));
        assert!(!contains_construct_boundary("STO RCL"));
    }

    #[test]
    fn version_increments() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let context = Context::empty();

        let mut state = IncrementalAnalysis::new("1", &registry, &mut interner, &context);
        assert_eq!(state.version(), 0);

        state.apply_edit(SpanEdit::new(0, 1, "2".into()), &registry, &mut interner, &context);
        assert_eq!(state.version(), 1);

        state.apply_edit(SpanEdit::new(0, 1, "3".into()), &registry, &mut interner, &context);
        assert_eq!(state.version(), 2);
    }
}
