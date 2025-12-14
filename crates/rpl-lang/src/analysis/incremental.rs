use rpl_core::Interner;
use rpl_source::SourceFile;

use super::{result::AnalysisResult, state::LineState, tokenizer::Tokenizer};
use crate::library::LibraryRegistry;

/// A text edit to apply to a source file.
#[derive(Clone, Debug)]
pub struct TextEdit {
    /// Line range to replace (1-indexed, end-exclusive).
    pub range: (u32, u32),
    /// New text to insert.
    pub new_text: String,
}

impl TextEdit {
    /// Create a new text edit.
    pub fn new(start_line: u32, end_line: u32, new_text: String) -> Self {
        Self {
            range: (start_line, end_line),
            new_text,
        }
    }

    /// Create an edit that inserts text at a line.
    pub fn insert(line: u32, text: String) -> Self {
        Self::new(line, line, text)
    }

    /// Create an edit that deletes lines.
    pub fn delete(start_line: u32, end_line: u32) -> Self {
        Self::new(start_line, end_line, String::new())
    }

    /// Create an edit that replaces a single line.
    pub fn replace_line(line: u32, text: String) -> Self {
        Self::new(line, line + 1, text)
    }
}

/// Apply an edit to a source file and incrementally update the analysis result.
pub fn apply_edit(
    result: &mut AnalysisResult,
    source: &mut SourceFile,
    edit: TextEdit,
    registry: &LibraryRegistry,
    interner: &mut Interner,
) {
    let (start_line, end_line) = edit.range;

    // 1. Update source text
    source.replace_lines(start_line, end_line, &edit.new_text);

    // 2. Find stable point before the edit (empty construct stack)
    let stable_line = find_stable_point(&result.lines, start_line);

    // 3. Count how many lines were in the old edit region
    let old_line_count = end_line.saturating_sub(start_line);
    let new_line_count = edit.new_text.matches('\n').count() as u32
        + if edit.new_text.is_empty() || edit.new_text.ends_with('\n') {
            0
        } else {
            1
        };
    let line_delta = new_line_count as i32 - old_line_count as i32;

    // 4. Re-tokenize from stable point
    let new_result = Tokenizer::new(source, registry, interner).tokenize();

    // 5. Find where old and new states match again (convergence point)
    let convergence = find_convergence_point(
        &result.lines,
        &new_result.lines,
        stable_line,
        end_line,
        line_delta,
    );

    // 6. Splice new tokens and line states
    splice_results(result, &new_result, stable_line, convergence, line_delta);

    // 7. Increment version
    result.version += 1;
}

/// Find a stable point before the given line where construct stack is empty.
/// Returns a line number (1-indexed) that is safe to start re-tokenizing from.
pub fn find_stable_point(lines: &[LineState], before: u32) -> u32 {
    if before <= 1 || lines.is_empty() {
        return 1;
    }

    // Search backwards from before-2 (the line before the edit point)
    // before is 1-indexed, so before-2 gives us 0-indexed line before edit
    let start_idx = ((before - 2) as usize).min(lines.len().saturating_sub(1));

    for i in (0..=start_idx).rev() {
        // We want a line where state_before is not in a construct
        // This means we can safely start tokenizing from this line
        if !lines[i].state_before.in_construct() {
            return (i + 1) as u32; // Convert back to 1-indexed
        }
    }

    // No stable point found, start from beginning
    1
}

/// Find where old and new analysis results converge after the edit.
fn find_convergence_point(
    old_lines: &[LineState],
    new_lines: &[LineState],
    stable_line: u32,
    old_end_line: u32,
    line_delta: i32,
) -> ConvergencePoint {
    // Start checking from just after the edit region in the new result
    let new_check_start = if line_delta >= 0 {
        old_end_line as i32 + line_delta
    } else {
        (old_end_line as i32 + line_delta).max(stable_line as i32)
    } as u32;

    // Find where states match
    for new_line in new_check_start..=(new_lines.len() as u32) {
        let new_idx = (new_line - 1) as usize;
        if new_idx >= new_lines.len() {
            break;
        }

        // Corresponding old line
        let old_line = (new_line as i32 - line_delta) as u32;
        if old_line < 1 {
            continue;
        }
        let old_idx = (old_line - 1) as usize;
        if old_idx >= old_lines.len() {
            continue;
        }

        // Check if states match
        if old_lines[old_idx].state_after == new_lines[new_idx].state_after
            && old_lines[old_idx].hash == new_lines[new_idx].hash
        {
            return ConvergencePoint {
                old_line,
                new_line,
                found: true,
            };
        }
    }

    // No convergence found, replace everything from stable point
    ConvergencePoint {
        old_line: old_lines.len() as u32 + 1,
        new_line: new_lines.len() as u32 + 1,
        found: false,
    }
}

struct ConvergencePoint {
    old_line: u32,
    new_line: u32,
    found: bool,
}

/// Splice new analysis results into the old result.
fn splice_results(
    result: &mut AnalysisResult,
    new_result: &AnalysisResult,
    stable_line: u32,
    convergence: ConvergencePoint,
    line_delta: i32,
) {
    let stable_idx = (stable_line - 1) as usize;

    // Calculate token ranges
    let old_token_start = if stable_idx < result.lines.len() {
        result.lines[stable_idx].first_token as usize
    } else {
        result.tokens.len()
    };

    let old_token_end = if convergence.found {
        let conv_idx = (convergence.old_line - 1) as usize;
        if conv_idx < result.lines.len() {
            result.lines[conv_idx].first_token as usize
        } else {
            result.tokens.len()
        }
    } else {
        result.tokens.len()
    };

    let new_token_start = if stable_idx < new_result.lines.len() {
        new_result.lines[stable_idx].first_token as usize
    } else {
        0
    };

    let new_token_end = if convergence.found {
        let conv_idx = (convergence.new_line - 1) as usize;
        if conv_idx < new_result.lines.len() {
            new_result.lines[conv_idx].first_token as usize
        } else {
            new_result.tokens.len()
        }
    } else {
        new_result.tokens.len()
    };

    // Splice tokens
    let new_tokens = new_result.tokens[new_token_start..new_token_end].to_vec();
    result
        .tokens
        .splice(old_token_start..old_token_end, new_tokens);

    // Splice line states
    let old_line_end = if convergence.found {
        (convergence.old_line - 1) as usize
    } else {
        result.lines.len()
    };

    let new_line_end = if convergence.found {
        (convergence.new_line - 1) as usize
    } else {
        new_result.lines.len()
    };

    let new_lines = new_result.lines[stable_idx..new_line_end].to_vec();
    result.lines.splice(stable_idx..old_line_end, new_lines);

    // Update token indices in lines after the splice
    let token_delta =
        (new_token_end - new_token_start) as i32 - (old_token_end - old_token_start) as i32;
    if token_delta != 0 {
        let update_start = if convergence.found {
            stable_idx + (new_line_end - stable_idx)
        } else {
            result.lines.len()
        };

        for line in result.lines.iter_mut().skip(update_start) {
            line.first_token = (line.first_token as i32 + token_delta) as u32;
        }
    }

    // Replace diagnostics (simple approach: regenerate from new result)
    // A more sophisticated approach would splice diagnostics too
    result.diagnostics = new_result.diagnostics.clone();

    // Handle line delta for remaining lines' first_token if needed
    let _ = line_delta; // Used in convergence calculation above
}

#[cfg(test)]
mod tests {
    use rpl_core::token::{SemanticKind, TokenInfo};
    use rpl_source::SourceId;

    use super::*;
    use crate::library::{
        CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteOk, ExecuteResult,
        Library, LibraryId, ProbeContext, ProbeResult,
    };

    // Simple mock library for testing
    struct MockLib;

    impl Library for MockLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(1)
        }

        fn name(&self) -> &'static str {
            "mock"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            let text = ctx.text();
            if text.chars().all(|c| c.is_ascii_digit()) {
                ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Number,
                }
            } else if text.chars().all(|c| c.is_ascii_alphabetic()) {
                ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Command,
                }
            } else {
                ProbeResult::NoMatch
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            Ok(ExecuteOk::Ok)
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }
    }

    fn make_registry() -> LibraryRegistry {
        let mut registry = LibraryRegistry::new();
        registry.register(MockLib, 100);
        registry
    }

    #[test]
    fn text_edit_new() {
        let edit = TextEdit::new(2, 4, "hello\n".into());
        assert_eq!(edit.range, (2, 4));
        assert_eq!(edit.new_text, "hello\n");
    }

    #[test]
    fn text_edit_insert() {
        let edit = TextEdit::insert(3, "new line\n".into());
        assert_eq!(edit.range, (3, 3));
    }

    #[test]
    fn text_edit_delete() {
        let edit = TextEdit::delete(2, 5);
        assert_eq!(edit.range, (2, 5));
        assert!(edit.new_text.is_empty());
    }

    #[test]
    fn text_edit_replace_line() {
        let edit = TextEdit::replace_line(3, "replaced\n".into());
        assert_eq!(edit.range, (3, 4));
    }

    #[test]
    fn find_stable_point_empty() {
        let lines: Vec<LineState> = vec![];
        assert_eq!(find_stable_point(&lines, 5), 1);
    }

    #[test]
    fn find_stable_point_no_constructs() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let source = SourceFile::new(SourceId::new(0), "test".into(), "a\nb\nc".into());
        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // All lines have empty construct stack, should return the line before
        assert_eq!(find_stable_point(&result.lines, 3), 2);
        assert_eq!(find_stable_point(&result.lines, 2), 1);
        assert_eq!(find_stable_point(&result.lines, 1), 1);
    }

    #[test]
    fn apply_edit_middle_of_file() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let mut source = SourceFile::new(SourceId::new(0), "test".into(), "a\nb\nc".into());
        let mut result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
        assert_eq!(result.lines.len(), 3);

        // Replace line 2 "b" with "X"
        let edit = TextEdit::replace_line(2, "X\n".into());
        apply_edit(&mut result, &mut source, edit, &registry, &mut interner);

        assert_eq!(source.source(), "a\nX\nc");
        assert_eq!(result.token_count(), 3);
        assert_eq!(result.lines.len(), 3);
    }

    #[test]
    fn apply_edit_insert_line() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let mut source = SourceFile::new(SourceId::new(0), "test".into(), "a\nc".into());
        let mut result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 2);
        assert_eq!(result.lines.len(), 2);

        // Insert "b\n" at line 2
        let edit = TextEdit::insert(2, "b\n".into());
        apply_edit(&mut result, &mut source, edit, &registry, &mut interner);

        assert_eq!(source.source(), "a\nb\nc");
        assert_eq!(result.token_count(), 3);
        assert_eq!(result.lines.len(), 3);
    }

    #[test]
    fn apply_edit_delete_line() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let mut source = SourceFile::new(SourceId::new(0), "test".into(), "a\nb\nc".into());
        let mut result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
        assert_eq!(result.lines.len(), 3);

        // Delete line 2
        let edit = TextEdit::delete(2, 3);
        apply_edit(&mut result, &mut source, edit, &registry, &mut interner);

        assert_eq!(source.source(), "a\nc");
        assert_eq!(result.token_count(), 2);
        assert_eq!(result.lines.len(), 2);
    }

    #[test]
    fn apply_edit_increments_version() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let mut source = SourceFile::new(SourceId::new(0), "test".into(), "a\nb".into());
        let mut result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.version, 0);

        let edit = TextEdit::replace_line(1, "X\n".into());
        apply_edit(&mut result, &mut source, edit, &registry, &mut interner);

        assert_eq!(result.version, 1);
    }

    // Mock library for construct testing
    struct MockConstructLib;

    impl Library for MockConstructLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(2)
        }

        fn name(&self) -> &'static str {
            "constructs"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            match ctx.text() {
                "BEGIN" => ProbeResult::Match {
                    info: TokenInfo::atom(5),
                    semantic: SemanticKind::Keyword,
                },
                "END" => ProbeResult::Match {
                    info: TokenInfo::atom(3),
                    semantic: SemanticKind::Keyword,
                },
                _ => ProbeResult::NoMatch,
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            Ok(ExecuteOk::Ok)
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }

        fn stack_effect(&self, token: &str) -> crate::library::StackEffect {
            match token {
                "BEGIN" => crate::library::StackEffect::StartConstruct,
                "END" => crate::library::StackEffect::EndConstruct,
                _ => crate::library::StackEffect::Dynamic,
            }
        }
    }

    fn make_registry_with_constructs() -> LibraryRegistry {
        let mut registry = LibraryRegistry::new();
        registry.register(MockLib, 50);
        registry.register(MockConstructLib, 100); // Higher priority for constructs
        registry
    }

    #[test]
    fn apply_edit_construct_nesting_change() {
        let registry = make_registry_with_constructs();
        let mut interner = Interner::new();
        // Source with construct: BEGIN a END
        let mut source =
            SourceFile::new(SourceId::new(0), "test".into(), "BEGIN\na\nEND\nb".into());
        let mut result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 4); // BEGIN, a, END, b
        assert_eq!(result.lines.len(), 4);

        // Line 1 (BEGIN) starts construct, state_after is in_construct
        assert!(result.lines[0].state_after.in_construct());
        // Line 2 (a) is inside construct
        assert!(result.lines[1].state_before.in_construct());
        assert!(result.lines[1].state_after.in_construct());
        // Line 3 (END) ends construct
        assert!(result.lines[2].state_before.in_construct());
        assert!(!result.lines[2].state_after.in_construct());

        // Now remove the END - this changes construct nesting
        // Replace line 3 (END) with another token that doesn't end construct
        let edit = TextEdit::replace_line(3, "c\n".into());
        apply_edit(&mut result, &mut source, edit, &registry, &mut interner);

        assert_eq!(source.source(), "BEGIN\na\nc\nb");
        // Now all remaining lines after BEGIN should be in construct
        // (since END was removed)
        assert!(result.lines[0].state_after.in_construct()); // BEGIN
        assert!(result.lines[1].state_after.in_construct()); // a
        assert!(result.lines[2].state_after.in_construct()); // c (was END)
        assert!(result.lines[3].state_after.in_construct()); // b (still in construct)
    }

    #[test]
    fn find_stable_point_with_construct() {
        let registry = make_registry_with_constructs();
        let mut interner = Interner::new();
        let source = SourceFile::new(
            SourceId::new(0),
            "test".into(),
            "a\nBEGIN\nb\nEND\nc".into(),
        );
        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // Line 1: "a" - not in construct, state_before empty
        // Line 2: "BEGIN" - starts construct, state_before empty
        // Line 3: "b" - in construct, state_before in_construct
        // Line 4: "END" - ends construct, state_before in_construct
        // Line 5: "c" - not in construct, state_before empty

        // Verify state_before values
        assert!(!result.lines[0].state_before.in_construct()); // line 1
        assert!(!result.lines[1].state_before.in_construct()); // line 2 (BEGIN)
        assert!(result.lines[2].state_before.in_construct()); // line 3 (b)
        assert!(result.lines[3].state_before.in_construct()); // line 4 (END)
        assert!(!result.lines[4].state_before.in_construct()); // line 5 (c)

        // If editing line 3 (inside construct), we search backwards from line 2
        // Line 2 has state_before empty, so we can safely start there
        assert_eq!(find_stable_point(&result.lines, 3), 2);

        // If editing line 4 (END, inside construct), we search backwards from line 3
        // Line 3 has state_before in_construct, line 2 has state_before empty
        assert_eq!(find_stable_point(&result.lines, 4), 2);

        // If editing line 5 (after construct), we search backwards from line 4
        // Line 4 has state_before in_construct, line 3 has state_before in_construct,
        // line 2 has state_before empty
        assert_eq!(find_stable_point(&result.lines, 5), 2);
    }
}
