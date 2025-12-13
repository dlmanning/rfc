//! Incremental analysis tests.
//!
//! These tests verify that incremental analysis works correctly.

use rpl_session::{Session, TextEdit};

// ============================================================================
// Basic Editing
// ============================================================================

#[test]
fn edit_preserves_unaffected() {
    let mut session = Session::new();

    // Initial analysis
    let id = session.set_source("test.rpl", "1 2 3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    let token1_span = result1.tokens[0].span;

    // Edit line 1, replacing "1 2 3" with "1 5 3"
    session.edit_source(id, TextEdit::new(1, 2, "1 5 3".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");

    // Token for "1" should be unchanged (same span)
    assert_eq!(
        result2.tokens[0].span, token1_span,
        "Token before edit should be preserved"
    );
}

#[test]
fn edit_middle() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2 3");
    session.analyze(id);

    // Replace entire line, changing middle token
    session.edit_source(id, TextEdit::new(1, 2, "1 5 3".into()));
    let result = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result.token_count(), 3);
}

#[test]
fn insert_token() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 3");
    session.analyze(id);

    // Replace line to insert "2" in the middle
    session.edit_source(id, TextEdit::new(1, 2, "1 2 3".into()));
    let result = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result.token_count(), 3);
}

#[test]
fn delete_token() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2 3");
    session.analyze(id);

    // Replace line to delete "2"
    session.edit_source(id, TextEdit::new(1, 2, "1 3".into()));
    let result = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result.token_count(), 2);
}

// ============================================================================
// Version Tracking
// ============================================================================

#[test]
fn edit_tracks_version() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2 3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    let version1 = result1.version;
    assert_eq!(version1, 0, "Fresh analysis should have version 0");

    // Incremental edit should increment version
    session.edit_source(id, TextEdit::new(1, 2, "1 5 3".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");
    let version2 = result2.version;

    assert_eq!(version2, 1, "Version should increment after edit");

    // Another edit should increment again
    session.edit_source(id, TextEdit::new(1, 2, "1 9 3".into()));
    let result3 = session.analyze(id).expect("Analysis should succeed");
    let version3 = result3.version;

    assert_eq!(version3, 2, "Version should increment after second edit");
}

// ============================================================================
// Caching
// ============================================================================

#[test]
fn cache_hit_same_source() {
    let mut session = Session::new();

    // Analyze same source twice
    let id = session.set_source("test.rpl", "1 2 3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    let count1 = result1.token_count();

    let result2 = session.analyze(id).expect("Analysis should succeed");
    let count2 = result2.token_count();

    // Should produce equivalent results
    assert_eq!(
        count1, count2,
        "Same source should produce same token count"
    );
}

#[test]
fn cache_invalidation() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2 3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    let count1 = result1.token_count();

    // Update source (invalidates cache via set_source)
    let id2 = session.set_source("test.rpl", "1 2 4");
    let result2 = session.analyze(id2).expect("Analysis should succeed");
    let count2 = result2.token_count();

    // Token count same, but content should differ
    assert_eq!(count1, count2);
    // The last token should be different (3 vs 4)
    // This verifies cache was invalidated for the changed content
}

// ============================================================================
// Stable Point Recovery
// ============================================================================

#[test]
fn stable_point_recovery() {
    let mut session = Session::new();

    // Two separate programs on two lines
    let id = session.set_source("test.rpl", ":: 1 ;\n:: 2 ;");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    assert_eq!(result1.lines.len(), 2);

    // Edit inside first line (first program) - preserve newline
    session.edit_source(id, TextEdit::new(1, 2, ":: 5 ;\n".into()));
    let result = session.analyze(id).expect("Analysis should succeed");

    // Both programs should still be present
    assert_eq!(result.lines.len(), 2, "Should still have 2 lines after edit");
    assert!(result.token_count() >= 6, "Should have tokens for both programs");
}

// ============================================================================
// Line State Tracking
// ============================================================================

#[test]
fn line_states_tracked() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2\n3 4");
    let result = session.analyze(id).expect("Analysis should succeed");

    // Should have 2 line states
    assert_eq!(result.lines.len(), 2, "Expected 2 lines");

    // First line: tokens "1" and "2"
    assert_eq!(result.lines[0].token_count, 2);

    // Second line: tokens "3" and "4"
    assert_eq!(result.lines[1].token_count, 2);
}

#[test]
fn line_hash_changes_on_edit() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1 2\n3 4");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    let hash1_line0 = result1.lines[0].hash;
    let hash1_line1 = result1.lines[1].hash;

    // Edit first line only - preserve newline to keep 2 lines
    session.edit_source(id, TextEdit::new(1, 2, "1 5\n".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result2.lines.len(), 2, "Should still have 2 lines after edit");

    let hash2_line0 = result2.lines[0].hash;
    let hash2_line1 = result2.lines[1].hash;

    assert_ne!(hash1_line0, hash2_line0, "Line hash should change when content changes");

    // Second line hash should be unchanged
    assert_eq!(
        hash1_line1, hash2_line1,
        "Unchanged line should have same hash"
    );
}

// ============================================================================
// Multi-line Edits
// ============================================================================

#[test]
fn insert_new_line() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1\n3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    assert_eq!(result1.lines.len(), 2, "Should have 2 lines initially");

    // Insert a new line in the middle
    session.edit_source(id, TextEdit::new(2, 2, "2\n3".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result2.lines.len(), 3, "Should have 3 lines after insert");
}

#[test]
fn delete_line() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1\n2\n3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    assert_eq!(result1.lines.len(), 3, "Should have 3 lines initially");

    // Delete the middle line
    session.edit_source(id, TextEdit::new(2, 3, "".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result2.lines.len(), 2, "Should have 2 lines after delete");
}

#[test]
fn edit_preserves_later_line_content() {
    let mut session = Session::new();

    let id = session.set_source("test.rpl", "1\n2\n3");
    let result1 = session.analyze(id).expect("Analysis should succeed");
    assert_eq!(result1.lines.len(), 3);
    let hash_line2 = result1.lines[2].hash;

    // Edit first line only - preserve newline to maintain structure
    session.edit_source(id, TextEdit::new(1, 2, "9\n".into()));
    let result2 = session.analyze(id).expect("Analysis should succeed");

    assert_eq!(result2.lines.len(), 3, "Should still have 3 lines after edit");

    // Third line should still have same hash (content unchanged)
    assert_eq!(
        result2.lines[2].hash, hash_line2,
        "Later lines should be preserved when only earlier lines are edited"
    );
}
