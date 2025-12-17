//! Tests for number literals and comments.

use super::assert_stack_eq;

// ============================================================================
// Number Literals
// ============================================================================

#[test]
fn single_integer() {
    assert_stack_eq("42", &[42.0]);
}

#[test]
fn single_float() {
    assert_stack_eq("3.15159", &[3.15159]);
}

#[test]
fn negative_literal() {
    assert_stack_eq("-17", &[-17.0]);
}

// ============================================================================
// Comments
// ============================================================================

#[test]
fn single_line_comment() {
    // Comment is ignored, only 5 remains on stack
    assert_stack_eq("5 @ this is a comment", &[5.0]);
}

#[test]
fn single_line_comment_with_newline() {
    // Comment ends at newline, 3 is pushed after
    assert_stack_eq("5 @ comment\n3 +", &[8.0]);
}

#[test]
fn permanent_comment() {
    // Permanent comment with @@ also ignored at runtime
    assert_stack_eq("5 @@ permanent comment", &[5.0]);
}

#[test]
fn multiline_comment() {
    // Multi-line comment with @@@
    assert_stack_eq("5 @@@ this is\na multi-line\ncomment @@@ 3 +", &[8.0]);
}

#[test]
fn empty_comment() {
    // Just @ alone is a valid empty comment
    assert_stack_eq("5 @", &[5.0]);
}

#[test]
fn comment_in_program() {
    // Comments work inside programs
    assert_stack_eq("<< 5 @ comment\n3 + >> EVAL", &[8.0]);
}

#[test]
fn multiple_comments() {
    // Multiple comments on different lines
    assert_stack_eq("5 @ first\n3 @ second\n+", &[8.0]);
}
