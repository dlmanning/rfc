//! Error handling and recovery tests.
//!
//! These tests verify error detection, reporting, and recovery behavior.

use rpl_core::ErrorCode;
use rpl_session::Session;

/// Helper to analyze code and check for errors.
fn analyze_has_errors(code: &str) -> bool {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let result = session.analyze(id).expect("Analysis should succeed");
    result.has_errors()
}

/// Helper to get error codes from analysis.
fn get_error_codes(code: &str) -> Vec<ErrorCode> {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let result = session.analyze(id).expect("Analysis should succeed");
    result.diagnostics.iter().map(|d| d.code()).collect()
}

/// Helper to check if eval produces a runtime error.
fn eval_runtime_error(code: &str) -> bool {
    let mut session = Session::new();
    session.eval(code).is_err()
}

// ============================================================================
// Analysis Errors
// ============================================================================

#[test]
fn unknown_token() {
    // #$% should not be recognized by any library
    let codes = get_error_codes("3 #$% 4");
    assert!(
        codes.contains(&ErrorCode::E001),
        "Expected E001 for unknown token, got {:?}",
        codes
    );
}

#[test]
fn unterminated_string() {
    let codes = get_error_codes("\"hello");
    assert!(
        codes.contains(&ErrorCode::E002),
        "Expected E002 for unterminated string, got {:?}",
        codes
    );
}

#[test]
fn multiple_errors() {
    // Multiple unknown tokens should each produce an error
    // Use characters that aren't valid in any syntax
    let codes = get_error_codes("## $$ %%");
    assert!(
        codes.len() >= 3,
        "Expected at least 3 errors, got {:?}",
        codes
    );
    assert!(
        codes.iter().all(|c| *c == ErrorCode::E001),
        "All errors should be E001, got {:?}",
        codes
    );
}

// ============================================================================
// Error Recovery
// ============================================================================

#[test]
fn recovery_produces_tokens() {
    // Even with errors, valid tokens should still be recognized
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "3 $$$ 4 +");
    let result = session.analyze(id).expect("Analysis should succeed");

    // Should have errors
    assert!(result.has_errors(), "Expected errors for $$$");

    // But should also have tokens for the valid parts
    // Token count should be > 1 (at least the valid ones plus error token)
    assert!(
        result.token_count() >= 3,
        "Expected at least 3 tokens (3, error, 4, +), got {}",
        result.token_count()
    );
}

#[test]
fn error_spans_correct() {
    let mut session = Session::new();
    // Use $$ which is not recognized by any library (not a valid identifier)
    let id = session.set_source("test.rpl", "3 $$ 4");
    let result = session.analyze(id).expect("Analysis should succeed");

    assert!(result.has_errors(), "Expected error for '$$'");

    // Find the error diagnostic
    let error = result
        .diagnostics
        .iter()
        .find(|d| d.code() == ErrorCode::E001)
        .expect("Should have E001 error");

    // Check span covers "$$" (positions 2-4 in "3 $$ 4")
    let span = error.span();
    assert_eq!(span.start().offset(), 2, "Error span should start at 2");
    assert_eq!(span.end().offset(), 4, "Error span should end at 4");
}

// ============================================================================
// Runtime Errors
// ============================================================================

#[test]
fn stack_underflow_runtime() {
    // + with empty stack should compile but fail at runtime
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "+");
    let _analysis = session.analyze(id);

    // Should compile (no analysis errors for the operator itself)
    // The operator is valid, just requires stack items

    // But execution should fail
    let result = session.eval("+");
    assert!(
        result.is_err(),
        "Expected runtime error for stack underflow"
    );
}

#[test]
fn division_by_zero() {
    // 1 0 / should compile but fail at runtime
    let result = eval_runtime_error("1 0 /");
    assert!(result, "Expected runtime error for division by zero");
}
