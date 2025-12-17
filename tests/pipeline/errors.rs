//! Runtime error tests (migrated from tests/errors.rs).
//!
//! These tests verify runtime error detection and reporting.

use rpl::Session;

/// Helper to check if eval produces a runtime error.
fn eval_produces_error(code: &str) -> bool {
    let mut session = Session::new();
    session.eval(code).is_err()
}

// ============================================================================
// Runtime Errors
// ============================================================================

#[test]
fn stack_underflow_runtime() {
    // + with empty stack should fail at runtime
    assert!(
        eval_produces_error("+"),
        "Expected runtime error for stack underflow"
    );
}

#[test]
fn stack_underflow_binary_op() {
    // Binary operators need two operands
    assert!(eval_produces_error("5 +"), "5 + needs another operand");
    assert!(eval_produces_error("*"), "* with empty stack");
    assert!(eval_produces_error("/"), "/ with empty stack");
    assert!(eval_produces_error("-"), "- with empty stack");
}

#[test]
fn division_by_zero() {
    // 1 0 / should fail at runtime
    assert!(
        eval_produces_error("1 0 /"),
        "Expected runtime error for division by zero"
    );
}

#[test]
fn integer_division_by_zero() {
    // Integer division by zero
    assert!(
        eval_produces_error("10 0 MOD"),
        "Expected runtime error for modulo by zero"
    );
}

#[test]
fn undefined_variable() {
    // RCL of undefined variable should fail
    assert!(
        eval_produces_error("\"undefined_var\" RCL"),
        "Expected runtime error for undefined variable"
    );
}

#[test]
fn purge_undefined_variable() {
    // PURGE of undefined variable should fail
    assert!(
        eval_produces_error("\"nonexistent\" PURGE"),
        "Expected runtime error for purging undefined variable"
    );
}

#[test]
fn type_error_add_string_to_number() {
    // Adding incompatible types should fail
    assert!(
        eval_produces_error("5 \"hello\" +"),
        "Expected type error adding string to number"
    );
}

#[test]
fn list_index_out_of_bounds() {
    // GET with invalid index
    assert!(
        eval_produces_error("{ 1 2 3 } 10 GET"),
        "Expected error for list index out of bounds"
    );
    assert!(
        eval_produces_error("{ 1 2 3 } 0 GET"),
        "Expected error for index 0 (1-indexed)"
    );
}

#[test]
fn pick_invalid_depth() {
    // PICK with invalid stack depth
    assert!(
        eval_produces_error("1 2 10 PICK"),
        "Expected error for PICK with invalid depth"
    );
}
