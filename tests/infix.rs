//! Symbolic expression parsing and precedence tests.
//!
//! These tests verify infix notation parsing within symbolic expressions.

use rpl_core::TypeId;
use rpl_lang::Value;
use rpl_session::Session;

/// Helper to evaluate code and extract a single f64 result.
fn eval_to_real(code: &str) -> f64 {
    let mut session = Session::new();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));
    assert_eq!(
        values.len(),
        1,
        "Expected 1 value for '{}', got {:?}",
        code,
        values
    );
    match &values[0] {
        Value::Real(r) => *r,
        Value::Int(i) => *i as f64,
        other => panic!("Expected Real for '{}', got {:?}", code, other),
    }
}

/// Helper to check if code produces a symbolic object.
fn produces_symbolic(code: &str) -> bool {
    let mut session = Session::new();
    let values = session.eval(code).unwrap_or_else(|e| {
        panic!("eval failed for '{}': {:?}", code, e)
    });
    if values.len() != 1 {
        return false;
    }
    matches!(&values[0], Value::Object { type_id, .. } if type_id.as_u16() == TypeId::SYMBOLIC.as_u16())
}

// ============================================================================
// Symbolic Object Creation
// ============================================================================

#[test]
fn symbolic_creates_object() {
    // '3 + 4' should create a symbolic object, not evaluate immediately
    assert!(
        produces_symbolic("'3 + 4'"),
        "Expected symbolic object from '3 + 4'"
    );
}

// ============================================================================
// Basic Evaluation
// ============================================================================

#[test]
fn symbolic_eval() {
    let result = eval_to_real("'3 + 4' EVAL");
    assert!(
        (result - 7.0).abs() < 1e-10,
        "Expected 7.0, got {}",
        result
    );
}

// ============================================================================
// Operator Precedence
// ============================================================================

#[test]
fn precedence_mul_over_add() {
    // 3 + 4 * 5 = 3 + 20 = 23 (not (3+4)*5 = 35)
    let result = eval_to_real("'3 + 4 * 5' EVAL");
    assert!(
        (result - 23.0).abs() < 1e-10,
        "Expected 23.0 (mul before add), got {}",
        result
    );
}

#[test]
fn precedence_paren() {
    // (3 + 4) * 5 = 7 * 5 = 35
    let result = eval_to_real("'(3 + 4) * 5' EVAL");
    assert!(
        (result - 35.0).abs() < 1e-10,
        "Expected 35.0, got {}",
        result
    );
}

#[test]
fn right_assoc_power() {
    // 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (right associative)
    let result = eval_to_real("'2 ^ 3 ^ 2' EVAL");
    assert!(
        (result - 512.0).abs() < 1e-10,
        "Expected 512.0 (right assoc), got {}",
        result
    );
}

#[test]
fn left_assoc_sub() {
    // 10 - 3 - 2 = (10 - 3) - 2 = 7 - 2 = 5 (left associative)
    let result = eval_to_real("'10 - 3 - 2' EVAL");
    assert!(
        (result - 5.0).abs() < 1e-10,
        "Expected 5.0 (left assoc), got {}",
        result
    );
}

// ============================================================================
// Unary Operators
// ============================================================================

#[test]
fn unary_minus_alone() {
    let result = eval_to_real("'-5' EVAL");
    assert!(
        (result - (-5.0)).abs() < 1e-10,
        "Expected -5.0, got {}",
        result
    );
}

#[test]
fn unary_minus_after_op() {
    // 3 * -2 = -6
    let result = eval_to_real("'3 * -2' EVAL");
    assert!(
        (result - (-6.0)).abs() < 1e-10,
        "Expected -6.0, got {}",
        result
    );
}

#[test]
#[ignore = "requires parser support for double negative"]
fn double_negative() {
    // --5 = 5
    let result = eval_to_real("'--5' EVAL");
    assert!(
        (result - 5.0).abs() < 1e-10,
        "Expected 5.0, got {}",
        result
    );
}

// ============================================================================
// Parentheses
// ============================================================================

#[test]
fn nested_parens() {
    let result = eval_to_real("'((2 + 3))' EVAL");
    assert!(
        (result - 5.0).abs() < 1e-10,
        "Expected 5.0, got {}",
        result
    );
}

#[test]
fn complex_expr() {
    // (1 + 2) * (3 + 4) = 3 * 7 = 21
    let result = eval_to_real("'(1 + 2) * (3 + 4)' EVAL");
    assert!(
        (result - 21.0).abs() < 1e-10,
        "Expected 21.0, got {}",
        result
    );
}
