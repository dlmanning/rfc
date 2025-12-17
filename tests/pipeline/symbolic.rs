//! Tests for symbolic expression evaluation.

use super::assert_stack_eq;

// ============================================================================
// Symbolic Expression Evaluation
// ============================================================================

#[test]
fn eval_symbolic_numeric() {
    // '3 + 4' EVAL → 7
    assert_stack_eq("'3 + 4' EVAL", &[7.0]);
}

#[test]
fn eval_symbolic_precedence() {
    // '3 + 4 * 5' EVAL → 23 (multiplication before addition)
    assert_stack_eq("'3 + 4 * 5' EVAL", &[23.0]);
}

#[test]
fn eval_symbolic_parentheses() {
    // '(3 + 4) * 5' EVAL → 35
    assert_stack_eq("'(3 + 4) * 5' EVAL", &[35.0]);
}

#[test]
fn eval_symbolic_power() {
    // '2 ^ 3' EVAL → 8
    assert_stack_eq("'2 ^ 3' EVAL", &[8.0]);
}

#[test]
fn eval_symbolic_with_global() {
    // 10 "x" STO 'x + 5' EVAL → 15
    assert_stack_eq("10 \"x\" STO 'x + 5' EVAL", &[15.0]);
}

#[test]
fn eval_symbolic_with_local() {
    // 5 → x « 'x + 3' EVAL » → 8
    assert_stack_eq("5 → x « 'x + 3' EVAL »", &[8.0]);
}

#[test]
fn eval_symbolic_complex() {
    // '(1 + 2) * (3 + 4)' EVAL → 21
    assert_stack_eq("'(1 + 2) * (3 + 4)' EVAL", &[21.0]);
}

// ============================================================================
// Operator Associativity (migrated from tests/infix.rs)
// ============================================================================

#[test]
fn eval_symbolic_right_assoc_power() {
    // 2 ^ 3 ^ 2 = 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (right associative)
    assert_stack_eq("'2 ^ 3 ^ 2' EVAL", &[512.0]);
}

#[test]
fn eval_symbolic_left_assoc_sub() {
    // 10 - 3 - 2 = (10 - 3) - 2 = 7 - 2 = 5 (left associative)
    assert_stack_eq("'10 - 3 - 2' EVAL", &[5.0]);
}

// ============================================================================
// Unary Operators (migrated from tests/infix.rs)
// ============================================================================

#[test]
fn eval_symbolic_unary_minus_alone() {
    // '-5' EVAL → -5
    assert_stack_eq("'-5' EVAL", &[-5.0]);
}

#[test]
fn eval_symbolic_unary_minus_after_op() {
    // '3 * -2' EVAL → -6
    assert_stack_eq("'3 * -2' EVAL", &[-6.0]);
}

// ============================================================================
// Parentheses (migrated from tests/infix.rs)
// ============================================================================

#[test]
fn eval_symbolic_nested_parens() {
    // '((2 + 3))' EVAL → 5
    assert_stack_eq("'((2 + 3))' EVAL", &[5.0]);
}
