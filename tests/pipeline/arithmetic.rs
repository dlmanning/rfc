//! Tests for arithmetic operations.

use super::assert_stack_eq;

// ============================================================================
// Binary Arithmetic
// ============================================================================

#[test]
fn binary_add() {
    assert_stack_eq("3 4 +", &[7.0]);
}

#[test]
fn binary_sub() {
    assert_stack_eq("10 3 -", &[7.0]);
}

#[test]
fn binary_mul() {
    assert_stack_eq("6 7 *", &[42.0]);
}

#[test]
fn binary_div() {
    assert_stack_eq("20 4 /", &[5.0]);
}

// ============================================================================
// Unary Arithmetic
// ============================================================================

#[test]
fn unary_neg() {
    assert_stack_eq("5 NEG", &[-5.0]);
}

#[test]
fn unary_abs() {
    assert_stack_eq("-3 ABS", &[3.0]);
}

// ============================================================================
// Real Number Functions
// ============================================================================

#[test]
fn sq_positive() {
    assert_stack_eq("5 SQ", &[25.0]);
}

#[test]
fn sq_negative() {
    assert_stack_eq("-4 SQ", &[16.0]);
}

#[test]
fn min_first_smaller() {
    assert_stack_eq("3 7 MIN", &[3.0]);
}

#[test]
fn min_second_smaller() {
    assert_stack_eq("8 2 MIN", &[2.0]);
}

#[test]
fn max_first_larger() {
    assert_stack_eq("9 4 MAX", &[9.0]);
}

#[test]
fn max_second_larger() {
    assert_stack_eq("2 6 MAX", &[6.0]);
}

#[test]
fn sign_positive() {
    assert_stack_eq("42 SIGN", &[1.0]);
}

#[test]
fn sign_negative() {
    assert_stack_eq("-17 SIGN", &[-1.0]);
}

#[test]
fn sign_zero() {
    assert_stack_eq("0 SIGN", &[0.0]);
}

// ============================================================================
// Chained Operations
// ============================================================================

#[test]
fn chained_ops() {
    assert_stack_eq("1 2 + 3 +", &[6.0]);
}

#[test]
fn mixed_precedence() {
    // RPN: 2 3 4 + * means 2 * (3 + 4) = 14
    assert_stack_eq("2 3 4 + *", &[14.0]);
}
