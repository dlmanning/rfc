//! Tests for comparison operators.

use super::assert_stack_eq;

// ============================================================================
// Comparison Operators
// Returns 1.0 for true, 0.0 for false
// ============================================================================

#[test]
fn compare_less_than_true() {
    assert_stack_eq("3 5 <", &[1.0]);
}

#[test]
fn compare_less_than_false() {
    assert_stack_eq("5 3 <", &[0.0]);
}

#[test]
fn compare_less_than_equal() {
    // Equal values should return false for <
    assert_stack_eq("5 5 <", &[0.0]);
}

#[test]
fn compare_greater_than_true() {
    assert_stack_eq("5 3 >", &[1.0]);
}

#[test]
fn compare_greater_than_false() {
    assert_stack_eq("3 5 >", &[0.0]);
}

#[test]
fn compare_less_equal_true() {
    assert_stack_eq("3 5 <=", &[1.0]);
}

#[test]
fn compare_less_equal_equal() {
    assert_stack_eq("5 5 <=", &[1.0]);
}

#[test]
fn compare_less_equal_false() {
    assert_stack_eq("5 3 <=", &[0.0]);
}

#[test]
fn compare_greater_equal_true() {
    assert_stack_eq("5 3 >=", &[1.0]);
}

#[test]
fn compare_greater_equal_equal() {
    assert_stack_eq("5 5 >=", &[1.0]);
}

#[test]
fn compare_greater_equal_false() {
    assert_stack_eq("3 5 >=", &[0.0]);
}

#[test]
fn compare_equal_true() {
    assert_stack_eq("5 5 ==", &[1.0]);
}

#[test]
fn compare_equal_false() {
    assert_stack_eq("5 3 ==", &[0.0]);
}

#[test]
fn compare_not_equal_true() {
    assert_stack_eq("5 3 !=", &[1.0]);
}

#[test]
fn compare_not_equal_false() {
    assert_stack_eq("5 5 !=", &[0.0]);
}

#[test]
fn compare_not_equal_alternate() {
    // Test <> alternate syntax
    assert_stack_eq("5 3 <>", &[1.0]);
}

#[test]
fn compare_same_true() {
    assert_stack_eq("42 42 SAME", &[1.0]);
}

#[test]
fn compare_same_false() {
    assert_stack_eq("42 43 SAME", &[0.0]);
}

#[test]
fn compare_chained() {
    // 3 5 < (true=1) then 1 0 > (true=1)
    assert_stack_eq("3 5 < 0 >", &[1.0]);
}

#[test]
fn compare_with_arithmetic() {
    // 2 + 3 = 5, 5 == 5 -> 1
    assert_stack_eq("2 3 + 5 ==", &[1.0]);
}

#[test]
fn compare_with_conditional() {
    // If 3 < 5 (true) then push 42, else push 0
    assert_stack_eq("3 5 < IF THEN 42 ELSE 0 END", &[42.0]);
}

#[test]
fn compare_with_conditional_false() {
    // If 5 < 3 (false) then push 42, else push 0
    assert_stack_eq("5 3 < IF THEN 42 ELSE 0 END", &[0.0]);
}

#[test]
fn compare_negative_numbers() {
    assert_stack_eq("-5 -3 <", &[1.0]);
    assert_stack_eq("-3 -5 <", &[0.0]);
}
