//! Tests using fixture files.

use super::{assert_stack_eq, eval_to_reals, eval_to_values};

// =============================================================================
// Fixture Files
// =============================================================================

#[test]
fn fixture_factorial() {
    let code = include_str!("../programs/factorial.rpl");
    assert_stack_eq(code, &[120.0]);
}

#[test]
fn fixture_fibonacci() {
    let code = include_str!("../programs/fibonacci.rpl");
    assert_stack_eq(code, &[55.0]);
}

#[test]
fn fixture_gcd() {
    let code = include_str!("../programs/gcd.rpl");
    assert_stack_eq(code, &[6.0]);
}

#[test]
#[allow(clippy::approx_constant)]
fn fixture_newton_sqrt() {
    let code = include_str!("../programs/newton_sqrt.rpl");
    let values = eval_to_reals(code);
    assert_eq!(values.len(), 1);
    // sqrt(2) ≈ 1.41421356...
    assert!((values[0] - 1.41421356).abs() < 0.0001);
}

#[test]
fn fixture_prime_sieve() {
    let code = include_str!("../programs/prime_sieve.rpl");
    // 17 is prime (1), 18 is not (0)
    assert_stack_eq(code, &[1.0, 0.0]);
}

#[test]
fn list_sum_function() {
    // Test sum from list_ops.rpl
    let code = "
        << -> lst <<
            0
            lst LIST->
            1 SWAP START
                +
            NEXT
        >> >>
        \"sum\" STO
        { 1 2 3 4 5 } sum
    ";
    assert_stack_eq(code, &[15.0]);
}

#[test]
fn fixture_list_ops() {
    let code = include_str!("../programs/list_ops.rpl");
    assert_stack_eq(code, &[15.0]);
}

#[test]
fn fixture_symbolic() {
    let code = include_str!("../programs/symbolic_derivative.rpl");
    let values = eval_to_values(code);
    // Should have 3 symbolic objects on the stack
    assert_eq!(values.len(), 3);
}

#[test]
fn fixture_quicksort() {
    let code = include_str!("../programs/quicksort.rpl");
    let values = eval_to_values(code);
    // Should have 1 sorted list on the stack
    assert_eq!(values.len(), 1);
}
