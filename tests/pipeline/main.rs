//! End-to-end compilation and execution tests.
//!
//! These tests verify the complete source → compile → execute path.
//! Tests are organized into modules by functionality.

use rpl::value::Value;
use rpl::Session;

/// Create a session with the standard library registered.
pub fn session_with_stdlib() -> Session {
    let mut session = Session::new();
    rpl_stdlib::register_interfaces(session.registry_mut());
    rpl_stdlib::register_impls(session.registry_mut());
    session
}

// Test modules
mod analysis;
mod arithmetic;
mod binary_int;
mod comparison;
mod complex;
mod directory;
mod errors;
mod fixtures;
mod flow;
mod libraries;
mod lists;
mod literals;
mod programs;
mod stack;
mod strings;
mod symbolic;
mod transcendentals;

// ============================================================================
// Test Helpers
// ============================================================================

/// Helper to evaluate code and extract the stack as f64 values.
pub fn eval_to_reals(code: &str) -> Vec<f64> {
    let mut session = session_with_stdlib();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));
    values
        .iter()
        .map(|v| match v {
            Value::Real(r) => *r,
            Value::Integer(i) => *i as f64,
            other => panic!("Expected Real or Integer, got {:?}", other),
        })
        .collect()
}

/// Helper to evaluate code and return all values (not just reals).
pub fn eval_to_values(code: &str) -> Vec<Value> {
    let mut session = session_with_stdlib();
    session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e))
}

/// Helper to check stack contents with floating point tolerance.
pub fn assert_stack_eq(code: &str, expected: &[f64]) {
    assert_stack_approx(code, expected, 1e-10);
}

/// Helper to check stack contents with custom epsilon for floating point comparison.
pub fn assert_stack_approx(code: &str, expected: &[f64], epsilon: f64) {
    let actual = eval_to_reals(code);
    assert_eq!(
        actual.len(),
        expected.len(),
        "Stack depth mismatch for '{}': expected {:?}, got {:?}",
        code,
        expected,
        actual
    );
    for (i, (a, e)) in actual.iter().zip(expected.iter()).enumerate() {
        assert!(
            (a - e).abs() < epsilon,
            "Stack[{}] mismatch for '{}': expected {}, got {} (diff: {})",
            i,
            code,
            e,
            a,
            (a - e).abs()
        );
    }
}

/// Helper to check that code produces an error containing a substring.
pub fn assert_error(code: &str, expected_substring: &str) {
    let mut session = session_with_stdlib();
    match session.eval(code) {
        Ok(_) => panic!("Expected error for '{}', but succeeded", code),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.to_lowercase()
                    .contains(&expected_substring.to_lowercase()),
                "Error '{}' doesn't contain '{}' for code '{}'",
                msg,
                expected_substring,
                code
            );
        }
    }
}

/// Helper to extract list contents as f64 values.
pub fn list_to_reals(value: &Value) -> Vec<f64> {
    match value {
        Value::List(elements) => elements
            .iter()
            .map(|v| match v {
                Value::Real(r) => *r,
                Value::Integer(i) => *i as f64,
                other => panic!("Expected Real or Integer in list, got {:?}", other),
            })
            .collect(),
        other => panic!("Expected List, got {:?}", other),
    }
}

/// Helper to extract string from Value.
pub fn to_string(value: &Value) -> &str {
    match value {
        Value::String(s) => s,
        other => panic!("Expected String, got {:?}", other),
    }
}

/// Helper to extract integer value.
pub fn to_int(value: &Value) -> i64 {
    match value {
        Value::Integer(i) => *i,
        Value::Real(r) => *r as i64,
        other => panic!("Expected Integer or Real, got {:?}", other),
    }
}
