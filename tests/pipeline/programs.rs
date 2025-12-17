//! Tests for program literals and local bindings.

use rpl::value::Value;
use rpl::Session;

use super::assert_stack_eq;

// ============================================================================
// Programs (<< ... >>)
// ============================================================================

/// Helper to evaluate code and check if a program object is on the stack.
fn eval_has_program(code: &str) -> bool {
    let mut session = Session::new();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));

    values.iter().any(|v| matches!(v, Value::Program(_)))
}

#[test]
fn program_literal_creates_object() {
    // << 1 2 + >> should create a program object on the stack
    assert!(eval_has_program("<< 1 2 + >>"));
}

#[test]
fn program_literal_empty() {
    // << >> should create an empty program
    assert!(eval_has_program("<< >>"));
}

#[test]
fn program_eval_simple() {
    // << 3 4 + >> EVAL should execute the program and leave 7
    assert_stack_eq("<< 3 4 + >> EVAL", &[7.0]);
}

#[test]
fn program_eval_with_stack() {
    // Push 5, then a program that adds 10, then EVAL
    // 5 << 10 + >> EVAL = 5 10 + = 15
    assert_stack_eq("5 << 10 + >> EVAL", &[15.0]);
}

#[test]
fn program_eval_nested() {
    // << << 1 2 + >> EVAL >> EVAL = 3
    assert_stack_eq("<< << 1 2 + >> EVAL >> EVAL", &[3.0]);
}

#[test]
fn program_multiple() {
    // Two programs, EVAL both
    // << 2 >> EVAL << 3 >> EVAL leaves 2 and 3 on stack
    assert_stack_eq("<< 2 >> EVAL << 3 >> EVAL", &[2.0, 3.0]);
}

#[test]
fn program_multiple_with_add() {
    // Two programs with addition after
    // << 2 >> << 3 >> EVAL SWAP EVAL +
    // Creates prog1, prog2, evals prog2 -> 3, swap -> prog1 3, evals prog1 -> 2, + -> 5
    assert_stack_eq("<< 2 >> << 3 >> EVAL SWAP EVAL +", &[5.0]);
}

#[test]
fn program_stored() {
    // Store a program and execute it
    assert_stack_eq("<< DUP * >> \"sq\" STO 5 sq", &[25.0]);
}

// ============================================================================
// Local Variable Bindings
// ============================================================================

#[test]
fn local_binding_simple() {
    assert_stack_eq("5 → x « x »", &[5.0]);
}

#[test]
fn local_binding_use_twice() {
    // x + x = 10
    assert_stack_eq("5 → x « x x + »", &[10.0]);
}

#[test]
fn local_binding_square() {
    // 4² = 16
    assert_stack_eq("4 → x « x x * »", &[16.0]);
}

#[test]
fn local_binding_multiple_params() {
    // a=3, b=4, a+b=7
    assert_stack_eq("3 4 → a b « a b + »", &[7.0]);
}

#[test]
fn local_binding_arrow_syntax() {
    // ASCII arrow -> works too
    assert_stack_eq("5 -> x << x >>", &[5.0]);
}

#[test]
fn local_binding_with_computation() {
    // 10/2 + 10*3 = 5 + 30 = 35
    assert_stack_eq("10 → x « x 2 / x 3 * + »", &[35.0]);
}

// Programs with local bindings work when stored and recalled.
// The interner is stored in the VM and available during nested EVAL calls.
#[test]
fn local_binding_in_stored_program() {
    assert_stack_eq(
        r#"<< -> n << n 1 + >> >> "add1" STO 5 add1"#,
        &[6.0],
    );
}

// Jump targets in stored programs are compiled as relative offsets to the
// program body start, so they work correctly when the program is extracted
// and executed via EVAL (where PC starts at 0 within the body).
#[test]
fn if_else_in_stored_program() {
    // IF with true condition - should return 100
    assert_stack_eq("<< IF 1 THEN 100 ELSE 200 END >> EVAL", &[100.0]);
    // IF with false condition - should return 200
    assert_stack_eq("<< IF 0 THEN 100 ELSE 200 END >> EVAL", &[200.0]);
}
