//! End-to-end compilation and execution tests.
//!
//! These tests verify the complete source → compile → execute path.

use rpl_lang::Value;
use rpl_session::Session;

/// Helper to evaluate code and extract the stack as f64 values.
fn eval_to_reals(code: &str) -> Vec<f64> {
    let mut session = Session::new();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));
    values
        .iter()
        .map(|v| match v {
            Value::Real(r) => *r,
            Value::Int(i) => *i as f64,
            other => panic!("Expected Real or Int, got {:?}", other),
        })
        .collect()
}

/// Helper to check stack contents with floating point tolerance.
fn assert_stack_eq(code: &str, expected: &[f64]) {
    assert_stack_approx(code, expected, 1e-10);
}

/// Helper to check stack contents with custom epsilon for floating point comparison.
fn assert_stack_approx(code: &str, expected: &[f64], epsilon: f64) {
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
fn assert_error(code: &str, expected_substring: &str) {
    let mut session = Session::new();
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

// ============================================================================
// Stack Manipulation
// ============================================================================

#[test]
fn dup() {
    // 5 DUP + = 5 5 + = 10
    assert_stack_eq("5 DUP +", &[10.0]);
}

#[test]
fn swap() {
    // 3 4 SWAP - = 4 3 - = 1
    assert_stack_eq("3 4 SWAP -", &[1.0]);
}

#[test]
fn over() {
    // 5 6 OVER = 5 6 5
    // 5 6 5 + = 5 11
    // 5 11 + = 16
    assert_stack_eq("5 6 OVER + +", &[16.0]);
}

#[test]
fn rot() {
    // ROT: (a b c -- b c a)
    // 1 2 3 ROT = 2 3 1
    assert_stack_eq("1 2 3 ROT", &[2.0, 3.0, 1.0]);
}

#[test]
fn depth() {
    // DEPTH pushes current stack depth before DEPTH executes
    // Stack: 1 2 3 -> DEPTH sees depth 3 -> pushes 3
    assert_stack_eq("1 2 3 DEPTH", &[1.0, 2.0, 3.0, 3.0]);
}

#[test]
fn drop() {
    assert_stack_eq("1 2 3 DROP", &[1.0, 2.0]);
}

#[test]
fn pick_first() {
    // PICK 1 copies top of stack
    assert_stack_eq("1 2 3 1 PICK", &[1.0, 2.0, 3.0, 3.0]);
}

#[test]
fn pick_second() {
    // PICK 2 copies second from top
    assert_stack_eq("1 2 3 2 PICK", &[1.0, 2.0, 3.0, 2.0]);
}

#[test]
fn pick_third() {
    // PICK 3 copies third from top
    assert_stack_eq("1 2 3 3 PICK", &[1.0, 2.0, 3.0, 1.0]);
}

#[test]
fn roll_two() {
    // ROLL 2 is like SWAP
    assert_stack_eq("1 2 3 2 ROLL", &[1.0, 3.0, 2.0]);
}

#[test]
fn roll_three() {
    // ROLL 3 is like ROT
    assert_stack_eq("1 2 3 3 ROLL", &[2.0, 3.0, 1.0]);
}

#[test]
fn roll_one() {
    // ROLL 1 is a no-op
    assert_stack_eq("1 2 3 1 ROLL", &[1.0, 2.0, 3.0]);
}

#[test]
fn dup2_basic() {
    // DUP2: (a b -- a b a b)
    assert_stack_eq("1 2 DUP2", &[1.0, 2.0, 1.0, 2.0]);
}

#[test]
fn ift_true() {
    // IFT: keep obj if flag is true
    assert_stack_eq("42 1 IFT", &[42.0]);
}

#[test]
fn ift_false() {
    // IFT: drop obj if flag is false
    assert_stack_eq("42 0 IFT", &[]);
}

#[test]
fn ifte_true() {
    // IFTE: keep true_obj if flag is true
    assert_stack_eq("10 20 1 IFTE", &[10.0]);
}

#[test]
fn ifte_false() {
    // IFTE: keep false_obj if flag is false
    assert_stack_eq("10 20 0 IFTE", &[20.0]);
}

// ============================================================================
// Additional Stack Commands
// ============================================================================

#[test]
fn unrot() {
    // UNROT: ( a b c -- c a b ) - opposite of ROT
    assert_stack_eq("1 2 3 UNROT", &[3.0, 1.0, 2.0]);
}

#[test]
fn drop2() {
    // DROP2: ( a b -- ) - drop top two items
    assert_stack_eq("1 2 3 4 DROP2", &[1.0, 2.0]);
}

#[test]
fn nip() {
    // NIP: ( a b -- b ) - remove second item
    assert_stack_eq("1 2 NIP", &[2.0]);
}

#[test]
fn nip_with_more() {
    assert_stack_eq("1 2 3 NIP", &[1.0, 3.0]);
}

#[test]
fn dupn() {
    // DUPN: ( ... n -- ... ... ) - duplicate top n items
    assert_stack_eq("1 2 3 2 DUPN", &[1.0, 2.0, 3.0, 2.0, 3.0]);
}

#[test]
fn dupn_one() {
    // DUPN 1 is like DUP
    assert_stack_eq("5 1 DUPN", &[5.0, 5.0]);
}

#[test]
fn dupn_zero() {
    // DUPN 0 does nothing
    assert_stack_eq("1 2 0 DUPN", &[1.0, 2.0]);
}

#[test]
fn dropn() {
    // DROPN: ( ... n -- ) - drop top n items
    assert_stack_eq("1 2 3 4 2 DROPN", &[1.0, 2.0]);
}

#[test]
fn dropn_zero() {
    // DROPN 0 does nothing
    assert_stack_eq("1 2 0 DROPN", &[1.0, 2.0]);
}

#[test]
fn rolld() {
    // ROLLD: roll top to nth position (opposite of ROLL)
    // ROLLD 3: ( a b c -- b c a ) wait no, that's ROT
    // Actually ROLLD 3: ( a b c -- c a b ) moves top to 3rd position
    assert_stack_eq("1 2 3 3 ROLLD", &[3.0, 1.0, 2.0]);
}

#[test]
fn rolld_2() {
    // ROLLD 2 is like SWAP
    assert_stack_eq("1 2 2 ROLLD", &[2.0, 1.0]);
}

#[test]
fn dupdup() {
    // DUPDUP: ( a -- a a a ) - duplicate twice
    assert_stack_eq("5 DUPDUP", &[5.0, 5.0, 5.0]);
}

#[test]
fn pick3() {
    // PICK3: ( a b c -- a b c a ) - copy third to top
    assert_stack_eq("1 2 3 PICK3", &[1.0, 2.0, 3.0, 1.0]);
}

#[test]
fn ndupn() {
    // NDUPN: ( obj n -- obj obj ... obj n ) - duplicate obj n times, push n
    assert_stack_eq("5 3 NDUPN", &[5.0, 5.0, 5.0, 3.0]);
}

#[test]
fn ndupn_one() {
    // NDUPN with n=1 keeps obj and pushes 1
    assert_stack_eq("5 1 NDUPN", &[5.0, 1.0]);
}

#[test]
fn revn() {
    // REVN: ( ... n -- ... ) - reverse top n items
    assert_stack_eq("1 2 3 3 REVN", &[3.0, 2.0, 1.0]);
}

#[test]
fn revn_2() {
    // REVN 2 is like SWAP
    assert_stack_eq("1 2 2 REVN", &[2.0, 1.0]);
}

#[test]
fn revn_1() {
    // REVN 1 does nothing
    assert_stack_eq("1 2 1 REVN", &[1.0, 2.0]);
}

#[test]
fn unpick() {
    // UNPICK: ( ... obj n -- ... ) - move obj to nth position
    assert_stack_eq("1 2 99 3 UNPICK", &[99.0, 1.0, 2.0]);
}

#[test]
fn unpick_1() {
    // UNPICK 1 leaves obj on top
    assert_stack_eq("1 2 99 1 UNPICK", &[1.0, 2.0, 99.0]);
}

// ============================================================================
// Multiple Values
// ============================================================================

#[test]
fn multiple_values() {
    assert_stack_eq("1 2 3", &[1.0, 2.0, 3.0]);
}

#[test]
fn empty_program() {
    assert_stack_eq("", &[]);
}

// ============================================================================
// Programs (:: ... ;)
// ============================================================================

/// Helper to evaluate code and check if a program object is on the stack.
fn eval_has_program(code: &str) -> bool {
    use rpl_core::TypeId;
    let mut session = Session::new();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));

    values.iter().any(|v| match v {
        Value::Program { .. } => true,
        Value::Object { type_id, .. } => *type_id == TypeId::PROGRAM,
        _ => false,
    })
}

#[test]
fn program_literal_creates_object() {
    // :: 1 2 + ; should create a program object on the stack
    assert!(eval_has_program(":: 1 2 + ;"));
}

#[test]
fn program_literal_empty() {
    // :: ; should create an empty program
    assert!(eval_has_program(":: ;"));
}

#[test]
fn program_eval_simple() {
    // :: 3 4 + ; EVAL should execute the program and leave 7
    assert_stack_eq(":: 3 4 + ; EVAL", &[7.0]);
}

#[test]
fn program_eval_with_stack() {
    // Push 5, then a program that adds 10, then EVAL
    // 5 :: 10 + ; EVAL = 5 10 + = 15
    assert_stack_eq("5 :: 10 + ; EVAL", &[15.0]);
}

#[test]
fn program_eval_nested() {
    // :: :: 1 2 + ; EVAL ; EVAL = 3
    assert_stack_eq(":: :: 1 2 + ; EVAL ; EVAL", &[3.0]);
}

#[test]
fn program_multiple() {
    // Two programs, EVAL both - simpler version without operators after EVAL
    // :: 2 ; EVAL :: 3 ; EVAL leaves 2 and 3 on stack
    assert_stack_eq(":: 2 ; EVAL :: 3 ; EVAL", &[2.0, 3.0]);
}

#[test]
fn program_multiple_with_add() {
    // Two programs with addition after - uses dynamic dispatch
    // :: 2 ; :: 3 ; EVAL SWAP EVAL +
    // Creates prog1, prog2, evals prog2 -> 3, swap -> prog1 3, evals prog1 -> 2, + -> 5
    assert_stack_eq(":: 2 ; :: 3 ; EVAL SWAP EVAL +", &[5.0]);
}

// ============================================================================
// Programs with chevron syntax (<< ... >>)
// ============================================================================

#[test]
fn program_chevron_literal() {
    // << 1 2 + >> should create a program object
    assert!(eval_has_program("<< 1 2 + >>"));
}

#[test]
fn program_chevron_eval() {
    // << 3 4 + >> EVAL should execute and leave 7
    assert_stack_eq("<< 3 4 + >> EVAL", &[7.0]);
}

#[test]
fn program_chevron_nested() {
    // << << 5 >> EVAL >> EVAL = 5
    assert_stack_eq("<< << 5 >> EVAL >> EVAL", &[5.0]);
}

#[test]
fn program_chevron_mixed_with_colon() {
    // Can mix chevrons and colons - they're equivalent
    assert_stack_eq("<< 2 3 + ; EVAL", &[5.0]); // open with <<, close with ;
    assert_stack_eq(":: 2 3 + >> EVAL", &[5.0]); // open with ::, close with >>
}

#[test]
fn program_chevron_stored() {
    // Store a chevron program and execute it
    assert_stack_eq("<< DUP * >> \"sq\" STO 5 sq", &[25.0]);
}

// ============================================================================
// Flow Control: IF/THEN/ELSE/END
// HP RPL syntax: IF <test> THEN <true-clause> [ELSE <false-clause>] END
// ============================================================================

#[test]
fn if_then_end_true() {
    // IF 1 THEN 42 END -> test is 1 (truthy), execute true-clause
    assert_stack_eq("IF 1 THEN 42 END", &[42.0]);
}

#[test]
fn if_then_end_false() {
    // IF 0 THEN 42 END -> test is 0 (falsy), skip true-clause
    assert_stack_eq("IF 0 THEN 42 END", &[]);
}

#[test]
fn if_then_else_true() {
    // IF 1 THEN 42 ELSE 99 END -> test is truthy, result 42
    assert_stack_eq("IF 1 THEN 42 ELSE 99 END", &[42.0]);
}

#[test]
fn if_then_else_false() {
    // IF 0 THEN 42 ELSE 99 END -> test is falsy, result 99
    assert_stack_eq("IF 0 THEN 42 ELSE 99 END", &[99.0]);
}

#[test]
fn if_with_arithmetic_in_test() {
    // IF 3 4 + 7 - THEN 1 ELSE 0 END -> test is 0 (falsy), result 0
    assert_stack_eq("IF 3 4 + 7 - THEN 1 ELSE 0 END", &[0.0]);
}

#[test]
fn if_with_arithmetic_in_branches() {
    // IF 1 THEN 3 4 + ELSE 10 20 + END -> test truthy, result 7
    assert_stack_eq("IF 1 THEN 3 4 + ELSE 10 20 + END", &[7.0]);
    // IF 0 THEN 3 4 + ELSE 10 20 + END -> test falsy, result 30
    assert_stack_eq("IF 0 THEN 3 4 + ELSE 10 20 + END", &[30.0]);
}

#[test]
fn if_preserves_stack() {
    // 5 IF 1 THEN 2 * ELSE DROP 0 END -> 5 on stack, test truthy, 5 2 * = 10
    assert_stack_eq("5 IF 1 THEN 2 * ELSE DROP 0 END", &[10.0]);
}

#[test]
fn if_nested() {
    // IF 1 THEN IF 1 THEN 42 END END -> outer true, inner true, result 42
    assert_stack_eq("IF 1 THEN IF 1 THEN 42 END END", &[42.0]);
}

#[test]
fn if_nested_false_outer() {
    // IF 0 THEN IF 1 THEN 42 END ELSE 99 END -> outer false, result 99
    assert_stack_eq("IF 0 THEN IF 1 THEN 42 END ELSE 99 END", &[99.0]);
}

#[test]
fn if_nested_false_inner() {
    // IF 1 THEN IF 0 THEN 42 ELSE 55 END END -> outer true, inner false, result 55
    assert_stack_eq("IF 1 THEN IF 0 THEN 42 ELSE 55 END END", &[55.0]);
}

// ============================================================================
// Flow Control: DO/UNTIL/END (test at end)
// HP RPL syntax: DO <loop-body> <test> UNTIL END
// Executes body, then tests; repeats while test is false
// ============================================================================

#[test]
fn do_until_once() {
    // DO body executes once, UNTIL pops and tests - exits when truthy
    assert_stack_eq("DO 1 UNTIL END", &[]);
}

#[test]
fn do_until_counter() {
    // 3 - 1 = 2, DUP leaves copy, UNTIL pops 2 (truthy), exits with 2 on stack
    assert_stack_eq("3 DO 1 - DUP UNTIL END", &[2.0]);
}

#[test]
fn do_until_to_zero() {
    // 1-1=0, DUP, UNTIL pops 0 (falsy), loops back
    // 0-1=-1, DUP, UNTIL pops -1 (truthy), exits with -1 on stack
    assert_stack_eq("1 DO 1 - DUP UNTIL END", &[-1.0]);
}

// ============================================================================
// Flow Control: WHILE/REPEAT/END (test at start)
// HP RPL syntax: WHILE <test> REPEAT <body> END
// Tests first; if true executes body, repeats
// ============================================================================

#[test]
fn while_never_executes() {
    // Test is 0 (falsy), body never executes
    assert_stack_eq("WHILE 0 REPEAT 999 END", &[]);
}

#[test]
fn while_executes_once() {
    // 1, test truthy, subtract 1 -> 0, test 0 (falsy), exit with 0
    assert_stack_eq("1 WHILE DUP REPEAT 1 - END", &[0.0]);
}

#[test]
fn while_counter() {
    // Count down from 3: 3->2->1->0, exit when 0 (falsy)
    assert_stack_eq("3 WHILE DUP REPEAT 1 - END", &[0.0]);
}

#[test]
fn while_accumulator() {
    // Sum 1+2+3=6: accumulator and counter on stack
    assert_stack_eq("0 3 WHILE DUP REPEAT SWAP OVER + SWAP 1 - END DROP", &[6.0]);
}

// ============================================================================
// Flow Control: START/NEXT (definite loop)
// HP RPL syntax: start finish START <body> NEXT
// Executes body (finish - start + 1) times
// ============================================================================

#[test]
fn start_next_once() {
    // 1 1 START 42 NEXT -> 1 iteration, pushes 42
    assert_stack_eq("1 1 START 42 NEXT", &[42.0]);
}

#[test]
fn start_next_three_times() {
    // 1 3 START 1 NEXT -> 3 iterations, pushes 1 three times
    assert_stack_eq("1 3 START 1 NEXT", &[1.0, 1.0, 1.0]);
}

#[test]
fn start_next_reversed() {
    // 5 4 START 42 NEXT -> finish < start, HP behavior: body runs once, then 6 <= 4 fails
    // So body executes exactly once
    assert_stack_eq("5 4 START 42 NEXT", &[42.0]);
}

#[test]
fn start_next_accumulator() {
    // Sum by repeated addition: 0 then add 1 five times -> 5
    // 0 1 5 START 1 + NEXT -> 0+1+1+1+1+1 = 5
    assert_stack_eq("0 1 5 START 1 + NEXT", &[5.0]);
}

#[test]
fn start_next_with_dup() {
    // 0 1 3 START DUP NEXT -> 0 0 0 0 (original 0 plus 3 dups)
    assert_stack_eq("0 1 3 START DUP NEXT", &[0.0, 0.0, 0.0, 0.0]);
}

// ============================================================================
// FOR/NEXT loops (with loop variable)
// ============================================================================

#[test]
fn start_with_nested_local() {
    // Test START/NEXT with a nested local binding - should iterate
    assert_stack_eq("1 1 3 START -> x :: x 1 + ; NEXT", &[4.0]);

    // With outer local binding (nested scopes)
    assert_stack_eq("0 -> y :: 1 1 3 START -> x :: x 1 + ; NEXT ;", &[4.0]);

    // Using outer var inside the loop body
    assert_stack_eq("5 -> y :: 0 1 3 START -> x :: x y + ; NEXT ;", &[15.0]);
}

#[test]
fn start_next_basic_accumulator() {
    // START/NEXT basic: 0 1 5 START 1 + NEXT -> 5 iterations, add 1 each time
    assert_stack_eq("0 1 5 START 1 + NEXT", &[5.0]);
}

#[test]
fn for_next_accumulator() {
    // FOR/NEXT basic: 0 1 5 FOR i i + NEXT -> 0+1+2+3+4+5 = 15
    assert_stack_eq("0 1 5 FOR i i + NEXT", &[15.0]);
}

#[test]
fn nested_start_with_outer_local() {
    // Nested START with locals: 5 -> x :: 0 1 3 START x + NEXT ;
    // Start with 0, add x=5 three times: 0+5+5+5 = 15
    assert_stack_eq("5 -> x :: 0 1 3 START x + NEXT ;", &[15.0]);
}

#[test]
fn for_doesnt_clobber_outer_local() {
    // FOR doesn't clobber outer: 10 -> y :: 1 3 FOR i i y + NEXT ;
    // Pushes i+y for i=1,2,3: 1+10=11, 2+10=12, 3+10=13
    assert_stack_eq("10 -> y :: 1 3 FOR i i y + NEXT ;", &[11.0, 12.0, 13.0]);
}

#[test]
fn nested_for_inside_start() {
    // FOR with nested START: outer FOR runs i=1,2, inner START adds i three times
    // i=1: 0+1+1+1 = 3, i=2: 3+2+2+2 = 9
    assert_stack_eq("0 1 2 FOR i 1 3 START i + NEXT NEXT", &[9.0]);
}

#[test]
fn nested_start_inside_for() {
    // START with nested FOR: outer runs 2 times, inner adds 1+2+3=6 each time
    // First iteration: 0+1+2+3 = 6, second: 6+1+2+3 = 12
    assert_stack_eq("0 1 2 START 1 3 FOR j j + NEXT NEXT", &[12.0]);
}

#[test]
fn start_reversed_with_accumulator() {
    // HP behavior: 0 5 4 START 1 + NEXT -> finish < start, body runs once
    // Stack: 0, then after body: 1, then 6 <= 4 fails, exit with 1
    assert_stack_eq("0 5 4 START 1 + NEXT", &[1.0]);
}

#[test]
fn for_reversed_with_accumulator() {
    // HP behavior: 0 5 4 FOR i i + NEXT -> finish < start, body runs once
    // Stack: 0, i=5, after body: 0+5=5, then 6 <= 4 fails, exit with 5
    assert_stack_eq("0 5 4 FOR i i + NEXT", &[5.0]);
}

#[test]
fn for_next_basic() {
    // 1 3 FOR i i NEXT -> pushes 1 2 3
    assert_stack_eq("1 3 FOR i i NEXT", &[1.0, 2.0, 3.0]);
}

#[test]
fn for_next_sum() {
    // Sum 1 to 5 using FOR loop variable
    // 0 1 5 FOR i i + NEXT -> 0+1+2+3+4+5 = 15
    assert_stack_eq("0 1 5 FOR i i + NEXT", &[15.0]);
}

#[test]
fn for_next_once() {
    // 5 5 FOR x x NEXT -> just pushes 5
    assert_stack_eq("5 5 FOR x x NEXT", &[5.0]);
}

#[test]
fn for_next_reversed() {
    // 5 4 FOR i i NEXT -> finish < start, HP behavior: body runs once
    // i=5, push 5, then 6 <= 4 fails, exit with [5]
    assert_stack_eq("5 4 FOR i i NEXT", &[5.0]);
}

#[test]
fn for_next_squares() {
    // Push squares of 1-4: 1, 4, 9, 16
    assert_stack_eq("1 4 FOR n n n * NEXT", &[1.0, 4.0, 9.0, 16.0]);
}

// ============================================================================
// START/STEP loops (with custom increment)
// ============================================================================

#[test]
fn start_step_by_two() {
    // 1 5 START 1 + 2 STEP -> counter: 1, 3, 5 (3 iterations), result: 0+1+1+1=3
    assert_stack_eq("0 1 5 START 1 + 2 STEP", &[3.0]);
}

#[test]
fn start_step_descending() {
    // 5 1 START 1 + -1 STEP -> counter: 5, 4, 3, 2, 1 (5 iterations), result: 0+5=5
    // Direction is descending (5 > 1), so STEP uses >= comparison
    assert_stack_eq("0 5 1 START 1 + -1 STEP", &[5.0]);
}

#[test]
fn start_step_single() {
    // 3 3 START 42 1 STEP -> counter: 3 (1 iteration), push 42
    assert_stack_eq("3 3 START 42 1 STEP", &[42.0]);
}

// ============================================================================
// FOR/STEP loops (with loop variable and custom increment)
// ============================================================================

#[test]
fn for_step_ascending_sum() {
    // 0 1 5 FOR i i + 1 STEP -> i: 1, 2, 3, 4, 5, sum = 15
    assert_stack_eq("0 1 5 FOR i i + 1 STEP", &[15.0]);
}

#[test]
fn for_step_descending_sum() {
    // 0 5 1 FOR i i + -1 STEP -> i: 5, 4, 3, 2, 1, sum = 15
    // Direction is descending (5 > 1), so STEP uses >= comparison
    assert_stack_eq("0 5 1 FOR i i + -1 STEP", &[15.0]);
}

#[test]
fn for_step_by_two() {
    // 0 1 5 FOR i i + 2 STEP -> i: 1, 3, 5, sum = 9
    assert_stack_eq("0 1 5 FOR i i + 2 STEP", &[9.0]);
}

#[test]
fn for_step_single_iteration() {
    // 3 3 FOR x x 1 STEP -> x: 3 (single iteration), push 3
    assert_stack_eq("3 3 FOR x x 1 STEP", &[3.0]);
}

// ============================================================================
// FORUP loops (ascending FOR with zero-iteration skip)
// ============================================================================

#[test]
fn forup_ascending() {
    // 0 1 5 FORUP i i + NEXT -> i: 1, 2, 3, 4, 5, sum = 15
    assert_stack_eq("0 1 5 FORUP i i + NEXT", &[15.0]);
}

#[test]
fn forup_zero_iterations() {
    // 0 5 1 FORUP i i + NEXT -> skip (5 > 1), result = 0
    assert_stack_eq("0 5 1 FORUP i i + NEXT", &[0.0]);
}

#[test]
fn forup_with_step() {
    // 0 1 5 FORUP i i + 2 STEP -> i: 1, 3, 5, sum = 9
    assert_stack_eq("0 1 5 FORUP i i + 2 STEP", &[9.0]);
}

#[test]
fn forup_equal_bounds() {
    // 0 3 3 FORUP i i + NEXT -> single iteration, i=3, sum = 3
    assert_stack_eq("0 3 3 FORUP i i + NEXT", &[3.0]);
}

// ============================================================================
// FORDN loops (descending FOR with zero-iteration skip)
// ============================================================================

#[test]
fn fordn_descending() {
    // 0 5 1 FORDN i i + -1 STEP -> i: 5, 4, 3, 2, 1, sum = 15
    assert_stack_eq("0 5 1 FORDN i i + -1 STEP", &[15.0]);
}

#[test]
fn fordn_zero_iterations() {
    // 0 1 5 FORDN i i + -1 STEP -> skip (1 < 5), result = 0
    assert_stack_eq("0 1 5 FORDN i i + -1 STEP", &[0.0]);
}

#[test]
fn fordn_with_next() {
    // 0 5 1 FORDN i i + NEXT -> runs once (NEXT always +1), i=5, then 6<=1 fails
    assert_stack_eq("0 5 1 FORDN i i + NEXT", &[5.0]);
}

#[test]
fn fordn_equal_bounds() {
    // 0 3 3 FORDN i i + -1 STEP -> single iteration, i=3, sum = 3
    assert_stack_eq("0 3 3 FORDN i i + -1 STEP", &[3.0]);
}

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

#[test]
fn compare_floats() {
    assert_stack_eq("3.14 2.71 >", &[1.0]);
    assert_stack_eq("2.71 3.14 >", &[0.0]);
}

// ============================================================================
// Lists
// ============================================================================

/// Helper to evaluate code and return all values (not just reals).
fn eval_to_values(code: &str) -> Vec<Value> {
    let mut session = Session::new();
    session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e))
}

/// Helper to extract list contents as f64 values.
fn list_to_reals(value: &Value) -> Vec<f64> {
    match value {
        Value::List(elements) => elements
            .iter()
            .map(|v| match v {
                Value::Real(r) => *r,
                Value::Int(i) => *i as f64,
                other => panic!("Expected Real or Int in list, got {:?}", other),
            })
            .collect(),
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn list_empty() {
    let values = eval_to_values("{ }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), Vec::<f64>::new());
}

#[test]
fn list_single_element() {
    let values = eval_to_values("{ 42 }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![42.0]);
}

#[test]
fn list_multiple_elements() {
    let values = eval_to_values("{ 1 2 3 }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_with_expressions() {
    // Elements can be computed
    let values = eval_to_values("{ 1 2 + 3 4 * }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![3.0, 12.0]); // 1+2=3, 3*4=12
}

#[test]
fn list_size() {
    assert_stack_eq("{ 1 2 3 } SIZE", &[3.0]);
}

#[test]
fn list_size_empty() {
    assert_stack_eq("{ } SIZE", &[0.0]);
}

#[test]
fn list_head() {
    assert_stack_eq("{ 1 2 3 } HEAD", &[1.0]);
}

#[test]
fn list_tail() {
    let values = eval_to_values("{ 1 2 3 } TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![2.0, 3.0]);
}

#[test]
fn list_tail_to_empty() {
    let values = eval_to_values("{ 1 } TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), Vec::<f64>::new());
}

#[test]
fn list_get() {
    // GET uses 1-based indexing
    assert_stack_eq("{ 10 20 30 } 2 GET", &[20.0]);
}

#[test]
fn list_get_first() {
    assert_stack_eq("{ 10 20 30 } 1 GET", &[10.0]);
}

#[test]
fn list_get_last() {
    assert_stack_eq("{ 10 20 30 } 3 GET", &[30.0]);
}

#[test]
fn list_put() {
    // PUT: list index value -> list'
    let values = eval_to_values("{ 1 2 3 } 2 99 PUT");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 99.0, 3.0]);
}

#[test]
fn list_revlist() {
    let values = eval_to_values("{ 1 2 3 } REVLIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![3.0, 2.0, 1.0]);
}

#[test]
fn list_to_explode() {
    // LIST→ explodes list onto stack and pushes count
    assert_stack_eq("{ 10 20 30 } LIST->", &[10.0, 20.0, 30.0, 3.0]);
}

#[test]
fn list_from_stack() {
    // →LIST: n elements count -> list
    let values = eval_to_values("10 20 30 3 ->LIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![10.0, 20.0, 30.0]);
}

#[test]
fn list_roundtrip() {
    // Explode and reconstruct
    let values = eval_to_values("{ 1 2 3 } LIST-> ->LIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_nested() {
    // Nested lists
    let values = eval_to_values("{ { 1 2 } { 3 4 } }");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(outer) => {
            assert_eq!(outer.len(), 2);
            assert_eq!(list_to_reals(&outer[0]), vec![1.0, 2.0]);
            assert_eq!(list_to_reals(&outer[1]), vec![3.0, 4.0]);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn list_head_of_nested() {
    // HEAD of nested list returns inner list
    let values = eval_to_values("{ { 1 2 } { 3 4 } } HEAD");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0]);
}

// ============================================================================
// Strings
// ============================================================================

/// Helper to extract string value.
fn to_string(value: &Value) -> &str {
    match value {
        Value::String(s) => s,
        other => panic!("Expected String, got {:?}", other),
    }
}

/// Helper to extract integer value.
fn to_int(value: &Value) -> i64 {
    match value {
        Value::Int(i) => *i,
        Value::Real(r) => *r as i64,
        other => panic!("Expected Int or Real, got {:?}", other),
    }
}

#[test]
fn string_empty() {
    let values = eval_to_values("\"\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_simple() {
    let values = eval_to_values("\"hello\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_with_spaces() {
    let values = eval_to_values("\"hello world\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello world");
}

#[test]
fn string_with_escape_newline() {
    let values = eval_to_values("\"hello\\nworld\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello\nworld");
}

#[test]
fn string_with_escape_quote() {
    let values = eval_to_values("\"say \\\"hi\\\"\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "say \"hi\"");
}

#[test]
fn string_size() {
    assert_stack_eq("\"hello\" SIZE", &[5.0]);
}

#[test]
fn string_size_empty() {
    assert_stack_eq("\"\" SIZE", &[0.0]);
}

#[test]
fn string_size_short() {
    assert_stack_eq("\"hi\" SIZE", &[2.0]);
}

#[test]
fn string_head() {
    let values = eval_to_values("\"hello\" HEAD");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "h");
}

#[test]
fn string_tail() {
    let values = eval_to_values("\"hello\" TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "ello");
}

#[test]
fn string_tail_to_empty() {
    let values = eval_to_values("\"x\" TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_sub() {
    // SUB: string start length -> substring (1-based)
    let values = eval_to_values("\"hello world\" 1 5 SUB");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_sub_middle() {
    let values = eval_to_values("\"hello world\" 7 5 SUB");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "world");
}

#[test]
fn string_pos_found() {
    // POS: string pattern -> position (1-based, 0 if not found)
    assert_stack_eq("\"hello world\" \"world\" POS", &[7.0]);
}

#[test]
fn string_pos_not_found() {
    assert_stack_eq("\"hello world\" \"foo\" POS", &[0.0]);
}

#[test]
fn string_pos_at_start() {
    assert_stack_eq("\"hello\" \"hel\" POS", &[1.0]);
}

#[test]
fn string_num() {
    // NUM: Convert string to number
    assert_stack_eq("\"42\" NUM", &[42.0]);
}

#[test]
fn string_num_float() {
    assert_stack_eq("\"3.15\" NUM", &[3.15]);
}

#[test]
fn string_str() {
    // STR: Convert number to string
    let values = eval_to_values("42 STR");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "42");
}

#[test]
fn string_chr() {
    // CHR: Character code to string
    let values = eval_to_values("65 CHR");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "A");
}

#[test]
fn string_asc() {
    // ASC: String to character code
    assert_stack_eq("\"A\" ASC", &[65.0]);
}

#[test]
fn string_chr_asc_roundtrip() {
    assert_stack_eq("65 CHR ASC", &[65.0]);
}

#[test]
fn string_multiple_on_stack() {
    let values = eval_to_values("\"hello\" \"world\"");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_string(&values[1]), "world");
}

#[test]
fn string_in_list() {
    let values = eval_to_values("{ \"a\" \"b\" \"c\" }");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
            assert_eq!(to_string(&elements[0]), "a");
            assert_eq!(to_string(&elements[1]), "b");
            assert_eq!(to_string(&elements[2]), "c");
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

// ============================================================================
// Variables (STO, RCL, PURGE)
// ============================================================================

#[test]
fn var_sto_rcl() {
    // STO: value "name" STO - stores value
    // RCL: "name" RCL - recalls value
    assert_stack_eq("42 \"x\" STO \"x\" RCL", &[42.0]);
}

#[test]
fn var_sto_rcl_real() {
    assert_stack_eq("3.15 \"pi\" STO \"pi\" RCL", &[3.15]);
}

#[test]
fn var_sto_multiple() {
    // Store two different variables
    assert_stack_eq(
        "10 \"a\" STO 20 \"b\" STO \"a\" RCL \"b\" RCL",
        &[10.0, 20.0],
    );
}

#[test]
fn var_sto_overwrite() {
    // Overwriting a variable should work
    assert_stack_eq("10 \"x\" STO 20 \"x\" STO \"x\" RCL", &[20.0]);
}

#[test]
fn var_sto_string() {
    // Store a string value
    let values = eval_to_values("\"hello\" \"greeting\" STO \"greeting\" RCL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn var_sto_list() {
    // Store a list value
    let values = eval_to_values("{ 1 2 3 } \"mylist\" STO \"mylist\" RCL");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn var_sto_preserves_stack() {
    // STO should consume both the value and name, leaving stack empty
    let values = eval_to_values("42 \"x\" STO");
    assert_eq!(values.len(), 0);
}

#[test]
fn var_sto_in_program() {
    // Use STO inside a program
    assert_stack_eq(":: 100 \"result\" STO ; EVAL \"result\" RCL", &[100.0]);
}

#[test]
fn var_use_in_calculation() {
    // Store value, then use it in calculation
    assert_stack_eq("5 \"n\" STO \"n\" RCL \"n\" RCL *", &[25.0]);
}

#[test]
fn var_purge() {
    // PURGE removes a variable - we test by storing, purging, then storing again
    // (can't easily test the error case in this test framework)
    assert_stack_eq("42 \"x\" STO \"x\" PURGE 99 \"x\" STO \"x\" RCL", &[99.0]);
}

// ============================================================================
// Directory Commands: INCR, DECR
// ============================================================================

#[test]
fn var_incr() {
    // INCR: Increment variable and return new value
    assert_stack_eq("5 \"x\" STO \"x\" INCR", &[6.0]);
}

#[test]
fn var_incr_updates_var() {
    // INCR should also update the variable
    assert_stack_eq("5 \"x\" STO \"x\" INCR DROP \"x\" RCL", &[6.0]);
}

#[test]
fn var_decr() {
    // DECR: Decrement variable and return new value
    assert_stack_eq("5 \"x\" STO \"x\" DECR", &[4.0]);
}

#[test]
fn var_decr_updates_var() {
    // DECR should also update the variable
    assert_stack_eq("5 \"x\" STO \"x\" DECR DROP \"x\" RCL", &[4.0]);
}

#[test]
fn var_incr_loop() {
    // Use INCR in a counting loop
    assert_stack_eq(
        "0 \"cnt\" STO 1 5 START \"cnt\" INCR DROP NEXT \"cnt\" RCL",
        &[5.0],
    );
}

// ============================================================================
// Directory Commands: VARS, CLVAR
// ============================================================================

#[test]
fn var_vars_returns_list() {
    // VARS: Returns list of variable names
    let values = eval_to_values("CLVAR 42 \"testvar\" STO VARS");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert!(!elements.is_empty());
            // Check that our variable is in the list
            let has_testvar = elements
                .iter()
                .any(|e| matches!(e, Value::String(s) if s == "testvar"));
            assert!(has_testvar, "VARS should contain 'testvar'");
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn var_clvar() {
    // CLVAR: Clears all variables
    // Store two vars, clear, try to RCL (should error)
    let mut session = Session::new();
    session.eval("10 \"a\" STO 20 \"b\" STO").unwrap();
    session.eval("CLVAR").unwrap();
    // After CLVAR, RCL should fail
    assert!(
        session.eval("\"a\" RCL").is_err(),
        "a should be undefined after CLVAR"
    );
    assert!(
        session.eval("\"b\" RCL").is_err(),
        "b should be undefined after CLVAR"
    );
}

// ============================================================================
// Directory Commands: RENAME
// ============================================================================

#[test]
fn var_rename() {
    // RENAME: Rename a variable
    assert_stack_eq("42 \"old\" STO \"old\" \"new\" RENAME \"new\" RCL", &[42.0]);
}

#[test]
fn var_rename_old_gone() {
    // After RENAME, old name should not exist
    let mut session = Session::new();
    session
        .eval("42 \"old\" STO \"old\" \"new\" RENAME")
        .unwrap();
    assert!(
        session.eval("\"old\" RCL").is_err(),
        "old name should not exist after RENAME"
    );
    let result = session.eval("\"new\" RCL").unwrap();
    assert_eq!(result.len(), 1);
}

// ============================================================================
// Directory Hierarchy: CRDIR, PGDIR, UPDIR, HOME, PATH
// ============================================================================

#[test]
fn dir_crdir_basic() {
    // CRDIR creates a subdirectory
    let mut session = Session::new();
    session.eval("\"subdir\" CRDIR").unwrap();
    // No error means success
}

#[test]
fn dir_crdir_duplicate_fails() {
    // Creating a directory that already exists should fail
    let mut session = Session::new();
    session.eval("\"subdir\" CRDIR").unwrap();
    assert!(
        session.eval("\"subdir\" CRDIR").is_err(),
        "duplicate CRDIR should fail"
    );
}

#[test]
fn dir_path_at_home() {
    // PATH at root should return empty list
    let mut session = Session::new();
    let result = session.eval("PATH").unwrap();
    assert_eq!(result.len(), 1, "PATH should push one value");
    // The value should be an empty list
    match &result[0] {
        Value::List(items) => assert!(items.is_empty(), "PATH at root should be empty list"),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_home_noop_at_root() {
    // HOME at root is a no-op, PATH should still be empty
    let mut session = Session::new();
    let result = session.eval("HOME PATH").unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::List(items) => assert!(items.is_empty()),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_updir_at_root_noop() {
    // UPDIR at root is a no-op (not an error), PATH should still be empty
    let mut session = Session::new();
    let result = session.eval("UPDIR PATH").unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::List(items) => assert!(items.is_empty()),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_pgdir_basic() {
    // PGDIR removes an empty directory
    let mut session = Session::new();
    session.eval("\"subdir\" CRDIR").unwrap();
    session.eval("\"subdir\" PGDIR").unwrap();
    // Recreating should work now
    session.eval("\"subdir\" CRDIR").unwrap();
}

#[test]
fn dir_pgdir_nonexistent_fails() {
    // PGDIR on nonexistent directory should fail
    let mut session = Session::new();
    assert!(
        session.eval("\"nonexistent\" PGDIR").is_err(),
        "PGDIR on nonexistent should fail"
    );
}

#[test]
fn dir_vars_isolation() {
    // Variables in different directories are isolated
    let mut session = Session::new();

    // Store in root
    session.eval("42 \"x\" STO").unwrap();

    // Create and enter subdirectory
    session.eval("\"sub\" CRDIR").unwrap();

    // Root variable should still be accessible (we haven't entered sub yet)
    let result = session.eval("\"x\" RCL").unwrap();
    assert_eq!(result.len(), 1);
}

// ============================================================================
// Flow Control with Comparison
// ============================================================================

#[test]
fn while_count_to_five() {
    // Count up from 0 until reaching 5
    assert_stack_eq("0 WHILE DUP 5 < REPEAT 1 + END", &[5.0]);
}

// ============================================================================
// Local Variable Bindings
// ============================================================================

#[test]
fn local_binding_simple() {
    assert_stack_eq("5 → x :: x ;", &[5.0]);
}

#[test]
fn local_binding_use_twice() {
    // x + x = 10
    assert_stack_eq("5 → x :: x x + ;", &[10.0]);
}

#[test]
fn local_binding_square() {
    // 4² = 16
    assert_stack_eq("4 → x :: x x * ;", &[16.0]);
}

#[test]
fn local_binding_multiple_params() {
    // a=3, b=4, a+b=7
    assert_stack_eq("3 4 → a b :: a b + ;", &[7.0]);
}

#[test]
fn local_binding_arrow_syntax() {
    // ASCII arrow -> works too
    assert_stack_eq("5 -> x :: x ;", &[5.0]);
}

#[test]
fn local_binding_with_computation() {
    // 10/2 + 10*3 = 5 + 30 = 35
    assert_stack_eq("10 → x :: x 2 / x 3 * + ;", &[35.0]);
}

// Programs with local bindings work when stored and recalled.
// The interner is stored in the VM and available during nested EVAL calls.
#[test]
fn local_binding_in_stored_program() {
    assert_stack_eq(
        r#":: -> n :: n 1 + ; ; "add1" STO 5 "add1" RCL EVAL"#,
        &[6.0],
    );
}

// Jump targets in stored programs are compiled as relative offsets to the
// program body start, so they work correctly when the program is extracted
// and executed via EVAL (where PC starts at 0 within the body).
#[test]
fn if_else_in_stored_program() {
    // IF with true condition - should return 100
    assert_stack_eq(":: IF 1 THEN 100 ELSE 200 END ; EVAL", &[100.0]);
    // IF with false condition - should return 200
    assert_stack_eq(":: IF 0 THEN 100 ELSE 200 END ; EVAL", &[200.0]);
}

// =============================================================================
// Comments
// =============================================================================

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
    assert_stack_eq(":: 5 @ comment\n3 + ; EVAL", &[8.0]);
}

#[test]
fn multiple_comments() {
    // Multiple comments on different lines
    assert_stack_eq("5 @ first\n3 @ second\n+", &[8.0]);
}

// =============================================================================
// Fixture Files
// =============================================================================

#[test]
fn fixture_factorial() {
    let code = include_str!("programs/factorial.rpl");
    assert_stack_eq(code, &[120.0]);
}

#[test]
fn fixture_fibonacci() {
    let code = include_str!("programs/fibonacci.rpl");
    assert_stack_eq(code, &[55.0]);
}

#[test]
fn fixture_gcd() {
    let code = include_str!("programs/gcd.rpl");
    assert_stack_eq(code, &[6.0]);
}

#[test]
#[allow(clippy::approx_constant)]
fn fixture_newton_sqrt() {
    let code = include_str!("programs/newton_sqrt.rpl");
    let values = eval_to_reals(code);
    assert_eq!(values.len(), 1);
    // sqrt(2) ≈ 1.41421356...
    assert!((values[0] - 1.41421356).abs() < 0.0001);
}

#[test]
fn fixture_prime_sieve() {
    let code = include_str!("programs/prime_sieve.rpl");
    // 17 is prime (1), 18 is not (0)
    assert_stack_eq(code, &[1.0, 0.0]);
}

#[test]
fn list_sum_function() {
    // Test sum from list_ops.rpl
    let code = "
        :: -> lst ::
            0
            lst LIST->
            1 SWAP START
                +
            NEXT
        ; ;
        \"sum\" STO
        { 1 2 3 4 5 } sum
    ";
    assert_stack_eq(code, &[15.0]);
}

#[test]
fn fixture_list_ops() {
    let code = include_str!("programs/list_ops.rpl");
    assert_stack_eq(code, &[15.0]);
}

#[test]
fn fixture_symbolic() {
    let code = include_str!("programs/symbolic_derivative.rpl");
    let values = eval_to_values(code);
    // Should have 3 symbolic objects on the stack
    assert_eq!(values.len(), 3);
}

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
    // 5 → x :: 'x + 3' EVAL ; → 8
    assert_stack_eq("5 → x :: 'x + 3' EVAL ;", &[8.0]);
}

#[test]
fn eval_symbolic_complex() {
    // '(1 + 2) * (3 + 4)' EVAL → 21
    assert_stack_eq("'(1 + 2) * (3 + 4)' EVAL", &[21.0]);
}

// ============================================================================
// Transcendental Functions
// ============================================================================

// Basic trigonometric
#[test]
fn transcendental_sin() {
    assert_stack_approx("0 SIN", &[0.0], 1e-10);
}

#[test]
fn transcendental_cos() {
    assert_stack_approx("0 COS", &[1.0], 1e-10);
}

#[test]
fn transcendental_sin_pi_over_2() {
    assert_stack_approx("PI 2 / SIN", &[1.0], 1e-10);
}

#[test]
fn transcendental_cos_pi() {
    assert_stack_approx("PI COS", &[-1.0], 1e-10);
}

#[test]
fn transcendental_tan_zero() {
    assert_stack_approx("0 TAN", &[0.0], 1e-10);
}

// Inverse trigonometric
#[test]
fn transcendental_asin() {
    assert_stack_approx("1 ASIN PI 2 / -", &[0.0], 1e-10);
}

#[test]
fn transcendental_acos() {
    assert_stack_approx("0 ACOS PI 2 / -", &[0.0], 1e-10);
}

#[test]
fn transcendental_atan() {
    assert_stack_approx("1 ATAN PI 4 / -", &[0.0], 1e-10);
}

#[test]
fn transcendental_atan2() {
    assert_stack_approx("1 1 ATAN2 PI 4 / -", &[0.0], 1e-10);
}

// Logarithmic
#[test]
fn transcendental_ln_e() {
    assert_stack_approx("1 EXP LN", &[1.0], 1e-10);
}

#[test]
fn transcendental_log_10() {
    assert_stack_approx("10 LOG", &[1.0], 1e-10);
}

#[test]
fn transcendental_alog() {
    assert_stack_approx("2 ALOG", &[100.0], 1e-10);
}

#[test]
fn transcendental_exp_ln_roundtrip() {
    assert_stack_approx("5 LN EXP", &[5.0], 1e-10);
}

// Hyperbolic
#[test]
fn transcendental_sinh_cosh_identity() {
    // cosh²(x) - sinh²(x) = 1
    assert_stack_approx("2 COSH 2 ^ 2 SINH 2 ^ -", &[1.0], 1e-10);
}

#[test]
fn transcendental_tanh() {
    assert_stack_approx("0 TANH", &[0.0], 1e-10);
}

// Inverse hyperbolic
#[test]
fn transcendental_asinh_roundtrip() {
    assert_stack_approx("2 SINH ASINH", &[2.0], 1e-10);
}

#[test]
fn transcendental_acosh_roundtrip() {
    assert_stack_approx("2 COSH ACOSH", &[2.0], 1e-10);
}

#[test]
fn transcendental_atanh_roundtrip() {
    assert_stack_approx("0.5 TANH ATANH", &[0.5], 1e-10);
}

// Square root
#[test]
fn transcendental_sqrt() {
    assert_stack_approx("16 SQRT", &[4.0], 1e-10);
}

#[test]
fn transcendental_sqrt_2() {
    assert_stack_approx("2 SQRT 2 ^", &[2.0], 1e-10);
}

// Precision functions
#[test]
fn transcendental_expm_small() {
    // For small x, expm(x) ≈ x
    assert_stack_approx("0.0000001 EXPM", &[1.0000000500000001e-7], 1e-15);
}

#[test]
fn transcendental_lnp1_small() {
    // For small x, lnp1(x) ≈ x
    // Using a looser tolerance since exact float values vary
    assert_stack_approx("0.0000001 LNP1", &[0.0000001_f64.ln_1p()], 1e-15);
}

// Pi constant
#[test]
fn transcendental_pi() {
    assert_stack_approx("PI", &[std::f64::consts::PI], 1e-15);
}

#[test]
fn transcendental_pi_unicode() {
    assert_stack_approx("π", &[std::f64::consts::PI], 1e-15);
}

#[test]
fn transcendental_circle_circumference() {
    // C = 2πr, for r=1
    assert_stack_approx("PI 2 *", &[std::f64::consts::TAU], 1e-10);
}

// Pythagorean identity: sin²(x) + cos²(x) = 1
#[test]
fn transcendental_pythagorean_identity() {
    assert_stack_approx("1 SIN 2 ^ 1 COS 2 ^ +", &[1.0], 1e-10);
}

// Error cases
#[test]
fn transcendental_asin_domain_error() {
    assert_error("2 ASIN", "Domain error");
}

#[test]
fn transcendental_acos_domain_error() {
    assert_error("-2 ACOS", "Domain error");
}

#[test]
fn transcendental_ln_domain_error() {
    assert_error("0 LN", "Domain error");
}

#[test]
fn transcendental_ln_negative_error() {
    assert_error("-1 LN", "Domain error");
}

#[test]
fn transcendental_sqrt_negative_error() {
    assert_error("-1 SQRT", "Domain error");
}

#[test]
fn transcendental_acosh_domain_error() {
    assert_error("0.5 ACOSH", "Domain error");
}

#[test]
fn transcendental_atanh_domain_error() {
    assert_error("1 ATANH", "Domain error");
}

#[test]
fn transcendental_atan2_zero_zero_error() {
    assert_error("0 0 ATAN2", "undefined");
}

// Case insensitivity
#[test]
fn transcendental_case_insensitive() {
    assert_stack_approx("0 sin", &[0.0], 1e-10);
    assert_stack_approx("0 Sin", &[0.0], 1e-10);
    assert_stack_approx("pi", &[std::f64::consts::PI], 1e-15);
}

// ============================================================================
// Library System (CRLIB, ATTACH, LIBPTR)
// ============================================================================

#[test]
fn library_crlib_creates_library() {
    // Create a library with a single named command that pushes 42
    // { { "ANSWER" << 42 >> } } "TEST" CRLIB
    let mut session = Session::new();

    // First, just verify CRLIB works and produces something
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "TEST" CRLIB"#);
    assert!(result.is_ok(), "CRLIB should succeed");

    // Stack should have 1 item (the library object)
    let values = result.unwrap();
    assert_eq!(values.len(), 1, "CRLIB should produce one value");

    // The value should be a library object
    match &values[0] {
        Value::Object { type_id, data } => {
            assert_eq!(type_id.as_u16(), 102, "Should be LIBRARY type (102)");
            // data[0] should be the lib ID for "TEST"
            assert!(data.len() >= 2, "Library should have header");
        }
        _ => panic!("Expected library object"),
    }
}

#[test]
fn library_attach_stores_library() {
    let mut session = Session::new();

    // Create and attach a library
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "MATH" CRLIB ATTACH"#);
    assert!(result.is_ok(), "CRLIB + ATTACH should succeed");

    // Stack should be empty after ATTACH
    let values = result.unwrap();
    assert_eq!(values.len(), 0, "ATTACH should consume the library");

    // The library should be stored in .SETTINGS.LIB.MATH
    // We can verify by checking it was stored (no direct way to verify in pipeline test)
}

#[test]
fn library_detach_removes_library() {
    let mut session = Session::new();

    // Create, attach, then detach
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "TEST" CRLIB ATTACH "TEST" DETACH"#);
    assert!(result.is_ok(), "CRLIB + ATTACH + DETACH should succeed");
}

// ============================================================================
// Library Private Data (LIBSTO, LIBRCL, LIBDEFRCL, LIBCLEAR)
// ============================================================================

#[test]
fn library_libsto_librcl_roundtrip() {
    let mut session = Session::new();

    // Store a value in library private namespace
    let result = session.eval(r#"42 "TEST" "myvar" LIBSTO"#);
    assert!(result.is_ok(), "LIBSTO should succeed");

    // Recall the value
    let result = session.eval(r#""TEST" "myvar" LIBRCL"#);
    assert!(result.is_ok(), "LIBRCL should succeed");

    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Int(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_libdefrcl_returns_stored_value() {
    let mut session = Session::new();

    // Store a value
    session.eval(r#"100 "MATH" "setting" LIBSTO"#).unwrap();

    // Recall with default - should return stored value
    let result = session.eval(r#"0 "MATH" "setting" LIBDEFRCL"#).unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Int(i) => assert_eq!(*i, 100),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_libdefrcl_returns_default() {
    let mut session = Session::new();

    // Recall non-existent variable with default
    let result = session
        .eval(r#"999 "TEST" "nonexistent" LIBDEFRCL"#)
        .unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 999.0),
        Value::Int(i) => assert_eq!(*i, 999),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_librcl_error_on_missing() {
    let mut session = Session::new();

    // Try to recall non-existent variable
    let result = session.eval(r#""TEST" "missing" LIBRCL"#);
    assert!(result.is_err(), "LIBRCL should fail for missing variable");
}

#[test]
fn library_libclear_succeeds() {
    let mut session = Session::new();

    // LIBCLEAR should succeed (even though our implementation is a no-op)
    let result = session.eval(r#""TEST" LIBCLEAR"#);
    assert!(result.is_ok(), "LIBCLEAR should succeed");
}

#[test]
fn library_libclear_actually_clears_data() {
    let mut session = Session::new();

    // Store some values
    session.eval(r#"1 "TEST" "var1" LIBSTO"#).unwrap();
    session.eval(r#"2 "TEST" "var2" LIBSTO"#).unwrap();
    session.eval(r#"3 "TEST" "var3" LIBSTO"#).unwrap();

    // Verify they're stored
    let result = session.eval(r#""TEST" "var1" LIBRCL"#);
    assert!(result.is_ok(), "var1 should exist before LIBCLEAR");

    // Clear the library data
    session.eval(r#""TEST" LIBCLEAR"#).unwrap();

    // Verify they're gone - LIBRCL should fail
    let result = session.eval(r#""TEST" "var1" LIBRCL"#);
    assert!(result.is_err(), "var1 should be gone after LIBCLEAR");

    let result = session.eval(r#""TEST" "var2" LIBRCL"#);
    assert!(result.is_err(), "var2 should be gone after LIBCLEAR");
}

// ============================================================================
// User Library System - Full Integration Test
// ============================================================================

/// Comprehensive test demonstrating the full user library workflow:
/// 1. Create library commands as programs
/// 2. Package them into a library with CRLIB
/// 3. Attach the library
/// 4. Use library private data for state
/// 5. Clean up with DETACH and LIBCLEAR
#[test]
fn library_full_integration() {
    let mut session = Session::new();

    // Step 1: Create a library with two named commands and attach it in one go
    // - DOUBLE: doubles a number (2 *)
    // - SQUARE: squares a number (DUP *)
    let result = session.eval(
        r#"
        { { "DOUBLE" << 2 * >> } { "SQUARE" << DUP * >> } } "MATH" CRLIB ATTACH
    "#,
    );
    assert!(
        result.is_ok(),
        "CRLIB + ATTACH should succeed: {:?}",
        result
    );
    assert_eq!(
        result.unwrap().len(),
        0,
        "Stack should be empty after ATTACH"
    );

    // Step 2: Use library private data
    // Store a "precision" setting for our MATH library
    let result = session.eval(r#"10 "MATH" "precision" LIBSTO"#);
    assert!(result.is_ok(), "LIBSTO should succeed");

    // Recall the setting
    let result = session.eval(r#""MATH" "precision" LIBRCL"#);
    assert!(result.is_ok(), "LIBRCL should succeed");
    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 10.0),
        Value::Int(i) => assert_eq!(*i, 10),
        _ => panic!("Expected number"),
    }

    // Use LIBDEFRCL for a setting that doesn't exist - should return default
    let result = session.eval(r#"100 "MATH" "missing_setting" LIBDEFRCL"#);
    assert!(result.is_ok(), "LIBDEFRCL should succeed");
    let values = result.unwrap();
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Int(i) => assert_eq!(*i, 100),
        _ => panic!("Expected default value"),
    }

    // Step 3: Store multiple settings, then clear them all
    session.eval(r#"1 "MATH" "setting1" LIBSTO"#).unwrap();
    session.eval(r#"2 "MATH" "setting2" LIBSTO"#).unwrap();
    session.eval(r#"3 "MATH" "setting3" LIBSTO"#).unwrap();

    // Verify one exists
    assert!(session.eval(r#""MATH" "setting2" LIBRCL"#).is_ok());

    // Clear all MATH library data
    session.eval(r#""MATH" LIBCLEAR"#).unwrap();

    // Verify all settings are gone
    assert!(
        session.eval(r#""MATH" "setting1" LIBRCL"#).is_err(),
        "setting1 should be cleared"
    );
    assert!(
        session.eval(r#""MATH" "setting2" LIBRCL"#).is_err(),
        "setting2 should be cleared"
    );
    assert!(
        session.eval(r#""MATH" "setting3" LIBRCL"#).is_err(),
        "setting3 should be cleared"
    );
    // Note: "precision" was also cleared since it's in the same library namespace

    // Step 4: Detach the library
    let result = session.eval(r#""MATH" DETACH"#);
    assert!(result.is_ok(), "DETACH should succeed");
}

/// Test that library data is isolated between different libraries
#[test]
fn library_data_isolation() {
    let mut session = Session::new();

    // Store data in two different library namespaces
    session.eval(r#"100 "LIB1" "value" LIBSTO"#).unwrap();
    session.eval(r#"200 "LIB2" "value" LIBSTO"#).unwrap();

    // Each library should see its own value
    let result = session.eval(r#""LIB1" "value" LIBRCL"#).unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Int(i) => assert_eq!(*i, 100),
        _ => panic!("Expected 100"),
    }

    let result = session.eval(r#""LIB2" "value" LIBRCL"#).unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 200.0),
        Value::Int(i) => assert_eq!(*i, 200),
        _ => panic!("Expected 200"),
    }

    // Clearing LIB1 should not affect LIB2
    session.eval(r#""LIB1" LIBCLEAR"#).unwrap();

    assert!(
        session.eval(r#""LIB1" "value" LIBRCL"#).is_err(),
        "LIB1 data should be cleared"
    );
    assert!(
        session.eval(r#""LIB2" "value" LIBRCL"#).is_ok(),
        "LIB2 data should still exist"
    );
}

// ============================================================================
// Flow Control: CASE/THENCASE/ENDTHEN/ENDCASE
// HP RPL syntax: CASE test1 THENCASE action1 ENDTHEN test2 THENCASE action2 ENDTHEN default ENDCASE
// ============================================================================

#[test]
fn case_first_match() {
    // First test is true, execute its action, skip rest
    assert_stack_eq(
        "CASE 1 THENCASE 42 ENDTHEN 0 THENCASE 99 ENDTHEN 0 ENDCASE",
        &[42.0],
    );
}

#[test]
fn case_second_match() {
    // First test is false, second is true
    assert_stack_eq(
        "CASE 0 THENCASE 42 ENDTHEN 1 THENCASE 99 ENDTHEN 0 ENDCASE",
        &[99.0],
    );
}

#[test]
fn case_default() {
    // All tests false, execute default
    assert_stack_eq(
        "CASE 0 THENCASE 42 ENDTHEN 0 THENCASE 99 ENDTHEN 77 ENDCASE",
        &[77.0],
    );
}

#[test]
fn case_no_default() {
    // All tests false, no default action (nothing pushed)
    assert_stack_eq(
        "CASE 0 THENCASE 42 ENDTHEN 0 THENCASE 99 ENDTHEN ENDCASE",
        &[],
    );
}

#[test]
fn case_with_expressions() {
    // Tests can be expressions
    assert_stack_eq(
        "CASE 3 4 > THENCASE 1 ENDTHEN 3 4 < THENCASE 2 ENDTHEN 0 ENDCASE",
        &[2.0],
    );
}

#[test]
fn case_single_branch() {
    // Single THENCASE branch
    assert_stack_eq("CASE 1 THENCASE 42 ENDTHEN ENDCASE", &[42.0]);
    assert_stack_eq("CASE 0 THENCASE 42 ENDTHEN ENDCASE", &[]);
}

#[test]
fn case_nested() {
    // Nested CASE inside action
    assert_stack_eq(
        "CASE 1 THENCASE CASE 1 THENCASE 42 ENDTHEN ENDCASE ENDTHEN ENDCASE",
        &[42.0],
    );
}

// ============================================================================
// Flow Control: IFERR/THENERR/ELSEERR/ENDERR (Error Handling)
// Note: These tests verify the structure compiles; actual error trapping
// requires runtime error generation which may need specific error-producing code.
// ============================================================================

#[test]
fn iferr_no_error() {
    // Protected code succeeds, handler not executed
    assert_stack_eq("IFERR 42 THENERR 99 ENDERR", &[42.0]);
}

#[test]
fn iferr_no_error_with_elseerr() {
    // Protected code succeeds, ELSEERR code executed
    assert_stack_eq("IFERR 42 THENERR 99 ELSEERR 77 ENDERR", &[42.0, 77.0]);
}

#[test]
fn iferr_nested() {
    // Nested IFERR
    assert_stack_eq(
        "IFERR IFERR 10 THENERR 20 ENDERR THENERR 99 ENDERR",
        &[10.0],
    );
}

// ============================================================================
// String Functions: →UTF8/UTF8→ (byte conversion)
// ============================================================================

#[test]
fn string_toutf8_ascii() {
    // →UTF8: Convert string to list of UTF-8 bytes
    let values = eval_to_values("\"ABC\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
            assert_eq!(to_int(&elements[0]), 65); // 'A'
            assert_eq!(to_int(&elements[1]), 66); // 'B'
            assert_eq!(to_int(&elements[2]), 67); // 'C'
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_toutf8_unicode() {
    // UTF-8 encoding of "é" is [195, 169]
    let values = eval_to_values("\"é\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 2);
            assert_eq!(to_int(&elements[0]), 195);
            assert_eq!(to_int(&elements[1]), 169);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_toutf8_empty() {
    let values = eval_to_values("\"\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 0);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_fromutf8_ascii() {
    // UTF8→: Convert list of UTF-8 bytes to string
    let values = eval_to_values("{ 72 105 } UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "Hi");
}

#[test]
fn string_fromutf8_unicode() {
    // UTF-8 bytes for "é"
    let values = eval_to_values("{ 195 169 } UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "é");
}

#[test]
fn string_utf8_roundtrip() {
    let values = eval_to_values("\"hello\" →UTF8 UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

// ============================================================================
// String Functions: SREV (reverse)
// ============================================================================

#[test]
fn string_srev_simple() {
    let values = eval_to_values("\"hello\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "olleh");
}

#[test]
fn string_srev_empty() {
    let values = eval_to_values("\"\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_srev_single_char() {
    let values = eval_to_values("\"x\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "x");
}

#[test]
fn string_srev_unicode() {
    // Reverse preserves Unicode characters
    let values = eval_to_values("\"café\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "éfac");
}

// ============================================================================
// String Functions: NTOKENS, NTHTOKEN, NTHTOKENPOS (tokenization)
// ============================================================================

#[test]
fn string_ntokens_simple() {
    assert_stack_eq("\"hello world\" NTOKENS", &[2.0]);
}

#[test]
fn string_ntokens_multiple_spaces() {
    assert_stack_eq("\"a   b   c\" NTOKENS", &[3.0]);
}

#[test]
fn string_ntokens_empty() {
    assert_stack_eq("\"\" NTOKENS", &[0.0]);
}

#[test]
fn string_ntokens_whitespace_only() {
    assert_stack_eq("\"   \" NTOKENS", &[0.0]);
}

#[test]
fn string_ntokens_single_word() {
    assert_stack_eq("\"hello\" NTOKENS", &[1.0]);
}

#[test]
fn string_nthtoken_first() {
    let values = eval_to_values("\"hello world\" 1 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_nthtoken_second() {
    let values = eval_to_values("\"hello world\" 2 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "world");
}

#[test]
fn string_nthtoken_with_extra_spaces() {
    let values = eval_to_values("\"  a   b   c  \" 2 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "b");
}

#[test]
fn string_nthtoken_out_of_range() {
    // Returns error for out-of-range index
    assert_error("\"hello world\" 5 NTHTOKEN", "not found");
}

#[test]
fn string_nthtokenpos_first() {
    // Returns start and end positions (1-based)
    assert_stack_eq("\"hello world\" 1 NTHTOKENPOS", &[1.0, 5.0]);
}

#[test]
fn string_nthtokenpos_second() {
    assert_stack_eq("\"hello world\" 2 NTHTOKENPOS", &[7.0, 11.0]);
}

#[test]
fn string_nthtokenpos_with_leading_space() {
    assert_stack_eq("\"  hello\" 1 NTHTOKENPOS", &[3.0, 7.0]);
}

#[test]
fn string_nthtokenpos_out_of_range() {
    // Returns error for out-of-range index
    assert_error("\"hello\" 5 NTHTOKENPOS", "not found");
}

// ============================================================================
// String Functions: TRIM, RTRIM
// ============================================================================

#[test]
fn string_trim_both_sides() {
    let values = eval_to_values("\"  hello  \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_leading_only() {
    let values = eval_to_values("\"   hello\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_trailing_only() {
    let values = eval_to_values("\"hello   \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_no_whitespace() {
    let values = eval_to_values("\"hello\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_all_whitespace() {
    let values = eval_to_values("\"   \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_trim_tabs_and_newlines() {
    let values = eval_to_values("\"\\t\\nhello\\n\\t\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_rtrim_trailing() {
    let values = eval_to_values("\"hello   \" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_rtrim_preserves_leading() {
    let values = eval_to_values("\"   hello   \" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "   hello");
}

#[test]
fn string_rtrim_no_trailing() {
    let values = eval_to_values("\"   hello\" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "   hello");
}

// ============================================================================
// String Functions: STRLENCP (code point length)
// ============================================================================

#[test]
fn string_strlencp_ascii() {
    assert_stack_eq("\"hello\" STRLENCP", &[5.0]);
}

#[test]
fn string_strlencp_unicode() {
    // "café" has 4 code points
    assert_stack_eq("\"café\" STRLENCP", &[4.0]);
}

#[test]
fn string_strlencp_empty() {
    assert_stack_eq("\"\" STRLENCP", &[0.0]);
}

#[test]
fn string_strlencp_emoji() {
    // Single emoji is one code point
    assert_stack_eq("\"😀\" STRLENCP", &[1.0]);
}

// ============================================================================
// String Functions: SREPL (search and replace)
// ============================================================================

#[test]
fn string_srepl_simple() {
    // SREPL: string search replace -> result count
    let values = eval_to_values("\"hello world\" \"world\" \"universe\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello universe");
    assert_eq!(to_int(&values[1]), 1);
}

#[test]
fn string_srepl_multiple() {
    let values = eval_to_values("\"aaa\" \"a\" \"b\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "bbb");
    assert_eq!(to_int(&values[1]), 3);
}

#[test]
fn string_srepl_not_found() {
    let values = eval_to_values("\"hello\" \"xyz\" \"abc\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_int(&values[1]), 0);
}

#[test]
fn string_srepl_empty_search() {
    // Empty search string: no replacement, 0 count
    let values = eval_to_values("\"hello\" \"\" \"x\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_int(&values[1]), 0);
}

#[test]
fn string_srepl_to_empty() {
    // Replace with empty string (deletion)
    let values = eval_to_values("\"hello world\" \" \" \"\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "helloworld");
    assert_eq!(to_int(&values[1]), 1);
}

#[test]
fn string_srepl_overlapping() {
    // Non-overlapping replacement
    let values = eval_to_values("\"aaa\" \"aa\" \"b\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "ba");
    assert_eq!(to_int(&values[1]), 1);
}

// ============================================================================
// Complex Numbers
// ============================================================================

#[test]
fn complex_hp_style_literal() {
    // (3, 2) should create a complex number
    let values = eval_to_values("(3, 2)");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Object { type_id, data } => {
            assert_eq!(*type_id, rpl_core::TypeId::COMPLEX);
            // 4 words: re_hi, re_lo, im_hi, im_lo
            assert_eq!(data.len(), 4);
        }
        _ => panic!("Expected complex Object, got {:?}", values[0]),
    }
}

// ============================================================================
// PACKDIR (Directory Serialization)
// ============================================================================

#[test]
fn packdir_creates_object() {
    // PACKDIR should create a PackDir object
    let mut session = Session::new();
    session.eval("42 \"x\" STO").unwrap();
    let values = session.eval("PACKDIR").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Object { type_id, .. } => {
            assert_eq!(*type_id, rpl_core::TypeId::PACKDIR);
        }
        _ => panic!("Expected PackDir object, got {:?}", values[0]),
    }
}

#[test]
fn packdir_roundtrip_single_var() {
    // Pack directory, clear, unpack, verify value restored
    // All in one eval to preserve stack between operations
    let mut session = Session::new();
    session.eval("42 \"x\" STO PACKDIR CLVAR UNPACKDIR").unwrap();
    let values = session.eval("\"x\" RCL").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Int(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }
}

#[test]
fn packdir_roundtrip_multiple_vars() {
    // Pack multiple variables of different types
    let mut session = Session::new();
    session.eval("42 \"num\" STO \"hello\" \"msg\" STO { 1 2 3 } \"lst\" STO").unwrap();
    session.eval("PACKDIR CLVAR UNPACKDIR").unwrap();

    // Verify all values restored
    let values = session.eval("\"num\" RCL").unwrap();
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Int(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }

    let values = session.eval("\"msg\" RCL").unwrap();
    assert_eq!(to_string(&values[0]), "hello");

    let values = session.eval("\"lst\" RCL").unwrap();
    match &values[0] {
        Value::List(elements) => assert_eq!(elements.len(), 3),
        _ => panic!("Expected list"),
    }
}

#[test]
fn packdir_unpack_into_named_subdir() {
    // Unpack into a new named subdirectory
    let mut session = Session::new();
    // Store, pack, clear in one eval to preserve stack
    session.eval("42 \"x\" STO PACKDIR CLVAR \"backup\" UNPACKDIR").unwrap();

    // Variable should NOT be in current directory (it's in "backup" subdir)
    assert!(session.eval("\"x\" RCL").is_err());

    // Verify the subdirectory was created and has content
    // PGDIR should FAIL because directory is not empty (contains x)
    assert!(
        session.eval("\"backup\" PGDIR").is_err(),
        "PGDIR should fail because backup contains the unpacked variable"
    );
}

#[test]
fn packdir_packinfo_returns_names() {
    // PACKINFO should return list of entry names
    let mut session = Session::new();
    session.eval("CLVAR").unwrap();
    session.eval("1 \"alpha\" STO").unwrap();
    session.eval("2 \"beta\" STO").unwrap();
    let values = session.eval("PACKDIR PACKINFO").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 2);
            // Names should be alpha and beta (order may vary)
            let names: Vec<String> = elements.iter().map(|e| to_string(e).to_owned()).collect();
            assert!(names.contains(&"alpha".to_string()));
            assert!(names.contains(&"beta".to_string()));
        }
        _ => panic!("Expected list of names"),
    }
}

#[test]
fn packdir_unpack_conflict_error() {
    // UNPACKDIR should error if a name already exists
    let mut session = Session::new();
    session.eval("42 \"x\" STO").unwrap();
    session.eval("PACKDIR").unwrap();
    // Don't clear - x still exists
    let result = session.eval("UNPACKDIR");
    assert!(result.is_err(), "UNPACKDIR should fail on conflict");
}

#[test]
fn packdir_pack_named_subdir() {
    // Pack a specific subdirectory by name
    // Note: We can only test packing an empty subdirectory since there's
    // no RPL command to enter a directory programmatically
    let mut session = Session::new();
    session.eval("\"mydir\" CRDIR").unwrap();

    // Pack "mydir" from parent and get PACKINFO in one eval
    let values = session.eval("\"mydir\" PACKDIR PACKINFO").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 0, "Empty directory should have no entries");
        }
        _ => panic!("Expected list from PACKINFO"),
    }
}

#[test]
fn packdir_with_subdirectories() {
    // Pack directory that contains subdirectories
    // Note: We can only test with empty subdirectories since there's no
    // RPL command to enter a directory programmatically
    let mut session = Session::new();
    session.eval("CLVAR 1 \"rootvar\" STO \"sub\" CRDIR").unwrap();

    // Pack root (includes empty subdirectory), clear, then unpack
    session.eval("PACKDIR \"sub\" PGDIR CLVAR UNPACKDIR").unwrap();

    // Verify root variable was restored
    let values = session.eval("\"rootvar\" RCL").unwrap();
    match &values[0] {
        Value::Int(i) => assert_eq!(*i, 1),
        Value::Real(r) => assert_eq!(*r, 1.0),
        _ => panic!("Expected number"),
    }

    // Verify subdirectory was recreated (PGDIR should work on it - it's empty)
    session.eval("\"sub\" PGDIR").unwrap();
}
