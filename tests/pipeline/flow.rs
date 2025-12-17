//! Tests for flow control structures.

use super::assert_stack_eq;

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
// Test IF THEN without ELSE inside WHILE
#[test]
fn while_with_if_then_no_else() {
    // Simplified prime check: 1 2 WHILE DUP 4 <= REPEAT ... END DROP
    // Should modify first value based on condition
    assert_stack_eq(
        "1 2 WHILE DUP DUP * 18 <= REPEAT 18 OVER MOD 0 == IF THEN SWAP DROP 0 SWAP END 1 + END DROP",
        &[0.0],
    );
}

// Test local binding with nested IF and WHILE
#[test]
fn local_binding_with_nested_if_while() {
    // Simplified is_prime for 18: should return 0
    assert_stack_eq(
        "<< -> n <<
            IF n 2 <
            THEN 0
            ELSE
                IF n 2 ==
                THEN 1
                ELSE
                    1 2
                    WHILE DUP DUP * n <= REPEAT
                        n OVER MOD 0 ==
                        IF THEN SWAP DROP 0 SWAP END
                        1 +
                    END
                    DROP
                END
            END
        >> >> 18 SWAP EVAL",
        &[0.0],
    );
}

// Test local binding with simpler structure
#[test]
fn local_binding_with_while() {
    // Use local n in WHILE condition
    assert_stack_eq(
        "<< -> n << 1 2 WHILE DUP DUP * n <= REPEAT n OVER MOD 0 == IF THEN SWAP DROP 0 SWAP END 1 + END DROP >> >> 18 SWAP EVAL",
        &[0.0],
    );
}

// Basic local binding test
#[test]
fn local_binding_basic() {
    // Just push local n
    assert_stack_eq(
        "<< -> n << n >> >> 42 SWAP EVAL",
        &[42.0],
    );
}

// Local binding with arithmetic
#[test]
fn local_binding_arithmetic() {
    assert_stack_eq(
        "<< -> n << n n + >> >> 10 SWAP EVAL",
        &[20.0],
    );
}

// Local in simple WHILE
#[test]
fn local_in_simple_while() {
    // Just reference n in WHILE condition
    assert_stack_eq(
        "<< -> n << WHILE n REPEAT END >> >> 0 SWAP EVAL",
        &[],
    );
}

// Local after WHILE
#[test]
fn local_after_while() {
    // WHILE with literal 0 (no iterations), then push n
    assert_stack_eq(
        "<< -> n << WHILE 0 REPEAT END n >> >> 42 SWAP EVAL",
        &[42.0],
    );
}

// Check if `n` in WHILE condition resolves
#[test]
fn local_in_while_check() {
    // n=0, WHILE n (should be false immediately)
    assert_stack_eq(
        "<< -> n << 100 WHILE n REPEAT DROP 1 END >> >> 0 SWAP EVAL",
        &[100.0],
    );
}

// Test local directly (no outer program or EVAL)
#[test]
fn local_binding_direct() {
    // Direct: 5 -> n << n >> should give 5
    assert_stack_eq(
        "5 -> n << n >>",
        &[5.0],
    );
}

// Local in WHILE directly (no outer program)
#[test]
fn local_in_while_direct() {
    // 5 -> n << WHILE n REPEAT ... END >>
    assert_stack_eq(
        "5 -> n << 0 WHILE DUP n < REPEAT 1 + END >>",
        &[5.0],
    );
}

// Even simpler: just WHILE with local in condition
#[test]
fn local_in_while_simplest() {
    // 0 -> n << WHILE n REPEAT END >> - should just exit immediately
    assert_stack_eq(
        "0 -> n << WHILE n REPEAT END >>",
        &[],
    );
}

// Local used BEFORE WHILE in same body - does it work?
#[test]
fn local_before_while() {
    // 5 -> n << n WHILE 0 REPEAT END >> - push n then skip WHILE
    assert_stack_eq(
        "5 -> n << n WHILE 0 REPEAT END >>",
        &[5.0],
    );
}

// Multi-token condition with local - test that locals work in WHILE condition
#[test]
fn local_in_multi_token_condition() {
    // Use local n in a condition that evaluates to false immediately
    // 5 0 == is 0 (false), so the loop body never executes
    assert_stack_eq(
        "5 -> n << 0 WHILE n 0 == REPEAT DROP 1 END >>",
        &[0.0],
    );
}

// Just test DUP n
#[test]
fn local_after_dup() {
    // 0 DUP n -> should give 0, 5
    assert_stack_eq(
        "5 -> n << 0 DUP n >>",
        &[0.0, 0.0, 5.0],
    );
}

// Test n at different positions
#[test]
fn local_after_literal() {
    // Just 1 n -> should give 1, 5
    assert_stack_eq(
        "5 -> n << 1 n >>",
        &[1.0, 5.0],
    );
}

// n twice
#[test]
fn local_twice() {
    assert_stack_eq(
        "5 -> n << n n >>",
        &[5.0, 5.0],
    );
}

// Local after a command
#[test]
fn local_after_command() {
    // 1 DROP n -> should give 5
    assert_stack_eq(
        "5 -> n << 1 DROP n >>",
        &[5.0],
    );
}

// Local after DUP command
#[test]
fn local_after_dup_only() {
    // n DUP n -> n n n
    assert_stack_eq(
        "5 -> n << n DUP n >>",
        &[5.0, 5.0, 5.0],
    );
}

// Literal then DUP then n
#[test]
fn local_after_literal_dup() {
    // 0 DUP -> 0 0, then n should be 5
    assert_stack_eq(
        "5 -> n << 7 DUP n >>",
        &[7.0, 7.0, 5.0],
    );
}

// Two locals with different names
#[test]
fn two_locals_test() {
    // a=5, b=10, push a then b
    assert_stack_eq(
        "5 10 -> a b << a b >>",
        &[5.0, 10.0],
    );
}

// Test stored program with local binding
#[test]
fn stored_program_with_local() {
    // Simple: store a program that uses a local, call it
    assert_stack_eq(
        "<< -> n << n >> >> \"test\" STO 5 test",
        &[5.0],
    );
}

// Test simple local access inside WHILE
#[test]
fn local_in_while_condition() {
    // Simple: count from 1 while counter <= n
    assert_stack_eq(
        "<< -> n << 0 WHILE DUP n < REPEAT 1 + END >> >> 5 SWAP EVAL",
        &[5.0],
    );
}

// Test local access in body
#[test]
fn local_in_while_body() {
    // Accumulate n three times
    assert_stack_eq(
        "<< -> n << 0 3 WHILE DUP 0 > REPEAT 1 - SWAP n + SWAP END DROP >> >> 10 SWAP EVAL",
        &[30.0],
    );
}

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

#[test]
fn while_count_to_five() {
    // Count up from 0 until reaching 5
    assert_stack_eq("0 WHILE DUP 5 < REPEAT 1 + END", &[5.0]);
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

#[test]
fn start_with_nested_local() {
    // Test START/NEXT with a nested local binding - should iterate
    assert_stack_eq("1 1 3 START -> x << x 1 + >> NEXT", &[4.0]);

    // With outer local binding (nested scopes)
    assert_stack_eq("0 -> y << 1 1 3 START -> x << x 1 + >> NEXT >>", &[4.0]);

    // Using outer var inside the loop body
    assert_stack_eq("5 -> y << 0 1 3 START -> x << x y + >> NEXT >>", &[15.0]);
}

#[test]
fn start_next_basic_accumulator() {
    // START/NEXT basic: 0 1 5 START 1 + NEXT -> 5 iterations, add 1 each time
    assert_stack_eq("0 1 5 START 1 + NEXT", &[5.0]);
}

#[test]
fn start_reversed_with_accumulator() {
    // HP behavior: 0 5 4 START 1 + NEXT -> finish < start, body runs once
    // Stack: 0, then after body: 1, then 6 <= 4 fails, exit with 1
    assert_stack_eq("0 5 4 START 1 + NEXT", &[1.0]);
}

// ============================================================================
// FOR/NEXT loops (with loop variable)
// ============================================================================

#[test]
fn for_next_accumulator() {
    // FOR/NEXT basic: 0 1 5 FOR i i + NEXT -> 0+1+2+3+4+5 = 15
    assert_stack_eq("0 1 5 FOR i i + NEXT", &[15.0]);
}

#[test]
fn nested_start_with_outer_local() {
    // Nested START with locals: 5 -> x << 0 1 3 START x + NEXT >>
    // Start with 0, add x=5 three times: 0+5+5+5 = 15
    assert_stack_eq("5 -> x << 0 1 3 START x + NEXT >>", &[15.0]);
}

#[test]
fn for_doesnt_clobber_outer_local() {
    // FOR doesn't clobber outer: 10 -> y << 1 3 FOR i i y + NEXT >>
    // Pushes i+y for i=1,2,3: 1+10=11, 2+10=12, 3+10=13
    assert_stack_eq("10 -> y << 1 3 FOR i i y + NEXT >>", &[11.0, 12.0, 13.0]);
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
// Flow Control: CASE/THEN/END
// RPL syntax: CASE cond1 THEN action1 END cond2 THEN action2 END [default] END
// ============================================================================

#[test]
fn case_first_match() {
    // First test is true, execute its action, skip rest
    assert_stack_eq(
        "CASE 1 THEN 42 END 0 THEN 99 END 0 END",
        &[42.0],
    );
}

#[test]
fn case_second_match() {
    // First test is false, second is true
    assert_stack_eq(
        "CASE 0 THEN 42 END 1 THEN 99 END 0 END",
        &[99.0],
    );
}

#[test]
fn case_default() {
    // All tests false, execute default
    assert_stack_eq(
        "CASE 0 THEN 42 END 0 THEN 99 END 77 END",
        &[77.0],
    );
}

#[test]
fn case_no_default() {
    // All tests false, no default action (nothing pushed)
    assert_stack_eq(
        "CASE 0 THEN 42 END 0 THEN 99 END END",
        &[],
    );
}

#[test]
fn case_with_expressions() {
    // Tests can be expressions
    assert_stack_eq(
        "CASE 3 4 > THEN 1 END 3 4 < THEN 2 END 0 END",
        &[2.0],
    );
}

#[test]
fn case_single_branch() {
    // Single THEN branch
    assert_stack_eq("CASE 1 THEN 42 END END", &[42.0]);
    assert_stack_eq("CASE 0 THEN 42 END END", &[]);
}

#[test]
fn case_nested() {
    // Nested CASE inside action
    // Inner CASE: CASE 1 THEN 42 END END (branch END + CASE END)
    // Outer CASE: CASE 1 THEN <inner> END END (branch END + CASE END)
    assert_stack_eq(
        "CASE 1 THEN CASE 1 THEN 42 END END END END",
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
