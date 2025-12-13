//! Tests for stack manipulation operations.

use super::assert_stack_eq;

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
    // ROLLD 3: ( a b c -- c a b ) moves top to 3rd position
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
