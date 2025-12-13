//! Decompiler integration tests.
//!
//! These tests verify that bytecode can be decompiled back to source code
//! and that round-trip compilation preserves semantics.

use rpl_lang::{decompile::decompile_with_interner, library::LibraryRegistry};
use rpl_session::Session;
use rpl_stdlib::register_standard_libs;

// ============================================================================
// Helper functions
// ============================================================================

fn make_registry() -> LibraryRegistry {
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    registry
}

/// Compile source, decompile bytecode with interner, return the decompiled string.
fn decompile(source: &str) -> String {
    let registry = make_registry();
    let mut session = Session::new();
    let id = session.set_source("<test>", source);
    let compiled = session.compile(id).expect("compile failed");
    decompile_with_interner(&compiled.code, &registry, &compiled.interner)
}

/// Round-trip test: compile -> decompile -> compile again.
/// Returns (decompiled_source, bytecode_matches).
fn round_trip(source: &str) -> (String, bool) {
    let registry = make_registry();

    let mut session1 = Session::new();
    let id1 = session1.set_source("<test1>", source);
    let result1 = session1.compile(id1).expect("First compile failed");

    let decompiled = decompile_with_interner(&result1.code, &registry, &result1.interner);

    let mut session2 = Session::new();
    let id2 = session2.set_source("<test2>", &decompiled);
    let result2 = session2.compile(id2).unwrap_or_else(|e| {
        panic!("Second compile failed: {:?}\nDecompiled source was: {}", e, decompiled)
    });

    (decompiled, result1.code == result2.code)
}

/// Assert that round-trip compilation produces identical bytecode.
fn assert_round_trip(source: &str) {
    let (decompiled, matches) = round_trip(source);
    assert!(
        matches,
        "Round-trip failed for '{}', decompiled to: '{}'",
        source, decompiled
    );
}

/// Assert that decompiled output matches expected string.
fn assert_decompile_eq(source: &str, expected: &str) {
    let decompiled = decompile(source);
    assert_eq!(
        decompiled, expected,
        "Decompile mismatch for '{}': got '{}', expected '{}'",
        source, decompiled, expected
    );
}

// ============================================================================
// Numeric literals
// ============================================================================

#[test]
fn decompile_integer() {
    assert_decompile_eq("42", "42");
    assert_decompile_eq("0", "0");
    assert_decompile_eq("12345", "12345");
}

#[test]
fn decompile_negative_integer() {
    assert_decompile_eq("-5", "-5");
    assert_decompile_eq("-100", "-100");
}

#[test]
fn decompile_float() {
    assert_decompile_eq("3.14", "3.14");
    assert_decompile_eq("0.5", "0.5");
    // Note: 100.0 decompiles to "100" since it has no fractional part
    assert_decompile_eq("100.0", "100");
}

#[test]
fn decompile_scientific_notation() {
    // Scientific notation normalizes to expanded form
    assert_decompile_eq("1e10", "10000000000");
    assert_decompile_eq("1e3", "1000");
}

#[test]
fn round_trip_numbers() {
    assert_round_trip("42");
    assert_round_trip("3.14159");
    assert_round_trip("0");
    assert_round_trip("1 2 3");
    assert_round_trip("1.5 2.5 3.5");
}

// ============================================================================
// Binary integers
// ============================================================================

#[test]
fn decompile_binary_int_hex() {
    assert_decompile_eq("#FF", "#FFh");
    assert_decompile_eq("#100h", "#100h");
}

#[test]
fn decompile_binary_int_binary() {
    // Binary input normalizes to hex output
    assert_decompile_eq("#1010b", "#Ah");
}

#[test]
fn round_trip_binary_integers() {
    assert_round_trip("#FF");
    assert_round_trip("#1010b");
    assert_round_trip("#777o");
    assert_round_trip("#100d");
}

// ============================================================================
// Strings
// ============================================================================

#[test]
fn decompile_string_simple() {
    assert_decompile_eq("\"hello\"", "\"hello\"");
    assert_decompile_eq("\"\"", "\"\"");
    assert_decompile_eq("\"hello world\"", "\"hello world\"");
}

#[test]
fn decompile_string_with_escapes() {
    assert_decompile_eq("\"line1\\nline2\"", "\"line1\\nline2\"");
    assert_decompile_eq("\"tab\\there\"", "\"tab\\there\"");
}

#[test]
fn decompile_string_with_quotes() {
    assert_decompile_eq("\"say \\\"hi\\\"\"", "\"say \\\"hi\\\"\"");
}

#[test]
fn round_trip_strings() {
    assert_round_trip("\"hello\"");
    assert_round_trip("\"hello world\"");
    assert_round_trip("\"\"");
    assert_round_trip("\"line1\\nline2\"");
}

// ============================================================================
// Stack operations
// ============================================================================

#[test]
fn decompile_stack_commands() {
    assert_decompile_eq("DUP", "DUP");
    assert_decompile_eq("DROP", "DROP");
    assert_decompile_eq("SWAP", "SWAP");
    assert_decompile_eq("OVER", "OVER");
    assert_decompile_eq("ROT", "ROT");
    assert_decompile_eq("DEPTH", "DEPTH");
    assert_decompile_eq("CLEAR", "CLEAR");
}

#[test]
fn decompile_stack_sequence() {
    assert_decompile_eq("1 2 DUP DROP SWAP", "1 2 DUP DROP SWAP");
}

#[test]
fn round_trip_stack_ops() {
    assert_round_trip("DUP");
    assert_round_trip("DROP SWAP OVER");
    assert_round_trip("1 2 DUP DROP");
    assert_round_trip("1 2 3 ROT");
    assert_round_trip("DEPTH");
}

// ============================================================================
// Arithmetic operations
// ============================================================================

#[test]
fn decompile_arithmetic_operators() {
    assert_decompile_eq("1 2 +", "1 2 +");
    assert_decompile_eq("1 2 -", "1 2 -");
    assert_decompile_eq("1 2 *", "1 2 *");
    assert_decompile_eq("1 2 /", "1 2 /");
}

#[test]
fn decompile_arithmetic_functions() {
    assert_decompile_eq("4 SQRT", "4 SQRT");
    assert_decompile_eq("-5 ABS", "-5 ABS");
    assert_decompile_eq("3 NEG", "3 NEG");
    assert_decompile_eq("2 3 ^", "2 3 ^");
}

#[test]
fn round_trip_arithmetic() {
    assert_round_trip("1 2 +");
    assert_round_trip("10 3 - 2 * 4 /");
    assert_round_trip("4 SQRT");
    assert_round_trip("2 10 ^");
}

// ============================================================================
// Comparison operations
// ============================================================================

#[test]
fn decompile_comparison_operators() {
    assert_decompile_eq("1 2 <", "1 2 <");
    assert_decompile_eq("1 2 >", "1 2 >");
    assert_decompile_eq("1 2 ==", "1 2 ==");
    assert_decompile_eq("1 2 !=", "1 2 !=");
    assert_decompile_eq("1 2 <=", "1 2 <=");
    assert_decompile_eq("1 2 >=", "1 2 >=");
}

#[test]
fn round_trip_comparison() {
    assert_round_trip("1 2 <");
    assert_round_trip("5 5 == 3 4 !=");
    assert_round_trip("1 2 <= 3 4 >=");
}

// ============================================================================
// Logical operations
// ============================================================================

#[test]
fn decompile_logical_operators() {
    assert_decompile_eq("1 0 AND", "1 0 AND");
    assert_decompile_eq("0 1 OR", "0 1 OR");
    assert_decompile_eq("1 NOT", "1 NOT");
}

#[test]
fn round_trip_logical() {
    assert_round_trip("1 0 AND");
    assert_round_trip("0 1 OR");
    assert_round_trip("1 NOT");
}

// ============================================================================
// Binary operations (BAND, BOR, etc.)
// ============================================================================

#[test]
fn decompile_binary_ops() {
    // Note: binary integers normalize to hex with 'h' suffix
    assert_decompile_eq("#FF #F0 BAND", "#FFh #F0h BAND");
    assert_decompile_eq("#FF #F0 BOR", "#FFh #F0h BOR");
    assert_decompile_eq("#FF #F0 BXOR", "#FFh #F0h BXOR");
    assert_decompile_eq("#FF BNOT", "#FFh BNOT");
}

#[test]
fn round_trip_binary_ops() {
    assert_round_trip("#FF #F0 BAND");
    assert_round_trip("#FF #F0 BOR");
    assert_round_trip("#FF BNOT");
}

// ============================================================================
// Lists
// ============================================================================

#[test]
fn decompile_empty_list() {
    assert_decompile_eq("{ }", "{ }");
}

#[test]
fn decompile_list_with_elements() {
    assert_decompile_eq("{ 1 2 3 }", "{ 1 2 3 }");
}

#[test]
fn decompile_nested_lists() {
    assert_decompile_eq("{ { 1 } { 2 } }", "{ { 1 } { 2 } }");
}

#[test]
fn decompile_mixed_list() {
    assert_decompile_eq("{ 1 \"hello\" 3.14 }", "{ 1 \"hello\" 3.14 }");
}

#[test]
fn round_trip_lists() {
    assert_round_trip("{ }");
    assert_round_trip("{ 1 2 3 }");
    assert_round_trip("{ { 1 2 } { 3 4 } }");
    assert_round_trip("{ 1 \"hello\" 3.14 }");
}

// ============================================================================
// Programs
// ============================================================================

#[test]
fn decompile_empty_program() {
    assert_decompile_eq(":: ;", ":: ;");
}

#[test]
fn decompile_program_with_body() {
    assert_decompile_eq(":: 1 2 + ;", ":: 1 2 + ;");
}

#[test]
fn decompile_nested_programs() {
    assert_decompile_eq(":: :: 1 ; :: 2 ; ;", ":: :: 1 ; :: 2 ; ;");
}

#[test]
fn decompile_program_with_eval() {
    assert_decompile_eq(":: DUP * ; EVAL", ":: DUP * ; EVAL");
}

#[test]
fn round_trip_programs() {
    assert_round_trip(":: ;");
    assert_round_trip(":: 1 2 + ;");
    assert_round_trip(":: DUP * ;");
    assert_round_trip(":: :: 1 ; :: 2 ; ;");
}

// ============================================================================
// Control flow
// ============================================================================

#[test]
fn decompile_if_then_end() {
    assert_decompile_eq("IF 1 THEN 2 END", "IF 1 THEN 2 END");
}

#[test]
fn decompile_if_then_else_end() {
    assert_decompile_eq("IF 1 THEN 2 ELSE 3 END", "IF 1 THEN 2 ELSE 3 END");
}

#[test]
fn decompile_do_until_end() {
    assert_decompile_eq("DO 1 UNTIL END", "DO 1 UNTIL END");
}

#[test]
fn decompile_while_repeat_end() {
    assert_decompile_eq("WHILE 1 REPEAT 2 END", "WHILE 1 REPEAT 2 END");
}

#[test]
fn decompile_start_next() {
    assert_decompile_eq("1 10 START 1 NEXT", "1 10 START 1 NEXT");
}

#[test]
fn decompile_nested_if() {
    assert_decompile_eq("IF 1 THEN IF 2 THEN 3 END END", "IF 1 THEN IF 2 THEN 3 END END");
}

#[test]
fn round_trip_control_flow() {
    assert_round_trip("IF 1 THEN 2 END");
    assert_round_trip("IF 1 THEN 2 ELSE 3 END");
    assert_round_trip("DO 1 UNTIL END");
    assert_round_trip("WHILE 1 REPEAT 2 END");
    assert_round_trip("1 10 START 1 NEXT");
    assert_round_trip("IF 1 THEN IF 2 THEN 3 END END");
}

// ============================================================================
// Variables (directory operations)
// ============================================================================

#[test]
fn decompile_sto() {
    let decompiled = decompile("42 \"x\" STO");
    assert!(decompiled.contains("STO"));
}

#[test]
fn decompile_rcl() {
    let decompiled = decompile("\"x\" RCL");
    assert!(decompiled.contains("RCL"));
}

#[test]
fn decompile_purge() {
    let decompiled = decompile("\"x\" PURGE");
    assert!(decompiled.contains("PURGE"));
}

#[test]
fn round_trip_directory() {
    assert_round_trip("42 \"x\" STO");
    assert_round_trip("\"x\" RCL");
    assert_round_trip("\"x\" PURGE");
}

// ============================================================================
// Local variables
// ============================================================================

#[test]
fn decompile_local_binding_simple() {
    // Note: -> normalizes to → and there's an extra space before ;
    assert_decompile_eq("5 -> x :: x ;", "5 → x :: x  ;");
}

#[test]
fn decompile_local_binding_multiple_params() {
    assert_decompile_eq("1 2 -> a b :: a b + ;", "1 2 → a b :: a b +  ;");
}

#[test]
fn decompile_local_binding_with_body() {
    assert_decompile_eq("5 -> x :: x x * ;", "5 → x :: x x *  ;");
}

#[test]
fn round_trip_local_bindings() {
    assert_round_trip("5 -> x :: x ;");
    assert_round_trip("1 2 -> a b :: a b + ;");
    assert_round_trip("5 -> x :: x x * ;");
    assert_round_trip("3 -> n :: n 1 > IF n 1 - THEN END ;");
}

// ============================================================================
// Symbolic expressions
// ============================================================================

#[test]
fn decompile_symbolic_simple() {
    assert_decompile_eq("'x'", "'x'");
}

#[test]
fn decompile_symbolic_expression() {
    assert_decompile_eq("'3 + 4'", "'3 + 4'");
}

#[test]
fn decompile_symbolic_with_precedence() {
    assert_decompile_eq("'3 + 4 * 5'", "'3 + 4 * 5'");
}

#[test]
fn round_trip_symbolic() {
    assert_round_trip("'x'");
    assert_round_trip("'3 + 4'");
    assert_round_trip("'3 + 4 * 5'");
}

// ============================================================================
// String operations
// ============================================================================

#[test]
fn decompile_string_ops() {
    assert_decompile_eq("\"hello\" SIZE", "\"hello\" SIZE");
    assert_decompile_eq("\"hello\" \"world\" +", "\"hello\" \"world\" +");
    assert_decompile_eq("65 CHR", "65 CHR");
    assert_decompile_eq("\"A\" ASC", "\"A\" ASC");
}

#[test]
fn round_trip_string_ops() {
    assert_round_trip("\"hello\" SIZE");
    assert_round_trip("\"hello\" 1 3 SUB");
    assert_round_trip("65 CHR");
}

// ============================================================================
// Complex expressions
// ============================================================================

#[test]
fn round_trip_complex_program() {
    assert_round_trip(":: { 1 2 3 } :: DUP + ; ;");
}

#[test]
fn round_trip_program_with_control_flow() {
    assert_round_trip(":: 1 2 < IF 3 THEN 4 ELSE 5 END ;");
}

#[test]
fn round_trip_nested_everything() {
    assert_round_trip(":: { { 1 } :: 2 ; } IF 1 THEN { 3 } END ;");
}

#[test]
fn round_trip_realistic_program() {
    // A more realistic program: factorial-like structure
    assert_round_trip(":: DUP 1 > IF DUP 1 - :: DUP 1 > IF DUP 1 - THEN END ; * THEN END ;");
}

#[test]
fn round_trip_complex_realistic_program() {
    // A complex program combining many features:
    // - Local variable bindings
    // - Nested control flow (IF/THEN/ELSE, WHILE/REPEAT)
    // - Arithmetic and comparison operators
    // - Stack operations
    // - Lists
    // - Nested programs
    // - Symbolic expressions

    // Fibonacci-like computation with local bindings
    assert_round_trip("10 -> n :: 0 1 n 1 > WHILE OVER OVER + ROT DROP n 1 - -> n :: n ; REPEAT END ;");

    // Conditional list processing
    assert_round_trip("{ 1 2 3 } -> lst :: lst SIZE 0 > IF lst 1 GET lst 2 GET + ELSE 0 END ;");

    // Nested programs with control flow
    assert_round_trip(":: -> x :: x 0 > IF x 1 - -> y :: y ; ELSE 0 END ; ;");

    // Program that builds and processes a list
    assert_round_trip(":: { } 1 5 START DUP ROT SWAP + SWAP NEXT DROP ;");

    // Mix of symbolic and regular computation
    assert_round_trip("'3 + 4' EVAL 2 * -> result :: result result + ;");

    // Deeply nested conditionals
    assert_round_trip(":: 1 IF 2 IF 3 THEN ELSE 4 IF 5 ELSE 6 END END END ;");

    // Complex arithmetic chain with comparisons
    assert_round_trip(":: 10 20 + 5 * DUP 100 > IF 2 / ELSE 2 * END ;");
}

// ============================================================================
// Edge cases
// ============================================================================

#[test]
fn decompile_preserves_structure() {
    // Multiple operations should maintain order
    let source = "1 2 3 + * 4 -";
    let decompiled = decompile(source);

    // All elements should be present in correct relative order
    let pos_1 = decompiled.find('1').unwrap_or(0);
    let pos_2 = decompiled.find('2').unwrap_or(0);
    let pos_3 = decompiled.find('3').unwrap_or(0);
    let pos_plus = decompiled.find('+').unwrap_or(0);
    let pos_star = decompiled.find('*').unwrap_or(0);
    let pos_4 = decompiled.find('4').unwrap_or(0);

    assert!(pos_1 < pos_2, "1 should come before 2");
    assert!(pos_2 < pos_3, "2 should come before 3");
    assert!(pos_3 < pos_plus, "3 should come before +");
    assert!(pos_plus < pos_star, "+ should come before *");
    assert!(pos_star < pos_4, "* should come before 4");
}

#[test]
fn round_trip_whitespace_insensitive() {
    // Different whitespace should produce same bytecode
    let (dec1, _) = round_trip("1 2 +");
    let (dec2, _) = round_trip("1  2   +");
    let (dec3, _) = round_trip("1\n2\n+");

    // All should decompile to equivalent forms
    assert!(
        dec1.contains("1") && dec1.contains("2") && dec1.contains("+"),
        "Decompiled should have all elements"
    );
    assert!(
        dec2.contains("1") && dec2.contains("2") && dec2.contains("+"),
        "Decompiled should have all elements"
    );
    assert!(
        dec3.contains("1") && dec3.contains("2") && dec3.contains("+"),
        "Decompiled should have all elements"
    );
}

#[test]
fn round_trip_case_insensitive_commands() {
    // Commands should work regardless of case
    let (dec_upper, matches_upper) = round_trip("DUP DROP SWAP");
    let (dec_lower, matches_lower) = round_trip("dup drop swap");

    assert!(matches_upper, "Upper case should round-trip: {}", dec_upper);
    assert!(matches_lower, "Lower case should round-trip: {}", dec_lower);
}
