//! Tests for binary integer operations (migrated from tests/binary_int.rs).
//!
//! Tests HP-style binary integer literals and bitwise operations.

use rpl::value::Value;

/// Helper to evaluate code and extract the stack as i64 values.
fn eval_to_ints(code: &str) -> Vec<i64> {
    let mut session = crate::session_with_stdlib();
    let values = session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}': {:?}", code, e));
    values
        .iter()
        .map(|v| match v {
            Value::Integer(i) => *i,
            Value::Real(r) => *r as i64,
            other => panic!("Expected Int, got {:?}", other),
        })
        .collect()
}

/// Helper to check stack contents.
fn assert_stack_eq(code: &str, expected: &[i64]) {
    let actual = eval_to_ints(code);
    assert_eq!(
        actual.len(),
        expected.len(),
        "Stack depth mismatch for '{}': expected {:?}, got {:?}",
        code,
        expected,
        actual
    );
    for (i, (a, e)) in actual.iter().zip(expected.iter()).enumerate() {
        assert_eq!(
            a, e,
            "Stack[{}] mismatch for '{}': expected {}, got {}",
            i, code, e, a
        );
    }
}

/// Helper that expects an error.
fn assert_error(code: &str) {
    let mut session = crate::session_with_stdlib();
    let result = session.eval(code);
    assert!(
        result.is_err(),
        "Expected error for '{}', got {:?}",
        code,
        result
    );
}

// ============================================================================
// Binary Integer Literals
// ============================================================================

#[test]
fn bint_hex_default() {
    assert_stack_eq("#0", &[0]);
    assert_stack_eq("#FF", &[255]);
    assert_stack_eq("#10", &[16]);
    assert_stack_eq("#FFFF", &[65535]);
}

#[test]
fn bint_hex_explicit() {
    assert_stack_eq("#FFh", &[255]);
    assert_stack_eq("#10h", &[16]);
}

#[test]
fn bint_binary() {
    assert_stack_eq("#0b", &[0]);
    assert_stack_eq("#1b", &[1]);
    assert_stack_eq("#10b", &[2]);
    assert_stack_eq("#1010b", &[10]);
    assert_stack_eq("#11111111b", &[255]);
}

#[test]
fn bint_octal() {
    assert_stack_eq("#0o", &[0]);
    assert_stack_eq("#7o", &[7]);
    assert_stack_eq("#10o", &[8]);
    assert_stack_eq("#377o", &[255]);
}

#[test]
fn bint_decimal() {
    assert_stack_eq("#0d", &[0]);
    assert_stack_eq("#10d", &[10]);
    assert_stack_eq("#255d", &[255]);
    assert_stack_eq("#1000d", &[1000]);
}

#[test]
fn bint_multiple() {
    assert_stack_eq("#10 #20 #30", &[16, 32, 48]);
}

// ============================================================================
// Bitwise Operations
// ============================================================================

#[test]
fn band_basic() {
    assert_stack_eq("#F0 #0F BAND", &[0]);
    assert_stack_eq("#FF #0F BAND", &[0x0F]);
    assert_stack_eq("#FF #FF BAND", &[255]);
}

#[test]
fn bor_basic() {
    assert_stack_eq("#F0 #0F BOR", &[0xFF]);
    assert_stack_eq("#FF #0F BOR", &[0xFF]);
    assert_stack_eq("#0 #0 BOR", &[0]);
}

#[test]
fn bxor_basic() {
    assert_stack_eq("#FF #F0 BXOR", &[0x0F]);
    assert_stack_eq("#FF #FF BXOR", &[0]);
    assert_stack_eq("#FF #0 BXOR", &[0xFF]);
}

#[test]
fn bnot_64bit() {
    // With 64-bit integers, NOT of 0 is all 1s (-1 in two's complement)
    assert_stack_eq("#0 BNOT", &[-1]);
    // NOT of #FF gives all bits set except lowest 8
    assert_stack_eq("#FF BNOT", &[!0xFFi64]);
}

#[test]
fn blsl_basic() {
    assert_stack_eq("#1 #4 BLSL", &[16]);
    assert_stack_eq("#1 #0 BLSL", &[1]);
    assert_stack_eq("#FF #8 BLSL", &[0xFF00]);
}

#[test]
fn blsr_basic() {
    assert_stack_eq("#10 #4 BLSR", &[1]);
    assert_stack_eq("#FF #4 BLSR", &[0x0F]);
    assert_stack_eq("#100 #8 BLSR", &[1]);
}

// ============================================================================
// Binary Integer Arithmetic
// ============================================================================

#[test]
fn badd_basic() {
    assert_stack_eq("#10 #5 BADD", &[0x15]);
    assert_stack_eq("#FF #1 BADD", &[0x100]);
    assert_stack_eq("#0 #0 BADD", &[0]);
}

#[test]
fn bsub_basic() {
    assert_stack_eq("#10 #5 BSUB", &[0x0B]);
    assert_stack_eq("#FF #1 BSUB", &[0xFE]);
    assert_stack_eq("#100 #1 BSUB", &[0xFF]);
}

#[test]
fn bmul_basic() {
    assert_stack_eq("#10 #5 BMUL", &[0x50]);
    assert_stack_eq("#FF #0 BMUL", &[0]);
    assert_stack_eq("#2 #2 BMUL", &[4]);
}

#[test]
fn bdiv_basic() {
    assert_stack_eq("#10 #5 BDIV", &[3]);
    assert_stack_eq("#FF #10 BDIV", &[15]);
    assert_stack_eq("#100 #10 BDIV", &[16]);
}

#[test]
fn bdiv_by_zero() {
    assert_error("#10 #0 BDIV");
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn chained_bitwise() {
    // (#F0 | #0F) & #FF = #FF & #FF = #FF
    assert_stack_eq("#F0 #0F BOR #FF BAND", &[0xFF]);
}

#[test]
fn shift_and_mask() {
    // (#1 << 4) | #0F = #10 | #0F = #1F
    assert_stack_eq("#1 #4 BLSL #0F BOR", &[0x1F]);
}

#[test]
fn extract_byte() {
    // Extract second byte from #AABBCCDD: shift right 8, mask with FF
    assert_stack_eq("#AABBCCDDh #8 BLSR #FF BAND", &[0xCC]);
}

#[test]
fn set_bit() {
    // Set bit 3 in #F0: #F0 | (1 << 3) = #F0 | #8 = #F8
    assert_stack_eq("#F0 #1 #3 BLSL BOR", &[0xF8]);
}

#[test]
fn clear_bit() {
    // Clear bit 4 in #FF: #FF & ~(1 << 4) = #FF & ~#10
    // With 64-bit, ~#10 has many high bits set, but AND with #FF masks to 8 bits
    assert_stack_eq("#FF #1 #4 BLSL BNOT BAND", &[0xEF]);
}

#[test]
fn toggle_bits() {
    // Toggle bits 0-3 in #F0: #F0 ^ #0F = #FF
    assert_stack_eq("#F0 #0F BXOR", &[0xFF]);
}

#[test]
fn rotate_left_8bit() {
    // Simulate 8-bit rotate left by 2: ((x << 2) | (x >> 6)) & 0xFF
    // #81 rotated left 2 = #06 (10000001 -> 00000110)
    assert_stack_eq("#81h DUP #2 BLSL SWAP #6 BLSR BOR #FF BAND", &[0x06]);
}

#[test]
fn multiply_powers_of_two() {
    // Multiply by 8 using shift: #A * 8 = #A << 3 = #50
    assert_stack_eq("#A #3 BLSL", &[0x50]);
}

#[test]
fn divide_powers_of_two() {
    // Divide by 4 using shift: #40 / 4 = #40 >> 2 = #10
    assert_stack_eq("#40 #2 BLSR", &[0x10]);
}

#[test]
fn bitmask_check() {
    // Check if bits 2 and 3 are both set in #0F: (#0F & #0C) == #0C
    // Result: #0C (both bits set)
    assert_stack_eq("#0F #0C BAND", &[0x0C]);
}

#[test]
fn combine_nibbles() {
    // Combine two nibbles: (high << 4) | low = (#A << 4) | #B = #AB
    assert_stack_eq("#A #4 BLSL #B BOR", &[0xAB]);
}

#[test]
fn swap_nibbles_8bit() {
    // Swap nibbles in 8-bit value: #AB -> #BA
    // ((x << 4) | (x >> 4)) & 0xFF
    assert_stack_eq("#AB DUP #4 BLSL SWAP #4 BLSR BOR #FF BAND", &[0xBA]);
}

#[test]
fn sign_extension_simulation() {
    // Sign extend 8-bit to 16-bit (if high bit set, OR with #FF00)
    // #80 (negative in 8-bit) -> #FF80
    // Using: if (x & #80) != 0 then x | #FF00 else x
    // Simplified: #80 | #FF00 = #FF80
    assert_stack_eq("#80 #FF00 BOR", &[0xFF80]);
}

#[test]
fn mixed_operations() {
    // Complex: ((#A + #5) * #2) ^ #F = (#F * #2) ^ #F = #1E ^ #F = #11
    assert_stack_eq("#A #5 BADD #2 BMUL #F BXOR", &[0x11]);
}

#[test]
fn nested_shifts() {
    // (#1 << 4) << 4 = #10 << 4 = #100
    assert_stack_eq("#1 #4 BLSL #4 BLSL", &[0x100]);
}

#[test]
fn multiple_values_on_stack() {
    // Push several binary integers, operate on pairs
    assert_stack_eq("#FF #F0 BAND #0F #01 BOR", &[0xF0, 0x0F]);
}

// ============================================================================
// Case Sensitivity
// ============================================================================

#[test]
fn commands_case_sensitive() {
    // Only uppercase commands work (case-sensitive)
    assert_stack_eq("#FF #0F BAND", &[0x0F]);
}

// ============================================================================
// Mixed with Real Numbers
// ============================================================================

#[test]
fn bint_and_real_on_stack() {
    // Binary integers and reals can coexist on stack
    let mut session = crate::session_with_stdlib();
    let values = session.eval("#FF 3.15").unwrap();
    assert_eq!(values.len(), 2);
    assert!(matches!(values[0], Value::Integer(255)));
    assert!(matches!(values[1], Value::Real(r) if (r - 3.15).abs() < 1e-10));
}

// ============================================================================
// Type Coercion and Error Tests
// ============================================================================

#[test]
fn real_coerced_to_int_for_bint_ops() {
    // Reals are automatically truncated to integers for binary ops
    // 3.14 truncates to 3, 3 & 5 = 1
    assert_stack_eq("3.14 #5 BAND", &[1]);
    // 7.9 truncates to 7, 7 | 8 = 15
    assert_stack_eq("7.9 #8 BOR", &[15]);
}

#[test]
fn type_error_string_with_bint_op() {
    // Strings cannot be used with binary integer operations
    assert_error("\"hello\" #5 BAND");
}

#[test]
fn underflow_error_band() {
    // BAND on empty stack should give underflow
    assert_error("BAND");
}

#[test]
fn underflow_error_bnot() {
    // BNOT on empty stack should give underflow
    assert_error("BNOT");
}
