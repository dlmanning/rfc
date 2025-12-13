//! Tests for transcendental functions (trig, log, exp).

use super::{assert_error, assert_stack_approx};

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
