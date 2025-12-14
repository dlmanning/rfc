//! Library for complex numbers.
//!
//! This library provides:
//! - `(re, im)` - Complex number literals (rectangular form)
//! - `(mag∠angle)` - Complex number literals (polar form)
//! - `RE` - Extract real part
//! - `IM` - Extract imaginary part
//! - `ARG` - Extract argument (angle)
//! - `ABS` - Magnitude (handled by operator registry)
//! - `CONJ` - Complex conjugate
//! - `C→R` - Split complex to two reals
//! - `R→C` - Combine two reals to complex
//! - `→POLAR` - Convert to polar form
//! - `→RECT` - Convert to rectangular form
//! - Arithmetic: +, -, *, / (via operator registry)

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{ConstructKind, ExecuteContext, ExecuteOk};

rpl_macros::define_library! {
    pub library ComplexLib(30, "Complex");

    commands {
        RE (1 -> 1) "Extract real part" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let re = if is_polar {
                        let (re, _) = polar_to_rect(c1, c2, mode == 1);
                        re
                    } else {
                        c1
                    };
                    if ctx.push_real(re).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        IM (1 -> 1) "Extract imaginary part" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let im = if is_polar {
                        let (_, im) = polar_to_rect(c1, c2, mode == 1);
                        im
                    } else {
                        c2
                    };
                    if ctx.push_real(im).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        ARG (1 -> 1) "Extract argument (angle) in radians" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let angle = if is_polar {
                        if mode == 1 { c2.to_radians() } else { c2 }
                    } else {
                        c2.atan2(c1)
                    };
                    if ctx.push_real(angle).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        CONJ (1 -> 1) "Complex conjugate" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    if is_polar {
                        if push_complex_polar(ctx, c1, -c2, mode).is_err() {
                            return Err("Stack overflow".to_string());
                        }
                    } else if push_complex(ctx, c1, -c2).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        C2R | "C→R" | "C->R" (1 -> 2) "Split complex to two reals" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let (re, im) = if is_polar {
                        polar_to_rect(c1, c2, mode == 1)
                    } else {
                        (c1, c2)
                    };
                    if ctx.push_real(re).is_err() || ctx.push_real(im).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        R2C | "R→C" | "R->C" (2 -> 1) "Combine two reals to complex" {
            let im = match ctx.pop_real() {
                Ok(v) => v,
                Err(_) => {
                    return Err(
                        "R→C requires two numeric values (got underflow or type error for imaginary part)".to_string()
                    );
                }
            };
            let re = match ctx.pop_real() {
                Ok(v) => v,
                Err(_) => {
                    return Err(
                        "R→C requires two numeric values (got underflow or type error for real part)".to_string()
                    );
                }
            };
            if push_complex(ctx, re, im).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        TOPOLAR | "→POLAR" | "->POLAR" (1 -> 1) "Convert to polar form (radians)" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let (mag, angle) = if is_polar {
                        let angle_rad = if mode == 1 { c2.to_radians() } else { c2 };
                        (c1, angle_rad)
                    } else {
                        rect_to_polar(c1, c2)
                    };
                    if push_complex_polar(ctx, mag, angle, 0).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        TORECT | "→RECT" | "->RECT" (1 -> 1) "Convert to rectangular form" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    let (re, im) = if is_polar {
                        polar_to_rect(c1, c2, mode == 1)
                    } else {
                        (c1, c2)
                    };
                    if push_complex(ctx, re, im).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        // Internal commands for operator dispatch
        @ABS (1 -> 1) "Magnitude" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, _mode)) => {
                    let mag = if is_polar {
                        c1.abs()
                    } else {
                        (c1 * c1 + c2 * c2).sqrt()
                    };
                    if ctx.push_real(mag).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        @NEG (1 -> 1) "Negate complex number" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    if is_polar {
                        let new_angle = c2 + if mode == 1 { 180.0 } else { std::f64::consts::PI };
                        if push_complex_polar(ctx, c1, new_angle, mode).is_err() {
                            return Err("Stack overflow".to_string());
                        }
                    } else if push_complex(ctx, -c1, -c2).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        @INV (1 -> 1) "Inverse (1/z)" {
            match pop_complex(ctx) {
                Ok((c1, c2, is_polar, mode)) => {
                    if is_polar {
                        if c1 == 0.0 {
                            return Err("Division by zero".to_string());
                        }
                        if push_complex_polar(ctx, 1.0 / c1, -c2, mode).is_err() {
                            return Err("Stack overflow".to_string());
                        }
                    } else {
                        let mag_sq = c1 * c1 + c2 * c2;
                        if mag_sq == 0.0 {
                            return Err("Division by zero".to_string());
                        }
                        if push_complex(ctx, c1 / mag_sq, -c2 / mag_sq).is_err() {
                            return Err("Stack overflow".to_string());
                        }
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(_) => Err("Expected complex number".to_string()),
            }
        }

        @ADD (2 -> 1) "Complex addition" {
            let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let (a_re, a_im) = if a_polar { polar_to_rect(a1, a2, a_mode == 1) } else { (a1, a2) };
            let (b_re, b_im) = if b_polar { polar_to_rect(b1, b2, b_mode == 1) } else { (b1, b2) };

            if push_complex(ctx, a_re + b_re, a_im + b_im).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @SUB (2 -> 1) "Complex subtraction" {
            let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let (a_re, a_im) = if a_polar { polar_to_rect(a1, a2, a_mode == 1) } else { (a1, a2) };
            let (b_re, b_im) = if b_polar { polar_to_rect(b1, b2, b_mode == 1) } else { (b1, b2) };

            if push_complex(ctx, a_re - b_re, a_im - b_im).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @MUL (2 -> 1) "Complex multiplication" {
            let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let (a_mag, a_angle) = if a_polar {
                (a1, if a_mode == 1 { a2.to_radians() } else { a2 })
            } else {
                rect_to_polar(a1, a2)
            };
            let (b_mag, b_angle) = if b_polar {
                (b1, if b_mode == 1 { b2.to_radians() } else { b2 })
            } else {
                rect_to_polar(b1, b2)
            };

            let result_mag = a_mag * b_mag;
            let result_angle = a_angle + b_angle;
            let (re, im) = polar_to_rect(result_mag, result_angle, false);

            if push_complex(ctx, re, im).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @DIV (2 -> 1) "Complex division" {
            let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let (a_mag, a_angle) = if a_polar {
                (a1, if a_mode == 1 { a2.to_radians() } else { a2 })
            } else {
                rect_to_polar(a1, a2)
            };
            let (b_mag, b_angle) = if b_polar {
                (b1, if b_mode == 1 { b2.to_radians() } else { b2 })
            } else {
                rect_to_polar(b1, b2)
            };

            if b_mag == 0.0 {
                return Err("Division by zero".to_string());
            }

            let result_mag = a_mag / b_mag;
            let result_angle = a_angle - b_angle;
            let (re, im) = polar_to_rect(result_mag, result_angle, false);

            if push_complex(ctx, re, im).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @REAL_TO_COMPLEX (1 -> 1) "Coerce real to complex" {
            let r = match ctx.pop_real() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if push_complex(ctx, r, 0.0).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        @INT_TO_COMPLEX (1 -> 1) "Coerce int to complex" {
            let r = match ctx.pop_real() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if push_complex(ctx, r, 0.0).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }
    }

    operators {
        Add(Symmetric COMPLEX) -> CMD_ADD { commutative };
        Sub(Symmetric COMPLEX) -> CMD_SUB;
        Mul(Symmetric COMPLEX) -> CMD_MUL { commutative };
        Div(Symmetric COMPLEX) -> CMD_DIV;
        Neg(Unary COMPLEX) -> CMD_NEG;
        Inv(Unary COMPLEX) -> CMD_INV;
        Abs(Unary COMPLEX) -> CMD_ABS { result: REAL };
    }

    coercions {
        REAL -> COMPLEX -> CMD_REAL_TO_COMPLEX { implicit, priority: 5 };
        BINT -> COMPLEX -> CMD_INT_TO_COMPLEX { implicit, priority: 10 };
    }

    custom probe {
        let text = ctx.text();

        // Handle construct delimiters for (re, im) syntax
        // Only claim ( when NOT in infix/symbolic mode
        match text {
            "(" => {
                if !ctx.in_infix() {
                    return rpl_lang::library::ProbeResult::Match {
                        info: TokenInfo::open_bracket(1),
                        semantic: SemanticKind::Bracket,
                    };
                }
            }
            "," => {
                if ctx.construct() == Some(ConstructKind::Complex) {
                    return rpl_lang::library::ProbeResult::Match {
                        info: TokenInfo::comma(1),
                        semantic: SemanticKind::Operator,
                    };
                }
            }
            ")" => {
                if ctx.construct() == Some(ConstructKind::Complex) {
                    return rpl_lang::library::ProbeResult::Match {
                        info: TokenInfo::close_bracket(1),
                        semantic: SemanticKind::Bracket,
                    };
                }
            }
            _ => {}
        }

        // Check for algebraic complex literal (3+4i)
        if parse_algebraic(text).is_some() {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Number,
            };
        }

        // Check for complex commands
        if Self::command_id(text).is_some() {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        rpl_lang::library::ProbeResult::NoMatch
    }

    custom compile {
        let text = ctx.text();

        // Handle construct delimiters for (re, im) syntax
        match text {
            "(" => {
                return rpl_lang::library::CompileResult::StartConstruct {
                    kind: ConstructKind::Complex,
                };
            }
            "," => {
                if ctx.current_construct() == Some(ConstructKind::Complex) {
                    return rpl_lang::library::CompileResult::Ok;
                }
            }
            ")" => {
                if ctx.current_construct() == Some(ConstructKind::Complex) {
                    ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_R2C);
                    return rpl_lang::library::CompileResult::EndConstruct;
                }
            }
            _ => {}
        }

        // Check for algebraic complex literal (3+4i)
        if let Some((re, im, _, _)) = parse_algebraic(text) {
            ctx.emit_prolog(TypeId::COMPLEX.as_u16(), 4);
            let (re_hi, re_lo) = encode_f64(re);
            let (im_hi, im_lo) = encode_f64(im);
            ctx.emit(re_hi);
            ctx.emit(re_lo);
            ctx.emit(im_hi);
            ctx.emit(im_lo);
            return rpl_lang::library::CompileResult::Ok;
        }

        // Check for complex commands
        if let Some(cmd_id) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd_id);
            return rpl_lang::library::CompileResult::Ok;
        }

        rpl_lang::library::CompileResult::NoMatch
    }

    custom decompile_prolog {
        // Check for COMPLEX prolog
        if let Some(word) = ctx.peek()
            && rpl_core::is_prolog(word)
            && rpl_core::extract_type(word) == TypeId::COMPLEX.as_u16()
        {
            let size = rpl_core::extract_size(word) as usize;
            ctx.read(); // consume prolog

            if size == 4 {
                // Rectangular: (re, im)
                let re_hi = ctx.read().unwrap_or(0);
                let re_lo = ctx.read().unwrap_or(0);
                let im_hi = ctx.read().unwrap_or(0);
                let im_lo = ctx.read().unwrap_or(0);

                let re = decode_f64(re_hi, re_lo);
                let im = decode_f64(im_hi, im_lo);

                ctx.write(&format!("({}, {})", re, im));
                rpl_lang::library::DecompileResult::Ok
            } else if size == 5 {
                // Polar: (mag∠angle) or (mag∠angle°)
                let mag_hi = ctx.read().unwrap_or(0);
                let mag_lo = ctx.read().unwrap_or(0);
                let angle_hi = ctx.read().unwrap_or(0);
                let angle_lo = ctx.read().unwrap_or(0);
                let mode = ctx.read().unwrap_or(0) as u8;

                let mag = decode_f64(mag_hi, mag_lo);
                let angle = decode_f64(angle_hi, angle_lo);

                if mode == 1 {
                    ctx.write(&format!("({}∠{}°)", mag, angle));
                } else {
                    ctx.write(&format!("({}∠{})", mag, angle));
                }
                rpl_lang::library::DecompileResult::Ok
            } else {
                rpl_lang::library::DecompileResult::Unknown
            }
        } else {
            rpl_lang::library::DecompileResult::Unknown
        }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Encode an f64 as two u32 words.
fn encode_f64(v: f64) -> (u32, u32) {
    let bits = v.to_bits();
    ((bits >> 32) as u32, bits as u32)
}

/// Decode two u32 words as an f64.
fn decode_f64(hi: u32, lo: u32) -> f64 {
    let bits = ((hi as u64) << 32) | (lo as u64);
    f64::from_bits(bits)
}

/// Create a rectangular complex Value.
fn make_complex(re: f64, im: f64) -> rpl_lang::Value {
    let (re_hi, re_lo) = encode_f64(re);
    let (im_hi, im_lo) = encode_f64(im);
    rpl_lang::Value::Object {
        type_id: TypeId::COMPLEX,
        data: vec![re_hi, re_lo, im_hi, im_lo],
    }
}

/// Create a polar complex Value.
fn make_complex_polar(mag: f64, angle: f64, mode: u8) -> rpl_lang::Value {
    let (mag_hi, mag_lo) = encode_f64(mag);
    let (angle_hi, angle_lo) = encode_f64(angle);
    rpl_lang::Value::Object {
        type_id: TypeId::COMPLEX,
        data: vec![mag_hi, mag_lo, angle_hi, angle_lo, mode as u32],
    }
}

/// Extract complex components from a Value.
fn extract_complex(value: &rpl_lang::Value) -> Option<(f64, f64, bool, u8)> {
    match value {
        rpl_lang::Value::Object { type_id, data } if *type_id == TypeId::COMPLEX => {
            if data.len() == 4 {
                let re = decode_f64(data[0], data[1]);
                let im = decode_f64(data[2], data[3]);
                Some((re, im, false, 0))
            } else if data.len() == 5 {
                let mag = decode_f64(data[0], data[1]);
                let angle = decode_f64(data[2], data[3]);
                let mode = data[4] as u8;
                Some((mag, angle, true, mode))
            } else {
                None
            }
        }
        rpl_lang::Value::Real(r) => Some((*r, 0.0, false, 0)),
        rpl_lang::Value::Int(i) => Some((*i as f64, 0.0, false, 0)),
        _ => None,
    }
}

/// Pop a complex number from the context's stack.
fn pop_complex(ctx: &mut ExecuteContext) -> Result<(f64, f64, bool, u8), String> {
    let value = ctx.pop().map_err(|_| "Stack underflow".to_string())?;
    extract_complex(&value).ok_or_else(|| "Expected complex number".to_string())
}

/// Push a rectangular complex number onto the context's stack.
fn push_complex(ctx: &mut ExecuteContext, re: f64, im: f64) -> Result<(), String> {
    ctx.push(make_complex(re, im)).map_err(|_| "Stack overflow".to_string())
}

/// Push a polar complex number onto the context's stack.
fn push_complex_polar(ctx: &mut ExecuteContext, mag: f64, angle: f64, mode: u8) -> Result<(), String> {
    ctx.push(make_complex_polar(mag, angle, mode)).map_err(|_| "Stack overflow".to_string())
}

/// Convert polar to rectangular form.
fn polar_to_rect(mag: f64, angle: f64, degrees: bool) -> (f64, f64) {
    let angle_rad = if degrees { angle.to_radians() } else { angle };
    (mag * angle_rad.cos(), mag * angle_rad.sin())
}

/// Convert rectangular to polar form.
fn rect_to_polar(re: f64, im: f64) -> (f64, f64) {
    let mag = (re * re + im * im).sqrt();
    let angle = im.atan2(re);
    (mag, angle)
}

/// Parse algebraic notation: 3+4i, 3-4i, 4i, -4i, etc.
fn parse_algebraic(text: &str) -> Option<(f64, f64, bool, u8)> {
    let text = text.trim();

    // Must end with 'i' or 'I'
    if !text.ends_with('i') && !text.ends_with('I') {
        return None;
    }

    let without_i = &text[..text.len() - 1];

    // Pure imaginary: 4i, -4i, or just i
    if without_i.is_empty() {
        return Some((0.0, 1.0, false, 0));
    }
    if without_i == "-" {
        return Some((0.0, -1.0, false, 0));
    }
    if without_i == "+" {
        return Some((0.0, 1.0, false, 0));
    }

    // Check if it's just a number followed by i (pure imaginary)
    if let Ok(im) = without_i.parse::<f64>() {
        return Some((0.0, im, false, 0));
    }

    // Look for + or - in the middle (not at the start) to split re and im
    let bytes = without_i.as_bytes();
    let mut split_pos = None;

    for i in (1..bytes.len()).rev() {
        if bytes[i] == b'+' || bytes[i] == b'-' {
            if i > 0 && (bytes[i - 1] == b'e' || bytes[i - 1] == b'E') {
                continue;
            }
            split_pos = Some(i);
            break;
        }
    }

    if let Some(pos) = split_pos {
        let re_str = &without_i[..pos];
        let im_str = &without_i[pos..];

        let re: f64 = re_str.parse().ok()?;

        let im: f64 = if im_str == "+" {
            1.0
        } else if im_str == "-" {
            -1.0
        } else {
            im_str.parse().ok()?
        };

        return Some((re, im, false, 0));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_complex_construct_start() {
        let interner = Interner::new();
        let lib = ComplexLib;

        let ctx = make_probe_ctx("(", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_commands() {
        let interner = Interner::new();
        let lib = ComplexLib;

        for cmd in &["RE", "IM", "ARG", "CONJ", "C→R", "R→C"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                cmd
            );
        }
    }

    #[test]
    fn polar_to_rect_conversion() {
        let (re, im) = polar_to_rect(5.0, 0.9272952180016122, false);
        assert!((re - 3.0).abs() < 0.0001);
        assert!((im - 4.0).abs() < 0.0001);
    }

    #[test]
    fn rect_to_polar_conversion() {
        let (mag, angle) = rect_to_polar(3.0, 4.0);
        assert!((mag - 5.0).abs() < 0.0001);
        assert!((angle - 0.9272952180016122).abs() < 0.0001);
    }

    #[test]
    fn test_parse_algebraic_full() {
        assert_eq!(parse_algebraic("3+4i"), Some((3.0, 4.0, false, 0)));
        assert_eq!(parse_algebraic("3-4i"), Some((3.0, -4.0, false, 0)));
        assert_eq!(parse_algebraic("-3+4i"), Some((-3.0, 4.0, false, 0)));
        assert_eq!(parse_algebraic("-3-4i"), Some((-3.0, -4.0, false, 0)));
    }

    #[test]
    fn test_parse_algebraic_pure_imaginary() {
        assert_eq!(parse_algebraic("4i"), Some((0.0, 4.0, false, 0)));
        assert_eq!(parse_algebraic("-4i"), Some((0.0, -4.0, false, 0)));
        assert_eq!(parse_algebraic("i"), Some((0.0, 1.0, false, 0)));
        assert_eq!(parse_algebraic("-i"), Some((0.0, -1.0, false, 0)));
    }

    #[test]
    fn test_parse_algebraic_floats() {
        assert_eq!(parse_algebraic("1.5+2.5i"), Some((1.5, 2.5, false, 0)));
        assert_eq!(parse_algebraic("-1.5-2.5i"), Some((-1.5, -2.5, false, 0)));
    }

    #[test]
    fn test_parse_algebraic_coefficient_one() {
        assert_eq!(parse_algebraic("3+i"), Some((3.0, 1.0, false, 0)));
        assert_eq!(parse_algebraic("3-i"), Some((3.0, -1.0, false, 0)));
    }

    #[test]
    fn probe_algebraic() {
        let interner = Interner::new();
        let lib = ComplexLib;

        let ctx = make_probe_ctx("3+4i", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("4i", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("i", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }
}
