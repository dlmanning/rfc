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
use rpl_lang::library::{
    CompileContext, CompileResult, ConstructKind, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};

/// Library for complex number operations.
pub struct ComplexLib;

impl ComplexLib {
    /// Library ID for complex numbers (same as TypeId::COMPLEX).
    pub const ID: LibraryId = LibraryId::new(30);

    // Command IDs for complex operations
    const CMD_RE: u16 = 0;
    const CMD_IM: u16 = 1;
    const CMD_ARG: u16 = 2;
    const CMD_ABS: u16 = 3;
    const CMD_CONJ: u16 = 4;
    const CMD_C2R: u16 = 5;   // C→R
    const CMD_R2C: u16 = 6;   // R→C
    const CMD_TOPOLAR: u16 = 7;  // →POLAR
    const CMD_TORECT: u16 = 8;   // →RECT
    const CMD_NEG: u16 = 9;
    const CMD_INV: u16 = 10;
    const CMD_ADD: u16 = 11;
    const CMD_SUB: u16 = 12;
    const CMD_MUL: u16 = 13;
    const CMD_DIV: u16 = 14;
    // Coercion commands
    const CMD_REAL_TO_COMPLEX: u16 = 50;
    const CMD_INT_TO_COMPLEX: u16 = 51;

    /// Get command ID from name.
    fn command_id(text: &str) -> Option<u16> {
        let upper = text.to_ascii_uppercase();
        match upper.as_str() {
            "RE" => Some(Self::CMD_RE),
            "IM" => Some(Self::CMD_IM),
            "ARG" => Some(Self::CMD_ARG),
            "CONJ" => Some(Self::CMD_CONJ),
            "C→R" | "C->R" | "C2R" => Some(Self::CMD_C2R),
            "R→C" | "R->C" | "R2C" => Some(Self::CMD_R2C),
            "→POLAR" | "->POLAR" | "TOPOLAR" => Some(Self::CMD_TOPOLAR),
            "→RECT" | "->RECT" | "TORECT" => Some(Self::CMD_TORECT),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_RE => Some("RE"),
            Self::CMD_IM => Some("IM"),
            Self::CMD_ARG => Some("ARG"),
            Self::CMD_ABS => Some("ABS"),
            Self::CMD_CONJ => Some("CONJ"),
            Self::CMD_C2R => Some("C→R"),
            Self::CMD_R2C => Some("R→C"),
            Self::CMD_TOPOLAR => Some("→POLAR"),
            Self::CMD_TORECT => Some("→RECT"),
            Self::CMD_NEG => Some("NEG"),
            Self::CMD_INV => Some("INV"),
            Self::CMD_ADD => Some("+"),
            Self::CMD_SUB => Some("-"),
            Self::CMD_MUL => Some("*"),
            Self::CMD_DIV => Some("/"),
            _ => None,
        }
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
        // Handle: 3+4i, 3-4i, -3+4i, -3-4i
        let bytes = without_i.as_bytes();
        let mut split_pos = None;

        // Search from the end backwards, skip the first character (could be sign of real part)
        for i in (1..bytes.len()).rev() {
            if bytes[i] == b'+' || bytes[i] == b'-' {
                // Make sure this isn't part of scientific notation (e.g., 1e+10)
                if i > 0 && (bytes[i - 1] == b'e' || bytes[i - 1] == b'E') {
                    continue;
                }
                split_pos = Some(i);
                break;
            }
        }

        if let Some(pos) = split_pos {
            let re_str = &without_i[..pos];
            let im_str = &without_i[pos..]; // includes the sign

            let re: f64 = re_str.parse().ok()?;

            // im_str is like "+4" or "-4" or "+" or "-"
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
}

// Helper functions for encoding/decoding f64 as two Words
fn encode_f64(v: f64) -> (u32, u32) {
    let bits = v.to_bits();
    ((bits >> 32) as u32, bits as u32)
}

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
/// Returns Some((c1, c2, is_polar, mode)) or None if not a complex number.
fn extract_complex(value: &rpl_lang::Value) -> Option<(f64, f64, bool, u8)> {
    match value {
        rpl_lang::Value::Object { type_id, data } if *type_id == TypeId::COMPLEX => {
            if data.len() == 4 {
                // Rectangular
                let re = decode_f64(data[0], data[1]);
                let im = decode_f64(data[2], data[3]);
                Some((re, im, false, 0))
            } else if data.len() == 5 {
                // Polar
                let mag = decode_f64(data[0], data[1]);
                let angle = decode_f64(data[2], data[3]);
                let mode = data[4] as u8;
                Some((mag, angle, true, mode))
            } else {
                None
            }
        }
        // Auto-coerce real to complex
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

impl Library for ComplexLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Complex"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        // Handle construct delimiters for (re, im) syntax
        // Only claim ( when NOT in infix/symbolic mode
        // In infix mode, ( is grouping handled by symbolic library
        match text {
            "(" => {
                if !ctx.in_infix() {
                    // Opening paren starts a complex construct
                    return ProbeResult::Match {
                        info: TokenInfo::open_bracket(1),
                        semantic: SemanticKind::Bracket,
                    };
                }
            }
            "," => {
                // Comma is a separator inside complex construct
                if ctx.construct() == Some(ConstructKind::Complex) {
                    return ProbeResult::Match {
                        info: TokenInfo::comma(1),
                        semantic: SemanticKind::Operator,
                    };
                }
            }
            ")" => {
                // Closing paren ends complex construct
                if ctx.construct() == Some(ConstructKind::Complex) {
                    return ProbeResult::Match {
                        info: TokenInfo::close_bracket(1),
                        semantic: SemanticKind::Bracket,
                    };
                }
            }
            _ => {}
        }

        // Check for algebraic complex literal (3+4i)
        if Self::parse_algebraic(text).is_some() {
            return ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Number,
            };
        }

        // Check for complex commands
        if Self::command_id(text).is_some() {
            return ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        ProbeResult::NoMatch
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        // Handle construct delimiters for (re, im) syntax
        match text {
            "(" => {
                // Start complex construct - compiler will emit COMPLEX prolog
                return CompileResult::StartConstruct {
                    kind: ConstructKind::Complex,
                };
            }
            "," => {
                // Comma separator inside complex - just consume it
                if ctx.current_construct() == Some(ConstructKind::Complex) {
                    return CompileResult::Ok;
                }
            }
            ")" => {
                // End complex construct - emit R→C to create complex from stack values
                if ctx.current_construct() == Some(ConstructKind::Complex) {
                    // Emit R→C command (pops re, im from stack and creates complex)
                    ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_R2C);
                    return CompileResult::EndConstruct;
                }
            }
            _ => {}
        }

        // Check for algebraic complex literal (3+4i)
        if let Some((re, im, _, _)) = Self::parse_algebraic(text) {
            // Algebraic form is always rectangular
            ctx.emit_prolog(TypeId::COMPLEX.as_u16(), 4);
            let (re_hi, re_lo) = encode_f64(re);
            let (im_hi, im_lo) = encode_f64(im);
            ctx.emit(re_hi);
            ctx.emit(re_lo);
            ctx.emit(im_hi);
            ctx.emit(im_lo);
            return CompileResult::Ok;
        }

        // Check for complex commands
        if let Some(cmd_id) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd_id);
            return CompileResult::Ok;
        }

        CompileResult::NoMatch
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_RE => {
                // RE: Extract real part
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let re = if is_polar {
                            let (re, _) = polar_to_rect(c1, c2, mode == 1);
                            re
                        } else {
                            c1
                        };
                        if ctx.push_real(re).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_IM => {
                // IM: Extract imaginary part
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let im = if is_polar {
                            let (_, im) = polar_to_rect(c1, c2, mode == 1);
                            im
                        } else {
                            c2
                        };
                        if ctx.push_real(im).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_ARG => {
                // ARG: Extract argument (angle) in radians
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let angle = if is_polar {
                            if mode == 1 { c2.to_radians() } else { c2 }
                        } else {
                            c2.atan2(c1)
                        };
                        if ctx.push_real(angle).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_ABS => {
                // ABS: Magnitude
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, _mode)) => {
                        let mag = if is_polar {
                            c1.abs() // magnitude is already c1 in polar
                        } else {
                            (c1 * c1 + c2 * c2).sqrt()
                        };
                        if ctx.push_real(mag).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_CONJ => {
                // CONJ: Complex conjugate
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        if is_polar {
                            // Conjugate in polar: negate the angle
                            if push_complex_polar(ctx,c1, -c2, mode).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        } else {
                            // Conjugate in rectangular: negate imaginary
                            if push_complex(ctx,c1, -c2).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_C2R => {
                // C→R: Split complex to two reals
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let (re, im) = if is_polar {
                            polar_to_rect(c1, c2, mode == 1)
                        } else {
                            (c1, c2)
                        };
                        if ctx.push_real(re).is_err() || ctx.push_real(im).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_R2C => {
                // R→C: Combine two reals to complex
                // Used both by explicit R→C command and by (re, im) syntax
                let im = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => {
                        return ExecuteResult::Error(
                            "R→C requires two numeric values (got underflow or type error for imaginary part)".to_string()
                        );
                    }
                };
                let re = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => {
                        return ExecuteResult::Error(
                            "R→C requires two numeric values (got underflow or type error for real part)".to_string()
                        );
                    }
                };
                if push_complex(ctx, re, im).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_TOPOLAR => {
                // →POLAR: Convert to polar form (radians)
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let (mag, angle) = if is_polar {
                            // Already polar, just ensure radians
                            let angle_rad = if mode == 1 { c2.to_radians() } else { c2 };
                            (c1, angle_rad)
                        } else {
                            rect_to_polar(c1, c2)
                        };
                        if push_complex_polar(ctx,mag, angle, 0).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_TORECT => {
                // →RECT: Convert to rectangular form
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        let (re, im) = if is_polar {
                            polar_to_rect(c1, c2, mode == 1)
                        } else {
                            (c1, c2)
                        };
                        if push_complex(ctx,re, im).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_NEG => {
                // NEG: Negate complex number
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        if is_polar {
                            // Negate in polar: add π to angle
                            let new_angle = c2 + if mode == 1 { 180.0 } else { std::f64::consts::PI };
                            if push_complex_polar(ctx, c1, new_angle, mode).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        } else if push_complex(ctx, -c1, -c2).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_INV => {
                // INV: Inverse (1/z)
                match pop_complex(ctx) {
                    Ok((c1, c2, is_polar, mode)) => {
                        if is_polar {
                            // Inverse in polar: 1/mag, negate angle
                            if c1 == 0.0 {
                                return ExecuteResult::Error("Division by zero".to_string());
                            }
                            if push_complex_polar(ctx,1.0 / c1, -c2, mode).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        } else {
                            // Inverse in rectangular: conj(z) / |z|^2
                            let mag_sq = c1 * c1 + c2 * c2;
                            if mag_sq == 0.0 {
                                return ExecuteResult::Error("Division by zero".to_string());
                            }
                            if push_complex(ctx,c1 / mag_sq, -c2 / mag_sq).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Expected complex number".to_string()),
                }
            }
            Self::CMD_ADD => {
                // ADD: Complex addition
                let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Convert both to rectangular for addition
                let (a_re, a_im) = if a_polar { polar_to_rect(a1, a2, a_mode == 1) } else { (a1, a2) };
                let (b_re, b_im) = if b_polar { polar_to_rect(b1, b2, b_mode == 1) } else { (b1, b2) };

                if push_complex(ctx,a_re + b_re, a_im + b_im).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_SUB => {
                // SUB: Complex subtraction
                let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let (a_re, a_im) = if a_polar { polar_to_rect(a1, a2, a_mode == 1) } else { (a1, a2) };
                let (b_re, b_im) = if b_polar { polar_to_rect(b1, b2, b_mode == 1) } else { (b1, b2) };

                if push_complex(ctx,a_re - b_re, a_im - b_im).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_MUL => {
                // MUL: Complex multiplication
                let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Multiplication is easier in polar form
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

                // r1*r2 ∠ (θ1+θ2), then convert back to rectangular
                let result_mag = a_mag * b_mag;
                let result_angle = a_angle + b_angle;
                let (re, im) = polar_to_rect(result_mag, result_angle, false);

                if push_complex(ctx,re, im).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_DIV => {
                // DIV: Complex division
                let (b1, b2, b_polar, b_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let (a1, a2, a_polar, a_mode) = match pop_complex(ctx) {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Division is easier in polar form
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
                    return ExecuteResult::Error("Division by zero".to_string());
                }

                // r1/r2 ∠ (θ1-θ2), then convert back to rectangular
                let result_mag = a_mag / b_mag;
                let result_angle = a_angle - b_angle;
                let (re, im) = polar_to_rect(result_mag, result_angle, false);

                if push_complex(ctx,re, im).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_REAL_TO_COMPLEX | Self::CMD_INT_TO_COMPLEX => {
                // Coerce real/int to complex
                let r = match ctx.pop_real() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if push_complex(ctx,r, 0.0).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown complex command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
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
                        DecompileResult::Ok
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
                        DecompileResult::Ok
                    } else {
                        DecompileResult::Unknown
                    }
                } else {
                    DecompileResult::Unknown
                }
            }
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        // Construct delimiters
        match token {
            "(" => return StackEffect::StartConstruct,
            ")" => return StackEffect::EndConstruct,
            "," => return StackEffect::Dynamic, // No stack effect, just separator
            _ => {}
        }

        // Algebraic complex literals produce one value
        if Self::parse_algebraic(token).is_some() {
            return StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            };
        }

        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            "RE" | "IM" | "ARG" | "CONJ" | "→POLAR" | "->POLAR" | "TOPOLAR"
            | "→RECT" | "->RECT" | "TORECT" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "C→R" | "C->R" | "C2R" => StackEffect::Fixed {
                consumes: 1,
                produces: 2,
            },
            "R→C" | "R->C" | "R2C" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            _ => StackEffect::Dynamic,
        }
    }

    fn register_operators(&self, registry: &mut rpl_lang::operator::OperatorRegistry) {
        use rpl_lang::operator::{OperatorKind, OpSignature, OperatorRegistration, CoercionRegistration};

        // Register binary operators for Complex type
        let binary_ops = [
            (OperatorKind::Add, Self::CMD_ADD, true),  // commutative
            (OperatorKind::Sub, Self::CMD_SUB, false),
            (OperatorKind::Mul, Self::CMD_MUL, true),  // commutative
            (OperatorKind::Div, Self::CMD_DIV, false),
        ];

        for (kind, cmd, commutative) in binary_ops {
            registry.register_operator(OperatorRegistration {
                op: kind,
                signature: OpSignature::Symmetric(TypeId::COMPLEX),
                result: TypeId::COMPLEX,
                lib: Self::ID,
                command: cmd,
                priority: 100,
                commutative,
            });
        }

        // Register unary operators for Complex type
        let unary_ops = [
            (OperatorKind::Neg, Self::CMD_NEG),
            (OperatorKind::Inv, Self::CMD_INV),
            (OperatorKind::Abs, Self::CMD_ABS),
        ];

        for (kind, cmd) in unary_ops {
            // Note: ABS returns Real (magnitude), not Complex
            let result = if kind == OperatorKind::Abs {
                TypeId::REAL
            } else {
                TypeId::COMPLEX
            };

            registry.register_operator(OperatorRegistration {
                op: kind,
                signature: OpSignature::Unary(TypeId::COMPLEX),
                result,
                lib: Self::ID,
                command: cmd,
                priority: 100,
                commutative: false,
            });
        }

        // Register coercions: Real -> Complex, Int -> Complex
        registry.register_coercion(CoercionRegistration {
            from: TypeId::REAL,
            to: TypeId::COMPLEX,
            lib: Self::ID,
            command: Self::CMD_REAL_TO_COMPLEX,
            implicit: true,
            priority: 5,
        });

        registry.register_coercion(CoercionRegistration {
            from: TypeId::BINT,
            to: TypeId::COMPLEX,
            lib: Self::ID,
            command: Self::CMD_INT_TO_COMPLEX,
            implicit: true,
            priority: 10,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    // Note: Rectangular "(3, 4)" and polar "(5∠0.927)" forms are now parsed
    // via the construct-based approach, tested by integration tests in tests/pipeline.rs

    #[test]
    fn probe_complex_construct_start() {
        let interner = Interner::new();
        let lib = ComplexLib;

        // Opening paren starts a complex construct
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
    fn parse_algebraic_full() {
        // 3+4i
        let result = ComplexLib::parse_algebraic("3+4i");
        assert_eq!(result, Some((3.0, 4.0, false, 0)));

        // 3-4i
        let result = ComplexLib::parse_algebraic("3-4i");
        assert_eq!(result, Some((3.0, -4.0, false, 0)));

        // -3+4i
        let result = ComplexLib::parse_algebraic("-3+4i");
        assert_eq!(result, Some((-3.0, 4.0, false, 0)));

        // -3-4i
        let result = ComplexLib::parse_algebraic("-3-4i");
        assert_eq!(result, Some((-3.0, -4.0, false, 0)));
    }

    #[test]
    fn parse_algebraic_pure_imaginary() {
        // 4i
        let result = ComplexLib::parse_algebraic("4i");
        assert_eq!(result, Some((0.0, 4.0, false, 0)));

        // -4i
        let result = ComplexLib::parse_algebraic("-4i");
        assert_eq!(result, Some((0.0, -4.0, false, 0)));

        // i
        let result = ComplexLib::parse_algebraic("i");
        assert_eq!(result, Some((0.0, 1.0, false, 0)));

        // -i
        let result = ComplexLib::parse_algebraic("-i");
        assert_eq!(result, Some((0.0, -1.0, false, 0)));
    }

    #[test]
    fn parse_algebraic_floats() {
        // 1.5+2.5i
        let result = ComplexLib::parse_algebraic("1.5+2.5i");
        assert_eq!(result, Some((1.5, 2.5, false, 0)));

        // -1.5-2.5i
        let result = ComplexLib::parse_algebraic("-1.5-2.5i");
        assert_eq!(result, Some((-1.5, -2.5, false, 0)));
    }

    #[test]
    fn parse_algebraic_coefficient_one() {
        // 3+i (coefficient 1 implied)
        let result = ComplexLib::parse_algebraic("3+i");
        assert_eq!(result, Some((3.0, 1.0, false, 0)));

        // 3-i (coefficient 1 implied)
        let result = ComplexLib::parse_algebraic("3-i");
        assert_eq!(result, Some((3.0, -1.0, false, 0)));
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
