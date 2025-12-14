//! Literal codecs for RPL standard types.
//!
//! This module provides `LiteralCodec` implementations for the standard
//! literal types: real numbers, strings, binary integers, etc.

use rpl_core::{TypeId, token::SemanticKind};
use rpl_lang::library::LiteralCodec;

/// Codec for real number literals.
///
/// Handles parsing, encoding, and formatting of floating-point numbers.
/// Supports integers, decimals, and scientific notation.
///
/// # Examples
/// - `123` - integer
/// - `3.14` - decimal
/// - `-2.5e10` - scientific notation
pub struct RealCodec;

impl LiteralCodec for RealCodec {
    type Value = f64;

    fn probe(text: &str) -> Option<usize> {
        parse_real(text).map(|_| text.len())
    }

    fn parse(text: &str) -> Result<f64, String> {
        parse_real(text).ok_or_else(|| format!("Invalid number: {}", text))
    }

    fn encode(value: &f64) -> Vec<u32> {
        let bits = value.to_bits();
        vec![(bits >> 32) as u32, (bits & 0xFFFF_FFFF) as u32]
    }

    fn decode(words: &[u32]) -> Option<f64> {
        if words.len() >= 2 {
            let bits = ((words[0] as u64) << 32) | (words[1] as u64);
            Some(f64::from_bits(bits))
        } else {
            None
        }
    }

    fn format(value: &f64) -> String {
        format_real(*value)
    }

    fn type_id() -> TypeId {
        TypeId::REAL
    }

    fn semantic() -> SemanticKind {
        SemanticKind::Number
    }
}

/// Codec for string literals.
///
/// Handles parsing, encoding, and formatting of quoted strings with escape sequences.
///
/// # Examples
/// - `"hello"` - simple string
/// - `"with \"quotes\""` - escaped quotes
/// - `"line\nbreak"` - escape sequences
pub struct StringCodec;

impl LiteralCodec for StringCodec {
    type Value = String;

    fn probe(text: &str) -> Option<usize> {
        // Must start and end with double quotes
        if text.starts_with('"') && text.len() >= 2 {
            // Find closing quote, accounting for escapes
            let mut chars = text[1..].chars().peekable();
            let mut len = 1; // Opening quote
            while let Some(c) = chars.next() {
                len += c.len_utf8();
                if c == '\\' {
                    // Skip escaped character
                    if let Some(escaped) = chars.next() {
                        len += escaped.len_utf8();
                    }
                } else if c == '"' {
                    return Some(len);
                }
            }
        }
        None
    }

    fn parse(text: &str) -> Result<String, String> {
        parse_string_literal(text).ok_or_else(|| format!("Invalid string: {}", text))
    }

    fn encode(value: &String) -> Vec<u32> {
        encode_string(value)
    }

    fn decode(words: &[u32]) -> Option<String> {
        decode_string(words)
    }

    fn format(value: &String) -> String {
        format_string(value)
    }

    fn type_id() -> TypeId {
        TypeId::STRING
    }

    fn semantic() -> SemanticKind {
        SemanticKind::String
    }
}

/// Codec for binary integer literals.
///
/// Handles parsing, encoding, and formatting of integers in various bases.
///
/// # Examples
/// - `#1010b` - binary
/// - `#FFh` - hexadecimal
/// - `#777o` - octal
/// - `#123d` or `#123` - decimal
pub struct BintCodec;

impl LiteralCodec for BintCodec {
    type Value = i64;

    fn probe(text: &str) -> Option<usize> {
        if text.starts_with('#') && text.len() >= 2 {
            parse_binary_int(text).map(|_| text.len())
        } else {
            None
        }
    }

    fn parse(text: &str) -> Result<i64, String> {
        parse_binary_int(text).ok_or_else(|| format!("Invalid binary integer: {}", text))
    }

    fn encode(value: &i64) -> Vec<u32> {
        // Store as two 32-bit words (high, low)
        vec![(*value >> 32) as u32, (*value & 0xFFFF_FFFF) as u32]
    }

    fn decode(words: &[u32]) -> Option<i64> {
        if words.len() >= 2 {
            let bits = ((words[0] as u64) << 32) | (words[1] as u64);
            Some(bits as i64)
        } else {
            None
        }
    }

    fn format(value: &i64) -> String {
        // Default to hex format
        format!("#{:X}h", value)
    }

    fn type_id() -> TypeId {
        TypeId::BINT
    }

    fn semantic() -> SemanticKind {
        SemanticKind::Number
    }
}

// =============================================================================
// Helper functions
// =============================================================================

/// Parse a string as a real number.
pub fn parse_real(text: &str) -> Option<f64> {
    let text = text.trim();

    // Try standard float parsing
    if let Ok(value) = text.parse::<f64>() {
        // Check it's a valid number (not infinity or NaN from weird input)
        if value.is_finite()
            || text.eq_ignore_ascii_case("inf")
            || text.eq_ignore_ascii_case("-inf")
        {
            return Some(value);
        }
    }

    // Handle integers
    if let Ok(value) = text.parse::<i64>() {
        return Some(value as f64);
    }

    None
}

/// Format a real number for display.
pub fn format_real(value: f64) -> String {
    if value.fract() == 0.0 && value.abs() < 1e15 {
        // Display as integer if it's a whole number
        format!("{}", value as i64)
    } else {
        // Use default float formatting
        format!("{}", value)
    }
}

/// Parse a quoted string literal with escape sequences.
pub fn parse_string_literal(text: &str) -> Option<String> {
    if !text.starts_with('"') || !text.ends_with('"') || text.len() < 2 {
        return None;
    }

    let inner = &text[1..text.len() - 1];
    let mut result = String::with_capacity(inner.len());
    let mut chars = inner.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    Some(result)
}

/// Encode a string to bytecode words.
///
/// Format: UTF-8 bytes packed into u32s (little-endian), null-terminated.
/// The VM uses the prolog size to know how many words, and null-termination
/// to find the actual string length.
pub fn encode_string(s: &str) -> Vec<u32> {
    let bytes = s.as_bytes();
    // Add 1 for null terminator, then round up to words
    let total_bytes = bytes.len() + 1;
    let num_words = total_bytes.div_ceil(4);
    let mut words = Vec::with_capacity(num_words);

    // Pack bytes into words (4 bytes per word, little-endian)
    // Null terminator is implicitly added via zero-padding
    for chunk in bytes.chunks(4) {
        let mut word = 0u32;
        for (i, &b) in chunk.iter().enumerate() {
            word |= (b as u32) << (i * 8);
        }
        words.push(word);
    }

    // If the string length is a multiple of 4, we need an extra word for the null
    if bytes.len() % 4 == 0 {
        words.push(0);
    }

    words
}

/// Decode bytecode words to a string.
///
/// Format: UTF-8 bytes packed into u32s (little-endian), null-terminated.
pub fn decode_string(words: &[u32]) -> Option<String> {
    if words.is_empty() {
        return Some(String::new());
    }

    let mut bytes = Vec::with_capacity(words.len() * 4);

    for &word in words {
        bytes.push((word & 0xFF) as u8);
        bytes.push(((word >> 8) & 0xFF) as u8);
        bytes.push(((word >> 16) & 0xFF) as u8);
        bytes.push(((word >> 24) & 0xFF) as u8);
    }

    // Find null terminator
    let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    bytes.truncate(len);
    String::from_utf8(bytes).ok()
}

/// Format a string for display (with quotes and escapes).
pub fn format_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result.push('"');
    result
}

/// Parse a binary integer literal (#FFh, #1010b, etc).
///
/// Formats:
/// - `#digits` -> hex (default)
/// - `#digitsb` -> binary (only valid if digits are 0/1)
/// - `#digitso` -> octal (only valid if digits are 0-7)
/// - `#digitsd` -> decimal
/// - `#digitsh` -> hex (explicit)
pub fn parse_binary_int(text: &str) -> Option<i64> {
    // Must start with #
    let text = text.strip_prefix('#')?;

    if text.is_empty() {
        return None;
    }

    // Try binary suffix (b/B) - only if all preceding chars are 0 or 1
    if let Some(d) = text
        .strip_suffix('b')
        .or_else(|| text.strip_suffix('B'))
        .filter(|d| !d.is_empty() && d.chars().all(|c| c == '0' || c == '1'))
    {
        return u64::from_str_radix(d, 2).ok().map(|v| v as i64);
    }

    // Try octal suffix (o/O) - only if all preceding chars are 0-7
    if let Some(d) = text
        .strip_suffix('o')
        .or_else(|| text.strip_suffix('O'))
        .filter(|d| !d.is_empty() && d.chars().all(|c| c.is_digit(8)))
    {
        return u64::from_str_radix(d, 8).ok().map(|v| v as i64);
    }

    // Try decimal suffix (d/D) - only if all preceding chars are 0-9
    if let Some(d) = text
        .strip_suffix('d')
        .or_else(|| text.strip_suffix('D'))
        .filter(|d| !d.is_empty() && d.chars().all(|c| c.is_ascii_digit()))
    {
        return d.parse::<u64>().ok().map(|v| v as i64);
    }

    // Try hex suffix (h/H) - only if all preceding chars are hex digits
    if let Some(d) = text
        .strip_suffix('h')
        .or_else(|| text.strip_suffix('H'))
        .filter(|d| !d.is_empty() && d.chars().all(|c| c.is_ascii_hexdigit()))
    {
        return u64::from_str_radix(d, 16).ok().map(|v| v as i64);
    }

    // No suffix - default to hex
    u64::from_str_radix(text, 16).ok().map(|v| v as i64)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Real codec tests
    #[test]
    fn real_probe() {
        assert_eq!(RealCodec::probe("123"), Some(3));
        assert_eq!(RealCodec::probe("-45.67"), Some(6));
        assert_eq!(RealCodec::probe("1.5e10"), Some(6));
        assert_eq!(RealCodec::probe("abc"), None);
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn real_roundtrip() {
        let values = [0.0, 1.0, -1.0, 3.14159, 1e100, -1e-100];
        for &v in &values {
            let words = RealCodec::encode(&v);
            let decoded = RealCodec::decode(&words).unwrap();
            assert_eq!(v, decoded);
        }
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn real_format() {
        assert_eq!(RealCodec::format(&42.0), "42");
        assert_eq!(RealCodec::format(&3.14), "3.14");
    }

    // String codec tests
    #[test]
    fn string_probe() {
        assert_eq!(StringCodec::probe("\"hello\""), Some(7));
        assert_eq!(StringCodec::probe("\"with \\\"quotes\\\"\""), Some(17));
        assert_eq!(StringCodec::probe("hello"), None);
        assert_eq!(StringCodec::probe("\"unclosed"), None);
    }

    #[test]
    fn string_parse() {
        assert_eq!(StringCodec::parse("\"hello\""), Ok("hello".to_string()));
        assert_eq!(
            StringCodec::parse("\"line\\nbreak\""),
            Ok("line\nbreak".to_string())
        );
        assert_eq!(
            StringCodec::parse("\"with \\\"quotes\\\"\""),
            Ok("with \"quotes\"".to_string())
        );
    }

    #[test]
    fn string_roundtrip() {
        let values = ["", "hello", "with spaces", "line\nbreak", "quote\"here"];
        for s in values {
            let words = StringCodec::encode(&s.to_string());
            let decoded = StringCodec::decode(&words).unwrap();
            assert_eq!(s, decoded);
        }
    }

    #[test]
    fn string_format() {
        assert_eq!(StringCodec::format(&"hello".to_string()), "\"hello\"");
        assert_eq!(
            StringCodec::format(&"line\nbreak".to_string()),
            "\"line\\nbreak\""
        );
    }

    // Binary integer codec tests
    #[test]
    fn bint_probe() {
        assert_eq!(BintCodec::probe("#FF"), Some(3));
        assert_eq!(BintCodec::probe("#FFh"), Some(4));
        assert_eq!(BintCodec::probe("#1010b"), Some(6));
        assert_eq!(BintCodec::probe("#777o"), Some(5));
        assert_eq!(BintCodec::probe("123"), None);
    }

    #[test]
    fn bint_parse() {
        assert_eq!(BintCodec::parse("#FF"), Ok(255));
        assert_eq!(BintCodec::parse("#FFh"), Ok(255));
        assert_eq!(BintCodec::parse("#1010b"), Ok(10));
        assert_eq!(BintCodec::parse("#777o"), Ok(511));
        assert_eq!(BintCodec::parse("#123d"), Ok(123));
    }

    #[test]
    fn bint_roundtrip() {
        let values = [0i64, 1, -1, 255, 0xDEADBEEF, -0x7FFFFFFF];
        for &v in &values {
            let words = BintCodec::encode(&v);
            let decoded = BintCodec::decode(&words).unwrap();
            assert_eq!(v, decoded);
        }
    }
}
