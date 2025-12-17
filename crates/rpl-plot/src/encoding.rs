//! Byte stream encoding for plot objects.
//!
//! # Number Encoding
//!
//! Numbers use a variable-length format:
//!
//! ```text
//! Byte 0: SLLL_NNNN
//!   S = sign bit (1 = negative)
//!   L = length (0-4 additional bytes)
//!   N = low nibble of value
//!
//! Bytes 1-N: remaining bytes in big-endian order
//! ```
//!
//! | Absolute Value Range | Total Bytes |
//! |---------------------|-------------|
//! | 0 - 7 | 1 |
//! | 8 - 2,047 | 2 |
//! | 2,048 - 524,287 | 3 |
//! | 524,288 - 134,217,727 | 4 |
//! | 134,217,728+ | 5 |
//!
//! # Fixed-Point Coordinates
//!
//! Coordinates use Q16.16 fixed-point format:
//! - 16 bits integer part, 16 bits fractional
//! - Range: approximately -32768.0 to 32767.99998
//! - Precision: ~0.000015 (1/65536)
//!
//! # String Encoding
//!
//! Strings use a 0x5L prefix format:
//! - `0x50-0x5E`: inline length (0-14 bytes), followed by string bytes
//! - `0x5F`: extended length, followed by encoded length, then string bytes

/// Fixed-point shift amount (Q16.16 format)
const FP_SHIFT: i32 = 16;
const FP_SCALE: f64 = (1 << FP_SHIFT) as f64;

/// Convert a floating-point coordinate to Q16.16 fixed-point.
///
/// # Example
/// ```
/// use rpl_plot::to_fixed_point;
/// assert_eq!(to_fixed_point(1.0), 65536);
/// assert_eq!(to_fixed_point(0.5), 32768);
/// ```
pub fn to_fixed_point(f: f64) -> i32 {
    (f * FP_SCALE).round() as i32
}

/// Convert Q16.16 fixed-point back to floating-point.
///
/// # Example
/// ```
/// use rpl_plot::from_fixed_point;
/// assert_eq!(from_fixed_point(65536), 1.0);
/// assert_eq!(from_fixed_point(32768), 0.5);
/// ```
pub fn from_fixed_point(fp: i32) -> f64 {
    fp as f64 / FP_SCALE
}

/// Encode a signed integer using variable-length format.
///
/// # Example
/// ```
/// use rpl_plot::encode_number;
/// assert_eq!(encode_number(0), vec![0x00]);
/// assert_eq!(encode_number(7), vec![0x07]);
/// assert_eq!(encode_number(-1), vec![0x81]);
/// ```
pub fn encode_number(n: i64) -> Vec<u8> {
    let sign = if n < 0 { 0x80u8 } else { 0x00u8 };
    let abs = n.unsigned_abs();

    if abs <= 0x07 {
        vec![sign | (abs as u8) & 0x0F]
    } else if abs <= 0x07FF {
        vec![sign | 0x10 | ((abs >> 8) as u8) & 0x0F, abs as u8]
    } else if abs <= 0x07_FFFF {
        vec![
            sign | 0x20 | ((abs >> 16) as u8) & 0x0F,
            (abs >> 8) as u8,
            abs as u8,
        ]
    } else if abs <= 0x07FF_FFFF {
        vec![
            sign | 0x30 | ((abs >> 24) as u8) & 0x0F,
            (abs >> 16) as u8,
            (abs >> 8) as u8,
            abs as u8,
        ]
    } else {
        vec![
            sign | 0x40 | ((abs >> 32) as u8) & 0x0F,
            (abs >> 24) as u8,
            (abs >> 16) as u8,
            (abs >> 8) as u8,
            abs as u8,
        ]
    }
}

/// Encode a string using 0x5L prefix format.
///
/// # Example
/// ```
/// use rpl_plot::encode_string;
/// assert_eq!(encode_string("hi"), vec![0x52, b'h', b'i']);
/// ```
pub fn encode_string(s: &str) -> Vec<u8> {
    let bytes = s.as_bytes();
    let len = bytes.len();

    if len <= 14 {
        let mut result = vec![0x50 | (len as u8)];
        result.extend_from_slice(bytes);
        result
    } else {
        let mut result = vec![0x5F];
        result.extend(encode_number(len as i64));
        result.extend_from_slice(bytes);
        result
    }
}

/// Decode a variable-length number from bytes.
///
/// Returns `(value, bytes_consumed)` on success.
///
/// # Example
/// ```
/// use rpl_plot::{encode_number, decode_number};
/// let encoded = encode_number(42);
/// let (value, len) = decode_number(&encoded).unwrap();
/// assert_eq!(value, 42);
/// ```
pub fn decode_number(bytes: &[u8]) -> Option<(i64, usize)> {
    if bytes.is_empty() {
        return None;
    }

    let first = bytes[0];
    let sign = (first & 0x80) != 0;
    let len = ((first >> 4) & 0x07) as usize;
    let low_nibble = (first & 0x0F) as u64;

    if bytes.len() < len + 1 {
        return None;
    }

    let abs = match len {
        0 => low_nibble,
        1 => (low_nibble << 8) | bytes[1] as u64,
        2 => (low_nibble << 16) | (bytes[1] as u64) << 8 | bytes[2] as u64,
        3 => {
            (low_nibble << 24) | (bytes[1] as u64) << 16 | (bytes[2] as u64) << 8 | bytes[3] as u64
        }
        4 => {
            (low_nibble << 32)
                | (bytes[1] as u64) << 24
                | (bytes[2] as u64) << 16
                | (bytes[3] as u64) << 8
                | bytes[4] as u64
        }
        _ => return None,
    };

    let value = if sign { -(abs as i64) } else { abs as i64 };
    Some((value, len + 1))
}

/// Decode a string from bytes.
///
/// Returns `(string, bytes_consumed)` on success.
///
/// # Example
/// ```
/// use rpl_plot::{encode_string, decode_string};
/// let encoded = encode_string("hello");
/// let (s, len) = decode_string(&encoded).unwrap();
/// assert_eq!(s, "hello");
/// ```
pub fn decode_string(bytes: &[u8]) -> Option<(String, usize)> {
    if bytes.is_empty() {
        return None;
    }

    let first = bytes[0];
    if (first & 0xF0) != 0x50 {
        return None;
    }

    let (len, header_size) = if first == 0x5F {
        // Extended length
        let (n, consumed) = decode_number(&bytes[1..])?;
        (n as usize, 1 + consumed)
    } else {
        // Inline length
        ((first & 0x0F) as usize, 1)
    };

    if bytes.len() < header_size + len {
        return None;
    }

    let s = std::str::from_utf8(&bytes[header_size..header_size + len]).ok()?;
    Some((s.to_string(), header_size + len))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_number_roundtrip_small() {
        for n in -7..=7 {
            let encoded = encode_number(n);
            assert_eq!(encoded.len(), 1, "small number {n} should be 1 byte");
            let (decoded, consumed) = decode_number(&encoded).unwrap();
            assert_eq!(decoded, n);
            assert_eq!(consumed, 1);
        }
    }

    #[test]
    fn test_number_roundtrip_medium() {
        for n in [8, 100, 255, 1000, 2047, -8, -100, -2047] {
            let encoded = encode_number(n);
            assert_eq!(encoded.len(), 2, "medium number {n} should be 2 bytes");
            let (decoded, _) = decode_number(&encoded).unwrap();
            assert_eq!(decoded, n);
        }
    }

    #[test]
    fn test_number_roundtrip_large() {
        for n in [2048, 10000, 524287, -2048, -524287] {
            let encoded = encode_number(n);
            assert_eq!(encoded.len(), 3, "large number {n} should be 3 bytes");
            let (decoded, _) = decode_number(&encoded).unwrap();
            assert_eq!(decoded, n);
        }
    }

    #[test]
    fn test_number_boundary_values() {
        // Test boundary values between encoding sizes
        let boundaries = [
            (0x07, 1),
            (0x08, 2),
            (0x07FF, 2),
            (0x0800, 3),
            (0x07_FFFF, 3),
            (0x08_0000, 4),
        ];
        for (n, expected_len) in boundaries {
            let encoded = encode_number(n);
            assert_eq!(
                encoded.len(),
                expected_len,
                "boundary {n:#x} should be {expected_len} bytes"
            );
            let (decoded, _) = decode_number(&encoded).unwrap();
            assert_eq!(decoded, n);
        }
    }

    #[test]
    fn test_fixed_point_roundtrip() {
        let values = [0.0, 1.0, -1.0, 0.5, -0.5, 100.25, -100.25, 0.001];
        for f in values {
            let fp = to_fixed_point(f);
            let back = from_fixed_point(fp);
            assert!(
                (back - f).abs() < 0.0001,
                "fixed-point roundtrip failed for {f}: got {back}"
            );
        }
    }

    #[test]
    fn test_fixed_point_precision() {
        // Q16.16 has precision of 1/65536 ≈ 0.000015
        let fp = to_fixed_point(0.000015);
        assert_eq!(fp, 1); // Smallest representable fraction
    }

    #[test]
    fn test_string_roundtrip_short() {
        for s in ["", "a", "hello", "12345678901234"] {
            let encoded = encode_string(s);
            assert_eq!(
                encoded[0] & 0xF0,
                0x50,
                "short string should have 0x5L prefix"
            );
            let (decoded, _) = decode_string(&encoded).unwrap();
            assert_eq!(decoded, s);
        }
    }

    #[test]
    fn test_string_roundtrip_long() {
        let long_string = "a".repeat(100);
        let encoded = encode_string(&long_string);
        assert_eq!(encoded[0], 0x5F, "long string should have 0x5F prefix");
        let (decoded, _) = decode_string(&encoded).unwrap();
        assert_eq!(decoded, long_string);
    }

    #[test]
    fn test_decode_empty_fails() {
        assert!(decode_number(&[]).is_none());
        assert!(decode_string(&[]).is_none());
    }

    #[test]
    fn test_decode_truncated_fails() {
        // 2-byte encoding but only 1 byte provided
        assert!(decode_number(&[0x10]).is_none());
        // String says 5 bytes but only 2 provided
        assert!(decode_string(&[0x55, b'h', b'i']).is_none());
    }
}
