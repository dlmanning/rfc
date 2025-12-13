//! Byte stream encoding for plot objects.
//!
//! Numbers use a variable-length format with high nibble SLLL:
//! - S = sign bit (1 = negative)
//! - LLL = length in additional bytes (0-4)
//!
//! Coordinates use Q8.24 fixed-point format.

use rpl_core::Word;

/// Convert a floating-point coordinate to Q8.24 fixed-point.
pub fn to_fixed_point(f: f64) -> i32 {
    (f * (1 << 24) as f64).round() as i32
}

/// Convert Q8.24 fixed-point back to floating-point.
#[allow(dead_code)]
pub fn from_fixed_point(fp: i32) -> f64 {
    fp as f64 / (1 << 24) as f64
}

/// Encode a signed integer using variable-length format.
///
/// High nibble encodes sign and length:
/// - Bits 7: sign (1 = negative)
/// - Bits 6-4: additional bytes (0-4)
///
/// Returns the encoded bytes.
pub fn encode_number(n: i64) -> Vec<u8> {
    let sign = if n < 0 { 0x80u8 } else { 0x00u8 };
    let abs = n.unsigned_abs();

    if abs <= 0x07 {
        // LLL=0: 1 byte, range [-8, +7]
        let low = (abs as u8) & 0x0F;
        vec![sign | low]
    } else if abs <= 0x07FF {
        // LLL=1: 2 bytes, range [-2048, +2047]
        let high = ((abs >> 8) as u8) & 0x0F;
        let low = abs as u8;
        vec![sign | 0x10 | high, low]
    } else if abs <= 0x07_FFFF {
        // LLL=2: 3 bytes, range [-524288, +524287]
        let high = ((abs >> 16) as u8) & 0x0F;
        let mid = (abs >> 8) as u8;
        let low = abs as u8;
        vec![sign | 0x20 | high, mid, low]
    } else if abs <= 0x07FF_FFFF {
        // LLL=3: 4 bytes, range [-134217728, +134217727]
        let high = ((abs >> 24) as u8) & 0x0F;
        let mid2 = (abs >> 16) as u8;
        let mid1 = (abs >> 8) as u8;
        let low = abs as u8;
        vec![sign | 0x30 | high, mid2, mid1, low]
    } else {
        // LLL=4: 5 bytes, range [-34359738368, +34359738367]
        let high = ((abs >> 32) as u8) & 0x0F;
        let mid3 = (abs >> 24) as u8;
        let mid2 = (abs >> 16) as u8;
        let mid1 = (abs >> 8) as u8;
        let low = abs as u8;
        vec![sign | 0x40 | high, mid3, mid2, mid1, low]
    }
}

/// Decode a variable-length number from bytes.
///
/// Returns (value, bytes_consumed).
#[allow(dead_code)]
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

/// Encode a string using 0x5L prefix format.
///
/// - 0x50-0x5E: inline length 0-14 chars
/// - 0x5F: extended length (followed by encoded length)
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

/// Decode a string from bytes.
///
/// Returns (string, bytes_consumed).
#[allow(dead_code)]
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

/// Pack bytes into 32-bit words (big-endian, zero-padded).
pub fn bytes_to_words(bytes: &[u8]) -> Vec<Word> {
    let mut words = Vec::with_capacity(bytes.len().div_ceil(4));

    for chunk in bytes.chunks(4) {
        let mut word: u32 = 0;
        for (i, &b) in chunk.iter().enumerate() {
            word |= (b as u32) << (24 - i * 8);
        }
        words.push(word);
    }

    // Prepend byte length as first word
    let mut result = vec![bytes.len() as Word];
    result.extend(words);
    result
}

/// Unpack 32-bit words back to bytes.
pub fn words_to_bytes(words: &[Word]) -> Vec<u8> {
    if words.is_empty() {
        return Vec::new();
    }

    let byte_len = words[0] as usize;
    let mut bytes = Vec::with_capacity(byte_len);

    for &word in &words[1..] {
        for i in 0..4 {
            if bytes.len() >= byte_len {
                break;
            }
            bytes.push((word >> (24 - i * 8)) as u8);
        }
    }

    bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(clippy::approx_constant)]
    fn fixed_point_conversion() {
        // Test integer values
        assert_eq!(from_fixed_point(to_fixed_point(0.0)), 0.0);
        assert_eq!(from_fixed_point(to_fixed_point(1.0)), 1.0);
        assert_eq!(from_fixed_point(to_fixed_point(-1.0)), -1.0);

        // Test fractional values (with some tolerance for floating-point)
        let val = from_fixed_point(to_fixed_point(0.5));
        assert!((val - 0.5).abs() < 1e-6);

        let val = from_fixed_point(to_fixed_point(3.14159));
        assert!((val - 3.14159).abs() < 1e-6);
    }

    #[test]
    fn number_encoding_small() {
        // Small positive
        let encoded = encode_number(5);
        assert_eq!(encoded.len(), 1);
        let (decoded, consumed) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 5);
        assert_eq!(consumed, 1);

        // Small negative
        let encoded = encode_number(-3);
        assert_eq!(encoded.len(), 1);
        let (decoded, consumed) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, -3);
        assert_eq!(consumed, 1);

        // Zero
        let encoded = encode_number(0);
        assert_eq!(encoded.len(), 1);
        let (decoded, _) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 0);
    }

    #[test]
    fn number_encoding_medium() {
        // 2-byte range
        let encoded = encode_number(1000);
        assert_eq!(encoded.len(), 2);
        let (decoded, consumed) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 1000);
        assert_eq!(consumed, 2);

        let encoded = encode_number(-2000);
        let (decoded, _) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, -2000);
    }

    #[test]
    fn number_encoding_large() {
        // 3-byte range
        let encoded = encode_number(100000);
        assert_eq!(encoded.len(), 3);
        let (decoded, _) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 100000);

        // 4-byte range
        let encoded = encode_number(10000000);
        assert_eq!(encoded.len(), 4);
        let (decoded, _) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 10000000);

        // 5-byte range
        let encoded = encode_number(10000000000i64);
        assert_eq!(encoded.len(), 5);
        let (decoded, _) = decode_number(&encoded).unwrap();
        assert_eq!(decoded, 10000000000i64);
    }

    #[test]
    fn string_encoding_short() {
        let encoded = encode_string("hello");
        assert_eq!(encoded[0], 0x55); // 0x50 | 5
        let (decoded, consumed) = decode_string(&encoded).unwrap();
        assert_eq!(decoded, "hello");
        assert_eq!(consumed, 6); // 1 header + 5 chars
    }

    #[test]
    fn string_encoding_empty() {
        let encoded = encode_string("");
        assert_eq!(encoded[0], 0x50);
        let (decoded, _) = decode_string(&encoded).unwrap();
        assert_eq!(decoded, "");
    }

    #[test]
    fn string_encoding_long() {
        let long_str = "a".repeat(100);
        let encoded = encode_string(&long_str);
        assert_eq!(encoded[0], 0x5F); // Extended length marker
        let (decoded, _) = decode_string(&encoded).unwrap();
        assert_eq!(decoded, long_str);
    }

    #[test]
    fn bytes_words_roundtrip() {
        let original = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let words = bytes_to_words(&original);
        let recovered = words_to_bytes(&words);
        assert_eq!(original, recovered);
    }

    #[test]
    fn bytes_words_empty() {
        let original: Vec<u8> = vec![];
        let words = bytes_to_words(&original);
        let recovered = words_to_bytes(&words);
        assert_eq!(original, recovered);
    }
}
