//! Generic Value serialization for RPL.
//!
//! This module provides functions to serialize any RPL Value to bytes
//! and deserialize bytes back to Values. Used by PACKDIR and potentially
//! other features requiring portable value storage.
//!
//! ## Wire Format
//!
//! Each value is prefixed with a 1-byte type tag:
//!
//! | Tag | Type | Payload |
//! |-----|------|---------|
//! | 0x01 | Real | 8 bytes (f64 LE) |
//! | 0x02 | Int | 8 bytes (i64 LE) |
//! | 0x03 | Bool | 1 byte (0 or 1) |
//! | 0x04 | String | u32 len + UTF-8 bytes |
//! | 0x05 | Symbol | u32 len + UTF-8 bytes |
//! | 0x06 | List | u32 count + serialized elements |
//! | 0x07 | Program | u32 word_count + words |
//! | 0x08 | Object | u16 type_id + u32 word_count + words |

use std::fmt;

use rpl_core::TypeId;
use rpl_lang::Value;

// Type tags
const TAG_REAL: u8 = 0x01;
const TAG_INT: u8 = 0x02;
const TAG_BOOL: u8 = 0x03;
const TAG_STRING: u8 = 0x04;
const TAG_SYMBOL: u8 = 0x05;
const TAG_LIST: u8 = 0x06;
const TAG_PROGRAM: u8 = 0x07;
const TAG_OBJECT: u8 = 0x08;

/// Error type for serialization/deserialization failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SerializeError {
    /// Unexpected end of input
    UnexpectedEnd,
    /// Invalid type tag
    InvalidTag(u8),
    /// Invalid UTF-8 in string data
    InvalidUtf8,
}

impl fmt::Display for SerializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEnd => write!(f, "Unexpected end of data"),
            Self::InvalidTag(tag) => write!(f, "Invalid type tag: 0x{:02x}", tag),
            Self::InvalidUtf8 => write!(f, "Invalid UTF-8 string data"),
        }
    }
}

impl std::error::Error for SerializeError {}

/// Serialize a Value to bytes.
pub fn serialize_value(value: &Value) -> Vec<u8> {
    let mut buf = Vec::new();
    serialize_into(&mut buf, value);
    buf
}

/// Serialize a Value into an existing buffer.
fn serialize_into(buf: &mut Vec<u8>, value: &Value) {
    match value {
        Value::Real(r) => {
            buf.push(TAG_REAL);
            buf.extend(&r.to_le_bytes());
        }
        Value::Int(i) => {
            buf.push(TAG_INT);
            buf.extend(&i.to_le_bytes());
        }
        Value::Bool(b) => {
            buf.push(TAG_BOOL);
            buf.push(if *b { 1 } else { 0 });
        }
        Value::String(s) => {
            buf.push(TAG_STRING);
            write_string(buf, s);
        }
        Value::Symbol(sym) => {
            // Symbols are session-specific; serialize as string representation
            buf.push(TAG_SYMBOL);
            let s = format!("__sym_{}", sym.as_u32());
            write_string(buf, &s);
        }
        Value::List(items) => {
            buf.push(TAG_LIST);
            write_u32(buf, items.len() as u32);
            for item in items {
                serialize_into(buf, item);
            }
        }
        Value::Program { code, .. } => {
            // Strip debug_info - not portable
            buf.push(TAG_PROGRAM);
            write_u32(buf, code.len() as u32);
            for word in code {
                buf.extend(&word.to_le_bytes());
            }
        }
        Value::Object { type_id, data } => {
            buf.push(TAG_OBJECT);
            buf.extend(&type_id.as_u16().to_le_bytes());
            write_u32(buf, data.len() as u32);
            for word in data {
                buf.extend(&word.to_le_bytes());
            }
        }
    }
}

/// Deserialize a Value from bytes.
/// Returns (value, bytes_consumed) on success.
pub fn deserialize_value(bytes: &[u8]) -> Result<(Value, usize), SerializeError> {
    if bytes.is_empty() {
        return Err(SerializeError::UnexpectedEnd);
    }

    let tag = bytes[0];
    let rest = &bytes[1..];

    match tag {
        TAG_REAL => {
            if rest.len() < 8 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let r = f64::from_le_bytes(rest[..8].try_into().unwrap());
            Ok((Value::Real(r), 9))
        }
        TAG_INT => {
            if rest.len() < 8 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let i = i64::from_le_bytes(rest[..8].try_into().unwrap());
            Ok((Value::Int(i), 9))
        }
        TAG_BOOL => {
            if rest.is_empty() {
                return Err(SerializeError::UnexpectedEnd);
            }
            let b = rest[0] != 0;
            Ok((Value::Bool(b), 2))
        }
        TAG_STRING => {
            let (s, consumed) = read_string(rest)?;
            Ok((Value::String(s), 1 + consumed))
        }
        TAG_SYMBOL => {
            // Deserialize as String (symbols are session-specific)
            let (s, consumed) = read_string(rest)?;
            Ok((Value::String(s), 1 + consumed))
        }
        TAG_LIST => {
            if rest.len() < 4 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let count = read_u32(rest)? as usize;
            let mut offset = 4;
            let mut items = Vec::with_capacity(count);

            for _ in 0..count {
                let (item, consumed) = deserialize_value(&rest[offset..])?;
                items.push(item);
                offset += consumed;
            }

            Ok((Value::List(items), 1 + offset))
        }
        TAG_PROGRAM => {
            if rest.len() < 4 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let word_count = read_u32(rest)? as usize;
            let bytes_needed = 4 + word_count * 4;

            if rest.len() < bytes_needed {
                return Err(SerializeError::UnexpectedEnd);
            }

            let mut code = Vec::with_capacity(word_count);
            for i in 0..word_count {
                let start = 4 + i * 4;
                let word = u32::from_le_bytes(rest[start..start + 4].try_into().unwrap());
                code.push(word);
            }

            Ok((
                Value::Program {
                    code,
                    debug_info: None,
                },
                1 + bytes_needed,
            ))
        }
        TAG_OBJECT => {
            if rest.len() < 6 {
                return Err(SerializeError::UnexpectedEnd);
            }

            let type_id_raw = u16::from_le_bytes(rest[..2].try_into().unwrap());
            let type_id = TypeId::new(type_id_raw);
            let word_count = read_u32(&rest[2..])? as usize;
            let bytes_needed = 6 + word_count * 4;

            if rest.len() < bytes_needed {
                return Err(SerializeError::UnexpectedEnd);
            }

            let mut data = Vec::with_capacity(word_count);
            for i in 0..word_count {
                let start = 6 + i * 4;
                let word = u32::from_le_bytes(rest[start..start + 4].try_into().unwrap());
                data.push(word);
            }

            Ok((Value::Object { type_id, data }, 1 + bytes_needed))
        }
        _ => Err(SerializeError::InvalidTag(tag)),
    }
}

// Helper functions

fn write_u32(buf: &mut Vec<u8>, val: u32) {
    buf.extend(&val.to_le_bytes());
}

fn read_u32(bytes: &[u8]) -> Result<u32, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }
    Ok(u32::from_le_bytes(bytes[..4].try_into().unwrap()))
}

fn write_string(buf: &mut Vec<u8>, s: &str) {
    write_u32(buf, s.len() as u32);
    buf.extend(s.as_bytes());
}

fn read_string(bytes: &[u8]) -> Result<(String, usize), SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let len = read_u32(bytes)? as usize;

    if bytes.len() < 4 + len {
        return Err(SerializeError::UnexpectedEnd);
    }

    let s = std::str::from_utf8(&bytes[4..4 + len])
        .map_err(|_| SerializeError::InvalidUtf8)?
        .to_string();

    Ok((s, 4 + len))
}

/// Pack bytes into u32 words with length prefix.
/// Format: [byte_length: u32] [packed_bytes: 4 per word, LE]
pub fn bytes_to_words(bytes: &[u8]) -> Vec<u32> {
    let mut words = Vec::with_capacity(1 + bytes.len().div_ceil(4));
    words.push(bytes.len() as u32);

    for chunk in bytes.chunks(4) {
        let mut word = 0u32;
        for (i, &b) in chunk.iter().enumerate() {
            word |= (b as u32) << (i * 8);
        }
        words.push(word);
    }

    words
}

/// Unpack u32 words back to bytes.
/// Expects first word to be byte length.
pub fn words_to_bytes(words: &[u32]) -> Vec<u8> {
    if words.is_empty() {
        return Vec::new();
    }

    let byte_len = words[0] as usize;
    let mut bytes = Vec::with_capacity(byte_len);

    for &word in &words[1..] {
        bytes.push((word & 0xFF) as u8);
        if bytes.len() < byte_len {
            bytes.push(((word >> 8) & 0xFF) as u8);
        }
        if bytes.len() < byte_len {
            bytes.push(((word >> 16) & 0xFF) as u8);
        }
        if bytes.len() < byte_len {
            bytes.push(((word >> 24) & 0xFF) as u8);
        }
    }

    bytes.truncate(byte_len);
    bytes
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: Value) -> Value {
        let bytes = serialize_value(&value);
        let (result, consumed) = deserialize_value(&bytes).unwrap();
        assert_eq!(consumed, bytes.len());
        result
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_real() {
        let value = Value::Real(3.14159);
        let result = round_trip(value.clone());
        assert!(matches!(result, Value::Real(r) if (r - 3.14159).abs() < 1e-10));
    }

    #[test]
    fn test_real_special() {
        // Infinity
        let result = round_trip(Value::Real(f64::INFINITY));
        assert!(matches!(result, Value::Real(r) if r.is_infinite() && r.is_sign_positive()));

        // Negative infinity
        let result = round_trip(Value::Real(f64::NEG_INFINITY));
        assert!(matches!(result, Value::Real(r) if r.is_infinite() && r.is_sign_negative()));

        // Zero
        let result = round_trip(Value::Real(0.0));
        assert!(matches!(result, Value::Real(r) if r == 0.0));
    }

    #[test]
    fn test_int() {
        let result = round_trip(Value::Int(42));
        assert!(matches!(result, Value::Int(42)));

        let result = round_trip(Value::Int(-1000000));
        assert!(matches!(result, Value::Int(-1000000)));

        let result = round_trip(Value::Int(i64::MAX));
        assert!(matches!(result, Value::Int(i) if i == i64::MAX));

        let result = round_trip(Value::Int(i64::MIN));
        assert!(matches!(result, Value::Int(i) if i == i64::MIN));
    }

    #[test]
    fn test_bool() {
        let result = round_trip(Value::Bool(true));
        assert!(matches!(result, Value::Bool(true)));

        let result = round_trip(Value::Bool(false));
        assert!(matches!(result, Value::Bool(false)));
    }

    #[test]
    fn test_string() {
        let result = round_trip(Value::String("hello".to_string()));
        assert!(matches!(result, Value::String(s) if s == "hello"));

        // Empty string
        let result = round_trip(Value::String(String::new()));
        assert!(matches!(result, Value::String(s) if s.is_empty()));

        // Unicode
        let result = round_trip(Value::String("héllo 世界 🎉".to_string()));
        assert!(matches!(result, Value::String(s) if s == "héllo 世界 🎉"));
    }

    #[test]
    fn test_symbol() {
        // Symbols deserialize as strings
        let sym = rpl_core::Symbol::from_raw(123);
        let bytes = serialize_value(&Value::Symbol(sym));
        let (result, _) = deserialize_value(&bytes).unwrap();
        assert!(matches!(result, Value::String(s) if s == "__sym_123"));
    }

    #[test]
    fn test_list_empty() {
        let result = round_trip(Value::List(vec![]));
        assert!(matches!(result, Value::List(v) if v.is_empty()));
    }

    #[test]
    fn test_list_primitives() {
        let list = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = round_trip(list);
        if let Value::List(items) = result {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0], Value::Int(1)));
            assert!(matches!(items[1], Value::Int(2)));
            assert!(matches!(items[2], Value::Int(3)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_list_mixed() {
        let list = Value::List(vec![
            Value::Int(42),
            Value::Real(3.14),
            Value::String("hello".to_string()),
            Value::Bool(true),
        ]);
        let result = round_trip(list);
        if let Value::List(items) = result {
            assert_eq!(items.len(), 4);
            assert!(matches!(items[0], Value::Int(42)));
            assert!(matches!(items[1], Value::Real(r) if (r - 3.14).abs() < 1e-10));
            assert!(matches!(&items[2], Value::String(s) if s == "hello"));
            assert!(matches!(items[3], Value::Bool(true)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_list_nested() {
        let inner = Value::List(vec![Value::Int(1), Value::Int(2)]);
        let outer = Value::List(vec![inner, Value::Int(3)]);
        let result = round_trip(outer);

        if let Value::List(items) = result {
            assert_eq!(items.len(), 2);
            if let Value::List(inner_items) = &items[0] {
                assert_eq!(inner_items.len(), 2);
                assert!(matches!(inner_items[0], Value::Int(1)));
                assert!(matches!(inner_items[1], Value::Int(2)));
            } else {
                panic!("Expected nested list");
            }
            assert!(matches!(items[1], Value::Int(3)));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_program() {
        let program = Value::Program {
            code: vec![0x12345678, 0xDEADBEEF, 0x00000000],
            debug_info: None,
        };
        let result = round_trip(program);

        if let Value::Program { code, debug_info } = result {
            assert_eq!(code, vec![0x12345678, 0xDEADBEEF, 0x00000000]);
            assert!(debug_info.is_none()); // Debug info is stripped
        } else {
            panic!("Expected program");
        }
    }

    #[test]
    fn test_object() {
        let obj = Value::Object {
            type_id: TypeId::BLOB,
            data: vec![42, 100, 0xFFFFFFFF],
        };
        let result = round_trip(obj);

        if let Value::Object { type_id, data } = result {
            assert_eq!(type_id, TypeId::BLOB);
            assert_eq!(data, vec![42, 100, 0xFFFFFFFF]);
        } else {
            panic!("Expected object");
        }
    }

    #[test]
    fn test_object_empty() {
        let obj = Value::Object {
            type_id: TypeId::new(200),
            data: vec![],
        };
        let result = round_trip(obj);

        if let Value::Object { type_id, data } = result {
            assert_eq!(type_id.as_u16(), 200);
            assert!(data.is_empty());
        } else {
            panic!("Expected object");
        }
    }

    #[test]
    fn test_bytes_to_words_roundtrip() {
        let original = vec![1, 2, 3, 4, 5, 6, 7];
        let words = bytes_to_words(&original);
        let result = words_to_bytes(&words);
        assert_eq!(result, original);
    }

    #[test]
    fn test_bytes_to_words_empty() {
        let words = bytes_to_words(&[]);
        let result = words_to_bytes(&words);
        assert!(result.is_empty());
    }

    #[test]
    fn test_bytes_to_words_aligned() {
        let original = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let words = bytes_to_words(&original);
        let result = words_to_bytes(&words);
        assert_eq!(result, original);
    }

    #[test]
    fn test_error_unexpected_end() {
        let bytes = [TAG_REAL]; // Missing 8 bytes
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::UnexpectedEnd)));
    }

    #[test]
    fn test_error_invalid_tag() {
        let bytes = [0xFF, 0, 0, 0];
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::InvalidTag(0xFF))));
    }

    #[test]
    fn test_error_invalid_utf8() {
        let mut bytes = vec![TAG_STRING];
        bytes.extend(&4u32.to_le_bytes()); // length = 4
        bytes.extend(&[0xFF, 0xFE, 0xFF, 0xFE]); // invalid UTF-8
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::InvalidUtf8)));
    }

    #[test]
    fn test_error_empty() {
        let result = deserialize_value(&[]);
        assert!(matches!(result, Err(SerializeError::UnexpectedEnd)));
    }
}
