//! Literal codec trait for defining how literal values are parsed and compiled.
//!
//! The `LiteralCodec` trait provides a standardized interface for handling
//! literal values in RPL (numbers, strings, binary integers, etc.).

use rpl_core::token::SemanticKind;
use rpl_core::TypeId;

/// Trait for encoding and decoding literal values.
///
/// Implementors define how a specific literal type is:
/// - Recognized (probed) from source text
/// - Parsed from text to a native value
/// - Encoded to bytecode words
/// - Decoded from bytecode words
/// - Formatted back to source text
///
/// # Example
///
/// ```ignore
/// pub struct RealCodec;
///
/// impl LiteralCodec for RealCodec {
///     type Value = f64;
///
///     fn probe(text: &str) -> Option<usize> {
///         parse_real(text).map(|_| text.len())
///     }
///
///     fn parse(text: &str) -> Result<f64, String> {
///         parse_real(text).ok_or_else(|| format!("Invalid number: {}", text))
///     }
///
///     fn encode(value: &f64) -> Vec<u32> {
///         let bits = value.to_bits();
///         vec![(bits >> 32) as u32, bits as u32]
///     }
///
///     fn decode(words: &[u32]) -> Option<f64> {
///         if words.len() >= 2 {
///             let bits = ((words[0] as u64) << 32) | (words[1] as u64);
///             Some(f64::from_bits(bits))
///         } else {
///             None
///         }
///     }
///
///     fn format(value: &f64) -> String {
///         format_real(*value)
///     }
///
///     fn type_id() -> TypeId { TypeId::REAL }
///     fn semantic() -> SemanticKind { SemanticKind::Number }
/// }
/// ```
pub trait LiteralCodec: 'static + Send + Sync {
    /// The native Rust type for this literal.
    type Value;

    /// Probe text to see if it matches this literal type.
    ///
    /// Returns `Some(len)` if the text matches, where `len` is the number
    /// of bytes consumed. Returns `None` if it doesn't match.
    fn probe(text: &str) -> Option<usize>;

    /// Parse text into a native value.
    ///
    /// Called after `probe` returns `Some`, so text is known to be valid.
    fn parse(text: &str) -> Result<Self::Value, String>;

    /// Encode a value to bytecode words.
    ///
    /// These words are emitted after the prolog in compiled bytecode.
    fn encode(value: &Self::Value) -> Vec<u32>;

    /// Decode bytecode words back to a value.
    ///
    /// The `words` slice contains the data words after the prolog.
    /// Returns `None` if the words are invalid.
    fn decode(words: &[u32]) -> Option<Self::Value>;

    /// Format a value back to source text for decompilation.
    fn format(value: &Self::Value) -> String;

    /// The type ID for this literal type.
    ///
    /// Used in the prolog word when compiling.
    fn type_id() -> TypeId;

    /// The semantic kind for syntax highlighting.
    fn semantic() -> SemanticKind;

    /// Number of data words this literal produces (excluding prolog).
    ///
    /// Default implementation calls `encode` on a default value,
    /// but can be overridden for efficiency.
    fn word_count() -> usize
    where
        Self::Value: Default,
    {
        Self::encode(&Self::Value::default()).len()
    }
}

/// Helper struct for using a codec in library implementations.
///
/// This provides convenience methods that work with the codec trait.
pub struct LiteralHelper<C: LiteralCodec>(std::marker::PhantomData<C>);

impl<C: LiteralCodec> LiteralHelper<C> {
    /// Check if text matches this literal type.
    #[inline]
    pub fn probe(text: &str) -> Option<usize> {
        C::probe(text)
    }

    /// Parse and encode text to bytecode words (prolog + data).
    ///
    /// Returns `None` if parsing fails.
    pub fn compile(text: &str) -> Option<(u16, Vec<u32>)> {
        let value = C::parse(text).ok()?;
        let data = C::encode(&value);
        Some((C::type_id().as_u16(), data))
    }

    /// Decode words and format to source text.
    ///
    /// Returns `None` if decoding fails.
    pub fn decompile(words: &[u32]) -> Option<String> {
        let value = C::decode(words)?;
        Some(C::format(&value))
    }

    /// Get the semantic kind for this literal.
    #[inline]
    pub fn semantic() -> SemanticKind {
        C::semantic()
    }

    /// Get the type ID for this literal.
    #[inline]
    pub fn type_id() -> TypeId {
        C::type_id()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A simple test codec for integers.
    struct TestIntCodec;

    impl LiteralCodec for TestIntCodec {
        type Value = i32;

        fn probe(text: &str) -> Option<usize> {
            text.parse::<i32>().ok().map(|_| text.len())
        }

        fn parse(text: &str) -> Result<i32, String> {
            text.parse().map_err(|e| format!("{}", e))
        }

        fn encode(value: &i32) -> Vec<u32> {
            vec![*value as u32]
        }

        fn decode(words: &[u32]) -> Option<i32> {
            words.first().map(|&w| w as i32)
        }

        fn format(value: &i32) -> String {
            value.to_string()
        }

        fn type_id() -> TypeId {
            TypeId::REAL // Using REAL for test purposes
        }

        fn semantic() -> SemanticKind {
            SemanticKind::Number
        }
    }

    #[test]
    fn probe_matches() {
        assert_eq!(TestIntCodec::probe("123"), Some(3));
        assert_eq!(TestIntCodec::probe("-45"), Some(3));
        assert_eq!(TestIntCodec::probe("abc"), None);
    }

    #[test]
    fn parse_works() {
        assert_eq!(TestIntCodec::parse("123"), Ok(123));
        assert_eq!(TestIntCodec::parse("-45"), Ok(-45));
        assert!(TestIntCodec::parse("abc").is_err());
    }

    #[test]
    fn encode_decode_roundtrip() {
        let value = 42i32;
        let words = TestIntCodec::encode(&value);
        let decoded = TestIntCodec::decode(&words);
        assert_eq!(decoded, Some(42));
    }

    #[test]
    fn format_works() {
        assert_eq!(TestIntCodec::format(&123), "123");
        assert_eq!(TestIntCodec::format(&-45), "-45");
    }

    #[test]
    fn helper_compile() {
        let result = LiteralHelper::<TestIntCodec>::compile("123");
        assert!(result.is_some());
        let (type_id, data) = result.unwrap();
        assert_eq!(type_id, TypeId::REAL.as_u16());
        assert_eq!(data, vec![123]);
    }

    #[test]
    fn helper_decompile() {
        let words = vec![456u32];
        let result = LiteralHelper::<TestIntCodec>::decompile(&words);
        assert_eq!(result, Some("456".to_string()));
    }
}
