//! Library for binary integers - type owner and literal parsing.
//!
//! This library:
//! - Probes and compiles binary integer literals (#FF, #1010b, #777o, #123d)
//! - Owns the BinaryInt type (TypeId::BINT = 12)
//!
//! Binary operations (BAND, BOR, etc.) are in BinaryOpsLib.

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};

/// Library for binary integers.
pub struct BinaryIntLib;

impl BinaryIntLib {
    /// Library ID for binary integers.
    pub const ID: LibraryId = LibraryId::new(12);
}

/// Parse a binary integer literal.
/// Formats:
/// - #digits     -> hex (default)
/// - #digitsb    -> binary (only if digits are all 0/1)
/// - #digitso    -> octal (only if digits are all 0-7)
/// - #digitsd    -> decimal (only if digits are all 0-9)
/// - #digitsh    -> hex (explicit)
fn parse_binary_int(text: &str) -> Option<i64> {
    // Must start with #
    let text = text.strip_prefix('#')?;

    if text.is_empty() {
        return None;
    }

    // Try binary suffix (b/B) - only if all preceding chars are 0 or 1
    if let Some(d) = text.strip_suffix('b').or_else(|| text.strip_suffix('B'))
        && !d.is_empty()
        && d.chars().all(|c| c == '0' || c == '1')
    {
        return u64::from_str_radix(d, 2).ok().map(|v| v as i64);
    }

    // Try octal suffix (o/O) - only if all preceding chars are 0-7
    if let Some(d) = text.strip_suffix('o').or_else(|| text.strip_suffix('O'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_digit(8))
    {
        return u64::from_str_radix(d, 8).ok().map(|v| v as i64);
    }

    // Try decimal suffix (d/D) - only if all preceding chars are 0-9
    if let Some(d) = text.strip_suffix('d').or_else(|| text.strip_suffix('D'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_ascii_digit())
    {
        return d.parse::<u64>().ok().map(|v| v as i64);
    }

    // Try hex suffix (h/H) - only if all preceding chars are hex digits
    if let Some(d) = text.strip_suffix('h').or_else(|| text.strip_suffix('H'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_ascii_hexdigit())
    {
        return u64::from_str_radix(d, 16).ok().map(|v| v as i64);
    }

    // No suffix - default to hex
    u64::from_str_radix(text, 16).ok().map(|v| v as i64)
}

/// Check if text looks like a binary integer literal.
fn is_binary_int_literal(text: &str) -> bool {
    if !text.starts_with('#') {
        return false;
    }
    let rest = &text[1..];
    if rest.is_empty() {
        return false;
    }

    // Try each suffix, but only if the preceding digits are valid for that base.
    // This prevents #AB from being parsed as binary (where A is invalid).

    // Try binary suffix (b/B) - only if all preceding chars are 0 or 1
    if let Some(d) = rest.strip_suffix('b').or_else(|| rest.strip_suffix('B'))
        && !d.is_empty()
        && d.chars().all(|c| c == '0' || c == '1')
    {
        return true;
    }

    // Try octal suffix (o/O) - only if all preceding chars are 0-7
    if let Some(d) = rest.strip_suffix('o').or_else(|| rest.strip_suffix('O'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_digit(8))
    {
        return true;
    }

    // Try decimal suffix (d/D) - only if all preceding chars are 0-9
    if let Some(d) = rest.strip_suffix('d').or_else(|| rest.strip_suffix('D'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_ascii_digit())
    {
        return true;
    }

    // Try hex suffix (h/H) - only if all preceding chars are hex digits
    if let Some(d) = rest.strip_suffix('h').or_else(|| rest.strip_suffix('H'))
        && !d.is_empty()
        && d.chars().all(|c| c.is_ascii_hexdigit())
    {
        return true;
    }

    // No suffix - default to hex, all chars must be hex digits
    rest.chars().all(|c| c.is_ascii_hexdigit())
}

impl Library for BinaryIntLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "BinaryInt"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if is_binary_int_literal(text) {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Number,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        if let Some(value) = parse_binary_int(text) {
            // Emit prolog: type=BINT, size=2 (for i64 as 2 words)
            ctx.emit_prolog(TypeId::BINT.as_u16(), 2);
            // Emit the i64 as two 32-bit words (high word first, then low)
            let bits = value as u64;
            ctx.emit((bits >> 32) as u32);
            ctx.emit((bits & 0xFFFF_FFFF) as u32);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
        // BinaryIntLib doesn't have commands to execute.
        // The prolog execution is handled by the VM's execute_prolog.
        ExecuteResult::Error("BinaryIntLib has no executable commands".into())
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for binary integer literals (prolog encoding)
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::BINT.as_u16()
                {
                    ctx.read(); // consume prolog
                    // Read the two data words (high word first, then low)
                    let hi = ctx.read().unwrap_or(0) as u64;
                    let lo = ctx.read().unwrap_or(0) as u64;
                    let bits = (hi << 32) | lo;
                    let value = bits as i64;

                    // Format as hex with # prefix
                    if value == 0 {
                        ctx.write("#0");
                    } else {
                        ctx.write(&format!("#{:X}h", value));
                    }
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
            DecompileMode::Call(_) => DecompileResult::Unknown,
        }
    }

    fn stack_effect(&self, _token: &str) -> StackEffect {
        // Binary integer literals produce one value
        StackEffect::Fixed {
            consumes: 0,
            produces: 1,
        }
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

    #[test]
    fn parse_hex_default() {
        assert_eq!(parse_binary_int("#0"), Some(0));
        assert_eq!(parse_binary_int("#FF"), Some(255));
        assert_eq!(parse_binary_int("#ff"), Some(255));
        assert_eq!(parse_binary_int("#10"), Some(16));
        assert_eq!(parse_binary_int("#FFFF"), Some(65535));
    }

    #[test]
    fn parse_hex_explicit() {
        assert_eq!(parse_binary_int("#FFh"), Some(255));
        assert_eq!(parse_binary_int("#FFH"), Some(255));
        assert_eq!(parse_binary_int("#10h"), Some(16));
    }

    #[test]
    fn parse_binary() {
        assert_eq!(parse_binary_int("#0b"), Some(0));
        assert_eq!(parse_binary_int("#1b"), Some(1));
        assert_eq!(parse_binary_int("#10b"), Some(2));
        assert_eq!(parse_binary_int("#1010b"), Some(10));
        assert_eq!(parse_binary_int("#11111111b"), Some(255));
    }

    #[test]
    fn parse_octal() {
        assert_eq!(parse_binary_int("#0o"), Some(0));
        assert_eq!(parse_binary_int("#7o"), Some(7));
        assert_eq!(parse_binary_int("#10o"), Some(8));
        assert_eq!(parse_binary_int("#377o"), Some(255));
    }

    #[test]
    fn parse_decimal() {
        assert_eq!(parse_binary_int("#0d"), Some(0));
        assert_eq!(parse_binary_int("#10d"), Some(10));
        assert_eq!(parse_binary_int("#255d"), Some(255));
        assert_eq!(parse_binary_int("#1000d"), Some(1000));
    }

    #[test]
    fn parse_invalid() {
        assert_eq!(parse_binary_int(""), None);
        assert_eq!(parse_binary_int("#"), None);
        assert_eq!(parse_binary_int("FF"), None); // no #
        assert_eq!(parse_binary_int("#GG"), None); // invalid hex
    }

    #[test]
    fn parse_ambiguous_suffix() {
        // #B is valid hex (11), not empty binary
        assert_eq!(parse_binary_int("#B"), Some(0xB));
        assert_eq!(parse_binary_int("#b"), Some(0xB));
        // #2b is valid hex (43), not invalid binary
        assert_eq!(parse_binary_int("#2b"), Some(0x2B));
        // #ABd is valid hex (2749), not invalid decimal
        assert_eq!(parse_binary_int("#ABd"), Some(0xABD));
        // But #10b is binary 2, not hex 0x10B
        assert_eq!(parse_binary_int("#10b"), Some(2));
        // And #10d is decimal 10, not hex 0x10D
        assert_eq!(parse_binary_int("#10d"), Some(10));
    }

    #[test]
    fn probe_hex() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        for text in &["#0", "#FF", "#10", "#FFFF", "#FFh"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_binary() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        for text in &["#0b", "#1b", "#1010b", "#11111111b"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_octal() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        for text in &["#0o", "#7o", "#377o"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_decimal() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        for text in &["#0d", "#10d", "#255d"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                text
            );
        }
    }

    #[test]
    fn probe_returns_number_semantic() {
        let interner = Interner::new();
        let lib = BinaryIntLib;
        let ctx = make_probe_ctx("#FF", &interner);

        if let ProbeResult::Match { semantic, .. } = lib.probe(&ctx) {
            assert_eq!(semantic, SemanticKind::Number);
        } else {
            panic!("expected match");
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        // These should NOT match as binary integer literals
        for text in &["123", "0xFF", "FF", "#", "#GG", "hello"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::NoMatch),
                "Should not match: {}",
                text
            );
        }
    }

    #[test]
    fn probe_ambiguous_suffix() {
        let interner = Interner::new();
        let lib = BinaryIntLib;

        // These SHOULD match - they're valid hex even though they end in b/d/o
        for text in &["#B", "#b", "#2b", "#AB", "#ABd", "#10d", "#10b"] {
            let ctx = make_probe_ctx(text, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match as hex: {}",
                text
            );
        }
    }

    #[test]
    fn stack_effect_produces_one() {
        let lib = BinaryIntLib;
        let effect = lib.stack_effect("#FF");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 0,
                produces: 1
            }
        ));
    }
}
