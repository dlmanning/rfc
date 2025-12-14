//! Library for binary integers - type owner and literal parsing.
//!
//! This library:
//! - Probes and compiles binary integer literals (#FF, #1010b, #777o, #123d)
//! - Owns the BinaryInt type (TypeId::BINT = 12)
//!
//! Binary operations (BAND, BOR, etc.) are in BinaryOpsLib.

use crate::codecs::BintCodec;

rpl_macros::define_library! {
    pub library BinaryIntLib(12, "BinaryInt");

    literals {
        bint: BintCodec;
    }

    prologs {
        BINT: format(BintCodec);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::SemanticKind;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult, StackEffect};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
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
