//! Library for comments.
//!
//! This library provides:
//! - `@ text` - Single-line comment (ends at newline)
//! - `@@ text` - Permanent single-line comment (not stripped by STRIPCOMMENTS)
//! - `@@@ text @@@` - Multi-line comment
//!
//! Comments are stored in bytecode but do nothing at runtime.
//! Based on HP RPL Library 20.

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;

rpl_macros::define_library! {
    pub library CommentsLib(20, "Comments");

    commands {
        STRIPCOMMENTS (1 -> 1) "Strip comments from a program" {
            // TODO: Implement STRIPCOMMENTS
            Err("STRIPCOMMENTS not yet implemented".to_string())
        }
    }

    custom probe {
        let text = ctx.text();

        // Check for comment start
        if !text.starts_with('@') {
            return rpl_lang::library::ProbeResult::NoMatch;
        }

        // Check for STRIPCOMMENTS command first (before comment syntax)
        let upper = text.to_ascii_uppercase();
        if upper == "STRIPCOMMENTS" {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        // Parse comment
        if let Some((_, _, len)) = extract_comment(text) {
            // Cap length at 255 for TokenInfo (comments can be longer but we
            // handle that in compile by using the full text)
            let info_len = len.min(255) as u8;
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(info_len),
                semantic: SemanticKind::Comment,
            };
        }

        rpl_lang::library::ProbeResult::NoMatch
    }

    custom compile {
        let text = ctx.text();

        // Check for STRIPCOMMENTS command
        let upper = text.to_ascii_uppercase();
        if upper == "STRIPCOMMENTS" {
            ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_STRIPCOMMENTS);
            return rpl_lang::library::CompileResult::Ok;
        }

        // Check for comment
        if !text.starts_with('@') {
            return rpl_lang::library::CompileResult::NoMatch;
        }

        if let Some((comment_text, _, _)) = extract_comment(text) {
            // Encode comment in bytecode:
            // - Prolog with COMMENT type and size in words
            // - Comment bytes packed into words (including @ markers)
            let bytes = comment_text.as_bytes();
            let word_count = bytes.len().div_ceil(4);

            // Emit prolog
            ctx.emit(rpl_core::make_prolog(
                TypeId::COMMENT.as_u16(),
                word_count as u16,
            ));

            // Pack bytes into words (little-endian)
            for chunk in bytes.chunks(4) {
                let mut word: u32 = 0;
                for (i, &byte) in chunk.iter().enumerate() {
                    word |= (byte as u32) << (i * 8);
                }
                ctx.emit(word);
            }

            return rpl_lang::library::CompileResult::Ok;
        }

        rpl_lang::library::CompileResult::NoMatch
    }
}

/// Comment type based on @ prefix count.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CommentType {
    /// Single `@` - single-line, can be stripped
    SingleLine,
    /// Double `@@` - single-line, permanent
    Permanent,
    /// Triple `@@@` - multi-line
    MultiLine,
}

/// Count leading @ symbols and determine comment type.
fn count_at_prefix(text: &str) -> Option<(CommentType, usize)> {
    let mut count = 0;
    for c in text.chars() {
        if c == '@' {
            count += 1;
        } else {
            break;
        }
    }

    if count == 0 {
        None
    } else if count >= 3 {
        Some((CommentType::MultiLine, count))
    } else if count == 2 {
        Some((CommentType::Permanent, count))
    } else {
        Some((CommentType::SingleLine, count))
    }
}

/// Find the end of a comment, returning end position.
fn find_comment_end(text: &str, comment_type: CommentType, at_count: usize) -> usize {
    match comment_type {
        CommentType::SingleLine | CommentType::Permanent => {
            // Single-line: ends at newline or end of input
            text.find('\n').map(|p| p + 1).unwrap_or(text.len())
        }
        CommentType::MultiLine => {
            // Multi-line: find matching @@@ (at least 3 @s)
            let content_start = at_count;
            if content_start >= text.len() {
                return text.len();
            }

            // Look for closing @@@
            let search_area = &text[content_start..];
            if let Some(pos) = search_area.find("@@@") {
                // Include the closing @@@
                content_start + pos + 3
            } else {
                // No closing found - entire rest is comment
                text.len()
            }
        }
    }
}

/// Extract comment text from source.
fn extract_comment(text: &str) -> Option<(String, CommentType, usize)> {
    let (comment_type, at_count) = count_at_prefix(text)?;
    let end = find_comment_end(text, comment_type, at_count);
    Some((text[..end].to_string(), comment_type, end))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult, StackEffect};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn count_at_prefix_single() {
        let (ty, count) = count_at_prefix("@ comment").unwrap();
        assert_eq!(ty, CommentType::SingleLine);
        assert_eq!(count, 1);
    }

    #[test]
    fn count_at_prefix_permanent() {
        let (ty, count) = count_at_prefix("@@ permanent").unwrap();
        assert_eq!(ty, CommentType::Permanent);
        assert_eq!(count, 2);
    }

    #[test]
    fn count_at_prefix_multi() {
        let (ty, count) = count_at_prefix("@@@ multi @@@").unwrap();
        assert_eq!(ty, CommentType::MultiLine);
        assert_eq!(count, 3);
    }

    #[test]
    fn count_at_prefix_many() {
        let (ty, count) = count_at_prefix("@@@@@ many @@@@@").unwrap();
        assert_eq!(ty, CommentType::MultiLine);
        assert_eq!(count, 5);
    }

    #[test]
    fn count_at_prefix_none() {
        assert!(count_at_prefix("no comment").is_none());
    }

    #[test]
    fn find_end_single_line() {
        let text = "@ comment\nmore";
        let end = find_comment_end(text, CommentType::SingleLine, 1);
        assert_eq!(end, 10); // includes newline
        assert_eq!(&text[..end], "@ comment\n");
    }

    #[test]
    fn find_end_single_line_no_newline() {
        let text = "@ comment";
        let end = find_comment_end(text, CommentType::SingleLine, 1);
        assert_eq!(end, 9);
    }

    #[test]
    fn find_end_multiline() {
        let text = "@@@ multi\nline @@@ more";
        let end = find_comment_end(text, CommentType::MultiLine, 3);
        assert_eq!(end, 18); // includes closing @@@
        assert_eq!(&text[..end], "@@@ multi\nline @@@");
    }

    #[test]
    fn find_end_multiline_unclosed() {
        let text = "@@@ unclosed";
        let end = find_comment_end(text, CommentType::MultiLine, 3);
        assert_eq!(end, 12);
    }

    #[test]
    fn probe_single_line_comment() {
        let interner = Interner::new();
        let lib = CommentsLib;
        let ctx = make_probe_ctx("@ this is a comment", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Comment);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_permanent_comment() {
        let interner = Interner::new();
        let lib = CommentsLib;
        let ctx = make_probe_ctx("@@ permanent comment", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Comment);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_multiline_comment() {
        let interner = Interner::new();
        let lib = CommentsLib;
        let ctx = make_probe_ctx("@@@ multi\nline @@@", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Comment);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_empty_comment() {
        let interner = Interner::new();
        let lib = CommentsLib;
        let ctx = make_probe_ctx("@", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Comment);
            }
            _ => panic!("expected match for empty comment"),
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = CommentsLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_comment() {
        let lib = CommentsLib;
        // Comments have dynamic stack effect since we check for @ prefix
        assert!(matches!(
            lib.stack_effect("@ comment"),
            StackEffect::Dynamic
        ));
    }

    #[test]
    fn stack_effect_stripcomments() {
        let lib = CommentsLib;
        assert!(matches!(
            lib.stack_effect("STRIPCOMMENTS"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
    }
}
