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
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};

/// Library for comments.
pub struct CommentsLib;

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

impl CommentsLib {
    /// Library ID for comments (Library 20 in HP RPL).
    pub const ID: LibraryId = LibraryId::new(20);

    // Command IDs
    const CMD_STRIPCOMMENTS: u16 = 0;

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

    /// Find the end of a comment, returning (end_position, includes_terminator).
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

    /// Extract comment text (including @ markers) from source.
    fn extract_comment(text: &str) -> Option<(String, CommentType, usize)> {
        let (comment_type, at_count) = Self::count_at_prefix(text)?;
        let end = Self::find_comment_end(text, comment_type, at_count);
        Some((text[..end].to_string(), comment_type, end))
    }
}

impl Library for CommentsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Comments"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        // Check for comment start
        if !text.starts_with('@') {
            return ProbeResult::NoMatch;
        }

        // Check for STRIPCOMMENTS command first (before comment syntax)
        let upper = text.to_ascii_uppercase();
        if upper == "STRIPCOMMENTS" {
            return ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        // Parse comment
        if let Some((_, _, len)) = Self::extract_comment(text) {
            // Cap length at 255 for TokenInfo (comments can be longer but we
            // handle that in compile by using the full text)
            let info_len = len.min(255) as u8;
            return ProbeResult::Match {
                info: TokenInfo::atom(info_len),
                semantic: SemanticKind::Comment,
            };
        }

        ProbeResult::NoMatch
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        // Check for STRIPCOMMENTS command
        let upper = text.to_ascii_uppercase();
        if upper == "STRIPCOMMENTS" {
            ctx.emit_opcode(Self::ID.as_u16(), Self::CMD_STRIPCOMMENTS);
            return CompileResult::Ok;
        }

        // Check for comment
        if !text.starts_with('@') {
            return CompileResult::NoMatch;
        }

        if let Some((comment_text, _, _)) = Self::extract_comment(text) {
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

            return CompileResult::Ok;
        }

        CompileResult::NoMatch
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_STRIPCOMMENTS => {
                // TODO: Implement STRIPCOMMENTS
                // For now, just return an error
                ExecuteResult::Error("STRIPCOMMENTS not yet implemented".to_string())
            }
            _ => ExecuteResult::Error(format!("Unknown comment command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for COMMENT prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::COMMENT.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read(); // consume prolog

                    // Read comment data
                    let mut bytes = Vec::with_capacity(size * 4);
                    for _ in 0..size {
                        if let Some(w) = ctx.read() {
                            bytes.push((w & 0xFF) as u8);
                            bytes.push(((w >> 8) & 0xFF) as u8);
                            bytes.push(((w >> 16) & 0xFF) as u8);
                            bytes.push(((w >> 24) & 0xFF) as u8);
                        }
                    }

                    // Find actual length (stop at null padding)
                    let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
                    let comment = String::from_utf8_lossy(&bytes[..len]);

                    ctx.write(&comment);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
            DecompileMode::Call(cmd) => match cmd {
                Self::CMD_STRIPCOMMENTS => {
                    ctx.write("STRIPCOMMENTS");
                    DecompileResult::Ok
                }
                _ => DecompileResult::Unknown,
            },
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        // Comments don't affect the stack
        if token.starts_with('@') && !token.eq_ignore_ascii_case("STRIPCOMMENTS") {
            return StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            };
        }

        // STRIPCOMMENTS: program -> program
        if token.eq_ignore_ascii_case("STRIPCOMMENTS") {
            return StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            };
        }

        StackEffect::Dynamic
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
    fn count_at_prefix_single() {
        let (ty, count) = CommentsLib::count_at_prefix("@ comment").unwrap();
        assert_eq!(ty, CommentType::SingleLine);
        assert_eq!(count, 1);
    }

    #[test]
    fn count_at_prefix_permanent() {
        let (ty, count) = CommentsLib::count_at_prefix("@@ permanent").unwrap();
        assert_eq!(ty, CommentType::Permanent);
        assert_eq!(count, 2);
    }

    #[test]
    fn count_at_prefix_multi() {
        let (ty, count) = CommentsLib::count_at_prefix("@@@ multi @@@").unwrap();
        assert_eq!(ty, CommentType::MultiLine);
        assert_eq!(count, 3);
    }

    #[test]
    fn count_at_prefix_many() {
        let (ty, count) = CommentsLib::count_at_prefix("@@@@@ many @@@@@").unwrap();
        assert_eq!(ty, CommentType::MultiLine);
        assert_eq!(count, 5);
    }

    #[test]
    fn count_at_prefix_none() {
        assert!(CommentsLib::count_at_prefix("no comment").is_none());
    }

    #[test]
    fn find_end_single_line() {
        let text = "@ comment\nmore";
        let end = CommentsLib::find_comment_end(text, CommentType::SingleLine, 1);
        assert_eq!(end, 10); // includes newline
        assert_eq!(&text[..end], "@ comment\n");
    }

    #[test]
    fn find_end_single_line_no_newline() {
        let text = "@ comment";
        let end = CommentsLib::find_comment_end(text, CommentType::SingleLine, 1);
        assert_eq!(end, 9);
    }

    #[test]
    fn find_end_multiline() {
        let text = "@@@ multi\nline @@@ more";
        let end = CommentsLib::find_comment_end(text, CommentType::MultiLine, 3);
        assert_eq!(end, 18); // includes closing @@@
        assert_eq!(&text[..end], "@@@ multi\nline @@@");
    }

    #[test]
    fn find_end_multiline_unclosed() {
        let text = "@@@ unclosed";
        let end = CommentsLib::find_comment_end(text, CommentType::MultiLine, 3);
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
        assert!(matches!(
            lib.stack_effect("@ comment"),
            StackEffect::Fixed {
                consumes: 0,
                produces: 0
            }
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
