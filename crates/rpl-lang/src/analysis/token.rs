use rpl_core::{Span, TypeId};
use crate::library::LibraryId;
use rpl_core::token::{SemanticKind, TokenInfo};

/// Definition identifier (stub for later phases).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct DefinitionId(pub u32);

/// Reference identifier (stub for later phases).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ReferenceId(pub u32);

/// Context information about a token's role in parsing.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct TokenContext {
    /// True if this was resolved as a unary operator.
    pub resolved_unary: bool,
    /// True if this was resolved as a binary operator.
    pub resolved_binary: bool,
    /// True if this token starts a construct.
    pub starts_construct: bool,
    /// True if this token ends a construct.
    pub ends_construct: bool,
    /// Nesting depth of constructs at this token.
    pub construct_depth: u16,
}

impl TokenContext {
    pub fn new() -> Self {
        Self::default()
    }
}

/// A fully resolved token with all analysis information.
#[derive(Clone, Debug)]
pub struct ResolvedToken {
    /// Source span of this token.
    pub span: Span,
    /// Library that matched this token.
    pub lib: LibraryId,
    /// Token metadata from the library.
    pub info: TokenInfo,
    /// Semantic classification.
    pub semantic: SemanticKind,
    /// Context information.
    pub context: TokenContext,
    /// Definition this token creates, if any.
    pub defines: Option<DefinitionId>,
    /// Reference this token represents, if any.
    pub references: Option<ReferenceId>,
    /// Literal type for number/string literals.
    pub literal_type: Option<TypeId>,
    /// Error message if this token is invalid.
    pub error: Option<String>,
}

impl ResolvedToken {
    pub fn new(span: Span, lib: LibraryId, info: TokenInfo, semantic: SemanticKind) -> Self {
        Self {
            span,
            lib,
            info,
            semantic,
            context: TokenContext::new(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        }
    }

    /// Create an error token.
    pub fn error(span: Span, message: String) -> Self {
        Self {
            span,
            lib: LibraryId::new(0),
            info: TokenInfo::atom(span.len() as u8),
            semantic: SemanticKind::Invalid,
            context: TokenContext::new(),
            defines: None,
            references: None,
            literal_type: None,
            error: Some(message),
        }
    }

    /// Check if this token represents an error.
    pub fn is_error(&self) -> bool {
        self.error.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::Pos;

    #[test]
    fn token_context_default() {
        let ctx = TokenContext::new();
        assert!(!ctx.resolved_unary);
        assert!(!ctx.resolved_binary);
        assert!(!ctx.starts_construct);
        assert!(!ctx.ends_construct);
        assert_eq!(ctx.construct_depth, 0);
    }

    #[test]
    fn resolved_token_new() {
        let span = Span::new(Pos::new(0), Pos::new(3));
        let token = ResolvedToken::new(
            span,
            LibraryId::new(72),
            TokenInfo::atom(3),
            SemanticKind::Command,
        );

        assert_eq!(token.span, span);
        assert_eq!(token.lib, LibraryId::new(72));
        assert_eq!(token.semantic, SemanticKind::Command);
        assert!(!token.is_error());
    }

    #[test]
    fn resolved_token_error() {
        let span = Span::new(Pos::new(0), Pos::new(3));
        let token = ResolvedToken::error(span, "unknown token".to_string());

        assert!(token.is_error());
        assert_eq!(token.semantic, SemanticKind::Invalid);
        assert_eq!(token.error.as_deref(), Some("unknown token"));
    }
}
