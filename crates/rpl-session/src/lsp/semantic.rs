use rpl_lang::analysis::AnalysisResult;
use rpl_core::Span;
use rpl_core::token::SemanticKind;

/// LSP semantic token type.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SemanticTokenType {
    /// Number literal.
    Number,
    /// String literal.
    String,
    /// Variable reference.
    Variable,
    /// Function/command name.
    Function,
    /// Operator.
    Operator,
    /// Keyword.
    Keyword,
    /// Comment.
    Comment,
    /// Type name.
    Type,
    /// Parameter.
    Parameter,
    /// Property.
    Property,
}

impl SemanticTokenType {
    /// Get the LSP token type index.
    pub fn as_u32(self) -> u32 {
        match self {
            Self::Number => 0,
            Self::String => 1,
            Self::Variable => 2,
            Self::Function => 3,
            Self::Operator => 4,
            Self::Keyword => 5,
            Self::Comment => 6,
            Self::Type => 7,
            Self::Parameter => 8,
            Self::Property => 9,
        }
    }

    /// Get the LSP token type name.
    pub fn name(self) -> &'static str {
        match self {
            Self::Number => "number",
            Self::String => "string",
            Self::Variable => "variable",
            Self::Function => "function",
            Self::Operator => "operator",
            Self::Keyword => "keyword",
            Self::Comment => "comment",
            Self::Type => "type",
            Self::Parameter => "parameter",
            Self::Property => "property",
        }
    }

    /// Get all token type names for LSP legend.
    pub fn legend() -> &'static [&'static str] {
        &[
            "number",
            "string",
            "variable",
            "function",
            "operator",
            "keyword",
            "comment",
            "type",
            "parameter",
            "property",
        ]
    }
}

/// A semantic token for syntax highlighting.
#[derive(Clone, Debug)]
pub struct SemanticToken {
    /// The span of the token.
    pub span: Span,
    /// The token type.
    pub token_type: SemanticTokenType,
    /// Token modifiers (bitfield).
    pub modifiers: u32,
}

impl SemanticToken {
    /// Create a new semantic token.
    pub fn new(span: Span, token_type: SemanticTokenType) -> Self {
        Self {
            span,
            token_type,
            modifiers: 0,
        }
    }

    /// Add a modifier.
    pub fn with_modifier(mut self, modifier: SemanticModifier) -> Self {
        self.modifiers |= modifier.as_u32();
        self
    }
}

/// Semantic token modifiers.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SemanticModifier {
    /// Definition of a symbol.
    Definition,
    /// Declaration of a symbol.
    Declaration,
    /// Read-only symbol.
    Readonly,
    /// Static symbol.
    Static,
    /// Deprecated symbol.
    Deprecated,
    /// Abstract symbol.
    Abstract,
    /// Async symbol.
    Async,
    /// Modification of a symbol.
    Modification,
    /// Documentation.
    Documentation,
    /// Default library.
    DefaultLibrary,
}

impl SemanticModifier {
    /// Get the modifier bit.
    pub fn as_u32(self) -> u32 {
        1 << match self {
            Self::Definition => 0,
            Self::Declaration => 1,
            Self::Readonly => 2,
            Self::Static => 3,
            Self::Deprecated => 4,
            Self::Abstract => 5,
            Self::Async => 6,
            Self::Modification => 7,
            Self::Documentation => 8,
            Self::DefaultLibrary => 9,
        }
    }

    /// Get all modifier names for LSP legend.
    pub fn legend() -> &'static [&'static str] {
        &[
            "definition",
            "declaration",
            "readonly",
            "static",
            "deprecated",
            "abstract",
            "async",
            "modification",
            "documentation",
            "defaultLibrary",
        ]
    }
}

/// Get semantic tokens for an analysis result.
pub fn semantic_tokens(analysis: &AnalysisResult) -> Vec<SemanticToken> {
    let mut tokens = Vec::with_capacity(analysis.tokens.len());

    for token in &analysis.tokens {
        let token_type = match token.semantic {
            SemanticKind::Number => SemanticTokenType::Number,
            SemanticKind::String => SemanticTokenType::String,
            SemanticKind::Variable | SemanticKind::Local => SemanticTokenType::Variable,
            SemanticKind::Parameter => SemanticTokenType::Parameter,
            SemanticKind::Command => SemanticTokenType::Function,
            SemanticKind::Operator => SemanticTokenType::Operator,
            SemanticKind::Keyword => SemanticTokenType::Keyword,
            SemanticKind::Comment => SemanticTokenType::Comment,
            SemanticKind::Definition => SemanticTokenType::Variable,
            SemanticKind::Constant => SemanticTokenType::Property,
            SemanticKind::Unit => SemanticTokenType::Type,
            SemanticKind::Bracket => continue, // Skip brackets
            SemanticKind::Invalid => continue, // Skip invalid tokens
        };

        let mut sem_token = SemanticToken::new(token.span, token_type);

        // Add definition modifier if this token defines a symbol
        if token.defines.is_some() {
            sem_token = sem_token.with_modifier(SemanticModifier::Definition);
        }

        tokens.push(sem_token);
    }

    tokens
}

/// Encode semantic tokens in LSP delta format.
/// Returns (line, start_char, length, token_type, modifiers) tuples.
pub fn encode_semantic_tokens(
    tokens: &[SemanticToken],
    source: &rpl_source::SourceFile,
) -> Vec<u32> {
    let mut data = Vec::with_capacity(tokens.len() * 5);
    let mut prev_line = 0u32;
    let mut prev_char = 0u32;

    for token in tokens {
        let start = source.line_col(token.span.start());
        let end = source.line_col(token.span.end());

        // LSP uses 0-indexed lines/columns
        let line = start.line.saturating_sub(1);
        let char = start.col.saturating_sub(1);
        let length = if start.line == end.line {
            end.col.saturating_sub(start.col)
        } else {
            // Multi-line tokens: just use the first line
            token.span.len()
        };

        // Delta encoding
        let delta_line = line - prev_line;
        let delta_char = if delta_line == 0 {
            char - prev_char
        } else {
            char
        };

        data.push(delta_line);
        data.push(delta_char);
        data.push(length);
        data.push(token.token_type.as_u32());
        data.push(token.modifiers);

        prev_line = line;
        prev_char = char;
    }

    data
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_lang::analysis::{ResolvedToken, TokenContext};
    use rpl_core::{Pos, Span};
    use rpl_stdlib::RealNumbersLib;
    use rpl_source::SourceId;
    use rpl_core::token::TokenInfo;

    fn make_analysis() -> AnalysisResult {
        AnalysisResult {
            source_id: SourceId::new(0),
            version: 0,
            tokens: vec![],
            lines: vec![],
            diagnostics: vec![],
        }
    }

    #[test]
    fn semantic_token_type_as_u32() {
        assert_eq!(SemanticTokenType::Number.as_u32(), 0);
        assert_eq!(SemanticTokenType::Function.as_u32(), 3);
        assert_eq!(SemanticTokenType::Keyword.as_u32(), 5);
    }

    #[test]
    fn semantic_token_type_name() {
        assert_eq!(SemanticTokenType::Number.name(), "number");
        assert_eq!(SemanticTokenType::Function.name(), "function");
    }

    #[test]
    fn semantic_token_type_legend() {
        let legend = SemanticTokenType::legend();
        assert!(legend.contains(&"number"));
        assert!(legend.contains(&"function"));
        assert!(legend.contains(&"keyword"));
    }

    #[test]
    fn semantic_modifier_as_u32() {
        assert_eq!(SemanticModifier::Definition.as_u32(), 1);
        assert_eq!(SemanticModifier::Readonly.as_u32(), 4);
    }

    #[test]
    fn semantic_modifier_legend() {
        let legend = SemanticModifier::legend();
        assert!(legend.contains(&"definition"));
        assert!(legend.contains(&"readonly"));
    }

    #[test]
    fn semantic_token_new() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let token = SemanticToken::new(span, SemanticTokenType::Number);

        assert_eq!(token.span.start(), Pos::new(0));
        assert_eq!(token.token_type, SemanticTokenType::Number);
        assert_eq!(token.modifiers, 0);
    }

    #[test]
    fn semantic_token_with_modifier() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let token = SemanticToken::new(span, SemanticTokenType::Variable)
            .with_modifier(SemanticModifier::Definition);

        assert_eq!(token.modifiers, 1); // Definition is bit 0
    }

    #[test]
    fn semantic_token_multiple_modifiers() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let token = SemanticToken::new(span, SemanticTokenType::Variable)
            .with_modifier(SemanticModifier::Definition)
            .with_modifier(SemanticModifier::Readonly);

        assert_eq!(token.modifiers, 5); // Definition (1) + Readonly (4)
    }

    #[test]
    fn semantic_tokens_basic() {
        let mut analysis = make_analysis();

        // Add a number token
        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(2)),
            lib: RealNumbersLib::ID,
            info: TokenInfo::atom(2),
            semantic: SemanticKind::Number,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        });

        // Add an operator token
        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(3), Pos::new(4)),
            lib: RealNumbersLib::ID,
            info: TokenInfo::atom(1),
            semantic: SemanticKind::Operator,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        });

        let tokens = semantic_tokens(&analysis);

        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, SemanticTokenType::Number);
        assert_eq!(tokens[1].token_type, SemanticTokenType::Operator);
    }

    #[test]
    fn semantic_tokens_skips_invalid() {
        let mut analysis = make_analysis();

        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(3)),
            lib: RealNumbersLib::ID,
            info: TokenInfo::atom(3),
            semantic: SemanticKind::Invalid,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: Some("test error".to_string()),
        });

        let tokens = semantic_tokens(&analysis);
        assert!(tokens.is_empty());
    }

    #[test]
    fn semantic_tokens_definition_modifier() {
        use rpl_lang::analysis::token::DefinitionId;

        let mut analysis = make_analysis();

        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(2)),
            lib: RealNumbersLib::ID,
            info: TokenInfo::atom(2),
            semantic: SemanticKind::Variable,
            context: TokenContext::default(),
            defines: Some(DefinitionId(0)),
            references: None,
            literal_type: None,
            error: None,
        });

        let tokens = semantic_tokens(&analysis);

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, SemanticTokenType::Variable);
        assert!(tokens[0].modifiers & SemanticModifier::Definition.as_u32() != 0);
    }

    #[test]
    fn encode_semantic_tokens_basic() {
        use rpl_source::SourceFile;

        let source = SourceFile::new(SourceId::new(0), "test".into(), "42 +".into());

        let tokens = vec![
            SemanticToken::new(
                Span::new(Pos::new(0), Pos::new(2)),
                SemanticTokenType::Number,
            ),
            SemanticToken::new(
                Span::new(Pos::new(3), Pos::new(4)),
                SemanticTokenType::Operator,
            ),
        ];

        let data = encode_semantic_tokens(&tokens, &source);

        // First token: line 0, char 0, length 2, type Number(0), modifiers 0
        assert_eq!(data[0], 0); // delta line
        assert_eq!(data[1], 0); // delta char
        assert_eq!(data[2], 2); // length
        assert_eq!(data[3], 0); // token type (Number)
        assert_eq!(data[4], 0); // modifiers

        // Second token: same line, char 3, length 1, type Operator(4)
        assert_eq!(data[5], 0); // delta line (same line)
        assert_eq!(data[6], 3); // delta char (0 -> 3 = 3)
        assert_eq!(data[7], 1); // length
        assert_eq!(data[8], 4); // token type (Operator)
    }
}
