use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use rpl_core::{
    Interner, Pos, Span,
    error::{Diagnostic, ErrorCode},
    token::{SemanticKind, TokenInfo},
};
use rpl_source::SourceFile;

use super::{
    result::AnalysisResult,
    state::{LineState, ParseState},
    token::ResolvedToken,
};
use crate::{
    library::{ConstructKind, LibraryRegistry, ProbeContext, ProbeResult, StackEffect},
    user_libs::UserLibraryRegistry,
};

/// Tokenizer for RPL source code.
pub struct Tokenizer<'a> {
    source: &'a SourceFile,
    registry: &'a LibraryRegistry,
    interner: &'a mut Interner,
    user_lib_registry: Option<&'a UserLibraryRegistry>,
    pos: usize,
    state: ParseState,
    tokens: Vec<ResolvedToken>,
    diagnostics: Vec<Diagnostic>,
    line_states: Vec<LineState>,
    current_line: u32,
    line_start_token: u32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        source: &'a SourceFile,
        registry: &'a LibraryRegistry,
        interner: &'a mut Interner,
    ) -> Self {
        Self {
            source,
            registry,
            interner,
            user_lib_registry: None,
            pos: 0,
            state: ParseState::new(),
            tokens: Vec::new(),
            diagnostics: Vec::new(),
            line_states: Vec::new(),
            current_line: 1,
            line_start_token: 0,
        }
    }

    /// Create a tokenizer with access to the user library registry for compile-time resolution.
    pub fn with_user_lib_registry(
        source: &'a SourceFile,
        registry: &'a LibraryRegistry,
        interner: &'a mut Interner,
        user_lib_registry: &'a UserLibraryRegistry,
    ) -> Self {
        Self {
            source,
            registry,
            interner,
            user_lib_registry: Some(user_lib_registry),
            pos: 0,
            state: ParseState::new(),
            tokens: Vec::new(),
            diagnostics: Vec::new(),
            line_states: Vec::new(),
            current_line: 1,
            line_start_token: 0,
        }
    }

    /// Tokenize the entire source file.
    pub fn tokenize(mut self) -> AnalysisResult {
        self.start_line();

        while self.skip_whitespace() {
            if let Some(span) = self.extract_raw_token() {
                let text = self.source.span_text(span).to_string();
                let token = self.probe_token(&text, span);
                self.tokens.push(token);
            }
        }

        self.finish_line();
        self.resolve_context();
        self.build_result()
    }

    /// Resolve context-dependent token properties.
    fn resolve_context(&mut self) {
        super::resolver::resolve_context(&mut self.tokens);
    }

    /// Skip whitespace, tracking line changes. Returns true if more input.
    fn skip_whitespace(&mut self) -> bool {
        let src = self.source.source();
        let bytes = src.as_bytes();

        while self.pos < bytes.len() {
            match bytes[self.pos] {
                b' ' | b'\t' | b'\r' => {
                    self.pos += 1;
                }
                b'\n' => {
                    self.finish_line();
                    self.pos += 1;
                    self.current_line += 1;
                    self.start_line();
                }
                _ => return true,
            }
        }
        false
    }

    /// Extract a raw token from the current position.
    fn extract_raw_token(&mut self) -> Option<Span> {
        let src = self.source.source();
        if self.pos >= src.len() {
            return None;
        }

        let start = self.pos;
        let bytes = src.as_bytes();

        // Handle string literals
        if bytes[self.pos] == b'"' {
            return self.extract_string_literal();
        }

        // Handle comments (@ for single-line, @@ for permanent, @@@ for multi-line)
        if bytes[self.pos] == b'@' {
            return self.extract_comment();
        }

        // Handle special single-character delimiters that should be their own tokens
        match bytes[self.pos] {
            b'(' | b')' | b'[' | b']' | b'{' | b'}' | b',' | b'\'' => {
                self.pos += 1;
                return Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)));
            }
            _ => {}
        }

        // Read until whitespace or delimiter
        while self.pos < bytes.len() {
            match bytes[self.pos] {
                b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')' | b'[' | b']' | b'{' | b'}' | b','
                | b'"' | b'\'' => break,
                _ => self.pos += 1,
            }
        }

        if self.pos > start {
            Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)))
        } else {
            None
        }
    }

    /// Extract a string literal (handles unterminated strings and escape sequences).
    fn extract_string_literal(&mut self) -> Option<Span> {
        let src = self.source.source();
        let start = self.pos;
        let bytes = src.as_bytes();

        // Skip opening quote
        self.pos += 1;

        // Find closing quote or end of line, handling escape sequences
        while self.pos < bytes.len() {
            match bytes[self.pos] {
                b'\\' => {
                    // Skip escape character and the following character
                    self.pos += 1;
                    if self.pos < bytes.len() && bytes[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                }
                b'"' => {
                    self.pos += 1;
                    return Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)));
                }
                b'\n' => {
                    // Unterminated string - stop at newline
                    let span = Span::new(Pos::new(start as u32), Pos::new(self.pos as u32));
                    self.diagnostics.push(
                        Diagnostic::error(ErrorCode::E002, span)
                            .message("unterminated string literal")
                            .label("string starts here but is not closed")
                            .build(),
                    );
                    return Some(span);
                }
                _ => self.pos += 1,
            }
        }

        // End of file without closing quote
        let span = Span::new(Pos::new(start as u32), Pos::new(self.pos as u32));
        self.diagnostics.push(
            Diagnostic::error(ErrorCode::E002, span)
                .message("unterminated string literal")
                .label("string starts here but is not closed")
                .build(),
        );
        Some(span)
    }

    /// Extract a comment (@ for single-line, @@ for permanent, @@@ for multi-line).
    fn extract_comment(&mut self) -> Option<Span> {
        let src = self.source.source();
        let start = self.pos;
        let bytes = src.as_bytes();

        // Count leading @ symbols
        let mut at_count = 0;
        while self.pos < bytes.len() && bytes[self.pos] == b'@' {
            at_count += 1;
            self.pos += 1;
        }

        if at_count >= 3 {
            // Multi-line comment: find closing @@@
            while self.pos < bytes.len() {
                if bytes[self.pos] == b'@' {
                    // Check for at least 3 @s
                    let close_start = self.pos;
                    let mut close_count = 0;
                    while self.pos < bytes.len() && bytes[self.pos] == b'@' {
                        close_count += 1;
                        self.pos += 1;
                    }
                    if close_count >= 3 {
                        // Found closing @@@
                        return Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)));
                    }
                    // Not enough @s, continue searching
                    // (pos is already advanced past the @s we found)
                    if self.pos == close_start {
                        self.pos += 1;
                    }
                } else if bytes[self.pos] == b'\n' {
                    // Track line changes in multi-line comment
                    self.finish_line();
                    self.pos += 1;
                    self.current_line += 1;
                    self.start_line();
                } else {
                    self.pos += 1;
                }
            }
            // Unclosed multi-line comment - take rest of input
            Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)))
        } else {
            // Single-line comment (@ or @@): ends at newline
            while self.pos < bytes.len() && bytes[self.pos] != b'\n' {
                self.pos += 1;
            }
            Some(Span::new(Pos::new(start as u32), Pos::new(self.pos as u32)))
        }
    }

    /// Probe libraries to identify a token.
    fn probe_token(&mut self, text: &str, span: Span) -> ResolvedToken {
        // Check user library commands FIRST (they take precedence over standard libraries)
        // This ensures that if a user library defines GET, it shadows the standard GET.
        if let Some(user_libs) = self.user_lib_registry
            && user_libs.lookup_command(text).is_some()
        {
            // Found in user library - return as a LibPtrLib match
            let mut token = ResolvedToken::new(
                span,
                crate::user_libs::LIBPTR_LIB_ID,
                TokenInfo::atom(text.len() as u8),
                SemanticKind::Command,
            );
            token.context.construct_depth = self.state.construct_stack.len() as u16;
            return token;
        }

        // Create ProbeContext with user library registry if available
        let ctx = if let Some(user_libs) = self.user_lib_registry {
            ProbeContext::with_user_lib_registry(
                text,
                self.source.source(),
                span,
                self.state.in_infix(),
                self.state.current_construct(),
                None, // previous token info - simplified for now
                self.interner,
                user_libs,
            )
        } else {
            ProbeContext::new(
                text,
                self.source.source(),
                span,
                self.state.in_infix(),
                self.state.current_construct(),
                None, // previous token info - simplified for now
                self.interner,
            )
        };

        match self.registry.probe(&ctx) {
            Some((lib_id, ProbeResult::Match { info, semantic })) => {
                let mut token = ResolvedToken::new(span, lib_id, info, semantic);
                // Set literal_type for number literals
                if semantic == SemanticKind::Number {
                    token.literal_type = Some(rpl_core::TypeId::REAL);
                }
                // Update state based on stack effect
                if let Some(lib) = self.registry.get(lib_id) {
                    self.apply_stack_effect(lib.stack_effect(text), text, &mut token);
                }
                // Track infix mode for symbolic expressions (quote character)
                if text == "'" {
                    use rpl_core::token::TokenType;
                    if info.ty() == TokenType::OpenBracket {
                        self.state.enter_infix();
                    } else if info.ty() == TokenType::CloseBracket {
                        self.state.exit_infix();
                    }
                }
                token
            }
            Some((lib_id, ProbeResult::NotAllowed { reason })) => {
                self.diagnostics.push(
                    Diagnostic::error(ErrorCode::E001, span)
                        .message(format!("token not allowed here: {}", reason))
                        .build(),
                );
                ResolvedToken::new(
                    span,
                    lib_id,
                    TokenInfo::atom(text.len() as u8),
                    SemanticKind::Invalid,
                )
            }
            Some((_, ProbeResult::NoMatch)) | None => {
                // Unknown token - create error token and advance
                self.diagnostics.push(
                    Diagnostic::error(ErrorCode::E001, span)
                        .message(format!("unrecognized token '{}'", text))
                        .label("not recognized")
                        .build(),
                );
                self.make_error_token(span, format!("unrecognized token '{}'", text))
            }
        }
    }

    /// Apply stack effect to state and token context.
    fn apply_stack_effect(&mut self, effect: StackEffect, text: &str, token: &mut ResolvedToken) {
        match effect {
            StackEffect::StartConstruct => {
                // Infer the construct kind from the token
                let kind = Self::infer_construct_kind(text);
                self.state.push_construct(kind);
                token.context.starts_construct = true;
                token.context.construct_depth = self.state.construct_stack.len() as u16;
            }
            StackEffect::EndConstruct => {
                self.state.pop_construct();
                token.context.ends_construct = true;
                token.context.construct_depth = self.state.construct_stack.len() as u16;
            }
            _ => {
                token.context.construct_depth = self.state.construct_stack.len() as u16;
            }
        }
    }

    /// Infer the construct kind from the token that starts it.
    fn infer_construct_kind(token: &str) -> ConstructKind {
        match token {
            "(" => ConstructKind::Complex,
            "{" => ConstructKind::List,
            "<<" | "::" => ConstructKind::Program,
            "→" | "->" => ConstructKind::LocalBinding,
            _ => {
                // Check control flow keywords (case-insensitive)
                match token.to_ascii_uppercase().as_str() {
                    "IF" => ConstructKind::If,
                    "CASE" => ConstructKind::Case,
                    "IFERR" => ConstructKind::ErrorHandler,
                    "DO" => ConstructKind::DoUntil,
                    "WHILE" => ConstructKind::While,
                    "FOR" => ConstructKind::For,
                    "FORUP" => ConstructKind::ForUp,
                    "FORDN" => ConstructKind::ForDn,
                    "START" => ConstructKind::Start,
                    _ => ConstructKind::Program,
                }
            }
        }
    }

    /// Create an error token for recovery.
    fn make_error_token(&self, span: Span, message: String) -> ResolvedToken {
        ResolvedToken::error(span, message)
    }

    /// Start tracking a new line.
    fn start_line(&mut self) {
        let state_before = self.state.clone();
        let hash = self.hash_current_line();
        self.line_start_token = self.tokens.len() as u32;
        self.line_states
            .push(LineState::new(hash, self.line_start_token, state_before));
    }

    /// Finish tracking the current line.
    fn finish_line(&mut self) {
        if let Some(line_state) = self.line_states.last_mut() {
            line_state.token_count = self.tokens.len() as u32 - line_state.first_token;
            line_state.state_after = self.state.clone();
        }
    }

    /// Hash the current line content for change detection.
    fn hash_current_line(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        if let Some(text) = self.source.line_text(self.current_line) {
            text.hash(&mut hasher);
        }
        hasher.finish()
    }

    /// Build the final analysis result.
    fn build_result(self) -> AnalysisResult {
        AnalysisResult {
            source_id: self.source.id(),
            version: 0,
            tokens: self.tokens,
            lines: self.line_states,
            diagnostics: self.diagnostics,
        }
    }
}

#[cfg(test)]
mod tests {
    use rpl_source::SourceId;

    use super::*;
    use crate::library::{
        CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteResult, Library,
        LibraryId,
    };

    // Mock library for testing
    struct MockNumberLib;

    impl Library for MockNumberLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(10)
        }

        fn name(&self) -> &'static str {
            "numbers"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            let text = ctx.text();
            if text.chars().all(|c| c.is_ascii_digit()) {
                ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Number,
                }
            } else {
                ProbeResult::NoMatch
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            ExecuteResult::Ok
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }
    }

    struct MockStackLib;

    impl Library for MockStackLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(72)
        }

        fn name(&self) -> &'static str {
            "stack"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            match ctx.text() {
                "DUP" | "DROP" | "SWAP" => ProbeResult::Match {
                    info: TokenInfo::atom(ctx.text().len() as u8),
                    semantic: SemanticKind::Command,
                },
                _ => ProbeResult::NoMatch,
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            ExecuteResult::Ok
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }
    }

    struct MockOpLib;

    impl Library for MockOpLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(64)
        }

        fn name(&self) -> &'static str {
            "operators"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            match ctx.text() {
                "+" | "-" | "*" | "/" => ProbeResult::Match {
                    info: TokenInfo::binary_left(1, 10),
                    semantic: SemanticKind::Operator,
                },
                _ => ProbeResult::NoMatch,
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            ExecuteResult::Ok
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }
    }

    fn make_registry() -> LibraryRegistry {
        let mut registry = LibraryRegistry::new();
        registry.register(MockNumberLib, 100);
        registry.register(MockStackLib, 50);
        registry.register(MockOpLib, 50);
        registry
    }

    #[test]
    fn tokenize_simple_program() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3 4 +".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
        assert_eq!(result.tokens[0].semantic, SemanticKind::Number);
        assert_eq!(result.tokens[1].semantic, SemanticKind::Number);
        assert_eq!(result.tokens[2].semantic, SemanticKind::Operator);
        assert!(!result.has_errors());
    }

    #[test]
    fn tokenize_with_commands() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3 DUP +".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
        assert_eq!(result.tokens[0].semantic, SemanticKind::Number);
        assert_eq!(result.tokens[1].semantic, SemanticKind::Command);
        assert_eq!(result.tokens[2].semantic, SemanticKind::Operator);
    }

    #[test]
    fn tokenize_whitespace_handling() {
        let source = SourceFile::new(
            SourceId::new(0),
            "test.rpl".into(),
            "  3   4  \t +  ".into(),
        );
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
    }

    #[test]
    fn tokenize_multiple_lines() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3\n4\n+".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.token_count(), 3);
        assert_eq!(result.lines.len(), 3);
    }

    #[test]
    fn tokenize_error_recovery() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3 foo 4".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // Should still produce 3 tokens, with the middle one being an error
        assert_eq!(result.token_count(), 3);
        assert!(result.tokens[1].is_error());
        assert!(result.has_errors());
        assert_eq!(result.diagnostics.len(), 1);
    }

    #[test]
    fn tokenize_line_states() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3 4\n+ DUP".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        assert_eq!(result.lines.len(), 2);
        assert_eq!(result.lines[0].token_count, 2); // "3 4"
        assert_eq!(result.lines[1].token_count, 2); // "+ DUP"
    }

    #[test]
    fn tokenize_state_tracking() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "3".into());
        let registry = make_registry();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // State before and after should be tracked
        assert!(!result.lines[0].state_before.in_construct());
        assert!(!result.lines[0].state_after.in_construct());
    }

    // Mock library for construct testing
    struct MockConstructLib;

    impl Library for MockConstructLib {
        fn id(&self) -> LibraryId {
            LibraryId::new(20)
        }

        fn name(&self) -> &'static str {
            "constructs"
        }

        fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
            match ctx.text() {
                "<<" => ProbeResult::Match {
                    info: TokenInfo::atom(2),
                    semantic: SemanticKind::Bracket,
                },
                ">>" => ProbeResult::Match {
                    info: TokenInfo::atom(2),
                    semantic: SemanticKind::Bracket,
                },
                _ => ProbeResult::NoMatch,
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut crate::library::ExecuteContext) -> ExecuteResult {
            ExecuteResult::Ok
        }

        fn decompile(&self, _ctx: &mut DecompileContext) -> DecompileResult {
            DecompileResult::Ok
        }

        fn stack_effect(&self, token: &str) -> StackEffect {
            match token {
                "<<" => StackEffect::StartConstruct,
                ">>" => StackEffect::EndConstruct,
                _ => StackEffect::Dynamic,
            }
        }
    }

    fn make_registry_with_constructs() -> LibraryRegistry {
        let mut registry = LibraryRegistry::new();
        registry.register(MockNumberLib, 100);
        registry.register(MockStackLib, 50);
        registry.register(MockOpLib, 50);
        registry.register(MockConstructLib, 80);
        registry
    }

    #[test]
    fn tokenize_construct_state_tracking() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "<< 3 4 + >>".into());
        let registry = make_registry_with_constructs();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // Should have 5 tokens: << 3 4 + >>
        assert_eq!(result.token_count(), 5);

        // First token starts construct
        assert!(result.tokens[0].context.starts_construct);
        assert!(!result.tokens[0].context.ends_construct);
        assert_eq!(result.tokens[0].context.construct_depth, 1);

        // Middle tokens are inside construct
        assert!(!result.tokens[1].context.starts_construct);
        assert!(!result.tokens[1].context.ends_construct);
        assert_eq!(result.tokens[1].context.construct_depth, 1);

        // Last token ends construct (depth is 0 after ending)
        assert!(!result.tokens[4].context.starts_construct);
        assert!(result.tokens[4].context.ends_construct);
        assert_eq!(result.tokens[4].context.construct_depth, 0);

        // State before line is not in construct, state after is not in construct
        assert!(!result.lines[0].state_before.in_construct());
        assert!(!result.lines[0].state_after.in_construct());
    }

    #[test]
    fn tokenize_nested_constructs() {
        let source = SourceFile::new(SourceId::new(0), "test.rpl".into(), "<< << 3 >> >>".into());
        let registry = make_registry_with_constructs();
        let mut interner = Interner::new();

        let result = Tokenizer::new(&source, &registry, &mut interner).tokenize();

        // Should have 5 tokens: << << 3 >> >>
        assert_eq!(result.token_count(), 5);

        // First << starts first construct (depth 1)
        assert_eq!(result.tokens[0].context.construct_depth, 1);
        assert!(result.tokens[0].context.starts_construct);

        // Second << starts nested construct (depth 2)
        assert_eq!(result.tokens[1].context.construct_depth, 2);
        assert!(result.tokens[1].context.starts_construct);

        // 3 is at depth 2
        assert_eq!(result.tokens[2].context.construct_depth, 2);

        // First >> ends inner construct (depth 1 after ending inner)
        assert!(result.tokens[3].context.ends_construct);
        assert_eq!(result.tokens[3].context.construct_depth, 1);

        // Second >> ends outer construct (depth 0 after ending outer)
        assert!(result.tokens[4].context.ends_construct);
        assert_eq!(result.tokens[4].context.construct_depth, 0);
    }
}
