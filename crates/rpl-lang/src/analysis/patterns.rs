use rpl_core::{Interner, Span, Symbol, TypeId};
use rpl_source::SourceFile;

use super::token::ResolvedToken;

/// Kind of symbol pattern.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PatternKind {
    /// Global variable definition (STO pattern).
    GlobalDefine,
    /// Global variable reference (RCL or bare name).
    GlobalReference,
    /// Global variable modification (STO+, etc.).
    GlobalModify,
    /// Global variable deletion (PURGE).
    GlobalDelete,
    /// Local variable definition (→).
    LocalDefine,
    /// Loop variable (FOR).
    LoopVariable,
}

/// A matched name within a pattern.
#[derive(Clone, Debug)]
pub struct NameMatch {
    pub name: Symbol,
    pub span: Span,
}

impl NameMatch {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }
}

/// Result of matching a symbol pattern.
#[derive(Clone, Debug)]
pub struct PatternMatch {
    /// Kind of pattern matched.
    pub kind: PatternKind,
    /// Span covering the entire pattern.
    pub span: Span,
    /// Names matched within the pattern.
    pub names: Vec<NameMatch>,
    /// Token range covered by this pattern (start, end exclusive).
    pub token_range: (usize, usize),
    /// Inferred type of the value being stored (for GlobalDefine patterns).
    pub value_type: Option<TypeId>,
}

impl PatternMatch {
    pub fn new(
        kind: PatternKind,
        span: Span,
        names: Vec<NameMatch>,
        token_range: (usize, usize),
    ) -> Self {
        Self {
            kind,
            span,
            names,
            token_range,
            value_type: None,
        }
    }

    /// Create a pattern match with a known value type.
    pub fn with_type(
        kind: PatternKind,
        span: Span,
        names: Vec<NameMatch>,
        token_range: (usize, usize),
        value_type: TypeId,
    ) -> Self {
        Self {
            kind,
            span,
            names,
            token_range,
            value_type: Some(value_type),
        }
    }

    /// Get the number of tokens this pattern covers.
    pub fn token_count(&self) -> usize {
        self.token_range.1 - self.token_range.0
    }
}

/// Type for pattern detector functions.
///
/// Parameters:
/// - tokens: The token stream
/// - pos: Current position in the token stream
/// - source: Source file for extracting token text
/// - interner: String interner for creating symbols
pub type PatternDetector =
    fn(&[ResolvedToken], usize, &SourceFile, &mut Interner) -> Option<PatternMatch>;

/// A symbol pattern with its detector.
pub struct SymbolPattern {
    /// Kind of pattern.
    pub kind: PatternKind,
    /// Detector function.
    pub detector: PatternDetector,
}

impl SymbolPattern {
    pub fn new(kind: PatternKind, detector: PatternDetector) -> Self {
        Self { kind, detector }
    }

    /// Try to match this pattern at the given position.
    pub fn try_match(
        &self,
        tokens: &[ResolvedToken],
        pos: usize,
        source: &SourceFile,
        interner: &mut Interner,
    ) -> Option<PatternMatch> {
        (self.detector)(tokens, pos, source, interner)
    }
}

/// Registry of symbol patterns.
pub struct PatternRegistry {
    patterns: Vec<SymbolPattern>,
}

impl PatternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
        }
    }

    /// Register a pattern.
    pub fn register(&mut self, pattern: SymbolPattern) {
        self.patterns.push(pattern);
    }

    /// Iterate over all registered patterns.
    pub fn iter(&self) -> impl Iterator<Item = &SymbolPattern> {
        self.patterns.iter()
    }

    /// Get the number of registered patterns.
    pub fn len(&self) -> usize {
        self.patterns.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    /// Try all patterns at the given position, returning the longest match.
    pub fn try_match(
        &self,
        tokens: &[ResolvedToken],
        pos: usize,
        source: &SourceFile,
        interner: &mut Interner,
    ) -> Option<PatternMatch> {
        let mut best_match: Option<PatternMatch> = None;

        for pattern in &self.patterns {
            if let Some(matched) = pattern.try_match(tokens, pos, source, interner) {
                match &best_match {
                    None => best_match = Some(matched),
                    Some(current) if matched.token_count() > current.token_count() => {
                        best_match = Some(matched);
                    }
                    _ => {}
                }
            }
        }

        best_match
    }
}

impl Default for PatternRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Built-in Pattern Detectors
// ============================================================================

/// Extract the inner name from a quoted symbol token.
///
/// For a token like `'x'`, this returns `"x"`.
/// For a token like `'foo'`, this returns `"foo"`.
fn extract_quoted_name(text: &str) -> Option<&str> {
    // A quoted symbol starts with ' and ends with '
    if text.len() >= 2 && text.starts_with('\'') && text.ends_with('\'') {
        Some(&text[1..text.len() - 1])
    } else {
        None
    }
}

/// Check if a token text matches a command name (case-insensitive).
fn is_command(text: &str, expected: &str) -> bool {
    text.eq_ignore_ascii_case(expected)
}

/// Infer the type of a value from a token.
///
/// This looks at the token preceding a STO to determine what type is being stored.
fn infer_value_type(token: &ResolvedToken, source: &SourceFile) -> Option<TypeId> {
    use rpl_core::token::SemanticKind;

    // If the token has a literal_type, use that
    if let Some(ty) = token.literal_type {
        return Some(ty);
    }

    // Infer type from semantic kind
    match token.semantic {
        SemanticKind::Number => Some(TypeId::REAL),
        SemanticKind::String => Some(TypeId::STRING),
        // If the previous token ends a construct, check what kind
        _ if token.context.ends_construct => {
            let text = source.span_text(token.span);
            match text {
                ">>" | "»" => Some(TypeId::PROGRAM),
                "}" => Some(TypeId::LIST),
                "'" => Some(TypeId::SYMBOLIC),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Detect `'name' STO` pattern (GlobalDefine).
pub fn detect_sto_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    // Get token text
    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    // Check for 'name' STO pattern
    let inner_name = extract_quoted_name(name_text)?;
    if !is_command(cmd_text, "STO") {
        return None;
    }

    // Intern the name
    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    // Try to infer the type of the value being stored
    // Look at the token before the pattern (if any)
    let value_type = if pos > 0 {
        infer_value_type(&tokens[pos - 1], source)
    } else {
        None
    };

    let mut pattern = PatternMatch::new(
        PatternKind::GlobalDefine,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    );
    pattern.value_type = value_type;
    Some(pattern)
}

/// Detect `'name' RCL` pattern (GlobalReference).
pub fn detect_rcl_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_quoted_name(name_text)?;
    if !is_command(cmd_text, "RCL") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalReference,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `'name' PURGE` pattern (GlobalDelete).
pub fn detect_purge_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_quoted_name(name_text)?;
    if !is_command(cmd_text, "PURGE") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalDelete,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Extract the inner content from a string literal token.
///
/// For a token like `"x"`, this returns `"x"`.
/// For a token like `"foo"`, this returns `"foo"`.
fn extract_string_content(text: &str) -> Option<&str> {
    if text.len() >= 2 && text.starts_with('"') && text.ends_with('"') {
        Some(&text[1..text.len() - 1])
    } else {
        None
    }
}

/// Detect `"name" STO` pattern (GlobalDefine with string name).
pub fn detect_string_sto_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_string_content(name_text)?;
    if !is_command(cmd_text, "STO") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalDefine,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `"name" RCL` pattern (GlobalReference with string name).
pub fn detect_string_rcl_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_string_content(name_text)?;
    if !is_command(cmd_text, "RCL") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalReference,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `"name" PURGE` pattern (GlobalDelete with string name).
pub fn detect_string_purge_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_string_content(name_text)?;
    if !is_command(cmd_text, "PURGE") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalDelete,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `"name" →` or `"name" ->` pattern (LocalDefine).
pub fn detect_local_define_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let arrow_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let arrow_text = source.span_text(arrow_token.span);

    // Check for string name
    let inner_name = extract_string_content(name_text)?;

    // Check for → or ->
    if arrow_text != "→" && arrow_text != "->" {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), arrow_token.span.end());

    Some(PatternMatch::new(
        PatternKind::LocalDefine,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `'name' STO+` pattern (GlobalModify - increment).
pub fn detect_sto_plus_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    // Support both quoted and string names
    let inner_name = extract_quoted_name(name_text)
        .or_else(|| extract_string_content(name_text))?;

    if !is_command(cmd_text, "STO+") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalModify,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `'name' STO-` pattern (GlobalModify - decrement).
pub fn detect_sto_minus_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_quoted_name(name_text)
        .or_else(|| extract_string_content(name_text))?;

    if !is_command(cmd_text, "STO-") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalModify,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `'name' STO*` pattern (GlobalModify - multiply).
pub fn detect_sto_times_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_quoted_name(name_text)
        .or_else(|| extract_string_content(name_text))?;

    if !is_command(cmd_text, "STO*") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalModify,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Detect `'name' STO/` pattern (GlobalModify - divide).
pub fn detect_sto_divide_pattern(
    tokens: &[ResolvedToken],
    pos: usize,
    source: &SourceFile,
    interner: &mut Interner,
) -> Option<PatternMatch> {
    if pos + 1 >= tokens.len() {
        return None;
    }

    let name_token = &tokens[pos];
    let cmd_token = &tokens[pos + 1];

    let name_text = source.span_text(name_token.span);
    let cmd_text = source.span_text(cmd_token.span);

    let inner_name = extract_quoted_name(name_text)
        .or_else(|| extract_string_content(name_text))?;

    if !is_command(cmd_text, "STO/") {
        return None;
    }

    let symbol = interner.intern(inner_name);
    let span = Span::new(name_token.span.start(), cmd_token.span.end());

    Some(PatternMatch::new(
        PatternKind::GlobalModify,
        span,
        vec![NameMatch::new(symbol, name_token.span)],
        (pos, pos + 2),
    ))
}

/// Create a PatternRegistry with built-in patterns registered.
pub fn default_pattern_registry() -> PatternRegistry {
    let mut registry = PatternRegistry::new();

    // Quoted name patterns ('name' STO, etc.)
    registry.register(SymbolPattern::new(
        PatternKind::GlobalDefine,
        detect_sto_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalReference,
        detect_rcl_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalDelete,
        detect_purge_pattern,
    ));

    // String name patterns ("name" STO, etc.)
    registry.register(SymbolPattern::new(
        PatternKind::GlobalDefine,
        detect_string_sto_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalReference,
        detect_string_rcl_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalDelete,
        detect_string_purge_pattern,
    ));

    // Local variable pattern ("name" →)
    registry.register(SymbolPattern::new(
        PatternKind::LocalDefine,
        detect_local_define_pattern,
    ));

    // Global modify patterns (STO+, STO-, STO*, STO/)
    registry.register(SymbolPattern::new(
        PatternKind::GlobalModify,
        detect_sto_plus_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalModify,
        detect_sto_minus_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalModify,
        detect_sto_times_pattern,
    ));
    registry.register(SymbolPattern::new(
        PatternKind::GlobalModify,
        detect_sto_divide_pattern,
    ));

    registry
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos};
    use crate::library::LibraryId;
    use rpl_source::SourceId;
    use rpl_core::token::{SemanticKind, TokenInfo};

    fn make_token(start: u32, end: u32) -> ResolvedToken {
        ResolvedToken::new(
            Span::new(Pos::new(start), Pos::new(end)),
            LibraryId::new(1),
            TokenInfo::atom(1),
            SemanticKind::Command,
        )
    }

    fn make_source(text: &str) -> SourceFile {
        SourceFile::new(SourceId::new(0), "test.rpl".to_string(), text.to_string())
    }

    // Mock detector that always matches 1 token
    fn mock_single_detector(
        tokens: &[ResolvedToken],
        pos: usize,
        _source: &SourceFile,
        _interner: &mut Interner,
    ) -> Option<PatternMatch> {
        if pos < tokens.len() {
            Some(PatternMatch::new(
                PatternKind::GlobalReference,
                tokens[pos].span,
                vec![],
                (pos, pos + 1),
            ))
        } else {
            None
        }
    }

    // Mock detector that matches 2 tokens
    fn mock_double_detector(
        tokens: &[ResolvedToken],
        pos: usize,
        _source: &SourceFile,
        _interner: &mut Interner,
    ) -> Option<PatternMatch> {
        if pos + 1 < tokens.len() {
            let span = Span::new(tokens[pos].span.start(), tokens[pos + 1].span.end());
            Some(PatternMatch::new(
                PatternKind::GlobalDefine,
                span,
                vec![],
                (pos, pos + 2),
            ))
        } else {
            None
        }
    }

    // Mock detector that never matches
    fn mock_no_match_detector(
        _tokens: &[ResolvedToken],
        _pos: usize,
        _source: &SourceFile,
        _interner: &mut Interner,
    ) -> Option<PatternMatch> {
        None
    }

    #[test]
    fn pattern_kind_equality() {
        assert_eq!(PatternKind::GlobalDefine, PatternKind::GlobalDefine);
        assert_ne!(PatternKind::GlobalDefine, PatternKind::LocalDefine);
    }

    #[test]
    fn name_match_new() {
        let mut interner = Interner::new();
        let name = interner.intern("test");
        let span = Span::new(Pos::new(0), Pos::new(5));
        let nm = NameMatch::new(name, span);
        assert_eq!(nm.name, name);
        assert_eq!(nm.span, span);
    }

    #[test]
    fn pattern_match_new() {
        let span = Span::new(Pos::new(0), Pos::new(10));
        let pm = PatternMatch::new(PatternKind::GlobalDefine, span, vec![], (0, 2));
        assert_eq!(pm.kind, PatternKind::GlobalDefine);
        assert_eq!(pm.token_count(), 2);
    }

    #[test]
    fn symbol_pattern_try_match() {
        let tokens = vec![make_token(0, 5), make_token(6, 10)];
        let source = make_source("test content");
        let mut interner = Interner::new();

        let pattern = SymbolPattern::new(PatternKind::GlobalReference, mock_single_detector);
        let result = pattern.try_match(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        assert_eq!(result.unwrap().kind, PatternKind::GlobalReference);
    }

    #[test]
    fn pattern_registry_new() {
        let registry = PatternRegistry::new();
        assert!(registry.is_empty());
        assert_eq!(registry.len(), 0);
    }

    #[test]
    fn pattern_registry_register() {
        let mut registry = PatternRegistry::new();

        registry.register(SymbolPattern::new(
            PatternKind::GlobalReference,
            mock_single_detector,
        ));
        registry.register(SymbolPattern::new(
            PatternKind::GlobalDefine,
            mock_double_detector,
        ));

        assert_eq!(registry.len(), 2);
        assert!(!registry.is_empty());
    }

    #[test]
    fn pattern_registry_iter() {
        let mut registry = PatternRegistry::new();
        registry.register(SymbolPattern::new(
            PatternKind::GlobalReference,
            mock_single_detector,
        ));
        registry.register(SymbolPattern::new(
            PatternKind::GlobalDefine,
            mock_double_detector,
        ));

        let kinds: Vec<_> = registry.iter().map(|p| p.kind).collect();
        assert_eq!(kinds.len(), 2);
        assert!(kinds.contains(&PatternKind::GlobalReference));
        assert!(kinds.contains(&PatternKind::GlobalDefine));
    }

    #[test]
    fn pattern_registry_try_match_longest() {
        let mut registry = PatternRegistry::new();

        // Register single-token pattern first
        registry.register(SymbolPattern::new(
            PatternKind::GlobalReference,
            mock_single_detector,
        ));
        // Register double-token pattern second
        registry.register(SymbolPattern::new(
            PatternKind::GlobalDefine,
            mock_double_detector,
        ));

        let tokens = vec![make_token(0, 5), make_token(6, 10), make_token(11, 15)];
        let source = make_source("test content for tokens");
        let mut interner = Interner::new();

        // Should return the longer match (double-token)
        let result = registry.try_match(&tokens, 0, &source, &mut interner);
        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalDefine);
        assert_eq!(matched.token_count(), 2);
    }

    #[test]
    fn pattern_registry_try_match_no_match() {
        let mut registry = PatternRegistry::new();
        registry.register(SymbolPattern::new(
            PatternKind::GlobalReference,
            mock_no_match_detector,
        ));

        let tokens = vec![make_token(0, 5)];
        let source = make_source("test");
        let mut interner = Interner::new();

        let result = registry.try_match(&tokens, 0, &source, &mut interner);
        assert!(result.is_none());
    }

    #[test]
    fn pattern_registry_try_match_empty() {
        let registry = PatternRegistry::new();
        let tokens = vec![make_token(0, 5)];
        let source = make_source("test");
        let mut interner = Interner::new();

        let result = registry.try_match(&tokens, 0, &source, &mut interner);
        assert!(result.is_none());
    }

    // ========================================================================
    // Built-in pattern detector tests
    // ========================================================================

    #[test]
    fn extract_quoted_name_valid() {
        assert_eq!(extract_quoted_name("'x'"), Some("x"));
        assert_eq!(extract_quoted_name("'foo'"), Some("foo"));
        assert_eq!(extract_quoted_name("'MyVar'"), Some("MyVar"));
    }

    #[test]
    fn extract_quoted_name_invalid() {
        assert_eq!(extract_quoted_name("x"), None);
        assert_eq!(extract_quoted_name("'x"), None);
        assert_eq!(extract_quoted_name("x'"), None);
        assert_eq!(extract_quoted_name("''"), Some("")); // empty name is valid syntax
        assert_eq!(extract_quoted_name("'"), None); // too short
    }

    #[test]
    fn detect_sto_pattern_matches() {
        // Source: "'x' STO"
        let source = make_source("'x' STO");
        let tokens = vec![make_token(0, 3), make_token(4, 7)]; // 'x' at 0-3, STO at 4-7
        let mut interner = Interner::new();

        let result = detect_sto_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalDefine);
        assert_eq!(matched.token_count(), 2);
        assert_eq!(matched.names.len(), 1);

        // Check the symbol was interned correctly
        let sym = matched.names[0].name;
        assert_eq!(interner.resolve(sym), "x");
    }

    #[test]
    fn detect_sto_pattern_case_insensitive() {
        let source = make_source("'x' sto");
        let tokens = vec![make_token(0, 3), make_token(4, 7)];
        let mut interner = Interner::new();

        let result = detect_sto_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_some());
    }

    #[test]
    fn detect_sto_pattern_no_match() {
        // Not a quoted symbol
        let source = make_source("x STO");
        let tokens = vec![make_token(0, 1), make_token(2, 5)];
        let mut interner = Interner::new();

        let result = detect_sto_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_none());
    }

    #[test]
    fn detect_rcl_pattern_matches() {
        let source = make_source("'y' RCL");
        let tokens = vec![make_token(0, 3), make_token(4, 7)];
        let mut interner = Interner::new();

        let result = detect_rcl_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalReference);
        assert_eq!(interner.resolve(matched.names[0].name), "y");
    }

    #[test]
    fn detect_purge_pattern_matches() {
        let source = make_source("'z' PURGE");
        let tokens = vec![make_token(0, 3), make_token(4, 9)];
        let mut interner = Interner::new();

        let result = detect_purge_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalDelete);
        assert_eq!(interner.resolve(matched.names[0].name), "z");
    }

    #[test]
    fn default_pattern_registry_has_builtin_patterns() {
        let registry = default_pattern_registry();
        // 3 quoted patterns + 3 string patterns + 1 local + 4 modify = 11
        assert_eq!(registry.len(), 11);
    }

    #[test]
    fn default_pattern_registry_detects_all_patterns() {
        let registry = default_pattern_registry();

        // Test STO detection
        {
            let source = make_source("'a' STO");
            let tokens = vec![make_token(0, 3), make_token(4, 7)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::GlobalDefine);
        }

        // Test RCL detection
        {
            let source = make_source("'b' RCL");
            let tokens = vec![make_token(0, 3), make_token(4, 7)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::GlobalReference);
        }

        // Test PURGE detection
        {
            let source = make_source("'c' PURGE");
            let tokens = vec![make_token(0, 3), make_token(4, 9)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::GlobalDelete);
        }
    }

    // ========================================================================
    // String-based pattern tests
    // ========================================================================

    #[test]
    fn extract_string_content_valid() {
        assert_eq!(extract_string_content("\"x\""), Some("x"));
        assert_eq!(extract_string_content("\"foo\""), Some("foo"));
        assert_eq!(extract_string_content("\"MyVar\""), Some("MyVar"));
    }

    #[test]
    fn extract_string_content_invalid() {
        assert_eq!(extract_string_content("x"), None);
        assert_eq!(extract_string_content("\"x"), None);
        assert_eq!(extract_string_content("x\""), None);
        assert_eq!(extract_string_content("\"\""), Some("")); // empty is valid
        assert_eq!(extract_string_content("\""), None); // too short
    }

    #[test]
    fn detect_string_sto_pattern_matches() {
        // Source: "x" STO
        let source = make_source("\"x\" STO");
        let tokens = vec![make_token(0, 3), make_token(4, 7)];
        let mut interner = Interner::new();

        let result = detect_string_sto_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalDefine);
        assert_eq!(matched.token_count(), 2);
        assert_eq!(matched.names.len(), 1);
        assert_eq!(interner.resolve(matched.names[0].name), "x");
    }

    #[test]
    fn detect_string_rcl_pattern_matches() {
        let source = make_source("\"y\" RCL");
        let tokens = vec![make_token(0, 3), make_token(4, 7)];
        let mut interner = Interner::new();

        let result = detect_string_rcl_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalReference);
        assert_eq!(interner.resolve(matched.names[0].name), "y");
    }

    #[test]
    fn detect_string_purge_pattern_matches() {
        let source = make_source("\"z\" PURGE");
        let tokens = vec![make_token(0, 3), make_token(4, 9)];
        let mut interner = Interner::new();

        let result = detect_string_purge_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalDelete);
        assert_eq!(interner.resolve(matched.names[0].name), "z");
    }

    // ========================================================================
    // Local define pattern tests
    // ========================================================================

    #[test]
    fn detect_local_define_arrow_unicode() {
        let source = make_source("\"x\" →");
        // "x" is 3 chars (including quotes), → is 3 bytes in UTF-8
        let tokens = vec![make_token(0, 3), make_token(4, 7)];
        let mut interner = Interner::new();

        let result = detect_local_define_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::LocalDefine);
        assert_eq!(matched.names.len(), 1);
        assert_eq!(interner.resolve(matched.names[0].name), "x");
    }

    #[test]
    fn detect_local_define_arrow_ascii() {
        let source = make_source("\"y\" ->");
        let tokens = vec![make_token(0, 3), make_token(4, 6)];
        let mut interner = Interner::new();

        let result = detect_local_define_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::LocalDefine);
        assert_eq!(interner.resolve(matched.names[0].name), "y");
    }

    #[test]
    fn detect_local_define_no_match_wrong_arrow() {
        let source = make_source("\"x\" <-");
        let tokens = vec![make_token(0, 3), make_token(4, 6)];
        let mut interner = Interner::new();

        let result = detect_local_define_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_none());
    }

    // ========================================================================
    // Global modify pattern tests
    // ========================================================================

    #[test]
    fn detect_sto_plus_quoted() {
        let source = make_source("'x' STO+");
        let tokens = vec![make_token(0, 3), make_token(4, 8)];
        let mut interner = Interner::new();

        let result = detect_sto_plus_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalModify);
        assert_eq!(interner.resolve(matched.names[0].name), "x");
    }

    #[test]
    fn detect_sto_plus_string() {
        let source = make_source("\"x\" STO+");
        let tokens = vec![make_token(0, 3), make_token(4, 8)];
        let mut interner = Interner::new();

        let result = detect_sto_plus_pattern(&tokens, 0, &source, &mut interner);

        assert!(result.is_some());
        let matched = result.unwrap();
        assert_eq!(matched.kind, PatternKind::GlobalModify);
    }

    #[test]
    fn detect_sto_minus_matches() {
        let source = make_source("'x' STO-");
        let tokens = vec![make_token(0, 3), make_token(4, 8)];
        let mut interner = Interner::new();

        let result = detect_sto_minus_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_some());
        assert_eq!(result.unwrap().kind, PatternKind::GlobalModify);
    }

    #[test]
    fn detect_sto_times_matches() {
        let source = make_source("'x' STO*");
        let tokens = vec![make_token(0, 3), make_token(4, 8)];
        let mut interner = Interner::new();

        let result = detect_sto_times_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_some());
        assert_eq!(result.unwrap().kind, PatternKind::GlobalModify);
    }

    #[test]
    fn detect_sto_divide_matches() {
        let source = make_source("'x' STO/");
        let tokens = vec![make_token(0, 3), make_token(4, 8)];
        let mut interner = Interner::new();

        let result = detect_sto_divide_pattern(&tokens, 0, &source, &mut interner);
        assert!(result.is_some());
        assert_eq!(result.unwrap().kind, PatternKind::GlobalModify);
    }

    #[test]
    fn default_registry_detects_string_patterns() {
        let registry = default_pattern_registry();

        // Test "name" STO
        {
            let source = make_source("\"a\" STO");
            let tokens = vec![make_token(0, 3), make_token(4, 7)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::GlobalDefine);
        }

        // Test "name" RCL
        {
            let source = make_source("\"b\" RCL");
            let tokens = vec![make_token(0, 3), make_token(4, 7)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::GlobalReference);
        }

        // Test "name" →
        {
            let source = make_source("\"c\" →");
            let tokens = vec![make_token(0, 3), make_token(4, 7)];
            let mut interner = Interner::new();
            let result = registry.try_match(&tokens, 0, &source, &mut interner);
            assert!(result.is_some());
            assert_eq!(result.unwrap().kind, PatternKind::LocalDefine);
        }
    }
}
