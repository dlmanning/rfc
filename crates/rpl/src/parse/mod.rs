//! Parser for RPL source code.
//!
//! Converts source text to IR nodes.
//!
//! # Grammar
//!
//! ```text
//! program     = "«" expr* "»" | "<<" expr* ">>"
//! list        = "{" expr* "}"
//! symbolic    = "'" infix-expr "'"
//! string      = '"' char* '"'
//! integer     = decimal | hex | binary | hp-binary
//! real        = decimal "." decimal [exponent]
//! decimal     = digit+
//! hex         = "0x" hex-digit+
//! binary      = "0b" ("0" | "1")+
//! hp-binary   = "#" hex-digit+ ["h" | "b" | "o" | "d"]
//! command     = identifier (looked up in registry)
//! symbol      = identifier (unresolved name)
//! local-ref   = identifier (resolved to local variable)
//! expr        = program | list | symbolic | string | integer | real | command | symbol | local-ref
//! ```
//!
//! # Comment Syntax
//!
//! ```text
//! // ...      - C-style line comment
//! @ ...       - HP-style strippable comment
//! @@ ...      - HP-style permanent comment (not stripped by STRIPCOMMENTS)
//! @@@ ... @@@ - HP-style multi-line comment
//! ```

pub mod infix;

use std::collections::HashMap;

use crate::core::{Interner, Pos, Span, Symbol};

use crate::{ir::Node, libs::ClaimContext, registry::InterfaceRegistry};

/// Comment type based on @ prefix count.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentKind {
    /// `//` style comment (C/Rust style)
    DoubleSlash,
    /// Single `@` - single-line, can be stripped
    SingleLine,
    /// Double `@@` - single-line, permanent (not stripped by STRIPCOMMENTS)
    Permanent,
    /// Triple `@@@` - multi-line
    MultiLine,
}

/// A comment with its location and type.
#[derive(Clone, Debug)]
pub struct CommentSpan {
    /// The span of the comment in source.
    pub span: Span,
    /// The kind of comment.
    pub kind: CommentKind,
    /// The comment text (including markers like @ or //).
    pub text: String,
}

/// Source map containing auxiliary information from parsing.
#[derive(Clone, Debug, Default)]
pub struct SourceMap {
    /// All comments found during tokenization.
    pub comments: Vec<CommentSpan>,
}

impl SourceMap {
    /// Create a new empty source map.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Get all strippable comments (single @ style, not @@ or @@@).
    pub fn strippable_comments(&self) -> impl Iterator<Item = &CommentSpan> {
        self.comments
            .iter()
            .filter(|c| c.kind == CommentKind::SingleLine)
    }

    /// Get all permanent comments (@@ style).
    pub fn permanent_comments(&self) -> impl Iterator<Item = &CommentSpan> {
        self.comments
            .iter()
            .filter(|c| c.kind == CommentKind::Permanent)
    }
}

/// Parse error with context about what was expected and found.
#[derive(Clone, Debug, Default)]
pub struct ParseError {
    /// Human-readable error message.
    pub message: String,
    /// Location of the error.
    pub span: Span,
    /// What was expected (if applicable).
    #[doc(hidden)]
    pub expected: Option<String>,
    /// What was found (if applicable).
    #[doc(hidden)]
    pub found: Option<String>,
}

impl ParseError {
    /// Create a simple error with just a message.
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: None,
            found: None,
        }
    }

    /// Create an error with expected/found context.
    pub fn expected(expected: impl Into<String>, found: impl Into<String>, span: Span) -> Self {
        let expected = expected.into();
        let found = found.into();
        Self {
            message: format!("expected {}, found {}", expected, found),
            span,
            expected: Some(expected),
            found: Some(found),
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

/// A simple token from the source.
#[derive(Clone, Debug)]
pub struct Token {
    pub text: String,
    pub span: Span,
}

/// Check if a character is a delimiter that should be its own token.
fn is_delimiter(c: char) -> bool {
    matches!(
        c,
        '{' | '}' | '(' | ')' | '[' | ']' | ';' | '«' | '»' | '\''
    )
}

/// Tokenize source into tokens.
#[must_use]
pub fn tokenize(source: &str) -> Vec<Token> {
    tokenize_with_source_map(source).0
}

/// Tokenize source into tokens and collect comments into a source map.
#[must_use]
pub fn tokenize_with_source_map(source: &str) -> (Vec<Token>, SourceMap) {
    // Pre-allocate based on heuristic: ~1 token per 4 characters
    let mut tokens = Vec::with_capacity(source.len() / 4);
    let mut source_map = SourceMap::new();
    let mut chars = source.char_indices().peekable();

    while let Some(&(start, ch)) = chars.peek() {
        // Skip whitespace
        if ch.is_whitespace() {
            chars.next();
            continue;
        }

        // Handle comments (// to end of line)
        if ch == '/' {
            let mut peek_chars = chars.clone();
            peek_chars.next();
            if let Some(&(_, '/')) = peek_chars.peek() {
                let comment_start = start;
                let mut text = String::with_capacity(64);
                let mut end = start;
                // Collect comment text
                while let Some(&(i, c)) = chars.peek() {
                    text.push(c);
                    end = i + c.len_utf8();
                    chars.next();
                    if c == '\n' {
                        break;
                    }
                }
                source_map.comments.push(CommentSpan {
                    span: Span::new(Pos::new(comment_start as u32), Pos::new(end as u32)),
                    kind: CommentKind::DoubleSlash,
                    text,
                });
                continue;
            }
        }

        // Handle HP-style comments (@ to end of line, @@ permanent, @@@ multi-line @@@)
        if ch == '@' {
            let comment_start = start;
            chars.next();
            let mut at_count = 1;

            // Count consecutive @ symbols
            while let Some(&(_, '@')) = chars.peek() {
                chars.next();
                at_count += 1;
            }

            if at_count >= 3 {
                // Multi-line comment: find closing @@@
                let mut text = "@".repeat(at_count);
                let mut end = comment_start + at_count;
                while let Some(&(i, c)) = chars.peek() {
                    text.push(c);
                    end = i + c.len_utf8();
                    chars.next();
                    if c == '@' {
                        let mut close_count = 1;
                        while let Some(&(j, '@')) = chars.peek() {
                            text.push('@');
                            end = j + 1;
                            chars.next();
                            close_count += 1;
                        }
                        if close_count >= 3 {
                            break;
                        }
                    }
                }
                source_map.comments.push(CommentSpan {
                    span: Span::new(Pos::new(comment_start as u32), Pos::new(end as u32)),
                    kind: CommentKind::MultiLine,
                    text,
                });
            } else {
                // Single-line comment (@ or @@): collect to end of line
                let kind = if at_count >= 2 {
                    CommentKind::Permanent
                } else {
                    CommentKind::SingleLine
                };
                let mut text = "@".repeat(at_count);
                let mut end = comment_start + at_count;
                while let Some(&(i, c)) = chars.peek() {
                    if c == '\n' {
                        end = i + 1;
                        text.push(c);
                        chars.next();
                        break;
                    }
                    text.push(c);
                    end = i + c.len_utf8();
                    chars.next();
                }
                // Handle EOF without newline
                if chars.peek().is_none() {
                    end = source.len();
                }
                source_map.comments.push(CommentSpan {
                    span: Span::new(Pos::new(comment_start as u32), Pos::new(end as u32)),
                    kind,
                    text,
                });
            }
            continue;
        }

        // String literal with escape sequence support
        if ch == '"' {
            chars.next();
            let mut text = String::with_capacity(32);
            let mut end = start + 1;
            while let Some(&(i, c)) = chars.peek() {
                end = i + c.len_utf8();
                chars.next();
                if c == '"' {
                    break;
                }
                if c == '\\' {
                    // Handle escape sequence
                    if let Some(&(i2, escaped)) = chars.peek() {
                        end = i2 + escaped.len_utf8();
                        chars.next();
                        match escaped {
                            'n' => text.push('\n'),
                            't' => text.push('\t'),
                            'r' => text.push('\r'),
                            '"' => text.push('"'),
                            '\\' => text.push('\\'),
                            _ => {
                                // Unknown escape, keep as-is
                                text.push('\\');
                                text.push(escaped);
                            }
                        }
                    } else {
                        text.push(c);
                    }
                } else {
                    text.push(c);
                }
            }
            tokens.push(Token {
                text: format!("\"{}\"", text),
                span: Span::new(Pos::new(start as u32), Pos::new(end as u32)),
            });
            continue;
        }

        // Check for two-char tokens: << and >>
        if ch == '<' || ch == '>' {
            let mut peek_chars = chars.clone();
            peek_chars.next();
            if let Some(&(_, next_ch)) = peek_chars.peek()
                && ((ch == '<' && next_ch == '<') || (ch == '>' && next_ch == '>'))
            {
                chars.next();
                chars.next();
                let text = if ch == '<' { "<<" } else { ">>" };
                tokens.push(Token {
                    text: text.to_string(),
                    span: Span::new(Pos::new(start as u32), Pos::new((start + 2) as u32)),
                });
                continue;
            }
        }

        // Single-char delimiters
        if is_delimiter(ch) {
            chars.next();
            tokens.push(Token {
                text: ch.to_string(),
                span: Span::new(
                    Pos::new(start as u32),
                    Pos::new((start + ch.len_utf8()) as u32),
                ),
            });
            continue;
        }

        // Collect a word
        let mut text = String::with_capacity(16);
        let mut end = start;

        while let Some(&(i, c)) = chars.peek() {
            if c.is_whitespace() || is_delimiter(c) {
                break;
            }
            // Check for << or >> starting
            if (c == '<' || c == '>') && !text.is_empty() {
                let mut peek = chars.clone();
                peek.next();
                if let Some(&(_, next)) = peek.peek()
                    && ((c == '<' && next == '<') || (c == '>' && next == '>'))
                {
                    break;
                }
            }
            text.push(c);
            end = i + c.len_utf8();
            chars.next();
        }

        if !text.is_empty() {
            tokens.push(Token {
                text,
                span: Span::new(Pos::new(start as u32), Pos::new(end as u32)),
            });
        }
    }

    (tokens, source_map)
}

/// A scope for local variable tracking.
#[derive(Clone, Debug, Default)]
struct Scope {
    /// Map from symbol to local index.
    locals: HashMap<Symbol, usize>,
    /// Program depth when this scope was created.
    /// Used to prevent capturing locals from outer programs.
    program_depth: usize,
}

/// Parse context.
pub struct ParseContext<'a> {
    tokens: Vec<Token>,
    position: usize,
    pub registry: &'a InterfaceRegistry,
    pub interner: &'a mut Interner,
    /// Stack of scopes for local variable tracking.
    scopes: Vec<Scope>,
    /// Next local index to allocate.
    next_local: usize,
    /// Program nesting depth (for closure semantics).
    program_depth: usize,
    /// Source map with comments and other metadata.
    source_map: SourceMap,
}

impl<'a> ParseContext<'a> {
    /// Create a new parse context.
    pub fn new(source: &str, registry: &'a InterfaceRegistry, interner: &'a mut Interner) -> Self {
        let (tokens, source_map) = tokenize_with_source_map(source);
        Self {
            tokens,
            position: 0,
            registry,
            interner,
            scopes: Vec::new(),
            next_local: 0,
            program_depth: 0,
            source_map,
        }
    }

    /// Take the source map, consuming it from the context.
    pub fn take_source_map(&mut self) -> SourceMap {
        std::mem::take(&mut self.source_map)
    }

    /// Get a reference to the source map.
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Peek at the current token.
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    /// Peek at a token at an offset from current position.
    pub fn peek_at(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.position + offset)
    }

    /// Advance to the next token.
    pub fn advance(&mut self) -> Option<Token> {
        if self.position < self.tokens.len() {
            let token = self.tokens[self.position].clone();
            self.position += 1;
            Some(token)
        } else {
            None
        }
    }

    /// Check if at end of input.
    pub fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    /// Get the span of the current or last token.
    pub fn current_span(&self) -> Span {
        if self.position < self.tokens.len() {
            self.tokens[self.position].span
        } else if self.position > 0 {
            self.tokens[self.position - 1].span
        } else {
            Span::new(Pos::new(0), Pos::new(0))
        }
    }

    /// Parse a single object (public interface for library parsers).
    pub fn parse_one(&mut self) -> Result<Node, ParseError> {
        parse_one(self)
    }

    // === Scope management ===

    /// Enter a new scope.
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            locals: HashMap::new(),
            program_depth: self.program_depth,
        });
    }

    /// Exit the current scope.
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Declare a local variable in the current scope.
    /// Returns the allocated local index.
    pub fn declare_local(&mut self, name: Symbol) -> usize {
        let index = self.next_local;
        self.next_local += 1;

        if let Some(scope) = self.scopes.last_mut() {
            scope.locals.insert(name, index);
        }

        index
    }

    /// Resolve a symbol to a local index, if it exists in any scope.
    ///
    /// Searches from innermost to outermost scope, but only resolves locals
    /// from scopes at the current program depth or deeper. This prevents
    /// closures (capturing locals from outer programs) while allowing locals
    /// declared inside the current program to be resolved.
    pub fn resolve_local(&self, name: Symbol) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .take_while(|scope| scope.program_depth >= self.program_depth)
            .find_map(|scope| scope.locals.get(&name).copied())
    }

    /// Enter a program (increases nesting depth).
    pub fn enter_program(&mut self) {
        self.program_depth += 1;
    }

    /// Exit a program (decreases nesting depth).
    pub fn exit_program(&mut self) {
        if self.program_depth > 0 {
            self.program_depth -= 1;
        }
    }

    /// Check if we're inside a nested program.
    pub fn in_nested_program(&self) -> bool {
        self.program_depth > 0
    }

    /// Get the current number of allocated locals.
    pub fn local_count(&self) -> usize {
        self.next_local
    }
}

/// Parse source code into IR nodes.
///
/// # Errors
///
/// Returns `ParseError` if the source contains invalid syntax.
pub fn parse(
    source: &str,
    registry: &InterfaceRegistry,
    interner: &mut Interner,
) -> Result<Vec<Node>, ParseError> {
    parse_with_source_map(source, registry, interner).map(|(nodes, _)| nodes)
}

/// Parse source code into IR nodes and return the source map.
///
/// # Errors
///
/// Returns `ParseError` if the source contains invalid syntax.
pub fn parse_with_source_map(
    source: &str,
    registry: &InterfaceRegistry,
    interner: &mut Interner,
) -> Result<(Vec<Node>, SourceMap), ParseError> {
    let mut ctx = ParseContext::new(source, registry, interner);
    let mut nodes = Vec::new();

    while !ctx.at_end() {
        nodes.push(parse_one(&mut ctx)?);
    }

    Ok((nodes, ctx.take_source_map()))
}

/// Check if a token is a program opener.
fn is_program_open(text: &str) -> bool {
    matches!(text, "«" | "<<")
}

/// Check if a token is a program closer.
fn is_program_close(text: &str) -> bool {
    matches!(text, "»" | ">>")
}

/// Check if a token is a list opener.
fn is_list_open(text: &str) -> bool {
    text == "{"
}

/// Check if a token is a list closer.
fn is_list_close(text: &str) -> bool {
    text == "}"
}

/// Check if a token is a symbolic expression opener.
fn is_symbolic_open(text: &str) -> bool {
    text == "'"
}

/// Parse a single object.
fn parse_one(ctx: &mut ParseContext) -> Result<Node, ParseError> {
    let token = ctx.advance().ok_or_else(|| ParseError {
        message: "unexpected end of input".into(),
        span: Span::new(Pos::new(0), Pos::new(0)),
        expected: None,
        found: None,
    })?;

    // Program: « ... » or << ... >>
    if is_program_open(&token.text) {
        return parse_program(ctx, token.span);
    }

    // List: { ... }
    if is_list_open(&token.text) {
        return parse_list(ctx, token.span);
    }

    // Symbolic expression: ' ... '
    if is_symbolic_open(&token.text) {
        return parse_symbolic(ctx, token.span);
    }

    // Try to parse as number
    if let Some(node) = try_parse_number(&token) {
        return Ok(node);
    }

    // Try to parse as string
    if token.text.starts_with('"') && token.text.ends_with('"') {
        let s = &token.text[1..token.text.len() - 1];
        return Ok(Node::string(s, token.span));
    }

    // Check for token claims (library-defined syntax like IF, FOR, etc.)
    if let Some(claim) = ctx.registry.find_claim(&token.text, ClaimContext::Any)
        && let Some(library) = ctx.registry.get(claim.lib_id)
    {
        return library.parse(&token.text, ctx);
    }

    // Try to find as command
    if let Some((lib_id, cmd_id)) = ctx.registry.find_command(&token.text) {
        return Ok(Node::command(lib_id, cmd_id, token.span));
    }

    // Intern the symbol for potential local lookup
    let sym = ctx.interner.intern(&token.text);

    // Check if it's a local variable
    if let Some(index) = ctx.resolve_local(sym) {
        return Ok(Node::local_ref(index, token.span));
    }

    // Unknown - treat as unresolved symbol (runtime lookup)
    Ok(Node::symbol(sym, token.span))
}

/// Parse a program construct: « body » or << body >>
fn parse_program(ctx: &mut ParseContext, open_span: Span) -> Result<Node, ParseError> {
    // Track program nesting (for closure semantics - no local capture in nested programs)
    ctx.enter_program();

    let mut body = Vec::new();

    loop {
        // Check for end of input
        let token = ctx.peek().ok_or_else(|| ParseError {
            message: "unterminated program, expected » or >>".into(),
            span: open_span,
            expected: Some("» or >>".into()),
            found: Some("end of input".into()),
        })?;

        // Check for program closer
        if is_program_close(&token.text) {
            let close_span = token.span;
            ctx.advance(); // consume closer

            ctx.exit_program();

            // Create span from open to close
            let full_span = Span::new(open_span.start(), close_span.end());
            return Ok(Node::program(body, full_span));
        }

        // Parse next element
        body.push(parse_one(ctx)?);
    }
}

/// Parse a list construct: { items }
fn parse_list(ctx: &mut ParseContext, open_span: Span) -> Result<Node, ParseError> {
    let mut items = Vec::new();

    loop {
        // Check for end of input
        let token = ctx.peek().ok_or_else(|| ParseError {
            message: "unterminated list, expected }".into(),
            span: open_span,
            expected: Some("}".into()),
            found: Some("end of input".into()),
        })?;

        // Check for list closer
        if is_list_close(&token.text) {
            let close_span = token.span;
            ctx.advance(); // consume closer

            // Create span from open to close
            let full_span = Span::new(open_span.start(), close_span.end());
            return Ok(Node::list(items, full_span));
        }

        // Parse next element
        items.push(parse_one(ctx)?);
    }
}

/// Parse a symbolic expression: ' expression '
fn parse_symbolic(ctx: &mut ParseContext, open_span: Span) -> Result<Node, ParseError> {
    // Collect tokens until closing '
    let mut expr_text = String::new();

    loop {
        let token = ctx.peek().ok_or_else(|| ParseError {
            message: "unterminated symbolic expression, expected '".into(),
            span: open_span,
            expected: Some("'".into()),
            found: Some("end of input".into()),
        })?;

        // Check for closing '
        if token.text == "'" {
            let close_span = token.span;
            ctx.advance(); // consume closing '

            // Parse the collected expression with the infix parser
            let sym_expr = infix::InfixParser::parse_str(&expr_text).map_err(|msg| ParseError {
                message: format!("invalid symbolic expression: {}", msg),
                span: Span::new(open_span.start(), close_span.end()),
                expected: None,
                found: None,
            })?;

            // Create span from open to close
            let full_span = Span::new(open_span.start(), close_span.end());
            return Ok(Node::symbolic(sym_expr, full_span));
        }

        // Add token to expression (with spacing)
        if !expr_text.is_empty() {
            expr_text.push(' ');
        }
        expr_text.push_str(&token.text);
        ctx.advance();
    }
}

/// Try to parse a token as a number.
fn try_parse_number(token: &Token) -> Option<Node> {
    let text = &token.text;
    let bytes = text.as_bytes();

    // Fast path: check first character to determine number type
    let first = *bytes.first()?;

    // HP-style binary: #FF, #FFh, #1010b, etc.
    if first == b'#' {
        return try_parse_hp_binary(text).map(|n| Node::integer(n, token.span));
    }

    // Check for 0x/0b prefix
    if first == b'0' && bytes.len() > 2 {
        let second = bytes[1];
        if second == b'x' || second == b'X' {
            return i64::from_str_radix(&text[2..], 16)
                .ok()
                .map(|n| Node::integer(n, token.span));
        }
        if second == b'b' || second == b'B' {
            return i64::from_str_radix(&text[2..], 2)
                .ok()
                .map(|n| Node::integer(n, token.span));
        }
    }

    // Only try numeric parsing if first char is digit or sign
    if !first.is_ascii_digit() && first != b'-' && first != b'+' {
        return None;
    }

    // Try integer first (most common case)
    if let Ok(n) = text.parse::<i64>() {
        return Some(Node::integer(n, token.span));
    }

    // Try real (contains '.' or 'e'/'E')
    if (text.contains('.') || text.contains('e') || text.contains('E'))
        && let Ok(n) = text.parse::<f64>()
    {
        return Some(Node::real(n, token.span));
    }

    None
}

/// Try to parse HP-style binary integer literals.
/// Formats:
/// - #FF or #FFh - hexadecimal (default)
/// - #1010b - binary
/// - #377o - octal
/// - #255d - decimal
fn try_parse_hp_binary(text: &str) -> Option<i64> {
    if !text.starts_with('#') || text.len() < 2 {
        return None;
    }

    let digits = &text[1..]; // Skip the #

    // Check for suffix to determine base
    // Only use suffix if the remaining digits are valid for that base
    let last_char = digits.chars().last()?;
    let suffix_base = match last_char.to_ascii_lowercase() {
        'b' => Some(2),  // binary
        'o' => Some(8),  // octal
        'd' => Some(10), // decimal
        'h' => Some(16), // explicit hex
        _ => None,
    };

    if let Some(radix) = suffix_base {
        let num_str = &digits[..digits.len() - 1];
        if !num_str.is_empty() {
            // Try to parse with the suffix-indicated base
            if let Ok(n) = i64::from_str_radix(num_str, radix) {
                return Some(n);
            }
        }
        // If suffix didn't work (e.g., #AB where "A" isn't valid binary),
        // fall through to try as hex
    }

    // Default to hex interpretation of all digits
    i64::from_str_radix(digits, 16).ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{AtomKind, CompositeKind, NodeKind};

    #[test]
    fn tokenize_simple() {
        let tokens = tokenize("1 2 +");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].text, "1");
        assert_eq!(tokens[1].text, "2");
        assert_eq!(tokens[2].text, "+");
    }

    #[test]
    fn tokenize_string() {
        let tokens = tokenize("\"hello world\"");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].text, "\"hello world\"");
    }

    #[test]
    fn tokenize_program_chevrons() {
        let tokens = tokenize("<< 1 2 + >>");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].text, "<<");
        assert_eq!(tokens[1].text, "1");
        assert_eq!(tokens[2].text, "2");
        assert_eq!(tokens[3].text, "+");
        assert_eq!(tokens[4].text, ">>");
    }

    #[test]
    fn tokenize_program_guillemets() {
        let tokens = tokenize("« 1 2 + »");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].text, "«");
        assert_eq!(tokens[4].text, "»");
    }

    #[test]
    fn tokenize_list() {
        let tokens = tokenize("{ 1 2 3 }");
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].text, "{");
        assert_eq!(tokens[4].text, "}");
    }

    #[test]
    fn tokenize_nested() {
        // { { 1 } { 2 } } = 8 tokens
        let tokens = tokenize("{ { 1 } { 2 } }");
        assert_eq!(tokens.len(), 8);
    }

    #[test]
    fn tokenize_comment() {
        let tokens = tokenize("1 // this is a comment\n2");
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].text, "1");
        assert_eq!(tokens[1].text, "2");
    }

    #[test]
    fn parse_integers() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("1 2 42", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 3);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(1)) => {}
            _ => panic!("expected integer 1"),
        }
        match &nodes[1].kind {
            NodeKind::Atom(AtomKind::Integer(2)) => {}
            _ => panic!("expected integer 2"),
        }
        match &nodes[2].kind {
            NodeKind::Atom(AtomKind::Integer(42)) => {}
            _ => panic!("expected integer 42"),
        }
    }

    // Note: parse_command test moved to rpl-stdlib (requires stdlib registration)

    #[test]
    fn parse_hex() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("0xFF 0x10", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 2);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(255)) => {}
            _ => panic!("expected 255"),
        }
        match &nodes[1].kind {
            NodeKind::Atom(AtomKind::Integer(16)) => {}
            _ => panic!("expected 16"),
        }
    }

    #[test]
    fn parse_hp_binary_hex() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("#FF #10 #FFh", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 3);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(255)) => {}
            _ => panic!("expected 255"),
        }
        match &nodes[1].kind {
            NodeKind::Atom(AtomKind::Integer(16)) => {}
            _ => panic!("expected 16"),
        }
        match &nodes[2].kind {
            NodeKind::Atom(AtomKind::Integer(255)) => {}
            _ => panic!("expected 255 (explicit h)"),
        }
    }

    #[test]
    fn parse_hp_binary_formats() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        // Binary, octal, decimal
        let nodes = parse("#1010b #377o #255d", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 3);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(10)) => {} // 1010 binary = 10
            _ => panic!("expected 10 from binary"),
        }
        match &nodes[1].kind {
            NodeKind::Atom(AtomKind::Integer(255)) => {} // 377 octal = 255
            _ => panic!("expected 255 from octal"),
        }
        match &nodes[2].kind {
            NodeKind::Atom(AtomKind::Integer(255)) => {} // 255 decimal
            _ => panic!("expected 255 from decimal"),
        }
    }

    #[test]
    fn parse_hp_single_hex_digit() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        // Single hex digit A = 10
        let nodes = parse("#A", &reg, &mut interner).unwrap();
        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(10)) => {}
            other => panic!("expected 10 from #A, got {:?}", other),
        }
    }

    #[test]
    fn parse_hp_double_hex_ab() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("#AB", &reg, &mut interner).unwrap();
        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Atom(AtomKind::Integer(171)) => {} // AB hex = 171
            other => panic!("expected 171 from #AB, got {:?}", other),
        }
    }

    #[test]
    fn parse_program_chevrons() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("<< 1 2 + >>", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::Program, branches) => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].len(), 3); // 1, 2, +
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn parse_program_guillemets() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("« 1 + »", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::Program, branches) => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].len(), 2); // 1, +
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn parse_list() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("{ 1 2 3 }", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::List, branches) => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].len(), 3); // 1, 2, 3
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn parse_nested_lists() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("{ { 1 } { 2 3 } }", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::List, branches) => {
                assert_eq!(branches[0].len(), 2); // Two nested lists
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn parse_program_with_list() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("<< { 1 2 } >>", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::Program, branches) => {
                assert_eq!(branches[0].len(), 1); // One list
                match &branches[0][0].kind {
                    NodeKind::Composite(CompositeKind::List, _) => {}
                    _ => panic!("expected list inside program"),
                }
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn parse_span_tracking() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("{ 1 2 }", &reg, &mut interner).unwrap();

        // List should span from { to }
        assert_eq!(nodes[0].span.start().offset(), 0);
        assert_eq!(nodes[0].span.end().offset(), 7);
    }

    #[test]
    fn parse_unterminated_program() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let result = parse("<< 1 2", &reg, &mut interner);

        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("unterminated program"));
    }

    #[test]
    fn parse_unterminated_list() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let result = parse("{ 1 2", &reg, &mut interner);

        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("unterminated list"));
    }

    #[test]
    fn parse_empty_program() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("<< >>", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::Program, branches) => {
                assert_eq!(branches[0].len(), 0);
            }
            _ => panic!("expected empty program"),
        }
    }

    #[test]
    fn parse_list_with_program() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("{ << 42 >> }", &reg, &mut interner).unwrap();

        // Should be one list node
        assert_eq!(nodes.len(), 1, "should be one node, got: {:?}", nodes);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::List, branches) => {
                // List should contain one element (the program)
                assert_eq!(branches[0].len(), 1, "list should have 1 element");
                match &branches[0][0].kind {
                    NodeKind::Composite(CompositeKind::Program, _) => {}
                    other => panic!("expected program inside list, got: {:?}", other),
                }
            }
            other => panic!("expected list, got: {:?}", other),
        }
    }

    #[test]
    fn parse_empty_list() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse("{ }", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 1);
        match &nodes[0].kind {
            NodeKind::Composite(CompositeKind::List, branches) => {
                assert_eq!(branches[0].len(), 0);
            }
            _ => panic!("expected empty list"),
        }
    }

    // === Source Map Tests ===

    #[test]
    fn source_map_double_slash_comment() {
        let (tokens, source_map) = tokenize_with_source_map("1 // this is a comment\n2");
        assert_eq!(tokens.len(), 2);
        assert_eq!(source_map.comments.len(), 1);

        let comment = &source_map.comments[0];
        assert_eq!(comment.kind, CommentKind::DoubleSlash);
        assert!(comment.text.starts_with("//"));
        assert!(comment.text.contains("this is a comment"));
    }

    #[test]
    fn source_map_single_at_comment() {
        let (tokens, source_map) = tokenize_with_source_map("1 @ strippable comment\n2");
        assert_eq!(tokens.len(), 2);
        assert_eq!(source_map.comments.len(), 1);

        let comment = &source_map.comments[0];
        assert_eq!(comment.kind, CommentKind::SingleLine);
        assert!(comment.text.starts_with("@"));
        assert!(comment.text.contains("strippable comment"));

        // Check strippable_comments iterator
        assert_eq!(source_map.strippable_comments().count(), 1);
    }

    #[test]
    fn source_map_permanent_comment() {
        let (tokens, source_map) = tokenize_with_source_map("1 @@ permanent comment\n2");
        assert_eq!(tokens.len(), 2);
        assert_eq!(source_map.comments.len(), 1);

        let comment = &source_map.comments[0];
        assert_eq!(comment.kind, CommentKind::Permanent);
        assert!(comment.text.starts_with("@@"));
        assert!(comment.text.contains("permanent comment"));

        // Check permanent_comments iterator
        assert_eq!(source_map.permanent_comments().count(), 1);
        assert_eq!(source_map.strippable_comments().count(), 0);
    }

    #[test]
    fn source_map_multiline_comment() {
        let (tokens, source_map) = tokenize_with_source_map("1 @@@ multi\nline\ncomment @@@ 2");
        assert_eq!(tokens.len(), 2);
        assert_eq!(source_map.comments.len(), 1);

        let comment = &source_map.comments[0];
        assert_eq!(comment.kind, CommentKind::MultiLine);
        assert!(comment.text.starts_with("@@@"));
        assert!(comment.text.ends_with("@@@"));
        assert!(comment.text.contains("multi"));
        assert!(comment.text.contains("line"));
    }

    #[test]
    fn source_map_multiple_comments() {
        let source = "1 @ first\n2 @@ second\n3 // third\n4";
        let (tokens, source_map) = tokenize_with_source_map(source);
        assert_eq!(tokens.len(), 4);
        assert_eq!(source_map.comments.len(), 3);

        assert_eq!(source_map.comments[0].kind, CommentKind::SingleLine);
        assert_eq!(source_map.comments[1].kind, CommentKind::Permanent);
        assert_eq!(source_map.comments[2].kind, CommentKind::DoubleSlash);

        assert_eq!(source_map.strippable_comments().count(), 1);
        assert_eq!(source_map.permanent_comments().count(), 1);
    }

    #[test]
    fn source_map_comment_spans() {
        let source = "1 @ comment\n2";
        let (_, source_map) = tokenize_with_source_map(source);

        let comment = &source_map.comments[0];
        // "1 " = 2 chars, comment starts at offset 2
        assert_eq!(comment.span.start().offset(), 2);
        // Comment ends at newline (offset 11) or after
        assert!(comment.span.end().offset() >= 11);
    }

    #[test]
    fn source_map_eof_comment() {
        // Comment at EOF without trailing newline
        let (tokens, source_map) = tokenize_with_source_map("1 @ comment");
        assert_eq!(tokens.len(), 1);
        assert_eq!(source_map.comments.len(), 1);

        let comment = &source_map.comments[0];
        assert_eq!(comment.kind, CommentKind::SingleLine);
    }

    #[test]
    fn parse_with_source_map_returns_comments() {
        let reg = InterfaceRegistry::new();
        let mut interner = Interner::new();
        let (nodes, source_map) =
            parse_with_source_map("1 @ comment\n2 +", &reg, &mut interner).unwrap();

        assert_eq!(nodes.len(), 3); // 1, 2, +
        assert_eq!(source_map.comments.len(), 1);
        assert_eq!(source_map.comments[0].kind, CommentKind::SingleLine);
    }
}
