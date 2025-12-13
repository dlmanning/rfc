use rpl_lang::analysis::{AnalysisResult, SymbolTable};
use rpl_core::{Interner, Pos, Span, TypeId};
use rpl_lang::library::LibraryRegistry;
use rpl_source::SourceFile;
use rpl_core::token::SemanticKind;

use super::completion::token_at_pos;

/// Result of a hover request.
#[derive(Clone, Debug)]
pub struct HoverResult {
    /// The range of the hovered token.
    pub range: Span,
    /// The hover content (markdown).
    pub contents: String,
}

impl HoverResult {
    /// Create a new hover result.
    pub fn new(range: Span, contents: impl Into<String>) -> Self {
        Self {
            range,
            contents: contents.into(),
        }
    }
}

/// Get hover information at a position.
pub fn hover(
    analysis: &AnalysisResult,
    source: &SourceFile,
    registry: &LibraryRegistry,
    symbols: &SymbolTable,
    _interner: &Interner,
    pos: Pos,
) -> Option<HoverResult> {
    hover_impl(analysis, source, registry, symbols, pos, false)
}

/// Get verbose hover information (shows internal debug info).
pub fn hover_verbose(
    analysis: &AnalysisResult,
    source: &SourceFile,
    registry: &LibraryRegistry,
    symbols: &SymbolTable,
    pos: Pos,
) -> Option<HoverResult> {
    hover_impl(analysis, source, registry, symbols, pos, true)
}

fn hover_impl(
    analysis: &AnalysisResult,
    source: &SourceFile,
    registry: &LibraryRegistry,
    symbols: &SymbolTable,
    pos: Pos,
    verbose: bool,
) -> Option<HoverResult> {
    let token = token_at_pos(analysis, pos)?;

    // If there's an error, show the error message
    if let Some(ref err) = token.error {
        return Some(HoverResult::new(token.span, format!("**Error**: {}", err)));
    }

    // Get the token text
    let text = source.span_text(token.span);

    // Build hover content based on token type
    let contents = match token.semantic {
        SemanticKind::Number => {
            let type_info = token
                .literal_type
                .map(|t| format!("Type: `{:?}`", t))
                .unwrap_or_else(|| "Type: Real".to_string());
            format!("**Number**: `{}`\n\n{}", text, type_info)
        }

        SemanticKind::String => {
            format!("**String**: `{}`\n\nType: String", text)
        }

        SemanticKind::Variable => {
            // Check if it's a quoted name (starts with ')
            if text.starts_with('\'') && text.ends_with('\'') {
                let name = &text[1..text.len() - 1];

                // Try to get the type of the stored value from the definition
                let type_info = token
                    .defines
                    .and_then(|def_id| {
                        let id = rpl_lang::analysis::DefinitionId::new(def_id.0);
                        symbols.get_definition(id)
                    })
                    .and_then(|def| def.value_type)
                    .map(|ty| format!("\n\nStores: `{}`", type_name(ty)))
                    .unwrap_or_default();

                format!(
                    "**Name**: `{}`\n\nQuoted symbol for variable operations (STO/RCL){}",
                    name, type_info
                )
            } else {
                format!("**Variable**: `{}`", text)
            }
        }

        SemanticKind::Local => {
            format!("**Local Variable**: `{}`\n\nBound with `→`", text)
        }

        SemanticKind::Parameter => {
            format!("**Parameter**: `{}`", text)
        }

        SemanticKind::Definition => {
            format!("**Definition**: `{}`", text)
        }

        SemanticKind::Command => {
            // Try to get documentation from the library
            if let Some(lib) = registry.get(token.lib)
                && let Some(help) = lib.help(text)
            {
                return Some(HoverResult::new(
                    token.span,
                    format!("**Command**: `{}`\n\n{}", text, help),
                ));
            }
            format!("**Command**: `{}`", text)
        }

        SemanticKind::Operator => {
            let stack_effect = get_operator_stack_effect(text);
            format!(
                "**Operator**: `{}`\n\nStack: `{}`",
                text,
                stack_effect.unwrap_or("dynamic")
            )
        }

        SemanticKind::Keyword => {
            let description = get_keyword_description(text);
            format!("**Keyword**: `{}`\n\n{}", text, description)
        }

        SemanticKind::Comment => "**Comment**".to_string(),

        SemanticKind::Bracket => {
            let description = get_delimiter_description(text);
            format!("**Bracket**: `{}`\n\n{}", text, description)
        }

        SemanticKind::Unit => {
            format!("**Unit**: `{}`", text)
        }

        SemanticKind::Constant => {
            format!("**Constant**: `{}`", text)
        }

        SemanticKind::Invalid => {
            format!("**Invalid**: `{}`", text)
        }
    };

    // Add verbose debug info if requested
    let contents = if verbose {
        let lib_name = registry
            .get(token.lib)
            .map(|l| l.name())
            .unwrap_or("unknown");

        let mut debug = format!("{}\n\n---\n**Debug Info**\n", contents);
        debug.push_str(&format!("- Semantic: `{:?}`\n", token.semantic));
        debug.push_str(&format!("- Library: `{}` (id={})\n", lib_name, token.lib.as_u16()));
        debug.push_str(&format!("- Span: `{}..{}`\n", token.span.start().offset(), token.span.end().offset()));

        if let Some(lit_type) = token.literal_type {
            debug.push_str(&format!("- Literal Type: `{:?}`\n", lit_type));
        }
        if token.defines.is_some() {
            debug.push_str(&format!("- Defines: `{:?}`\n", token.defines));
        }
        if token.references.is_some() {
            debug.push_str(&format!("- References: `{:?}`\n", token.references));
        }

        let ctx = &token.context;
        if ctx.starts_construct || ctx.ends_construct || ctx.construct_depth > 0 {
            debug.push_str(&format!(
                "- Context: depth={}, starts={}, ends={}\n",
                ctx.construct_depth, ctx.starts_construct, ctx.ends_construct
            ));
        }

        debug
    } else {
        contents
    };

    Some(HoverResult::new(token.span, contents))
}

/// Get a human-readable name for a TypeId.
fn type_name(ty: TypeId) -> &'static str {
    match ty {
        TypeId::REAL => "Real",
        TypeId::BINT => "Integer",
        TypeId::STRING => "String",
        TypeId::LIST => "List",
        TypeId::PROGRAM => "Program",
        TypeId::SYMBOLIC => "Symbolic",
        TypeId::COMMENT => "Comment",
        _ => "Unknown",
    }
}

/// Get stack effect description for an operator.
fn get_operator_stack_effect(op: &str) -> Option<&'static str> {
    match op {
        "+" => Some("( a b -- a+b )"),
        "-" => Some("( a b -- a-b )"),
        "*" => Some("( a b -- a*b )"),
        "/" => Some("( a b -- a/b )"),
        "^" => Some("( a b -- a^b )"),
        "==" | "=" => Some("( a b -- flag )"),
        "!=" | "<>" => Some("( a b -- flag )"),
        "<" => Some("( a b -- flag )"),
        ">" => Some("( a b -- flag )"),
        "<=" | "≤" => Some("( a b -- flag )"),
        ">=" | "≥" => Some("( a b -- flag )"),
        _ => {
            let upper = op.to_ascii_uppercase();
            match upper.as_str() {
                "NEG" => Some("( a -- -a )"),
                "ABS" => Some("( a -- |a| )"),
                "SQ" => Some("( a -- a² )"),
                "SQRT" | "√" => Some("( a -- √a )"),
                "INV" => Some("( a -- 1/a )"),
                "AND" => Some("( a b -- a∧b )"),
                "OR" => Some("( a b -- a∨b )"),
                "NOT" => Some("( a -- ¬a )"),
                "XOR" => Some("( a b -- a⊕b )"),
                _ => None,
            }
        }
    }
}

/// Get description for a keyword.
fn get_keyword_description(keyword: &str) -> &'static str {
    let upper = keyword.to_ascii_uppercase();
    match upper.as_str() {
        "IF" => "Begins a conditional. Usage: `test IF true-branch THEN`",
        "THEN" => "Ends an IF conditional block",
        "ELSE" => "Separates true and false branches in an IF statement",
        "END" => "Ends a program or structure",
        "FOR" => "Begins a counted loop. Usage: `start end FOR var body NEXT var`",
        "NEXT" => "Ends a FOR loop, incrementing the counter",
        "STEP" => "Ends a FOR loop with a custom step value",
        "WHILE" => "Begins a conditional loop",
        "REPEAT" => "Marks the body of a WHILE loop",
        "DO" => "Begins a DO-UNTIL loop",
        "UNTIL" => "Tests the exit condition of a DO loop",
        "CASE" => "Begins a case statement",
        "START" => "Begins an indefinite loop",
        "→" | "->" => "Begins local variable binding",
        "«" | "<<" => "Begins a program",
        "»" | ">>" => "Ends a program",
        _ => "Control flow keyword",
    }
}

/// Get description for a delimiter.
fn get_delimiter_description(delim: &str) -> &'static str {
    match delim {
        "(" => "Opens infix expression or comment",
        ")" => "Closes infix expression or comment",
        "[" => "Opens a vector",
        "]" => "Closes a vector",
        "{" => "Opens a list",
        "}" => "Closes a list",
        "«" | "<<" => "Opens a program",
        "»" | ">>" => "Closes a program",
        "'" => "Quote prefix for symbolic evaluation",
        "\"" => "String delimiter",
        _ => "Delimiter",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_lang::analysis::{ResolvedToken, TokenContext};
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::LibraryRegistry;
    use rpl_stdlib::{ArithmeticLib, RealNumbersLib, register_standard_libs};
    use rpl_source::SourceId;
    use rpl_core::token::TokenInfo;

    fn make_source(text: &str) -> SourceFile {
        SourceFile::new(SourceId::new(0), "test".into(), text.into())
    }

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
    fn hover_result_new() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let result = HoverResult::new(span, "test content");
        assert_eq!(result.contents, "test content");
        assert_eq!(result.range.start(), Pos::new(0));
    }

    #[test]
    fn hover_number() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("42");
        let mut analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

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

        let result = hover(&analysis, &source, &registry, &symbols, &interner, Pos::new(1));
        assert!(result.is_some());
        let hover = result.unwrap();
        assert!(hover.contents.contains("Number"));
        assert!(hover.contents.contains("42"));
    }

    #[test]
    fn hover_operator() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("+");
        let mut analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(1)),
            lib: ArithmeticLib::ID,
            info: TokenInfo::atom(1),
            semantic: SemanticKind::Operator,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        });

        let result = hover(&analysis, &source, &registry, &symbols, &interner, Pos::new(0));
        assert!(result.is_some());
        let hover = result.unwrap();
        assert!(hover.contents.contains("Operator"));
        assert!(hover.contents.contains("a b -- a+b"));
    }

    #[test]
    fn hover_error_token() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("@@@");
        let mut analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

        analysis.tokens.push(ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(3)),
            lib: RealNumbersLib::ID,
            info: TokenInfo::atom(3),
            semantic: SemanticKind::Invalid,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: Some("Unknown token".to_string()),
        });

        let result = hover(&analysis, &source, &registry, &symbols, &interner, Pos::new(1));
        assert!(result.is_some());
        let hover = result.unwrap();
        assert!(hover.contents.contains("Error"));
        assert!(hover.contents.contains("Unknown token"));
    }

    #[test]
    fn hover_outside_token() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("42 ");
        let mut analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

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

        // Position 3 is after the token
        let result = hover(&analysis, &source, &registry, &symbols, &interner, Pos::new(3));
        assert!(result.is_none());
    }

    #[test]
    fn get_operator_stack_effect_known() {
        assert_eq!(get_operator_stack_effect("+"), Some("( a b -- a+b )"));
        assert_eq!(get_operator_stack_effect("NEG"), Some("( a -- -a )"));
        assert_eq!(get_operator_stack_effect("neg"), Some("( a -- -a )"));
    }

    #[test]
    fn get_operator_stack_effect_unknown() {
        assert_eq!(get_operator_stack_effect("???"), None);
    }

    #[test]
    fn get_keyword_description_known() {
        let desc = get_keyword_description("IF");
        assert!(desc.contains("conditional"));
    }
}
