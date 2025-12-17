//! LSP support for RPL2.
//!
//! This module provides types and functions for Language Server Protocol
//! features like completions, hover, go-to-definition, etc.

use crate::core::{Interner, Pos, Span};
use crate::source::SourceFile;

use crate::{
    analysis::{AnalysisResult, Definition, DefinitionKind, IncrementalAnalysis},
    registry::Registry,
};

// ============================================================================
// Types
// ============================================================================

/// Kind of completion item.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompletionItemKind {
    /// A variable.
    Variable,
    /// A command/function.
    Command,
    /// A keyword.
    Keyword,
    /// A constant.
    Constant,
    /// An operator.
    Operator,
}

/// A completion item.
#[derive(Clone, Debug)]
pub struct CompletionItem {
    /// The label shown in the completion list.
    pub label: String,
    /// The kind of completion.
    pub kind: CompletionItemKind,
    /// Optional detail text.
    pub detail: Option<String>,
    /// Optional documentation.
    pub documentation: Option<String>,
    /// Text to insert (if different from label).
    pub insert_text: Option<String>,
}

impl CompletionItem {
    /// Create a new completion item.
    pub fn new(label: impl Into<String>, kind: CompletionItemKind) -> Self {
        Self {
            label: label.into(),
            kind,
            detail: None,
            documentation: None,
            insert_text: None,
        }
    }

    /// Add detail text.
    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    /// Add documentation.
    pub fn with_documentation(mut self, doc: impl Into<String>) -> Self {
        self.documentation = Some(doc.into());
        self
    }
}

/// Result of a hover request.
#[derive(Clone, Debug)]
pub struct HoverResult {
    /// The hover contents (markdown).
    pub contents: String,
    /// The range of the hovered item.
    pub range: Span,
}

/// Result of a go-to-definition request.
#[derive(Clone, Debug)]
pub struct GotoResult {
    /// The span of the definition.
    pub span: Span,
}

/// Result of a find-references request.
#[derive(Clone, Debug)]
pub struct ReferenceResult {
    /// The locations of all references.
    pub locations: Vec<Span>,
}

/// A semantic token for syntax highlighting.
#[derive(Clone, Debug)]
pub struct SemanticToken {
    /// Start position.
    pub start: Pos,
    /// Length in bytes.
    pub length: u32,
    /// Token type index.
    pub token_type: u32,
    /// Modifier bitset.
    pub modifiers: u32,
}

/// Semantic token types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SemanticTokenType {
    /// A keyword.
    Keyword,
    /// A number literal.
    Number,
    /// A string literal.
    String,
    /// A comment.
    Comment,
    /// An operator.
    Operator,
    /// A function/command.
    Function,
    /// A variable.
    Variable,
    /// A parameter.
    Parameter,
}

impl SemanticTokenType {
    /// Get the legend (list of token type names).
    pub fn legend() -> &'static [&'static str] {
        &[
            "keyword",
            "number",
            "string",
            "comment",
            "operator",
            "function",
            "variable",
            "parameter",
        ]
    }

    /// Get the index for this token type.
    pub fn index(self) -> u32 {
        match self {
            SemanticTokenType::Keyword => 0,
            SemanticTokenType::Number => 1,
            SemanticTokenType::String => 2,
            SemanticTokenType::Comment => 3,
            SemanticTokenType::Operator => 4,
            SemanticTokenType::Function => 5,
            SemanticTokenType::Variable => 6,
            SemanticTokenType::Parameter => 7,
        }
    }
}

/// Semantic token modifiers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SemanticModifier {
    /// Definition of a symbol.
    Definition,
    /// Read-only/constant.
    Readonly,
    /// Deprecated.
    Deprecated,
}

impl SemanticModifier {
    /// Get the legend (list of modifier names).
    pub fn legend() -> &'static [&'static str] {
        &["definition", "readonly", "deprecated"]
    }
}

/// Kind of document symbol.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DocumentSymbolKind {
    /// A function/program.
    Function,
    /// A global variable.
    Variable,
    /// A local variable.
    Local,
    /// A loop variable.
    LoopVariable,
}

/// A document symbol for the outline view.
#[derive(Clone, Debug)]
pub struct DocumentSymbol {
    /// Symbol name.
    pub name: String,
    /// Optional detail.
    pub detail: Option<String>,
    /// Symbol kind.
    pub kind: DocumentSymbolKind,
    /// Full range of the symbol.
    pub range: Span,
    /// Range of the symbol's name.
    pub selection_range: Span,
}

// ============================================================================
// Functions
// ============================================================================

/// Get completions at a position.
pub fn complete(
    analysis: &AnalysisResult,
    source: &SourceFile,
    registry: &Registry,
    interner: &Interner,
    pos: Pos,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Get the partial text being typed
    let text = source.source();
    let offset = pos.offset() as usize;
    let prefix = get_word_before(text, offset);

    // Add matching commands from registry
    for cmd in registry.all_commands() {
        if prefix.is_empty() || cmd.name.starts_with(&prefix.to_uppercase()) {
            items.push(
                CompletionItem::new(cmd.name, CompletionItemKind::Command)
                    .with_detail(format!("Library {}", cmd.lib)),
            );
        }
    }

    // Add matching variables from analysis
    for def in analysis.symbols.definitions() {
        let name = &def.name;
        if prefix.is_empty()
            || name.starts_with(&prefix)
            || name.to_lowercase().starts_with(&prefix.to_lowercase())
        {
            let kind = match def.kind {
                DefinitionKind::Global => CompletionItemKind::Variable,
                DefinitionKind::Local => CompletionItemKind::Variable,
                DefinitionKind::LoopVar => CompletionItemKind::Variable,
            };
            items.push(CompletionItem::new(name.clone(), kind));
        }
    }

    // Suppress unused warning
    let _ = interner;

    items
}

/// Get hover information at a position.
pub fn hover(
    analysis: &AnalysisResult,
    registry: &Registry,
    interner: &Interner,
    pos: Pos,
) -> Option<HoverResult> {
    // Check if position is on a definition
    for def in analysis.symbols.definitions() {
        if def.span.contains(pos) {
            return Some(make_definition_hover(def));
        }
    }

    // Check if position is on a reference
    for reference in analysis.symbols.references() {
        if reference.span.contains(pos) {
            // Try to find the definition
            if let Some(def_id) = reference.definition
                && let Some(def) = analysis.symbols.get_definition(def_id)
            {
                return Some(make_definition_hover(def));
            }
            // Unresolved reference - might be a command
            return hover_command(&reference.name, reference.span, registry);
        }
    }

    // Suppress unused warning
    let _ = interner;

    None
}

fn make_definition_hover(def: &Definition) -> HoverResult {
    let kind_str = match def.kind {
        DefinitionKind::Global => {
            if def.arity.is_some() {
                "function"
            } else {
                "global variable"
            }
        }
        DefinitionKind::Local => "local variable",
        DefinitionKind::LoopVar => "loop variable",
    };

    let mut details = Vec::new();

    if let Some(ref ty) = def.value_type {
        details.push(format!("Type: `{}`", ty));
    }

    if let Some(arity) = def.arity {
        details.push(format!("Arity: {}", arity));
    }

    let detail_str = if details.is_empty() {
        String::new()
    } else {
        format!("\n\n{}", details.join("\n"))
    };

    HoverResult {
        contents: format!("**{}** `{}`{}", kind_str, def.name, detail_str),
        range: def.span,
    }
}

fn hover_command(name: &str, span: Span, registry: &Registry) -> Option<HoverResult> {
    // Look up command in registry
    if let Some((lib_id, cmd_id)) = registry.find_command(name) {
        let mut parts = Vec::new();

        // Get library name
        if let Some(lib_name) = registry.get_library_name(lib_id) {
            parts.push(format!("Library: {}", lib_name));
        }

        // Get stack effect
        // Use an empty type stack for static effect lookup
        let types = crate::types::CStack::new();
        let effect = registry.get_command_effect(lib_id, cmd_id, &types);
        let notation = effect.to_notation();
        if notation != "(dynamic)" {
            parts.push(format!("Effect: `{}`", notation));
        }

        let details = if parts.is_empty() {
            String::new()
        } else {
            format!("\n\n{}", parts.join("\n\n"))
        };

        return Some(HoverResult {
            contents: format!("**{}**{}", name, details),
            range: span,
        });
    }
    None
}

/// Go to the definition of the symbol at the given position.
pub fn goto_definition(
    analysis: &AnalysisResult,
    interner: &Interner,
    pos: Pos,
) -> Option<GotoResult> {
    // Check if we're on a reference
    for reference in analysis.symbols.references() {
        if reference.span.contains(pos)
            && let Some(def_id) = reference.definition
            && let Some(def) = analysis.symbols.get_definition(def_id)
        {
            return Some(GotoResult { span: def.span });
        }
    }

    // Check if we're already on a definition
    for def in analysis.symbols.definitions() {
        if def.span.contains(pos) {
            return Some(GotoResult { span: def.span });
        }
    }

    // Suppress unused warning
    let _ = interner;

    None
}

/// Find all references to the symbol at the given position.
pub fn find_references(
    analysis: &AnalysisResult,
    interner: &Interner,
    pos: Pos,
    include_definition: bool,
) -> Option<ReferenceResult> {
    // Find which symbol we're looking for
    let target_name: Option<String> = analysis
        .symbols
        .definitions()
        .find(|def| def.span.contains(pos))
        .map(|def| def.name.clone())
        .or_else(|| {
            analysis
                .symbols
                .references()
                .find(|r| r.span.contains(pos))
                .map(|r| r.name.clone())
        });

    let name = target_name?;

    let mut locations = Vec::new();

    // Include definition if requested
    if include_definition {
        for def in analysis.symbols.definitions() {
            if def.name == name {
                locations.push(def.span);
            }
        }
    }

    // Include all references
    for reference in analysis.symbols.references() {
        if reference.name == name {
            locations.push(reference.span);
        }
    }

    // Suppress unused warning
    let _ = interner;

    if locations.is_empty() {
        None
    } else {
        Some(ReferenceResult { locations })
    }
}

/// Get semantic tokens for syntax highlighting.
pub fn semantic_tokens(analysis: &IncrementalAnalysis) -> Vec<SemanticToken> {
    let mut tokens = Vec::new();
    let result = analysis.result();

    // Add tokens for definitions
    for def in result.symbols.definitions() {
        let token_type = match def.kind {
            DefinitionKind::Global => SemanticTokenType::Variable,
            DefinitionKind::Local => SemanticTokenType::Parameter,
            DefinitionKind::LoopVar => SemanticTokenType::Parameter,
        };
        tokens.push(SemanticToken {
            start: def.span.start(),
            length: def.span.len(),
            token_type: token_type.index(),
            modifiers: 1, // definition modifier
        });
    }

    // Add tokens for references
    for reference in result.symbols.references() {
        tokens.push(SemanticToken {
            start: reference.span.start(),
            length: reference.span.len(),
            token_type: SemanticTokenType::Variable.index(),
            modifiers: 0,
        });
    }

    // Sort by position
    tokens.sort_by_key(|t| t.start.offset());

    tokens
}

/// Encode semantic tokens for LSP (delta encoding).
///
/// LSP uses 0-indexed line and column positions, while our `line_col`
/// returns 1-indexed values. This function handles the conversion.
pub fn encode_semantic_tokens(tokens: &[SemanticToken], source: &SourceFile) -> Vec<u32> {
    let mut result = Vec::with_capacity(tokens.len() * 5);
    let mut prev_line = 0u32;
    let mut prev_char = 0u32;

    for token in tokens {
        let lc = source.line_col(token.start);
        // Convert from 1-indexed to 0-indexed for LSP
        let line = lc.line.saturating_sub(1);
        let col = lc.col.saturating_sub(1);

        let delta_line = line - prev_line;
        let delta_char = if delta_line == 0 {
            col - prev_char
        } else {
            col
        };

        result.push(delta_line);
        result.push(delta_char);
        result.push(token.length);
        result.push(token.token_type);
        result.push(token.modifiers);

        prev_line = line;
        prev_char = col;
    }

    result
}

/// Get document symbols for the outline view.
pub fn document_symbols(analysis: &AnalysisResult, interner: &Interner) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for def in analysis.symbols.definitions() {
        let kind = match def.kind {
            DefinitionKind::Global => DocumentSymbolKind::Variable,
            DefinitionKind::Local => DocumentSymbolKind::Local,
            DefinitionKind::LoopVar => DocumentSymbolKind::LoopVariable,
        };

        // Check if this is a program definition
        let kind = if let Some(ref ty) = def.value_type {
            if ty.is_program() {
                DocumentSymbolKind::Function
            } else {
                kind
            }
        } else {
            kind
        };

        symbols.push(DocumentSymbol {
            name: def.name.clone(),
            detail: def.value_type.as_ref().map(|t| format!("{:?}", t)),
            kind,
            range: def.span,
            selection_range: def.span,
        });
    }

    // Suppress unused warning
    let _ = interner;

    symbols
}

// ============================================================================
// Helpers
// ============================================================================

/// Get the word being typed before the cursor.
fn get_word_before(text: &str, offset: usize) -> String {
    let before = &text[..offset.min(text.len())];
    let start = before
        .rfind(|c: char| c.is_whitespace() || "«»{}()[]".contains(c))
        .map(|i| i + 1)
        .unwrap_or(0);
    before[start..].to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn completion_item_new() {
        let item = CompletionItem::new("DUP", CompletionItemKind::Command);
        assert_eq!(item.label, "DUP");
        assert_eq!(item.kind, CompletionItemKind::Command);
    }

    #[test]
    fn completion_item_with_detail() {
        let item = CompletionItem::new("DUP", CompletionItemKind::Command)
            .with_detail("Duplicates top of stack");
        assert_eq!(item.detail, Some("Duplicates top of stack".into()));
    }

    #[test]
    fn semantic_token_type_legend() {
        let legend = SemanticTokenType::legend();
        assert!(legend.contains(&"keyword"));
        assert!(legend.contains(&"variable"));
    }

    #[test]
    fn semantic_token_type_index() {
        assert_eq!(SemanticTokenType::Keyword.index(), 0);
        assert_eq!(SemanticTokenType::Variable.index(), 6);
    }

    #[test]
    fn get_word_before_simple() {
        assert_eq!(get_word_before("DU", 2), "DU");
        assert_eq!(get_word_before("1 2 DU", 6), "DU");
        assert_eq!(get_word_before("1 2 ", 4), "");
    }

    #[test]
    fn document_symbol_kind() {
        let sym = DocumentSymbol {
            name: "test".into(),
            detail: None,
            kind: DocumentSymbolKind::Variable,
            range: Span::DUMMY,
            selection_range: Span::DUMMY,
        };
        assert_eq!(sym.kind, DocumentSymbolKind::Variable);
    }
}
