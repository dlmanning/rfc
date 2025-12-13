//! LSP request handlers.
//!
//! Bridges LSP protocol requests to the Session API.

use std::collections::HashMap;

use lsp_types::*;
use rpl_session::{
    Pos, Session, SourceId, Span,
    lsp::{CompletionItemKind as RplCompletionKind, SemanticModifier, SemanticTokenType},
};

/// Server state holding the session and document mappings.
pub struct ServerState {
    pub session: Session,
    /// Maps URI strings to SourceIds
    pub documents: HashMap<String, SourceId>,
    /// Enable verbose hover with debug info
    pub verbose_hover: bool,
}

impl ServerState {
    pub fn new() -> Self {
        Self {
            session: Session::new(),
            documents: HashMap::new(),
            verbose_hover: false,
        }
    }
}

impl Default for ServerState {
    fn default() -> Self {
        Self::new()
    }
}

/// Handle initialize request.
pub fn handle_initialize(_params: InitializeParams) -> InitializeResult {
    InitializeResult {
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![" ".into(), "'".into()]),
                ..Default::default()
            }),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: SemanticTokenType::legend()
                            .iter()
                            .map(|s| lsp_types::SemanticTokenType::new(s))
                            .collect(),
                        token_modifiers: SemanticModifier::legend()
                            .iter()
                            .map(|s| SemanticTokenModifier::new(s))
                            .collect(),
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    ..Default::default()
                }),
            ),
            document_symbol_provider: Some(OneOf::Left(true)),
            ..Default::default()
        },
        server_info: Some(ServerInfo {
            name: "rpl-lsp".into(),
            version: Some(env!("CARGO_PKG_VERSION").into()),
        }),
    }
}

/// Handle textDocument/didOpen notification.
pub fn handle_did_open(state: &mut ServerState, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri.to_string();
    let text = params.text_document.text;
    let name = uri_to_filename(&uri);

    let id = state.session.set_source(&name, &text);
    state.documents.insert(uri, id);
}

/// Handle textDocument/didChange notification.
pub fn handle_did_change(state: &mut ServerState, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri.to_string();

    // We use full sync, so there's exactly one change with the full content
    if let Some(change) = params.content_changes.into_iter().next() {
        let name = uri_to_filename(&uri);
        let id = state.session.set_source(&name, &change.text);
        state.documents.insert(uri, id);
    }
}

/// Handle textDocument/didClose notification.
pub fn handle_did_close(state: &mut ServerState, params: DidCloseTextDocumentParams) {
    let uri = params.text_document.uri.to_string();
    state.documents.remove(&uri);
}

/// Handle textDocument/completion request.
pub fn handle_completion(
    state: &mut ServerState,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = params.text_document_position.text_document.uri.to_string();
    let id = *state.documents.get(&uri)?;
    let source = state.session.get_source(id)?;
    let pos = lsp_pos_to_offset(&params.text_document_position.position, source.source());

    let items = state.session.completions(id, pos);
    let lsp_items: Vec<CompletionItem> = items
        .into_iter()
        .map(|item| CompletionItem {
            label: item.label.clone(),
            kind: Some(match item.kind {
                RplCompletionKind::Variable => CompletionItemKind::VARIABLE,
                RplCompletionKind::Command => CompletionItemKind::FUNCTION,
                RplCompletionKind::Keyword => CompletionItemKind::KEYWORD,
                RplCompletionKind::Constant => CompletionItemKind::CONSTANT,
                RplCompletionKind::Operator => CompletionItemKind::OPERATOR,
            }),
            detail: item.detail,
            documentation: item.documentation.map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d,
                })
            }),
            insert_text: item.insert_text,
            ..Default::default()
        })
        .collect();

    Some(CompletionResponse::Array(lsp_items))
}

/// Handle textDocument/hover request.
pub fn handle_hover(state: &mut ServerState, params: HoverParams) -> Option<Hover> {
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    let id = *state.documents.get(&uri)?;

    // Get source text first (need to release borrow before mutable call)
    let source_text = state.session.get_source(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position_params.position, &source_text);

    let result = if state.verbose_hover {
        state.session.hover_verbose(id, pos)?
    } else {
        state.session.hover(id, pos)?
    };

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: result.contents,
        }),
        range: Some(span_to_range(result.range, &source_text)),
    })
}

/// Handle textDocument/definition request.
pub fn handle_definition(
    state: &mut ServerState,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    let id = *state.documents.get(&uri)?;

    // Get source text first (need to release borrow before mutable call)
    let source_text = state.session.get_source(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position_params.position, &source_text);

    let result = state.session.definition(id, pos)?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
        range: span_to_range(result.span, &source_text),
    }))
}

/// Handle textDocument/references request.
pub fn handle_references(
    state: &mut ServerState,
    params: ReferenceParams,
) -> Option<Vec<Location>> {
    let uri = params.text_document_position.text_document.uri.to_string();
    let id = *state.documents.get(&uri)?;

    // Get source text first (need to release borrow before mutable call)
    let source_text = state.session.get_source(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position.position, &source_text);

    let result = state.session.references(id, pos)?;

    let locations: Vec<Location> = result
        .locations
        .iter()
        .map(|span| Location {
            uri: params.text_document_position.text_document.uri.clone(),
            range: span_to_range(*span, &source_text),
        })
        .collect();

    Some(locations)
}

/// Handle textDocument/semanticTokens/full request.
pub fn handle_semantic_tokens_full(
    state: &mut ServerState,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let uri = params.text_document.uri.to_string();
    let id = *state.documents.get(&uri)?;

    let tokens = state.session.semantic_tokens(id);
    let source = state.session.get_source(id)?;
    let encoded = rpl_session::lsp::encode_semantic_tokens(&tokens, source);

    // Convert flat Vec<u32> into SemanticToken structs (groups of 5)
    let data: Vec<lsp_types::SemanticToken> = encoded
        .chunks_exact(5)
        .map(|chunk| lsp_types::SemanticToken {
            delta_line: chunk[0],
            delta_start: chunk[1],
            length: chunk[2],
            token_type: chunk[3],
            token_modifiers_bitset: chunk[4],
        })
        .collect();

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    }))
}

/// Handle textDocument/documentSymbol request.
pub fn handle_document_symbol(
    state: &mut ServerState,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = params.text_document.uri.to_string();
    let id = *state.documents.get(&uri)?;
    let source = state.session.get_source(id)?;
    let source_text = source.source().to_string();
    let doc_symbols = state.session.document_symbols(id)?;

    let lsp_symbols: Vec<lsp_types::DocumentSymbol> = doc_symbols
        .into_iter()
        .map(|s| {
            let kind = match s.kind {
                rpl_session::lsp::DocumentSymbolKind::Function => SymbolKind::FUNCTION,
                rpl_session::lsp::DocumentSymbolKind::Variable => SymbolKind::VARIABLE,
                rpl_session::lsp::DocumentSymbolKind::Local => SymbolKind::VARIABLE,
                rpl_session::lsp::DocumentSymbolKind::LoopVariable => SymbolKind::VARIABLE,
            };

            #[allow(deprecated)] // DocumentSymbol::deprecated is deprecated but required
            lsp_types::DocumentSymbol {
                name: s.name,
                detail: s.detail,
                kind,
                tags: None,
                deprecated: None,
                range: span_to_range(s.range, &source_text),
                selection_range: span_to_range(s.selection_range, &source_text),
                children: None,
            }
        })
        .collect();

    Some(DocumentSymbolResponse::Nested(lsp_symbols))
}

/// Get diagnostics for a document.
pub fn get_diagnostics(state: &mut ServerState, uri: &str) -> Vec<Diagnostic> {
    let Some(&id) = state.documents.get(uri) else {
        return Vec::new();
    };

    let Some(source) = state.session.get_source(id) else {
        return Vec::new();
    };

    let source_text = source.source().to_string();
    let diagnostics = state.session.diagnostics(id);

    diagnostics
        .iter()
        .map(|d| Diagnostic {
            range: span_to_range(d.span(), &source_text),
            severity: Some(match d.severity() {
                rpl_session::Severity::Error => DiagnosticSeverity::ERROR,
                rpl_session::Severity::Warning => DiagnosticSeverity::WARNING,
                rpl_session::Severity::Note => DiagnosticSeverity::INFORMATION,
            }),
            code: Some(NumberOrString::String(d.code().to_string())),
            source: Some("rpl".into()),
            message: d.message().to_string(),
            ..Default::default()
        })
        .collect()
}

// === Helper functions ===

fn uri_to_filename(uri: &str) -> String {
    // Extract filename from URI, or use the whole URI if no path
    uri.rsplit('/').next().unwrap_or(uri).to_string()
}

/// Convert LSP Position (0-indexed line/char) to byte offset Pos.
fn lsp_pos_to_offset(pos: &Position, source: &str) -> Pos {
    let mut offset = 0u32;
    let mut current_line = 0u32;

    for (i, ch) in source.char_indices() {
        if current_line == pos.line {
            // We're on the right line, count characters
            for (col, (j, c)) in source[i..].char_indices().enumerate() {
                if col as u32 == pos.character {
                    return Pos::new((i + j) as u32);
                }
                if c == '\n' {
                    break;
                }
            }
            // If we get here, position is past end of line
            return Pos::new((i + source[i..].find('\n').unwrap_or(source.len() - i)) as u32);
        }
        if ch == '\n' {
            current_line += 1;
        }
        offset = (i + ch.len_utf8()) as u32;
    }

    // Position is past end of file
    Pos::new(offset)
}

fn span_to_range(span: Span, source: &str) -> Range {
    let start = offset_to_position(span.start().offset() as usize, source);
    let end = offset_to_position(span.end().offset() as usize, source);
    Range { start, end }
}

fn offset_to_position(offset: usize, source: &str) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Position::new(line, col)
}
