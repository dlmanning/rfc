//! LSP request handlers.
//!
//! Bridges LSP protocol requests to the rpl AnalysisSession API.

use std::collections::HashMap;
use std::path::PathBuf;

use lsp_types::*;
use rpl::{
    AnalysisSession, Pos, SourceId, Span,
    analysis::Severity,
    lsp::{
        CompletionItemKind as RplCompletionKind, DocumentSymbolKind, SemanticModifier,
        SemanticTokenType,
    },
};
use rpl_project::ProjectIndex;

/// Server state holding the analysis session and document mappings.
///
/// Uses [`AnalysisSession`] rather than full [`Session`] since the LSP
/// only needs parsing and analysis - not compilation or execution.
pub struct ServerState {
    pub session: AnalysisSession,
    /// Maps URI strings to SourceIds
    pub documents: HashMap<String, SourceId>,
    /// Enable verbose hover with debug info
    pub verbose_hover: bool,
    /// Maps project root paths to their indexes
    pub projects: HashMap<PathBuf, ProjectIndex>,
    /// Maps document URIs to their project root (if any)
    pub document_projects: HashMap<String, PathBuf>,
}

impl ServerState {
    /// Create a new server state with standard library interfaces registered.
    pub fn new() -> Self {
        let mut session = AnalysisSession::new();
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        Self::with_session(session)
    }

    /// Create a server state with a pre-configured analysis session.
    ///
    /// Use this to add application-specific library interfaces:
    ///
    /// ```ignore
    /// let mut session = AnalysisSession::new();
    /// session.interfaces_mut().add(MyCustomLib::interface());
    /// let state = ServerState::with_session(session);
    /// ```
    pub fn with_session(session: AnalysisSession) -> Self {
        Self {
            session,
            documents: HashMap::new(),
            verbose_hover: false,
            projects: HashMap::new(),
            document_projects: HashMap::new(),
        }
    }

    /// Find the project root for a file path by walking up looking for project.toml.
    pub fn find_project_root(path: &std::path::Path) -> Option<PathBuf> {
        let mut current = path.parent()?;
        loop {
            if current.join("project.toml").exists() {
                return Some(current.to_owned());
            }
            current = current.parent()?;
        }
    }

    /// Get or build a project index for a project root.
    pub fn get_or_build_project(&mut self, project_root: &std::path::Path) -> Option<&ProjectIndex> {
        // Check if already cached
        if self.projects.contains_key(project_root) {
            return self.projects.get(project_root);
        }

        // Build new index
        match ProjectIndex::build(project_root) {
            Ok(index) => {
                self.projects.insert(project_root.to_owned(), index);
                self.projects.get(project_root)
            }
            Err(e) => {
                log::warn!("Failed to build project index for {:?}: {:?}", project_root, e);
                None
            }
        }
    }

    /// Get the project index for a document URI, if it's in a project.
    pub fn get_project_for_uri(&self, uri: &str) -> Option<&ProjectIndex> {
        let project_root = self.document_projects.get(uri)?;
        self.projects.get(project_root)
    }

    /// Get the project key for a document URI (e.g., "lib/square" for "lib/square.rpl").
    pub fn get_project_key_for_uri(&self, uri: &str) -> Option<String> {
        let project_root = self.document_projects.get(uri)?;
        let file_path = uri_to_path(uri)?;
        rpl_project::loader::path_to_key(project_root, &file_path).ok()
    }

    /// Invalidate the project cache for a URI, forcing rebuild on next access.
    pub fn invalidate_project_for_uri(&mut self, uri: &str) {
        if let Some(project_root) = self.document_projects.get(uri).cloned() {
            self.projects.remove(&project_root);
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
            workspace_symbol_provider: Some(OneOf::Left(true)),
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

    // Detect if file is in a project
    if let Some(file_path) = uri_to_path(&uri)
        && let Some(project_root) = ServerState::find_project_root(&file_path) {
            // Store the document-to-project mapping
            state.document_projects.insert(uri.clone(), project_root.clone());
            // Ensure project index is built and update session context
            state.get_or_build_project(&project_root);
            // Update context from project (separate borrow scope)
            if let Some(project) = state.projects.get(&project_root) {
                state.session.set_context(project.to_context());
            }
        }

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

    // Note: ProjectIndex is NOT rebuilt here because it reads from disk.
    // Cross-file analysis updates on save (see handle_did_save).
}

/// Handle textDocument/didSave notification.
pub fn handle_did_save(state: &mut ServerState, params: lsp_types::DidSaveTextDocumentParams) {
    let uri = params.text_document.uri.to_string();

    // Invalidate and rebuild project index now that disk content is updated
    state.invalidate_project_for_uri(&uri);
    if let Some(project_root) = state.document_projects.get(&uri).cloned() {
        // Rebuild project index
        state.get_or_build_project(&project_root);
        // Update context from project (separate borrow scope)
        if let Some(project) = state.projects.get(&project_root) {
            state.session.set_context(project.to_context());
        }
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
    let source = state.session.sources().get(id)?;
    let pos = lsp_pos_to_offset(&params.text_document_position.position, source.source());

    let items = state.session.completions(id, pos);
    let mut lsp_items: Vec<CompletionItem> = items
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

    // Add project entries to completions
    if let Some(project) = state.get_project_for_uri(&uri) {
        for (key, entry) in &project.entries {
            // Skip if item already exists (e.g., from local analysis)
            if lsp_items.iter().any(|i| i.label == *key) {
                continue;
            }

            let kind = match entry.value_type {
                rpl_project::ValueType::Program => CompletionItemKind::FUNCTION,
                rpl_project::ValueType::List => CompletionItemKind::VARIABLE,
                _ => CompletionItemKind::VALUE,
            };

            let detail = entry.signature.as_ref().map(|sig| sig.to_string());
            let documentation = if entry.value_type.is_program() {
                entry.signature.as_ref().map(|sig| {
                    Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**{}**\n\n`{}`", key, sig),
                    })
                })
            } else {
                Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("**{}** ({:?})", key, entry.value_type),
                }))
            };

            lsp_items.push(CompletionItem {
                label: key.clone(),
                kind: Some(kind),
                detail,
                documentation,
                ..Default::default()
            });
        }
    }

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
    let source_text = state.session.sources().get(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position_params.position, &source_text);

    // First try local hover
    if let Some(result) = state.session.hover(id, pos) {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: result.contents,
            }),
            range: Some(span_to_range(result.range, &source_text)),
        });
    }

    // Check for project entry hover
    if let Some(project_root) = state.document_projects.get(&uri).cloned() {
        let word = get_word_at(&source_text, pos.offset() as usize);
        if !word.is_empty()
            && let Some(project) = state.projects.get(&project_root)
                && let Some(entry) = project.get(&word) {
                    // Build hover content for project entry
                    let mut content = format!("**{}**\n\n", word);

                    match entry.value_type {
                        rpl_project::ValueType::Program => {
                            if let Some(ref sig) = entry.signature {
                                content.push_str(&format!("`{}`\n\n", sig));
                            }
                            content.push_str("*Project program*");
                        }
                        rpl_project::ValueType::List => {
                            content.push_str("*Project list*");
                        }
                        rpl_project::ValueType::Integer => {
                            content.push_str("*Project integer*");
                        }
                        rpl_project::ValueType::Real => {
                            content.push_str("*Project real*");
                        }
                        rpl_project::ValueType::String => {
                            content.push_str("*Project string*");
                        }
                        _ => {
                            content.push_str(&format!("*Project {:?}*", entry.value_type));
                        }
                    }

                    // Calculate word span for range
                    let word_start = pos.offset() as usize;
                    // Find actual start by going back
                    let bytes = source_text.as_bytes();
                    let mut start = word_start;
                    while start > 0 && is_word_char(bytes[start - 1]) {
                        start -= 1;
                    }
                    let end = start + word.len();
                    let span = Span::new(Pos::new(start as u32), Pos::new(end as u32));

                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: Some(span_to_range(span, &source_text)),
                    });
                }
    }

    None
}

/// Check if a byte is a valid word character (for project paths).
fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'/' || b == b'-'
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
    let source_text = state.session.sources().get(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position_params.position, &source_text);

    // First try local definition
    if let Some(result) = state.session.definition(id, pos) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: params
                .text_document_position_params
                .text_document
                .uri
                .clone(),
            range: span_to_range(result.span, &source_text),
        }));
    }

    // Check for cross-file project reference
    if let Some(project_root) = state.document_projects.get(&uri).cloned() {
        // Extract word at cursor position
        let word = get_word_at(&source_text, pos.offset() as usize);
        if !word.is_empty() {
            // Check if it matches a project entry
            if let Some(project) = state.projects.get(&project_root)
                && let Some(entry) = project.get(&word) {
                    // Return location of the project file
                    let target_uri_str = path_to_uri(&entry.source_path);
                    let target_uri: Uri = target_uri_str.parse().ok()?;
                    // Read the target file to get the span as a range
                    let target_source = std::fs::read_to_string(&entry.source_path).ok()?;
                    let range = span_to_range(
                        entry.ast.first().map(|n| n.span).unwrap_or_default(),
                        &target_source,
                    );
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: target_uri,
                        range,
                    }));
                }
        }
    }

    None
}

/// Handle textDocument/references request.
pub fn handle_references(
    state: &mut ServerState,
    params: ReferenceParams,
) -> Option<Vec<Location>> {
    let uri = params.text_document_position.text_document.uri.to_string();
    let id = *state.documents.get(&uri)?;

    // Get source text first (need to release borrow before mutable call)
    let source_text = state.session.sources().get(id)?.source().to_string();
    let pos = lsp_pos_to_offset(&params.text_document_position.position, &source_text);

    let include_declaration = params.context.include_declaration;
    let result = state.session.references(id, pos, include_declaration)?;

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
    let source = state.session.sources().get(id)?;
    let encoded = rpl::lsp::encode_semantic_tokens(&tokens, source);

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
    let source = state.session.sources().get(id)?;
    let source_text = source.source().to_string();
    let doc_symbols = state.session.document_symbols(id);

    let lsp_symbols: Vec<lsp_types::DocumentSymbol> = doc_symbols
        .into_iter()
        .map(|s| {
            let kind = match s.kind {
                DocumentSymbolKind::Function => SymbolKind::FUNCTION,
                DocumentSymbolKind::Variable => SymbolKind::VARIABLE,
                DocumentSymbolKind::Local => SymbolKind::VARIABLE,
                DocumentSymbolKind::LoopVariable => SymbolKind::VARIABLE,
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

/// Handle workspace/symbol request.
///
/// Returns all project entries as symbols for workspace-wide navigation.
pub fn handle_workspace_symbol(
    state: &mut ServerState,
    params: WorkspaceSymbolParams,
) -> Option<WorkspaceSymbolResponse> {
    let query = params.query.to_lowercase();

    let mut symbols = Vec::new();

    // Collect symbols from all loaded projects
    for (project_root, project) in &state.projects {
        for (key, entry) in &project.entries {
            // Filter by query if provided
            if !query.is_empty() && !key.to_lowercase().contains(&query) {
                continue;
            }

            let kind = match entry.value_type {
                rpl_project::ValueType::Program => SymbolKind::FUNCTION,
                rpl_project::ValueType::List => SymbolKind::ARRAY,
                rpl_project::ValueType::Integer | rpl_project::ValueType::Real => {
                    SymbolKind::NUMBER
                }
                rpl_project::ValueType::String => SymbolKind::STRING,
                _ => SymbolKind::VARIABLE,
            };

            // Build URI and location
            let uri_str = path_to_uri(&entry.source_path);
            let uri: Uri = match uri_str.parse() {
                Ok(u) => u,
                Err(_) => continue,
            };

            // Read target file to compute range
            let range = if let Ok(source) = std::fs::read_to_string(&entry.source_path) {
                span_to_range(
                    entry.ast.first().map(|n| n.span).unwrap_or_default(),
                    &source,
                )
            } else {
                Range::default()
            };

            // Container name from directory structure
            let container_name = if key.contains('/') {
                key.rsplit_once('/').map(|(dir, _)| dir.to_string())
            } else {
                project_root
                    .file_name()
                    .map(|s| s.to_string_lossy().to_string())
            };

            #[allow(deprecated)] // SymbolInformation::deprecated is deprecated but required
            symbols.push(SymbolInformation {
                name: key.clone(),
                kind,
                tags: None,
                deprecated: None,
                location: Location { uri, range },
                container_name,
            });
        }
    }

    Some(WorkspaceSymbolResponse::Flat(symbols))
}

/// Get diagnostics for a document.
///
/// Note: Project entries are filtered at analysis time via the Context,
/// so no post-filtering is needed here.
pub fn get_diagnostics(state: &mut ServerState, uri: &str) -> Vec<Diagnostic> {
    let Some(&id) = state.documents.get(uri) else {
        return Vec::new();
    };

    let Some(source) = state.session.sources().get(id) else {
        return Vec::new();
    };

    let source_text = source.source().to_string();
    let diagnostics = state.session.diagnostics(id);

    diagnostics
        .iter()
        .map(|d| Diagnostic {
            range: span_to_range(d.span, &source_text),
            severity: Some(match d.severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
                Severity::Hint => DiagnosticSeverity::HINT,
            }),
            code: Some(NumberOrString::String(format!("{:?}", d.kind))),
            source: Some("rpl".into()),
            message: d.message.clone(),
            ..Default::default()
        })
        .collect()
}

// === Helper functions ===

fn uri_to_filename(uri: &str) -> String {
    // Extract filename from URI, or use the whole URI if no path
    uri.rsplit('/').next().unwrap_or(uri).to_string()
}

/// Convert a file:// URI to a PathBuf.
fn uri_to_path(uri: &str) -> Option<PathBuf> {
    if uri.starts_with("file://") {
        // Handle file:// URIs
        let path = uri.strip_prefix("file://")?;
        // On Windows, file:///C:/... has an extra slash
        #[cfg(windows)]
        let path = path.strip_prefix('/').unwrap_or(path);
        Some(PathBuf::from(path))
    } else {
        None
    }
}

/// Convert a PathBuf to a file:// URI.
fn path_to_uri(path: &std::path::Path) -> String {
    format!("file://{}", path.display())
}

/// Get the word at a given position in the source text.
/// A "word" here includes slashes for project paths like "lib/square".
fn get_word_at(source: &str, offset: usize) -> String {
    let bytes = source.as_bytes();
    if offset >= bytes.len() {
        return String::new();
    }

    if !is_word_char(bytes[offset]) {
        return String::new();
    }

    // Find start of word
    let mut start = offset;
    while start > 0 && is_word_char(bytes[start - 1]) {
        start -= 1;
    }

    // Find end of word
    let mut end = offset;
    while end < bytes.len() && is_word_char(bytes[end]) {
        end += 1;
    }

    source[start..end].to_string()
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
