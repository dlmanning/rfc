//! WASM-specific WIT bindings and exports.

use std::{cell::RefCell, collections::HashMap, path::Path};

use ::rpl::{
    AnalysisSession, Pos, Session, Span,
    source::{LineCol, SourceFile, SourceId},
    value::Value,
};
use ::rpl_project::{Project, ProjectIndex, ValueType};

wit_bindgen::generate!({
    world: "ide",
    path: "wit/ide.wit",
});

use self::{
    exports::rpl::ide::{
        file::Guest as FileGuest, project::Guest as ProjectGuest, repl::Guest as ReplGuest,
    },
    rpl::ide::types::{
        Diagnostic, DocumentSymbol, EvalResult, HoverInfo, Position, Range, SemanticToken,
        Severity, StackValue, SymbolKind, TokenType, TreeNode,
    },
};

struct RplIde;
export!(RplIde);

// ============================================================================
// State
// ============================================================================

thread_local! {
    static STATE: RefCell<State> = RefCell::new(State::new());
}

/// Combined project + index, always kept in sync.
struct LoadedProject {
    project: Project,
    index: ProjectIndex,
}

struct State {
    repl: Session,
    projects: HashMap<String, LoadedProject>,
}

impl State {
    fn new() -> Self {
        Self {
            repl: Self::create_session(),
            projects: HashMap::new(),
        }
    }

    fn create_session() -> Session {
        let mut session = Session::new();
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        rpl_sr5::register_interfaces(session.interfaces_mut());
        rpl_stdlib::register_lowerers(session.lowerers_mut());
        rpl_stdlib::register_executors(session.executors_mut());
        session
    }

    fn create_analysis_session() -> AnalysisSession {
        let mut session = AnalysisSession::new();
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        rpl_sr5::register_interfaces(session.interfaces_mut());
        session
    }

    /// Load a project if not already cached. Returns the project path if successful.
    fn ensure_project(&mut self, file_path: &str) -> Option<String> {
        let project_path = find_project_root(file_path)?;

        if !self.projects.contains_key(&project_path) {
            let project = Project::load(&project_path).ok()?;
            let index = ProjectIndex::build_with(Path::new(&project_path), |reg| {
                rpl_sr5::register_interfaces(reg);
            }).ok()?;
            self.projects
                .insert(project_path.clone(), LoadedProject { project, index });
        }

        Some(project_path)
    }

    /// Get analysis context for a file (if it's in a project).
    fn context_for(&self, file_path: Option<&str>) -> Option<::rpl::analysis::Context> {
        let project_path = file_path.and_then(find_project_root)?;
        self.projects
            .get(&project_path)
            .map(|p| p.index.to_context())
    }
}

/// Access state with a closure.
fn with_state<T>(f: impl FnOnce(&mut State) -> T) -> T {
    STATE.with(|s| f(&mut s.borrow_mut()))
}

// ============================================================================
// Conversions
// ============================================================================

fn to_position(lc: LineCol) -> Position {
    Position {
        line: lc.line,
        character: lc.col,
    }
}

fn to_range(file: &SourceFile, span: Span) -> Range {
    Range {
        start: to_position(file.line_col(span.start())),
        end: to_position(file.line_col(span.end())),
    }
}

fn to_severity(s: ::rpl::analysis::Severity) -> Severity {
    match s {
        ::rpl::analysis::Severity::Error => Severity::Error,
        ::rpl::analysis::Severity::Warning => Severity::Warning,
        ::rpl::analysis::Severity::Hint => Severity::Hint,
    }
}

fn to_symbol_kind(k: ::rpl::lsp::DocumentSymbolKind) -> SymbolKind {
    match k {
        ::rpl::lsp::DocumentSymbolKind::Function => SymbolKind::Function,
        ::rpl::lsp::DocumentSymbolKind::Variable => SymbolKind::Variable,
        ::rpl::lsp::DocumentSymbolKind::Local => SymbolKind::Local,
        ::rpl::lsp::DocumentSymbolKind::LoopVariable => SymbolKind::LoopVariable,
    }
}

fn to_token_type(index: u32) -> TokenType {
    match index {
        0 => TokenType::Keyword,
        1 => TokenType::Number,
        2 => TokenType::StringLiteral,
        3 => TokenType::Comment,
        4 => TokenType::Operator,
        5 => TokenType::Function,
        6 => TokenType::Variable,
        7 => TokenType::Parameter,
        _ => TokenType::Variable,
    }
}

fn to_stack_value(v: &Value) -> StackValue {
    StackValue {
        type_name: v.type_name().to_string(),
        display: format!("{}", v),
    }
}

fn to_eval_result(result: Result<Vec<Value>, impl std::fmt::Display>) -> EvalResult {
    match result {
        Ok(values) => EvalResult {
            stack: values.iter().map(to_stack_value).collect(),
            error: None,
        },
        Err(e) => EvalResult {
            stack: Vec::new(),
            error: Some(e.to_string()),
        },
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Walk up from a file path to find project.toml.
fn find_project_root(file_path: &str) -> Option<String> {
    let mut dir = Path::new(file_path).parent()?;
    loop {
        if dir.join("project.toml").exists() {
            return Some(dir.to_string_lossy().into_owned());
        }
        dir = dir.parent()?;
    }
}

/// Extract word at byte offset.
fn word_at(source: &str, offset: usize) -> Option<(&str, Span)> {
    let bytes = source.as_bytes();
    if offset >= bytes.len() || !is_word_char(bytes[offset]) {
        return None;
    }

    let mut start = offset;
    while start > 0 && is_word_char(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = offset;
    while end < bytes.len() && is_word_char(bytes[end]) {
        end += 1;
    }

    let span = Span::new(Pos::new(start as u32), Pos::new(end as u32));
    Some((&source[start..end], span))
}

fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'/' || b == b'-'
}

// ============================================================================
// File Interface
// ============================================================================

impl FileGuest for RplIde {
    fn check(content: String, file_path: Option<String>) -> Vec<Diagnostic> {
        with_state(|state| {
            // Try cached analysis first for project files
            if let Some(ref path) = file_path {
                if let Some(project_path) = state.ensure_project(path) {
                    if let Some(loaded) = state.projects.get(&project_path) {
                        if let Some(key) = loaded.index.key_for_path(Path::new(path)) {
                            if let Some(analysis) = loaded.index.get_cached_analysis(&key, &content) {
                                // Use cached diagnostics directly
                                let file = SourceFile::new(
                                    SourceId::new(0),
                                    "cached.rpl".into(),
                                    content,
                                );
                                return analysis
                                    .diagnostics
                                    .iter()
                                    .map(|d| Diagnostic {
                                        range: to_range(&file, d.span),
                                        severity: to_severity(d.severity),
                                        message: d.message.clone(),
                                    })
                                    .collect();
                            }
                        }
                    }
                }
            }

            // Fallback: fresh analysis (non-project files or changed content)
            let mut session = State::create_analysis_session();
            if let Some(ctx) = state.context_for(file_path.as_deref()) {
                session.set_context(ctx);
            }

            let id = session.set_source("input.rpl", &content);
            let file = SourceFile::new(id, "input.rpl".into(), content);

            session
                .diagnostics(id)
                .into_iter()
                .map(|d| Diagnostic {
                    range: to_range(&file, d.span),
                    severity: to_severity(d.severity),
                    message: d.message.clone(),
                })
                .collect()
        })
    }

    fn run(content: String, file_path: Option<String>) -> EvalResult {
        with_state(|state| {
            if let Some(project_path) = file_path.as_deref().and_then(|p| state.ensure_project(p)) {
                // Project mode: run entry point
                let project = &mut state.projects.get_mut(&project_path).unwrap().project;
                to_eval_result(project.run())
            } else {
                // Standalone mode: eval as script
                state.repl.reset_vm();
                to_eval_result(state.repl.eval(&content))
            }
        })
    }

    fn tokens(content: String) -> Vec<SemanticToken> {
        let mut session = State::create_session();
        let id = session.set_source("input.rpl", &content);
        let file = SourceFile::new(id, "input.rpl".into(), content);

        session
            .semantic_tokens(id)
            .into_iter()
            .map(|t| {
                let lc = file.line_col(t.start);
                SemanticToken {
                    line: lc.line,
                    start: lc.col,
                    length: t.length,
                    token_type: to_token_type(t.token_type),
                }
            })
            .collect()
    }

    fn symbols(content: String, file_path: Option<String>) -> Vec<DocumentSymbol> {
        with_state(|state| {
            // Ensure project loaded
            let project_path = file_path.as_deref().and_then(|p| state.ensure_project(p));

            let mut session = State::create_analysis_session();
            if let Some(ctx) = state.context_for(file_path.as_deref()) {
                session.set_context(ctx);
            }

            let id = session.set_source("input.rpl", &content);
            let file = SourceFile::new(id, "input.rpl".into(), content);

            // Get basic symbols from fresh analysis
            let mut symbols: Vec<DocumentSymbol> = session
                .document_symbols(id)
                .into_iter()
                .map(|sym| DocumentSymbol {
                    name: sym.name,
                    detail: sym.detail,
                    kind: to_symbol_kind(sym.kind),
                    range: to_range(&file, sym.range),
                    selection_range: to_range(&file, sym.selection_range),
                })
                .collect();

            // If we have a project, update details with resolved types from shared symbol table
            if let Some(ref project_path) = project_path {
                if let Some(loaded) = state.projects.get(project_path) {
                    let project_symbols = loaded.index.symbols();
                    for sym in &mut symbols {
                        // Look up in project's shared symbol table for resolved types
                        if let Some(def) = project_symbols.find_definitions_by_name(&sym.name).next() {
                            // Update detail with resolved type info
                            if let Some(ref sig) = def.signature {
                                sym.detail = Some(sig.to_string());
                            } else if let Some(ref ty) = def.value_type {
                                sym.detail = Some(ty.to_string());
                            }
                        }
                    }
                }
            }

            symbols
        })
    }

    fn hover(content: String, pos: Position, file_path: Option<String>) -> Option<HoverInfo> {
        with_state(|state| {
            // Ensure project loaded
            let project_path = file_path.as_deref().and_then(|p| state.ensure_project(p));

            // Create file for position calculation (doesn't require analysis)
            let file = SourceFile::new(SourceId::new(0), "input.rpl".into(), content.clone());
            let lc = LineCol::new(pos.line, pos.character);
            let byte_pos = file.pos_from_line_col(lc)?;

            // If we're in a project, try to get resolved types from the project's symbol table
            if let Some(ref project_path) = project_path {
                if let Some(loaded) = state.projects.get(project_path) {
                    if let Some((word, span)) = word_at(&content, byte_pos.offset() as usize) {
                        // Look up in project's shared symbol table for resolved types
                        if let Some(def) = loaded.index.symbols().find_definitions_by_name(word).next() {
                            let hover = ::rpl::lsp::make_definition_hover(def);
                            return Some(HoverInfo {
                                contents: hover.contents,
                                range: to_range(&file, span),
                            });
                        }

                        // Try project entry hover (for binary files, etc.)
                        if let Some(entry) = loaded.index.get(word) {
                            let contents = format_project_entry_hover(word, entry);
                            return Some(HoverInfo {
                                contents,
                                range: to_range(&file, span),
                            });
                        }
                    }

                    // For project files with unchanged content, use cached analysis for commands
                    if let Some(ref path) = file_path {
                        if let Some(key) = loaded.index.key_for_path(Path::new(path)) {
                            if let Some(analysis) = loaded.index.get_cached_analysis(&key, &content) {
                                // Use cached analysis for command hover
                                if let Some(hover) = ::rpl::lsp::hover(
                                    analysis,
                                    loaded.index.interfaces(),
                                    loaded.index.interner(),
                                    byte_pos,
                                ) {
                                    return Some(HoverInfo {
                                        contents: hover.contents,
                                        range: to_range(&file, hover.range),
                                    });
                                }
                            }
                        }
                    }
                }
            }

            // Fallback: fresh analysis for non-project files or changed content
            let mut session = State::create_analysis_session();
            if let Some(ctx) = state.context_for(file_path.as_deref()) {
                session.set_context(ctx);
            }
            let id = session.set_source("input.rpl", &content);

            // Try local hover (for commands, etc.)
            if let Some(hover) = session.hover(id, byte_pos) {
                return Some(HoverInfo {
                    contents: hover.contents,
                    range: to_range(&file, hover.range),
                });
            }

            // Try project entry hover as last resort
            let project_path = project_path?;
            let loaded = state.projects.get(&project_path)?;
            let (word, span) = word_at(&content, byte_pos.offset() as usize)?;
            let entry = loaded.index.get(word)?;

            let contents = format_project_entry_hover(word, entry);
            Some(HoverInfo {
                contents,
                range: to_range(&file, span),
            })
        })
    }

    fn project_path(file_path: String) -> Option<String> {
        with_state(|state| state.ensure_project(&file_path))
    }

    fn disassemble(content: String, file_path: Option<String>) -> String {
        with_state(|state| {
            if let Some(project_path) = file_path.as_deref().and_then(|p| state.ensure_project(p)) {
                disassemble_project(state, &project_path)
            } else {
                disassemble_standalone(&content)
            }
        })
    }
}

fn disassemble_project(state: &State, project_path: &str) -> String {
    let Some(loaded) = state.projects.get(project_path) else {
        return String::new();
    };

    let mut output = String::new();
    let mut keys: Vec<_> = loaded.index.entries.keys().collect();
    keys.sort();

    for key in keys {
        let Some(value) = loaded.project.session.vm().directory.lookup(key) else {
            continue;
        };

        if let Some(compiled) = value.as_program() {
            let disasm = ::rpl::DisassembledProgram::from_program_data(compiled, Some(key));
            output.push_str(&disasm.to_string());
            output.push('\n');
        } else {
            output.push_str(key);
            output.push_str(":\n");
            output.push_str(&format!("  ({}: {})\n\n", value.type_name(), value));
        }
    }

    output
}

fn disassemble_standalone(content: &str) -> String {
    let mut session = State::create_session();
    match session.compile(content) {
        Ok(program) => {
            let disasm = ::rpl::DisassembledProgram::from_compiled(&program, None);
            disasm.to_string()
        }
        Err(_) => String::new(),
    }
}

fn format_project_entry_hover(name: &str, entry: &::rpl_project::IndexEntry) -> String {
    let mut s = format!("**{}**\n\n", name);

    match entry.value_type {
        ValueType::Program => {
            if let Some(ref sig) = entry.signature {
                s.push_str(&format!("`{}`\n\n", sig));
            }
            s.push_str("*Project program*");
        }
        ValueType::List => s.push_str("*Project list*"),
        ValueType::Integer => s.push_str("*Project integer*"),
        ValueType::Real => s.push_str("*Project real*"),
        ValueType::String => s.push_str("*Project string*"),
        _ => s.push_str(&format!("*Project {:?}*", entry.value_type)),
    }

    s
}

// ============================================================================
// Project Interface
// ============================================================================

impl ProjectGuest for RplIde {
    fn list_open() -> Vec<String> {
        with_state(|state| state.projects.keys().cloned().collect())
    }

    fn open(path: String) -> Result<(), String> {
        with_state(|state| {
            if state.projects.contains_key(&path) {
                return Ok(());
            }

            let project = Project::load(&path).map_err(|e| e.to_string())?;
            let index = ProjectIndex::build_with(Path::new(&path), |reg| {
                rpl_sr5::register_interfaces(reg);
            }).map_err(|e| e.to_string())?;
            state
                .projects
                .insert(path, LoadedProject { project, index });
            Ok(())
        })
    }

    fn diagnostics(path: String) -> Vec<Diagnostic> {
        with_state(|state| {
            let Some(loaded) = state.projects.get(&path) else {
                return Vec::new();
            };

            loaded
                .index
                .entries
                .values()
                .filter_map(|entry| {
                    let analysis = entry.analysis.as_ref()?;
                    Some((entry, analysis))
                })
                .flat_map(|(entry, analysis)| {
                    let file = SourceFile::new(SourceId::new(0), "".into(), entry.source.clone());
                    analysis.diagnostics.iter().map(move |d| Diagnostic {
                        range: to_range(&file, d.span),
                        severity: to_severity(d.severity),
                        message: d.message.clone(),
                    })
                })
                .collect()
        })
    }

    fn tree(path: String) -> Vec<TreeNode> {
        with_state(|state| {
            let Some(loaded) = state.projects.get(&path) else {
                return Vec::new();
            };

            loaded
                .index
                .entries
                .values()
                .map(|e| TreeNode {
                    key: e.key.clone(),
                    name: e.key.rsplit('/').next().unwrap_or(&e.key).to_string(),
                    signature: e.signature.as_ref().map(|s| s.to_string()),
                })
                .collect()
        })
    }

    fn signature(path: String, key: String) -> Option<String> {
        with_state(|state| {
            state
                .projects
                .get(&path)?
                .index
                .get(&key)?
                .signature
                .as_ref()
                .map(|s| s.to_string())
        })
    }

    fn run(path: String) -> EvalResult {
        with_state(|state| {
            let Some(loaded) = state.projects.get_mut(&path) else {
                return EvalResult {
                    stack: Vec::new(),
                    error: Some(format!("Project not loaded: {}", path)),
                };
            };
            to_eval_result(loaded.project.run())
        })
    }

    fn reload(path: String) {
        with_state(|state| {
            if let Ok(project) = Project::load(&path)
                && let Ok(index) = ProjectIndex::build_with(Path::new(&path), |reg| {
                    rpl_sr5::register_interfaces(reg);
                })
            {
                state
                    .projects
                    .insert(path, LoadedProject { project, index });
            }
        });
    }

    fn close(path: String) {
        with_state(|state| {
            state.projects.remove(&path);
        });
    }
}

// ============================================================================
// REPL Interface
// ============================================================================

impl ReplGuest for RplIde {
    fn evaluate(code: String) -> EvalResult {
        with_state(|state| match state.repl.eval_repl(&code) {
            Ok(()) => EvalResult {
                stack: state
                    .repl
                    .vm()
                    .stack_contents()
                    .iter()
                    .map(to_stack_value)
                    .collect(),
                error: None,
            },
            Err(e) => EvalResult {
                stack: Vec::new(),
                error: Some(e.to_string()),
            },
        })
    }

    fn stack() -> Vec<StackValue> {
        with_state(|state| {
            state
                .repl
                .vm()
                .stack_contents()
                .iter()
                .map(to_stack_value)
                .collect()
        })
    }

    fn reset() {
        with_state(|state| state.repl.reset_vm());
    }
}
