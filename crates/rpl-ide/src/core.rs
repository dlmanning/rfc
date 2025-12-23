//! Core IDE logic, independent of WIT bindings.
//!
//! This module contains the actual implementation that can be tested directly.

use std::{collections::HashMap, path::Path};

use ::rpl::{
    AnalysisSession, Pos, Session, Span,
    source::{LineCol, SourceFile, SourceId},
    value::Value,
};
use ::rpl_project::{Project, ProjectIndex, ValueType};

// ============================================================================
// Types
// ============================================================================

/// Combined project + index, always kept in sync.
pub struct LoadedProject {
    pub project: Project,
    pub index: ProjectIndex,
}

/// Core IDE state.
pub struct IdeState {
    pub repl: Session,
    pub projects: HashMap<String, LoadedProject>,
}

/// A diagnostic message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
    pub severity: Severity,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Hint,
}

/// A stack value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue {
    pub type_name: String,
    pub display: String,
}

/// Evaluation result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalResult {
    pub stack: Vec<StackValue>,
    pub error: Option<String>,
}

/// Semantic token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticToken {
    pub line: u32,
    pub start: u32,
    pub length: u32,
    pub token_type: TokenType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Comment,
    Keyword,
    Number,
    StringLiteral,
    Operator,
    Function,
    Variable,
    Parameter,
}

/// Document symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentSymbol {
    pub name: String,
    pub detail: Option<String>,
    pub kind: SymbolKind,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Variable,
    Local,
    LoopVariable,
}

/// Hover information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoverInfo {
    pub contents: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

/// Project tree node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TreeNode {
    pub key: String,
    pub name: String,
    pub signature: Option<String>,
}


// ============================================================================
// State Implementation
// ============================================================================

impl IdeState {
    pub fn new() -> Self {
        Self {
            repl: Self::create_session(),
            projects: HashMap::new(),
        }
    }

    pub fn create_session() -> Session {
        let mut session = Session::new();
        ::rpl_stdlib::register_interfaces(session.interfaces_mut());
        ::rpl_stdlib::register_lowerers(session.lowerers_mut());
        ::rpl_stdlib::register_executors(session.executors_mut());
        session
    }

    pub fn create_analysis_session() -> AnalysisSession {
        let mut session = AnalysisSession::new();
        ::rpl_stdlib::register_interfaces(session.interfaces_mut());
        session
    }

    /// Load a project if not already cached. Returns the project path if successful.
    pub fn ensure_project(&mut self, file_path: &str) -> Option<String> {
        let project_path = find_project_root(file_path)?;

        if !self.projects.contains_key(&project_path) {
            let project = Project::load(&project_path).ok()?;
            let index = ProjectIndex::build(Path::new(&project_path)).ok()?;
            self.projects
                .insert(project_path.clone(), LoadedProject { project, index });
        }

        Some(project_path)
    }

    /// Get analysis context for a file (if it's in a project).
    pub fn context_for(&self, file_path: Option<&str>) -> Option<::rpl::analysis::Context> {
        let project_path = file_path.and_then(find_project_root)?;
        self.projects
            .get(&project_path)
            .map(|p| p.index.to_context())
    }

    /// Open a project explicitly.
    pub fn open_project(&mut self, path: &str) -> Result<(), String> {
        if self.projects.contains_key(path) {
            return Ok(());
        }

        let project = Project::load(path).map_err(|e| e.to_string())?;
        let index = ProjectIndex::build(Path::new(path)).map_err(|e| e.to_string())?;
        self.projects
            .insert(path.to_string(), LoadedProject { project, index });
        Ok(())
    }

    /// Close a project.
    pub fn close_project(&mut self, path: &str) {
        self.projects.remove(path);
    }

    /// Reload a project.
    pub fn reload_project(&mut self, path: &str) {
        if let Ok(project) = Project::load(path)
            && let Ok(index) = ProjectIndex::build(Path::new(path))
        {
            self.projects
                .insert(path.to_string(), LoadedProject { project, index });
        }
    }

    /// List open projects.
    pub fn list_projects(&self) -> Vec<String> {
        self.projects.keys().cloned().collect()
    }
}

impl Default for IdeState {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// File Operations
// ============================================================================

/// Check a file for diagnostics.
pub fn check_file(state: &mut IdeState, content: &str, file_path: Option<&str>) -> Vec<Diagnostic> {
    // Ensure project is loaded if file is in one
    if let Some(path) = file_path {
        state.ensure_project(path);
    }

    let mut session = IdeState::create_analysis_session();
    if let Some(ctx) = state.context_for(file_path) {
        session.set_context(ctx);
    }

    let id = session.set_source("input.rpl", content);
    let file = SourceFile::new(id, "input.rpl".into(), content.to_string());

    session
        .diagnostics(id)
        .into_iter()
        .map(|d| {
            let start = file.line_col(d.span.start());
            let end = file.line_col(d.span.end());
            Diagnostic {
                start_line: start.line,
                start_col: start.col,
                end_line: end.line,
                end_col: end.col,
                severity: match d.severity {
                    ::rpl::analysis::Severity::Error => Severity::Error,
                    ::rpl::analysis::Severity::Warning => Severity::Warning,
                    ::rpl::analysis::Severity::Hint => Severity::Hint,
                },
                message: d.message.clone(),
            }
        })
        .collect()
}

/// Run a file or project.
pub fn run_file(state: &mut IdeState, content: &str, file_path: Option<&str>) -> EvalResult {
    if let Some(project_path) = file_path.and_then(|p| state.ensure_project(p)) {
        // Project mode: run entry point
        let project = &mut state.projects.get_mut(&project_path).unwrap().project;
        to_eval_result(project.run())
    } else {
        // Standalone mode: eval as script
        state.repl.reset_vm();
        to_eval_result(state.repl.eval(content))
    }
}

/// Get semantic tokens.
pub fn get_tokens(content: &str) -> Vec<SemanticToken> {
    let mut session = IdeState::create_session();
    let id = session.set_source("input.rpl", content);
    let file = SourceFile::new(id, "input.rpl".into(), content.to_string());

    session
        .semantic_tokens(id)
        .into_iter()
        .map(|t| {
            let lc = file.line_col(t.start);
            SemanticToken {
                line: lc.line,
                start: lc.col,
                length: t.length,
                token_type: match t.token_type {
                    0 => TokenType::Keyword,
                    1 => TokenType::Number,
                    2 => TokenType::StringLiteral,
                    3 => TokenType::Comment,
                    4 => TokenType::Operator,
                    5 => TokenType::Function,
                    6 => TokenType::Variable,
                    7 => TokenType::Parameter,
                    _ => TokenType::Variable,
                },
            }
        })
        .collect()
}

/// Get document symbols.
pub fn get_symbols(content: &str) -> Vec<DocumentSymbol> {
    let mut session = IdeState::create_analysis_session();
    let id = session.set_source("input.rpl", content);
    let file = SourceFile::new(id, "input.rpl".into(), content.to_string());

    session
        .document_symbols(id)
        .into_iter()
        .map(|sym| {
            let start = file.line_col(sym.range.start());
            let end = file.line_col(sym.range.end());
            DocumentSymbol {
                name: sym.name,
                detail: sym.detail,
                kind: match sym.kind {
                    ::rpl::lsp::DocumentSymbolKind::Function => SymbolKind::Function,
                    ::rpl::lsp::DocumentSymbolKind::Variable => SymbolKind::Variable,
                    ::rpl::lsp::DocumentSymbolKind::Local => SymbolKind::Local,
                    ::rpl::lsp::DocumentSymbolKind::LoopVariable => SymbolKind::LoopVariable,
                },
                start_line: start.line,
                start_col: start.col,
                end_line: end.line,
                end_col: end.col,
            }
        })
        .collect()
}

/// Disassemble bytecode for a file or project, returning formatted text.
pub fn disassemble(state: &mut IdeState, content: &str, file_path: Option<&str>) -> String {
    if let Some(project_path) = file_path.and_then(|p| state.ensure_project(p)) {
        // Project mode: disassemble all entries
        disassemble_project(state, &project_path)
    } else {
        // Standalone mode: compile and disassemble the content
        disassemble_standalone(content)
    }
}

fn disassemble_project(state: &IdeState, project_path: &str) -> String {
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
    let mut session = IdeState::create_session();
    match session.compile(content) {
        Ok(program) => ::rpl::DisassembledProgram::from_compiled(&program, None).to_string(),
        Err(_) => String::new(),
    }
}

/// Get hover information.
pub fn get_hover(
    state: &mut IdeState,
    content: &str,
    line: u32,
    col: u32,
    file_path: Option<&str>,
) -> Option<HoverInfo> {
    // Ensure project loaded
    if let Some(path) = file_path {
        state.ensure_project(path);
    }

    let mut session = IdeState::create_analysis_session();
    if let Some(ctx) = state.context_for(file_path) {
        session.set_context(ctx);
    }

    let id = session.set_source("input.rpl", content);
    let file = SourceFile::new(id, "input.rpl".into(), content.to_string());

    let lc = LineCol::new(line, col);
    let byte_pos = file.pos_from_line_col(lc)?;

    // Try local hover first
    if let Some(hover) = session.hover(id, byte_pos) {
        let start = file.line_col(hover.range.start());
        let end = file.line_col(hover.range.end());
        return Some(HoverInfo {
            contents: hover.contents,
            start_line: start.line,
            start_col: start.col,
            end_line: end.line,
            end_col: end.col,
        });
    }

    // Try project entry hover
    let project_path = file_path.and_then(find_project_root)?;
    let loaded = state.projects.get(&project_path)?;
    let (word, span) = word_at(content, byte_pos.offset() as usize)?;
    let entry = loaded.index.get(word)?;

    let contents = format_project_entry_hover(word, entry);
    let start = file.line_col(span.start());
    let end = file.line_col(span.end());
    Some(HoverInfo {
        contents,
        start_line: start.line,
        start_col: start.col,
        end_line: end.line,
        end_col: end.col,
    })
}

// ============================================================================
// Project Operations
// ============================================================================

/// Get project tree.
pub fn get_project_tree(state: &IdeState, path: &str) -> Vec<TreeNode> {
    let Some(loaded) = state.projects.get(path) else {
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
}

/// Get project diagnostics.
pub fn get_project_diagnostics(state: &IdeState, path: &str) -> Vec<Diagnostic> {
    let Some(loaded) = state.projects.get(path) else {
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
            analysis.diagnostics.iter().map(move |d| {
                let start = file.line_col(d.span.start());
                let end = file.line_col(d.span.end());
                Diagnostic {
                    start_line: start.line,
                    start_col: start.col,
                    end_line: end.line,
                    end_col: end.col,
                    severity: match d.severity {
                        ::rpl::analysis::Severity::Error => Severity::Error,
                        ::rpl::analysis::Severity::Warning => Severity::Warning,
                        ::rpl::analysis::Severity::Hint => Severity::Hint,
                    },
                    message: d.message.clone(),
                }
            })
        })
        .collect()
}

/// Get signature for a project entry.
pub fn get_signature(state: &IdeState, path: &str, key: &str) -> Option<String> {
    state
        .projects
        .get(path)?
        .index
        .get(key)?
        .signature
        .as_ref()
        .map(|s| s.to_string())
}

/// Run project entry point.
pub fn run_project(state: &mut IdeState, path: &str) -> EvalResult {
    let Some(loaded) = state.projects.get_mut(path) else {
        return EvalResult {
            stack: Vec::new(),
            error: Some(format!("Project not loaded: {}", path)),
        };
    };
    to_eval_result(loaded.project.run())
}

// ============================================================================
// REPL Operations
// ============================================================================

/// Evaluate REPL input.
pub fn repl_evaluate(state: &mut IdeState, code: &str) -> EvalResult {
    match state.repl.eval_repl(code) {
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
    }
}

/// Get current REPL stack.
pub fn repl_stack(state: &IdeState) -> Vec<StackValue> {
    state
        .repl
        .vm()
        .stack_contents()
        .iter()
        .map(to_stack_value)
        .collect()
}

/// Reset REPL.
pub fn repl_reset(state: &mut IdeState) {
    state.repl.reset_vm();
}

// ============================================================================
// Helpers
// ============================================================================

/// Walk up from a file path to find project.toml.
pub fn find_project_root(file_path: &str) -> Option<String> {
    let mut dir = Path::new(file_path).parent()?;
    loop {
        if dir.join("project.toml").exists() {
            return Some(dir.to_string_lossy().into_owned());
        }
        dir = dir.parent()?;
    }
}

/// Extract word at byte offset.
pub fn word_at(source: &str, offset: usize) -> Option<(&str, Span)> {
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
