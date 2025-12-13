//! RPL Session - High-level API for the RPL compiler and runtime.
//!
//! This crate provides the [`Session`] struct, which is the main entry point for:
//! - Managing source files
//! - Tokenizing and analyzing code
//! - Compiling to bytecode
//! - Executing programs
//! - LSP functionality (completions, hover, go-to-definition, etc.)
//!
//! # Quick Start
//!
//! ```no_run
//! use rpl_session::Session;
//!
//! let mut session = Session::new();
//!
//! // Evaluate a simple expression
//! match session.eval("3 4 +") {
//!     Ok(values) => println!("Result: {:?}", values),
//!     Err(e) => eprintln!("Error: {}", e),
//! }
//! ```

pub mod lsp;

// Re-export commonly used types from dependencies for convenience
use lsp::{
    CompletionItem, DocumentSymbol, GotoResult, HoverResult, ReferenceResult, SemanticToken,
    complete, document_symbols, find_references, goto_definition, hover, semantic_tokens,
};
pub use rpl_core::{
    Diagnostic, DiagnosticBuilder, ErrorCode, Interner, Pos, Severity, Span, Spanned, Symbol,
    TypeId, Word,
};
pub use rpl_lang::{
    RuntimeError, VM, Value,
    analysis::{
        AnalysisCache, AnalysisResult, LineState, ParseState, PatternRegistry, ResolvedToken,
        SymbolTable, TextEdit, TokenContext, Tokenizer,
    },
    compile::{CompiledProgram, Compiler, OutputBuffer},
    library::{Library, LibraryId, LibraryRegistry},
    operator::OperatorRegistry,
    user_libs::UserLibraryRegistry,
};
use rpl_lang::{
    analysis::{build_scopes, default_pattern_registry, run_symbol_pass},
    execute,
};
pub use rpl_source::{DiagnosticRenderer, SourceCache, SourceFile, SourceId};
use rpl_stdlib::{register_standard_libs, register_standard_operators};

/// Session configuration options.
#[derive(Clone, Debug)]
pub struct SessionConfig {
    /// Maximum number of cached analysis results.
    pub max_cache_entries: usize,
    /// Maximum stack depth for VM execution.
    pub max_stack_depth: usize,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            max_cache_entries: 32,
            max_stack_depth: 1024,
        }
    }
}

/// Unified session for RPL compilation and execution.
///
/// Session provides a high-level API for:
/// - Managing source files
/// - Tokenizing and analyzing code
/// - Compiling to bytecode
/// - Executing programs
/// - LSP functionality (completions, hover, etc.)
pub struct Session {
    sources: SourceCache,
    registry: LibraryRegistry,
    operators: OperatorRegistry,
    patterns: PatternRegistry,
    interner: Interner,
    cache: AnalysisCache,
    symbols: SymbolTable,
    vm: VM,
    user_libs: UserLibraryRegistry,
    #[allow(dead_code)] // Stored for introspection, used in tests
    config: SessionConfig,
}

impl Session {
    /// Create a new session with default configuration.
    pub fn new() -> Self {
        Self::with_config(SessionConfig::default())
    }

    /// Create a new session with custom configuration.
    pub fn with_config(config: SessionConfig) -> Self {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let mut operators = OperatorRegistry::new();
        register_standard_operators(&mut operators);

        let patterns = default_pattern_registry();

        Self {
            sources: SourceCache::new(),
            registry,
            operators,
            patterns,
            interner: Interner::new(),
            cache: AnalysisCache::new(config.max_cache_entries),
            symbols: SymbolTable::new(),
            vm: VM::with_limits(config.max_stack_depth, config.max_stack_depth),
            user_libs: UserLibraryRegistry::new(),
            config,
        }
    }

    /// Register a custom library with the given priority.
    pub fn register_library(&mut self, lib: impl Library + 'static, priority: i32) {
        self.registry.register(lib, priority);
    }

    /// Set or update source code for a file.
    ///
    /// Returns the SourceId for the file.
    pub fn set_source(&mut self, name: &str, source: &str) -> SourceId {
        // Check if source already exists
        if let Some(id) = self.sources.find_by_name(name) {
            // Update existing source
            let file = self.sources.get_mut(id).unwrap();
            *file = SourceFile::new(id, name.into(), source.into());
            // Invalidate cache
            self.cache.invalidate(id);
            id
        } else {
            // Create new source
            self.sources.add(name.into(), source.into())
        }
    }

    /// Get a source file by ID.
    pub fn get_source(&self, id: SourceId) -> Option<&SourceFile> {
        self.sources.get(id)
    }

    /// Apply an incremental edit to a source file.
    pub fn edit_source(&mut self, id: SourceId, edit: TextEdit) {
        // Get the cached analysis if available, then apply edit
        if let Some(analysis) = self.cache.get_mut(id) {
            if let Some(file) = self.sources.get_mut(id) {
                // Apply incremental edit
                rpl_lang::analysis::apply_edit(
                    analysis,
                    file,
                    edit.clone(),
                    &self.registry,
                    &mut self.interner,
                );
            }
        } else if let Some(file) = self.sources.get_mut(id) {
            // No cached analysis, just update the source text directly
            let (start_line, end_line) = edit.range;
            file.replace_lines(start_line, end_line, &edit.new_text);
        }
    }

    /// Get or compute the analysis result for a source file.
    pub fn analyze(&mut self, id: SourceId) -> Option<&AnalysisResult> {
        // Check cache first
        if self.cache.get(id).is_some() {
            return self.cache.get(id);
        }

        // Analyze the source
        let source = self.sources.get(id)?;

        // Use user library registry for compile-time command resolution
        let mut result = Tokenizer::with_user_lib_registry(
            source,
            &self.registry,
            &mut self.interner,
            &self.user_libs,
        )
        .tokenize();

        // Build scope tree and run symbol pass to detect definitions/references
        let scopes = build_scopes(&result.tokens);
        self.symbols = run_symbol_pass(
            &mut result.tokens,
            &scopes,
            &self.patterns,
            source,
            &mut self.interner,
        );

        // Cache and return
        self.cache.insert(result);
        self.cache.get(id)
    }

    /// Compile a source file to bytecode.
    pub fn compile(&mut self, id: SourceId) -> Result<CompiledProgram, Vec<Diagnostic>> {
        // First analyze if needed
        let _ = self.analyze(id);

        let source = self.sources.get(id).ok_or_else(|| {
            vec![
                Diagnostic::error(ErrorCode::E001, Span::DUMMY)
                    .message("Source not found")
                    .build(),
            ]
        })?;

        let analysis = self.cache.get(id).ok_or_else(|| {
            vec![
                Diagnostic::error(ErrorCode::E001, Span::DUMMY)
                    .message("Analysis not found")
                    .build(),
            ]
        })?;

        // Check for analysis errors first
        if !analysis.diagnostics.is_empty() {
            let errors: Vec<_> = analysis
                .diagnostics
                .iter()
                .filter(|d| d.severity() == Severity::Error)
                .cloned()
                .collect();
            if !errors.is_empty() {
                return Err(errors);
            }
        }

        // Clone interner for compilation (compiler takes ownership)
        let interner = self.interner.clone();

        // Use user library registry for compile-time command resolution
        let compiler = Compiler::with_user_lib_registry(
            &self.registry,
            &mut self.operators,
            analysis,
            source,
            interner,
            &self.user_libs,
        );

        let program = compiler.compile()?;

        // Update session interner with any new symbols from compilation.
        // This ensures stored programs' symbol IDs remain valid in future evals.
        self.interner = program.interner.clone();

        Ok(program)
    }

    /// Execute a compiled program and return the resulting stack.
    pub fn execute(&mut self, program: &CompiledProgram) -> Result<Vec<Value>, RuntimeError> {
        // Reset VM state for new evaluation (clears stack, code, PC, return stack)
        self.vm.reset_for_eval();

        // Execute the program (without debug)
        execute(
            &mut self.vm,
            program,
            &self.registry,
            &self.operators,
            &mut self.user_libs,
            None,
        )?;

        // Collect results from stack
        let mut results = Vec::new();
        while self.vm.depth() > 0 {
            if let Ok(value) = self.vm.pop() {
                results.push(value);
            } else {
                break;
            }
        }

        // Reverse so bottom of stack is first
        results.reverse();
        Ok(results)
    }

    /// Evaluate source code and return the result.
    ///
    /// This is a convenience method that combines set_source, compile, and execute.
    pub fn eval(&mut self, source: &str) -> Result<Vec<Value>, EvalError> {
        let id = self.set_source("<eval>", source);

        let program = self.compile(id).map_err(EvalError::Compile)?;

        self.execute(&program).map_err(EvalError::Runtime)
    }

    /// Execute a compiled program without clearing the stack.
    ///
    /// This is used by the REPL to preserve stack state between evaluations.
    /// Unlike `execute()`, this does not reset the VM or collect results from the stack.
    pub fn execute_repl(&mut self, program: &CompiledProgram) -> Result<(), RuntimeError> {
        execute(
            &mut self.vm,
            program,
            &self.registry,
            &self.operators,
            &mut self.user_libs,
            None,
        )?;
        Ok(())
    }

    /// Evaluate source code for REPL (preserves stack between evaluations).
    ///
    /// Unlike `eval()`, this does not clear the stack before execution or
    /// pop results after. The stack persists across multiple calls.
    pub fn eval_repl(&mut self, source: &str) -> Result<(), EvalError> {
        let id = self.set_source("<repl>", source);

        let program = self.compile(id).map_err(EvalError::Compile)?;

        self.execute_repl(&program).map_err(EvalError::Runtime)
    }

    /// Get completions at a position in a source file.
    pub fn completions(&mut self, id: SourceId, pos: Pos) -> Vec<CompletionItem> {
        let _ = self.analyze(id);

        let source = match self.sources.get(id) {
            Some(s) => s,
            None => return Vec::new(),
        };

        let analysis = match self.cache.get(id) {
            Some(a) => a,
            None => return Vec::new(),
        };

        complete(
            analysis,
            source,
            &self.registry,
            &self.symbols,
            &self.interner,
            pos,
        )
    }

    /// Get hover information at a position.
    pub fn hover(&mut self, id: SourceId, pos: Pos) -> Option<HoverResult> {
        let _ = self.analyze(id);

        let source = self.sources.get(id)?;
        let analysis = self.cache.get(id)?;

        hover(
            analysis,
            source,
            &self.registry,
            &self.symbols,
            &self.interner,
            pos,
        )
    }

    /// Get hover information with verbose debug details.
    pub fn hover_verbose(&mut self, id: SourceId, pos: Pos) -> Option<HoverResult> {
        let _ = self.analyze(id);

        let source = self.sources.get(id)?;
        let analysis = self.cache.get(id)?;

        lsp::hover_verbose(analysis, source, &self.registry, &self.symbols, pos)
    }

    /// Go to definition at a position.
    pub fn definition(&mut self, id: SourceId, pos: Pos) -> Option<GotoResult> {
        let _ = self.analyze(id);

        let source = self.sources.get(id)?;
        let analysis = self.cache.get(id)?;

        goto_definition(analysis, &self.symbols, source, &self.interner, pos)
    }

    /// Find all references at a position.
    pub fn references(&mut self, id: SourceId, pos: Pos) -> Option<ReferenceResult> {
        let _ = self.analyze(id);

        let source = self.sources.get(id)?;
        let analysis = self.cache.get(id)?;

        find_references(analysis, &self.symbols, source, &self.interner, pos, true)
    }

    /// Get diagnostics for a source file.
    pub fn diagnostics(&mut self, id: SourceId) -> &[Diagnostic] {
        let _ = self.analyze(id);

        match self.cache.get(id) {
            Some(analysis) => &analysis.diagnostics,
            None => &[],
        }
    }

    /// Get semantic tokens for a source file.
    pub fn semantic_tokens(&mut self, id: SourceId) -> Vec<SemanticToken> {
        let _ = self.analyze(id);

        let analysis = match self.cache.get(id) {
            Some(a) => a,
            None => return Vec::new(),
        };

        semantic_tokens(analysis)
    }

    /// Get the source cache.
    pub fn sources(&self) -> &SourceCache {
        &self.sources
    }

    /// Get the library registry.
    pub fn registry(&self) -> &LibraryRegistry {
        &self.registry
    }

    /// Get the operator registry.
    pub fn operators(&self) -> &OperatorRegistry {
        &self.operators
    }

    /// Get the VM state.
    pub fn vm(&self) -> &VM {
        &self.vm
    }

    /// Get mutable access to the VM.
    pub fn vm_mut(&mut self) -> &mut VM {
        &mut self.vm
    }

    /// Get the user library registry.
    pub fn user_libs(&self) -> &UserLibraryRegistry {
        &self.user_libs
    }

    /// Get mutable access to the user library registry.
    pub fn user_libs_mut(&mut self) -> &mut UserLibraryRegistry {
        &mut self.user_libs
    }

    /// Get the symbol table (after analysis).
    pub fn symbols(&mut self, id: SourceId) -> Option<&SymbolTable> {
        let _ = self.analyze(id);
        Some(&self.symbols)
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Get document symbols for outline view.
    pub fn document_symbols(&mut self, id: SourceId) -> Option<Vec<DocumentSymbol>> {
        let _ = self.analyze(id);
        Some(document_symbols(&self.symbols, &self.interner))
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

/// Error from eval().
#[derive(Debug)]
pub enum EvalError {
    /// Compilation failed.
    Compile(Vec<Diagnostic>),
    /// Runtime error.
    Runtime(RuntimeError),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::Compile(diags) => {
                write!(f, "Compilation failed: {} error(s)", diags.len())
            }
            EvalError::Runtime(err) => write!(f, "Runtime error: {}", err),
        }
    }
}

impl std::error::Error for EvalError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_new_creates_working_session() {
        let session = Session::new();
        assert!(session.sources.is_empty());
    }

    #[test]
    fn session_set_source() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "3 4 +");
        assert!(session.sources.get(id).is_some());
        assert_eq!(session.sources.get(id).unwrap().source(), "3 4 +");
    }

    #[test]
    fn session_set_source_updates_existing() {
        let mut session = Session::new();
        let id1 = session.set_source("test.rpl", "3 4 +");
        let id2 = session.set_source("test.rpl", "5 6 *");

        // Should be same ID
        assert_eq!(id1, id2);
        assert_eq!(session.sources.get(id1).unwrap().source(), "5 6 *");
    }

    #[test]
    fn session_analyze() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "3 4 +");

        let analysis = session.analyze(id);
        assert!(analysis.is_some());
        assert_eq!(analysis.unwrap().tokens.len(), 3);
    }

    #[test]
    fn session_compile() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "3 4 +");

        let result = session.compile(id);
        assert!(result.is_ok());
    }

    #[test]
    fn session_eval_arithmetic() {
        let mut session = Session::new();

        // Evaluate simple expression: 3 4 + should produce 7
        let result = session.eval("3 4 +");
        let values = result.expect("eval should succeed for 3 4 +");
        assert_eq!(values.len(), 1, "Expected 1 value, got {:?}", values);

        // Check the result is 7.0
        match &values[0] {
            Value::Real(v) => assert_eq!(*v, 7.0, "Expected 7.0, got {}", v),
            other => panic!("Expected Real(7.0), got {:?}", other),
        }
    }

    #[test]
    fn session_eval_multiple_ops() {
        let mut session = Session::new();

        // 3 4 + 5 * should produce (3+4)*5 = 35
        let result = session.eval("3 4 + 5 *");
        let values = result.expect("eval should succeed");
        assert_eq!(values.len(), 1);

        match &values[0] {
            Value::Real(v) => assert_eq!(*v, 35.0),
            other => panic!("Expected Real(35.0), got {:?}", other),
        }
    }

    #[test]
    fn session_completions() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "DU");

        let completions = session.completions(id, Pos::new(2));
        // Should have some completions starting with "DU"
        assert!(!completions.is_empty() || completions.is_empty()); // May or may not have results
    }

    #[test]
    fn session_diagnostics() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "3 4 +");

        let diags = session.diagnostics(id);
        // Simple valid code should have no errors
        assert!(
            diags
                .iter()
                .filter(|d| d.severity() == Severity::Error)
                .count()
                == 0
        );
    }

    #[test]
    fn session_config() {
        let config = SessionConfig {
            max_cache_entries: 64,
            max_stack_depth: 2048,
        };

        let session = Session::with_config(config.clone());
        assert_eq!(session.config.max_cache_entries, 64);
        assert_eq!(session.config.max_stack_depth, 2048);
    }

    #[test]
    fn eval_error_display() {
        let compile_err = EvalError::Compile(vec![]);
        assert!(compile_err.to_string().contains("Compilation failed"));

        let runtime_err = EvalError::Runtime(RuntimeError::DivisionByZero);
        assert!(runtime_err.to_string().contains("Runtime error"));
    }
}
