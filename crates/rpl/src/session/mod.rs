//! High-level session API for RPL2.
//!
//! This module provides the [`Session`] struct, which is the main entry point for:
//! - Managing source files
//! - Parsing and analyzing code
//! - Compiling to bytecode
//! - Executing programs
//! - LSP functionality (completions, hover, go-to-definition, etc.)
//!
//! # Quick Start
//!
//! ```
//! use rpl::Session;
//!
//! let mut session = Session::new();
//!
//! // Evaluate a simple expression
//! match session.eval("3 4 +") {
//!     Ok(values) => println!("Result: {:?}", values),
//!     Err(e) => eprintln!("Error: {}", e),
//! }
//! ```

pub mod debug;
pub mod lsp;

use std::collections::HashMap;

use crate::core::Interner;
use crate::source::{SourceCache, SourceFile, SourceId};

use crate::analysis::{
    AnalysisResult, Diagnostic, IncrementalAnalysis, Severity, SpanEdit,
};
use crate::lower::lower;
use crate::parse::parse;
use crate::registry::Registry;
use crate::value::Value;
use crate::vm::Vm;

/// Session configuration options.
#[derive(Clone, Debug)]
pub struct SessionConfig {
    /// Maximum stack depth for VM execution.
    pub max_stack_depth: usize,
}

impl Default for SessionConfig {
    fn default() -> Self {
        Self {
            max_stack_depth: 1024,
        }
    }
}

/// Error from eval().
#[derive(Debug)]
pub enum EvalError {
    /// Parsing failed.
    Parse(String),
    /// Lowering failed.
    Lower(String),
    /// Runtime error.
    Runtime(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::Parse(e) => write!(f, "Parse error: {}", e),
            EvalError::Lower(e) => write!(f, "Lower error: {}", e),
            EvalError::Runtime(e) => write!(f, "Runtime error: {}", e),
        }
    }
}

impl std::error::Error for EvalError {}

/// Unified session for RPL2 compilation and execution.
///
/// Session provides a high-level API for:
/// - Managing source files
/// - Parsing and analyzing code
/// - Compiling to bytecode
/// - Executing programs
/// - LSP functionality (completions, hover, etc.)
pub struct Session {
    sources: SourceCache,
    registry: Registry,
    interner: Interner,
    /// Incremental analysis state per source file.
    analysis_cache: HashMap<SourceId, IncrementalAnalysis>,
    vm: Vm,
    #[allow(dead_code)]
    config: SessionConfig,
}

impl Session {
    /// Create a new session with default configuration.
    pub fn new() -> Self {
        Self::with_config(SessionConfig::default())
    }

    /// Create a new session with custom configuration.
    ///
    /// The registry starts empty. To use standard library commands, register
    /// them using `rpl_stdlib`:
    ///
    /// ```ignore
    /// use rpl::Session;
    ///
    /// let mut session = Session::new();
    /// rpl_stdlib::register_interfaces(session.registry_mut());
    /// rpl_stdlib::register_impls(session.registry_mut());
    /// ```
    pub fn with_config(config: SessionConfig) -> Self {
        Self {
            sources: SourceCache::new(),
            registry: Registry::new(),
            interner: Interner::new(),
            analysis_cache: HashMap::new(),
            vm: Vm::new(),
            config,
        }
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
            self.analysis_cache.remove(&id);
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
    pub fn edit_source(&mut self, id: SourceId, edit: SpanEdit) {
        if let Some(analysis) = self.analysis_cache.get_mut(&id) {
            analysis.apply_edit(edit, &self.registry, &mut self.interner);
            // Update the source file to match
            if let Some(file) = self.sources.get_mut(id) {
                let name = file.name().to_string();
                *file = SourceFile::new(id, name, analysis.source().into());
            }
        } else if let Some(file) = self.sources.get_mut(id) {
            // No cached analysis, create one and apply edit
            let mut analysis = IncrementalAnalysis::new(
                file.source(),
                &self.registry,
                &mut self.interner,
            );
            analysis.apply_edit(edit, &self.registry, &mut self.interner);
            let name = file.name().to_string();
            *file = SourceFile::new(id, name, analysis.source().into());
            self.analysis_cache.insert(id, analysis);
        }
    }

    /// Get or compute the analysis result for a source file.
    pub fn analyze(&mut self, id: SourceId) -> Option<&AnalysisResult> {
        // Ensure we have an analysis
        if !self.analysis_cache.contains_key(&id) {
            let source = self.sources.get(id)?;
            let analysis = IncrementalAnalysis::new(
                source.source(),
                &self.registry,
                &mut self.interner,
            );
            self.analysis_cache.insert(id, analysis);
        }

        self.analysis_cache.get(&id).map(|a| a.result())
    }

    /// Evaluate source code and return the result.
    ///
    /// This is a convenience method that parses, lowers, and executes.
    pub fn eval(&mut self, source: &str) -> Result<Vec<Value>, EvalError> {
        // Parse
        let nodes = parse(source, &self.registry, &mut self.interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        // Lower to bytecode
        let program = lower(&nodes, &self.registry, &self.interner)
            .map_err(|e| EvalError::Lower(e.message))?;

        // Execute
        self.vm.reset();
        self.vm
            .execute(&program.code, &self.registry, &program.rodata)
            .map_err(|e| EvalError::Runtime(e.to_string()))?;

        // Collect results
        Ok(self.vm.stack_contents().to_vec())
    }

    /// Evaluate source code for REPL (preserves stack between evaluations).
    ///
    /// Unlike `eval()`, this does not clear the stack before execution.
    pub fn eval_repl(&mut self, source: &str) -> Result<(), EvalError> {
        // Parse
        let nodes = parse(source, &self.registry, &mut self.interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        // Lower to bytecode
        let program = lower(&nodes, &self.registry, &self.interner)
            .map_err(|e| EvalError::Lower(e.message))?;

        // Execute without reset
        self.vm
            .execute(&program.code, &self.registry, &program.rodata)
            .map_err(|e| EvalError::Runtime(e.to_string()))?;

        Ok(())
    }

    /// Get diagnostics for a source file.
    pub fn diagnostics(&mut self, id: SourceId) -> Vec<&Diagnostic> {
        match self.analyze(id) {
            Some(result) => result.diagnostics.iter().collect(),
            None => Vec::new(),
        }
    }

    /// Check if a source file has errors.
    pub fn has_errors(&mut self, id: SourceId) -> bool {
        self.diagnostics(id)
            .iter()
            .any(|d| d.severity == Severity::Error)
    }

    /// Get the source cache.
    pub fn sources(&self) -> &SourceCache {
        &self.sources
    }

    /// Get the registry.
    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    /// Get the VM state.
    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    /// Get mutable access to the VM.
    pub fn vm_mut(&mut self) -> &mut Vm {
        &mut self.vm
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Get mutable access to the interner.
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    /// Get mutable access to the registry.
    ///
    /// Use this to register external libraries.
    pub fn registry_mut(&mut self) -> &mut Registry {
        &mut self.registry
    }

    // === LSP Methods ===

    /// Get completions at a position in a source file.
    pub fn completions(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Vec<lsp::CompletionItem> {
        let _ = self.analyze(id);

        let source = match self.sources.get(id) {
            Some(s) => s,
            None => return Vec::new(),
        };

        let analysis = match self.analysis_cache.get(&id) {
            Some(a) => a.result(),
            None => return Vec::new(),
        };

        lsp::complete(analysis, source, &self.registry, &self.interner, pos)
    }

    /// Get hover information at a position.
    pub fn hover(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Option<lsp::HoverResult> {
        let _ = self.analyze(id);

        let analysis = self.analysis_cache.get(&id)?.result();

        lsp::hover(analysis, &self.registry, &self.interner, pos)
    }

    /// Go to definition at a position.
    pub fn definition(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Option<lsp::GotoResult> {
        let _ = self.analyze(id);

        let analysis = self.analysis_cache.get(&id)?.result();

        lsp::goto_definition(analysis, &self.interner, pos)
    }

    /// Find all references at a position.
    pub fn references(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
        include_definition: bool,
    ) -> Option<lsp::ReferenceResult> {
        let _ = self.analyze(id);

        let analysis = self.analysis_cache.get(&id)?.result();

        lsp::find_references(analysis, &self.interner, pos, include_definition)
    }

    /// Get semantic tokens for a source file.
    pub fn semantic_tokens(&mut self, id: SourceId) -> Vec<lsp::SemanticToken> {
        let _ = self.analyze(id);

        let analysis = match self.analysis_cache.get(&id) {
            Some(a) => a,
            None => return Vec::new(),
        };

        lsp::semantic_tokens(analysis)
    }

    /// Get document symbols for outline view.
    pub fn document_symbols(&mut self, id: SourceId) -> Vec<lsp::DocumentSymbol> {
        let _ = self.analyze(id);

        let analysis = match self.analysis_cache.get(&id) {
            Some(a) => a.result(),
            None => return Vec::new(),
        };

        lsp::document_symbols(analysis, &self.interner)
    }

    // === Debug Methods ===

    /// Compile source code to a CompiledProgram (with debug info).
    ///
    /// This returns a CompiledProgram with source span mappings
    /// that can be used with execute_debug().
    pub fn compile(&mut self, source: &str) -> Result<crate::lower::CompiledProgram, EvalError> {
        let nodes = parse(source, &self.registry, &mut self.interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        lower(&nodes, &self.registry, &self.interner)
            .map_err(|e| EvalError::Lower(e.message))
    }

    /// Execute a compiled program with debugging support.
    ///
    /// Unlike eval(), this method:
    /// - Takes a pre-compiled program
    /// - Uses a DebugState for breakpoint and stepping control
    /// - Returns ExecuteOutcome to indicate if execution completed or paused
    ///
    /// To resume after a debug pause, call this method again with the same
    /// program and debug state.
    pub fn execute_debug(
        &mut self,
        program: &crate::lower::CompiledProgram,
        debug: &mut crate::vm::DebugState,
    ) -> Result<crate::vm::ExecuteOutcome, EvalError> {
        self.vm
            .execute_debug(program, &self.registry, debug)
            .map_err(|e| EvalError::Runtime(e.to_string()))
    }

    /// Reset the VM state for a new debug session.
    pub fn reset_vm(&mut self) {
        self.vm.reset();
    }

    /// Get the current call depth (for step-over/step-out).
    pub fn call_depth(&self) -> usize {
        self.vm.call_depth()
    }

    /// Get the current PC (program counter) for debugging.
    pub fn current_pc(&self) -> usize {
        self.vm.pc
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_new() {
        let session = Session::new();
        assert!(session.sources.is_empty());
    }

    #[test]
    fn session_set_source() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "1 2 3");

        let source = session.get_source(id).unwrap();
        assert_eq!(source.source(), "1 2 3");
    }

    #[test]
    fn session_set_source_updates_existing() {
        let mut session = Session::new();
        let id1 = session.set_source("test.rpl", "1 2 3");
        let id2 = session.set_source("test.rpl", "4 5 6");

        assert_eq!(id1, id2);
        assert_eq!(session.get_source(id1).unwrap().source(), "4 5 6");
    }

    #[test]
    fn session_eval_literals() {
        let mut session = Session::new();

        let result = session.eval("3 4 5").unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], Value::integer(3));
        assert_eq!(result[1], Value::integer(4));
        assert_eq!(result[2], Value::integer(5));
    }

    #[test]
    fn session_eval_repl_preserves_stack() {
        let mut session = Session::new();

        session.eval_repl("1 2").unwrap();
        session.eval_repl("3 4").unwrap();

        // Stack should have 1, 2, 3, 4
        let stack = session.vm().stack_contents();
        assert_eq!(stack.len(), 4);
    }

    #[test]
    fn session_analyze_literals() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "42 \"hello\"");

        let analysis = session.analyze(id).unwrap();
        // Literals don't create symbol definitions
        assert_eq!(analysis.symbols.definition_count(), 0);
    }

    #[test]
    fn session_diagnostics_empty_for_literals() {
        let mut session = Session::new();
        let id = session.set_source("test.rpl", "1 2 3");

        let diags = session.diagnostics(id);
        let errors: Vec<_> = diags.iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(errors.is_empty());
    }

    // Note: Tests for IFERR, DOERR, ERRN, ERRM and other stdlib commands
    // are in the integration test suite (tests/pipeline/) which uses rpl-stdlib.
}
