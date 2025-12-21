//! High-level session API for RPL2.
//!
//! This module provides three main types for different use cases:
//!
//! - [`AnalysisSession`]: For LSP/IDE - parsing and analysis only
//! - [`Runtime`]: For executing pre-compiled bytecode
//! - [`Session`]: Full pipeline for REPL/debugger (composes the above)
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
//!
//! # For LSP/IDE (analysis only)
//!
//! ```
//! use rpl::AnalysisSession;
//!
//! let mut session = AnalysisSession::new();
//! let id = session.set_source("test.rpl", "1 2 +");
//! let completions = session.completions(id, rpl::Pos::new(0));
//! ```
//!
//! # For Runtime (execution only)
//!
//! ```ignore
//! use rpl::Runtime;
//!
//! let mut runtime = Runtime::new();
//! // Register executors...
//! runtime.execute(&compiled_program)?;
//! ```

pub mod debug;
pub mod lsp;

use std::collections::HashMap;

use crate::core::Interner;
use crate::source::{SourceCache, SourceFile, SourceId};

use crate::analysis::{
    AnalysisResult, Diagnostic, IncrementalAnalysis, Severity, SpanEdit,
};
use crate::lower::{lower, CompiledProgram};
use crate::parse::parse;
use crate::registry::{InterfaceRegistry, LowererRegistry, ExecutorRegistry};
use crate::value::Value;
use crate::vm::{Vm, DebugState, ExecuteOutcome, VmError};

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

// ============================================================================
// Runtime - for executing pre-compiled bytecode
// ============================================================================

/// Runtime for executing pre-compiled RPL bytecode.
///
/// Use this when you have already-compiled bytecode and just need to execute it.
/// This is useful for:
/// - Running compiled `.rplc` binaries
/// - Embedding RPL execution without compilation overhead
/// - Scenarios where parsing/lowering is done separately
///
/// # Example
///
/// ```ignore
/// use rpl::Runtime;
///
/// let mut runtime = Runtime::new();
/// rpl_stdlib::register_executors(runtime.executors_mut());
///
/// // Load and execute pre-compiled program
/// let program = load_compiled_program("program.rplc")?;
/// runtime.execute(&program)?;
/// println!("Result: {:?}", runtime.stack_contents());
/// ```
pub struct Runtime {
    executors: ExecutorRegistry,
    vm: Vm,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    /// Create a new runtime with empty executor registry.
    pub fn new() -> Self {
        Self {
            executors: ExecutorRegistry::new(),
            vm: Vm::new(),
        }
    }

    /// Execute a compiled program.
    ///
    /// Resets the VM before execution. For REPL-style execution that
    /// preserves stack state, use `execute_continue()`.
    pub fn execute(&mut self, program: &CompiledProgram) -> Result<(), VmError> {
        self.vm.reset();
        self.vm.execute(&program.code, &self.executors, &program.rodata)
    }

    /// Execute a compiled program without resetting the VM.
    ///
    /// Preserves existing stack contents across executions.
    pub fn execute_continue(&mut self, program: &CompiledProgram) -> Result<(), VmError> {
        self.vm.execute(&program.code, &self.executors, &program.rodata)
    }

    /// Execute a compiled program with debugging support.
    ///
    /// Returns `ExecuteOutcome` to indicate if execution completed or paused
    /// at a breakpoint/step.
    pub fn execute_debug(
        &mut self,
        program: &CompiledProgram,
        debug: &mut DebugState,
    ) -> Result<ExecuteOutcome, VmError> {
        self.vm.execute_debug(program, &self.executors, debug)
    }

    /// Reset the VM state.
    pub fn reset(&mut self) {
        self.vm.reset();
    }

    /// Get the current stack contents.
    pub fn stack_contents(&self) -> &[Value] {
        self.vm.stack_contents()
    }

    /// Get the current call depth (for step-over/step-out).
    pub fn call_depth(&self) -> usize {
        self.vm.call_depth()
    }

    /// Get the current PC (program counter).
    pub fn current_pc(&self) -> usize {
        self.vm.pc
    }

    /// Get the VM state.
    pub fn vm(&self) -> &Vm {
        &self.vm
    }

    /// Get mutable access to the VM.
    pub fn vm_mut(&mut self) -> &mut Vm {
        &mut self.vm
    }

    /// Get the executor registry.
    pub fn executors(&self) -> &ExecutorRegistry {
        &self.executors
    }

    /// Get mutable access to the executor registry.
    ///
    /// Use this to register library executors.
    pub fn executors_mut(&mut self) -> &mut ExecutorRegistry {
        &mut self.executors
    }
}

// ============================================================================
// AnalysisSession - for LSP/IDE analysis
// ============================================================================

/// Session for parsing and analyzing RPL code without execution.
///
/// Use this for LSP servers, IDEs, and other tools that need to understand
/// code structure without running it. This provides:
/// - Source file management
/// - Parsing and analysis
/// - Completions, hover, go-to-definition, etc.
///
/// # Example
///
/// ```
/// use rpl::AnalysisSession;
///
/// let mut session = AnalysisSession::new();
/// // Register interfaces for command recognition
/// // rpl_stdlib::register_interfaces(session.interfaces_mut());
///
/// let id = session.set_source("test.rpl", "1 2 +");
/// let diags = session.diagnostics(id);
/// ```
pub struct AnalysisSession {
    sources: SourceCache,
    interfaces: InterfaceRegistry,
    interner: Interner,
    analysis_cache: HashMap<SourceId, IncrementalAnalysis>,
}

impl Default for AnalysisSession {
    fn default() -> Self {
        Self::new()
    }
}

impl AnalysisSession {
    /// Create a new analysis session with empty interface registry.
    pub fn new() -> Self {
        Self {
            sources: SourceCache::new(),
            interfaces: InterfaceRegistry::new(),
            interner: Interner::new(),
            analysis_cache: HashMap::new(),
        }
    }

    /// Set or update source code for a file.
    ///
    /// Returns the SourceId for the file.
    pub fn set_source(&mut self, name: &str, source: &str) -> SourceId {
        if let Some(id) = self.sources.find_by_name(name) {
            let file = self.sources.get_mut(id).unwrap();
            *file = SourceFile::new(id, name.into(), source.into());
            self.analysis_cache.remove(&id);
            id
        } else {
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
            analysis.apply_edit(edit, &self.interfaces, &mut self.interner);
            if let Some(file) = self.sources.get_mut(id) {
                let name = file.name().to_string();
                *file = SourceFile::new(id, name, analysis.source().into());
            }
        } else if let Some(file) = self.sources.get_mut(id) {
            let mut analysis = IncrementalAnalysis::new(
                file.source(),
                &self.interfaces,
                &mut self.interner,
            );
            analysis.apply_edit(edit, &self.interfaces, &mut self.interner);
            let name = file.name().to_string();
            *file = SourceFile::new(id, name, analysis.source().into());
            self.analysis_cache.insert(id, analysis);
        }
    }

    /// Get or compute the analysis result for a source file.
    pub fn analyze(&mut self, id: SourceId) -> Option<&AnalysisResult> {
        if !self.analysis_cache.contains_key(&id) {
            let source = self.sources.get(id)?;
            let analysis = IncrementalAnalysis::new(
                source.source(),
                &self.interfaces,
                &mut self.interner,
            );
            self.analysis_cache.insert(id, analysis);
        }
        self.analysis_cache.get(&id).map(|a| a.result())
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

    /// Get the interface registry.
    pub fn interfaces(&self) -> &InterfaceRegistry {
        &self.interfaces
    }

    /// Get mutable access to the interface registry.
    ///
    /// Use this to register library interfaces.
    pub fn interfaces_mut(&mut self) -> &mut InterfaceRegistry {
        &mut self.interfaces
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Get mutable access to the interner.
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    /// Get both interfaces and interner for parsing.
    ///
    /// This avoids borrow conflicts when you need both references.
    pub fn parsing_context(&mut self) -> (&InterfaceRegistry, &mut Interner) {
        (&self.interfaces, &mut self.interner)
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

        lsp::complete(analysis, source, &self.interfaces, &self.interner, pos)
    }

    /// Get hover information at a position.
    pub fn hover(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Option<lsp::HoverResult> {
        let _ = self.analyze(id);
        let analysis = self.analysis_cache.get(&id)?.result();
        lsp::hover(analysis, &self.interfaces, &self.interner, pos)
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
}

// ============================================================================
// Session - full pipeline for REPL/debugger
// ============================================================================

/// Unified session for RPL2 compilation and execution.
///
/// Session provides a high-level API for:
/// - Managing source files
/// - Parsing and analyzing code
/// - Compiling to bytecode
/// - Executing programs
/// - LSP functionality (completions, hover, etc.)
///
/// Session composes [`AnalysisSession`] and [`Runtime`] with a lowerer registry
/// to provide the complete compilation pipeline.
pub struct Session {
    /// Analysis session for parsing and analysis.
    analysis: AnalysisSession,
    /// Lowerer registry for compilation (bridges analysis → runtime).
    lowerers: LowererRegistry,
    /// Runtime for execution.
    runtime: Runtime,
}

impl Session {
    /// Create a new session with default configuration.
    pub fn new() -> Self {
        Self::with_config(SessionConfig::default())
    }

    /// Create a new session with custom configuration.
    ///
    /// The registries start empty. To use standard library commands, register
    /// them using `rpl_stdlib`:
    ///
    /// ```ignore
    /// use rpl::Session;
    ///
    /// let mut session = Session::new();
    /// rpl_stdlib::register_interfaces(session.interfaces_mut());
    /// rpl_stdlib::register_lowerers(session.lowerers_mut());
    /// rpl_stdlib::register_executors(session.executors_mut());
    /// ```
    pub fn with_config(_config: SessionConfig) -> Self {
        // TODO: Wire up config.max_stack_depth to the runtime
        Self {
            analysis: AnalysisSession::new(),
            lowerers: LowererRegistry::new(),
            runtime: Runtime::new(),
        }
    }

    // === Source Management (delegates to AnalysisSession) ===

    /// Set or update source code for a file.
    ///
    /// Returns the SourceId for the file.
    pub fn set_source(&mut self, name: &str, source: &str) -> SourceId {
        self.analysis.set_source(name, source)
    }

    /// Get a source file by ID.
    pub fn get_source(&self, id: SourceId) -> Option<&SourceFile> {
        self.analysis.get_source(id)
    }

    /// Apply an incremental edit to a source file.
    pub fn edit_source(&mut self, id: SourceId, edit: SpanEdit) {
        self.analysis.edit_source(id, edit)
    }

    /// Get or compute the analysis result for a source file.
    pub fn analyze(&mut self, id: SourceId) -> Option<&AnalysisResult> {
        self.analysis.analyze(id)
    }

    /// Get diagnostics for a source file.
    pub fn diagnostics(&mut self, id: SourceId) -> Vec<&Diagnostic> {
        self.analysis.diagnostics(id)
    }

    /// Check if a source file has errors.
    pub fn has_errors(&mut self, id: SourceId) -> bool {
        self.analysis.has_errors(id)
    }

    // === Compilation and Execution ===

    /// Evaluate source code and return the result.
    ///
    /// This is a convenience method that analyzes, parses, lowers, and executes.
    pub fn eval(&mut self, source: &str) -> Result<Vec<Value>, EvalError> {
        // Analyze
        let id = self.analysis.set_source("__eval__", source);
        let _ = self.analysis.analyze(id);

        // Parse
        let (interfaces, interner) = self.analysis.parsing_context();
        let nodes = parse(source, interfaces, interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        // Get analysis result for type-informed lowering
        let analysis = self.analysis.analysis_cache.get(&id)
            .expect("analysis should be cached after analyze()")
            .result();

        // Lower to bytecode
        let program = lower(&nodes, self.analysis.interfaces(), &self.lowerers, self.analysis.interner(), analysis)
            .map_err(|e| EvalError::Lower(e.message))?;

        // Execute
        self.runtime.execute(&program)
            .map_err(|e| EvalError::Runtime(e.to_string()))?;

        // Collect results
        Ok(self.runtime.stack_contents().to_vec())
    }

    /// Evaluate source code for REPL (preserves stack between evaluations).
    ///
    /// Unlike `eval()`, this does not clear the stack before execution.
    pub fn eval_repl(&mut self, source: &str) -> Result<(), EvalError> {
        // Analyze
        let id = self.analysis.set_source("__repl__", source);
        let _ = self.analysis.analyze(id);

        // Parse
        let (interfaces, interner) = self.analysis.parsing_context();
        let nodes = parse(source, interfaces, interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        // Get analysis result for type-informed lowering
        let analysis = self.analysis.analysis_cache.get(&id)
            .expect("analysis should be cached after analyze()")
            .result();

        // Lower to bytecode
        let program = lower(&nodes, self.analysis.interfaces(), &self.lowerers, self.analysis.interner(), analysis)
            .map_err(|e| EvalError::Lower(e.message))?;

        // Execute without reset
        self.runtime.execute_continue(&program)
            .map_err(|e| EvalError::Runtime(e.to_string()))?;

        Ok(())
    }

    /// Compile source code to a CompiledProgram (with debug info).
    ///
    /// This runs the analyzer first to infer types for local variables,
    /// then uses that information during lowering for better code generation.
    ///
    /// The returned CompiledProgram includes source span mappings
    /// that can be used with execute_debug().
    pub fn compile(&mut self, source: &str) -> Result<CompiledProgram, EvalError> {
        // Add source to analysis session and run analysis
        let id = self.analysis.set_source("__compile__", source);
        let _ = self.analysis.analyze(id); // Trigger analysis, drop the reference

        // Now parse (needs mutable borrow of interner)
        let (interfaces, interner) = self.analysis.parsing_context();
        let nodes = parse(source, interfaces, interner)
            .map_err(|e| EvalError::Parse(format!("{:?}", e)))?;

        // Get analysis result (now as immutable borrow alongside other immutable borrows)
        let analysis = self.analysis.analysis_cache.get(&id)
            .expect("analysis should be cached after analyze()")
            .result();

        // Lower with analysis results for type-informed code generation
        lower(&nodes, self.analysis.interfaces(), &self.lowerers, self.analysis.interner(), analysis)
            .map_err(|e| EvalError::Lower(e.message))
    }

    // === Registry Access ===

    /// Get the source cache.
    pub fn sources(&self) -> &SourceCache {
        self.analysis.sources()
    }

    /// Get the interface registry.
    pub fn interfaces(&self) -> &InterfaceRegistry {
        self.analysis.interfaces()
    }

    /// Get the lowerer registry.
    pub fn lowerers(&self) -> &LowererRegistry {
        &self.lowerers
    }

    /// Get the executor registry.
    pub fn executors(&self) -> &ExecutorRegistry {
        self.runtime.executors()
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        self.analysis.interner()
    }

    /// Get mutable access to the interner.
    pub fn interner_mut(&mut self) -> &mut Interner {
        self.analysis.interner_mut()
    }

    /// Get mutable access to the interface registry.
    ///
    /// Use this to register external library interfaces.
    pub fn interfaces_mut(&mut self) -> &mut InterfaceRegistry {
        self.analysis.interfaces_mut()
    }

    /// Get mutable access to the lowerer registry.
    ///
    /// Use this to register external library lowerers.
    pub fn lowerers_mut(&mut self) -> &mut LowererRegistry {
        &mut self.lowerers
    }

    /// Get mutable access to the executor registry.
    ///
    /// Use this to register external library executors.
    pub fn executors_mut(&mut self) -> &mut ExecutorRegistry {
        self.runtime.executors_mut()
    }

    // === VM Access (delegates to Runtime) ===

    /// Get the VM state.
    pub fn vm(&self) -> &Vm {
        self.runtime.vm()
    }

    /// Get mutable access to the VM.
    pub fn vm_mut(&mut self) -> &mut Vm {
        self.runtime.vm_mut()
    }

    /// Reset the VM state for a new debug session.
    pub fn reset_vm(&mut self) {
        self.runtime.reset();
    }

    /// Get the current call depth (for step-over/step-out).
    pub fn call_depth(&self) -> usize {
        self.runtime.call_depth()
    }

    /// Get the current PC (program counter) for debugging.
    pub fn current_pc(&self) -> usize {
        self.runtime.current_pc()
    }

    // === LSP Methods (delegates to AnalysisSession) ===

    /// Get completions at a position in a source file.
    pub fn completions(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Vec<lsp::CompletionItem> {
        self.analysis.completions(id, pos)
    }

    /// Get hover information at a position.
    pub fn hover(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Option<lsp::HoverResult> {
        self.analysis.hover(id, pos)
    }

    /// Go to definition at a position.
    pub fn definition(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
    ) -> Option<lsp::GotoResult> {
        self.analysis.definition(id, pos)
    }

    /// Find all references at a position.
    pub fn references(
        &mut self,
        id: SourceId,
        pos: crate::core::Pos,
        include_definition: bool,
    ) -> Option<lsp::ReferenceResult> {
        self.analysis.references(id, pos, include_definition)
    }

    /// Get semantic tokens for a source file.
    pub fn semantic_tokens(&mut self, id: SourceId) -> Vec<lsp::SemanticToken> {
        self.analysis.semantic_tokens(id)
    }

    /// Get document symbols for outline view.
    pub fn document_symbols(&mut self, id: SourceId) -> Vec<lsp::DocumentSymbol> {
        self.analysis.document_symbols(id)
    }

    // === Debug Methods ===

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
        program: &CompiledProgram,
        debug: &mut DebugState,
    ) -> Result<ExecuteOutcome, EvalError> {
        self.runtime.execute_debug(program, debug)
            .map_err(|e| EvalError::Runtime(e.to_string()))
    }

    // === Component Access ===

    /// Get the analysis session.
    pub fn analysis(&self) -> &AnalysisSession {
        &self.analysis
    }

    /// Get mutable access to the analysis session.
    pub fn analysis_mut(&mut self) -> &mut AnalysisSession {
        &mut self.analysis
    }

    /// Get the runtime.
    pub fn runtime(&self) -> &Runtime {
        &self.runtime
    }

    /// Get mutable access to the runtime.
    pub fn runtime_mut(&mut self) -> &mut Runtime {
        &mut self.runtime
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
        assert!(session.sources().is_empty());
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
