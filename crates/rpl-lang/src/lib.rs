//! The RPL language engine.
//!
//! This crate provides the core language functionality:
//! - `library` - Library trait and registry for extensibility
//! - `analysis` - Tokenization, pattern detection, and symbol resolution
//! - `compile` - Bytecode compilation
//! - `operator` - Operator registry and dispatch
//! - `dispatch_impl` - Library dispatcher for VM command execution
//! - `execute` - High-level execution wrappers

pub mod analysis;
pub mod compile;
pub mod debug_helpers;
pub mod dispatch_impl;
pub mod execute;
pub mod library;
pub mod operator;
pub mod user_libs;
pub mod well_known;

// Re-export commonly used types at crate root
pub use analysis::{
    AnalysisCache, AnalysisResult, PatternDetector, PatternKind, PatternMatch, PatternRegistry,
    ResolvedToken, Scope, ScopeId, ScopeKind, ScopeTree, SymbolTable, TokenContext, Tokenizer,
    build_scopes, default_pattern_registry, resolve_references, run_symbol_pass,
};
pub use compile::{CompiledProgram, Compiler, Construct, OutputBuffer};
pub use library::{
    CompileContext, CompileResult, ConstructKind, ExecuteContext, ExecuteResult, Library,
    LibraryId, LibraryRegistry, ProbeContext, ProbeResult, StackEffect, TokenDoc,
};
pub use operator::{OperatorKind, OperatorRegistry, OpSignature, Resolution};

// Re-export VM types from rpl-vm for convenience
pub use rpl_vm::{
    CommandDispatch, DebugEvent, DebugMode, DebugState, DispatchResult, ExecuteOutcome,
    LocalFrame, ProgramDebugInfo, ReturnEntry, RuntimeError, StackError, VM, Value,
};

// Re-export debug helper functions
pub use debug_helpers::{find_pc_for_line, line_for_pc};

// Re-export dispatch constants and functions
pub use dispatch_impl::{
    DISPATCH_LIB, LibraryDispatcher, encode_dispatch, decode_dispatch_symbol,
    decode_dispatch, decode_operator_kind, operator_symbol,
};

// Re-export high-level execute functions
pub use execute::{execute, execute_with_source, execute_code};
