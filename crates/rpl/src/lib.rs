//! RPL: Two-pass RPL compiler with stack-based bytecode.
//!
//! This crate implements a clean two-pass compilation pipeline:
//! - **Parse**: Tokens → IR (intermediate representation)
//! - **Lower**: IR → Bytecode
//! - **Execute**: Bytecode runs on our VM
//!
//! # Architecture
//!
//! ```text
//! Tokens → IR → Bytecode → VM
//!          ↑        ↑        ↑
//!       Phase 1  Phase 2  Phase 3
//!       (parse)  (lower)  (execute)
//! ```
//!
//! # Key Design Decisions
//!
//! 1. **Three grammar primitives**: Program `« »`, List `{ }`, Symbolic `' '`
//! 2. **No complex literals**: Complex numbers via commands, not syntax
//! 3. **No closures**: Variables in nested programs use runtime lookup
//! 4. **Self-contained**: Own VM, Value type, and compilation infrastructure
//!
//! # Example
//!
//! ```
//! use rpl::{eval, value::Value};
//!
//! // Basic evaluation with literals (no stdlib needed)
//! let result = eval("1 2 3").unwrap();
//! assert_eq!(result, vec![Value::integer(1), Value::integer(2), Value::integer(3)]);
//!
//! // For arithmetic and other commands, use rpl_stdlib::eval()
//! ```

// Core types (formerly rpl-core)
pub mod core;
pub mod error;
pub mod token;

// Source file management (formerly rpl-source)
pub mod source;

// Compiler and VM
pub mod analysis;
pub mod ir;
pub mod libs;
pub mod lower;
pub mod parse;
pub mod registry;
pub mod serialize;
pub mod session;
pub mod symbolic;
pub mod types;
pub mod value;
pub mod vm;

// Interface declaration parser
pub mod interface;

// Re-export core types at crate root
pub use core::{Interner, Pos, Span, Spanned, Symbol, TypeId};
pub use error::{Diagnostic, DiagnosticBuilder, ErrorCode, Severity, Suggestion};
pub use token::{SemanticKind, TokenInfo, TokenType};

// Re-export source types at crate root
pub use source::{DiagnosticRenderer, LineCol, SourceCache, SourceFile, SourceId};

// Re-export commonly used types at crate root
pub use lower::CompiledProgram;
pub use session::{AnalysisSession, Runtime, Session, SessionConfig, EvalError};
pub use session::lsp;
pub use session::debug as debug_helpers;
pub use vm::{DebugState, DebugMode, DebugEvent, ExecuteOutcome, ReturnEntry};

/// Evaluate RPL source code and return the resulting stack.
///
/// This is a low-level function that uses empty registries. For evaluation
/// with standard library commands (+, -, IF, etc.), use `rpl_stdlib::eval()`.
///
/// This function can only handle basic literals (integers, reals, strings,
/// lists, programs) - no commands.
pub fn eval(source: &str) -> Result<Vec<value::Value>, String> {
    let interfaces = registry::InterfaceRegistry::new();
    let lowerers = registry::LowererRegistry::new();
    let executors = registry::ExecutorRegistry::new();
    eval_with_registries(source, &interfaces, &lowerers, &executors)
}

/// Evaluate RPL source code with custom registries.
///
/// Use this when you need to evaluate code with a specific set of libraries
/// registered.
pub fn eval_with_registries(
    source: &str,
    interfaces: &registry::InterfaceRegistry,
    lowerers: &registry::LowererRegistry,
    executors: &registry::ExecutorRegistry,
) -> Result<Vec<value::Value>, String> {
    let mut interner = Interner::new();

    // Parse
    let nodes = parse::parse(source, interfaces, &mut interner)
        .map_err(|e| format!("parse error: {}", e))?;

    // Lower
    let program = lower::lower(&nodes, interfaces, lowerers, &interner)
        .map_err(|e| format!("lower error: {}", e))?;

    // Execute
    let mut vm = vm::Vm::new();
    vm.execute(&program.code, executors, &program.rodata)
        .map_err(|e| format!("runtime error: {}", e))?;

    // Return stack contents
    Ok(vm.stack.as_slice().to_vec())
}

// Note: Tests requiring stdlib (arithmetic, flow control, etc.) are in rpl-stdlib.

#[cfg(test)]
mod tests {
    use super::*;
    use value::Value;

    #[test]
    fn eval_integers() {
        let result = eval("1 2 3").unwrap();
        assert_eq!(result, vec![
            Value::integer(1),
            Value::integer(2),
            Value::integer(3),
        ]);
    }

    #[test]
    fn eval_reals() {
        let result = eval("1.5 2.5").unwrap();
        assert_eq!(result, vec![
            Value::real(1.5),
            Value::real(2.5),
        ]);
    }

    #[test]
    fn eval_strings() {
        let result = eval(r#""hello" "world""#).unwrap();
        assert_eq!(result, vec![
            Value::string("hello"),
            Value::string("world"),
        ]);
    }

    #[test]
    fn eval_list() {
        let result = eval("{ 1 2 3 }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(1));
                assert_eq!(items[1], Value::integer(2));
                assert_eq!(items[2], Value::integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_empty_list() {
        let result = eval("{ }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected empty list"),
        }
    }

    #[test]
    fn eval_program() {
        // Program should create a program object on stack (bytecode will be empty without stdlib)
        let result = eval("<< 1 2 >>").unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].as_program().is_some());
    }
}
