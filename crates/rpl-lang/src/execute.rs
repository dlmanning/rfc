//! High-level execution wrappers.
//!
//! This module provides convenient functions for executing compiled programs
//! using the library dispatcher.

use rpl_vm::{DebugState, ExecuteOutcome, RuntimeError, VM};

use crate::compile::CompiledProgram;
use crate::dispatch_impl::LibraryDispatcher;
use crate::library::LibraryRegistry;
use crate::operator::OperatorRegistry;
use crate::user_libs::UserLibraryRegistry;

/// Execute a compiled program.
///
/// If `debug` is `Some`, execution may pause at breakpoints or after stepping.
/// The caller should inspect VM state and call `execute` again to resume.
pub fn execute(
    vm: &mut VM,
    program: &CompiledProgram,
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    user_libs: &mut UserLibraryRegistry,
    debug: Option<&mut DebugState>,
) -> Result<ExecuteOutcome, RuntimeError> {
    execute_with_source(vm, program, registry, operators, user_libs, debug, None, None)
}

/// Execute a compiled program with source information for debugging.
///
/// If `source` and `source_name` are provided, debug info will be extracted
/// when program prologs are executed, enabling breakpoints inside stored functions.
#[allow(clippy::too_many_arguments)]
pub fn execute_with_source(
    vm: &mut VM,
    program: &CompiledProgram,
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    user_libs: &mut UserLibraryRegistry,
    debug: Option<&mut DebugState>,
    source: Option<&str>,
    source_name: Option<&str>,
) -> Result<ExecuteOutcome, RuntimeError> {
    // Create the dispatcher
    let dispatcher = LibraryDispatcher::new(registry, operators, user_libs);

    // Call the underlying rpl-vm execute
    rpl_vm::execute_with_source(
        vm,
        &program.code,
        &dispatcher,
        debug,
        program.interner.clone(),
        Some(&program.spans),
        source,
        source_name,
    )
}

/// Execute bytecode directly (without a CompiledProgram wrapper).
///
/// This is useful when you have raw bytecode and an interner but not a
/// full CompiledProgram structure.
pub fn execute_code(
    vm: &mut VM,
    code: &[rpl_core::Word],
    registry: &LibraryRegistry,
    operators: &OperatorRegistry,
    user_libs: &mut UserLibraryRegistry,
    debug: Option<&mut DebugState>,
    interner: rpl_core::Interner,
) -> Result<ExecuteOutcome, RuntimeError> {
    let dispatcher = LibraryDispatcher::new(registry, operators, user_libs);

    rpl_vm::execute(vm, code, &dispatcher, debug, interner)
}
