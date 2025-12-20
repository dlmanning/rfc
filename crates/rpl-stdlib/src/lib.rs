//! RPL Standard Library
//!
//! This crate provides the standard library implementations for RPL:
//! - Arithmetic operations (+, -, *, /, comparisons)
//! - Stack manipulation (DUP, DROP, SWAP, etc.)
//! - Control flow (IF/THEN/ELSE, FOR/NEXT, etc.)
//! - String operations
//! - List operations
//! - Directory/variable storage
//! - Transcendental functions (SIN, COS, etc.)
//! - And more

// Library modules
pub mod arith;
pub mod binary;
pub mod comments;
pub mod directory;
pub mod flow;
pub mod list;
pub mod locals;
pub mod prog;
pub mod stack;
pub mod strings;
pub mod symbolic;
pub mod transcendentals;
pub mod userlib;

// Re-export library structs
pub use arith::ArithLib;
pub use binary::BinaryLib;
pub use comments::CommentsLib;
pub use directory::DirectoryLib;
pub use flow::FlowLib;
pub use list::ListLib;
pub use locals::LocalsLib;
pub use prog::ProgLib;
pub use stack::StackLib;
pub use strings::StringsLib;
pub use symbolic::SymbolicLib;
pub use transcendentals::TranscendentalsLib;
pub use userlib::UserLibLib;

// Re-export library IDs
pub use arith::ARITH_LIB;
pub use binary::BINARY_LIB;
pub use comments::COMMENTS_LIB;
pub use directory::DIRECTORY_LIB;
pub use flow::FLOW_LIB;
pub use list::LIST_LIB;
pub use locals::LOCALS_LIB;
pub use prog::PROG_LIB;
pub use stack::STACK_LIB;
pub use strings::STRINGS_LIB;
pub use symbolic::SYMBOLIC_LIB;
pub use transcendentals::TRANSCENDENTALS_LIB;
pub use userlib::USERLIB_LIB;

use rpl::registry::{InterfaceRegistry, LowererRegistry, ExecutorRegistry};
use rpl::value::Value;

/// Create registries with standard libraries.
///
/// Returns a tuple of (InterfaceRegistry, LowererRegistry, ExecutorRegistry)
/// with all standard library components registered.
pub fn stdlib_registries() -> (InterfaceRegistry, LowererRegistry, ExecutorRegistry) {
    let mut interfaces = InterfaceRegistry::new();
    let mut lowerers = LowererRegistry::new();
    let mut executors = ExecutorRegistry::new();
    register_interfaces(&mut interfaces);
    register_lowerers(&mut lowerers);
    register_executors(&mut executors);
    (interfaces, lowerers, executors)
}

/// Evaluate RPL source code with standard library.
///
/// This is a convenience function for quick evaluation:
///
/// ```
/// let result = rpl_stdlib::eval("1 2 +").unwrap();
/// assert_eq!(result, vec![rpl::value::Value::integer(3)]);
/// ```
pub fn eval(source: &str) -> Result<Vec<Value>, String> {
    let (interfaces, lowerers, executors) = stdlib_registries();
    rpl::eval_with_registries(source, &interfaces, &lowerers, &executors)
}

/// Register all standard library interfaces (for parsing, analysis).
///
/// This registers cloned InterfaceSpecs from each library module.
pub fn register_interfaces(registry: &mut InterfaceRegistry) {
    registry.add(stack::interface().clone());
    registry.add(arith::interface().clone());
    registry.add(symbolic::interface().clone());
    registry.add(list::interface().clone());
    registry.add(transcendentals::interface().clone());
    registry.add(strings::interface().clone());
    registry.add(comments::interface().clone());
    registry.add(directory::interface().clone());
    registry.add(userlib::interface().clone());
    registry.add(binary::interface().clone());
    registry.add(flow::interface().clone());
    registry.add(prog::interface().clone());
    registry.add(locals::interface().clone());
}

/// Register all standard library lowerers (for compilation).
///
/// This registers the XxxLib implementation structs for lowering.
pub fn register_lowerers(registry: &mut LowererRegistry) {
    registry.add(StackLib);
    registry.add(ArithLib);
    registry.add(SymbolicLib);
    registry.add(ListLib);
    registry.add(TranscendentalsLib);
    registry.add(StringsLib);
    registry.add(CommentsLib);
    registry.add(DirectoryLib);
    registry.add(UserLibLib);
    registry.add(BinaryLib);
    registry.add(FlowLib);
    registry.add(ProgLib);
    registry.add(LocalsLib);
}

/// Register all standard library executors (for runtime).
///
/// Note: ProgLib is not registered here because EVAL is handled
/// directly by the VM, not through the executor registry.
///
/// This registers the XxxLib implementation structs for execution.
pub fn register_executors(registry: &mut ExecutorRegistry) {
    registry.add(StackLib);
    registry.add(ArithLib);
    registry.add(SymbolicLib);
    registry.add(ListLib);
    registry.add(TranscendentalsLib);
    registry.add(StringsLib);
    registry.add(CommentsLib);
    registry.add(DirectoryLib);
    registry.add(UserLibLib);
    registry.add(BinaryLib);
    registry.add(FlowLib);
    // ProgLib: no executor - EVAL is handled by VM directly
    registry.add(LocalsLib);
}
