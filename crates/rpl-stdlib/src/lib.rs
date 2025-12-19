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

use rpl::registry::Registry;
use rpl::value::Value;

/// Create a registry with standard libraries.
pub fn stdlib_registry() -> Registry {
    let mut registry = Registry::new();
    register_interfaces(&mut registry);
    register_impls(&mut registry);
    registry
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
    let registry = stdlib_registry();
    rpl::eval_with_registry(source, &registry)
}

/// Register all standard library interfaces (for parsing, analysis).
///
/// This registers cloned InterfaceSpecs from each library module.
pub fn register_interfaces(registry: &mut Registry) {
    registry.add_interface(stack::interface().clone());
    registry.add_interface(arith::interface().clone());
    registry.add_interface(symbolic::interface().clone());
    registry.add_interface(list::interface().clone());
    registry.add_interface(transcendentals::interface().clone());
    registry.add_interface(strings::interface().clone());
    registry.add_interface(comments::interface().clone());
    registry.add_interface(directory::interface().clone());
    registry.add_interface(userlib::interface().clone());
    registry.add_interface(binary::interface().clone());
    registry.add_interface(flow::interface().clone());
    registry.add_interface(prog::interface().clone());
    registry.add_interface(locals::interface().clone());
}

/// Register all standard library implementations (for lowering, execution).
///
/// This registers the XxxLib implementation structs.
pub fn register_impls(registry: &mut Registry) {
    registry.add_impl(StackLib);
    registry.add_impl(ArithLib);
    registry.add_impl(SymbolicLib);
    registry.add_impl(ListLib);
    registry.add_impl(TranscendentalsLib);
    registry.add_impl(StringsLib);
    registry.add_impl(CommentsLib);
    registry.add_impl(DirectoryLib);
    registry.add_impl(UserLibLib);
    registry.add_impl(BinaryLib);
    registry.add_impl(FlowLib);
    registry.add_impl(ProgLib);
    registry.add_impl(LocalsLib);
}
