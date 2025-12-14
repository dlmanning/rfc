//! Procedural macros for the RPL language.
//!
//! This crate provides the `define_library!` macro for declaring RPL libraries
//! with minimal boilerplate.
//!
//! ## Example
//!
//! ```ignore
//! use rpl_lang::library::{ExecuteOk, EXEC_OK};
//!
//! define_library! {
//!     pub struct StackLib {
//!         id: 72,
//!         name: "Stack",
//!     }
//!
//!     commands {
//!         DUP => (1 -> 2) "Duplicate top of stack" {
//!             let val = ctx.peek(0)?.clone();
//!             ctx.push(val)?;
//!             EXEC_OK
//!         }
//!
//!         DROP => (1 -> 0) "Remove top of stack" {
//!             ctx.pop()?;
//!             EXEC_OK
//!         }
//!
//!         CLEAR => dynamic "Clear entire stack" {
//!             ctx.clear();
//!             EXEC_OK
//!         }
//!     }
//! }
//! ```

mod ast;
mod codegen;
mod parse;

use proc_macro::TokenStream;
use syn::parse_macro_input;

use ast::LibraryDef;

/// Define an RPL library with minimal boilerplate.
///
/// This macro generates:
/// - The struct definition
/// - Command ID constants
/// - `command_id()` and `command_name()` lookup functions
/// - Full `Library` trait implementation
/// - Token documentation
///
/// ## Syntax
///
/// ```ignore
/// define_library! {
///     [pub] struct LibName {
///         id: <library_id>,
///         name: "<display_name>",
///     }
///
///     commands {
///         COMMAND_NAME => (<consumes> -> <produces>) "<brief_doc>" {
///             // execute body - ctx: &mut ExecuteContext is in scope
///             // must return ExecuteResult
///         }
///
///         // Dynamic stack effect
///         OTHER_CMD => dynamic "Description" {
///             // ...
///         }
///
///         // Full documentation
///         FULL_DOC => (2 -> 1) [
///             brief: "Brief description",
///             stack: "( a b -- c )",
///             example: "1 2 ADD",
///             see_also: ["SUB", "MUL"],
///         ] {
///             // ...
///         }
///
///         // Internal command (not matched by probe)
///         @INTERNAL => (0 -> 0) "Internal only" {
///             // ...
///         }
///     }
///
///     // Optional: custom probe implementation
///     custom probe(ctx) {
///         // custom probe logic
///     }
///
///     // Optional: custom compile implementation
///     custom compile(ctx) {
///         // custom compile logic
///     }
/// }
/// ```
#[proc_macro]
pub fn define_library(input: TokenStream) -> TokenStream {
    let lib_def = parse_macro_input!(input as LibraryDef);
    codegen::generate(&lib_def).into()
}
