//! RPL Virtual Machine
//!
//! This crate provides the core virtual machine for executing RPL bytecode.
//! It is designed to be library-agnostic through the `CommandDispatch` trait,
//! allowing different library implementations to be plugged in.
//!
//! # Architecture
//!
//! The VM handles:
//! - Stack management (data stack and return stack)
//! - Bytecode execution loop
//! - Prolog (object literal) handling
//! - Local variable frames
//! - Directory (global variable) management
//! - Debug support (breakpoints, stepping)
//!
//! Library-specific functionality (arithmetic, flow control, etc.) is delegated
//! to the `CommandDispatch` implementation.
//!
//! # Example
//!
//! ```ignore
//! use rpl_vm::{VM, CommandDispatch, execute};
//!
//! // Create a VM
//! let mut vm = VM::new();
//!
//! // Create a dispatcher (implements CommandDispatch)
//! let dispatcher = MyDispatcher::new();
//!
//! // Execute bytecode
//! let outcome = execute(&mut vm, &code, &dispatcher, None, interner)?;
//! ```

mod debug;
mod directory;
mod dispatch;
mod error;
mod execute;
mod frame;
mod machine;
mod value;

// Re-export public types
pub use debug::{DebugEvent, DebugMode, DebugState};
pub use directory::{Directory, DirectoryTree};
pub use dispatch::{CommandDispatch, DispatchResult, NullDispatcher};
pub use error::{RuntimeError, StackError};
pub use execute::{execute, execute_inline, execute_with_source, ExecuteOutcome};
pub use frame::LocalFrame;
pub use machine::{ReturnEntry, VM};
pub use value::{ProgramDebugInfo, Value};
