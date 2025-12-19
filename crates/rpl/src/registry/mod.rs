//! Library registries for managing commands and token claims.
//!
//! The registry system is split into three phase-specific registries:
//!
//! - **InterfaceRegistry**: Parsing + analysis (token claims, command lookup, stack effects)
//! - **LowererRegistry**: Compilation (lowering commands and composites to bytecode)
//! - **ExecutorRegistry**: Runtime (executing commands)
//!
//! This separation ensures clean phase boundaries where:
//! - Lowering code can't accidentally call execute()
//! - Runtime code doesn't have access to parsing internals
//! - Each phase only sees what it needs
//!
//! # Example
//!
//! ```ignore
//! use rpl::registry::{InterfaceRegistry, LowererRegistry, ExecutorRegistry};
//! use rpl_stdlib;
//!
//! // Create registries
//! let mut interfaces = InterfaceRegistry::new();
//! let mut lowerers = LowererRegistry::new();
//! let mut executors = ExecutorRegistry::new();
//!
//! // Register standard library
//! rpl_stdlib::register_interfaces(&mut interfaces);
//! rpl_stdlib::register_lowerers(&mut lowerers);
//! rpl_stdlib::register_executors(&mut executors);
//! ```

mod executor;
mod interface;
mod lowerer;

pub use executor::ExecutorRegistry;
pub use interface::{CommandRef, InterfaceRegistry};
pub use lowerer::LowererRegistry;

// Re-export for convenience
pub use crate::libs::{ClaimContext, TokenClaim};
