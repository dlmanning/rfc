//! RPL IDE WASM Component
//!
//! Provides IDE functionality as a WASM component with WASI support.

pub mod core;

// WIT bindings and exports are only compiled for WASM target
#[cfg(target_arch = "wasm32")]
mod wasm;
