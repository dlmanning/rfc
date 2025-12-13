//! Decompilation module - converts bytecode back to source code.
//!
//! The decompiler iterates through bytecode and asks each registered library
//! to decompile words it recognizes. The result is RPL source code that,
//! when recompiled, should produce equivalent bytecode.

mod decompiler;

pub use decompiler::{Decompiler, decompile, decompile_inner, decompile_with_interner};
