//! Source file management.
//!
//! This module provides:
//! - `SourceFile` - A source file with line mapping
//! - `SourceCache` - Cache of source files by ID
//! - `SourceId` - Unique identifier for source files
//! - `DiagnosticRenderer` - Renders diagnostics with source context

mod cache;
mod file;
mod render;

pub use cache::SourceCache;
pub use file::{LineCol, SourceFile, SourceId};
pub use render::DiagnosticRenderer;
