//! Analysis context for cross-file awareness.
//!
//! The `Context` provides information about external names (like project entries)
//! that the analyzer should recognize as valid. This enables proper analysis of
//! files that reference other project files.

use std::collections::HashMap;

use crate::types::Signature;

/// Information about a known external entry.
#[derive(Debug, Clone)]
pub struct EntryInfo {
    /// Whether this is a program, list, integer, etc.
    pub is_program: bool,
    /// For programs: the inferred signature.
    pub signature: Option<Signature>,
}

impl EntryInfo {
    /// Create entry info for a program with optional signature.
    pub fn program(signature: Option<Signature>) -> Self {
        Self {
            is_program: true,
            signature,
        }
    }

    /// Create entry info for a non-program value.
    pub fn value() -> Self {
        Self {
            is_program: false,
            signature: None,
        }
    }
}

/// Context for analysis, providing information about external names.
///
/// When analyzing a file that's part of a project, the context contains
/// entries for all other project files. This allows the analyzer to:
/// - Not flag references to project entries as "undefined variable"
/// - Use signatures of project functions for type inference
#[derive(Debug, Clone, Default)]
pub struct Context {
    /// Known external entries by name.
    entries: HashMap<String, EntryInfo>,
}

impl Context {
    /// Create an empty context (for single-file analysis).
    pub fn new() -> Self {
        Self::default()
    }

    /// Create an empty context (alias for clarity at call sites).
    pub fn empty() -> Self {
        Self::new()
    }

    /// Add a known entry.
    pub fn add(&mut self, name: impl Into<String>, info: EntryInfo) {
        self.entries.insert(name.into(), info);
    }

    /// Add a known program entry with optional signature.
    pub fn add_program(&mut self, name: impl Into<String>, signature: Option<Signature>) {
        self.add(name, EntryInfo::program(signature));
    }

    /// Add a known non-program entry (list, integer, etc.).
    pub fn add_value(&mut self, name: impl Into<String>) {
        self.add(name, EntryInfo::value());
    }

    /// Check if a name is known in this context.
    pub fn is_known(&self, name: &str) -> bool {
        self.entries.contains_key(name)
    }

    /// Get the entry info for a known name.
    pub fn get(&self, name: &str) -> Option<&EntryInfo> {
        self.entries.get(name)
    }

    /// Get the signature for a known program.
    pub fn signature(&self, name: &str) -> Option<&Signature> {
        self.entries.get(name)?.signature.as_ref()
    }

    /// Iterate over all known entries.
    pub fn entries(&self) -> impl Iterator<Item = (&str, &EntryInfo)> {
        self.entries.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Number of known entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if context is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_context() {
        let ctx = Context::empty();
        assert!(ctx.is_empty());
        assert!(!ctx.is_known("foo"));
    }

    #[test]
    fn add_and_lookup() {
        let mut ctx = Context::new();
        ctx.add_program("lib/square", None);
        ctx.add_value("constants");

        assert!(ctx.is_known("lib/square"));
        assert!(ctx.is_known("constants"));
        assert!(!ctx.is_known("unknown"));

        assert!(ctx.get("lib/square").unwrap().is_program);
        assert!(!ctx.get("constants").unwrap().is_program);
    }
}
