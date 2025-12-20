//! Library executor traits and registry for runtime.
//!
//! This module provides the traits and types for library execution at runtime.
//! Libraries implement `LibraryExecutor` to handle CallLib instructions.

use std::collections::HashMap;
use std::sync::Arc;

use crate::directory::Directory;
use crate::stack::Stack;
use crate::value::Value;
use crate::LibId;

/// An RPL exception (for IFERR/THROW).
#[derive(Clone, Debug)]
pub struct RplException {
    pub code: i64,
    pub message: String,
}

impl RplException {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            code: 0,
            message: message.into(),
        }
    }
    pub fn with_code(code: i64, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

/// Context for executing library commands at runtime.
pub struct ExecuteContext<'a> {
    /// The data stack.
    pub stack: &'a mut Stack,
    /// The global directory.
    pub directory: &'a mut Directory,
    /// The command ID being executed.
    pub cmd: u16,
    /// The last error that was caught (for ERRN/ERRM).
    last_error: Option<RplException>,
}

impl<'a> ExecuteContext<'a> {
    /// Create a new execute context.
    pub fn new(
        stack: &'a mut Stack,
        directory: &'a mut Directory,
        cmd: u16,
        last_error: Option<RplException>,
    ) -> Self {
        Self {
            stack,
            directory,
            cmd,
            last_error,
        }
    }

    /// Get the last error (for ERRN/ERRM).
    pub fn last_error(&self) -> Option<&RplException> {
        self.last_error.as_ref()
    }

    /// Pop a value from the stack.
    pub fn pop(&mut self) -> Result<Value, String> {
        self.stack.pop().map_err(|e| e.to_string())
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: Value) -> Result<(), String> {
        self.stack.push(value).map_err(|e| e.to_string())
    }

    /// Peek at a value on the stack (0 = top).
    pub fn peek(&self, index: usize) -> Result<&Value, String> {
        self.stack.peek(index).map_err(|e| e.to_string())
    }

    /// Get the stack depth.
    pub fn depth(&self) -> usize {
        self.stack.depth()
    }

    // === Directory operations ===

    /// Store a value in the directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.directory.store(name, value);
    }

    /// Look up a value in the directory.
    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.directory.lookup(name)
    }

    /// Remove a variable from the directory.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.directory.purge(name)
    }

    /// Check if a variable exists.
    pub fn has_var(&self, name: &str) -> bool {
        self.directory.has_var(name)
    }

    /// Get an iterator over variable names.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.directory.vars()
    }

    /// Clear all variables.
    pub fn clear_vars(&mut self) {
        self.directory.clear();
    }

    /// Rename a variable.
    pub fn rename_var(&mut self, old_name: &str, new_name: &str) -> bool {
        self.directory.rename(old_name, new_name)
    }

    // === Directory navigation ===

    /// Create a subdirectory in the current directory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.directory.create_subdir(name)
    }

    /// Remove an empty subdirectory from the current directory.
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), String> {
        self.directory.remove_subdir(name)
    }

    /// Enter a subdirectory. Returns false if it doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        self.directory.enter_subdir(name)
    }

    /// Move up one directory level.
    pub fn updir(&mut self) {
        self.directory.updir();
    }

    /// Move to root directory.
    pub fn home(&mut self) {
        self.directory.home();
    }

    /// Get the current directory path.
    pub fn dir_path(&self) -> &[String] {
        self.directory.dir_path()
    }

    // === Path-based operations (for library data) ===

    /// Store a value at an absolute path (creates intermediate directories).
    pub fn store_at_path(&mut self, path: &[&str], name: &str, value: Value) {
        self.directory.store_at_path(path, name, value);
    }

    /// Look up a value at an absolute path.
    pub fn lookup_at_path(&self, path: &[&str], name: &str) -> Option<&Value> {
        self.directory.lookup_at_path(path, name)
    }

    /// Purge a value at an absolute path.
    pub fn purge_at_path(&mut self, path: &[&str], name: &str) -> Option<Value> {
        self.directory.purge_at_path(path, name)
    }

    /// Clear all variables at an absolute path.
    pub fn clear_at_path(&mut self, path: &[&str]) {
        self.directory.clear_at_path(path);
    }
}

/// Result of executing a command.
pub type ExecuteResult = Result<(), String>;

/// Executor trait: executes commands at runtime.
///
/// Implement this for libraries that need runtime execution.
/// Libraries that are entirely compile-time (like FlowLib) don't need this.
pub trait LibraryExecutor: Send + Sync {
    /// Get the library ID.
    fn id(&self) -> LibId;

    /// Execute a command at runtime.
    ///
    /// Called when the VM encounters a CallLib instruction.
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult;
}

/// Registry of library executors for runtime.
///
/// This registry is used during execution to handle CallLib instructions.
pub struct ExecutorRegistry {
    /// Library executors by ID.
    executors: HashMap<LibId, Arc<dyn LibraryExecutor>>,
}

impl Default for ExecutorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ExecutorRegistry {
    /// Create a new empty executor registry.
    pub fn new() -> Self {
        Self {
            executors: HashMap::new(),
        }
    }

    /// Register a library executor.
    pub fn add<T: LibraryExecutor + 'static>(&mut self, executor: T) {
        let id = executor.id();
        self.executors.insert(id, Arc::new(executor));
    }

    /// Register a library executor from an Arc (for shared executors).
    pub fn add_arc(&mut self, executor: Arc<dyn LibraryExecutor>) {
        let id = executor.id();
        self.executors.insert(id, executor);
    }

    /// Get a library executor by ID.
    pub fn get(&self, lib_id: LibId) -> Option<&dyn LibraryExecutor> {
        self.executors.get(&lib_id).map(|a| a.as_ref())
    }

    /// Check if an executor is registered for the given library ID.
    pub fn contains(&self, lib_id: LibId) -> bool {
        self.executors.contains_key(&lib_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestExecutor;

    impl LibraryExecutor for TestExecutor {
        fn id(&self) -> LibId {
            42
        }

        fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
            Ok(())
        }
    }

    #[test]
    fn add_and_get_executor() {
        let mut reg = ExecutorRegistry::new();
        reg.add(TestExecutor);

        assert!(reg.contains(42));
        assert!(reg.get(42).is_some());
        assert!(!reg.contains(99));
        assert!(reg.get(99).is_none());
    }
}
