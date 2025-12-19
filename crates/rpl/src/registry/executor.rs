//! Executor registry for runtime.
//!
//! The ExecutorRegistry holds library executors which execute
//! commands at runtime when the VM encounters CallLib instructions.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ir::LibId;
use crate::libs::LibraryExecutor;

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
    use crate::libs::{ExecuteContext, ExecuteResult};

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
