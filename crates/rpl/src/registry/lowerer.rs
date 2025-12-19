//! Lowerer registry for compilation.
//!
//! The LowererRegistry holds library lowerers which compile
//! commands and composites to bytecode during the lowering phase.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ir::LibId;
use crate::libs::LibraryLowerer;

/// Registry of library lowerers for compilation.
///
/// This registry is used during the lowering phase to compile
/// high-level IR to bytecode.
pub struct LowererRegistry {
    /// Library lowerers by ID.
    lowerers: HashMap<LibId, Arc<dyn LibraryLowerer>>,
}

impl Default for LowererRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl LowererRegistry {
    /// Create a new empty lowerer registry.
    pub fn new() -> Self {
        Self {
            lowerers: HashMap::new(),
        }
    }

    /// Register a library lowerer.
    pub fn add<T: LibraryLowerer + 'static>(&mut self, lowerer: T) {
        let id = lowerer.id();
        self.lowerers.insert(id, Arc::new(lowerer));
    }

    /// Register a library lowerer from an Arc (for shared lowerers).
    pub fn add_arc(&mut self, lowerer: Arc<dyn LibraryLowerer>) {
        let id = lowerer.id();
        self.lowerers.insert(id, lowerer);
    }

    /// Get a library lowerer by ID.
    pub fn get(&self, lib_id: LibId) -> Option<&dyn LibraryLowerer> {
        self.lowerers.get(&lib_id).map(|a| a.as_ref())
    }

    /// Check if a lowerer is registered for the given library ID.
    pub fn contains(&self, lib_id: LibId) -> bool {
        self.lowerers.contains_key(&lib_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Span;
    use crate::ir::Branch;
    use crate::lower::{LowerContext, LowerError};

    struct TestLowerer;

    impl LibraryLowerer for TestLowerer {
        fn id(&self) -> LibId {
            42
        }

        fn lower_command(
            &self,
            _cmd: u16,
            _span: Span,
            _ctx: &mut LowerContext,
        ) -> Result<(), LowerError> {
            Ok(())
        }

        fn lower_composite(
            &self,
            _construct_id: u16,
            _branches: &[Branch],
            _span: Span,
            _ctx: &mut LowerContext,
        ) -> Result<(), LowerError> {
            Ok(())
        }
    }

    #[test]
    fn add_and_get_lowerer() {
        let mut reg = LowererRegistry::new();
        reg.add(TestLowerer);

        assert!(reg.contains(42));
        assert!(reg.get(42).is_some());
        assert!(!reg.contains(99));
        assert!(reg.get(99).is_none());
    }
}
