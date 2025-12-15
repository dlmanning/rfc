use std::sync::Arc;

use super::context::ProbeContext;
use super::id::LibraryId;
use super::traits::{Library, ProbeResult};

/// Registry for libraries.
#[derive(Default)]
pub struct LibraryRegistry {
    by_id: Vec<Option<Arc<dyn Library>>>,
    by_priority: Vec<(i32, Arc<dyn Library>)>,
}

impl LibraryRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a library with the given priority.
    /// Higher priority libraries are probed first.
    pub fn register(&mut self, lib: impl Library + 'static, priority: i32) {
        let lib: Arc<dyn Library> = Arc::new(lib);
        let id = lib.id().as_u16() as usize;

        // Ensure by_id has enough space
        if id >= self.by_id.len() {
            self.by_id.resize(id + 1, None);
        }
        self.by_id[id] = Some(Arc::clone(&lib));

        // Insert sorted by priority (descending)
        let pos = self
            .by_priority
            .iter()
            .position(|(p, _)| *p < priority)
            .unwrap_or(self.by_priority.len());
        self.by_priority.insert(pos, (priority, lib));
    }

    /// Get a library by ID.
    pub fn get(&self, id: LibraryId) -> Option<&Arc<dyn Library>> {
        self.by_id
            .get(id.as_u16() as usize)
            .and_then(|opt| opt.as_ref())
    }

    /// Iterate over libraries in priority order (highest first).
    pub fn iter_by_priority(&self) -> impl Iterator<Item = &Arc<dyn Library>> {
        self.by_priority.iter().map(|(_, lib)| lib)
    }

    /// Probe all libraries for a token, returning the first match.
    pub fn probe(&self, ctx: &ProbeContext) -> Option<(LibraryId, ProbeResult)> {
        for lib in self.iter_by_priority() {
            match lib.probe(ctx) {
                ProbeResult::NoMatch => continue,
                result => return Some((lib.id(), result)),
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::super::context::CompileContext;
    use super::super::traits::{CompileResult, ExecuteOk, ExecuteResult};
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_core::token::{SemanticKind, TokenInfo};

    struct MockLibrary {
        id: LibraryId,
        name: &'static str,
        matches: bool,
    }

    impl Library for MockLibrary {
        fn id(&self) -> LibraryId {
            self.id
        }

        fn name(&self) -> &'static str {
            self.name
        }

        fn probe(&self, _ctx: &ProbeContext) -> ProbeResult {
            if self.matches {
                ProbeResult::Match {
                    info: TokenInfo::atom(3),
                    semantic: SemanticKind::Command,
                }
            } else {
                ProbeResult::NoMatch
            }
        }

        fn compile(&self, _ctx: &mut CompileContext) -> CompileResult {
            CompileResult::Ok
        }

        fn execute(&self, _ctx: &mut super::super::context::ExecuteContext) -> ExecuteResult {
            Ok(ExecuteOk::Ok)
        }
    }

    #[test]
    fn registry_stores_and_retrieves_by_id() {
        let mut registry = LibraryRegistry::new();

        registry.register(
            MockLibrary {
                id: LibraryId::new(10),
                name: "test",
                matches: false,
            },
            0,
        );

        let lib = registry.get(LibraryId::new(10));
        assert!(lib.is_some());
        assert_eq!(lib.unwrap().name(), "test");

        assert!(registry.get(LibraryId::new(99)).is_none());
    }

    #[test]
    fn registry_iterates_in_priority_order() {
        let mut registry = LibraryRegistry::new();

        registry.register(
            MockLibrary {
                id: LibraryId::new(1),
                name: "low",
                matches: false,
            },
            10,
        );
        registry.register(
            MockLibrary {
                id: LibraryId::new(2),
                name: "high",
                matches: false,
            },
            100,
        );
        registry.register(
            MockLibrary {
                id: LibraryId::new(3),
                name: "medium",
                matches: false,
            },
            50,
        );

        let names: Vec<_> = registry.iter_by_priority().map(|l| l.name()).collect();
        assert_eq!(names, vec!["high", "medium", "low"]);
    }

    #[test]
    fn probe_returns_highest_priority_match() {
        let mut registry = LibraryRegistry::new();

        registry.register(
            MockLibrary {
                id: LibraryId::new(1),
                name: "low_match",
                matches: true,
            },
            10,
        );
        registry.register(
            MockLibrary {
                id: LibraryId::new(2),
                name: "high_no_match",
                matches: false,
            },
            100,
        );
        registry.register(
            MockLibrary {
                id: LibraryId::new(3),
                name: "medium_match",
                matches: true,
            },
            50,
        );

        let interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));
        let ctx = ProbeContext::new("DUP", "DUP", span, false, None, None, &interner);

        let result = registry.probe(&ctx);
        assert!(result.is_some());
        let (id, _) = result.unwrap();
        // medium_match (priority 50) should win since high_no_match doesn't match
        assert_eq!(id, LibraryId::new(3));
    }

    #[test]
    fn probe_returns_none_when_no_match() {
        let mut registry = LibraryRegistry::new();

        registry.register(
            MockLibrary {
                id: LibraryId::new(1),
                name: "no_match",
                matches: false,
            },
            10,
        );

        let interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));
        let ctx = ProbeContext::new("FOO", "FOO", span, false, None, None, &interner);

        assert!(registry.probe(&ctx).is_none());
    }
}
