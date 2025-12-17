//! Scope tree for tracking variable visibility.
//!
//! RPL has several scope-creating constructs:
//! - Programs (`« »`) create a new scope
//! - Local bindings (`→ x « »`) create a nested scope
//! - Loops with variables (`FOR i`) create a scope for the loop variable

use crate::core::Span;

/// Unique identifier for a scope.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ScopeId(u32);

impl ScopeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// The root (global) scope.
    pub fn root() -> Self {
        Self(0)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Check if this is the root scope.
    pub fn is_root(self) -> bool {
        self.0 == 0
    }
}

/// The kind of scope.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScopeKind {
    /// The global/root scope.
    Global,
    /// A program scope (`« »`).
    Program,
    /// A local binding scope (`→ x « »`).
    LocalBinding,
    /// A loop scope (FOR).
    Loop,
    /// A conditional scope (IF branches).
    Conditional,
}

/// A single scope in the scope tree.
#[derive(Clone, Debug)]
pub struct Scope {
    /// Unique ID for this scope.
    pub id: ScopeId,
    /// Parent scope (None for root).
    pub parent: Option<ScopeId>,
    /// Kind of scope.
    pub kind: ScopeKind,
    /// Source span of this scope.
    pub span: Span,
    /// Child scopes.
    pub children: Vec<ScopeId>,
}

impl Scope {
    /// Create a new scope.
    pub fn new(kind: ScopeKind, span: Span, parent: Option<ScopeId>) -> Self {
        Self {
            id: ScopeId::new(0), // Set when added to tree
            parent,
            kind,
            span,
            children: Vec::new(),
        }
    }

    /// Create the root scope.
    pub fn root() -> Self {
        Self {
            id: ScopeId::root(),
            parent: None,
            kind: ScopeKind::Global,
            span: Span::DUMMY,
            children: Vec::new(),
        }
    }
}

/// A tree of scopes.
#[derive(Clone, Debug)]
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeTree {
    /// Create a new scope tree with just the root scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::root()],
        }
    }

    /// Get the root scope ID.
    pub fn root(&self) -> ScopeId {
        ScopeId::root()
    }

    /// Add a new scope as a child of the given parent.
    pub fn add_scope(&mut self, mut scope: Scope, parent: ScopeId) -> ScopeId {
        let id = ScopeId::new(self.scopes.len() as u32);
        scope.id = id;
        scope.parent = Some(parent);
        self.scopes.push(scope);

        // Add as child of parent
        if let Some(parent_scope) = self.scopes.get_mut(parent.as_u32() as usize) {
            parent_scope.children.push(id);
        }

        id
    }

    /// Get a scope by ID.
    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(id.as_u32() as usize)
    }

    /// Get a mutable scope by ID.
    pub fn get_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(id.as_u32() as usize)
    }

    /// Get the parent of a scope.
    pub fn parent(&self, id: ScopeId) -> Option<ScopeId> {
        self.get(id).and_then(|s| s.parent)
    }

    /// Get all ancestors of a scope (from parent to root).
    pub fn ancestors(&self, id: ScopeId) -> Vec<ScopeId> {
        let mut result = Vec::new();
        let mut current = self.parent(id);
        while let Some(parent_id) = current {
            result.push(parent_id);
            current = self.parent(parent_id);
        }
        result
    }

    /// Check if `inner` is contained within `outer` (or is the same scope).
    pub fn contains(&self, outer: ScopeId, inner: ScopeId) -> bool {
        if outer == inner {
            return true;
        }
        self.ancestors(inner).contains(&outer)
    }

    /// Get the number of scopes.
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    /// Check if the tree is empty (only root).
    pub fn is_empty(&self) -> bool {
        self.scopes.len() <= 1
    }

    /// Iterate over all scopes.
    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter()
    }

    /// Find the innermost scope containing a position.
    pub fn scope_at(&self, pos: crate::core::Pos) -> ScopeId {
        // Start from root and find the deepest scope containing this position
        let mut best = ScopeId::root();
        for scope in &self.scopes {
            if scope.span.contains(pos) {
                // Prefer deeper scopes (higher ID means added later, thus nested)
                if scope.id.as_u32() > best.as_u32() {
                    best = scope.id;
                }
            }
        }
        best
    }

    /// Get the depth of a scope (0 for root).
    pub fn depth(&self, id: ScopeId) -> usize {
        self.ancestors(id).len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn scope_id_root() {
        let root = ScopeId::root();
        assert!(root.is_root());
        assert_eq!(root.as_u32(), 0);
    }

    #[test]
    fn scope_tree_new() {
        let tree = ScopeTree::new();
        assert_eq!(tree.len(), 1);
        assert!(tree.is_empty()); // Only root

        let root = tree.get(ScopeId::root()).unwrap();
        assert_eq!(root.kind, ScopeKind::Global);
        assert!(root.parent.is_none());
    }

    #[test]
    fn add_scope() {
        let mut tree = ScopeTree::new();

        let child_id = tree.add_scope(
            Scope::new(ScopeKind::Program, make_span(0, 100), None),
            ScopeId::root(),
        );

        assert_eq!(child_id.as_u32(), 1);
        assert_eq!(tree.len(), 2);
        assert!(!tree.is_empty());

        let child = tree.get(child_id).unwrap();
        assert_eq!(child.kind, ScopeKind::Program);
        assert_eq!(child.parent, Some(ScopeId::root()));

        let root = tree.get(ScopeId::root()).unwrap();
        assert!(root.children.contains(&child_id));
    }

    #[test]
    fn ancestors() {
        let mut tree = ScopeTree::new();

        let s1 = tree.add_scope(
            Scope::new(ScopeKind::Program, make_span(0, 100), None),
            ScopeId::root(),
        );
        let s2 = tree.add_scope(
            Scope::new(ScopeKind::LocalBinding, make_span(10, 90), None),
            s1,
        );
        let s3 = tree.add_scope(
            Scope::new(ScopeKind::Loop, make_span(20, 80), None),
            s2,
        );

        let ancestors = tree.ancestors(s3);
        assert_eq!(ancestors, vec![s2, s1, ScopeId::root()]);

        let root_ancestors = tree.ancestors(ScopeId::root());
        assert!(root_ancestors.is_empty());
    }

    #[test]
    fn contains() {
        let mut tree = ScopeTree::new();

        let s1 = tree.add_scope(
            Scope::new(ScopeKind::Program, make_span(0, 100), None),
            ScopeId::root(),
        );
        let s2 = tree.add_scope(
            Scope::new(ScopeKind::LocalBinding, make_span(10, 90), None),
            s1,
        );

        assert!(tree.contains(ScopeId::root(), s2));
        assert!(tree.contains(s1, s2));
        assert!(tree.contains(s2, s2)); // Same scope
        assert!(!tree.contains(s2, s1)); // s1 is not in s2
    }

    #[test]
    fn depth() {
        let mut tree = ScopeTree::new();

        let s1 = tree.add_scope(
            Scope::new(ScopeKind::Program, make_span(0, 100), None),
            ScopeId::root(),
        );
        let s2 = tree.add_scope(
            Scope::new(ScopeKind::LocalBinding, make_span(10, 90), None),
            s1,
        );

        assert_eq!(tree.depth(ScopeId::root()), 0);
        assert_eq!(tree.depth(s1), 1);
        assert_eq!(tree.depth(s2), 2);
    }

    #[test]
    fn scope_at() {
        let mut tree = ScopeTree::new();

        let s1 = tree.add_scope(
            Scope::new(ScopeKind::Program, make_span(10, 100), None),
            ScopeId::root(),
        );
        let s2 = tree.add_scope(
            Scope::new(ScopeKind::LocalBinding, make_span(20, 80), None),
            s1,
        );

        // Position before any scope
        assert_eq!(tree.scope_at(Pos::new(5)), ScopeId::root());

        // Position in s1 but not s2
        assert_eq!(tree.scope_at(Pos::new(15)), s1);

        // Position in s2 (deepest)
        assert_eq!(tree.scope_at(Pos::new(50)), s2);

        // Position after s2 but still in s1
        assert_eq!(tree.scope_at(Pos::new(85)), s1);
    }
}
