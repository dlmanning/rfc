use rpl_core::{Pos, Span, Symbol};

use super::token::ResolvedToken;

/// Scope identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ScopeId(pub u32);

impl ScopeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// Kind of scope.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ScopeKind {
    /// Global scope (root).
    Global,
    /// Program scope (`:: ... ;`).
    Program,
    /// Local frame scope (`→ « »`).
    LocalFrame,
    /// Loop scope (`FOR ... NEXT/STEP`).
    Loop { variable: Option<Symbol> },
}

/// A scope in the scope tree.
#[derive(Clone, Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub kind: ScopeKind,
    pub span: Span,
    pub children: Vec<ScopeId>,
}

impl Scope {
    fn new(id: ScopeId, parent: Option<ScopeId>, kind: ScopeKind, span: Span) -> Self {
        Self {
            id,
            parent,
            kind,
            span,
            children: Vec::new(),
        }
    }
}

/// Tree of scopes for a source file.
pub struct ScopeTree {
    scopes: Vec<Scope>,
    root: ScopeId,
}

impl ScopeTree {
    /// Create a new scope tree with a root Global scope.
    pub fn new() -> Self {
        let root_id = ScopeId::new(0);
        let root = Scope::new(
            root_id,
            None,
            ScopeKind::Global,
            Span::new(Pos::new(0), Pos::new(u32::MAX)),
        );
        Self {
            scopes: vec![root],
            root: root_id,
        }
    }

    /// Get the root scope ID.
    pub fn root(&self) -> ScopeId {
        self.root
    }

    /// Add a new scope as a child of the parent.
    pub fn add_scope(&mut self, parent: ScopeId, kind: ScopeKind, span: Span) -> ScopeId {
        let id = ScopeId::new(self.scopes.len() as u32);
        let scope = Scope::new(id, Some(parent), kind, span);
        self.scopes.push(scope);

        // Add as child of parent
        if let Some(parent_scope) = self.scopes.get_mut(parent.as_u32() as usize) {
            parent_scope.children.push(id);
        }

        id
    }

    /// Close a scope by updating its end position.
    pub fn close_scope(&mut self, id: ScopeId, end: Pos) {
        if let Some(scope) = self.scopes.get_mut(id.as_u32() as usize) {
            scope.span = Span::new(scope.span.start(), end);
        }
    }

    /// Get a scope by ID.
    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(id.as_u32() as usize)
    }

    /// Get the parent of a scope.
    pub fn parent(&self, id: ScopeId) -> Option<ScopeId> {
        self.get(id).and_then(|s| s.parent)
    }

    /// Iterate over ancestors from the given scope up to (and including) the root.
    pub fn ancestors(&self, id: ScopeId) -> AncestorIter<'_> {
        AncestorIter {
            tree: self,
            current: Some(id),
        }
    }

    /// Find the innermost scope containing the given position.
    pub fn scope_at(&self, pos: Pos) -> ScopeId {
        self.scope_at_recursive(self.root, pos)
    }

    fn scope_at_recursive(&self, scope_id: ScopeId, pos: Pos) -> ScopeId {
        let scope = match self.get(scope_id) {
            Some(s) => s,
            None => return self.root,
        };

        // Check if position is in this scope
        if !scope.span.contains(pos) {
            return self.root;
        }

        // Check children (innermost wins)
        for &child_id in &scope.children {
            if let Some(child) = self.get(child_id)
                && child.span.contains(pos)
            {
                return self.scope_at_recursive(child_id, pos);
            }
        }

        // Position is in this scope but not in any child
        scope_id
    }

    /// Get the number of scopes.
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    /// Check if the tree has no scopes (should never be true after construction).
    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    /// Check if the tree only has the root scope (no user-defined scopes).
    pub fn has_only_root(&self) -> bool {
        self.scopes.len() <= 1
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator over ancestors of a scope.
pub struct AncestorIter<'a> {
    tree: &'a ScopeTree,
    current: Option<ScopeId>,
}

impl Iterator for AncestorIter<'_> {
    type Item = ScopeId;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.current?;
        self.current = self.tree.parent(id);
        Some(id)
    }
}

/// Build a scope tree from resolved tokens.
pub fn build_scopes(tokens: &[ResolvedToken]) -> ScopeTree {
    let mut tree = ScopeTree::new();
    let mut scope_stack = vec![tree.root()];

    for token in tokens {
        if token.context.starts_construct {
            // For now, all constructs create Program scopes
            // TODO: Distinguish LocalFrame and Loop based on token content
            let parent = *scope_stack.last().unwrap_or(&tree.root());
            let scope_id = tree.add_scope(parent, ScopeKind::Program, token.span);
            scope_stack.push(scope_id);
        }

        if token.context.ends_construct
            && let Some(scope_id) = scope_stack.pop()
        {
            // Don't pop the root
            if scope_id != tree.root() {
                tree.close_scope(scope_id, token.span.end());
            } else {
                // Put root back if we accidentally popped it
                scope_stack.push(scope_id);
            }
        }
    }

    tree
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::library::LibraryId;
    use rpl_core::token::{SemanticKind, TokenInfo};

    fn make_token(start: u32, end: u32) -> ResolvedToken {
        ResolvedToken::new(
            Span::new(Pos::new(start), Pos::new(end)),
            LibraryId::new(1),
            TokenInfo::atom(1),
            SemanticKind::Command,
        )
    }

    fn make_start_construct(start: u32, end: u32) -> ResolvedToken {
        let mut token = make_token(start, end);
        token.context.starts_construct = true;
        token
    }

    fn make_end_construct(start: u32, end: u32) -> ResolvedToken {
        let mut token = make_token(start, end);
        token.context.ends_construct = true;
        token
    }

    #[test]
    fn scope_id_new() {
        let id = ScopeId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn scope_tree_new() {
        let tree = ScopeTree::new();
        assert_eq!(tree.len(), 1);
        assert!(!tree.is_empty()); // Has root
        assert!(tree.has_only_root()); // Only root, no user scopes

        let root = tree.get(tree.root()).unwrap();
        assert_eq!(root.kind, ScopeKind::Global);
        assert!(root.parent.is_none());
    }

    #[test]
    fn scope_tree_add_scope() {
        let mut tree = ScopeTree::new();
        let span = Span::new(Pos::new(10), Pos::new(20));

        let child_id = tree.add_scope(tree.root(), ScopeKind::Program, span);

        assert_eq!(tree.len(), 2);
        let child = tree.get(child_id).unwrap();
        assert_eq!(child.kind, ScopeKind::Program);
        assert_eq!(child.parent, Some(tree.root()));
        assert_eq!(child.span, span);

        // Check parent has child
        let root = tree.get(tree.root()).unwrap();
        assert!(root.children.contains(&child_id));
    }

    #[test]
    fn scope_tree_close_scope() {
        let mut tree = ScopeTree::new();
        let span = Span::new(Pos::new(10), Pos::new(20));
        let child_id = tree.add_scope(tree.root(), ScopeKind::Program, span);

        tree.close_scope(child_id, Pos::new(50));

        let child = tree.get(child_id).unwrap();
        assert_eq!(child.span, Span::new(Pos::new(10), Pos::new(50)));
    }

    #[test]
    fn scope_tree_ancestors() {
        let mut tree = ScopeTree::new();
        let child1 = tree.add_scope(tree.root(), ScopeKind::Program, Span::DUMMY);
        let child2 = tree.add_scope(child1, ScopeKind::Program, Span::DUMMY);
        let child3 = tree.add_scope(child2, ScopeKind::Program, Span::DUMMY);

        let ancestors: Vec<_> = tree.ancestors(child3).collect();
        assert_eq!(ancestors, vec![child3, child2, child1, tree.root()]);
    }

    #[test]
    fn scope_tree_scope_at() {
        let mut tree = ScopeTree::new();

        // Create a scope from position 10-50
        let outer = tree.add_scope(
            tree.root(),
            ScopeKind::Program,
            Span::new(Pos::new(10), Pos::new(50)),
        );

        // Create a nested scope from position 20-40
        let inner = tree.add_scope(
            outer,
            ScopeKind::Program,
            Span::new(Pos::new(20), Pos::new(40)),
        );

        // Position 5 is in root only
        assert_eq!(tree.scope_at(Pos::new(5)), tree.root());

        // Position 15 is in outer but not inner
        assert_eq!(tree.scope_at(Pos::new(15)), outer);

        // Position 30 is in inner (innermost)
        assert_eq!(tree.scope_at(Pos::new(30)), inner);

        // Position 45 is in outer but not inner
        assert_eq!(tree.scope_at(Pos::new(45)), outer);

        // Position 55 is in root only
        assert_eq!(tree.scope_at(Pos::new(55)), tree.root());
    }

    #[test]
    fn build_scopes_empty() {
        let tokens: Vec<ResolvedToken> = vec![];
        let tree = build_scopes(&tokens);

        assert_eq!(tree.len(), 1); // Only root
    }

    #[test]
    fn build_scopes_simple_program() {
        // Simulate `:: ... ;`
        let tokens = vec![
            make_start_construct(0, 2), // ::
            make_token(3, 4),           // content
            make_end_construct(5, 6),   // ;
        ];

        let tree = build_scopes(&tokens);

        assert_eq!(tree.len(), 2); // Root + Program
        let root = tree.get(tree.root()).unwrap();
        assert_eq!(root.children.len(), 1);

        let program = tree.get(root.children[0]).unwrap();
        assert_eq!(program.kind, ScopeKind::Program);
        assert_eq!(program.span.end(), Pos::new(6));
    }

    #[test]
    fn build_scopes_nested_programs() {
        // Simulate `:: :: ; ;`
        let tokens = vec![
            make_start_construct(0, 2), // outer ::
            make_start_construct(3, 5), // inner ::
            make_end_construct(6, 7),   // inner ;
            make_end_construct(8, 9),   // outer ;
        ];

        let tree = build_scopes(&tokens);

        assert_eq!(tree.len(), 3); // Root + 2 Programs

        let root = tree.get(tree.root()).unwrap();
        assert_eq!(root.children.len(), 1);

        let outer = tree.get(root.children[0]).unwrap();
        assert_eq!(outer.kind, ScopeKind::Program);
        assert_eq!(outer.children.len(), 1);

        let inner = tree.get(outer.children[0]).unwrap();
        assert_eq!(inner.kind, ScopeKind::Program);
        assert_eq!(inner.parent, Some(root.children[0]));
    }

    #[test]
    fn build_scopes_scope_at_returns_innermost() {
        // Simulate `:: :: content ; ;`
        let tokens = vec![
            make_start_construct(0, 2), // outer :: at 0-2
            make_start_construct(3, 5), // inner :: at 3-5
            make_token(6, 10),          // content at 6-10
            make_end_construct(11, 12), // inner ; at 11-12
            make_end_construct(13, 14), // outer ; at 13-14
        ];

        let tree = build_scopes(&tokens);

        // Position 7 (in content) should be in innermost scope
        let scope_id = tree.scope_at(Pos::new(7));
        let scope = tree.get(scope_id).unwrap();

        // Should be the inner program, not outer or root
        assert_eq!(scope.kind, ScopeKind::Program);
        assert!(scope.parent.is_some());
        let parent = tree.get(scope.parent.unwrap()).unwrap();
        assert_eq!(parent.kind, ScopeKind::Program); // Parent is outer program
    }
}
