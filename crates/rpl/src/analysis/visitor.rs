//! Visitor pattern for walking IR trees.
//!
//! This module provides a trait and helper functions for traversing
//! the IR tree structure, which is useful for analysis passes.

use crate::ir::{AtomKind, Branch, CompositeKind, Node, NodeKind};
use crate::symbolic::SymExpr;

/// Trait for visiting IR nodes.
///
/// Implement this trait to perform custom traversal of the IR tree.
/// The default implementations call `walk_*` functions to continue traversal.
pub trait Visitor {
    /// Visit a node. Return `false` to skip visiting children.
    fn visit_node(&mut self, node: &Node) -> bool {
        let _ = node;
        true
    }

    /// Visit after processing a node's children.
    fn visit_node_post(&mut self, node: &Node) {
        let _ = node;
    }

    /// Visit an atom.
    fn visit_atom(&mut self, atom: &AtomKind, node: &Node) {
        let _ = (atom, node);
    }

    /// Visit a composite.
    fn visit_composite(&mut self, kind: CompositeKind, branches: &[Branch], node: &Node) {
        let _ = (kind, branches, node);
    }

    /// Visit a branch (sequence of nodes).
    fn visit_branch(&mut self, branch: &Branch) {
        let _ = branch;
    }

    // === Specific atom visitors ===

    /// Visit an integer literal.
    fn visit_integer(&mut self, value: i64, node: &Node) {
        let _ = (value, node);
    }

    /// Visit a real literal.
    fn visit_real(&mut self, value: f64, node: &Node) {
        let _ = (value, node);
    }

    /// Visit a string literal.
    fn visit_string(&mut self, value: &str, node: &Node) {
        let _ = (value, node);
    }

    /// Visit an unresolved symbol.
    fn visit_symbol(&mut self, sym: crate::core::Symbol, node: &Node) {
        let _ = (sym, node);
    }

    /// Visit a local reference.
    fn visit_local_ref(&mut self, index: usize, node: &Node) {
        let _ = (index, node);
    }

    /// Visit a command.
    fn visit_command(&mut self, lib: crate::ir::LibId, cmd: u16, node: &Node) {
        let _ = (lib, cmd, node);
    }

    /// Visit a symbolic expression.
    fn visit_symbolic(&mut self, expr: &SymExpr, node: &Node) {
        let _ = (expr, node);
    }

    // === Specific composite visitors ===

    /// Visit a program. Called before visiting the body.
    fn visit_program(&mut self, body: &Branch, node: &Node) {
        let _ = (body, node);
    }

    /// Visit after a program's body has been visited.
    fn visit_program_post(&mut self, body: &Branch, node: &Node) {
        let _ = (body, node);
    }

    /// Visit a list. Called before visiting items.
    fn visit_list(&mut self, items: &Branch, node: &Node) {
        let _ = (items, node);
    }

    /// Visit an extended composite (library-defined construct).
    fn visit_extended(&mut self, lib: crate::ir::LibId, id: u16, branches: &[Branch], node: &Node) {
        let _ = (lib, id, branches, node);
    }
}

/// Walk a sequence of nodes, calling visitor methods.
pub fn walk_nodes<V: Visitor>(visitor: &mut V, nodes: &[Node]) {
    for node in nodes {
        walk_node(visitor, node);
    }
}

/// Walk a single node, calling visitor methods.
pub fn walk_node<V: Visitor>(visitor: &mut V, node: &Node) {
    // Pre-visit - return false to skip children
    if !visitor.visit_node(node) {
        return;
    }

    match &node.kind {
        NodeKind::Atom(atom) => {
            visitor.visit_atom(atom, node);
            walk_atom(visitor, atom, node);
        }
        NodeKind::Composite(kind, branches) => {
            visitor.visit_composite(*kind, branches, node);
            walk_composite(visitor, *kind, branches, node);
        }
    }

    // Post-visit
    visitor.visit_node_post(node);
}

/// Walk an atom, calling specific visitor methods.
fn walk_atom<V: Visitor>(visitor: &mut V, atom: &AtomKind, node: &Node) {
    match atom {
        AtomKind::Integer(value) => visitor.visit_integer(*value, node),
        AtomKind::Real(value) => visitor.visit_real(*value, node),
        AtomKind::String(value) => visitor.visit_string(value, node),
        AtomKind::Symbol(sym) => visitor.visit_symbol(*sym, node),
        AtomKind::LocalRef(index) => visitor.visit_local_ref(*index, node),
        AtomKind::GlobalRef(sym) => visitor.visit_symbol(*sym, node),
        AtomKind::Command(lib, cmd) => visitor.visit_command(*lib, *cmd, node),
        AtomKind::Symbolic(expr) => visitor.visit_symbolic(expr, node),
    }
}

/// Walk a composite, calling specific visitor methods.
fn walk_composite<V: Visitor>(visitor: &mut V, kind: CompositeKind, branches: &[Branch], node: &Node) {
    match kind {
        CompositeKind::Program => {
            if let Some(body) = branches.first() {
                visitor.visit_program(body, node);
                visitor.visit_branch(body);
                walk_nodes(visitor, body);
                visitor.visit_program_post(body, node);
            }
        }
        CompositeKind::List => {
            if let Some(items) = branches.first() {
                visitor.visit_list(items, node);
                visitor.visit_branch(items);
                walk_nodes(visitor, items);
            }
        }
        CompositeKind::Extended(lib, id) => {
            visitor.visit_extended(lib, id, branches, node);
            for branch in branches {
                visitor.visit_branch(branch);
                walk_nodes(visitor, branch);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::Node;
    use crate::core::{Pos, Span};

    fn dummy_span() -> Span {
        Span::new(Pos::new(0), Pos::new(1))
    }

    struct CountingVisitor {
        nodes: usize,
        integers: usize,
        programs: usize,
    }

    impl CountingVisitor {
        fn new() -> Self {
            Self {
                nodes: 0,
                integers: 0,
                programs: 0,
            }
        }
    }

    impl Visitor for CountingVisitor {
        fn visit_node(&mut self, _node: &Node) -> bool {
            self.nodes += 1;
            true
        }

        fn visit_integer(&mut self, _value: i64, _node: &Node) {
            self.integers += 1;
        }

        fn visit_program(&mut self, _body: &Branch, _node: &Node) {
            self.programs += 1;
        }
    }

    #[test]
    fn visit_integers() {
        let nodes = vec![
            Node::integer(1, dummy_span()),
            Node::integer(2, dummy_span()),
            Node::integer(3, dummy_span()),
        ];

        let mut visitor = CountingVisitor::new();
        walk_nodes(&mut visitor, &nodes);

        assert_eq!(visitor.nodes, 3);
        assert_eq!(visitor.integers, 3);
        assert_eq!(visitor.programs, 0);
    }

    #[test]
    fn visit_program() {
        let program = Node::program(
            vec![
                Node::integer(1, dummy_span()),
                Node::integer(2, dummy_span()),
            ],
            dummy_span(),
        );

        let mut visitor = CountingVisitor::new();
        walk_node(&mut visitor, &program);

        assert_eq!(visitor.nodes, 3); // program + 2 integers
        assert_eq!(visitor.integers, 2);
        assert_eq!(visitor.programs, 1);
    }

    #[test]
    fn visit_nested() {
        let program = Node::program(
            vec![
                Node::integer(1, dummy_span()),
                Node::list(
                    vec![
                        Node::integer(2, dummy_span()),
                        Node::integer(3, dummy_span()),
                    ],
                    dummy_span(),
                ),
            ],
            dummy_span(),
        );

        let mut visitor = CountingVisitor::new();
        walk_node(&mut visitor, &program);

        assert_eq!(visitor.nodes, 5); // program + 1 + list + 2
        assert_eq!(visitor.integers, 3);
    }

    struct SkippingVisitor {
        visited: Vec<String>,
    }

    impl Visitor for SkippingVisitor {
        fn visit_node(&mut self, node: &Node) -> bool {
            // Skip programs (don't visit their children)
            if matches!(node.kind, NodeKind::Composite(CompositeKind::Program, _)) {
                self.visited.push("program(skipped)".to_string());
                return false;
            }
            true
        }

        fn visit_integer(&mut self, value: i64, _node: &Node) {
            self.visited.push(format!("int({})", value));
        }
    }

    #[test]
    fn skip_children() {
        let nodes = vec![
            Node::integer(1, dummy_span()),
            Node::program(
                vec![Node::integer(99, dummy_span())],
                dummy_span(),
            ),
            Node::integer(2, dummy_span()),
        ];

        let mut visitor = SkippingVisitor { visited: Vec::new() };
        walk_nodes(&mut visitor, &nodes);

        assert_eq!(visitor.visited, vec![
            "int(1)".to_string(),
            "program(skipped)".to_string(),
            "int(2)".to_string(),
        ]);
    }
}
