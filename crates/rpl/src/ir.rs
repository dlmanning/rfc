//! Intermediate representation for parsed RPL code.
//!
//! The IR is a tree structure representing parsed RPL code before lowering
//! to bytecode. It captures the essential structure while abstracting away
//! syntactic details.

use crate::symbolic::SymExpr;
use crate::core::{Span, Symbol};
use std::sync::Arc;

/// Library identifier (matches LibId in the plan).
pub type LibId = u16;

/// A node in the IR tree.
#[derive(Clone, Debug)]
pub struct Node {
    /// The kind of node.
    pub kind: NodeKind,
    /// Source location.
    pub span: Span,
}

impl Node {
    /// Create a new node.
    pub fn new(kind: NodeKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Create an integer literal node.
    pub fn integer(value: i64, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::Integer(value)), span)
    }

    /// Create a real literal node.
    pub fn real(value: f64, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::Real(value)), span)
    }

    /// Create a string literal node.
    pub fn string(value: impl Into<Arc<str>>, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::String(value.into())), span)
    }

    /// Create a symbol (unresolved name) node.
    pub fn symbol(sym: Symbol, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::Symbol(sym)), span)
    }

    /// Create a local variable reference node.
    pub fn local_ref(index: usize, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::LocalRef(index)), span)
    }

    /// Create a command reference node.
    pub fn command(lib: LibId, cmd: u16, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::Command(lib, cmd)), span)
    }

    /// Create a program node.
    pub fn program(body: Vec<Node>, span: Span) -> Self {
        Self::new(
            NodeKind::Composite(CompositeKind::Program, vec![body]),
            span,
        )
    }

    /// Create a list node.
    pub fn list(items: Vec<Node>, span: Span) -> Self {
        Self::new(NodeKind::Composite(CompositeKind::List, vec![items]), span)
    }

    /// Create a symbolic expression node.
    pub fn symbolic(expr: SymExpr, span: Span) -> Self {
        Self::new(NodeKind::Atom(AtomKind::Symbolic(Arc::new(expr))), span)
    }

    /// Create an extended composite node.
    pub fn extended(lib: LibId, construct_id: u16, branches: Vec<Branch>, span: Span) -> Self {
        Self::new(
            NodeKind::Composite(CompositeKind::Extended(lib, construct_id), branches),
            span,
        )
    }

    /// Check if this is an atom node.
    pub fn is_atom(&self) -> bool {
        matches!(self.kind, NodeKind::Atom(_))
    }

    /// Check if this is a composite node.
    pub fn is_composite(&self) -> bool {
        matches!(self.kind, NodeKind::Composite(_, _))
    }
}

/// The kind of an IR node.
#[derive(Clone, Debug)]
pub enum NodeKind {
    /// An atomic value (literal, reference, command).
    Atom(AtomKind),
    /// A composite structure with branches.
    Composite(CompositeKind, Vec<Branch>),
}

/// A branch is a sequence of nodes (used for construct bodies).
pub type Branch = Vec<Node>;

/// Kinds of atomic values.
#[derive(Clone, Debug)]
pub enum AtomKind {
    /// Integer literal.
    Integer(i64),
    /// Real (floating point) literal.
    Real(f64),
    /// String literal.
    String(Arc<str>),
    /// Unresolved symbol (for runtime lookup).
    Symbol(Symbol),
    /// Resolved local variable reference.
    LocalRef(usize),
    /// Resolved global variable reference.
    GlobalRef(Symbol),
    /// Resolved command reference.
    Command(LibId, u16),
    /// Symbolic expression (unevaluated algebraic expression).
    Symbolic(Arc<SymExpr>),
}

/// Kinds of composite structures.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompositeKind {
    /// Program: `« ... »` or `<< ... >>`
    Program,
    /// List: `{ ... }`
    List,
    /// Library-defined construct with ID (lib_id, construct_id).
    Extended(LibId, u16),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn dummy_span() -> Span {
        Span::new(Pos::new(0), Pos::new(1))
    }

    #[test]
    fn integer_node() {
        let node = Node::integer(42, dummy_span());
        assert!(node.is_atom());
        match node.kind {
            NodeKind::Atom(AtomKind::Integer(n)) => assert_eq!(n, 42),
            _ => panic!("expected integer"),
        }
    }

    #[test]
    fn program_node() {
        let body = vec![
            Node::integer(1, dummy_span()),
            Node::integer(2, dummy_span()),
        ];
        let node = Node::program(body, dummy_span());
        assert!(node.is_composite());
        match node.kind {
            NodeKind::Composite(CompositeKind::Program, branches) => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].len(), 2);
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn list_node() {
        let items = vec![
            Node::integer(1, dummy_span()),
            Node::integer(2, dummy_span()),
            Node::integer(3, dummy_span()),
        ];
        let node = Node::list(items, dummy_span());
        match node.kind {
            NodeKind::Composite(CompositeKind::List, branches) => {
                assert_eq!(branches.len(), 1);
                assert_eq!(branches[0].len(), 3);
            }
            _ => panic!("expected list"),
        }
    }
}
