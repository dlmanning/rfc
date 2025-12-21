//! Core type system for analysis.
//!
//! This module defines:
//! - `Type`: Compile-time type representation
//! - `Origin`: Where a stack value came from (parallel tracking)
//! - `Requirement`: What types are acceptable for a constraint
//! - `Constraint`: Type constraints collected during traversal

use smallvec::SmallVec;
use std::fmt;

use super::DefinitionId;
use crate::core::{Span, TypeId};

/// Type variable identifier for Hindley-Milner style inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "α{}", self.0)
    }
}

/// Compile-time type representation.
///
/// Provenance is tracked separately via `Origin` rather than being embedded
/// in the type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// A known concrete type.
    Known(TypeId),
    /// One of several possible types (union).
    OneOf(SmallVec<[TypeId; 4]>),
    /// A type variable placeholder (for HM inference).
    TypeVar(TypeVar),
    /// Type is completely unknown.
    Unknown,
}

impl Type {
    /// Create a known integer type.
    pub fn integer() -> Self {
        Type::Known(TypeId::BINT)
    }

    /// Create a known real type.
    pub fn real() -> Self {
        Type::Known(TypeId::REAL)
    }

    /// Create a known string type.
    pub fn string() -> Self {
        Type::Known(TypeId::STRING)
    }

    /// Create a known list type.
    pub fn list() -> Self {
        Type::Known(TypeId::LIST)
    }

    /// Create a known program type.
    pub fn program() -> Self {
        Type::Known(TypeId::PROGRAM)
    }

    /// Create a known symbolic type.
    pub fn symbolic() -> Self {
        Type::Known(TypeId::SYMBOLIC)
    }

    /// Create a numeric type (Int | Real).
    pub fn numeric() -> Self {
        Type::OneOf(smallvec::smallvec![TypeId::BINT, TypeId::REAL])
    }

    /// Check if this type is known to be a specific type.
    pub fn is_known(&self) -> bool {
        matches!(self, Type::Known(_))
    }

    /// Check if this type is unknown.
    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown)
    }

    /// Get the TypeId if this is a Known type.
    pub fn as_known(&self) -> Option<TypeId> {
        match self {
            Type::Known(t) => Some(*t),
            _ => None,
        }
    }

    /// Check if this is a numeric type.
    pub fn is_numeric(&self) -> bool {
        match self {
            Type::Known(t) => *t == TypeId::BINT || *t == TypeId::REAL,
            Type::OneOf(ts) => ts.iter().all(|t| *t == TypeId::BINT || *t == TypeId::REAL),
            _ => false,
        }
    }

    /// Check if this is known to be an integer.
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Known(t) if *t == TypeId::BINT)
    }

    /// Check if this is known to be a real.
    pub fn is_real(&self) -> bool {
        matches!(self, Type::Known(t) if *t == TypeId::REAL)
    }

    /// Check if this is a known program type.
    pub fn is_program(&self) -> bool {
        matches!(self, Type::Known(t) if *t == TypeId::PROGRAM)
    }

    /// Check if this is a type variable.
    pub fn is_type_var(&self) -> bool {
        matches!(self, Type::TypeVar(_))
    }

    /// Get the TypeVar if this is a type variable.
    pub fn as_type_var(&self) -> Option<TypeVar> {
        match self {
            Type::TypeVar(tv) => Some(*tv),
            _ => None,
        }
    }

    /// Join two types (least upper bound).
    ///
    /// Used when merging branches in control flow.
    pub fn join(&self, other: &Type) -> Type {
        if self == other {
            return self.clone();
        }

        match (self, other) {
            (Type::Unknown, t) | (t, Type::Unknown) => t.clone(),
            (Type::Known(a), Type::Known(b)) => {
                if a == b {
                    Type::Known(*a)
                } else {
                    Type::OneOf(smallvec::smallvec![*a, *b])
                }
            }
            (Type::Known(a), Type::OneOf(bs)) | (Type::OneOf(bs), Type::Known(a)) => {
                let mut result = bs.clone();
                if !result.contains(a) {
                    result.push(*a);
                }
                Type::OneOf(result)
            }
            (Type::OneOf(as_), Type::OneOf(bs)) => {
                let mut result = as_.clone();
                for b in bs {
                    if !result.contains(b) {
                        result.push(*b);
                    }
                }
                Type::OneOf(result)
            }
            // TypeVars need unification, which happens in resolution
            (Type::TypeVar(_), _) | (_, Type::TypeVar(_)) => Type::Unknown,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Known(t) => {
                if let Some(sym) = t.symbol() {
                    write!(f, "{}", sym)
                } else {
                    write!(f, "Type({})", t.as_u16())
                }
            }
            Type::OneOf(ts) => {
                let syms: Vec<_> = ts
                    .iter()
                    .map(|t| t.symbol().unwrap_or("?"))
                    .collect();
                write!(f, "{}", syms.join(" ∪ "))
            }
            Type::TypeVar(v) => write!(f, "{}", v),
            Type::Unknown => write!(f, "?"),
        }
    }
}

/// Origin tracking - where a stack value came from.
///
/// Origins are tracked in parallel with types, enabling constraint
/// propagation from any usage site back to the original binding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Origin {
    /// Value came from a literal in source code.
    Literal(Span),
    /// Value came from reading a local or global binding.
    Binding(DefinitionId),
    /// Value is a loop counter variable (FOR loops).
    LoopVar(DefinitionId),
    /// Value is the result of an operation or function call.
    Result(Span),
    /// Value could come from multiple sources (control flow merge).
    Phi(Vec<Origin>),
    /// Origin is unknown (e.g., dynamic operations).
    Unknown,
}

impl Origin {
    /// Get the definition ID if this origin is a binding or loop var.
    pub fn def_id(&self) -> Option<DefinitionId> {
        match self {
            Origin::Binding(id) | Origin::LoopVar(id) => Some(*id),
            _ => None,
        }
    }

    /// Collect all definition IDs from this origin (including Phi branches).
    pub fn all_def_ids(&self) -> Vec<DefinitionId> {
        match self {
            Origin::Binding(id) | Origin::LoopVar(id) => vec![*id],
            Origin::Phi(origins) => origins.iter().flat_map(|o| o.all_def_ids()).collect(),
            _ => vec![],
        }
    }

    /// Join two origins into a Phi node.
    pub fn join(a: Origin, b: Origin) -> Origin {
        if a == b {
            return a;
        }

        let mut origins = Vec::new();

        // Flatten nested Phis
        match a {
            Origin::Phi(os) => origins.extend(os),
            other => origins.push(other),
        }
        match b {
            Origin::Phi(os) => {
                for o in os {
                    if !origins.contains(&o) {
                        origins.push(o);
                    }
                }
            }
            other => {
                if !origins.contains(&other) {
                    origins.push(other);
                }
            }
        }

        if origins.len() == 1 {
            origins.pop().unwrap()
        } else {
            Origin::Phi(origins)
        }
    }
}

/// A requirement on what type(s) are acceptable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Requirement {
    /// Must be exactly this type.
    Exact(TypeId),
    /// Must be one of these types.
    OneOf(SmallVec<[TypeId; 4]>),
    /// Any type is acceptable.
    Any,
}

impl Requirement {
    /// Create a numeric requirement (Int | Real).
    pub fn numeric() -> Self {
        Requirement::OneOf(smallvec::smallvec![TypeId::BINT, TypeId::REAL])
    }

    /// Intersect two requirements.
    ///
    /// Returns None if the requirements conflict.
    pub fn intersect(&self, other: &Requirement) -> Option<Requirement> {
        match (self, other) {
            (Requirement::Any, r) | (r, Requirement::Any) => Some(r.clone()),
            (Requirement::Exact(a), Requirement::Exact(b)) => {
                if a == b {
                    Some(Requirement::Exact(*a))
                } else {
                    None // Conflict
                }
            }
            (Requirement::Exact(a), Requirement::OneOf(bs))
            | (Requirement::OneOf(bs), Requirement::Exact(a)) => {
                if bs.contains(a) {
                    Some(Requirement::Exact(*a))
                } else {
                    None // Conflict
                }
            }
            (Requirement::OneOf(as_), Requirement::OneOf(bs)) => {
                let intersection: SmallVec<[TypeId; 4]> =
                    as_.iter().filter(|a| bs.contains(a)).copied().collect();

                if intersection.is_empty() {
                    None // Conflict
                } else if intersection.len() == 1 {
                    Some(Requirement::Exact(intersection[0]))
                } else {
                    Some(Requirement::OneOf(intersection))
                }
            }
        }
    }

    /// Convert this requirement to a Type.
    pub fn as_type(&self) -> Type {
        match self {
            Requirement::Exact(t) => Type::Known(*t),
            Requirement::OneOf(ts) => Type::OneOf(ts.clone()),
            Requirement::Any => Type::Unknown,
        }
    }
}

impl fmt::Display for Requirement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Requirement::Exact(t) => {
                if let Some(sym) = t.symbol() {
                    write!(f, "{}", sym)
                } else {
                    write!(f, "Type({})", t.as_u16())
                }
            }
            Requirement::OneOf(ts) => {
                let syms: Vec<_> = ts
                    .iter()
                    .map(|t| t.symbol().unwrap_or("?"))
                    .collect();
                write!(f, "{}", syms.join(" ∪ "))
            }
            Requirement::Any => write!(f, "∀"),
        }
    }
}

/// A constraint collected during traversal to be solved in Phase 4.
#[derive(Clone, Debug)]
pub enum Constraint {
    /// Definition must satisfy this requirement (from usage).
    /// These constraints are INTERSECTED - the type must satisfy ALL usage requirements.
    MustBe {
        def_id: DefinitionId,
        requirement: Requirement,
        span: Span,
        operation: String,
    },
    /// Definition is called with this type at a call site.
    /// These constraints are UNIONED - the type must accept ANY of the call site types.
    CalledWith {
        def_id: DefinitionId,
        type_id: TypeId,
        span: Span,
    },
    /// Two definitions must have the same type (for HM inference).
    Equal {
        def_a: DefinitionId,
        def_b: DefinitionId,
        span: Span,
    },
}

impl Constraint {
    /// Create a MustBe constraint.
    pub fn must_be(
        def_id: DefinitionId,
        requirement: Requirement,
        span: Span,
        operation: impl Into<String>,
    ) -> Self {
        Constraint::MustBe {
            def_id,
            requirement,
            span,
            operation: operation.into(),
        }
    }

    /// Create a CalledWith constraint (for function call sites).
    pub fn called_with(def_id: DefinitionId, type_id: TypeId, span: Span) -> Self {
        Constraint::CalledWith {
            def_id,
            type_id,
            span,
        }
    }

    /// Create an Equal constraint.
    pub fn equal(def_a: DefinitionId, def_b: DefinitionId, span: Span) -> Self {
        Constraint::Equal { def_a, def_b, span }
    }
}

/// Snapshot of stack state at a specific node.
///
/// Used to cache analysis results for lowering and incremental updates.
/// Contains just the information the lowerer needs for code generation decisions.
#[derive(Clone, Debug)]
pub struct StackSnapshot {
    /// Type at top of stack (TOS).
    pub tos: Type,
    /// Type at next-on-stack (NOS) - second item.
    pub nos: Type,
    /// Known stack depth.
    pub depth: usize,
    /// Whether the depth is known at compile time.
    pub depth_known: bool,
}

impl StackSnapshot {
    /// Create a snapshot with all unknown types.
    pub fn unknown() -> Self {
        Self {
            tos: Type::Unknown,
            nos: Type::Unknown,
            depth: 0,
            depth_known: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_join() {
        let int = Type::Known(TypeId::BINT);
        let real = Type::Known(TypeId::REAL);

        // Same types
        assert_eq!(int.join(&int), int);

        // Different types form union
        let joined = int.join(&real);
        assert!(matches!(joined, Type::OneOf(_)));

        // Unknown is absorbed
        assert_eq!(int.join(&Type::Unknown), int);
        assert_eq!(Type::Unknown.join(&real), real);
    }

    #[test]
    fn requirement_intersect() {
        let int = Requirement::Exact(TypeId::BINT);
        let numeric = Requirement::numeric();

        // Exact with matching OneOf narrows to Exact
        assert_eq!(int.intersect(&numeric), Some(int.clone()));

        // Exact with non-matching fails
        let string = Requirement::Exact(TypeId::STRING);
        assert_eq!(int.intersect(&string), None);

        // Any is identity
        assert_eq!(int.intersect(&Requirement::Any), Some(int.clone()));
    }

    #[test]
    fn origin_join() {
        let lit = Origin::Literal(Span::DUMMY);
        let binding = Origin::Binding(DefinitionId::new(1));

        // Same origin returns same
        assert_eq!(Origin::join(lit.clone(), lit.clone()), lit);

        // Different origins form Phi
        let joined = Origin::join(lit.clone(), binding.clone());
        assert!(matches!(joined, Origin::Phi(_)));

        if let Origin::Phi(origins) = joined {
            assert_eq!(origins.len(), 2);
        }
    }
}
