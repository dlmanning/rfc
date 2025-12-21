//! Type system utilities.
//!
//! This module provides:
//! - Program signatures for function type tracking
//! - Type constraints for constraint-based inference

use crate::analysis::DefinitionId;
use crate::analysis::Type;
use crate::core::{Span, TypeId};
use smallvec::{smallvec, SmallVec};

// ============================================================================
// Program Signatures
// ============================================================================

/// Inferred signature for a program/function.
///
/// Represents the input types (parameters) and output types (return values)
/// of a user-defined program with explicit local bindings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    /// Input types (from local parameter constraints).
    pub inputs: Vec<Type>,
    /// Output types (from final stack state).
    pub outputs: Vec<Type>,
    /// Parameter definition IDs for refreshing types after constraint resolution.
    /// Stored so we can look up the narrowed types after analysis completes.
    pub param_def_ids: Vec<DefinitionId>,
}

impl Signature {
    /// Create a new signature.
    pub fn new(inputs: Vec<Type>, outputs: Vec<Type>) -> Self {
        Self {
            inputs,
            outputs,
            param_def_ids: Vec::new(),
        }
    }

    /// Create a new signature with parameter definition IDs for later refresh.
    pub fn with_param_def_ids(
        inputs: Vec<Type>,
        outputs: Vec<Type>,
        param_def_ids: Vec<DefinitionId>,
    ) -> Self {
        Self {
            inputs,
            outputs,
            param_def_ids,
        }
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, ty) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, " → ")?;
        for (i, ty) in self.outputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, ")")
    }
}

// ============================================================================
// Type Constraints (for constraint-based inference)
// ============================================================================

/// A constraint on a type, derived from how a value is used.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeConstraint {
    /// Must be exactly this type.
    Exact(TypeId),
    /// Must be one of these types (e.g., Int | Real for numeric operations).
    OneOf(SmallVec<[TypeId; 4]>),
    /// No constraint (dynamic operation or type variable).
    Any,
    /// Must have the same type as another definition.
    /// Used for Hindley-Milner style inference to link call arguments to callee parameters.
    SameAs(DefinitionId),
}

impl TypeConstraint {
    /// Create a numeric constraint (Int | Real).
    pub fn numeric() -> Self {
        TypeConstraint::OneOf(smallvec![TypeId::BINT, TypeId::REAL])
    }

    /// Create an exact type constraint.
    pub fn exact(type_id: TypeId) -> Self {
        TypeConstraint::Exact(type_id)
    }

    /// Get the set of TypeIds this constraint allows.
    ///
    /// Returns None if the constraint allows any type (Any).
    pub fn allowed_types(&self) -> Option<SmallVec<[TypeId; 4]>> {
        match self {
            TypeConstraint::Exact(t) => {
                let mut types = SmallVec::new();
                types.push(*t);
                Some(types)
            }
            TypeConstraint::OneOf(ts) => Some(ts.clone()),
            TypeConstraint::Any => None,
            // SameAs doesn't have a concrete type set - resolved separately
            TypeConstraint::SameAs(_) => None,
        }
    }

    /// Check if this constraint allows numeric types (Int or Real).
    pub fn is_numeric(&self) -> bool {
        match self {
            TypeConstraint::Exact(t) => *t == TypeId::BINT || *t == TypeId::REAL,
            TypeConstraint::OneOf(ts) => {
                !ts.is_empty() && ts.iter().all(|t| *t == TypeId::BINT || *t == TypeId::REAL)
            }
            TypeConstraint::Any => true,
            TypeConstraint::SameAs(_) => true, // Unknown, assume it could be
        }
    }
}

impl std::fmt::Display for TypeConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeConstraint::Exact(t) => {
                if let Some(sym) = t.symbol() {
                    write!(f, "{}", sym)
                } else {
                    write!(f, "type({})", t.as_u16())
                }
            }
            TypeConstraint::OneOf(ts) => {
                let syms: Vec<_> = ts.iter().filter_map(|t| t.symbol()).collect();
                write!(f, "{}", syms.join(" ∪ "))
            }
            TypeConstraint::Any => write!(f, "∀"),
            TypeConstraint::SameAs(def_id) => write!(f, "≡def#{}", def_id.as_u32()),
        }
    }
}

/// Source of a type constraint (for error reporting).
#[derive(Clone, Debug)]
pub struct ConstraintSource {
    /// The span where the constraint was generated.
    pub span: Span,
    /// The operation that generated the constraint (e.g., "SIN").
    pub operation: String,
    /// The expected type(s).
    pub expected: TypeConstraint,
}

/// Result of unifying two constraints.
#[derive(Clone, Debug)]
pub struct TypeConflict {
    pub first: TypeConstraint,
    pub second: TypeConstraint,
}

impl std::fmt::Display for TypeConflict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} vs {}", self.first, self.second)
    }
}

/// Unify two type constraints using intersection semantics.
///
/// Returns the most specific compatible constraint, or an error if the
/// constraints are incompatible (empty intersection).
pub fn unify_constraints(
    a: &TypeConstraint,
    b: &TypeConstraint,
) -> Result<TypeConstraint, TypeConflict> {
    match (a, b) {
        // Any unifies with anything
        (TypeConstraint::Any, other) | (other, TypeConstraint::Any) => Ok(other.clone()),

        // Exact types: must be the same (intersection semantics)
        (TypeConstraint::Exact(t1), TypeConstraint::Exact(t2)) => {
            if t1 == t2 {
                Ok(TypeConstraint::Exact(*t1))
            } else {
                // Different exact types have empty intersection - conflict
                Err(TypeConflict {
                    first: a.clone(),
                    second: b.clone(),
                })
            }
        }

        // Exact vs OneOf: type must be in the set (intersection)
        (TypeConstraint::Exact(t), TypeConstraint::OneOf(ts))
        | (TypeConstraint::OneOf(ts), TypeConstraint::Exact(t)) => {
            if ts.contains(t) {
                Ok(TypeConstraint::Exact(*t))
            } else {
                Err(TypeConflict {
                    first: a.clone(),
                    second: b.clone(),
                })
            }
        }

        // OneOf intersection
        (TypeConstraint::OneOf(ts1), TypeConstraint::OneOf(ts2)) => {
            let intersection: SmallVec<_> =
                ts1.iter().filter(|t| ts2.contains(t)).copied().collect();

            if intersection.is_empty() {
                Err(TypeConflict {
                    first: a.clone(),
                    second: b.clone(),
                })
            } else if intersection.len() == 1 {
                Ok(TypeConstraint::Exact(intersection[0]))
            } else {
                Ok(TypeConstraint::OneOf(intersection))
            }
        }

        // SameAs constraints are resolved in a separate pass (resolve_sameas_constraints).
        // During normal unification, we defer by returning the non-SameAs constraint.
        (TypeConstraint::SameAs(_), other) | (other, TypeConstraint::SameAs(_)) => {
            Ok(other.clone())
        }
    }
}
