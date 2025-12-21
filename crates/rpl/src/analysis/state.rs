//! Stack state and traversal context for analyzer-v2.
//!
//! This module provides:
//! - `StackState`: Parallel tracking of types and origins on the abstract stack
//! - `Substitution`: Type variable substitution map
//! - `Context`: Traversal context carrying state through analysis

use std::collections::HashMap;

use super::ScopeId;
use crate::core::{Interner, Span};
use crate::libs::StackEffect;
use crate::registry::InterfaceRegistry;

use super::types::{Constraint, Origin, Type, TypeVar};

/// Parallel tracking of types and origins on the abstract stack.
///
/// Keeps types and origins in separate parallel vectors, enabling cleaner
/// separation of concerns and easier Phi node creation.
#[derive(Clone, Debug)]
pub struct StackState {
    /// Type at each stack position (bottom to top).
    types: Vec<Type>,
    /// Origin at each stack position (parallel to types).
    origins: Vec<Origin>,
    /// Whether the stack depth is known at compile time.
    pub depth_known: bool,
    /// Number of underflow attempts (items popped when stack was empty).
    underflow_count: usize,
}

impl Default for StackState {
    fn default() -> Self {
        Self::new()
    }
}

impl StackState {
    /// Create a new empty stack state.
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            origins: Vec::new(),
            depth_known: true,
            underflow_count: 0,
        }
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, ty: Type, origin: Origin) {
        self.types.push(ty);
        self.origins.push(origin);
    }

    /// Pop a value from the stack.
    ///
    /// Returns (Type::Unknown, Origin::Unknown) if stack is empty.
    pub fn pop(&mut self) -> (Type, Origin) {
        match (self.types.pop(), self.origins.pop()) {
            (Some(ty), Some(origin)) => (ty, origin),
            _ => {
                self.underflow_count += 1;
                (Type::Unknown, Origin::Unknown)
            }
        }
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.types.len()
    }

    /// Check if stack is empty.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    /// Get type at position from top (0 = top of stack).
    pub fn type_at(&self, from_top: usize) -> Type {
        if from_top < self.types.len() {
            self.types[self.types.len() - 1 - from_top].clone()
        } else {
            Type::Unknown
        }
    }

    /// Get origin at position from top (0 = top of stack).
    pub fn origin_at(&self, from_top: usize) -> Origin {
        if from_top < self.origins.len() {
            self.origins[self.origins.len() - 1 - from_top].clone()
        } else {
            Origin::Unknown
        }
    }

    /// Get type and origin at position from top.
    pub fn at(&self, from_top: usize) -> (Type, Origin) {
        (self.type_at(from_top), self.origin_at(from_top))
    }

    /// Clear the stack.
    pub fn clear(&mut self) {
        self.types.clear();
        self.origins.clear();
    }

    /// Take and reset the underflow count.
    pub fn take_underflow(&mut self) -> Option<usize> {
        if self.underflow_count > 0 {
            let count = self.underflow_count;
            self.underflow_count = 0;
            Some(count)
        } else {
            None
        }
    }

    /// Merge with another stack state (for control flow joins).
    ///
    /// Uses join semantics: types are joined, origins become Phi nodes.
    pub fn merge(&self, other: &StackState) -> StackState {
        // If either has unknown depth, result has unknown depth
        if !self.depth_known || !other.depth_known {
            return StackState {
                types: Vec::new(),
                origins: Vec::new(),
                depth_known: false,
                underflow_count: 0,
            };
        }

        // Use minimum depth
        let min_depth = self.types.len().min(other.types.len());

        let mut types = Vec::with_capacity(min_depth);
        let mut origins = Vec::with_capacity(min_depth);

        // Merge from bottom of stack
        for i in 0..min_depth {
            types.push(self.types[i].join(&other.types[i]));
            origins.push(Origin::join(
                self.origins[i].clone(),
                other.origins[i].clone(),
            ));
        }

        StackState {
            types,
            origins,
            depth_known: true,
            underflow_count: 0,
        }
    }

    /// Merge multiple stack states.
    pub fn merge_all(states: &[StackState]) -> StackState {
        if states.is_empty() {
            return StackState::new();
        }
        if states.len() == 1 {
            return states[0].clone();
        }

        // Partition by depth_known
        let known: Vec<_> = states.iter().filter(|s| s.depth_known).collect();

        if known.is_empty() {
            return StackState {
                types: Vec::new(),
                origins: Vec::new(),
                depth_known: false,
                underflow_count: 0,
            };
        }

        // Start with first known state
        let mut result = known[0].clone();

        // Merge remaining
        for state in &known[1..] {
            result = result.merge(state);
        }

        // If any state had unknown depth, propagate that
        result.depth_known = states.iter().all(|s| s.depth_known);

        result
    }

    /// Apply a stack effect to this state.
    pub fn apply_effect(&mut self, effect: &StackEffect, span: Span) {
        match effect {
            StackEffect::Fixed { consumes, results } => {
                let consumes = *consumes as usize;

                // Collect input types and origins before popping (for FromInput references)
                let mut input_items: Vec<(Type, Origin)> = Vec::with_capacity(consumes);
                for i in (0..consumes).rev() {
                    input_items.push((self.type_at(i), self.origin_at(i)));
                }

                // Pop consumed items
                for _ in 0..consumes {
                    self.pop();
                }

                // Push results
                for result in results.iter() {
                    let (ty, origin) = match result {
                        crate::libs::ResultType::Known(t) => {
                            (Type::Known(*t), Origin::Result(span))
                        }
                        crate::libs::ResultType::OneOf(ts) => {
                            // Convert SmallVec<[TypeId; 2]> to SmallVec<[TypeId; 4]>
                            (Type::OneOf(ts.iter().copied().collect()), Origin::Result(span))
                        }
                        crate::libs::ResultType::Unknown => {
                            (Type::Unknown, Origin::Result(span))
                        }
                        crate::libs::ResultType::FromInput(i) => {
                            // Preserve both type AND origin from input
                            input_items.get(*i as usize)
                                .cloned()
                                .unwrap_or((Type::Unknown, Origin::Unknown))
                        }
                    };
                    self.push(ty, origin);
                }
            }
            StackEffect::Dynamic => {
                // Unknown effect - clear stack and mark depth unknown
                self.clear();
                self.depth_known = false;
            }
        }
    }

    /// Duplicate top of stack (DUP operation).
    pub fn dup(&mut self) {
        if let Some(ty) = self.types.last().cloned() {
            let origin = self.origins.last().cloned().unwrap_or(Origin::Unknown);
            self.push(ty, origin);
        } else {
            self.underflow_count += 1;
            self.push(Type::Unknown, Origin::Unknown);
        }
    }

    /// Swap top two items (SWAP operation).
    pub fn swap(&mut self) {
        let len = self.types.len();
        if len >= 2 {
            self.types.swap(len - 1, len - 2);
            self.origins.swap(len - 1, len - 2);
        } else {
            self.underflow_count += 2 - len;
        }
    }

    /// Drop top item (DROP operation).
    pub fn drop_top(&mut self) {
        self.pop();
    }
}

/// Substitution map for type variables.
#[derive(Clone, Debug, Default)]
pub struct Substitution {
    map: HashMap<TypeVar, Type>,
}

impl Substitution {
    /// Create a new empty substitution.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a binding for a type variable.
    pub fn insert(&mut self, var: TypeVar, ty: Type) {
        self.map.insert(var, ty);
    }

    /// Get the binding for a type variable.
    pub fn get(&self, var: TypeVar) -> Option<&Type> {
        self.map.get(&var)
    }

    /// Apply substitution to a type, resolving type variables.
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::TypeVar(var) => {
                if let Some(bound) = self.get(*var) {
                    // Recursive resolution
                    self.apply(bound)
                } else {
                    ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }

    /// Unify a type variable with a type.
    ///
    /// If the variable is already bound, the types are joined.
    pub fn unify(&mut self, var: TypeVar, ty: Type) {
        if let Some(existing) = self.get(var).cloned() {
            // Already bound - join with new type
            self.insert(var, existing.join(&ty));
        } else {
            self.insert(var, ty);
        }
    }
}

/// Traversal context carrying state through analysis.
///
/// This is passed through the visitor methods during Phase 3.
pub struct Context<'a> {
    /// Registry for querying effects and bindings.
    pub registry: &'a InterfaceRegistry,
    /// Interner for symbol resolution.
    pub interner: &'a Interner,
    /// Current scope ID.
    pub current_scope: ScopeId,
    /// Abstract stack state with types and origins.
    pub stack: StackState,
    /// Saved stacks for nested programs.
    pub saved_stacks: Vec<StackState>,
    /// Collected constraints to solve in Phase 4.
    pub constraints: Vec<Constraint>,
    /// Type variable substitutions.
    pub substitution: Substitution,
    /// Counter for generating fresh type variables.
    next_type_var: u32,
}

impl<'a> Context<'a> {
    /// Create a new context.
    pub fn new(registry: &'a InterfaceRegistry, interner: &'a Interner) -> Self {
        Self {
            registry,
            interner,
            current_scope: ScopeId::root(),
            stack: StackState::new(),
            saved_stacks: Vec::new(),
            constraints: Vec::new(),
            substitution: Substitution::new(),
            next_type_var: 0,
        }
    }

    /// Generate a fresh type variable.
    pub fn fresh_type_var(&mut self) -> TypeVar {
        let var = TypeVar(self.next_type_var);
        self.next_type_var += 1;
        var
    }

    /// Get the current type variable counter (for resuming after global collection).
    pub fn type_var_counter(&self) -> u32 {
        self.next_type_var
    }

    /// Set the type variable counter (after global collection).
    pub fn set_type_var_counter(&mut self, counter: u32) {
        self.next_type_var = counter;
    }

    /// Add a constraint to be solved in Phase 4.
    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }

    /// Save the current stack state (entering a program body).
    pub fn save_stack(&mut self) {
        self.saved_stacks.push(std::mem::take(&mut self.stack));
        self.stack = StackState::new();
    }

    /// Restore the saved stack state (exiting a program body).
    pub fn restore_stack(&mut self) -> Option<StackState> {
        self.saved_stacks.pop().map(|saved| {
            std::mem::replace(&mut self.stack, saved)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::TypeId;

    #[test]
    fn stack_push_pop() {
        let mut stack = StackState::new();

        stack.push(Type::Known(TypeId::BINT), Origin::Literal(Span::DUMMY));
        stack.push(Type::Known(TypeId::REAL), Origin::Literal(Span::DUMMY));

        assert_eq!(stack.depth(), 2);

        let (ty, _) = stack.pop();
        assert_eq!(ty, Type::Known(TypeId::REAL));

        let (ty, _) = stack.pop();
        assert_eq!(ty, Type::Known(TypeId::BINT));

        assert!(stack.is_empty());
    }

    #[test]
    fn stack_merge() {
        let mut s1 = StackState::new();
        s1.push(Type::Known(TypeId::BINT), Origin::Literal(Span::DUMMY));

        let mut s2 = StackState::new();
        s2.push(Type::Known(TypeId::REAL), Origin::Literal(Span::DUMMY));

        let merged = s1.merge(&s2);
        assert_eq!(merged.depth(), 1);

        // Types should be joined
        let ty = merged.type_at(0);
        assert!(matches!(ty, Type::OneOf(_)));
    }

    #[test]
    fn stack_underflow() {
        let mut stack = StackState::new();

        // Pop from empty stack
        let (ty, _) = stack.pop();
        assert_eq!(ty, Type::Unknown);

        // Underflow should be recorded
        assert_eq!(stack.take_underflow(), Some(1));

        // Second take should return None
        assert_eq!(stack.take_underflow(), None);
    }

    #[test]
    fn substitution_apply() {
        let mut subst = Substitution::new();
        let var = TypeVar(0);

        subst.insert(var, Type::Known(TypeId::BINT));

        let resolved = subst.apply(&Type::TypeVar(var));
        assert_eq!(resolved, Type::Known(TypeId::BINT));
    }
}
