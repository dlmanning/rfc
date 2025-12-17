//! Compile-time type inference.
//!
//! This module provides type tracking during compilation, enabling:
//! - Emitting typed WASM opcodes (I64Add vs F64Add)
//! - Type error detection at compile time
//! - Optimization based on known types
//!
//! Adapted from rpl-lang/src/compile/types.rs

use crate::core::TypeId;
use smallvec::SmallVec;

use crate::libs::StackEffect;

/// Compile-time type representation.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum CType {
    /// A known concrete type.
    Known(TypeId),
    /// One of several possible types.
    OneOf(SmallVec<[TypeId; 4]>),
    /// Type is unknown (could be anything).
    #[default]
    Unknown,
    /// Unreachable code path (e.g., after error).
    Never,
}

impl CType {
    /// Create a known integer type.
    pub fn integer() -> Self {
        CType::Known(TypeId::BINT)
    }

    /// Create a known real type.
    pub fn real() -> Self {
        CType::Known(TypeId::REAL)
    }

    /// Create a known string type.
    pub fn string() -> Self {
        CType::Known(TypeId::STRING)
    }

    /// Create a known list type.
    pub fn list() -> Self {
        CType::Known(TypeId::LIST)
    }

    /// Create a known program type.
    pub fn program() -> Self {
        CType::Known(TypeId::PROGRAM)
    }

    /// Create a known symbolic type.
    pub fn symbolic() -> Self {
        CType::Known(TypeId::SYMBOLIC)
    }

    /// Join two types (for control flow merge).
    /// Returns the most specific common type.
    pub fn join(&self, other: &CType) -> CType {
        match (self, other) {
            // Never joins to the other type
            (CType::Never, other) => other.clone(),
            (other, CType::Never) => other.clone(),

            // Unknown joins to Unknown
            (CType::Unknown, _) | (_, CType::Unknown) => CType::Unknown,

            // Same known type stays known
            (CType::Known(a), CType::Known(b)) if a == b => CType::Known(*a),

            // Different known types become OneOf
            (CType::Known(a), CType::Known(b)) => {
                let mut types = SmallVec::new();
                types.push(*a);
                types.push(*b);
                CType::OneOf(types)
            }

            // Known + OneOf: add to the set if not present
            (CType::Known(a), CType::OneOf(types)) | (CType::OneOf(types), CType::Known(a)) => {
                let mut new_types = types.clone();
                if !new_types.contains(a) {
                    new_types.push(*a);
                }
                CType::OneOf(new_types)
            }

            // OneOf + OneOf: merge sets
            (CType::OneOf(a), CType::OneOf(b)) => {
                let mut new_types = a.clone();
                for t in b {
                    if !new_types.contains(t) {
                        new_types.push(*t);
                    }
                }
                CType::OneOf(new_types)
            }
        }
    }

    /// Get the type if it's a known concrete type.
    pub fn as_known(&self) -> Option<TypeId> {
        match self {
            CType::Known(t) => Some(*t),
            _ => None,
        }
    }

    /// Check if this is a known type.
    pub fn is_known(&self) -> bool {
        matches!(self, CType::Known(_))
    }

    /// Check if this is unknown.
    pub fn is_unknown(&self) -> bool {
        matches!(self, CType::Unknown)
    }

    /// Check if this is a known integer type.
    pub fn is_integer(&self) -> bool {
        matches!(self, CType::Known(t) if *t == TypeId::BINT)
    }

    /// Check if this is a known real type.
    pub fn is_real(&self) -> bool {
        matches!(self, CType::Known(t) if *t == TypeId::REAL)
    }

    /// Check if this is a known numeric type (integer or real).
    pub fn is_numeric(&self) -> bool {
        matches!(self, CType::Known(t) if *t == TypeId::BINT || *t == TypeId::REAL)
    }

    /// Check if this is a known program type.
    pub fn is_program(&self) -> bool {
        matches!(self, CType::Known(t) if *t == TypeId::PROGRAM)
    }
}

/// Compile-time stack for type tracking.
#[derive(Clone)]
pub struct CStack {
    slots: Vec<CType>,
    unknown_depth: bool,
}

impl CStack {
    /// Create a new empty compile-time stack.
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            unknown_depth: false,
        }
    }

    /// Push a type onto the stack.
    pub fn push(&mut self, ctype: CType) {
        self.slots.push(ctype);
    }

    /// Push a known type onto the stack.
    pub fn push_known(&mut self, type_id: TypeId) {
        self.slots.push(CType::Known(type_id));
    }

    /// Pop a type from the stack.
    /// Returns Unknown if stack is empty or has unknown depth.
    pub fn pop(&mut self) -> CType {
        if self.unknown_depth && self.slots.is_empty() {
            CType::Unknown
        } else {
            self.slots.pop().unwrap_or(CType::Unknown)
        }
    }

    /// Get the type at a given depth (0 = top).
    pub fn at(&self, depth: usize) -> CType {
        if depth < self.slots.len() {
            self.slots[self.slots.len() - 1 - depth].clone()
        } else {
            // Beyond known stack - return Unknown regardless of unknown_depth flag
            CType::Unknown
        }
    }

    /// Get the type at the top of stack without popping.
    pub fn top(&self) -> CType {
        self.at(0)
    }

    /// Get the type of the second item (next on stack).
    pub fn nos(&self) -> CType {
        self.at(1)
    }

    /// Get the known stack depth.
    pub fn depth(&self) -> usize {
        self.slots.len()
    }

    /// Check if the stack has unknown depth.
    pub fn has_unknown_depth(&self) -> bool {
        self.unknown_depth
    }

    /// Mark the stack as having unknown depth.
    /// This happens after operations like CLEAR.
    pub fn mark_unknown_depth(&mut self) {
        self.unknown_depth = true;
    }

    /// Clear the stack.
    pub fn clear(&mut self) {
        self.slots.clear();
    }

    /// Replace this stack's state with another's.
    ///
    /// Used to restore state after processing a branch.
    pub fn replace_with(&mut self, other: CStack) {
        self.slots = other.slots;
        self.unknown_depth = other.unknown_depth;
    }

    /// Join with another stack (for control flow merge).
    pub fn join(&self, other: &CStack) -> CStack {
        let unknown_depth = self.unknown_depth || other.unknown_depth;

        // If depths differ, result has unknown depth
        if self.slots.len() != other.slots.len() {
            let mut result = CStack::new();
            result.unknown_depth = true;

            // Join what we can
            let min_depth = self.slots.len().min(other.slots.len());
            for i in 0..min_depth {
                let self_idx = self.slots.len() - min_depth + i;
                let other_idx = other.slots.len() - min_depth + i;
                result
                    .slots
                    .push(self.slots[self_idx].join(&other.slots[other_idx]));
            }
            result
        } else {
            // Same depth: join element by element
            let mut result = CStack::new();
            result.unknown_depth = unknown_depth;
            for (a, b) in self.slots.iter().zip(other.slots.iter()) {
                result.slots.push(a.join(b));
            }
            result
        }
    }

    /// Duplicate the top type (for DUP).
    pub fn dup(&mut self) {
        let top = self.top();
        self.push(top);
    }

    /// Swap the top two types (for SWAP).
    pub fn swap(&mut self) {
        if self.slots.len() >= 2 {
            let len = self.slots.len();
            self.slots.swap(len - 1, len - 2);
        }
    }

    /// Rotate top three types (for ROT).
    pub fn rot(&mut self) {
        if self.slots.len() >= 3 {
            let len = self.slots.len();
            let bottom = self.slots.remove(len - 3);
            self.slots.push(bottom);
        }
    }

    /// Copy second to top (for OVER).
    pub fn over(&mut self) {
        let nos = self.nos();
        self.push(nos);
    }

    /// Push depth as integer (for DEPTH).
    pub fn depth_op(&mut self) {
        self.push(CType::integer());
    }

    /// Apply PICK: consume n (integer), produce unknown type.
    /// We don't know which stack position will be copied.
    pub fn pick_op(&mut self) {
        self.pop(); // consume n
        self.push(CType::Unknown); // result type unknown
    }

    /// Apply ROLL: consume n (integer), mark stack types as unknown.
    /// The permutation depends on runtime value of n.
    pub fn roll_op(&mut self) {
        self.pop(); // consume n
        // The rest of the stack is permuted in unknown way
        // Mark all remaining slots as Unknown
        for slot in &mut self.slots {
            *slot = CType::Unknown;
        }
    }

    /// Apply a stack effect, updating types accordingly.
    ///
    /// For permutations, this preserves type information through the transformation.
    /// For fixed effects, this pops `consumes` items and pushes the declared result types.
    pub fn apply_effect(&mut self, effect: &StackEffect) {
        match effect {
            StackEffect::Permutation { inputs, pattern } => {
                // Collect input types (deepest first)
                let inputs = *inputs as usize;
                let mut input_types = Vec::with_capacity(inputs);
                for i in (0..inputs).rev() {
                    input_types.push(self.at(i));
                }

                // Pop inputs
                for _ in 0..inputs {
                    self.pop();
                }

                // Push outputs according to pattern
                for &src_idx in pattern.iter() {
                    let ty = input_types
                        .get(src_idx as usize)
                        .cloned()
                        .unwrap_or(CType::Unknown);
                    self.push(ty);
                }
            }
            StackEffect::Fixed { consumes, results } => {
                // Pop consumed items
                for _ in 0..*consumes {
                    self.pop();
                }
                // Push result types (Some(TypeId) -> Known, None -> Unknown)
                for type_id in results.iter() {
                    let ctype = match type_id {
                        Some(id) => CType::Known(*id),
                        None => CType::Unknown,
                    };
                    self.push(ctype);
                }
            }
            StackEffect::Dynamic => {
                // Dynamic effects make the stack depth unknown
                self.mark_unknown_depth();
            }
        }
    }
}

impl Default for CStack {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ctype_known() {
        let t = CType::Known(TypeId::REAL);
        assert!(t.is_known());
        assert!(!t.is_unknown());
        assert_eq!(t.as_known(), Some(TypeId::REAL));
    }

    #[test]
    fn ctype_unknown() {
        let t = CType::Unknown;
        assert!(!t.is_known());
        assert!(t.is_unknown());
        assert_eq!(t.as_known(), None);
    }

    #[test]
    fn ctype_join_same() {
        let a = CType::Known(TypeId::REAL);
        let b = CType::Known(TypeId::REAL);
        let joined = a.join(&b);
        assert_eq!(joined, CType::Known(TypeId::REAL));
    }

    #[test]
    fn ctype_join_different() {
        let a = CType::Known(TypeId::REAL);
        let b = CType::Known(TypeId::STRING);
        let joined = a.join(&b);
        match joined {
            CType::OneOf(types) => {
                assert!(types.contains(&TypeId::REAL));
                assert!(types.contains(&TypeId::STRING));
            }
            _ => panic!("Expected OneOf"),
        }
    }

    #[test]
    fn ctype_join_never() {
        let a = CType::Never;
        let b = CType::Known(TypeId::REAL);
        assert_eq!(a.join(&b), CType::Known(TypeId::REAL));
        assert_eq!(b.join(&a), CType::Known(TypeId::REAL));
    }

    #[test]
    fn ctype_join_unknown() {
        let a = CType::Unknown;
        let b = CType::Known(TypeId::REAL);
        assert_eq!(a.join(&b), CType::Unknown);
        assert_eq!(b.join(&a), CType::Unknown);
    }

    #[test]
    fn ctype_numeric_checks() {
        assert!(CType::integer().is_integer());
        assert!(CType::integer().is_numeric());
        assert!(!CType::integer().is_real());

        assert!(CType::real().is_real());
        assert!(CType::real().is_numeric());
        assert!(!CType::real().is_integer());

        assert!(!CType::string().is_numeric());
    }

    #[test]
    fn cstack_push_pop() {
        let mut stack = CStack::new();
        stack.push_known(TypeId::REAL);
        assert_eq!(stack.depth(), 1);

        let popped = stack.pop();
        assert_eq!(popped, CType::Known(TypeId::REAL));
        assert_eq!(stack.depth(), 0);
    }

    #[test]
    fn cstack_pop_empty() {
        let mut stack = CStack::new();
        let popped = stack.pop();
        assert_eq!(popped, CType::Unknown);
    }

    #[test]
    fn cstack_at() {
        let mut stack = CStack::new();
        stack.push_known(TypeId::REAL); // bottom
        stack.push_known(TypeId::STRING); // top

        assert_eq!(stack.at(0), CType::Known(TypeId::STRING)); // top
        assert_eq!(stack.at(1), CType::Known(TypeId::REAL)); // bottom
        assert_eq!(stack.at(2), CType::Unknown); // beyond
    }

    #[test]
    fn cstack_top_nos() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.push(CType::real());

        assert_eq!(stack.top(), CType::real());
        assert_eq!(stack.nos(), CType::integer());
    }

    #[test]
    fn cstack_dup() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.dup();

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.top(), CType::integer());
        assert_eq!(stack.nos(), CType::integer());
    }

    #[test]
    fn cstack_swap() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.push(CType::real());
        stack.swap();

        assert_eq!(stack.top(), CType::integer());
        assert_eq!(stack.nos(), CType::real());
    }

    #[test]
    fn cstack_unknown_depth() {
        let mut stack = CStack::new();
        stack.push_known(TypeId::REAL);
        stack.mark_unknown_depth();

        assert!(stack.has_unknown_depth());

        // Pop beyond known elements returns Unknown
        stack.pop();
        let beyond = stack.pop();
        assert_eq!(beyond, CType::Unknown);
    }

    #[test]
    fn cstack_join_same_depth() {
        let mut a = CStack::new();
        a.push_known(TypeId::REAL);

        let mut b = CStack::new();
        b.push_known(TypeId::REAL);

        let joined = a.join(&b);
        assert_eq!(joined.depth(), 1);
        assert_eq!(joined.at(0), CType::Known(TypeId::REAL));
    }

    #[test]
    fn cstack_join_different_depth() {
        let mut a = CStack::new();
        a.push_known(TypeId::REAL);
        a.push_known(TypeId::STRING);

        let mut b = CStack::new();
        b.push_known(TypeId::REAL);

        let joined = a.join(&b);
        assert!(joined.has_unknown_depth());
    }

    #[test]
    fn apply_effect_dup() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.apply_effect(&StackEffect::dup());

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.at(0), CType::integer()); // top
        assert_eq!(stack.at(1), CType::integer()); // second
    }

    #[test]
    fn apply_effect_drop() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.push(CType::real());
        stack.apply_effect(&StackEffect::drop());

        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.at(0), CType::integer());
    }

    #[test]
    fn apply_effect_swap() {
        let mut stack = CStack::new();
        stack.push(CType::integer()); // bottom
        stack.push(CType::real()); // top
        stack.apply_effect(&StackEffect::swap());

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.at(0), CType::integer()); // top after swap
        assert_eq!(stack.at(1), CType::real()); // bottom after swap
    }

    #[test]
    fn apply_effect_rot() {
        let mut stack = CStack::new();
        stack.push(CType::integer()); // a (deepest)
        stack.push(CType::real()); // b
        stack.push(CType::string()); // c (top)
        // (a b c -- b c a)
        stack.apply_effect(&StackEffect::rot());

        assert_eq!(stack.depth(), 3);
        assert_eq!(stack.at(0), CType::integer()); // a now on top
        assert_eq!(stack.at(1), CType::string()); // c
        assert_eq!(stack.at(2), CType::real()); // b at bottom
    }

    #[test]
    fn apply_effect_over() {
        let mut stack = CStack::new();
        stack.push(CType::integer()); // a
        stack.push(CType::real()); // b (top)
        // (a b -- a b a)
        stack.apply_effect(&StackEffect::over());

        assert_eq!(stack.depth(), 3);
        assert_eq!(stack.at(0), CType::integer()); // a (top, copied)
        assert_eq!(stack.at(1), CType::real()); // b
        assert_eq!(stack.at(2), CType::integer()); // a (original)
    }

    #[test]
    fn apply_effect_fixed_unknown() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.push(CType::integer());
        // Fixed effect with unknown result type (None)
        stack.apply_effect(&StackEffect::fixed(2, &[None]));

        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.at(0), CType::Unknown);
    }

    #[test]
    fn apply_effect_fixed_known_result() {
        let mut stack = CStack::new();
        stack.push(CType::integer());
        stack.push(CType::integer());
        // Fixed effect with known integer result
        stack.apply_effect(&StackEffect::fixed(2, &[Some(TypeId::BINT)]));

        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.at(0), CType::integer());
    }

    #[test]
    fn apply_effect_fixed_multiple_results() {
        let mut stack = CStack::new();
        stack.push(CType::list());
        // Effect that consumes 1 and produces 2 (e.g., imaginary decompose)
        stack.apply_effect(&StackEffect::fixed(1, &[Some(TypeId::REAL), Some(TypeId::REAL)]));

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.at(0), CType::real()); // top
        assert_eq!(stack.at(1), CType::real()); // second
    }

    #[test]
    fn depth_op_produces_integer() {
        let mut stack = CStack::new();
        stack.push(CType::real());
        stack.depth_op();

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.at(0), CType::integer()); // DEPTH result
        assert_eq!(stack.at(1), CType::real()); // original item unchanged
    }

    #[test]
    fn pick_op_consumes_n_produces_unknown() {
        let mut stack = CStack::new();
        stack.push(CType::real()); // some item
        stack.push(CType::string()); // another item
        stack.push(CType::integer()); // n (the index)
        stack.pick_op();

        assert_eq!(stack.depth(), 3);
        assert_eq!(stack.at(0), CType::Unknown); // picked value (unknown which)
        assert_eq!(stack.at(1), CType::string()); // unchanged
        assert_eq!(stack.at(2), CType::real()); // unchanged
    }

    #[test]
    fn roll_op_marks_stack_unknown() {
        let mut stack = CStack::new();
        stack.push(CType::real());
        stack.push(CType::string());
        stack.push(CType::integer()); // n
        stack.roll_op();

        assert_eq!(stack.depth(), 2);
        // All remaining items are now unknown since we don't know the permutation
        assert_eq!(stack.at(0), CType::Unknown);
        assert_eq!(stack.at(1), CType::Unknown);
    }
}
