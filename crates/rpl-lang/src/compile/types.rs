use rpl_core::TypeId;
use smallvec::SmallVec;

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
}

/// Compile-time stack for type tracking.
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
    fn ctype_join_oneof_known() {
        let mut types = SmallVec::new();
        types.push(TypeId::REAL);
        let a = CType::OneOf(types);
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
    fn cstack_new() {
        let stack = CStack::new();
        assert_eq!(stack.depth(), 0);
        assert!(!stack.has_unknown_depth());
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
    fn cstack_join_different_types() {
        let mut a = CStack::new();
        a.push_known(TypeId::REAL);

        let mut b = CStack::new();
        b.push_known(TypeId::STRING);

        let joined = a.join(&b);
        assert_eq!(joined.depth(), 1);
        match joined.at(0) {
            CType::OneOf(types) => {
                assert!(types.contains(&TypeId::REAL));
                assert!(types.contains(&TypeId::STRING));
            }
            _ => panic!("Expected OneOf"),
        }
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
}
