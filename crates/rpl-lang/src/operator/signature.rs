use super::kind::OperatorKind;
use rpl_core::TypeId;
use crate::library::LibraryId;

/// Operator signature for dispatch.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OpSignature {
    /// Unary operator on a single type.
    Unary(TypeId),
    /// Binary operator on two operands of the same type.
    Symmetric(TypeId),
    /// Binary operator on two different types.
    Binary { left: TypeId, right: TypeId },
}

/// Registration for an operator implementation.
#[derive(Clone, Copy, Debug)]
pub struct OperatorRegistration {
    pub op: OperatorKind,
    pub signature: OpSignature,
    pub result: TypeId,
    pub lib: LibraryId,
    pub command: u16,
    pub priority: u8,
    pub commutative: bool,
}

/// Registration for a type coercion.
#[derive(Clone, Copy, Debug)]
pub struct CoercionRegistration {
    pub from: TypeId,
    pub to: TypeId,
    pub lib: LibraryId,
    pub command: u16,
    pub implicit: bool,
    pub priority: u8,
}

/// Resolved operator implementation.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Resolution {
    pub lib: LibraryId,
    pub command: u16,
    pub result_type: TypeId,
}

/// A path of coercions from one type to another.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CoercionPath {
    /// Each step: (target_type, lib, command)
    pub steps: Vec<(TypeId, LibraryId, u16)>,
    pub total_priority: u32,
}

impl CoercionPath {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            total_priority: 0,
        }
    }

    pub fn single(to: TypeId, lib: LibraryId, command: u16, priority: u8) -> Self {
        Self {
            steps: vec![(to, lib, command)],
            total_priority: priority as u32,
        }
    }

    pub fn extend(&self, to: TypeId, lib: LibraryId, command: u16, priority: u8) -> Self {
        let mut steps = self.steps.clone();
        steps.push((to, lib, command));
        Self {
            steps,
            total_priority: self.total_priority + priority as u32,
        }
    }

    pub fn len(&self) -> usize {
        self.steps.len()
    }

    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }
}

impl Default for CoercionPath {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn op_signature_unary() {
        let sig = OpSignature::Unary(TypeId::REAL);
        assert_eq!(sig, OpSignature::Unary(TypeId::REAL));
    }

    #[test]
    fn op_signature_symmetric() {
        let sig = OpSignature::Symmetric(TypeId::REAL);
        assert_eq!(sig, OpSignature::Symmetric(TypeId::REAL));
    }

    #[test]
    fn op_signature_binary() {
        let sig = OpSignature::Binary {
            left: TypeId::REAL,
            right: TypeId::BINT,
        };
        if let OpSignature::Binary { left, right } = sig {
            assert_eq!(left, TypeId::REAL);
            assert_eq!(right, TypeId::BINT);
        } else {
            panic!("expected Binary");
        }
    }

    #[test]
    fn coercion_path_single() {
        let path = CoercionPath::single(TypeId::REAL, LibraryId::new(10), 1, 5);
        assert_eq!(path.len(), 1);
        assert_eq!(path.total_priority, 5);
        assert_eq!(path.steps[0].0, TypeId::REAL);
    }

    #[test]
    fn coercion_path_extend() {
        let path1 = CoercionPath::single(TypeId::REAL, LibraryId::new(10), 1, 5);
        let path2 = path1.extend(TypeId::COMPLEX, LibraryId::new(10), 2, 3);

        assert_eq!(path2.len(), 2);
        assert_eq!(path2.total_priority, 8);
        assert_eq!(path2.steps[0].0, TypeId::REAL);
        assert_eq!(path2.steps[1].0, TypeId::COMPLEX);
    }

    #[test]
    fn resolution_equality() {
        let r1 = Resolution {
            lib: LibraryId::new(10),
            command: 5,
            result_type: TypeId::REAL,
        };
        let r2 = Resolution {
            lib: LibraryId::new(10),
            command: 5,
            result_type: TypeId::REAL,
        };
        assert_eq!(r1, r2);
    }
}
