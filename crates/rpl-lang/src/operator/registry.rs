use std::collections::{HashMap, HashSet};

use super::kind::OperatorKind;
use super::signature::{
    CoercionPath, CoercionRegistration, OpSignature, OperatorRegistration, Resolution,
};
use rpl_core::TypeId;
use crate::library::LibraryId;

/// Registry for operator implementations and type coercions.
#[derive(Debug, Default)]
pub struct OperatorRegistry {
    /// Unary operators: (op, type) -> resolution
    unary: HashMap<(OperatorKind, TypeId), Resolution>,
    /// Binary operators: (op, left, right) -> resolution
    binary: HashMap<(OperatorKind, TypeId, TypeId), Resolution>,
    /// Commutative operator pairs
    commutative: HashSet<(OperatorKind, TypeId, TypeId)>,
    /// Direct coercions: (from, to) -> (lib, cmd, priority)
    direct_coercions: HashMap<(TypeId, TypeId), (LibraryId, u16, u8)>,
    /// Cached coercion paths: (from, to) -> path
    coercion_cache: HashMap<(TypeId, TypeId), CoercionPath>,
}

impl OperatorRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register an operator implementation.
    pub fn register_operator(&mut self, reg: OperatorRegistration) {
        let resolution = Resolution {
            lib: reg.lib,
            command: reg.command,
            result_type: reg.result,
        };

        match reg.signature {
            OpSignature::Unary(ty) => {
                self.unary.insert((reg.op, ty), resolution);
            }
            OpSignature::Symmetric(ty) => {
                self.binary.insert((reg.op, ty, ty), resolution);
                if reg.commutative {
                    self.commutative.insert((reg.op, ty, ty));
                }
            }
            OpSignature::Binary { left, right } => {
                self.binary.insert((reg.op, left, right), resolution);
                if reg.commutative {
                    self.commutative.insert((reg.op, left, right));
                    self.commutative.insert((reg.op, right, left));
                }
            }
        }
    }

    /// Register a type coercion.
    pub fn register_coercion(&mut self, reg: CoercionRegistration) {
        self.direct_coercions
            .insert((reg.from, reg.to), (reg.lib, reg.command, reg.priority));
        // Invalidate cache when new coercions are added
        self.coercion_cache.clear();
    }

    /// Resolve a unary operator.
    pub fn resolve_unary(&self, op: OperatorKind, ty: TypeId) -> Option<&Resolution> {
        self.unary.get(&(op, ty))
    }

    /// Resolve a binary operator.
    pub fn resolve_binary(
        &self,
        op: OperatorKind,
        left: TypeId,
        right: TypeId,
    ) -> Option<&Resolution> {
        // Try direct match
        if let Some(res) = self.binary.get(&(op, left, right)) {
            return Some(res);
        }

        // Try commutative reverse
        if self.commutative.contains(&(op, left, right))
            && let Some(res) = self.binary.get(&(op, right, left))
        {
            return Some(res);
        }

        None
    }

    /// Find a coercion path from one type to another (max depth 2).
    pub fn find_coercion(&mut self, from: TypeId, to: TypeId) -> Option<&CoercionPath> {
        if from == to {
            return None;
        }

        // Check cache first
        if self.coercion_cache.contains_key(&(from, to)) {
            return self.coercion_cache.get(&(from, to));
        }

        // Try direct coercion
        if let Some(&(lib, cmd, priority)) = self.direct_coercions.get(&(from, to)) {
            let path = CoercionPath::single(to, lib, cmd, priority);
            self.coercion_cache.insert((from, to), path);
            return self.coercion_cache.get(&(from, to));
        }

        // Try two-step coercion
        let mut best_path: Option<CoercionPath> = None;

        for (&(f1, mid), &(lib1, cmd1, p1)) in &self.direct_coercions {
            if f1 != from {
                continue;
            }

            if let Some(&(lib2, cmd2, p2)) = self.direct_coercions.get(&(mid, to)) {
                let path = CoercionPath::single(mid, lib1, cmd1, p1).extend(to, lib2, cmd2, p2);

                if best_path
                    .as_ref()
                    .is_none_or(|bp| path.total_priority < bp.total_priority)
                {
                    best_path = Some(path);
                }
            }
        }

        if let Some(path) = best_path {
            self.coercion_cache.insert((from, to), path);
            return self.coercion_cache.get(&(from, to));
        }

        None
    }

    /// Find a common type that both types can coerce to.
    pub fn common_type(&mut self, a: TypeId, b: TypeId) -> Option<TypeId> {
        if a == b {
            return Some(a);
        }

        // Check if a can coerce to b
        if self.find_coercion(a, b).is_some() {
            return Some(b);
        }

        // Check if b can coerce to a
        if self.find_coercion(b, a).is_some() {
            return Some(a);
        }

        // Find a common target type
        // Collect potential targets from a first
        let a_targets: Vec<TypeId> = self
            .direct_coercions
            .keys()
            .filter_map(|&(from, to)| if from == a { Some(to) } else { None })
            .collect();

        let mut candidates: Vec<(TypeId, u32)> = Vec::new();

        for to in a_targets {
            // Check if b can also reach 'to'
            if b == to {
                candidates.push((to, 0));
            } else if let Some(b_path) = self.find_coercion(b, to) {
                let b_priority = b_path.total_priority;
                if let Some(a_path) = self.find_coercion(a, to) {
                    candidates.push((to, a_path.total_priority + b_priority));
                }
            }
        }

        candidates
            .into_iter()
            .min_by_key(|(_, priority)| *priority)
            .map(|(ty, _)| ty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_registry() -> OperatorRegistry {
        let mut reg = OperatorRegistry::new();

        // Register Real + Real -> Real
        reg.register_operator(OperatorRegistration {
            op: OperatorKind::Add,
            signature: OpSignature::Symmetric(TypeId::REAL),
            result: TypeId::REAL,
            lib: LibraryId::new(64),
            command: 1,
            priority: 0,
            commutative: true,
        });

        // Register Real - Real -> Real
        reg.register_operator(OperatorRegistration {
            op: OperatorKind::Sub,
            signature: OpSignature::Symmetric(TypeId::REAL),
            result: TypeId::REAL,
            lib: LibraryId::new(64),
            command: 2,
            priority: 0,
            commutative: false,
        });

        // Register -Real -> Real (unary)
        reg.register_operator(OperatorRegistration {
            op: OperatorKind::Neg,
            signature: OpSignature::Unary(TypeId::REAL),
            result: TypeId::REAL,
            lib: LibraryId::new(64),
            command: 3,
            priority: 0,
            commutative: false,
        });

        reg
    }

    #[test]
    fn resolve_unary_found() {
        let reg = make_registry();
        let res = reg.resolve_unary(OperatorKind::Neg, TypeId::REAL);
        assert!(res.is_some());
        let res = res.unwrap();
        assert_eq!(res.lib, LibraryId::new(64));
        assert_eq!(res.command, 3);
        assert_eq!(res.result_type, TypeId::REAL);
    }

    #[test]
    fn resolve_unary_not_found() {
        let reg = make_registry();
        let res = reg.resolve_unary(OperatorKind::Neg, TypeId::BINT);
        assert!(res.is_none());
    }

    #[test]
    fn resolve_binary_found() {
        let reg = make_registry();
        let res = reg.resolve_binary(OperatorKind::Add, TypeId::REAL, TypeId::REAL);
        assert!(res.is_some());
        let res = res.unwrap();
        assert_eq!(res.command, 1);
    }

    #[test]
    fn resolve_binary_commutative() {
        let mut reg = OperatorRegistry::new();

        // Register Real + Bint -> Real (commutative)
        reg.register_operator(OperatorRegistration {
            op: OperatorKind::Add,
            signature: OpSignature::Binary {
                left: TypeId::REAL,
                right: TypeId::BINT,
            },
            result: TypeId::REAL,
            lib: LibraryId::new(64),
            command: 10,
            priority: 0,
            commutative: true,
        });

        // Direct order
        let res = reg.resolve_binary(OperatorKind::Add, TypeId::REAL, TypeId::BINT);
        assert!(res.is_some());

        // Reverse order (commutative)
        let res = reg.resolve_binary(OperatorKind::Add, TypeId::BINT, TypeId::REAL);
        assert!(res.is_some());
    }

    #[test]
    fn coercion_path_single_step() {
        let mut reg = OperatorRegistry::new();

        reg.register_coercion(CoercionRegistration {
            from: TypeId::BINT,
            to: TypeId::REAL,
            lib: LibraryId::new(10),
            command: 1,
            implicit: true,
            priority: 5,
        });

        let path = reg.find_coercion(TypeId::BINT, TypeId::REAL);
        assert!(path.is_some());
        let path = path.unwrap();
        assert_eq!(path.len(), 1);
        assert_eq!(path.steps[0].0, TypeId::REAL);
    }

    #[test]
    fn coercion_path_two_steps() {
        let mut reg = OperatorRegistry::new();

        reg.register_coercion(CoercionRegistration {
            from: TypeId::BINT,
            to: TypeId::REAL,
            lib: LibraryId::new(10),
            command: 1,
            implicit: true,
            priority: 5,
        });

        reg.register_coercion(CoercionRegistration {
            from: TypeId::REAL,
            to: TypeId::COMPLEX,
            lib: LibraryId::new(10),
            command: 2,
            implicit: true,
            priority: 3,
        });

        let path = reg.find_coercion(TypeId::BINT, TypeId::COMPLEX);
        assert!(path.is_some());
        let path = path.unwrap();
        assert_eq!(path.len(), 2);
        assert_eq!(path.steps[0].0, TypeId::REAL);
        assert_eq!(path.steps[1].0, TypeId::COMPLEX);
        assert_eq!(path.total_priority, 8);
    }

    #[test]
    fn coercion_rejects_depth_greater_than_two() {
        let mut reg = OperatorRegistry::new();

        // A -> B -> C -> D (3 steps)
        let type_a = TypeId::new(100);
        let type_b = TypeId::new(101);
        let type_c = TypeId::new(102);
        let type_d = TypeId::new(103);

        reg.register_coercion(CoercionRegistration {
            from: type_a,
            to: type_b,
            lib: LibraryId::new(10),
            command: 1,
            implicit: true,
            priority: 1,
        });

        reg.register_coercion(CoercionRegistration {
            from: type_b,
            to: type_c,
            lib: LibraryId::new(10),
            command: 2,
            implicit: true,
            priority: 1,
        });

        reg.register_coercion(CoercionRegistration {
            from: type_c,
            to: type_d,
            lib: LibraryId::new(10),
            command: 3,
            implicit: true,
            priority: 1,
        });

        // A -> D requires 3 steps, should not be found
        let path = reg.find_coercion(type_a, type_d);
        assert!(path.is_none());

        // A -> C requires 2 steps, should be found
        let path = reg.find_coercion(type_a, type_c);
        assert!(path.is_some());
    }

    #[test]
    fn common_type_same() {
        let mut reg = OperatorRegistry::new();
        assert_eq!(
            reg.common_type(TypeId::REAL, TypeId::REAL),
            Some(TypeId::REAL)
        );
    }

    #[test]
    fn common_type_coercion() {
        let mut reg = OperatorRegistry::new();

        reg.register_coercion(CoercionRegistration {
            from: TypeId::BINT,
            to: TypeId::REAL,
            lib: LibraryId::new(10),
            command: 1,
            implicit: true,
            priority: 5,
        });

        assert_eq!(
            reg.common_type(TypeId::BINT, TypeId::REAL),
            Some(TypeId::REAL)
        );
        assert_eq!(
            reg.common_type(TypeId::REAL, TypeId::BINT),
            Some(TypeId::REAL)
        );
    }

    #[test]
    fn common_type_shared_target() {
        let mut reg = OperatorRegistry::new();

        // Both BINT and STRING can coerce to SYMBOLIC
        reg.register_coercion(CoercionRegistration {
            from: TypeId::BINT,
            to: TypeId::SYMBOLIC,
            lib: LibraryId::new(10),
            command: 1,
            implicit: true,
            priority: 5,
        });

        reg.register_coercion(CoercionRegistration {
            from: TypeId::STRING,
            to: TypeId::SYMBOLIC,
            lib: LibraryId::new(10),
            command: 2,
            implicit: true,
            priority: 5,
        });

        assert_eq!(
            reg.common_type(TypeId::BINT, TypeId::STRING),
            Some(TypeId::SYMBOLIC)
        );
    }
}
