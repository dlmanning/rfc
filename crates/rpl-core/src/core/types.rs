/// Type identifier for RPL objects.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(u16);

impl TypeId {
    // Well-known type IDs
    pub const PROGRAM: TypeId = TypeId(8);
    pub const REAL: TypeId = TypeId(10);
    pub const BINT: TypeId = TypeId(12);
    pub const COMMENT: TypeId = TypeId(20);
    pub const STRING: TypeId = TypeId(24);
    pub const COMPLEX: TypeId = TypeId(30);
    pub const SYMBOLIC: TypeId = TypeId(56);
    pub const LIST: TypeId = TypeId(62);
    pub const PLOT: TypeId = TypeId(88);
    pub const RENDER_STATUS: TypeId = TypeId(89);
    pub const LIBRARY: TypeId = TypeId(102);
    pub const LIBPTR: TypeId = TypeId(103);
    pub const BLOB: TypeId = TypeId(110);

    pub fn new(id: u16) -> Self {
        Self(id)
    }

    pub fn as_u16(self) -> u16 {
        self.0
    }

    /// Check if this is a built-in type (ID < 128).
    pub fn is_builtin(self) -> bool {
        self.0 < 128
    }

    /// Get the name of a well-known type, or None for unknown types.
    pub fn name(self) -> Option<&'static str> {
        match self {
            Self::PROGRAM => Some("Program"),
            Self::REAL => Some("Real"),
            Self::BINT => Some("Int"),
            Self::COMMENT => Some("Comment"),
            Self::STRING => Some("String"),
            Self::COMPLEX => Some("Complex"),
            Self::SYMBOLIC => Some("Symbolic"),
            Self::LIST => Some("List"),
            Self::PLOT => Some("Plot"),
            Self::RENDER_STATUS => Some("RenderStatus"),
            Self::LIBRARY => Some("Library"),
            Self::LIBPTR => Some("LibPtr"),
            Self::BLOB => Some("Blob"),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn well_known_ids() {
        assert_eq!(TypeId::PROGRAM.as_u16(), 8);
        assert_eq!(TypeId::REAL.as_u16(), 10);
        assert_eq!(TypeId::BINT.as_u16(), 12);
        assert_eq!(TypeId::STRING.as_u16(), 24);
        assert_eq!(TypeId::COMPLEX.as_u16(), 30);
        assert_eq!(TypeId::SYMBOLIC.as_u16(), 56);
        assert_eq!(TypeId::LIST.as_u16(), 62);
    }

    #[test]
    fn is_builtin() {
        assert!(TypeId::PROGRAM.is_builtin());
        assert!(TypeId::REAL.is_builtin());
        assert!(TypeId::new(0).is_builtin());
        assert!(TypeId::new(127).is_builtin());
        assert!(!TypeId::new(128).is_builtin());
        assert!(!TypeId::new(1000).is_builtin());
    }

    #[test]
    fn type_id_equality() {
        let t1 = TypeId::new(10);
        let t2 = TypeId::REAL;
        assert_eq!(t1, t2);
    }

    #[test]
    fn type_id_hash() {
        use std::collections::HashSet;

        let mut set = HashSet::new();
        set.insert(TypeId::REAL);
        set.insert(TypeId::BINT);
        set.insert(TypeId::new(10)); // same as REAL

        assert_eq!(set.len(), 2);
        assert!(set.contains(&TypeId::REAL));
        assert!(set.contains(&TypeId::BINT));
    }

    #[test]
    fn type_id_copy() {
        let t1 = TypeId::REAL;
        let t2 = t1; // copy
        assert_eq!(t1, t2);
    }
}
