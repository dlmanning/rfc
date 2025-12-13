/// Library identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LibraryId(u16);

impl LibraryId {
    pub const fn new(id: u16) -> Self {
        Self(id)
    }

    pub const fn as_u16(self) -> u16 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn library_id_new() {
        let id = LibraryId::new(42);
        assert_eq!(id.as_u16(), 42);
    }

    #[test]
    fn library_id_copy() {
        let id1 = LibraryId::new(10);
        let id2 = id1;
        assert_eq!(id1, id2);
    }

    #[test]
    fn library_id_hash() {
        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(LibraryId::new(1));
        set.insert(LibraryId::new(2));
        set.insert(LibraryId::new(1));
        assert_eq!(set.len(), 2);
    }
}
