use super::file::{SourceFile, SourceId};

/// Cache for source files.
#[derive(Debug, Default)]
pub struct SourceCache {
    files: Vec<SourceFile>,
}

impl SourceCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file, returning its ID.
    pub fn add(&mut self, name: String, source: String) -> SourceId {
        let id = SourceId::new(self.files.len() as u16);
        let file = SourceFile::new(id, name, source);
        self.files.push(file);
        id
    }

    /// Get a source file by ID.
    pub fn get(&self, id: SourceId) -> Option<&SourceFile> {
        self.files.get(id.as_u16() as usize)
    }

    /// Get a mutable reference to a source file by ID.
    pub fn get_mut(&mut self, id: SourceId) -> Option<&mut SourceFile> {
        self.files.get_mut(id.as_u16() as usize)
    }

    /// Find a source file by name.
    pub fn find_by_name(&self, name: &str) -> Option<SourceId> {
        self.files.iter().find(|f| f.name() == name).map(|f| f.id())
    }

    /// Get the number of source files.
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_and_get() {
        let mut cache = SourceCache::new();
        let id = cache.add("test.rpl".into(), "3 4 +".into());

        let file = cache.get(id).unwrap();
        assert_eq!(file.name(), "test.rpl");
        assert_eq!(file.source(), "3 4 +");
        assert_eq!(file.id(), id);
    }

    #[test]
    fn add_multiple() {
        let mut cache = SourceCache::new();
        let id1 = cache.add("a.rpl".into(), "1".into());
        let id2 = cache.add("b.rpl".into(), "2".into());

        assert_ne!(id1, id2);
        assert_eq!(cache.get(id1).unwrap().source(), "1");
        assert_eq!(cache.get(id2).unwrap().source(), "2");
    }

    #[test]
    fn get_nonexistent() {
        let cache = SourceCache::new();
        assert!(cache.get(SourceId::new(0)).is_none());
        assert!(cache.get(SourceId::new(100)).is_none());
    }

    #[test]
    fn get_mut() {
        let mut cache = SourceCache::new();
        let id = cache.add("test.rpl".into(), "original".into());

        // Verify we can get mutable reference
        let file = cache.get_mut(id).unwrap();
        assert_eq!(file.source(), "original");
    }
}
