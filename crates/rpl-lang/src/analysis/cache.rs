use std::collections::HashMap;

use rpl_source::SourceId;

use super::result::AnalysisResult;

/// Cache for analysis results.
pub struct AnalysisCache {
    results: HashMap<SourceId, AnalysisResult>,
    /// Order of insertion for LRU eviction.
    order: Vec<SourceId>,
    max_entries: usize,
}

impl AnalysisCache {
    /// Create a new cache with the given maximum number of entries.
    pub fn new(max_entries: usize) -> Self {
        Self {
            results: HashMap::new(),
            order: Vec::new(),
            max_entries,
        }
    }

    /// Get an analysis result by source ID.
    pub fn get(&self, id: SourceId) -> Option<&AnalysisResult> {
        self.results.get(&id)
    }

    /// Get a mutable reference to an analysis result by source ID.
    pub fn get_mut(&mut self, id: SourceId) -> Option<&mut AnalysisResult> {
        self.results.get_mut(&id)
    }

    /// Insert an analysis result, evicting the oldest if over capacity.
    pub fn insert(&mut self, result: AnalysisResult) {
        let id = result.source_id;

        // If already present, update and move to end of order
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.results.entry(id) {
            e.insert(result);
            self.touch(id);
            return;
        }

        // Don't insert if capacity is 0
        if self.max_entries == 0 {
            return;
        }

        // Evict oldest if at capacity
        while self.results.len() >= self.max_entries && !self.order.is_empty() {
            let oldest = self.order.remove(0);
            self.results.remove(&oldest);
        }

        // Insert new entry
        self.results.insert(id, result);
        self.order.push(id);
    }

    /// Invalidate (remove) an analysis result.
    pub fn invalidate(&mut self, id: SourceId) {
        self.results.remove(&id);
        self.order.retain(|&i| i != id);
    }

    /// Move an entry to the end of the order (most recently used).
    fn touch(&mut self, id: SourceId) {
        self.order.retain(|&i| i != id);
        self.order.push(id);
    }

    /// Get the number of cached results.
    pub fn len(&self) -> usize {
        self.results.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.results.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_result(id: u16) -> AnalysisResult {
        AnalysisResult::new(SourceId::new(id))
    }

    #[test]
    fn cache_new() {
        let cache = AnalysisCache::new(10);
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn cache_insert_and_get() {
        let mut cache = AnalysisCache::new(10);
        let result = make_result(1);

        cache.insert(result);
        assert_eq!(cache.len(), 1);

        let retrieved = cache.get(SourceId::new(1));
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().source_id, SourceId::new(1));
    }

    #[test]
    fn cache_get_mut() {
        let mut cache = AnalysisCache::new(10);
        cache.insert(make_result(1));

        let retrieved = cache.get_mut(SourceId::new(1));
        assert!(retrieved.is_some());

        // Modify through mutable reference
        retrieved.unwrap().version = 42;

        // Verify modification persisted
        assert_eq!(cache.get(SourceId::new(1)).unwrap().version, 42);
    }

    #[test]
    fn cache_get_nonexistent() {
        let cache = AnalysisCache::new(10);
        assert!(cache.get(SourceId::new(999)).is_none());
    }

    #[test]
    fn cache_invalidate() {
        let mut cache = AnalysisCache::new(10);
        cache.insert(make_result(1));
        cache.insert(make_result(2));

        assert_eq!(cache.len(), 2);

        cache.invalidate(SourceId::new(1));
        assert_eq!(cache.len(), 1);
        assert!(cache.get(SourceId::new(1)).is_none());
        assert!(cache.get(SourceId::new(2)).is_some());
    }

    #[test]
    fn cache_invalidate_nonexistent() {
        let mut cache = AnalysisCache::new(10);
        cache.insert(make_result(1));

        // Should not panic
        cache.invalidate(SourceId::new(999));
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn cache_eviction() {
        let mut cache = AnalysisCache::new(3);

        cache.insert(make_result(1));
        cache.insert(make_result(2));
        cache.insert(make_result(3));
        assert_eq!(cache.len(), 3);

        // Insert 4th entry, should evict oldest (1)
        cache.insert(make_result(4));
        assert_eq!(cache.len(), 3);
        assert!(cache.get(SourceId::new(1)).is_none());
        assert!(cache.get(SourceId::new(2)).is_some());
        assert!(cache.get(SourceId::new(3)).is_some());
        assert!(cache.get(SourceId::new(4)).is_some());
    }

    #[test]
    fn cache_update_existing() {
        let mut cache = AnalysisCache::new(3);

        cache.insert(make_result(1));
        cache.insert(make_result(2));
        cache.insert(make_result(3));

        // Update entry 1 (should move to end of order)
        let mut updated = make_result(1);
        updated.version = 100;
        cache.insert(updated);

        // Insert new entry, should evict 2 (oldest after 1 was touched)
        cache.insert(make_result(4));
        assert_eq!(cache.len(), 3);
        assert!(cache.get(SourceId::new(1)).is_some());
        assert!(cache.get(SourceId::new(2)).is_none());
        assert!(cache.get(SourceId::new(3)).is_some());
        assert!(cache.get(SourceId::new(4)).is_some());

        // Verify the updated version is preserved
        assert_eq!(cache.get(SourceId::new(1)).unwrap().version, 100);
    }

    #[test]
    fn cache_zero_capacity() {
        let mut cache = AnalysisCache::new(0);

        // Should not store anything
        cache.insert(make_result(1));
        assert!(cache.is_empty());
    }
}
