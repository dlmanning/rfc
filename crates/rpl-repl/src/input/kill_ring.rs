//! Kill ring for storing killed text.
//!
//! The kill ring stores text that has been "killed" (deleted with Ctrl-K, Ctrl-U, etc.)
//! and allows yanking it back with Ctrl-Y, and cycling through previous kills with Alt-Y.

use std::collections::VecDeque;

/// Kill ring for storing killed text.
#[derive(Clone, Debug)]
pub struct KillRing {
    /// The ring of killed text entries.
    entries: VecDeque<String>,
    /// Maximum number of entries to keep.
    max_entries: usize,
    /// Index of last yanked entry (for yank-pop).
    last_yank_index: Option<usize>,
}

impl KillRing {
    /// Create a new kill ring with the given capacity.
    pub fn new(max_entries: usize) -> Self {
        Self {
            entries: VecDeque::with_capacity(max_entries),
            max_entries,
            last_yank_index: None,
        }
    }

    /// Push text onto the kill ring.
    pub fn push(&mut self, text: String) {
        if text.is_empty() {
            return;
        }

        // Reset yank position when new text is killed
        self.last_yank_index = None;

        // Add to front
        self.entries.push_front(text);

        // Trim if over capacity
        while self.entries.len() > self.max_entries {
            self.entries.pop_back();
        }
    }

    /// Yank (retrieve) the most recent kill (Ctrl-Y).
    /// Returns None if the kill ring is empty.
    pub fn yank(&mut self) -> Option<&str> {
        if self.entries.is_empty() {
            return None;
        }
        self.last_yank_index = Some(0);
        self.entries.front().map(|s| s.as_str())
    }

    /// Yank-pop: cycle to the next entry in the kill ring (Alt-Y).
    /// Only valid immediately after yank or yank-pop.
    /// Returns None if not in yank context or ring is empty.
    pub fn yank_pop(&mut self) -> Option<&str> {
        let current = self.last_yank_index?;
        if self.entries.is_empty() {
            return None;
        }

        // Cycle to next entry
        let next = (current + 1) % self.entries.len();
        self.last_yank_index = Some(next);
        self.entries.get(next).map(|s| s.as_str())
    }

    /// Get the length of the last yanked text (for replacement).
    pub fn last_yank_len(&self) -> Option<usize> {
        self.last_yank_index
            .and_then(|i| self.entries.get(i))
            .map(|s| s.len())
    }

    /// Reset yank state (call when doing non-yank operations).
    pub fn reset_yank(&mut self) {
        self.last_yank_index = None;
    }

    /// Check if we're in a yank context (can do yank-pop).
    pub fn in_yank_context(&self) -> bool {
        self.last_yank_index.is_some()
    }
}

impl Default for KillRing {
    fn default() -> Self {
        Self::new(60) // Default capacity like Emacs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_and_yank() {
        let mut ring = KillRing::new(10);
        ring.push("first".to_string());
        ring.push("second".to_string());

        assert_eq!(ring.yank(), Some("second"));
    }

    #[test]
    fn test_yank_pop() {
        let mut ring = KillRing::new(10);
        ring.push("first".to_string());
        ring.push("second".to_string());
        ring.push("third".to_string());

        assert_eq!(ring.yank(), Some("third"));
        assert_eq!(ring.yank_pop(), Some("second"));
        assert_eq!(ring.yank_pop(), Some("first"));
        assert_eq!(ring.yank_pop(), Some("third")); // Wraps around
    }

    #[test]
    fn test_empty_ring() {
        let mut ring = KillRing::new(10);
        assert_eq!(ring.yank(), None);
        assert_eq!(ring.yank_pop(), None);
    }

    #[test]
    fn test_capacity() {
        let mut ring = KillRing::new(3);
        ring.push("a".to_string());
        ring.push("b".to_string());
        ring.push("c".to_string());
        ring.push("d".to_string());

        // "a" should be gone
        assert_eq!(ring.yank(), Some("d"));
        assert_eq!(ring.yank_pop(), Some("c"));
        assert_eq!(ring.yank_pop(), Some("b"));
        assert_eq!(ring.yank_pop(), Some("d")); // Wraps, no "a"
    }

    #[test]
    fn test_empty_push_ignored() {
        let mut ring = KillRing::new(10);
        ring.push("".to_string());
        assert_eq!(ring.yank(), None);
    }
}
