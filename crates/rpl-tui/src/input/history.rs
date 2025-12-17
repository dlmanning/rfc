//! Command history with persistence and incremental search.

use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;

/// Command history with file persistence.
#[derive(Clone, Debug)]
pub struct History {
    /// History entries (oldest first).
    entries: Vec<String>,
    /// Maximum number of entries to keep.
    max_entries: usize,
    /// Path to history file (if any).
    file_path: Option<PathBuf>,
    /// Current navigation position (None = not navigating).
    position: Option<usize>,
    /// Saved input when navigating.
    saved_input: String,
}

impl History {
    /// Create a new history with the given capacity.
    pub fn new(max_entries: usize) -> Self {
        Self {
            entries: Vec::new(),
            max_entries,
            file_path: None,
            position: None,
            saved_input: String::new(),
        }
    }

    /// Create history with file persistence.
    pub fn with_file(max_entries: usize, path: PathBuf) -> Self {
        let mut history = Self::new(max_entries);
        history.file_path = Some(path);
        history
    }

    /// Load history from file (if configured).
    pub fn load(&mut self) -> io::Result<()> {
        let path = match &self.file_path {
            Some(p) => p,
            None => return Ok(()),
        };

        if !path.exists() {
            return Ok(());
        }

        let file = File::open(path)?;
        let reader = BufReader::new(file);

        self.entries.clear();
        for line in reader.lines() {
            let line = line?;
            // Skip comments and empty lines
            if line.starts_with('#') || line.trim().is_empty() {
                continue;
            }
            // Unescape newlines
            let entry = line.replace("\\n", "\n").replace("\\\\", "\\");
            self.entries.push(entry);
        }

        // Trim to max entries (keep most recent)
        if self.entries.len() > self.max_entries {
            let excess = self.entries.len() - self.max_entries;
            self.entries.drain(..excess);
        }

        Ok(())
    }

    /// Save history to file (if configured).
    pub fn save(&self) -> io::Result<()> {
        let path = match &self.file_path {
            Some(p) => p,
            None => return Ok(()),
        };

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let mut file = File::create(path)?;
        writeln!(file, "# RPL History")?;

        for entry in &self.entries {
            // Escape backslashes and newlines
            let escaped = entry.replace('\\', "\\\\").replace('\n', "\\n");
            writeln!(file, "{}", escaped)?;
        }

        Ok(())
    }

    /// Add an entry to history.
    pub fn add(&mut self, entry: String) {
        if entry.trim().is_empty() {
            return;
        }

        // Don't add duplicates of the last entry
        if self.entries.last() == Some(&entry) {
            return;
        }

        self.entries.push(entry);

        // Trim to max entries
        if self.entries.len() > self.max_entries {
            self.entries.remove(0);
        }

        // Reset navigation
        self.position = None;
    }

    /// Get all entries.
    pub fn entries(&self) -> &[String] {
        &self.entries
    }

    /// Check if history is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    // ========================================================================
    // Navigation
    // ========================================================================

    /// Start or continue navigating to previous (older) entry.
    /// Returns the entry to display, or None if at the beginning.
    pub fn prev(&mut self, current_input: &str) -> Option<&str> {
        if self.entries.is_empty() {
            return None;
        }

        match self.position {
            None => {
                // Start navigation, save current input
                self.saved_input = current_input.to_string();
                self.position = Some(self.entries.len() - 1);
                self.entries.last().map(|s| s.as_str())
            }
            Some(pos) if pos > 0 => {
                self.position = Some(pos - 1);
                self.entries.get(pos - 1).map(|s| s.as_str())
            }
            _ => None, // Already at oldest
        }
    }

    /// Navigate to next (newer) entry.
    /// Returns the entry to display, or the saved input if back to current.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<&str> {
        match self.position {
            Some(pos) if pos + 1 < self.entries.len() => {
                self.position = Some(pos + 1);
                self.entries.get(pos + 1).map(|s| s.as_str())
            }
            Some(_) => {
                // Back to current input
                self.position = None;
                Some(&self.saved_input)
            }
            None => None,
        }
    }

    /// Reset navigation state.
    pub fn reset_navigation(&mut self) {
        self.position = None;
        self.saved_input.clear();
    }

    /// Check if currently navigating.
    pub fn is_navigating(&self) -> bool {
        self.position.is_some()
    }
}

/// Incremental history search state.
#[derive(Clone, Debug)]
pub struct HistorySearch {
    /// Current search query.
    query: String,
    /// Indices of matching entries (most recent first).
    matches: Vec<usize>,
    /// Current position in matches.
    current_match: usize,
}

impl HistorySearch {
    /// Start a new search.
    pub fn new() -> Self {
        Self {
            query: String::new(),
            matches: Vec::new(),
            current_match: 0,
        }
    }

    /// Get the current query.
    pub fn query(&self) -> &str {
        &self.query
    }

    /// Add a character to the search query and update matches.
    pub fn push_char(&mut self, c: char, history: &History) {
        self.query.push(c);
        self.update_matches(history);
    }

    /// Remove the last character from the query.
    pub fn pop_char(&mut self, history: &History) {
        self.query.pop();
        self.update_matches(history);
    }

    /// Update the list of matching entries.
    fn update_matches(&mut self, history: &History) {
        self.matches.clear();
        if self.query.is_empty() {
            return;
        }

        // Search from most recent to oldest
        for (i, entry) in history.entries().iter().enumerate().rev() {
            if entry.contains(&self.query) {
                self.matches.push(i);
            }
        }

        // Reset to first match
        self.current_match = 0;
    }

    /// Get the current matching entry.
    pub fn current_entry<'a>(&self, history: &'a History) -> Option<&'a str> {
        self.matches
            .get(self.current_match)
            .and_then(|&idx| history.entries().get(idx))
            .map(|s| s.as_str())
    }

    /// Move to the next match (older).
    pub fn next_match(&mut self) {
        if !self.matches.is_empty() && self.current_match + 1 < self.matches.len() {
            self.current_match += 1;
        }
    }

    /// Move to the previous match (newer).
    pub fn prev_match(&mut self) {
        if self.current_match > 0 {
            self.current_match -= 1;
        }
    }

    /// Check if there are any matches.
    pub fn has_matches(&self) -> bool {
        !self.matches.is_empty()
    }

    /// Get the number of matches.
    pub fn match_count(&self) -> usize {
        self.matches.len()
    }

    /// Get the current match index (1-based for display).
    pub fn current_match_index(&self) -> usize {
        if self.matches.is_empty() {
            0
        } else {
            self.current_match + 1
        }
    }
}

impl Default for HistorySearch {
    fn default() -> Self {
        Self::new()
    }
}

/// Get the default history file path.
pub fn default_history_path() -> Option<PathBuf> {
    dirs::data_local_dir().map(|p| p.join("rpl").join("history"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_and_navigate() {
        let mut history = History::new(100);
        history.add("first".to_string());
        history.add("second".to_string());
        history.add("third".to_string());

        assert_eq!(history.prev("current"), Some("third"));
        assert_eq!(history.prev(""), Some("second"));
        assert_eq!(history.prev(""), Some("first"));
        assert_eq!(history.prev(""), None); // At oldest

        assert_eq!(history.next(), Some("second"));
        assert_eq!(history.next(), Some("third"));
        assert_eq!(history.next(), Some("current")); // Back to saved
    }

    #[test]
    fn test_no_duplicate_last() {
        let mut history = History::new(100);
        history.add("same".to_string());
        history.add("same".to_string());
        history.add("same".to_string());

        assert_eq!(history.len(), 1);
    }

    #[test]
    fn test_max_entries() {
        let mut history = History::new(3);
        history.add("1".to_string());
        history.add("2".to_string());
        history.add("3".to_string());
        history.add("4".to_string());

        assert_eq!(history.len(), 3);
        assert_eq!(history.entries(), &["2", "3", "4"]);
    }

    #[test]
    fn test_search() {
        let mut history = History::new(100);
        history.add("hello world".to_string());
        history.add("foo bar".to_string());
        history.add("hello again".to_string());
        history.add("goodbye".to_string());

        let mut search = HistorySearch::new();
        search.push_char('h', &history);
        search.push_char('e', &history);
        search.push_char('l', &history);

        assert_eq!(search.match_count(), 2);
        assert_eq!(search.current_entry(&history), Some("hello again"));

        search.next_match();
        assert_eq!(search.current_entry(&history), Some("hello world"));
    }

    #[test]
    fn test_empty_not_added() {
        let mut history = History::new(100);
        history.add("".to_string());
        history.add("   ".to_string());

        assert!(history.is_empty());
    }
}
