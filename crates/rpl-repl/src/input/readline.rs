//! Readline-style input line editing.

use unicode_segmentation::UnicodeSegmentation;

/// A single line of input with cursor and editing operations.
#[derive(Clone, Debug, Default)]
pub struct InputLine {
    /// The text buffer.
    buffer: String,
    /// Cursor position (byte offset).
    cursor: usize,
}

impl InputLine {
    /// Create a new empty input line.
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            cursor: 0,
        }
    }

    /// Get the buffer contents.
    pub fn content(&self) -> &str {
        &self.buffer
    }

    /// Get the cursor position (byte offset).
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buffer.is_empty()
    }

    /// Set the buffer contents and move cursor to end.
    pub fn set(&mut self, s: &str) {
        self.buffer = s.to_string();
        self.cursor = self.buffer.len();
    }

    /// Take the buffer contents, leaving it empty.
    pub fn take(&mut self) -> String {
        self.cursor = 0;
        std::mem::take(&mut self.buffer)
    }

    /// Clear the buffer.
    pub fn clear(&mut self) {
        self.buffer.clear();
        self.cursor = 0;
    }

    // ========================================================================
    // Cursor movement
    // ========================================================================

    /// Move cursor left one character.
    pub fn move_left(&mut self) {
        if self.cursor > 0 {
            // Find previous grapheme boundary
            self.cursor = self.prev_grapheme_boundary(self.cursor);
        }
    }

    /// Move cursor right one character.
    pub fn move_right(&mut self) {
        if self.cursor < self.buffer.len() {
            // Find next grapheme boundary
            self.cursor = self.next_grapheme_boundary(self.cursor);
        }
    }

    /// Move cursor to start of line (Ctrl-A).
    pub fn move_to_start(&mut self) {
        self.cursor = 0;
    }

    /// Move cursor to end of line (Ctrl-E).
    pub fn move_to_end(&mut self) {
        self.cursor = self.buffer.len();
    }

    /// Move cursor left one word (Alt-B).
    pub fn move_word_left(&mut self) {
        self.cursor = self.find_word_start(self.cursor);
    }

    /// Move cursor right one word (Alt-F).
    pub fn move_word_right(&mut self) {
        self.cursor = self.find_word_end(self.cursor);
    }

    // ========================================================================
    // Character editing
    // ========================================================================

    /// Insert a character at cursor position.
    pub fn insert_char(&mut self, c: char) {
        self.buffer.insert(self.cursor, c);
        self.cursor += c.len_utf8();
    }

    /// Insert a string at cursor position.
    pub fn insert_str(&mut self, s: &str) {
        self.buffer.insert_str(self.cursor, s);
        self.cursor += s.len();
    }

    /// Delete character before cursor (Backspace).
    pub fn backspace(&mut self) {
        if self.cursor > 0 {
            let prev = self.prev_grapheme_boundary(self.cursor);
            self.buffer.drain(prev..self.cursor);
            self.cursor = prev;
        }
    }

    /// Delete character at cursor (Delete, Ctrl-D).
    pub fn delete(&mut self) {
        if self.cursor < self.buffer.len() {
            let next = self.next_grapheme_boundary(self.cursor);
            self.buffer.drain(self.cursor..next);
        }
    }

    /// Transpose characters before cursor (Ctrl-T).
    pub fn transpose_chars(&mut self) {
        if self.cursor == 0 || self.buffer.is_empty() {
            return;
        }

        // If at end, transpose last two chars
        // Otherwise, transpose char before cursor with char at cursor
        let pos = if self.cursor >= self.buffer.len() {
            self.prev_grapheme_boundary(self.cursor)
        } else {
            self.cursor
        };

        if pos == 0 {
            return;
        }

        let prev_start = self.prev_grapheme_boundary(pos);
        let curr_end = self.next_grapheme_boundary(pos);

        if curr_end <= pos {
            return;
        }

        // Extract the two characters
        let prev_char: String = self.buffer[prev_start..pos].to_string();
        let curr_char: String = self.buffer[pos..curr_end].to_string();

        // Replace with swapped order
        self.buffer.replace_range(prev_start..curr_end, &format!("{}{}", curr_char, prev_char));

        // Move cursor forward
        self.cursor = curr_end;
    }

    // ========================================================================
    // Kill operations (return killed text for kill ring)
    // ========================================================================

    /// Kill from cursor to end of line (Ctrl-K).
    pub fn kill_to_end(&mut self) -> String {
        let killed = self.buffer[self.cursor..].to_string();
        self.buffer.truncate(self.cursor);
        killed
    }

    /// Kill from start of line to cursor (Ctrl-U).
    pub fn kill_to_start(&mut self) -> String {
        let killed = self.buffer[..self.cursor].to_string();
        self.buffer.drain(..self.cursor);
        self.cursor = 0;
        killed
    }

    /// Kill word forward (Alt-D).
    pub fn kill_word_forward(&mut self) -> String {
        let end = self.find_word_end(self.cursor);
        let killed = self.buffer[self.cursor..end].to_string();
        self.buffer.drain(self.cursor..end);
        killed
    }

    /// Kill word backward (Alt-Backspace, Ctrl-W).
    pub fn kill_word_backward(&mut self) -> String {
        let start = self.find_word_start(self.cursor);
        let killed = self.buffer[start..self.cursor].to_string();
        self.buffer.drain(start..self.cursor);
        self.cursor = start;
        killed
    }

    // ========================================================================
    // Word boundary helpers
    // ========================================================================

    /// Find the start of the current/previous word.
    fn find_word_start(&self, from: usize) -> usize {
        if from == 0 {
            return 0;
        }

        let bytes = self.buffer.as_bytes();
        let mut pos = from;

        // Skip any whitespace before cursor
        while pos > 0 && bytes[pos - 1].is_ascii_whitespace() {
            pos -= 1;
        }

        // Skip word characters
        while pos > 0 && !bytes[pos - 1].is_ascii_whitespace() {
            pos -= 1;
        }

        pos
    }

    /// Find the end of the current/next word.
    fn find_word_end(&self, from: usize) -> usize {
        let len = self.buffer.len();
        if from >= len {
            return len;
        }

        let bytes = self.buffer.as_bytes();
        let mut pos = from;

        // Skip word characters
        while pos < len && !bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }

        // Skip whitespace after word
        while pos < len && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }

        pos
    }

    // ========================================================================
    // Grapheme boundary helpers
    // ========================================================================

    /// Find the previous grapheme boundary.
    fn prev_grapheme_boundary(&self, from: usize) -> usize {
        self.buffer[..from]
            .grapheme_indices(true)
            .next_back()
            .map(|(i, _)| i)
            .unwrap_or(0)
    }

    /// Find the next grapheme boundary.
    fn next_grapheme_boundary(&self, from: usize) -> usize {
        self.buffer[from..]
            .grapheme_indices(true)
            .nth(1)
            .map(|(i, _)| from + i)
            .unwrap_or(self.buffer.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_and_move() {
        let mut line = InputLine::new();
        line.insert_char('a');
        line.insert_char('b');
        line.insert_char('c');
        assert_eq!(line.content(), "abc");
        assert_eq!(line.cursor(), 3);

        line.move_left();
        assert_eq!(line.cursor(), 2);

        line.insert_char('X');
        assert_eq!(line.content(), "abXc");
    }

    #[test]
    fn test_word_movement() {
        let mut line = InputLine::new();
        line.set("hello world test");

        line.move_to_start();
        line.move_word_right();
        assert_eq!(line.cursor(), 6); // After "hello "

        line.move_word_right();
        assert_eq!(line.cursor(), 12); // After "world "

        line.move_word_left();
        assert_eq!(line.cursor(), 6); // Back to "world"
    }

    #[test]
    fn test_kill_to_end() {
        let mut line = InputLine::new();
        line.set("hello world");
        line.cursor = 6;

        let killed = line.kill_to_end();
        assert_eq!(killed, "world");
        assert_eq!(line.content(), "hello ");
    }

    #[test]
    fn test_kill_word_backward() {
        let mut line = InputLine::new();
        line.set("hello world");

        let killed = line.kill_word_backward();
        assert_eq!(killed, "world");
        assert_eq!(line.content(), "hello ");
    }

    #[test]
    fn test_transpose() {
        let mut line = InputLine::new();
        line.set("ab");
        line.transpose_chars();
        assert_eq!(line.content(), "ba");
    }
}
