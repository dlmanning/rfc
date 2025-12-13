//! Multi-line editor overlay for entering program blocks.

use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Clear, Paragraph};

/// Multi-line editor state.
#[derive(Clone, Debug, Default)]
pub struct MultilineEditor {
    /// Lines of text.
    lines: Vec<String>,
    /// Current cursor row (line index).
    row: usize,
    /// Current cursor column (byte position within line).
    col: usize,
}

impl MultilineEditor {
    /// Create a new empty editor.
    pub fn new() -> Self {
        Self {
            lines: vec![String::new()],
            row: 0,
            col: 0,
        }
    }

    /// Create editor with initial content (e.g., from current input line).
    pub fn with_content(content: &str) -> Self {
        let lines: Vec<String> = if content.is_empty() {
            vec![String::new()]
        } else {
            content.lines().map(|s| s.to_string()).collect()
        };

        // Ensure at least one line
        let lines = if lines.is_empty() {
            vec![String::new()]
        } else {
            lines
        };

        let row = lines.len() - 1;
        let col = lines[row].len();

        Self { lines, row, col }
    }

    /// Get all content as a single string with newlines.
    pub fn content(&self) -> String {
        self.lines.join("\n")
    }

    /// Clear the editor.
    pub fn clear(&mut self) {
        self.lines = vec![String::new()];
        self.row = 0;
        self.col = 0;
    }

    /// Get lines for rendering.
    pub fn lines(&self) -> &[String] {
        &self.lines
    }

    /// Check if the editor is empty.
    pub fn is_empty(&self) -> bool {
        self.lines.len() == 1 && self.lines[0].is_empty()
    }

    /// Get current cursor position (row, col).
    pub fn cursor_pos(&self) -> (usize, usize) {
        (self.row, self.col)
    }

    /// Get display column (character count, not bytes).
    pub fn cursor_display_col(&self) -> usize {
        self.lines[self.row][..self.col].chars().count()
    }

    // ========================================================================
    // Cursor movement
    // ========================================================================

    /// Move cursor up.
    pub fn move_up(&mut self) {
        if self.row > 0 {
            self.row -= 1;
            // Clamp column to line length
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    /// Move cursor down.
    pub fn move_down(&mut self) {
        if self.row + 1 < self.lines.len() {
            self.row += 1;
            // Clamp column to line length
            self.col = self.col.min(self.lines[self.row].len());
        }
    }

    /// Move cursor left.
    pub fn move_left(&mut self) {
        if self.col > 0 {
            // Move back one character
            let line = &self.lines[self.row];
            if let Some((i, _)) = line[..self.col].char_indices().next_back() {
                self.col = i;
            }
        } else if self.row > 0 {
            // Wrap to end of previous line
            self.row -= 1;
            self.col = self.lines[self.row].len();
        }
    }

    /// Move cursor right.
    pub fn move_right(&mut self) {
        let line = &self.lines[self.row];
        if self.col < line.len() {
            // Move forward one character
            if let Some(c) = line[self.col..].chars().next() {
                self.col += c.len_utf8();
            }
        } else if self.row + 1 < self.lines.len() {
            // Wrap to start of next line
            self.row += 1;
            self.col = 0;
        }
    }

    /// Move to start of line.
    pub fn move_to_line_start(&mut self) {
        self.col = 0;
    }

    /// Move to end of line.
    pub fn move_to_line_end(&mut self) {
        self.col = self.lines[self.row].len();
    }

    // ========================================================================
    // Editing
    // ========================================================================

    /// Insert a character at cursor.
    pub fn insert_char(&mut self, c: char) {
        self.lines[self.row].insert(self.col, c);
        self.col += c.len_utf8();
    }

    /// Insert a string at cursor (may contain newlines).
    pub fn insert_str(&mut self, s: &str) {
        for c in s.chars() {
            if c == '\n' {
                self.insert_newline();
            } else {
                self.insert_char(c);
            }
        }
    }

    /// Insert a newline (split the line).
    pub fn insert_newline(&mut self) {
        let current_line = &mut self.lines[self.row];
        let rest = current_line[self.col..].to_string();
        current_line.truncate(self.col);

        self.row += 1;
        self.lines.insert(self.row, rest);
        self.col = 0;
    }

    /// Delete character before cursor (backspace).
    pub fn backspace(&mut self) {
        if self.col > 0 {
            let line = &mut self.lines[self.row];
            // Find previous character boundary
            if let Some((i, c)) = line[..self.col].char_indices().next_back() {
                line.remove(i);
                self.col = i;
                let _ = c; // suppress unused warning
            }
        } else if self.row > 0 {
            // Join with previous line
            let current_line = self.lines.remove(self.row);
            self.row -= 1;
            self.col = self.lines[self.row].len();
            self.lines[self.row].push_str(&current_line);
        }
    }

    /// Delete character at cursor.
    pub fn delete(&mut self) {
        let line = &self.lines[self.row];
        if self.col < line.len() {
            self.lines[self.row].remove(self.col);
        } else if self.row + 1 < self.lines.len() {
            // Join with next line
            let next_line = self.lines.remove(self.row + 1);
            self.lines[self.row].push_str(&next_line);
        }
    }

    /// Delete to end of line.
    pub fn kill_to_end(&mut self) -> String {
        let line = &mut self.lines[self.row];
        let killed = line[self.col..].to_string();
        line.truncate(self.col);
        killed
    }

    /// Delete to start of line.
    pub fn kill_to_start(&mut self) -> String {
        let line = &mut self.lines[self.row];
        let killed = line[..self.col].to_string();
        *line = line[self.col..].to_string();
        self.col = 0;
        killed
    }
}

/// Render the multi-line editor as a centered overlay.
pub fn render_multiline_editor(frame: &mut Frame, editor: &MultilineEditor) {
    let area = frame.area();

    // Calculate centered overlay size (80% width, 60% height, min 40x10)
    let width = (area.width * 80 / 100).max(40).min(area.width);
    let height = (area.height * 60 / 100).max(10).min(area.height);

    let x = (area.width - width) / 2;
    let y = (area.height - height) / 2;

    let overlay_area = Rect::new(x, y, width, height);

    // Clear the area behind the overlay
    frame.render_widget(Clear, overlay_area);

    // Create the editor block
    let block = Block::default()
        .title(" Multi-line Editor (Ctrl-Enter: execute, Esc: cancel) ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Magenta));

    let inner = block.inner(overlay_area);
    frame.render_widget(block, overlay_area);

    // Render the lines with line numbers
    let lines = editor.lines();
    let mut spans: Vec<Line> = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        let line_num = format!("{:>3} ", i + 1);
        spans.push(Line::from(vec![
            Span::styled(line_num, Style::default().fg(Color::DarkGray)),
            Span::raw(line.as_str()),
        ]));
    }

    let paragraph = Paragraph::new(spans);
    frame.render_widget(paragraph, inner);

    // Position cursor
    let (cursor_row, _cursor_col) = editor.cursor_pos();
    let cursor_display_col = editor.cursor_display_col();

    // Account for line number prefix ("XXX ")
    let cursor_x = inner.x + 4 + cursor_display_col as u16;
    let cursor_y = inner.y + cursor_row as u16;

    // Only show cursor if within visible area
    if cursor_y < inner.y + inner.height {
        frame.set_cursor_position((cursor_x, cursor_y));
    }
}
