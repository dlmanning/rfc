use rpl_core::{Pos, Span};

/// Source file identifier.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct SourceId(u16);

impl SourceId {
    pub fn new(id: u16) -> Self {
        Self(id)
    }

    pub fn as_u16(self) -> u16 {
        self.0
    }
}

/// Line and column position (1-indexed for display).
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct LineCol {
    pub line: u32,
    pub col: u32,
}

impl LineCol {
    pub fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }
}

/// A source file with line mapping.
#[derive(Clone, Debug)]
pub struct SourceFile {
    id: SourceId,
    name: String,
    source: String,
    line_starts: Vec<u32>,
}

impl SourceFile {
    /// Create a new source file, computing line starts.
    pub fn new(id: SourceId, name: String, source: String) -> Self {
        let line_starts = Self::compute_line_starts(&source);
        Self {
            id,
            name,
            source,
            line_starts,
        }
    }

    fn compute_line_starts(source: &str) -> Vec<u32> {
        let mut starts = vec![0]; // First line starts at offset 0
        for (i, c) in source.char_indices() {
            if c == '\n' {
                starts.push((i + 1) as u32);
            }
        }
        starts
    }

    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    /// Convert a byte position to line/column (1-indexed).
    pub fn line_col(&self, pos: Pos) -> LineCol {
        let offset = pos.offset();
        // Binary search for the line containing this offset
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,      // Exact match: position is at start of line
            Err(idx) => idx - 1, // Not found: position is in previous line
        };
        let line_start = self.line_starts[line_idx];
        LineCol {
            line: (line_idx + 1) as u32,
            col: offset - line_start + 1,
        }
    }

    /// Convert line/column (1-indexed) to byte position.
    pub fn pos_from_line_col(&self, lc: LineCol) -> Option<Pos> {
        if lc.line == 0 || lc.col == 0 {
            return None;
        }
        let line_idx = (lc.line - 1) as usize;
        if line_idx >= self.line_starts.len() {
            return None;
        }
        let line_start = self.line_starts[line_idx];
        let offset = line_start + (lc.col - 1);
        // Check offset is within source bounds
        if offset as usize > self.source.len() {
            return None;
        }
        Some(Pos::new(offset))
    }

    /// Get the text of a line (1-indexed).
    pub fn line_text(&self, line: u32) -> Option<&str> {
        if line == 0 {
            return None;
        }
        let line_idx = (line - 1) as usize;
        if line_idx >= self.line_starts.len() {
            return None;
        }
        let start = self.line_starts[line_idx] as usize;
        let end = if line_idx + 1 < self.line_starts.len() {
            // Next line starts after the newline, so subtract 1 to exclude it
            (self.line_starts[line_idx + 1] - 1) as usize
        } else {
            self.source.len()
        };
        Some(&self.source[start..end])
    }

    /// Get the text covered by a span.
    pub fn span_text(&self, span: Span) -> &str {
        let start = span.start().offset() as usize;
        let end = span.end().offset() as usize;
        &self.source[start.min(self.source.len())..end.min(self.source.len())]
    }

    /// Get the number of lines.
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// Replace lines in the source (1-indexed, end-exclusive).
    /// Returns the byte offset where the edit started.
    pub fn replace_lines(&mut self, start_line: u32, end_line: u32, new_text: &str) -> u32 {
        let start_line = start_line.max(1);
        let start_idx = (start_line - 1) as usize;

        // Get byte offset of start
        let start_offset = if start_idx < self.line_starts.len() {
            self.line_starts[start_idx]
        } else {
            self.source.len() as u32
        };

        // Get byte offset of end (start of end_line, or end of file)
        let end_idx = (end_line - 1) as usize;
        let end_offset = if end_idx < self.line_starts.len() {
            self.line_starts[end_idx]
        } else {
            self.source.len() as u32
        };

        // Replace the text
        let start = start_offset as usize;
        let end = end_offset as usize;
        self.source.replace_range(start..end, new_text);

        // Recompute line starts
        self.line_starts = Self::compute_line_starts(&self.source);

        start_offset
    }

    /// Get the byte offset of a line start (1-indexed).
    pub fn line_start(&self, line: u32) -> Option<u32> {
        if line == 0 {
            return None;
        }
        let idx = (line - 1) as usize;
        self.line_starts.get(idx).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_col_empty_file() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "".into());
        let lc = file.line_col(Pos::new(0));
        assert_eq!(lc, LineCol::new(1, 1));
    }

    #[test]
    fn line_col_single_line() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "hello".into());
        assert_eq!(file.line_col(Pos::new(0)), LineCol::new(1, 1));
        assert_eq!(file.line_col(Pos::new(2)), LineCol::new(1, 3));
        assert_eq!(file.line_col(Pos::new(5)), LineCol::new(1, 6));
    }

    #[test]
    fn line_col_multiple_lines() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef\nghi".into());
        // Line 1: "abc" at offsets 0-2, newline at 3
        assert_eq!(file.line_col(Pos::new(0)), LineCol::new(1, 1)); // 'a'
        assert_eq!(file.line_col(Pos::new(2)), LineCol::new(1, 3)); // 'c'
        assert_eq!(file.line_col(Pos::new(3)), LineCol::new(1, 4)); // '\n'
        // Line 2: "def" at offsets 4-6, newline at 7
        assert_eq!(file.line_col(Pos::new(4)), LineCol::new(2, 1)); // 'd'
        assert_eq!(file.line_col(Pos::new(6)), LineCol::new(2, 3)); // 'f'
        // Line 3: "ghi" at offsets 8-10
        assert_eq!(file.line_col(Pos::new(8)), LineCol::new(3, 1)); // 'g'
        assert_eq!(file.line_col(Pos::new(10)), LineCol::new(3, 3)); // 'i'
    }

    #[test]
    fn line_col_position_at_newline() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "a\nb".into());
        // Newline at offset 1 is still on line 1
        assert_eq!(file.line_col(Pos::new(1)), LineCol::new(1, 2));
        // 'b' at offset 2 is on line 2
        assert_eq!(file.line_col(Pos::new(2)), LineCol::new(2, 1));
    }

    #[test]
    fn pos_from_line_col_round_trip() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef\nghi".into());
        for offset in 0..11 {
            let pos = Pos::new(offset);
            let lc = file.line_col(pos);
            let pos2 = file.pos_from_line_col(lc).unwrap();
            assert_eq!(pos, pos2, "round trip failed for offset {}", offset);
        }
    }

    #[test]
    fn pos_from_line_col_invalid() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef".into());
        assert!(file.pos_from_line_col(LineCol::new(0, 1)).is_none()); // line 0 invalid
        assert!(file.pos_from_line_col(LineCol::new(1, 0)).is_none()); // col 0 invalid
        assert!(file.pos_from_line_col(LineCol::new(5, 1)).is_none()); // line out of range
    }

    #[test]
    fn span_text_basic() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "hello world".into());
        let span = Span::new(Pos::new(0), Pos::new(5));
        assert_eq!(file.span_text(span), "hello");
    }

    #[test]
    fn span_text_crossing_lines() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef".into());
        let span = Span::new(Pos::new(2), Pos::new(5));
        assert_eq!(file.span_text(span), "c\nd");
    }

    #[test]
    fn line_text_basic() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef\nghi".into());
        assert_eq!(file.line_text(1), Some("abc"));
        assert_eq!(file.line_text(2), Some("def"));
        assert_eq!(file.line_text(3), Some("ghi"));
        assert_eq!(file.line_text(0), None);
        assert_eq!(file.line_text(4), None);
    }

    #[test]
    fn line_text_empty_lines() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "a\n\nb".into());
        assert_eq!(file.line_text(1), Some("a"));
        assert_eq!(file.line_text(2), Some(""));
        assert_eq!(file.line_text(3), Some("b"));
    }

    #[test]
    fn line_count() {
        assert_eq!(
            SourceFile::new(SourceId::new(0), "".into(), "".into()).line_count(),
            1
        );
        assert_eq!(
            SourceFile::new(SourceId::new(0), "".into(), "a".into()).line_count(),
            1
        );
        assert_eq!(
            SourceFile::new(SourceId::new(0), "".into(), "a\n".into()).line_count(),
            2
        );
        assert_eq!(
            SourceFile::new(SourceId::new(0), "".into(), "a\nb".into()).line_count(),
            2
        );
        assert_eq!(
            SourceFile::new(SourceId::new(0), "".into(), "a\nb\nc".into()).line_count(),
            3
        );
    }

    #[test]
    fn source_cache_add_get() {
        // Test is in cache.rs
    }

    #[test]
    fn replace_lines_middle() {
        let mut file = SourceFile::new(SourceId::new(0), "test".into(), "aaa\nbbb\nccc".into());
        assert_eq!(file.line_count(), 3);

        // Replace line 2 with new content
        file.replace_lines(2, 3, "XXX\n");
        assert_eq!(file.source(), "aaa\nXXX\nccc");
        assert_eq!(file.line_count(), 3);
    }

    #[test]
    fn replace_lines_insert() {
        let mut file = SourceFile::new(SourceId::new(0), "test".into(), "aaa\nbbb".into());

        // Insert at line 2 (replace empty range)
        file.replace_lines(2, 2, "NEW\n");
        assert_eq!(file.source(), "aaa\nNEW\nbbb");
        assert_eq!(file.line_count(), 3);
    }

    #[test]
    fn replace_lines_delete() {
        let mut file = SourceFile::new(SourceId::new(0), "test".into(), "aaa\nbbb\nccc".into());

        // Delete line 2
        file.replace_lines(2, 3, "");
        assert_eq!(file.source(), "aaa\nccc");
        assert_eq!(file.line_count(), 2);
    }

    #[test]
    fn replace_lines_multiple() {
        let mut file =
            SourceFile::new(SourceId::new(0), "test".into(), "aaa\nbbb\nccc\nddd".into());

        // Replace lines 2-3 with single line
        file.replace_lines(2, 4, "XXX\n");
        assert_eq!(file.source(), "aaa\nXXX\nddd");
        assert_eq!(file.line_count(), 3);
    }

    #[test]
    fn line_start_basic() {
        let file = SourceFile::new(SourceId::new(0), "test".into(), "abc\ndef\nghi".into());
        assert_eq!(file.line_start(1), Some(0));
        assert_eq!(file.line_start(2), Some(4));
        assert_eq!(file.line_start(3), Some(8));
        assert_eq!(file.line_start(0), None);
        assert_eq!(file.line_start(4), None);
    }
}
