//! Source location types for debug info.

/// Byte offset in source code.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Pos(u32);

impl Pos {
    pub fn new(offset: u32) -> Self {
        Self(offset)
    }

    pub fn offset(self) -> u32 {
        self.0
    }
}

/// A range in source code.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    /// Dummy span at position (0, 0).
    pub const DUMMY: Span = Span {
        start: Pos(0),
        end: Pos(0),
    };

    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    pub fn start(self) -> Pos {
        self.start
    }

    pub fn end(self) -> Pos {
        self.end
    }

    /// Check if this span contains a position.
    pub fn contains(self, pos: Pos) -> bool {
        pos >= self.start && pos < self.end
    }

    /// Merge two spans into one covering both.
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: if self.start < other.start {
                self.start
            } else {
                other.start
            },
            end: if self.end > other.end {
                self.end
            } else {
                other.end
            },
        }
    }

    /// Check if this span is empty.
    pub fn is_empty(self) -> bool {
        self.start >= self.end
    }

    /// Length of this span in bytes.
    pub fn len(self) -> u32 {
        self.end.0.saturating_sub(self.start.0)
    }
}
