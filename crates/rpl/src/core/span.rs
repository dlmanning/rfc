use std::ops::Deref;

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

/// A value with an associated span.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Spanned<T> {
    node: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.node
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            node: &self.node,
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pos_ordering() {
        let p1 = Pos::new(10);
        let p2 = Pos::new(20);
        assert!(p1 < p2);
        assert!(p2 > p1);
        assert_eq!(p1, Pos::new(10));
    }

    #[test]
    fn span_contains() {
        let span = Span::new(Pos::new(10), Pos::new(20));
        assert!(span.contains(Pos::new(10)));
        assert!(span.contains(Pos::new(15)));
        assert!(span.contains(Pos::new(19)));
        assert!(!span.contains(Pos::new(20))); // end is exclusive
        assert!(!span.contains(Pos::new(9)));
        assert!(!span.contains(Pos::new(25)));
    }

    #[test]
    fn span_merge() {
        let s1 = Span::new(Pos::new(10), Pos::new(20));
        let s2 = Span::new(Pos::new(15), Pos::new(30));
        let merged = s1.merge(s2);
        assert_eq!(merged.start(), Pos::new(10));
        assert_eq!(merged.end(), Pos::new(30));

        // Test merge in reverse order
        let merged2 = s2.merge(s1);
        assert_eq!(merged2.start(), Pos::new(10));
        assert_eq!(merged2.end(), Pos::new(30));

        // Test disjoint spans
        let s3 = Span::new(Pos::new(5), Pos::new(8));
        let merged3 = s1.merge(s3);
        assert_eq!(merged3.start(), Pos::new(5));
        assert_eq!(merged3.end(), Pos::new(20));
    }

    #[test]
    fn span_is_empty() {
        assert!(Span::DUMMY.is_empty());
        assert!(Span::new(Pos::new(10), Pos::new(10)).is_empty());
        assert!(!Span::new(Pos::new(10), Pos::new(11)).is_empty());
    }

    #[test]
    fn span_len() {
        assert_eq!(Span::DUMMY.len(), 0);
        assert_eq!(Span::new(Pos::new(10), Pos::new(20)).len(), 10);
        assert_eq!(Span::new(Pos::new(10), Pos::new(10)).len(), 0);
    }

    #[test]
    fn spanned_deref() {
        let spanned = Spanned::new(42, Span::DUMMY);
        assert_eq!(*spanned, 42);
    }

    #[test]
    fn spanned_map() {
        let spanned = Spanned::new(42, Span::new(Pos::new(1), Pos::new(3)));
        let mapped = spanned.map(|x| x * 2);
        assert_eq!(*mapped, 84);
        assert_eq!(mapped.span(), Span::new(Pos::new(1), Pos::new(3)));
    }

    #[test]
    fn spanned_as_ref() {
        let spanned = Spanned::new(String::from("hello"), Span::DUMMY);
        let ref_spanned = spanned.as_ref();
        assert_eq!(*ref_spanned, &String::from("hello"));
    }
}
