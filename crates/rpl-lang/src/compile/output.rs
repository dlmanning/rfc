use rpl_core::{Span, Word};

/// Buffer for emitting compiled bytecode.
#[derive(Clone)]
pub struct OutputBuffer {
    code: Vec<Word>,
    spans: Vec<Span>,
}

impl OutputBuffer {
    /// Create a new empty output buffer.
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            spans: Vec::new(),
        }
    }

    /// Emit a single word with its source span.
    pub fn emit(&mut self, word: Word, span: Span) {
        self.code.push(word);
        self.spans.push(span);
    }

    /// Emit multiple words with the same source span.
    pub fn emit_all(&mut self, words: &[Word], span: Span) {
        for &word in words {
            self.code.push(word);
            self.spans.push(span);
        }
    }

    /// Get the current length (position) of the buffer.
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Reserve space for `count` words, emitting zeros.
    /// Returns the start position of the reserved space.
    pub fn reserve(&mut self, count: usize, span: Span) -> usize {
        let pos = self.code.len();
        for _ in 0..count {
            self.code.push(0);
            self.spans.push(span);
        }
        pos
    }

    /// Patch a word at the given position.
    pub fn patch(&mut self, pos: usize, word: Word) {
        if pos < self.code.len() {
            self.code[pos] = word;
        }
    }

    /// Get the word at the given position.
    pub fn get(&self, pos: usize) -> Option<Word> {
        self.code.get(pos).copied()
    }

    /// Consume the buffer and return the code and spans.
    pub fn finish(self) -> (Vec<Word>, Vec<Span>) {
        (self.code, self.spans)
    }
}

impl Default for OutputBuffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::Pos;

    fn dummy_span() -> Span {
        Span::new(Pos::new(0), Pos::new(1))
    }

    #[test]
    fn output_buffer_new() {
        let buf = OutputBuffer::new();
        assert!(buf.is_empty());
        assert_eq!(buf.len(), 0);
    }

    #[test]
    fn output_buffer_emit() {
        let mut buf = OutputBuffer::new();
        buf.emit(0x1234, dummy_span());
        assert_eq!(buf.len(), 1);
        assert_eq!(buf.get(0), Some(0x1234));
    }

    #[test]
    fn output_buffer_emit_all() {
        let mut buf = OutputBuffer::new();
        buf.emit_all(&[0x1111, 0x2222, 0x3333], dummy_span());
        assert_eq!(buf.len(), 3);
        assert_eq!(buf.get(0), Some(0x1111));
        assert_eq!(buf.get(1), Some(0x2222));
        assert_eq!(buf.get(2), Some(0x3333));
    }

    #[test]
    fn output_buffer_reserve() {
        let mut buf = OutputBuffer::new();
        buf.emit(0xAAAA, dummy_span());
        let pos = buf.reserve(3, dummy_span());
        assert_eq!(pos, 1);
        assert_eq!(buf.len(), 4);
        assert_eq!(buf.get(1), Some(0));
        assert_eq!(buf.get(2), Some(0));
        assert_eq!(buf.get(3), Some(0));
    }

    #[test]
    fn output_buffer_patch() {
        let mut buf = OutputBuffer::new();
        buf.emit(0x0000, dummy_span());
        buf.patch(0, 0xFFFF);
        assert_eq!(buf.get(0), Some(0xFFFF));
    }

    #[test]
    fn output_buffer_patch_out_of_bounds() {
        let mut buf = OutputBuffer::new();
        buf.emit(0x1234, dummy_span());
        buf.patch(100, 0xFFFF); // Should not panic
        assert_eq!(buf.get(0), Some(0x1234)); // Unchanged
    }

    #[test]
    fn output_buffer_finish() {
        let mut buf = OutputBuffer::new();
        let span = dummy_span();
        buf.emit(0x1111, span);
        buf.emit(0x2222, span);
        let (code, spans) = buf.finish();
        assert_eq!(code, vec![0x1111, 0x2222]);
        assert_eq!(spans.len(), 2);
    }

    #[test]
    fn output_buffer_get_out_of_bounds() {
        let buf = OutputBuffer::new();
        assert_eq!(buf.get(0), None);
    }
}
