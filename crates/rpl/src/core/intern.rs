use std::collections::HashMap;

/// An interned string handle.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(u32);

impl Symbol {
    /// Create a symbol from a raw u32 value.
    ///
    /// # Safety
    /// This should only be used in tests or when reconstructing
    /// a symbol from a known valid value.
    pub fn from_raw(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// String interner for deduplicating strings.
#[derive(Clone, Debug, Default)]
pub struct Interner {
    to_symbol: HashMap<String, Symbol>,
    to_string: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    /// Intern a string, returning its symbol.
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&symbol) = self.to_symbol.get(s) {
            return symbol;
        }

        let symbol = Symbol(self.to_string.len() as u32);
        self.to_string.push(s.to_owned());
        self.to_symbol.insert(s.to_owned(), symbol);
        symbol
    }

    /// Resolve a symbol to its string.
    pub fn resolve(&self, symbol: Symbol) -> &str {
        &self.to_string[symbol.0 as usize]
    }

    /// Check if a string has been interned.
    pub fn contains(&self, s: &str) -> bool {
        self.to_symbol.contains_key(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_same_string_twice() {
        let mut interner = Interner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("hello");
        assert_eq!(s1, s2);
    }

    #[test]
    fn intern_different_strings() {
        let mut interner = Interner::new();
        let s1 = interner.intern("hello");
        let s2 = interner.intern("world");
        assert_ne!(s1, s2);
    }

    #[test]
    fn resolve_symbol() {
        let mut interner = Interner::new();
        let symbol = interner.intern("hello");
        assert_eq!(interner.resolve(symbol), "hello");
    }

    #[test]
    fn contains_interned() {
        let mut interner = Interner::new();
        assert!(!interner.contains("hello"));
        interner.intern("hello");
        assert!(interner.contains("hello"));
        assert!(!interner.contains("world"));
    }

    #[test]
    fn symbol_copy() {
        let mut interner = Interner::new();
        let s1 = interner.intern("test");
        let s2 = s1; // copy
        assert_eq!(s1, s2);
        assert_eq!(interner.resolve(s1), interner.resolve(s2));
    }

    #[test]
    fn symbol_hash() {
        use std::collections::HashSet;

        let mut interner = Interner::new();
        let s1 = interner.intern("a");
        let s2 = interner.intern("b");
        let s3 = interner.intern("a"); // same as s1

        let mut set = HashSet::new();
        set.insert(s1);
        set.insert(s2);
        set.insert(s3);

        assert_eq!(set.len(), 2);
        assert!(set.contains(&s1));
        assert!(set.contains(&s2));
    }

    #[test]
    fn intern_empty_string() {
        let mut interner = Interner::new();
        let s = interner.intern("");
        assert_eq!(interner.resolve(s), "");
    }

    #[test]
    fn intern_unicode() {
        let mut interner = Interner::new();
        let s = interner.intern("héllo wörld 🦀");
        assert_eq!(interner.resolve(s), "héllo wörld 🦀");
    }
}
