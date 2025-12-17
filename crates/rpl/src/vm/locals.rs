//! Indexed local variables.
//!
//! WASM-style local variables: a flat array indexed by u32.
//! Locals are allocated per call frame and accessed via `local.get`/`local.set`.

use crate::value::Value;

/// Error type for local variable operations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalsError {
    /// Invalid local index.
    InvalidIndex(u32),
    /// Local not initialized.
    Uninitialized(u32),
}

impl std::fmt::Display for LocalsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocalsError::InvalidIndex(i) => write!(f, "invalid local index: {}", i),
            LocalsError::Uninitialized(i) => write!(f, "uninitialized local: {}", i),
        }
    }
}

impl std::error::Error for LocalsError {}

/// Storage for indexed local variables.
#[derive(Clone, Debug, Default)]
pub struct Locals {
    /// Local variable slots. None = uninitialized.
    slots: Vec<Option<Value>>,
}

impl Locals {
    /// Create new locals storage.
    pub fn new() -> Self {
        Self { slots: Vec::new() }
    }

    /// Create locals storage with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            slots: Vec::with_capacity(capacity),
        }
    }

    /// Get the number of allocated local slots.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Check if no locals are allocated.
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Ensure we have at least `count` slots allocated.
    pub fn ensure_capacity(&mut self, count: usize) {
        if self.slots.len() < count {
            self.slots.resize(count, None);
        }
    }

    /// Get a local variable by index.
    pub fn get(&self, index: u32) -> Result<&Value, LocalsError> {
        let idx = index as usize;
        if idx >= self.slots.len() {
            return Err(LocalsError::InvalidIndex(index));
        }
        self.slots[idx]
            .as_ref()
            .ok_or(LocalsError::Uninitialized(index))
    }

    /// Set a local variable by index.
    pub fn set(&mut self, index: u32, value: Value) -> Result<(), LocalsError> {
        let idx = index as usize;
        // Auto-extend if needed
        if idx >= self.slots.len() {
            self.slots.resize(idx + 1, None);
        }
        self.slots[idx] = Some(value);
        Ok(())
    }

    /// Clear all locals.
    pub fn clear(&mut self) {
        self.slots.clear();
    }

    /// Allocate a new local slot and return its index.
    pub fn alloc(&mut self) -> u32 {
        let idx = self.slots.len() as u32;
        self.slots.push(None);
        idx
    }

    /// Allocate multiple local slots and return the starting index.
    pub fn alloc_many(&mut self, count: usize) -> u32 {
        let start = self.slots.len() as u32;
        for _ in 0..count {
            self.slots.push(None);
        }
        start
    }

    /// Get a local if it exists and is initialized.
    pub fn try_get(&self, index: u32) -> Option<&Value> {
        self.slots.get(index as usize)?.as_ref()
    }

    /// Save all locals (including indices) for later restoration.
    pub fn save(&self) -> Vec<Option<Value>> {
        self.slots.clone()
    }

    /// Restore locals from a saved state, clearing current state first.
    pub fn restore(&mut self, saved: Vec<Option<Value>>) {
        self.slots = saved;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_get_set() {
        let mut locals = Locals::new();

        // Set extends automatically
        locals.set(0, Value::integer(42)).unwrap();
        assert_eq!(locals.get(0).unwrap(), &Value::integer(42));
    }

    #[test]
    fn uninitialized() {
        let mut locals = Locals::new();
        locals.ensure_capacity(5);

        assert_eq!(locals.get(0), Err(LocalsError::Uninitialized(0)));
        assert_eq!(locals.get(4), Err(LocalsError::Uninitialized(4)));
    }

    #[test]
    fn invalid_index() {
        let locals = Locals::new();
        assert_eq!(locals.get(0), Err(LocalsError::InvalidIndex(0)));
    }

    #[test]
    fn alloc() {
        let mut locals = Locals::new();

        let idx0 = locals.alloc();
        let idx1 = locals.alloc();

        assert_eq!(idx0, 0);
        assert_eq!(idx1, 1);
        assert_eq!(locals.len(), 2);
    }

    #[test]
    fn alloc_many() {
        let mut locals = Locals::new();

        let start = locals.alloc_many(3);
        assert_eq!(start, 0);
        assert_eq!(locals.len(), 3);

        let start2 = locals.alloc_many(2);
        assert_eq!(start2, 3);
        assert_eq!(locals.len(), 5);
    }

    #[test]
    fn overwrite() {
        let mut locals = Locals::new();
        locals.set(0, Value::integer(1)).unwrap();
        locals.set(0, Value::integer(2)).unwrap();

        assert_eq!(locals.get(0).unwrap(), &Value::integer(2));
    }
}
