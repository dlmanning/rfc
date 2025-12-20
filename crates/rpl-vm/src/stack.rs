//! Calculator data stack.
//!
//! The stack holds `Value` objects and provides standard stack operations
//! plus calculator-specific operations like PICK and ROLL.

use crate::value::Value;

/// Error type for stack operations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StackError {
    /// Stack underflow - tried to pop from empty stack.
    Underflow,
    /// Invalid stack index.
    InvalidIndex(usize),
    /// Stack overflow - exceeded maximum size.
    Overflow,
}

impl std::fmt::Display for StackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackError::Underflow => write!(f, "stack underflow"),
            StackError::InvalidIndex(i) => write!(f, "invalid stack index: {}", i),
            StackError::Overflow => write!(f, "stack overflow"),
        }
    }
}

impl std::error::Error for StackError {}

/// The calculator data stack.
#[derive(Clone, Debug, Default)]
pub struct Stack {
    items: Vec<Value>,
    max_size: Option<usize>,
}

impl Stack {
    /// Create a new empty stack.
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            max_size: None,
        }
    }

    /// Create a stack with a maximum size limit.
    pub fn with_max_size(max: usize) -> Self {
        Self {
            items: Vec::new(),
            max_size: Some(max),
        }
    }

    /// Get the number of items on the stack.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Get the depth of the stack (alias for len).
    pub fn depth(&self) -> usize {
        self.items.len()
    }

    /// Check if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Push a value onto the stack.
    pub fn push(&mut self, value: Value) -> Result<(), StackError> {
        if let Some(max) = self.max_size
            && self.items.len() >= max {
                return Err(StackError::Overflow);
            }
        self.items.push(value);
        Ok(())
    }

    /// Pop a value from the stack.
    pub fn pop(&mut self) -> Result<Value, StackError> {
        self.items.pop().ok_or(StackError::Underflow)
    }

    /// Peek at the top of stack without removing it.
    pub fn top(&self) -> Result<&Value, StackError> {
        self.items.last().ok_or(StackError::Underflow)
    }

    /// Peek at the top of stack mutably.
    pub fn top_mut(&mut self) -> Result<&mut Value, StackError> {
        self.items.last_mut().ok_or(StackError::Underflow)
    }

    /// Get a reference to an item at a given depth (0 = top).
    pub fn peek(&self, depth: usize) -> Result<&Value, StackError> {
        if depth >= self.items.len() {
            return Err(StackError::InvalidIndex(depth));
        }
        Ok(&self.items[self.items.len() - 1 - depth])
    }

    /// Get a mutable reference to an item at a given depth (0 = top).
    pub fn peek_mut(&mut self, depth: usize) -> Result<&mut Value, StackError> {
        let len = self.items.len();
        if depth >= len {
            return Err(StackError::InvalidIndex(depth));
        }
        Ok(&mut self.items[len - 1 - depth])
    }

    /// Clear the stack.
    pub fn clear(&mut self) {
        self.items.clear();
    }

    /// Get a slice of all items (bottom to top).
    pub fn as_slice(&self) -> &[Value] {
        &self.items
    }

    /// Duplicate the top item (DUP).
    pub fn dup(&mut self) -> Result<(), StackError> {
        let top = self.top()?.clone();
        self.push(top)
    }

    /// Drop the top item (DROP).
    pub fn drop(&mut self) -> Result<(), StackError> {
        self.pop()?;
        Ok(())
    }

    /// Swap the top two items (SWAP).
    pub fn swap(&mut self) -> Result<(), StackError> {
        let len = self.items.len();
        if len < 2 {
            return Err(StackError::Underflow);
        }
        self.items.swap(len - 1, len - 2);
        Ok(())
    }

    /// Rotate top three items: 3 2 1 -> 2 1 3 (ROT).
    pub fn rot(&mut self) -> Result<(), StackError> {
        let len = self.items.len();
        if len < 3 {
            return Err(StackError::Underflow);
        }
        // 3 2 1 -> 2 1 3
        // items[len-3], items[len-2], items[len-1]
        // Move items[len-3] to the top
        let bottom = self.items.remove(len - 3);
        self.items.push(bottom);
        Ok(())
    }

    /// Copy second item to top (OVER). Equivalent to 2 PICK.
    pub fn over(&mut self) -> Result<(), StackError> {
        if self.items.len() < 2 {
            return Err(StackError::Underflow);
        }
        let value = self.items[self.items.len() - 2].clone();
        self.push(value)
    }

    /// Copy the nth item to top (PICK). n=1 is DUP.
    pub fn pick(&mut self, n: usize) -> Result<(), StackError> {
        if n == 0 || n > self.items.len() {
            return Err(StackError::InvalidIndex(n));
        }
        let value = self.items[self.items.len() - n].clone();
        self.push(value)
    }

    /// Move the nth item to top, shifting others down (ROLL). n=1 is no-op, n=2 is SWAP.
    pub fn roll(&mut self, n: usize) -> Result<(), StackError> {
        if n == 0 || n > self.items.len() {
            return Err(StackError::InvalidIndex(n));
        }
        if n == 1 {
            return Ok(()); // No-op
        }
        let idx = self.items.len() - n;
        let value = self.items.remove(idx);
        self.items.push(value);
        Ok(())
    }

    /// Push multiple values at once.
    pub fn push_many(&mut self, values: impl IntoIterator<Item = Value>) -> Result<(), StackError> {
        for v in values {
            self.push(v)?;
        }
        Ok(())
    }

    /// Pop multiple values at once (returns in pop order: top first).
    pub fn pop_many(&mut self, count: usize) -> Result<Vec<Value>, StackError> {
        if count > self.items.len() {
            return Err(StackError::Underflow);
        }
        let mut result = Vec::with_capacity(count);
        for _ in 0..count {
            result.push(self.items.pop().unwrap());
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_push_pop() {
        let mut stack = Stack::new();
        assert!(stack.is_empty());

        stack.push(Value::integer(1)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        assert_eq!(stack.len(), 2);

        assert_eq!(stack.pop().unwrap(), Value::integer(2));
        assert_eq!(stack.pop().unwrap(), Value::integer(1));
        assert!(stack.is_empty());
    }

    #[test]
    fn underflow() {
        let mut stack = Stack::new();
        assert_eq!(stack.pop(), Err(StackError::Underflow));
    }

    #[test]
    fn overflow() {
        let mut stack = Stack::with_max_size(2);
        stack.push(Value::integer(1)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        assert_eq!(stack.push(Value::integer(3)), Err(StackError::Overflow));
    }

    #[test]
    fn peek() {
        let mut stack = Stack::new();
        stack.push(Value::integer(1)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        stack.push(Value::integer(3)).unwrap();

        assert_eq!(stack.peek(0).unwrap(), &Value::integer(3));
        assert_eq!(stack.peek(1).unwrap(), &Value::integer(2));
        assert_eq!(stack.peek(2).unwrap(), &Value::integer(1));
        assert_eq!(stack.peek(3), Err(StackError::InvalidIndex(3)));
    }

    #[test]
    fn dup() {
        let mut stack = Stack::new();
        stack.push(Value::integer(42)).unwrap();
        stack.dup().unwrap();

        assert_eq!(stack.len(), 2);
        assert_eq!(stack.pop().unwrap(), Value::integer(42));
        assert_eq!(stack.pop().unwrap(), Value::integer(42));
    }

    #[test]
    fn swap() {
        let mut stack = Stack::new();
        stack.push(Value::integer(1)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        stack.swap().unwrap();

        assert_eq!(stack.pop().unwrap(), Value::integer(1));
        assert_eq!(stack.pop().unwrap(), Value::integer(2));
    }

    #[test]
    fn rot() {
        let mut stack = Stack::new();
        stack.push(Value::integer(3)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        stack.push(Value::integer(1)).unwrap();
        // Stack: [3, 2, 1] (1 on top)
        stack.rot().unwrap();
        // Stack: [2, 1, 3] (3 on top)

        assert_eq!(stack.pop().unwrap(), Value::integer(3));
        assert_eq!(stack.pop().unwrap(), Value::integer(1));
        assert_eq!(stack.pop().unwrap(), Value::integer(2));
    }

    #[test]
    fn pick() {
        let mut stack = Stack::new();
        stack.push(Value::integer(3)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        stack.push(Value::integer(1)).unwrap();

        stack.pick(2).unwrap(); // Copy level 2 (value 2) to top
        assert_eq!(stack.pop().unwrap(), Value::integer(2));
        assert_eq!(stack.len(), 3);
    }

    #[test]
    fn roll() {
        let mut stack = Stack::new();
        stack.push(Value::integer(3)).unwrap();
        stack.push(Value::integer(2)).unwrap();
        stack.push(Value::integer(1)).unwrap();

        stack.roll(3).unwrap(); // Move level 3 to top
        assert_eq!(stack.pop().unwrap(), Value::integer(3));
        assert_eq!(stack.pop().unwrap(), Value::integer(1));
        assert_eq!(stack.pop().unwrap(), Value::integer(2));
    }
}
