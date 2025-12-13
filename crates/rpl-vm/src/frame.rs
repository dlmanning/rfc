use std::collections::HashMap;

use crate::value::Value;

/// A local variable frame for → bindings.
#[derive(Clone, Debug, Default)]
pub struct LocalFrame {
    /// Variable bindings in this frame (name -> value).
    pub(crate) bindings: HashMap<String, Value>,
}

impl LocalFrame {
    /// Create a new empty local frame.
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    /// Bind a variable in this frame.
    pub fn bind(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    /// Get a variable from this frame.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }

    /// Check if this frame has a variable.
    pub fn has(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    /// Set a variable in this frame (same as bind but takes &str).
    pub fn set(&mut self, name: &str, value: Value) {
        self.bindings.insert(name.to_string(), value);
    }

    /// Increment the first (and usually only) integer variable in this frame.
    /// Used by FOR loops to update the loop counter.
    pub fn increment_first(&mut self) {
        if let Some(value) = self.bindings.values_mut().next() {
            if let Value::Int(i) = value {
                *i += 1;
            } else if let Value::Real(r) = value {
                *r += 1.0;
            }
        }
    }

    /// Get the value of the first variable as i64.
    /// Used by FOR loops to read the loop counter.
    pub fn get_first(&self) -> i64 {
        if let Some(value) = self.bindings.values().next() {
            match value {
                Value::Int(i) => *i,
                Value::Real(r) => *r as i64,
                _ => 0,
            }
        } else {
            0
        }
    }

    /// Set the value of the first variable.
    /// Used by FOR loops to update the loop counter.
    pub fn set_first(&mut self, new_value: i64) {
        if let Some(value) = self.bindings.values_mut().next() {
            match value {
                Value::Int(i) => *i = new_value,
                Value::Real(r) => *r = new_value as f64,
                _ => {}
            }
        }
    }
}
