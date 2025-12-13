use std::collections::HashMap;

use crate::value::Value;

/// A single directory node in the hierarchy.
#[derive(Clone, Debug, Default)]
pub struct Directory {
    /// Variables stored in this directory.
    vars: HashMap<String, Value>,
    /// Subdirectories.
    subdirs: HashMap<String, Directory>,
}

impl Directory {
    /// Create a new empty directory.
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            subdirs: HashMap::new(),
        }
    }

    /// Store a variable in this directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }

    /// Recall a variable from this directory.
    pub fn recall(&self, name: &str) -> Option<&Value> {
        self.vars.get(name)
    }

    /// Purge a variable from this directory.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.vars.remove(name)
    }

    /// Check if a variable exists in this directory.
    pub fn has_var(&self, name: &str) -> bool {
        self.vars.contains_key(name)
    }

    /// Get all variable names in this directory.
    pub fn var_names(&self) -> impl Iterator<Item = &String> {
        self.vars.keys()
    }

    /// Clear all variables from this directory.
    pub fn clear_vars(&mut self) {
        self.vars.clear();
    }

    /// Get the number of variables in this directory.
    pub fn var_count(&self) -> usize {
        self.vars.len()
    }

    /// Create a subdirectory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        if self.subdirs.contains_key(&name) || self.vars.contains_key(&name) {
            false // Already exists
        } else {
            self.subdirs.insert(name, Directory::new());
            true
        }
    }

    /// Check if a subdirectory exists.
    pub fn has_subdir(&self, name: &str) -> bool {
        self.subdirs.contains_key(name)
    }

    /// Get a subdirectory.
    pub fn get_subdir(&self, name: &str) -> Option<&Directory> {
        self.subdirs.get(name)
    }

    /// Get a mutable subdirectory.
    pub fn get_subdir_mut(&mut self, name: &str) -> Option<&mut Directory> {
        self.subdirs.get_mut(name)
    }

    /// Remove a subdirectory (only if empty).
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), &'static str> {
        if let Some(dir) = self.subdirs.get(name) {
            if !dir.vars.is_empty() || !dir.subdirs.is_empty() {
                return Err("Directory not empty");
            }
            self.subdirs.remove(name);
            Ok(())
        } else {
            Err("Directory not found")
        }
    }

    /// Get all subdirectory names.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.subdirs.keys()
    }

    /// Check if this directory is empty (no vars and no subdirs).
    pub fn is_empty(&self) -> bool {
        self.vars.is_empty() && self.subdirs.is_empty()
    }

    /// Get a snapshot of all variables (for debugging).
    pub fn vars_snapshot(&self) -> HashMap<String, Value> {
        self.vars.clone()
    }
}

/// The full directory tree with current position tracking.
#[derive(Clone, Debug)]
pub struct DirectoryTree {
    /// Root directory (HOME).
    root: Directory,
    /// Path from root to current directory (list of names).
    /// Empty = at root (HOME).
    current_path: Vec<String>,
}

impl Default for DirectoryTree {
    fn default() -> Self {
        Self::new()
    }
}

impl DirectoryTree {
    /// Create a new directory tree with empty root.
    pub fn new() -> Self {
        Self {
            root: Directory::new(),
            current_path: Vec::new(),
        }
    }

    /// Get the current directory path.
    pub fn path(&self) -> &[String] {
        &self.current_path
    }

    /// Check if we're at the root (HOME) directory.
    pub fn is_at_home(&self) -> bool {
        self.current_path.is_empty()
    }

    /// Navigate to home (root) directory.
    pub fn home(&mut self) {
        self.current_path.clear();
    }

    /// Navigate up one directory level.
    /// Returns false if already at root.
    pub fn updir(&mut self) -> bool {
        if self.current_path.is_empty() {
            false
        } else {
            self.current_path.pop();
            true
        }
    }

    /// Get a reference to the current directory.
    pub fn current_dir(&self) -> &Directory {
        let mut dir = &self.root;
        for name in &self.current_path {
            dir = dir.subdirs.get(name).expect("Invalid path state");
        }
        dir
    }

    /// Get a mutable reference to the current directory.
    pub fn current_dir_mut(&mut self) -> &mut Directory {
        let mut dir = &mut self.root;
        for name in &self.current_path {
            dir = dir.subdirs.get_mut(name).expect("Invalid path state");
        }
        dir
    }

    /// Store a variable in the current directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.current_dir_mut().store(name, value);
    }

    /// Recall a variable from the current directory.
    pub fn recall(&self, name: &str) -> Option<&Value> {
        self.current_dir().recall(name)
    }

    /// Purge a variable from the current directory.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.current_dir_mut().purge(name)
    }

    /// Check if a variable exists in the current directory.
    pub fn has_var(&self, name: &str) -> bool {
        self.current_dir().has_var(name)
    }

    /// Get all variable names in the current directory.
    pub fn var_names(&self) -> impl Iterator<Item = &String> {
        self.current_dir().var_names()
    }

    /// Clear all variables from the current directory.
    pub fn clear_vars(&mut self) {
        self.current_dir_mut().clear_vars();
    }

    /// Create a subdirectory in the current directory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.current_dir_mut().create_subdir(name)
    }

    /// Navigate into a subdirectory.
    /// Returns false if the subdirectory doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        if self.current_dir().has_subdir(name) {
            self.current_path.push(name.to_string());
            true
        } else {
            false
        }
    }

    /// Remove a subdirectory from the current directory (must be empty).
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), &'static str> {
        self.current_dir_mut().remove_subdir(name)
    }

    /// Get all subdirectory names in the current directory.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.current_dir().subdir_names()
    }

    /// Get a snapshot of all variables in the current directory.
    pub fn vars_snapshot(&self) -> HashMap<String, Value> {
        self.current_dir().vars_snapshot()
    }
}
