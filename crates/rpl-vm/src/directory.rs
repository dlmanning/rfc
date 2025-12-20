//! Hierarchical directory for variable storage.
//!
//! The directory stores named values in a tree structure with subdirectories.
//! This is distinct from indexed locals, which are scoped to a program execution.
//!
//! # Structure
//!
//! ```text
//! ROOT
//! ├── vars: {x: 42, y: "hello"}
//! └── subdirs:
//!     ├── math/
//!     │   └── vars: {pi: 3.14159}
//!     └── data/
//!         └── vars: {items: {...}}
//! ```

use std::collections::HashMap;

use crate::value::Value;

/// A single directory node containing variables and subdirectories.
#[derive(Clone, Debug, Default)]
pub struct DirNode {
    /// Variables stored in this directory.
    vars: HashMap<String, Value>,
    /// Subdirectories.
    subdirs: HashMap<String, DirNode>,
}

impl DirNode {
    /// Create a new empty directory node.
    pub fn new() -> Self {
        Self::default()
    }

    /// Store a value by name.
    pub fn store(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }

    /// Look up a value by name.
    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.vars.get(name)
    }

    /// Remove a variable by name.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.vars.remove(name)
    }

    /// Check if a variable exists.
    pub fn has_var(&self, name: &str) -> bool {
        self.vars.contains_key(name)
    }

    /// Get an iterator over variable names.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.vars.keys()
    }

    /// Get the number of variables.
    pub fn var_count(&self) -> usize {
        self.vars.len()
    }

    /// Clear all variables (but not subdirectories).
    pub fn clear_vars(&mut self) {
        self.vars.clear();
    }

    /// Rename a variable.
    pub fn rename_var(&mut self, old_name: &str, new_name: &str) -> bool {
        if let Some(value) = self.vars.remove(old_name) {
            self.vars.insert(new_name.to_string(), value);
            true
        } else {
            false
        }
    }

    /// Check if a subdirectory exists.
    pub fn has_subdir(&self, name: &str) -> bool {
        self.subdirs.contains_key(name)
    }

    /// Get a reference to a subdirectory.
    pub fn get_subdir(&self, name: &str) -> Option<&DirNode> {
        self.subdirs.get(name)
    }

    /// Get a mutable reference to a subdirectory.
    pub fn get_subdir_mut(&mut self, name: &str) -> Option<&mut DirNode> {
        self.subdirs.get_mut(name)
    }

    /// Create a subdirectory. Returns false if name already exists (as var or subdir).
    pub fn create_subdir(&mut self, name: String) -> bool {
        if self.vars.contains_key(&name) || self.subdirs.contains_key(&name) {
            return false;
        }
        self.subdirs.insert(name, DirNode::new());
        true
    }

    /// Remove an empty subdirectory. Returns error message if not empty or doesn't exist.
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), String> {
        match self.subdirs.get(name) {
            None => Err(format!("Directory '{}' does not exist", name)),
            Some(subdir) => {
                if !subdir.vars.is_empty() {
                    return Err(format!("Directory '{}' is not empty (has variables)", name));
                }
                if !subdir.subdirs.is_empty() {
                    return Err(format!(
                        "Directory '{}' is not empty (has subdirectories)",
                        name
                    ));
                }
                self.subdirs.remove(name);
                Ok(())
            }
        }
    }

    /// Get an iterator over subdirectory names.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.subdirs.keys()
    }

    /// Get the number of subdirectories.
    pub fn subdir_count(&self) -> usize {
        self.subdirs.len()
    }

    /// Check if this name exists as either a variable or subdirectory.
    pub fn name_exists(&self, name: &str) -> bool {
        self.vars.contains_key(name) || self.subdirs.contains_key(name)
    }

    /// Iterate over (name, value) pairs for all variables.
    pub fn vars_iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.vars.iter()
    }

    /// Iterate over (name, node) pairs for all subdirectories.
    pub fn subdirs_iter(&self) -> impl Iterator<Item = (&String, &DirNode)> {
        self.subdirs.iter()
    }

    /// Get or create a subdirectory, returning a mutable reference.
    pub fn ensure_subdir(&mut self, name: &str) -> &mut DirNode {
        if !self.subdirs.contains_key(name) {
            self.subdirs.insert(name.to_string(), DirNode::new());
        }
        self.subdirs.get_mut(name).unwrap()
    }
}

/// A hierarchical directory with navigation state.
#[derive(Clone, Debug)]
pub struct Directory {
    /// The root directory node.
    root: DirNode,
    /// Current path (list of subdirectory names from root).
    path: Vec<String>,
}

impl Default for Directory {
    fn default() -> Self {
        Self::new()
    }
}

impl Directory {
    /// Create a new empty directory at root.
    pub fn new() -> Self {
        Self {
            root: DirNode::new(),
            path: Vec::new(),
        }
    }

    /// Get a reference to the current directory node.
    fn current(&self) -> &DirNode {
        let mut node = &self.root;
        for name in &self.path {
            node = node.subdirs.get(name).expect("invalid path");
        }
        node
    }

    /// Get a mutable reference to the current directory node.
    fn current_mut(&mut self) -> &mut DirNode {
        let mut node = &mut self.root;
        for name in &self.path {
            node = node.subdirs.get_mut(name).expect("invalid path");
        }
        node
    }

    // === Variable operations (on current directory) ===

    /// Store a value by name in the current directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.current_mut().store(name, value);
    }

    /// Look up a value by name in the current directory.
    pub fn lookup(&self, name: &str) -> Option<&Value> {
        self.current().lookup(name)
    }

    /// Remove a variable by name from the current directory.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.current_mut().purge(name)
    }

    /// Check if a variable exists in the current directory.
    pub fn has_var(&self, name: &str) -> bool {
        self.current().has_var(name)
    }

    /// Get an iterator over variable names in the current directory.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.current().vars()
    }

    /// Get the number of variables in the current directory.
    pub fn var_count(&self) -> usize {
        self.current().var_count()
    }

    /// Clear all variables in the current directory.
    pub fn clear(&mut self) {
        self.current_mut().clear_vars();
    }

    /// Rename a variable in the current directory.
    pub fn rename(&mut self, old_name: &str, new_name: &str) -> bool {
        self.current_mut().rename_var(old_name, new_name)
    }

    // === Directory navigation ===

    /// Create a subdirectory in the current directory.
    /// Returns false if name already exists.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.current_mut().create_subdir(name)
    }

    /// Remove an empty subdirectory from the current directory.
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), String> {
        self.current_mut().remove_subdir(name)
    }

    /// Enter a subdirectory. Returns false if it doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        if self.current().has_subdir(name) {
            self.path.push(name.to_string());
            true
        } else {
            false
        }
    }

    /// Move up one directory level. Does nothing if at root.
    pub fn updir(&mut self) {
        self.path.pop();
    }

    /// Move to root directory.
    pub fn home(&mut self) {
        self.path.clear();
    }

    /// Get the current path as a list of directory names.
    pub fn dir_path(&self) -> &[String] {
        &self.path
    }

    /// Check if we're at the root directory.
    pub fn at_root(&self) -> bool {
        self.path.is_empty()
    }

    /// Check if a subdirectory exists in the current directory.
    pub fn has_subdir(&self, name: &str) -> bool {
        self.current().has_subdir(name)
    }

    /// Get subdirectory names in the current directory.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.current().subdir_names()
    }

    // === Path-based operations (for library data storage) ===

    /// Ensure a path exists, creating subdirectories as needed.
    /// path = ["SETTINGS", "LIBDATA", "TEST"] creates .SETTINGS.LIBDATA.TEST
    /// Returns a mutable reference to the final node.
    pub fn ensure_path(&mut self, path: &[&str]) -> &mut DirNode {
        let mut node = &mut self.root;
        for name in path {
            // Create subdir if it doesn't exist
            if !node.subdirs.contains_key(*name) {
                node.subdirs.insert((*name).to_string(), DirNode::new());
            }
            node = node.subdirs.get_mut(*name).unwrap();
        }
        node
    }

    /// Get a reference to a node at an absolute path, if it exists.
    fn get_node_at_path(&self, path: &[&str]) -> Option<&DirNode> {
        let mut node = &self.root;
        for name in path {
            node = node.subdirs.get(*name)?;
        }
        Some(node)
    }

    /// Get a mutable reference to a node at an absolute path, if it exists.
    fn get_node_at_path_mut(&mut self, path: &[&str]) -> Option<&mut DirNode> {
        let mut node = &mut self.root;
        for name in path {
            node = node.subdirs.get_mut(*name)?;
        }
        Some(node)
    }

    /// Store a value at an absolute path (creates intermediate directories).
    /// path = ["SETTINGS", "LIBDATA", "TEST"], name = "myvar"
    /// stores at .SETTINGS.LIBDATA.TEST.myvar
    pub fn store_at_path(&mut self, path: &[&str], name: &str, value: Value) {
        let node = self.ensure_path(path);
        node.store(name.to_string(), value);
    }

    /// Look up a value at an absolute path.
    pub fn lookup_at_path(&self, path: &[&str], name: &str) -> Option<&Value> {
        self.get_node_at_path(path)?.lookup(name)
    }

    /// Purge a value at an absolute path.
    pub fn purge_at_path(&mut self, path: &[&str], name: &str) -> Option<Value> {
        self.get_node_at_path_mut(path)?.purge(name)
    }

    /// Clear all variables at an absolute path (but not subdirectories).
    pub fn clear_at_path(&mut self, path: &[&str]) {
        if let Some(node) = self.get_node_at_path_mut(path) {
            node.clear_vars();
        }
    }

    /// Iterate over all variables at an absolute path.
    /// Returns (name, value) pairs.
    pub fn vars_at_path(&self, path: &[&str]) -> impl Iterator<Item = (&String, &Value)> {
        self.get_node_at_path(path)
            .map(|node| node.vars.iter())
            .into_iter()
            .flatten()
    }

    /// Get a reference to the current directory node (for packing).
    pub fn current_node(&self) -> &DirNode {
        self.current()
    }

    /// Get a mutable reference to the current directory node (for unpacking).
    pub fn current_node_mut(&mut self) -> &mut DirNode {
        self.current_mut()
    }

    /// Get a subdirectory node by name from the current directory.
    pub fn get_subdir(&self, name: &str) -> Option<&DirNode> {
        self.current().get_subdir(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn store_and_lookup() {
        let mut dir = Directory::new();
        dir.store("x".to_string(), Value::integer(42));

        assert_eq!(dir.lookup("x"), Some(&Value::integer(42)));
        assert_eq!(dir.lookup("y"), None);
    }

    #[test]
    fn purge() {
        let mut dir = Directory::new();
        dir.store("x".to_string(), Value::integer(42));

        assert_eq!(dir.purge("x"), Some(Value::integer(42)));
        assert_eq!(dir.purge("x"), None);
        assert_eq!(dir.lookup("x"), None);
    }

    #[test]
    fn has_var() {
        let mut dir = Directory::new();
        dir.store("x".to_string(), Value::integer(42));

        assert!(dir.has_var("x"));
        assert!(!dir.has_var("y"));
    }

    #[test]
    fn clear() {
        let mut dir = Directory::new();
        dir.store("x".to_string(), Value::integer(1));
        dir.store("y".to_string(), Value::integer(2));

        assert_eq!(dir.var_count(), 2);
        dir.clear();
        assert_eq!(dir.var_count(), 0);
    }

    #[test]
    fn rename() {
        let mut dir = Directory::new();
        dir.store("old".to_string(), Value::integer(42));

        assert!(dir.rename("old", "new"));
        assert!(!dir.has_var("old"));
        assert_eq!(dir.lookup("new"), Some(&Value::integer(42)));

        // Renaming non-existent returns false
        assert!(!dir.rename("nonexistent", "other"));
    }

    // === Hierarchical directory tests ===

    #[test]
    fn create_subdir() {
        let mut dir = Directory::new();
        assert!(dir.create_subdir("sub".to_string()));
        assert!(dir.has_subdir("sub"));

        // Can't create duplicate
        assert!(!dir.create_subdir("sub".to_string()));

        // Can't create subdir with same name as variable
        dir.store("x".to_string(), Value::integer(1));
        assert!(!dir.create_subdir("x".to_string()));
    }

    #[test]
    fn enter_subdir() {
        let mut dir = Directory::new();
        dir.create_subdir("sub".to_string());

        assert!(dir.enter_subdir("sub"));
        assert_eq!(dir.dir_path(), &["sub".to_string()]);

        // Can't enter non-existent
        assert!(!dir.enter_subdir("nonexistent"));
    }

    #[test]
    fn updir_and_home() {
        let mut dir = Directory::new();
        dir.create_subdir("a".to_string());
        dir.enter_subdir("a");
        dir.create_subdir("b".to_string());
        dir.enter_subdir("b");

        assert_eq!(dir.dir_path(), &["a".to_string(), "b".to_string()]);

        dir.updir();
        assert_eq!(dir.dir_path(), &["a".to_string()]);

        dir.home();
        assert!(dir.at_root());
        assert!(dir.dir_path().is_empty());
    }

    #[test]
    fn vars_in_subdirs() {
        let mut dir = Directory::new();
        dir.store("root_var".to_string(), Value::integer(1));

        dir.create_subdir("sub".to_string());
        dir.enter_subdir("sub");
        dir.store("sub_var".to_string(), Value::integer(2));

        // In subdir, can see sub_var but not root_var
        assert!(dir.has_var("sub_var"));
        assert!(!dir.has_var("root_var"));

        dir.updir();

        // At root, can see root_var but not sub_var
        assert!(dir.has_var("root_var"));
        assert!(!dir.has_var("sub_var"));
    }

    #[test]
    fn remove_empty_subdir() {
        let mut dir = Directory::new();
        dir.create_subdir("empty".to_string());

        assert!(dir.remove_subdir("empty").is_ok());
        assert!(!dir.has_subdir("empty"));
    }

    #[test]
    fn remove_nonempty_subdir_fails() {
        let mut dir = Directory::new();
        dir.create_subdir("nonempty".to_string());
        dir.enter_subdir("nonempty");
        dir.store("x".to_string(), Value::integer(1));
        dir.updir();

        let result = dir.remove_subdir("nonempty");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not empty"));
    }

    #[test]
    fn remove_nonexistent_subdir_fails() {
        let mut dir = Directory::new();
        let result = dir.remove_subdir("nonexistent");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("does not exist"));
    }

    #[test]
    fn nested_subdirs() {
        let mut dir = Directory::new();
        dir.create_subdir("a".to_string());
        dir.enter_subdir("a");
        dir.create_subdir("b".to_string());
        dir.enter_subdir("b");
        dir.create_subdir("c".to_string());
        dir.enter_subdir("c");

        dir.store("deep".to_string(), Value::integer(42));
        assert_eq!(dir.lookup("deep"), Some(&Value::integer(42)));

        dir.home();
        assert!(!dir.has_var("deep"));
    }

    // === Path-based operation tests ===

    #[test]
    fn store_at_path_creates_dirs() {
        let mut dir = Directory::new();

        // Store at .SETTINGS.LIBDATA.TEST.myvar
        dir.store_at_path(&["SETTINGS", "LIBDATA", "TEST"], "myvar", Value::integer(42));

        // Should be able to look it up
        assert_eq!(
            dir.lookup_at_path(&["SETTINGS", "LIBDATA", "TEST"], "myvar"),
            Some(&Value::integer(42))
        );

        // Intermediate directories should exist
        assert!(dir.has_subdir("SETTINGS"));
    }

    #[test]
    fn lookup_at_path_nonexistent() {
        let dir = Directory::new();

        // Looking up at nonexistent path should return None
        assert_eq!(dir.lookup_at_path(&["SETTINGS", "LIBDATA"], "myvar"), None);
    }

    #[test]
    fn purge_at_path() {
        let mut dir = Directory::new();

        dir.store_at_path(&["A", "B"], "x", Value::integer(99));
        assert_eq!(
            dir.purge_at_path(&["A", "B"], "x"),
            Some(Value::integer(99))
        );
        assert_eq!(dir.lookup_at_path(&["A", "B"], "x"), None);
    }

    #[test]
    fn clear_at_path() {
        let mut dir = Directory::new();

        dir.store_at_path(&["LIB", "DATA"], "a", Value::integer(1));
        dir.store_at_path(&["LIB", "DATA"], "b", Value::integer(2));
        dir.store_at_path(&["LIB", "DATA"], "c", Value::integer(3));

        dir.clear_at_path(&["LIB", "DATA"]);

        assert_eq!(dir.lookup_at_path(&["LIB", "DATA"], "a"), None);
        assert_eq!(dir.lookup_at_path(&["LIB", "DATA"], "b"), None);
        assert_eq!(dir.lookup_at_path(&["LIB", "DATA"], "c"), None);
    }

    #[test]
    fn path_isolation() {
        let mut dir = Directory::new();

        // Store in two different library namespaces
        dir.store_at_path(&["LIBDATA", "LIB1"], "value", Value::integer(100));
        dir.store_at_path(&["LIBDATA", "LIB2"], "value", Value::integer(200));

        // Each should see its own value
        assert_eq!(
            dir.lookup_at_path(&["LIBDATA", "LIB1"], "value"),
            Some(&Value::integer(100))
        );
        assert_eq!(
            dir.lookup_at_path(&["LIBDATA", "LIB2"], "value"),
            Some(&Value::integer(200))
        );

        // Clearing LIB1 should not affect LIB2
        dir.clear_at_path(&["LIBDATA", "LIB1"]);
        assert_eq!(dir.lookup_at_path(&["LIBDATA", "LIB1"], "value"), None);
        assert_eq!(
            dir.lookup_at_path(&["LIBDATA", "LIB2"], "value"),
            Some(&Value::integer(200))
        );
    }
}
