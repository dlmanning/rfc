//! Symbol table for tracking definitions and references.
//!
//! This module provides types for tracking where variables are defined
//! and where they are referenced throughout a program.

use crate::core::Span;

use super::scopes::ScopeId;
use super::Type;
use crate::types::{ConstraintSource, Signature};

/// Unique identifier for a definition.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct DefinitionId(pub u32);

impl DefinitionId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// Unique identifier for a reference.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ReferenceId(pub u32);

impl ReferenceId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// The kind of definition.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DefinitionKind {
    /// Global variable (created via STO).
    Global,
    /// Local variable (created via → x « »).
    Local,
    /// Loop variable (created via FOR i).
    LoopVar,
}

/// The kind of reference.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReferenceKind {
    /// Reading a variable (via RCL or implicit identifier).
    Read,
    /// Writing to a variable (via STO).
    Write,
    /// Deleting a variable (via PURGE).
    Delete,
    /// Incrementing a variable (via INCR).
    Increment,
    /// Decrementing a variable (via DECR).
    Decrement,
}

/// A variable definition.
#[derive(Clone, Debug)]
pub struct Definition {
    /// Unique ID for this definition.
    pub id: DefinitionId,
    /// The variable name.
    pub name: String,
    /// Source span where the definition occurs.
    pub span: Span,
    /// Kind of definition.
    pub kind: DefinitionKind,
    /// Scope containing this definition.
    pub scope: ScopeId,
    /// Whether this definition has been referenced.
    pub referenced: bool,
    /// Inferred type of the value stored to this variable, if known.
    pub value_type: Option<Type>,
    /// For functions: the number of parameters (arity).
    pub arity: Option<usize>,
    /// Type constraints from usage sites (for constraint-based inference).
    pub usage_constraints: Vec<ConstraintSource>,
    /// For locals/loop vars: the local index assigned during parsing.
    /// Used by lowering to look up type information.
    pub local_index: Option<usize>,
    /// For functions: inferred signature (inputs → outputs).
    /// Only set for programs with explicit local bindings.
    pub signature: Option<Signature>,
}

impl Definition {
    /// Create a new definition.
    pub fn new(name: String, span: Span, kind: DefinitionKind, scope: ScopeId) -> Self {
        Self {
            id: DefinitionId::new(0), // Will be set when added to table
            name,
            span,
            kind,
            scope,
            referenced: false,
            value_type: None,
            arity: None,
            usage_constraints: Vec::new(),
            local_index: None,
            signature: None,
        }
    }

    /// Create a new definition with a known value type.
    pub fn with_type(
        name: String,
        span: Span,
        kind: DefinitionKind,
        scope: ScopeId,
        value_type: Type,
    ) -> Self {
        Self {
            id: DefinitionId::new(0),
            name,
            span,
            kind,
            scope,
            referenced: false,
            value_type: Some(value_type),
            arity: None,
            usage_constraints: Vec::new(),
            local_index: None,
            signature: None,
        }
    }

    /// Create a new definition with a known value type and arity.
    pub fn with_type_and_arity(
        name: String,
        span: Span,
        kind: DefinitionKind,
        scope: ScopeId,
        value_type: Type,
        arity: usize,
    ) -> Self {
        Self {
            id: DefinitionId::new(0),
            name,
            span,
            kind,
            scope,
            referenced: false,
            value_type: Some(value_type),
            arity: Some(arity),
            usage_constraints: Vec::new(),
            local_index: None,
            signature: None,
        }
    }

    /// Create a new definition with a local index.
    pub fn with_local_index(
        name: String,
        span: Span,
        kind: DefinitionKind,
        scope: ScopeId,
        local_index: usize,
    ) -> Self {
        Self {
            id: DefinitionId::new(0),
            name,
            span,
            kind,
            scope,
            referenced: false,
            value_type: None,
            arity: None,
            usage_constraints: Vec::new(),
            local_index: Some(local_index),
            signature: None,
        }
    }

    /// Add a type constraint from a usage site.
    pub fn add_constraint(&mut self, constraint: ConstraintSource) {
        self.usage_constraints.push(constraint);
    }
}

/// A variable reference.
#[derive(Clone, Debug)]
pub struct Reference {
    /// Unique ID for this reference.
    pub id: ReferenceId,
    /// The variable name.
    pub name: String,
    /// Source span where the reference occurs.
    pub span: Span,
    /// Kind of reference.
    pub kind: ReferenceKind,
    /// Scope containing this reference.
    pub scope: ScopeId,
    /// The definition this reference resolves to, if any.
    pub definition: Option<DefinitionId>,
}

impl Reference {
    /// Create a new reference.
    pub fn new(name: String, span: Span, kind: ReferenceKind, scope: ScopeId) -> Self {
        Self {
            id: ReferenceId::new(0), // Will be set when added to table
            name,
            span,
            kind,
            scope,
            definition: None,
        }
    }

    /// Check if this reference is resolved.
    pub fn is_resolved(&self) -> bool {
        self.definition.is_some()
    }
}

/// Symbol table containing all definitions and references.
#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    definitions: Vec<Definition>,
    references: Vec<Reference>,
}

impl SymbolTable {
    /// Create a new empty symbol table.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a definition to the table.
    pub fn add_definition(&mut self, mut def: Definition) -> DefinitionId {
        let id = DefinitionId::new(self.definitions.len() as u32);
        def.id = id;
        self.definitions.push(def);
        id
    }

    /// Add a reference to the table.
    pub fn add_reference(&mut self, mut reference: Reference) -> ReferenceId {
        let id = ReferenceId::new(self.references.len() as u32);
        reference.id = id;
        self.references.push(reference);
        id
    }

    /// Get a definition by ID.
    pub fn get_definition(&self, id: DefinitionId) -> Option<&Definition> {
        self.definitions.get(id.as_u32() as usize)
    }

    /// Get a mutable definition by ID.
    pub fn get_definition_mut(&mut self, id: DefinitionId) -> Option<&mut Definition> {
        self.definitions.get_mut(id.as_u32() as usize)
    }

    /// Get a reference by ID.
    pub fn get_reference(&self, id: ReferenceId) -> Option<&Reference> {
        self.references.get(id.as_u32() as usize)
    }

    /// Get a mutable reference by ID.
    pub fn get_reference_mut(&mut self, id: ReferenceId) -> Option<&mut Reference> {
        self.references.get_mut(id.as_u32() as usize)
    }

    /// Iterate over all definitions.
    pub fn definitions(&self) -> impl Iterator<Item = &Definition> {
        self.definitions.iter()
    }

    /// Iterate over all references.
    pub fn references(&self) -> impl Iterator<Item = &Reference> {
        self.references.iter()
    }

    /// Get the number of definitions.
    pub fn definition_count(&self) -> usize {
        self.definitions.len()
    }

    /// Get the number of references.
    pub fn reference_count(&self) -> usize {
        self.references.len()
    }

    /// Find definitions by name.
    pub fn find_definitions_by_name<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &'a Definition> + 'a {
        self.definitions.iter().filter(move |d| d.name == name)
    }

    /// Find a definition by name in a specific scope.
    pub fn find_definition(&self, name: &str, scope: ScopeId) -> Option<&Definition> {
        self.definitions
            .iter()
            .find(|d| d.name == name && d.scope == scope)
    }

    /// Find a local definition by its local index.
    ///
    /// Used by lowering to get type information for local variables.
    pub fn find_by_local_index(&self, index: usize) -> Option<&Definition> {
        self.definitions
            .iter()
            .find(|d| d.local_index == Some(index))
    }

    /// Find references to a specific definition.
    pub fn references_to(&self, def_id: DefinitionId) -> impl Iterator<Item = &Reference> {
        self.references
            .iter()
            .filter(move |r| r.definition == Some(def_id))
    }

    /// Resolve a reference to a definition.
    pub fn resolve_reference(&mut self, ref_id: ReferenceId, def_id: DefinitionId) {
        if let Some(reference) = self.get_reference_mut(ref_id) {
            reference.definition = Some(def_id);
        }
        if let Some(definition) = self.get_definition_mut(def_id) {
            definition.referenced = true;
        }
    }

    /// Get unresolved references.
    pub fn unresolved_references(&self) -> impl Iterator<Item = &Reference> {
        self.references.iter().filter(|r| r.definition.is_none())
    }

    /// Get unreferenced definitions.
    pub fn unreferenced_definitions(&self) -> impl Iterator<Item = &Definition> {
        self.definitions.iter().filter(|d| !d.referenced)
    }

    /// Get definitions in a specific scope.
    pub fn definitions_in_scope(&self, scope: ScopeId) -> impl Iterator<Item = &Definition> {
        self.definitions.iter().filter(move |d| d.scope == scope)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn definition_id() {
        let id = DefinitionId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn reference_id() {
        let id = ReferenceId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn symbol_table_empty() {
        let table = SymbolTable::new();
        assert_eq!(table.definition_count(), 0);
        assert_eq!(table.reference_count(), 0);
    }

    #[test]
    fn add_definition() {
        let mut table = SymbolTable::new();
        let def = Definition::new(
            "x".to_string(),
            make_span(0, 5),
            DefinitionKind::Global,
            ScopeId::root(),
        );
        let id = table.add_definition(def);

        assert_eq!(id.as_u32(), 0);
        assert_eq!(table.definition_count(), 1);

        let retrieved = table.get_definition(id).unwrap();
        assert_eq!(retrieved.name, "x");
        assert_eq!(retrieved.kind, DefinitionKind::Global);
    }

    #[test]
    fn add_reference() {
        let mut table = SymbolTable::new();
        let reference = Reference::new(
            "x".to_string(),
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::root(),
        );
        let id = table.add_reference(reference);

        assert_eq!(id.as_u32(), 0);
        assert_eq!(table.reference_count(), 1);

        let retrieved = table.get_reference(id).unwrap();
        assert_eq!(retrieved.name, "x");
        assert!(!retrieved.is_resolved());
    }

    #[test]
    fn resolve_reference() {
        let mut table = SymbolTable::new();

        let def_id = table.add_definition(Definition::new(
            "x".to_string(),
            make_span(0, 5),
            DefinitionKind::Global,
            ScopeId::root(),
        ));

        let ref_id = table.add_reference(Reference::new(
            "x".to_string(),
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::root(),
        ));

        assert!(!table.get_reference(ref_id).unwrap().is_resolved());
        assert!(!table.get_definition(def_id).unwrap().referenced);

        table.resolve_reference(ref_id, def_id);

        assert!(table.get_reference(ref_id).unwrap().is_resolved());
        assert!(table.get_definition(def_id).unwrap().referenced);
    }

    #[test]
    fn find_definition_by_name() {
        let mut table = SymbolTable::new();

        table.add_definition(Definition::new(
            "x".to_string(),
            make_span(0, 5),
            DefinitionKind::Global,
            ScopeId::root(),
        ));
        table.add_definition(Definition::new(
            "y".to_string(),
            make_span(10, 15),
            DefinitionKind::Local,
            ScopeId::new(1),
        ));

        let found = table.find_definition("x", ScopeId::root());
        assert!(found.is_some());
        assert_eq!(found.unwrap().kind, DefinitionKind::Global);

        let not_found = table.find_definition("x", ScopeId::new(1));
        assert!(not_found.is_none());
    }

    #[test]
    fn unresolved_references() {
        let mut table = SymbolTable::new();

        let def_id = table.add_definition(Definition::new(
            "x".to_string(),
            make_span(0, 5),
            DefinitionKind::Global,
            ScopeId::root(),
        ));

        let ref1_id = table.add_reference(Reference::new(
            "x".to_string(),
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::root(),
        ));
        table.add_reference(Reference::new(
            "y".to_string(), // Unresolved
            make_span(20, 25),
            ReferenceKind::Read,
            ScopeId::root(),
        ));

        table.resolve_reference(ref1_id, def_id);

        let unresolved: Vec<_> = table.unresolved_references().collect();
        assert_eq!(unresolved.len(), 1);
        assert_eq!(unresolved[0].name, "y");
    }
}
