use rpl_core::{Span, Symbol, TypeId};

use super::scopes::ScopeId;

/// Definition identifier.
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

/// Reference identifier.
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

/// Kind of symbol definition.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DefinitionKind {
    /// Global variable (STO).
    GlobalVariable,
    /// Local variable (→).
    LocalVariable,
    /// Function parameter.
    Parameter,
    /// Loop variable (FOR).
    LoopVariable,
}

/// Kind of symbol reference.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ReferenceKind {
    /// Reading a value (RCL or implicit).
    Read,
    /// Writing a value (STO).
    Write,
    /// Modifying a value (STO+, etc.).
    Modify,
    /// Deleting a variable (PURGE).
    Delete,
}

/// A symbol definition.
#[derive(Clone, Debug)]
pub struct Definition {
    pub id: DefinitionId,
    pub name: Symbol,
    pub span: Span,
    pub kind: DefinitionKind,
    pub scope: ScopeId,
    /// The type of value stored to this variable, if known.
    pub value_type: Option<TypeId>,
}

impl Definition {
    pub fn new(
        id: DefinitionId,
        name: Symbol,
        span: Span,
        kind: DefinitionKind,
        scope: ScopeId,
    ) -> Self {
        Self {
            id,
            name,
            span,
            kind,
            scope,
            value_type: None,
        }
    }

    /// Create a definition with a known value type.
    pub fn with_type(
        id: DefinitionId,
        name: Symbol,
        span: Span,
        kind: DefinitionKind,
        scope: ScopeId,
        value_type: TypeId,
    ) -> Self {
        Self {
            id,
            name,
            span,
            kind,
            scope,
            value_type: Some(value_type),
        }
    }
}

/// A symbol reference.
#[derive(Clone, Debug)]
pub struct Reference {
    pub id: ReferenceId,
    pub name: Symbol,
    pub span: Span,
    pub kind: ReferenceKind,
    pub scope: ScopeId,
    pub definition: Option<DefinitionId>,
}

impl Reference {
    pub fn new(
        id: ReferenceId,
        name: Symbol,
        span: Span,
        kind: ReferenceKind,
        scope: ScopeId,
    ) -> Self {
        Self {
            id,
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

/// Symbol table containing definitions and references.
pub struct SymbolTable {
    definitions: Vec<Definition>,
    references: Vec<Reference>,
}

impl SymbolTable {
    /// Create a new empty symbol table.
    pub fn new() -> Self {
        Self {
            definitions: Vec::new(),
            references: Vec::new(),
        }
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

    /// Get a reference by ID.
    pub fn get_reference(&self, id: ReferenceId) -> Option<&Reference> {
        self.references.get(id.as_u32() as usize)
    }

    /// Get a mutable reference by ID.
    pub fn get_reference_mut(&mut self, id: ReferenceId) -> Option<&mut Reference> {
        self.references.get_mut(id.as_u32() as usize)
    }

    /// Iterate over definitions in a specific scope.
    pub fn definitions_in_scope(&self, scope: ScopeId) -> impl Iterator<Item = &Definition> {
        self.definitions.iter().filter(move |d| d.scope == scope)
    }

    /// Iterate over all definitions.
    pub fn definitions(&self) -> impl Iterator<Item = &Definition> {
        self.definitions.iter()
    }

    /// Iterate over all references.
    pub fn references(&self) -> impl Iterator<Item = &Reference> {
        self.references.iter()
    }

    /// Iterate over references to a specific definition.
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
    }

    /// Get the number of definitions.
    pub fn definition_count(&self) -> usize {
        self.definitions.len()
    }

    /// Get the number of references.
    pub fn reference_count(&self) -> usize {
        self.references.len()
    }

    /// Find definitions by name in a scope.
    pub fn find_definition(&self, name: Symbol, scope: ScopeId) -> Option<&Definition> {
        self.definitions
            .iter()
            .find(|d| d.name == name && d.scope == scope)
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos};

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn definition_id_new() {
        let id = DefinitionId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn reference_id_new() {
        let id = ReferenceId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn symbol_table_new() {
        let table = SymbolTable::new();
        assert_eq!(table.definition_count(), 0);
        assert_eq!(table.reference_count(), 0);
    }

    #[test]
    fn symbol_table_add_definition() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym = interner.intern("x");

        let def = Definition::new(
            DefinitionId::new(0),
            sym,
            make_span(0, 5),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        );

        let id = table.add_definition(def);
        assert_eq!(id.as_u32(), 0);
        assert_eq!(table.definition_count(), 1);

        let retrieved = table.get_definition(id).unwrap();
        assert_eq!(retrieved.name, sym);
        assert_eq!(retrieved.kind, DefinitionKind::GlobalVariable);
    }

    #[test]
    fn symbol_table_add_reference() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym = interner.intern("x");

        let reference = Reference::new(
            ReferenceId::new(0),
            sym,
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::new(0),
        );

        let id = table.add_reference(reference);
        assert_eq!(id.as_u32(), 0);
        assert_eq!(table.reference_count(), 1);

        let retrieved = table.get_reference(id).unwrap();
        assert_eq!(retrieved.name, sym);
        assert_eq!(retrieved.kind, ReferenceKind::Read);
        assert!(!retrieved.is_resolved());
    }

    #[test]
    fn symbol_table_definitions_in_scope() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym1 = interner.intern("a");
        let sym2 = interner.intern("b");
        let sym3 = interner.intern("c");

        // Add definitions in different scopes
        table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym1,
            make_span(0, 5),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        ));
        table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym2,
            make_span(10, 15),
            DefinitionKind::LocalVariable,
            ScopeId::new(1),
        ));
        table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym3,
            make_span(20, 25),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        ));

        let scope0_defs: Vec<_> = table.definitions_in_scope(ScopeId::new(0)).collect();
        assert_eq!(scope0_defs.len(), 2);

        let scope1_defs: Vec<_> = table.definitions_in_scope(ScopeId::new(1)).collect();
        assert_eq!(scope1_defs.len(), 1);
        assert_eq!(scope1_defs[0].name, sym2);
    }

    #[test]
    fn symbol_table_resolve_reference() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym = interner.intern("x");

        let def_id = table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym,
            make_span(0, 5),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        ));

        let ref_id = table.add_reference(Reference::new(
            ReferenceId::new(0),
            sym,
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::new(0),
        ));

        // Initially unresolved
        assert!(!table.get_reference(ref_id).unwrap().is_resolved());

        // Resolve
        table.resolve_reference(ref_id, def_id);

        // Now resolved
        let reference = table.get_reference(ref_id).unwrap();
        assert!(reference.is_resolved());
        assert_eq!(reference.definition, Some(def_id));
    }

    #[test]
    fn symbol_table_references_to() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym1 = interner.intern("x");
        let sym2 = interner.intern("y");

        let def_id = table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym1,
            make_span(0, 5),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        ));

        // Add some references
        let ref1_id = table.add_reference(Reference::new(
            ReferenceId::new(0),
            sym1,
            make_span(10, 15),
            ReferenceKind::Read,
            ScopeId::new(0),
        ));
        let _ref2_id = table.add_reference(Reference::new(
            ReferenceId::new(0),
            sym2, // Different name
            make_span(20, 25),
            ReferenceKind::Read,
            ScopeId::new(0),
        ));
        let ref3_id = table.add_reference(Reference::new(
            ReferenceId::new(0),
            sym1,
            make_span(30, 35),
            ReferenceKind::Write,
            ScopeId::new(0),
        ));

        // Resolve ref1 and ref3 to def
        table.resolve_reference(ref1_id, def_id);
        table.resolve_reference(ref3_id, def_id);

        // Get references to def
        let refs: Vec<_> = table.references_to(def_id).collect();
        assert_eq!(refs.len(), 2);
    }

    #[test]
    fn symbol_table_find_definition() {
        let mut table = SymbolTable::new();
        let mut interner = Interner::new();
        let sym1 = interner.intern("x");
        let sym2 = interner.intern("y");
        let sym_missing = interner.intern("z");

        table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym1,
            make_span(0, 5),
            DefinitionKind::GlobalVariable,
            ScopeId::new(0),
        ));
        table.add_definition(Definition::new(
            DefinitionId::new(0),
            sym2,
            make_span(10, 15),
            DefinitionKind::LocalVariable,
            ScopeId::new(1),
        ));

        // Find existing
        let found = table.find_definition(sym1, ScopeId::new(0));
        assert!(found.is_some());
        assert_eq!(found.unwrap().kind, DefinitionKind::GlobalVariable);

        // Find in different scope
        let found = table.find_definition(sym2, ScopeId::new(1));
        assert!(found.is_some());

        // Not found - wrong scope
        let not_found = table.find_definition(sym1, ScopeId::new(1));
        assert!(not_found.is_none());

        // Not found - wrong name
        let not_found = table.find_definition(sym_missing, ScopeId::new(0));
        assert!(not_found.is_none());
    }
}
