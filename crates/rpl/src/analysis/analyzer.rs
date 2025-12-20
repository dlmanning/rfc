//! Main analysis logic.
//!
//! This module implements the static analyzer that walks the IR tree
//! and builds symbol tables, scope trees, and diagnostics.

use std::collections::HashMap;

use super::{
    result::{AnalysisResult, Diagnostic},
    scopes::{Scope, ScopeId, ScopeKind, ScopeTree},
    symbols::{Definition, DefinitionId, DefinitionKind, Reference, ReferenceKind, SymbolTable},
    visitor::{Visitor, walk_nodes},
};
use crate::{
    core::{Interner, Span, Symbol},
    interface::BindingKind,
    ir::{AtomKind, Branch, CompositeKind, LibId, Node, NodeKind},
    libs::StackEffect,
    registry::InterfaceRegistry,
    symbolic::SymExpr,
    types::{CStack, CType},
};

/// Analyze a sequence of IR nodes.
///
/// This function walks the IR tree and produces an analysis result
/// containing the symbol table, scope tree, and any diagnostics.
pub fn analyze(nodes: &[Node], registry: &InterfaceRegistry, interner: &Interner) -> AnalysisResult {
    let mut analyzer = Analyzer::new(registry, interner);
    analyzer.analyze(nodes);
    analyzer.into_result()
}

/// The main analyzer.
struct Analyzer<'a> {
    registry: &'a InterfaceRegistry,
    interner: &'a Interner,
    symbols: SymbolTable,
    scopes: ScopeTree,
    diagnostics: Vec<Diagnostic>,
    /// Current scope during traversal.
    current_scope: ScopeId,
    /// Pending name value (for detecting STO/RCL patterns).
    /// Set by string literals ("x") or quoted symbols ('x').
    pending_name: Option<(String, Span)>,
    /// Map from name to definition for quick lookup during resolution.
    /// This is a stack of scopes, each containing a map of names to definitions.
    scope_definitions: Vec<HashMap<String, DefinitionId>>,
    /// Compile-time type stack for inferring value types.
    type_stack: CStack,
    /// Pending arity for functions being defined.
    /// Set when we see a program that starts with a local binding.
    pending_arity: Option<usize>,
    /// Stack of local index -> DefinitionId mappings for each scope.
    /// Used to mark local references (LocalRef) as referenced.
    local_index_to_def: Vec<HashMap<usize, DefinitionId>>,
    /// Saved type stacks for program scopes.
    /// When entering a program, we save the outer stack and start fresh.
    saved_type_stacks: Vec<CStack>,
    /// Set of spans that are binding metadata (index, name pairs).
    /// Nodes with these spans should not affect the type stack.
    binding_metadata_spans: std::collections::HashSet<Span>,
}

impl<'a> Analyzer<'a> {
    fn new(registry: &'a InterfaceRegistry, interner: &'a Interner) -> Self {
        Self {
            registry,
            interner,
            symbols: SymbolTable::new(),
            scopes: ScopeTree::new(),
            diagnostics: Vec::new(),
            current_scope: ScopeId::root(),
            pending_name: None,
            scope_definitions: vec![HashMap::new()], // Root scope
            type_stack: CStack::new(),
            pending_arity: None,
            local_index_to_def: vec![HashMap::new()], // Root scope
            saved_type_stacks: Vec::new(),
            binding_metadata_spans: std::collections::HashSet::new(),
        }
    }

    fn analyze(&mut self, nodes: &[Node]) {
        // First pass: collect definitions and references
        walk_nodes(self, nodes);

        // Second pass: resolve references and check for issues
        self.resolve_and_check();
    }

    fn into_result(self) -> AnalysisResult {
        AnalysisResult {
            symbols: self.symbols,
            scopes: self.scopes,
            diagnostics: self.diagnostics,
        }
    }

    /// Enter a new scope.
    fn enter_scope(&mut self, kind: ScopeKind, span: Span) {
        let scope = Scope::new(kind, span, Some(self.current_scope));
        let scope_id = self.scopes.add_scope(scope, self.current_scope);
        self.current_scope = scope_id;
        self.scope_definitions.push(HashMap::new());
        self.local_index_to_def.push(HashMap::new());
    }

    /// Exit the current scope.
    fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes.parent(self.current_scope) {
            self.current_scope = parent;
            self.scope_definitions.pop();
            self.local_index_to_def.pop();
        }
    }

    /// Add a definition with an optional inferred value type.
    fn add_definition_with_type(
        &mut self,
        name: String,
        span: Span,
        kind: DefinitionKind,
        value_type: Option<CType>,
    ) -> DefinitionId {
        let def = if let Some(vt) = value_type {
            Definition::with_type(name.clone(), span, kind, self.current_scope, vt)
        } else {
            Definition::new(name.clone(), span, kind, self.current_scope)
        };
        let def_id = self.symbols.add_definition(def);

        // Add to current scope's definition map
        if let Some(scope_map) = self.scope_definitions.last_mut() {
            // Check for duplicate in same scope
            if let Some(existing_id) = scope_map.get(&name)
                && let Some(existing) = self.symbols.get_definition(*existing_id)
            {
                self.diagnostics
                    .push(Diagnostic::duplicate_definition(&name, span, existing.span));
            }
            scope_map.insert(name, def_id);
        }

        def_id
    }

    /// Add a reference.
    fn add_reference(
        &mut self,
        name: String,
        span: Span,
        kind: ReferenceKind,
    ) -> super::symbols::ReferenceId {
        let reference = Reference::new(name, span, kind, self.current_scope);
        self.symbols.add_reference(reference)
    }

    /// Resolve all references and check for issues.
    fn resolve_and_check(&mut self) {
        // Resolve references
        let references: Vec<_> = self
            .symbols
            .references()
            .map(|r| (r.id, r.name.clone(), r.span))
            .collect();

        for (ref_id, name, span) in references {
            if let Some(def_id) = self.find_definition_for_reference(&name) {
                self.symbols.resolve_reference(ref_id, def_id);
            } else {
                // Only report undefined if it's a read reference
                // Write references (STO) create definitions
                if let Some(r) = self.symbols.get_reference(ref_id)
                    && matches!(r.kind, ReferenceKind::Read)
                {
                    self.diagnostics
                        .push(Diagnostic::undefined_variable(&name, span));
                }
            }
        }

        // Check for unused definitions (locals only - globals might be used elsewhere)
        let unused: Vec<_> = self
            .symbols
            .unreferenced_definitions()
            .filter(|d| matches!(d.kind, DefinitionKind::Local | DefinitionKind::LoopVar))
            .map(|d| (d.name.clone(), d.span))
            .collect();

        for (name, span) in unused {
            self.diagnostics
                .push(Diagnostic::unused_variable(&name, span));
        }
    }

    /// Find the definition for a reference, considering scope.
    fn find_definition_for_reference(&self, name: &str) -> Option<DefinitionId> {
        // For now, simple lookup - could be enhanced with scope-aware lookup
        self.symbols
            .find_definitions_by_name(name)
            .next()
            .map(|d| d.id)
    }

    /// Handle a string literal that might be part of a STO/RCL pattern.
    fn handle_string(&mut self, value: &str, span: Span) {
        self.pending_name = Some((value.to_string(), span));
        // Push string type - will be popped if used as a name
        self.type_stack.push(CType::string());
    }

    /// Handle an integer literal.
    fn handle_integer(&mut self, _value: i64) {
        self.type_stack.push(CType::integer());
    }

    /// Handle a real literal.
    fn handle_real(&mut self, _value: f64) {
        self.type_stack.push(CType::real());
    }

    /// Handle a symbolic expression that might be a quoted name for STO/RCL.
    fn handle_symbolic(&mut self, expr: &SymExpr, span: Span) {
        // If it's just a variable name (like 'x'), treat it as a potential name
        if let SymExpr::Var(name) = expr {
            self.pending_name = Some((name.to_string(), span));
        } else {
            // Complex symbolic expressions clear pending name
            self.pending_name = None;
        }
        // Push symbolic type
        self.type_stack
            .push(CType::Known(crate::core::TypeId::SYMBOLIC));
    }

    /// Handle a command, checking for binding effects.
    ///
    /// Uses the registry to determine if a command has a binding effect (Define, Read,
    /// Delete, Modify) and applies the stack effect from the registry.
    fn handle_command(&mut self, lib: LibId, cmd: u16, span: Span) {
        // Check if this command has a binding effect
        let binding_effect = self.registry.get_binding_effect(lib, cmd);

        // For Define, we need to get the value type before applying the effect
        let value_type = if matches!(binding_effect, Some(BindingKind::Define)) {
            // Stack is [..., value, name] - at(1) gets the value type (0=top=name, 1=value)
            Some(self.type_stack.at(1))
        } else {
            None
        };

        // Apply the stack effect from the registry (single source of truth)
        let effect = self.registry.get_command_effect(lib, cmd, &self.type_stack);

        // Get consume count for underflow reporting
        let consumes = match &effect {
            StackEffect::Fixed { consumes, .. } => *consumes as usize,
            StackEffect::Permutation { inputs, .. } => *inputs as usize,
            StackEffect::Dynamic => 0,
        };

        self.type_stack.apply_effect(&effect);

        // Check for stack underflow
        if let Some((needed, available)) = self.type_stack.take_underflow(consumes) {
            self.diagnostics
                .push(Diagnostic::stack_underflow(span, needed, available));
        }

        // Handle binding semantics (symbol tracking)
        if let Some(binding_kind) = binding_effect {
            let ref_kind = match binding_kind {
                BindingKind::Define => ReferenceKind::Write,
                BindingKind::Read => ReferenceKind::Read,
                BindingKind::Delete => ReferenceKind::Delete,
                BindingKind::Modify => ReferenceKind::Increment,
            };

            if let Some((name, name_span)) = self.pending_name.take() {
                if matches!(binding_kind, BindingKind::Define) {
                    // Create definition with inferred type and optional arity
                    let arity = self.pending_arity.take();
                    let inferred_type = value_type.filter(|ty| {
                        ty.is_known() || matches!(ty, CType::OneOf(_))
                    });

                    let def = match (inferred_type, arity) {
                        (Some(ty), Some(ar)) => Definition::with_type_and_arity(
                            name.clone(),
                            name_span,
                            DefinitionKind::Global,
                            self.current_scope,
                            ty,
                            ar,
                        ),
                        (Some(ty), None) => Definition::with_type(
                            name.clone(),
                            name_span,
                            DefinitionKind::Global,
                            self.current_scope,
                            ty,
                        ),
                        _ => Definition::new(
                            name.clone(),
                            name_span,
                            DefinitionKind::Global,
                            self.current_scope,
                        ),
                    };

                    self.symbols.add_definition(def);
                }

                self.add_reference(name, name_span, ref_kind);
            }
        } else {
            // No binding effect - clear pending name
            self.pending_name = None;
        }
    }

    /// Handle a symbol (unresolved identifier).
    ///
    /// This is typically a global word call. Since we don't know the stack
    /// effect of user-defined words, we mark the stack as having unknown depth.
    fn handle_symbol(&mut self, sym: Symbol, span: Span) {
        let name = self.interner.resolve(sym).to_string();
        self.add_reference(name, span, ReferenceKind::Read);

        // User-defined words have unknown stack effects, so mark stack as dynamic
        self.type_stack.apply_effect(&StackEffect::Dynamic);
    }

    /// Handle a local variable reference.
    fn handle_local_ref(&mut self, index: usize, span: Span) {
        // Look up the definition by local index in all enclosing scopes
        for scope_map in self.local_index_to_def.iter().rev() {
            if let Some(&def_id) = scope_map.get(&index) {
                // Get the name and type first to avoid borrow issues
                let (name, value_type) = self
                    .symbols
                    .get_definition(def_id)
                    .map(|d| (d.name.clone(), d.value_type.clone()))
                    .unwrap_or_default();

                if !name.is_empty() {
                    // Mark the definition as referenced
                    if let Some(def) = self.symbols.get_definition_mut(def_id) {
                        def.referenced = true;
                    }
                    // Add a reference for semantic token highlighting
                    let ref_id = self.add_reference(name, span, ReferenceKind::Read);
                    // Resolve the reference immediately
                    self.symbols.resolve_reference(ref_id, def_id);
                }

                // Push the variable's type onto the stack
                if let Some(ty) = value_type {
                    self.type_stack.push(ty);
                } else {
                    self.type_stack.push(CType::Unknown);
                }
                return;
            }
        }
        // If not found, push unknown type
        self.type_stack.push(CType::Unknown);
    }

    /// Handle bindings for any construct that creates local variables.
    ///
    /// This is a unified handler that works for any construct with bindings,
    /// determined dynamically via `Registry::binding_branches()`.
    ///
    /// The `is_loop` parameter determines whether bindings are loop variables
    /// (FOR loops) or local variables (-> syntax). Loop variables use a
    /// different definition kind to enable reassignment warnings.
    fn handle_bindings(
        &mut self,
        branches: &[Branch],
        binding_indices: &[usize],
        span: Span,
        is_loop: bool,
    ) {
        // Determine scope kind and definition kind based on construct type
        let (scope_kind, def_kind) = if is_loop {
            (ScopeKind::Loop, DefinitionKind::LoopVar)
        } else {
            (ScopeKind::LocalBinding, DefinitionKind::Local)
        };

        // Check if we're directly inside a Program scope (for arity tracking)
        // Only the outermost local binding in a program defines the function's arity
        let parent_is_program = self
            .scopes
            .get(self.current_scope)
            .map(|s| s.kind == ScopeKind::Program)
            .unwrap_or(false);

        self.enter_scope(scope_kind, span);

        // Track arity for function definitions - only for outermost local binding
        if parent_is_program && def_kind == DefinitionKind::Local {
            self.pending_arity = Some(binding_indices.len());
        }

        // Collect parameter names from the binding branches
        // Each binding branch has [index_node, name_node]
        let param_names: Vec<(String, Span, Option<usize>)> = binding_indices
            .iter()
            .filter_map(|&idx| branches.get(idx))
            .filter_map(|binding| {
                if binding.len() >= 2 {
                    let local_idx = if let NodeKind::Atom(AtomKind::Integer(n)) = &binding[0].kind {
                        Some(*n as usize)
                    } else {
                        None
                    };
                    if let NodeKind::Atom(AtomKind::String(name)) = &binding[1].kind {
                        Some((name.to_string(), binding[1].span, local_idx))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        // Infer types from stack for each parameter
        // Parameters bind in order: first param binds deepest, last param binds TOS
        let param_count = param_names.len();
        let mut param_types: Vec<CType> = Vec::with_capacity(param_count);
        for i in 0..param_count {
            let stack_pos = param_count - 1 - i;
            let ty = self.type_stack.at(stack_pos);
            param_types.push(ty);
        }

        // Pop the consumed types from the stack
        for _ in 0..param_count {
            self.type_stack.pop();
        }

        // Check for underflow from the pops above.
        // This handles the case where a local binding construct (like `-> n <<...>>`)
        // is used without enough values on the stack. The underflow should be reported
        // here, not leaked to the first command inside the body.
        //
        // EXCEPTION: When the local binding is directly inside a Program scope, the
        // pops represent function parameters, not stack underflow. The program expects
        // callers to provide these values, so we don't report underflow.
        if !parent_is_program {
            if let Some((needed, available)) = self.type_stack.take_underflow(param_count) {
                self.diagnostics
                    .push(Diagnostic::stack_underflow(span, needed, available));
            }
        } else {
            // Still clear the underflow counter to prevent it from leaking
            self.type_stack.take_underflow(param_count);
        }

        // Add definitions with inferred types
        for (i, (name, name_span, local_idx)) in param_names.into_iter().enumerate() {
            let ty = param_types.get(i).cloned();
            let def_id = self.add_definition_with_type(name, name_span, def_kind, ty);
            // Map local index to definition for LocalRef resolution
            if let Some(idx) = local_idx
                && let Some(map) = self.local_index_to_def.last_mut()
            {
                map.insert(idx, def_id);
            }
        }
    }
}

impl Visitor for Analyzer<'_> {
    fn visit_node(&mut self, _node: &Node) -> bool {
        true // Always continue
    }

    fn visit_integer(&mut self, value: i64, node: &Node) {
        // Skip type effects for binding metadata (e.g., local indices in -> n <<>>)
        if !self.binding_metadata_spans.contains(&node.span) {
            self.handle_integer(value);
        }
    }

    fn visit_real(&mut self, value: f64, _node: &Node) {
        self.handle_real(value);
    }

    fn visit_string(&mut self, value: &str, node: &Node) {
        // Skip type effects for binding metadata (e.g., local names in -> n <<>>)
        if !self.binding_metadata_spans.contains(&node.span) {
            self.handle_string(value, node.span);
        }
    }

    fn visit_symbol(&mut self, sym: Symbol, node: &Node) {
        self.handle_symbol(sym, node.span);
    }

    fn visit_local_ref(&mut self, index: usize, node: &Node) {
        self.handle_local_ref(index, node.span);
    }

    fn visit_symbolic(&mut self, expr: &SymExpr, node: &Node) {
        self.handle_symbolic(expr, node.span);
    }

    fn visit_command(&mut self, lib: LibId, cmd: u16, node: &Node) {
        self.handle_command(lib, cmd, node.span);
    }

    fn visit_program(&mut self, _body: &Branch, node: &Node) {
        self.enter_scope(ScopeKind::Program, node.span);
        // Save current type stack and start fresh for the program body.
        // This prevents the body's type changes from polluting the outer scope.
        let saved = std::mem::take(&mut self.type_stack);
        self.saved_type_stacks.push(saved);
    }

    fn visit_program_post(&mut self, _body: &Branch, _node: &Node) {
        self.exit_scope();
        // Restore the outer type stack and push Program type.
        // The body's internal type changes are discarded.
        if let Some(saved) = self.saved_type_stacks.pop() {
            self.type_stack = saved;
        }
        // A program literal pushes a program value onto the stack
        self.type_stack.push(CType::program());
    }

    fn visit_list(&mut self, _items: &Branch, _node: &Node) {
        // List will be visited, then we push list type
    }

    fn visit_extended(&mut self, lib: LibId, construct_id: u16, branches: &[Branch], node: &Node) {
        // Query the registry to determine if this construct has bindings
        let binding_indices = self.registry.binding_branches(lib, construct_id, branches.len());

        if !binding_indices.is_empty() {
            // Mark binding branch nodes as metadata (shouldn't affect type stack).
            // Binding branches contain [Integer(index), String(name)] which are parsed
            // metadata, not runtime values that should push onto the stack.
            for &idx in &binding_indices {
                if let Some(branch) = branches.get(idx) {
                    for binding_node in branch {
                        self.binding_metadata_spans.insert(binding_node.span);
                    }
                }
            }

            // Determine if this is a loop construct (FOR, FORUP, FORDN)
            // Loop constructs create LoopVar bindings, others create Local bindings
            // Use well-known construct IDs from the flow library
            let is_loop = matches!(construct_id, 20..=23); // FOR, FORUP, FORDN, START
            self.handle_bindings(branches, &binding_indices, node.span, is_loop);
        }
    }

    fn visit_node_post(&mut self, node: &Node) {
        // Exit scopes for extended constructs that have bindings
        if let NodeKind::Composite(CompositeKind::Extended(lib, construct_id), branches) = &node.kind {
            let binding_indices = self.registry.binding_branches(*lib, *construct_id, branches.len());
            if !binding_indices.is_empty() {
                self.exit_scope();
            }
        }

        // After visiting a list, push list type
        // (the items have been visited and their types pushed, but a list is a single value)
        if let NodeKind::Composite(CompositeKind::List, branches) = &node.kind {
            // Pop all item types (they're inside the list)
            if let Some(items) = branches.first() {
                for _ in 0..items.len() {
                    self.type_stack.pop();
                }
            }
            // Push the list type
            self.type_stack.push(CType::list());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::result::DiagnosticKind,
        core::Pos,
        interface::BindingKind,
        ir::{LibId, Node},
        libs::{CommandInfo, StackEffect},
        registry::InterfaceRegistry,
    };

    // Test constants matching directory lib
    const TEST_DIRECTORY_LIB: LibId = 28;
    mod test_dir_cmd {
        pub const STO: u16 = 0;
        pub const RCL: u16 = 1;
    }

    // Use these in tests
    use TEST_DIRECTORY_LIB as DIRECTORY_LIB;
    use test_dir_cmd as dir_cmd;

    /// Mock directory interface that implements binding_effect for tests.
    struct MockDirectoryInterface;

    impl crate::libs::LibraryInterface for MockDirectoryInterface {
        fn id(&self) -> LibId {
            DIRECTORY_LIB
        }
        fn name(&self) -> &'static str {
            "Directory"
        }
        fn commands(&self) -> Vec<CommandInfo> {
            vec![
                CommandInfo {
                    name: "STO",
                    lib_id: DIRECTORY_LIB,
                    cmd_id: dir_cmd::STO,
                    effect: StackEffect::fixed(2, &[]),
                },
                CommandInfo {
                    name: "RCL",
                    lib_id: DIRECTORY_LIB,
                    cmd_id: dir_cmd::RCL,
                    effect: StackEffect::fixed(1, &[None]),
                },
            ]
        }
        fn binding_effect(&self, cmd: u16) -> Option<BindingKind> {
            match cmd {
                0 => Some(BindingKind::Define), // STO
                1 => Some(BindingKind::Read),   // RCL
                _ => None,
            }
        }
    }

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    fn make_registry() -> InterfaceRegistry {
        let mut registry = InterfaceRegistry::new();
        registry.add(MockDirectoryInterface);
        registry
    }

    #[test]
    fn analyze_empty() {
        let registry = make_registry();
        let interner = Interner::new();
        let result = analyze(&[], &registry, &interner);

        assert_eq!(result.definition_count(), 0);
        assert_eq!(result.reference_count(), 0);
        assert!(!result.has_errors());
    }

    #[test]
    fn analyze_sto_pattern() {
        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 42 "x" STO
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // Should have one definition for "x"
        assert_eq!(result.definition_count(), 1);
        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "x");
        assert_eq!(def.kind, DefinitionKind::Global);
    }

    #[test]
    fn analyze_rcl_pattern() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let sym = interner.intern("x");

        // Simulate: 42 "x" STO x (using identifier, not RCL)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
            Node::symbol(sym, make_span(11, 12)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // Should have one definition and one reference
        assert_eq!(result.definition_count(), 1);
        assert_eq!(result.reference_count(), 2); // Write (STO) + Read (x)
    }

    #[test]
    fn analyze_undefined_variable() {
        let registry = make_registry();
        let mut interner = Interner::new();
        let sym = interner.intern("undefined");

        // Just reference an undefined variable
        let nodes = vec![Node::symbol(sym, make_span(0, 9))];

        let result = analyze(&nodes, &registry, &interner);

        assert!(result.has_errors());
        assert_eq!(result.errors().count(), 1);
        let error = result.errors().next().unwrap();
        assert!(error.message.contains("Undefined"));
    }

    #[test]
    fn analyze_program_scope() {
        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: « 42 "x" STO »
        let program = Node::program(
            vec![
                Node::integer(42, make_span(2, 4)),
                Node::string("x", make_span(5, 8)),
                Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(9, 12)),
            ],
            make_span(0, 14),
        );

        let result = analyze(&[program], &registry, &interner);

        // Should have 2 scopes (root + program)
        assert_eq!(result.scopes.len(), 2);
    }

    #[test]
    fn analyze_value_type_integer() {
        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 42 "x" STO
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "x");
        // Value type should be inferred as integer
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert!(
            value_type.is_integer(),
            "Expected integer type, got {:?}",
            value_type
        );
    }

    #[test]
    fn analyze_value_type_real() {
        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 3.14 "pi" STO
        let nodes = vec![
            Node::real(3.14, make_span(0, 4)),
            Node::string("pi", make_span(5, 9)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(10, 13)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "pi");
        // Value type should be inferred as real
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert!(
            value_type.is_real(),
            "Expected real type, got {:?}",
            value_type
        );
    }

    #[test]
    fn analyze_value_type_string() {
        use crate::core::TypeId;

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: "hello" "greeting" STO
        let nodes = vec![
            Node::string("hello", make_span(0, 7)),
            Node::string("greeting", make_span(8, 18)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(19, 22)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "greeting");
        // Value type should be inferred as string
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert_eq!(value_type.as_known(), Some(TypeId::STRING));
    }

    #[test]
    fn analyze_value_type_program() {
        use crate::core::TypeId;

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: « 1 + » "inc" STO
        let nodes = vec![
            Node::program(
                vec![
                    Node::integer(1, make_span(2, 3)),
                    // In real code there'd be a + command
                ],
                make_span(0, 6),
            ),
            Node::string("inc", make_span(7, 12)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(13, 16)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "inc");
        // Value type should be inferred as program
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert_eq!(value_type.as_known(), Some(TypeId::PROGRAM));
    }

    #[test]
    fn analyze_value_type_list() {
        use crate::core::TypeId;

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: { 1 2 3 } "nums" STO
        let nodes = vec![
            Node::list(
                vec![
                    Node::integer(1, make_span(2, 3)),
                    Node::integer(2, make_span(4, 5)),
                    Node::integer(3, make_span(6, 7)),
                ],
                make_span(0, 9),
            ),
            Node::string("nums", make_span(10, 16)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(17, 20)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "nums");
        // Value type should be inferred as list
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert_eq!(value_type.as_known(), Some(TypeId::LIST));
    }

    #[test]
    fn analyze_quoted_symbol_sto() {
        use crate::symbolic::SymExpr;

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 42 'x STO (using quoted symbol syntax)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::symbolic(SymExpr::var("x"), make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // Should have one definition for "x"
        assert_eq!(result.definition_count(), 1);
        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "x");
        assert_eq!(def.kind, DefinitionKind::Global);
        // Value type should be inferred as integer
        assert!(def.value_type.is_some());
        assert!(def.value_type.as_ref().unwrap().is_integer());
    }

    #[test]
    fn analyze_quoted_symbol_rcl() {
        use crate::symbolic::SymExpr;

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 42 'x STO 'x RCL (store then recall with quoted symbol)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::symbolic(SymExpr::var("x"), make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
            Node::symbolic(SymExpr::var("x"), make_span(11, 14)),
            Node::command(DIRECTORY_LIB, dir_cmd::RCL, make_span(15, 18)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // Should have one definition and two references (write + read)
        assert_eq!(result.definition_count(), 1);
        // Write reference from STO + Read reference from RCL
        assert_eq!(result.reference_count(), 2);
    }

    #[test]
    fn analyze_complex_symbolic_not_name() {
        use crate::symbolic::{BinOp, SymExpr};

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 42 'x+1 STO - complex symbolic should NOT be treated as name
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::symbolic(
                SymExpr::binary(BinOp::Add, SymExpr::var("x"), SymExpr::num(1.0)),
                make_span(3, 8),
            ),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(9, 12)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // Should have NO definitions - 'x+1' is not a valid name
        assert_eq!(result.definition_count(), 0);
    }

    #[test]
    fn analyze_value_type_symbolic() {
        use crate::{core::TypeId, symbolic::SymExpr};

        let registry = make_registry();
        let interner = Interner::new();

        // Simulate: 'X "expr" STO - storing a symbolic expression
        let nodes = vec![
            Node::symbolic(SymExpr::var("X"), make_span(0, 3)),
            Node::string("expr", make_span(4, 10)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(11, 14)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "expr");
        // Value type should be inferred as symbolic
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert_eq!(value_type.as_known(), Some(TypeId::SYMBOLIC));
    }

    // === Stack Underflow Detection Tests ===

    /// Mock stack interface with DROP (needs 1) and ROLL (dynamic).
    struct MockStackInterface;

    const MOCK_STACK_LIB: LibId = 72;
    mod mock_stack_cmd {
        pub const DROP: u16 = 1;
        pub const ROLL: u16 = 5;
    }

    impl crate::libs::LibraryInterface for MockStackInterface {
        fn id(&self) -> LibId {
            MOCK_STACK_LIB
        }
        fn name(&self) -> &'static str {
            "Stack"
        }
        fn commands(&self) -> Vec<CommandInfo> {
            vec![
                CommandInfo {
                    name: "DROP",
                    lib_id: MOCK_STACK_LIB,
                    cmd_id: mock_stack_cmd::DROP,
                    effect: StackEffect::fixed(1, &[]), // Consumes 1
                },
                CommandInfo {
                    name: "ROLL",
                    lib_id: MOCK_STACK_LIB,
                    cmd_id: mock_stack_cmd::ROLL,
                    effect: StackEffect::Dynamic, // Dynamic effect
                },
            ]
        }
        fn binding_effect(&self, _cmd: u16) -> Option<BindingKind> {
            None
        }
    }

    fn make_registry_with_stack() -> InterfaceRegistry {
        let mut registry = InterfaceRegistry::new();
        registry.add(MockDirectoryInterface);
        registry.add(MockStackInterface);
        registry
    }

    #[test]
    fn detects_stack_underflow() {
        let registry = make_registry_with_stack();
        let interner = Interner::new();

        // Just DROP on empty stack - needs 1, has 0
        let nodes = vec![Node::command(
            MOCK_STACK_LIB,
            mock_stack_cmd::DROP,
            make_span(0, 4),
        )];

        let result = analyze(&nodes, &registry, &interner);

        assert!(
            result.has_errors(),
            "Expected underflow error, got no errors"
        );
        assert!(result.diagnostics.iter().any(|d| matches!(
            d.kind,
            DiagnosticKind::StackUnderflow
        )));

        // Check the message
        let underflow_diag = result
            .diagnostics
            .iter()
            .find(|d| matches!(d.kind, DiagnosticKind::StackUnderflow))
            .unwrap();
        assert!(underflow_diag.message.contains("1"));
        assert!(underflow_diag.message.contains("0"));
    }

    #[test]
    fn no_underflow_with_enough_items() {
        let registry = make_registry_with_stack();
        let interner = Interner::new();

        // 42 DROP - has 1 item, DROP needs 1
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::command(MOCK_STACK_LIB, mock_stack_cmd::DROP, make_span(3, 7)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        assert!(
            !result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Expected no underflow, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn no_underflow_detection_after_dynamic() {
        let registry = make_registry_with_stack();
        let interner = Interner::new();

        // 1 ROLL DROP - ROLL makes depth unknown, so we can't detect underflow
        let nodes = vec![
            Node::integer(1, make_span(0, 1)),
            Node::command(MOCK_STACK_LIB, mock_stack_cmd::ROLL, make_span(2, 6)),
            Node::command(MOCK_STACK_LIB, mock_stack_cmd::DROP, make_span(7, 11)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // No underflow reported because ROLL set unknown_depth to true
        assert!(
            !result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Expected no underflow after dynamic op, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn partial_underflow_detected() {
        let registry = make_registry_with_stack();
        let interner = Interner::new();

        // 42 STO on empty stack - STO needs 2, we only have 1
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            // Note: this pushes the string, so now stack has 2 items
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // This should NOT have underflow - we have 2 items (42 and "x") and STO needs 2
        assert!(
            !result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Expected no underflow, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn underflow_with_one_item_missing() {
        let registry = make_registry_with_stack();
        let interner = Interner::new();

        // Just "x" STO - STO needs 2, we only have 1
        let nodes = vec![
            Node::string("x", make_span(0, 3)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(4, 7)),
        ];

        let result = analyze(&nodes, &registry, &interner);

        // This SHOULD have underflow - STO needs 2, only have 1
        assert!(
            result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Expected underflow, got: {:?}",
            result.diagnostics
        );

        let underflow_diag = result
            .diagnostics
            .iter()
            .find(|d| matches!(d.kind, DiagnosticKind::StackUnderflow))
            .unwrap();
        assert!(underflow_diag.message.contains("2")); // needs 2
        assert!(underflow_diag.message.contains("1")); // has 1
    }

    // === Local Binding Underflow Tests ===

    // Locals library: lib 32, construct 0
    // Branches: [binding_info...] + [body]
    // Each binding_info is [Integer(index), String(name)]
    const LOCALS_LIB: LibId = 32;
    const LOCALS_ARROW: u16 = 0;

    /// Mock Locals interface for binding tests.
    struct MockLocalsInterface;

    impl crate::libs::LibraryInterface for MockLocalsInterface {
        fn id(&self) -> LibId {
            LOCALS_LIB
        }
        fn name(&self) -> &'static str {
            "Locals"
        }
        fn commands(&self) -> Vec<CommandInfo> {
            vec![]
        }
        fn binding_branches(&self, _construct_id: u16, num_branches: usize) -> Vec<usize> {
            // All branches except the last are bindings
            if num_branches > 0 {
                (0..num_branches - 1).collect()
            } else {
                vec![]
            }
        }
    }

    fn make_registry_with_locals() -> InterfaceRegistry {
        let mut registry = InterfaceRegistry::new();
        registry.add(MockDirectoryInterface);
        registry.add(MockStackInterface);
        registry.add(MockLocalsInterface);
        registry
    }

    /// Helper to create a local binding Extended node: -> name << body >>
    fn make_local_binding(name: &str, index: usize, body: Vec<Node>, span: Span) -> Node {
        Node::extended(
            LOCALS_LIB,
            LOCALS_ARROW,
            vec![
                // Binding branch: [index, name]
                vec![
                    Node::integer(index as i64, span),
                    Node::string(name, span),
                ],
                // Body branch
                body,
            ],
            span,
        )
    }

    #[test]
    fn no_underflow_for_function_parameter() {
        // << -> n << n 1 + >> >>
        // The `-> n` is a function parameter, should NOT report underflow
        let registry = make_registry_with_locals();
        let interner = Interner::new();

        let inner_body = vec![
            Node::local_ref(0, make_span(10, 11)), // n
            Node::integer(1, make_span(12, 13)),
            // Would need + command but we don't have it, that's ok
        ];

        let locals_construct = make_local_binding("n", 0, inner_body, make_span(4, 20));

        let program = Node::program(vec![locals_construct], make_span(0, 25));

        let result = analyze(&[program], &registry, &interner);

        // Should NOT have underflow - n is a function parameter
        assert!(
            !result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Function parameter should not cause underflow, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn nested_binding_reports_underflow() {
        // << -> a << -> b << a b >> >> >>
        // The inner `-> b` is NOT a function parameter - it's nested inside `-> a`'s body.
        // Since the stack is empty after `-> a` consumes its argument, `-> b` should error.
        let registry = make_registry_with_locals();
        let interner = Interner::new();

        let innermost_body = vec![
            Node::local_ref(0, make_span(20, 21)), // a
            Node::local_ref(1, make_span(22, 23)), // b
        ];

        // -> b << a b >>
        let inner_binding = make_local_binding("b", 1, innermost_body, make_span(12, 28));

        // -> a << -> b << a b >> >>
        let outer_binding = make_local_binding("a", 0, vec![inner_binding], make_span(4, 32));

        let program = Node::program(vec![outer_binding], make_span(0, 36));

        let result = analyze(&[program], &registry, &interner);

        // SHOULD have underflow - the inner `-> b` needs a value but stack is empty
        assert!(
            result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Nested binding should report underflow when stack is empty, got: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn nested_binding_no_underflow_when_value_provided() {
        // << -> a << 42 -> b << a b >> >> >>
        // Here `42` provides a value for `-> b`, so no underflow.
        let registry = make_registry_with_locals();
        let interner = Interner::new();

        let innermost_body = vec![
            Node::local_ref(0, make_span(25, 26)), // a
            Node::local_ref(1, make_span(27, 28)), // b
        ];

        // -> b << a b >>
        let inner_binding = make_local_binding("b", 1, innermost_body, make_span(15, 32));

        // -> a << 42 -> b << a b >> >>
        let outer_body = vec![
            Node::integer(42, make_span(10, 12)), // This provides the value for -> b
            inner_binding,
        ];
        let outer_binding = make_local_binding("a", 0, outer_body, make_span(4, 36));

        let program = Node::program(vec![outer_binding], make_span(0, 40));

        let result = analyze(&[program], &registry, &interner);

        // Should NOT have underflow - 42 provides the value for -> b
        assert!(
            !result.diagnostics.iter().any(|d| matches!(
                d.kind,
                DiagnosticKind::StackUnderflow
            )),
            "Nested binding with value should not underflow, got: {:?}",
            result.diagnostics
        );
    }
}
