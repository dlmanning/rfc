//! Main analysis logic.
//!
//! This module implements the static analyzer that walks the IR tree
//! and builds symbol tables, scope trees, and diagnostics.

use std::collections::HashMap;

use crate::core::{Interner, Span, Symbol};

use super::{
    result::{AnalysisResult, Diagnostic},
    scopes::{Scope, ScopeId, ScopeKind, ScopeTree},
    symbols::{Definition, DefinitionId, DefinitionKind, Reference, ReferenceKind, SymbolTable},
    visitor::{Visitor, walk_nodes},
};
use crate::{
    ir::{AtomKind, Branch, CompositeKind, LibId, Node, NodeKind},
    libs::{
        directory::{DIRECTORY_LIB, cmd as dir_cmd},
        flow::{FLOW_LIB, constructs as flow_constructs},
        locals::{LOCALS_LIB, constructs as local_constructs},
    },
    symbolic::SymExpr,
    types::{CStack, CType},
};

/// Analyze a sequence of IR nodes.
///
/// This function walks the IR tree and produces an analysis result
/// containing the symbol table, scope tree, and any diagnostics.
pub fn analyze(nodes: &[Node], interner: &Interner) -> AnalysisResult {
    let mut analyzer = Analyzer::new(interner);
    analyzer.analyze(nodes);
    analyzer.into_result()
}

/// The main analyzer.
struct Analyzer<'a> {
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
}

impl<'a> Analyzer<'a> {
    fn new(interner: &'a Interner) -> Self {
        Self {
            interner,
            symbols: SymbolTable::new(),
            scopes: ScopeTree::new(),
            diagnostics: Vec::new(),
            current_scope: ScopeId::root(),
            pending_name: None,
            scope_definitions: vec![HashMap::new()], // Root scope
            type_stack: CStack::new(),
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
    }

    /// Exit the current scope.
    fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes.parent(self.current_scope) {
            self.current_scope = parent;
            self.scope_definitions.pop();
        }
    }

    /// Add a definition.
    fn add_definition(&mut self, name: String, span: Span, kind: DefinitionKind) -> DefinitionId {
        self.add_definition_with_type(name, span, kind, None)
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

    /// Handle a command, checking for STO/RCL patterns.
    fn handle_command(&mut self, lib: LibId, cmd: u16, span: Span) {
        if lib == DIRECTORY_LIB {
            match cmd {
                dir_cmd::STO => {
                    // STO creates a definition
                    // Stack: [..., value, "name"]
                    // Pop the string (name), then get the value's type
                    self.type_stack.pop(); // Pop string (consumed as name)
                    let value_type = self.type_stack.pop(); // Pop value being stored

                    if let Some((name, name_span)) = self.pending_name.take() {
                        // Create definition with inferred type
                        let inferred_type =
                            if value_type.is_known() || matches!(value_type, CType::OneOf(_)) {
                                Some(value_type)
                            } else {
                                None
                            };
                        self.add_definition_with_type(
                            name.clone(),
                            name_span,
                            DefinitionKind::Global,
                            inferred_type,
                        );
                        // Also add a write reference
                        self.add_reference(name, span, ReferenceKind::Write);
                    }
                }
                dir_cmd::RCL => {
                    // RCL is a read reference
                    // Pop the string (name), push unknown (value type depends on what's stored)
                    self.type_stack.pop();
                    self.type_stack.push(CType::Unknown);

                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.add_reference(name, name_span, ReferenceKind::Read);
                    }
                }
                dir_cmd::PURGE => {
                    // PURGE is a delete reference
                    self.type_stack.pop(); // Pop name

                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.add_reference(name, name_span, ReferenceKind::Delete);
                    }
                }
                dir_cmd::INCR => {
                    // INCR is a modify reference
                    self.type_stack.pop(); // Pop name

                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.add_reference(name, name_span, ReferenceKind::Increment);
                    }
                }
                dir_cmd::DECR => {
                    // DECR is a modify reference
                    self.type_stack.pop(); // Pop name

                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.add_reference(name, name_span, ReferenceKind::Decrement);
                    }
                }
                _ => {
                    self.pending_name = None;
                    // Other directory commands have various effects - mark unknown for now
                    self.type_stack.mark_unknown_depth();
                }
            }
        } else {
            // Other commands clear the pending string
            self.pending_name = None;
            // Mark stack as unknown since we don't have command signature info here.
            // The lowerer has full registry access for proper stack effects.
            self.type_stack.mark_unknown_depth();
        }
    }

    /// Handle a symbol (unresolved identifier).
    fn handle_symbol(&mut self, sym: Symbol, span: Span) {
        let name = self.interner.resolve(sym).to_string();
        self.add_reference(name, span, ReferenceKind::Read);
    }

    /// Handle a local binding.
    fn handle_local_binding(&mut self, branches: &[Branch], span: Span) {
        // branches[0] contains the local variable indices (as Integer nodes with symbols stored)
        // branches[1] contains the body
        // Actually, looking at the locals implementation more carefully...
        // The first branch contains Symbol nodes for the variable names

        self.enter_scope(ScopeKind::LocalBinding, span);

        if branches.len() >= 2 {
            // First branch: variable declarations
            for node in &branches[0] {
                if let NodeKind::Atom(AtomKind::Symbol(sym)) = &node.kind {
                    let name = self.interner.resolve(*sym).to_string();
                    self.add_definition(name, node.span, DefinitionKind::Local);
                }
                // Also handle Integer nodes which might store the local index
                // but we extract the name from the Symbol
            }

            // Second branch: body - will be visited normally
        }
    }

    /// Handle a FOR loop with variable.
    fn handle_for_loop(&mut self, branches: &[Branch], span: Span) {
        // FOR i has the loop variable in the first branch
        self.enter_scope(ScopeKind::Loop, span);

        if !branches.is_empty() {
            // Look for the loop variable (should be a symbol in the first branch)
            for node in &branches[0] {
                if let NodeKind::Atom(AtomKind::Symbol(sym)) = &node.kind {
                    let name = self.interner.resolve(*sym).to_string();
                    self.add_definition(name, node.span, DefinitionKind::LoopVar);
                    break; // Only the first one is the loop variable
                }
            }
        }
    }
}

impl Visitor for Analyzer<'_> {
    fn visit_node(&mut self, _node: &Node) -> bool {
        true // Always continue
    }

    fn visit_integer(&mut self, value: i64, _node: &Node) {
        self.handle_integer(value);
    }

    fn visit_real(&mut self, value: f64, _node: &Node) {
        self.handle_real(value);
    }

    fn visit_string(&mut self, value: &str, node: &Node) {
        self.handle_string(value, node.span);
    }

    fn visit_symbol(&mut self, sym: Symbol, node: &Node) {
        self.handle_symbol(sym, node.span);
    }

    fn visit_symbolic(&mut self, expr: &SymExpr, node: &Node) {
        self.handle_symbolic(expr, node.span);
    }

    fn visit_command(&mut self, lib: LibId, cmd: u16, node: &Node) {
        self.handle_command(lib, cmd, node.span);
    }

    fn visit_program(&mut self, _body: &Branch, node: &Node) {
        self.enter_scope(ScopeKind::Program, node.span);
    }

    fn visit_program_post(&mut self, _body: &Branch, _node: &Node) {
        self.exit_scope();
        // A program literal pushes a program value onto the stack
        self.type_stack.push(CType::program());
    }

    fn visit_list(&mut self, _items: &Branch, _node: &Node) {
        // List will be visited, then we push list type
    }

    fn visit_extended(&mut self, lib: LibId, id: u16, branches: &[Branch], node: &Node) {
        if lib == LOCALS_LIB && id == local_constructs::LOCAL_BINDING {
            self.handle_local_binding(branches, node.span);
        } else if lib == FLOW_LIB && id == flow_constructs::FOR_NEXT {
            self.handle_for_loop(branches, node.span);
        }
    }

    fn visit_node_post(&mut self, node: &Node) {
        // Exit scopes for extended constructs
        if let NodeKind::Composite(CompositeKind::Extended(lib, id), _) = &node.kind
            && ((*lib == LOCALS_LIB && *id == local_constructs::LOCAL_BINDING)
                || (*lib == FLOW_LIB && *id == flow_constructs::FOR_NEXT))
        {
            self.exit_scope();
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
    use crate::core::Pos;

    use super::*;
    use crate::ir::Node;

    fn make_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn analyze_empty() {
        let interner = Interner::new();
        let result = analyze(&[], &interner);

        assert_eq!(result.definition_count(), 0);
        assert_eq!(result.reference_count(), 0);
        assert!(!result.has_errors());
    }

    #[test]
    fn analyze_sto_pattern() {
        let interner = Interner::new();

        // Simulate: 42 "x" STO
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &interner);

        // Should have one definition for "x"
        assert_eq!(result.definition_count(), 1);
        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "x");
        assert_eq!(def.kind, DefinitionKind::Global);
    }

    #[test]
    fn analyze_rcl_pattern() {
        let mut interner = Interner::new();
        let sym = interner.intern("x");

        // Simulate: 42 "x" STO x (using identifier, not RCL)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
            Node::symbol(sym, make_span(11, 12)),
        ];

        let result = analyze(&nodes, &interner);

        // Should have one definition and one reference
        assert_eq!(result.definition_count(), 1);
        assert_eq!(result.reference_count(), 2); // Write (STO) + Read (x)
    }

    #[test]
    fn analyze_undefined_variable() {
        let mut interner = Interner::new();
        let sym = interner.intern("undefined");

        // Just reference an undefined variable
        let nodes = vec![Node::symbol(sym, make_span(0, 9))];

        let result = analyze(&nodes, &interner);

        assert!(result.has_errors());
        assert_eq!(result.errors().count(), 1);
        let error = result.errors().next().unwrap();
        assert!(error.message.contains("Undefined"));
    }

    #[test]
    fn analyze_program_scope() {
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

        let result = analyze(&[program], &interner);

        // Should have 2 scopes (root + program)
        assert_eq!(result.scopes.len(), 2);
    }

    #[test]
    fn analyze_value_type_integer() {
        let interner = Interner::new();

        // Simulate: 42 "x" STO
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::string("x", make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &interner);

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
        let interner = Interner::new();

        // Simulate: 3.14 "pi" STO
        let nodes = vec![
            Node::real(3.14, make_span(0, 4)),
            Node::string("pi", make_span(5, 9)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(10, 13)),
        ];

        let result = analyze(&nodes, &interner);

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

        let interner = Interner::new();

        // Simulate: "hello" "greeting" STO
        let nodes = vec![
            Node::string("hello", make_span(0, 7)),
            Node::string("greeting", make_span(8, 18)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(19, 22)),
        ];

        let result = analyze(&nodes, &interner);

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

        let result = analyze(&nodes, &interner);

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

        let result = analyze(&nodes, &interner);

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

        let interner = Interner::new();

        // Simulate: 42 'x STO (using quoted symbol syntax)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::symbolic(SymExpr::var("x"), make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
        ];

        let result = analyze(&nodes, &interner);

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

        let interner = Interner::new();

        // Simulate: 42 'x STO 'x RCL (store then recall with quoted symbol)
        let nodes = vec![
            Node::integer(42, make_span(0, 2)),
            Node::symbolic(SymExpr::var("x"), make_span(3, 6)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(7, 10)),
            Node::symbolic(SymExpr::var("x"), make_span(11, 14)),
            Node::command(DIRECTORY_LIB, dir_cmd::RCL, make_span(15, 18)),
        ];

        let result = analyze(&nodes, &interner);

        // Should have one definition and two references (write + read)
        assert_eq!(result.definition_count(), 1);
        // Write reference from STO + Read reference from RCL
        assert_eq!(result.reference_count(), 2);
    }

    #[test]
    fn analyze_complex_symbolic_not_name() {
        use crate::symbolic::{BinOp, SymExpr};

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

        let result = analyze(&nodes, &interner);

        // Should have NO definitions - 'x+1' is not a valid name
        assert_eq!(result.definition_count(), 0);
    }

    #[test]
    fn analyze_value_type_symbolic() {
        use crate::core::TypeId;

        use crate::symbolic::SymExpr;

        let interner = Interner::new();

        // Simulate: 'X "expr" STO - storing a symbolic expression
        let nodes = vec![
            Node::symbolic(SymExpr::var("X"), make_span(0, 3)),
            Node::string("expr", make_span(4, 10)),
            Node::command(DIRECTORY_LIB, dir_cmd::STO, make_span(11, 14)),
        ];

        let result = analyze(&nodes, &interner);

        let def = result.symbols.definitions().next().unwrap();
        assert_eq!(def.name, "expr");
        // Value type should be inferred as symbolic
        assert!(def.value_type.is_some());
        let value_type = def.value_type.as_ref().unwrap();
        assert_eq!(value_type.as_known(), Some(TypeId::SYMBOLIC));
    }
}
