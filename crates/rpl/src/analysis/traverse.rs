//! Phase 3: Main IR traversal with type and origin tracking.
//!
//! This module implements the Visitor trait to walk the IR tree,
//! tracking types and origins in parallel, and generating constraints
//! for Phase 4 resolution.

use std::collections::HashMap;

use super::{
    walk_nodes, Definition, DefinitionId, DefinitionKind, Diagnostic, Reference,
    ReferenceKind, Scope, ScopeId, ScopeKind, ScopeTree, SymbolTable, Visitor,
};
use crate::core::{Interner, Span, Symbol, TypeId};
use crate::interface::BindingKind;
use crate::ir::{AtomKind, Branch, CompositeKind, LibId, Node, NodeKind};
use crate::libs::StackEffect;
use crate::registry::InterfaceRegistry;
use crate::symbolic::SymExpr;
use crate::types::TypeConstraint;

use super::globals::GlobalMap;
use super::patterns::{Pattern, PatternMap};
use super::state::{StackState, Substitution};
use super::types::{Constraint, Origin, Requirement, StackSnapshot, Type, TypeVar};

/// The main traversal analyzer (Phase 3).
pub struct Traverser<'a> {
    // === Output being built ===
    /// Symbol table with definitions and references.
    pub symbols: SymbolTable,
    /// Scope tree.
    pub scopes: ScopeTree,
    /// Diagnostics collected during traversal.
    pub diagnostics: Vec<Diagnostic>,

    // === Traversal state ===
    /// Abstract stack with types and origins.
    stack: StackState,
    /// Saved stacks for nested programs.
    saved_stacks: Vec<StackState>,
    /// Current scope ID.
    current_scope: ScopeId,
    /// Scope definitions for name lookup (stack of name -> def_id maps).
    scope_definitions: Vec<HashMap<String, DefinitionId>>,
    /// Local index to definition mapping per scope.
    local_index_to_def: Vec<HashMap<usize, DefinitionId>>,
    /// Collected constraints for Phase 4.
    pub constraints: Vec<Constraint>,
    /// Type variable substitution.
    pub substitution: Substitution,
    /// Next type variable ID.
    next_type_var: u32,

    // === External references ===
    /// Registry for querying effects.
    registry: &'a InterfaceRegistry,
    /// Interner for symbol resolution.
    interner: &'a Interner,
    /// Patterns from Phase 1.
    patterns: &'a PatternMap,
    /// Globals from Phase 2.
    globals: &'a GlobalMap,

    // === Pattern state ===
    /// Pending name for STO/RCL patterns.
    pending_name: Option<(String, Span)>,
    /// Current function being analyzed (for return type constraints).
    current_function: Option<String>,
    /// Return origins for functions (captured during traversal for resolving Unknown types).
    return_origins: HashMap<String, Origin>,
    /// Stack snapshots at each command node for lowering.
    node_stacks: HashMap<Span, StackSnapshot>,
}

impl<'a> Traverser<'a> {
    /// Create a new traverser.
    pub fn new(
        registry: &'a InterfaceRegistry,
        interner: &'a Interner,
        patterns: &'a PatternMap,
        globals: &'a GlobalMap,
        initial_type_var: u32,
        symbols: SymbolTable,
    ) -> Self {
        // Pre-populate root scope with global definitions
        let mut root_scope_defs = HashMap::new();
        for (name, info) in globals {
            root_scope_defs.insert(name.clone(), info.def_id);
        }

        Self {
            symbols,
            scopes: ScopeTree::new(),
            diagnostics: Vec::new(),
            stack: StackState::new(),
            saved_stacks: Vec::new(),
            current_scope: ScopeId::root(),
            scope_definitions: vec![root_scope_defs],
            local_index_to_def: vec![HashMap::new()],
            constraints: Vec::new(),
            substitution: Substitution::new(),
            next_type_var: initial_type_var,
            registry,
            interner,
            patterns,
            globals,
            pending_name: None,
            current_function: None,
            return_origins: HashMap::new(),
            node_stacks: HashMap::new(),
        }
    }

    /// Run traversal on the IR nodes.
    pub fn traverse(mut self, nodes: &[Node]) -> TraversalResult {
        walk_nodes(&mut self, nodes);

        TraversalResult {
            symbols: self.symbols,
            scopes: self.scopes,
            diagnostics: self.diagnostics,
            constraints: self.constraints,
            substitution: self.substitution,
            next_type_var: self.next_type_var,
            return_origins: self.return_origins,
            node_stacks: self.node_stacks,
        }
    }

    /// Generate a fresh type variable.
    #[allow(dead_code)]
    fn fresh_type_var(&mut self) -> TypeVar {
        let var = TypeVar(self.next_type_var);
        self.next_type_var += 1;
        var
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

    /// Look up a definition by name in current and ancestor scopes.
    fn lookup_definition(&self, name: &str) -> Option<DefinitionId> {
        for scope_map in self.scope_definitions.iter().rev() {
            if let Some(&def_id) = scope_map.get(name) {
                return Some(def_id);
            }
        }
        None
    }

    /// Look up a definition by local index in current and ancestor scopes.
    fn lookup_local(&self, index: usize) -> Option<DefinitionId> {
        for scope_map in self.local_index_to_def.iter().rev() {
            if let Some(&def_id) = scope_map.get(&index) {
                return Some(def_id);
            }
        }
        None
    }

    /// Add a definition to the current scope.
    fn add_definition(&mut self, def: Definition) -> DefinitionId {
        let name = def.name.clone();
        let local_index = def.local_index;
        let def_id = self.symbols.add_definition(def);

        if let Some(scope_map) = self.scope_definitions.last_mut() {
            scope_map.insert(name, def_id);
        }

        if let (Some(index), Some(scope_map)) = (local_index, self.local_index_to_def.last_mut()) {
            scope_map.insert(index, def_id);
        }

        def_id
    }

    /// Add a reference.
    fn add_reference(&mut self, name: String, span: Span, kind: ReferenceKind) {
        let reference = Reference::new(name.clone(), span, kind, self.current_scope);
        let ref_id = self.symbols.add_reference(reference);

        // Try to resolve to a definition
        if let Some(def_id) = self.lookup_definition(&name) {
            self.symbols.resolve_reference(ref_id, def_id);
            if let Some(def) = self.symbols.get_definition_mut(def_id) {
                def.referenced = true;
            }
        }
    }

    /// Collect a constraint from an origin.
    fn collect_constraint(&mut self, origin: &Origin, requirement: Requirement, span: Span, operation: &str) {
        if matches!(requirement, Requirement::Any) {
            return;
        }

        match origin {
            Origin::Binding(def_id) | Origin::LoopVar(def_id) => {
                self.constraints.push(Constraint::must_be(
                    *def_id,
                    requirement,
                    span,
                    operation,
                ));
            }
            Origin::Phi(origins) => {
                // Propagate to all merged origins
                for o in origins {
                    self.collect_constraint(o, requirement.clone(), span, operation);
                }
            }
            _ => {
                // Literals and results don't need constraints
            }
        }
    }

    /// Convert a TypeConstraint to a Requirement.
    fn type_constraint_to_requirement(&self, tc: &TypeConstraint) -> Requirement {
        match tc {
            TypeConstraint::Exact(t) => Requirement::Exact(*t),
            TypeConstraint::OneOf(ts) => Requirement::OneOf(ts.clone()),
            TypeConstraint::Any => Requirement::Any,
            TypeConstraint::SameAs(_) => Requirement::Any, // Handled separately
        }
    }

    /// Handle a function call to a known global function.
    fn handle_function_call(&mut self, name: &str, span: Span) {
        if let Some(info) = self.globals.get(name) {
            let param_count = info.signature.inputs.len();

            // Create Equal constraints linking caller args to callee params
            for i in 0..param_count {
                let stack_pos = param_count - 1 - i;
                let origin = self.stack.origin_at(stack_pos);

                // If argument has known origin, link it to parameter
                if let Some(arg_def_id) = origin.def_id()
                    && let Some(&param_def_id) = info.param_def_ids.get(i) {
                        self.constraints.push(Constraint::equal(arg_def_id, param_def_id, span));
                    }

                // If argument has known type, record call site type (unioned, not intersected)
                let arg_ty = self.stack.type_at(stack_pos);
                if let Type::Known(t) = arg_ty
                    && let Some(&param_def_id) = info.param_def_ids.get(i) {
                        self.constraints.push(Constraint::called_with(
                            param_def_id,
                            t,
                            span,
                        ));
                    }
            }

            // Pop arguments
            for _ in 0..param_count {
                self.stack.pop();
            }

            // Push return type
            let return_ty = Type::TypeVar(info.return_type_var);
            self.stack.push(return_ty, Origin::Result(span));
        } else {
            // Unknown function - dynamic effect
            self.stack.clear();
            self.stack.depth_known = false;
        }
    }

    /// Handle an Extended construct with bindings but no alternatives (FOR, -> locals).
    ///
    /// This manually walks branches, skipping binding branches to avoid
    /// polluting the stack with their metadata nodes.
    fn handle_extended_with_bindings(
        &mut self,
        lib: LibId,
        construct_id: u16,
        branches: &[Branch],
        node: &Node,
    ) {
        let binding_indices = self.registry.binding_branches(lib, construct_id, branches.len());
        let is_loop = self.registry.is_loop_construct(lib, construct_id);

        // Collect binding info from branches BEFORE modifying stack
        let bindings: Vec<(usize, String, Span)> = binding_indices
            .iter()
            .filter_map(|&idx| branches.get(idx))
            .filter_map(|binding| {
                if binding.len() >= 2 {
                    let local_idx = if let NodeKind::Atom(AtomKind::Integer(n)) = &binding[0].kind {
                        Some(*n as usize)
                    } else {
                        None
                    };
                    let name = if let NodeKind::Atom(AtomKind::String(s)) = &binding[1].kind {
                        Some((s.to_string(), binding[1].span))
                    } else {
                        None
                    };
                    local_idx.zip(name).map(|(idx, (name, span))| (idx, name, span))
                } else {
                    None
                }
            })
            .collect();

        // Infer types from stack BEFORE consuming values
        // If stack type is Unknown (e.g., program parameters), use a TypeVar
        // so constraint resolution can narrow it based on usage.
        let param_count = bindings.len();
        let param_types: Vec<Type> = if is_loop {
            vec![Type::Known(TypeId::BINT); param_count]
        } else {
            (0..param_count)
                .map(|i| {
                    let ty = self.stack.type_at(param_count - 1 - i);
                    if ty.is_unknown() {
                        Type::TypeVar(self.fresh_type_var())
                    } else {
                        ty
                    }
                })
                .collect()
        };

        // Apply construct's stack effect (e.g., FOR consumes start/end ints)
        let effect = self.registry.get_construct_effect(lib, construct_id);
        if !matches!(effect, StackEffect::Dynamic) {
            self.stack.apply_effect(&effect, node.span);
        }

        // Pop bound values (locals binding always consumes from stack)
        // Note: The construct effect may consume additional values (like FOR's start/end),
        // but the binding itself consumes one value per bound name.
        if !is_loop {
            for _ in 0..param_count {
                self.stack.pop();
            }
        }

        // Enter scope for constructs with bindings
        let scope_kind = if is_loop { ScopeKind::Loop } else { ScopeKind::LocalBinding };
        self.enter_scope(scope_kind, node.span);

        // Create definitions
        let kind = if is_loop { DefinitionKind::LoopVar } else { DefinitionKind::Local };
        for (i, (local_idx, name, span)) in bindings.into_iter().enumerate() {
            let existing_def = if !is_loop {
                self.lookup_local(local_idx)
            } else {
                None
            };

            if let Some(existing_def_id) = existing_def {
                if let Some(scope_map) = self.scope_definitions.last_mut() {
                    scope_map.insert(name, existing_def_id);
                }
                if let Some(local_map) = self.local_index_to_def.last_mut() {
                    local_map.insert(local_idx, existing_def_id);
                }
            } else {
                let ty = param_types.get(i).cloned().unwrap_or(Type::Unknown);
                let mut def = Definition::with_type(
                    name,
                    span,
                    kind,
                    self.current_scope,
                    ty,
                );
                def.local_index = Some(local_idx);
                self.add_definition(def);
            }
        }

        // Walk non-binding branches (body branches only)
        let binding_set: std::collections::HashSet<_> = binding_indices.iter().copied().collect();
        for (idx, branch) in branches.iter().enumerate() {
            if !binding_set.contains(&idx) {
                walk_nodes(self, branch);
            }
        }

        // Exit scope
        self.exit_scope();
    }

    /// Handle an Extended construct with alternative branches (IF/THEN/ELSE, etc.).
    ///
    /// This manually walks the branches with proper stack save/restore for alternatives.
    fn handle_extended_with_alternatives(
        &mut self,
        lib: LibId,
        construct_id: u16,
        branches: &[Branch],
        node: &Node,
    ) {
        let binding_indices = self.registry.binding_branches(lib, construct_id, branches.len());
        let alternative_indices = self.registry.alternative_branches(lib, construct_id, branches.len());
        let is_loop = self.registry.is_loop_construct(lib, construct_id);

        // Apply construct's stack effect (e.g., IF consumes a boolean condition)
        let effect = self.registry.get_construct_effect(lib, construct_id);
        if !matches!(effect, StackEffect::Dynamic) {
            self.stack.apply_effect(&effect, node.span);
        }

        // Handle bindings (enter scope if needed)
        if !binding_indices.is_empty() {
            let scope_kind = if is_loop { ScopeKind::Loop } else { ScopeKind::LocalBinding };
            self.enter_scope(scope_kind, node.span);

            // Collect binding info from branches
            let bindings: Vec<(usize, String, Span)> = binding_indices
                .iter()
                .filter_map(|&idx| branches.get(idx))
                .filter_map(|binding| {
                    if binding.len() >= 2 {
                        let local_idx = if let NodeKind::Atom(AtomKind::Integer(n)) = &binding[0].kind {
                            Some(*n as usize)
                        } else {
                            None
                        };
                        let name = if let NodeKind::Atom(AtomKind::String(s)) = &binding[1].kind {
                            Some((s.to_string(), binding[1].span))
                        } else {
                            None
                        };
                        local_idx.zip(name).map(|(idx, (name, span))| (idx, name, span))
                    } else {
                        None
                    }
                })
                .collect();

            // Create definitions for bindings
            // Note: Loop variables always get new definitions - they never reuse parent scope defs
            let kind = if is_loop { DefinitionKind::LoopVar } else { DefinitionKind::Local };
            for (local_idx, name, span) in bindings {
                // Only check for existing definitions if this is NOT a loop construct
                let existing_def = if !is_loop {
                    self.lookup_local(local_idx)
                } else {
                    None
                };

                if let Some(existing_def_id) = existing_def {
                    if let Some(scope_map) = self.scope_definitions.last_mut() {
                        scope_map.insert(name, existing_def_id);
                    }
                    if let Some(local_map) = self.local_index_to_def.last_mut() {
                        local_map.insert(local_idx, existing_def_id);
                    }
                } else {
                    // Note: Unlike handle_extended_with_bindings, we don't infer types from
                    // the stack here. With alternatives, bindings occur inside branches where
                    // stack state is saved/restored for merging. Type inference for these
                    // bindings is handled later via constraint resolution.
                    let ty = Type::Unknown;
                    let mut def = Definition::with_type(
                        name,
                        span,
                        kind,
                        self.current_scope,
                        ty,
                    );
                    def.local_index = Some(local_idx);
                    self.add_definition(def);
                }
            }
        }

        // Collect all alternative branch indices into a set
        let alternative_set: std::collections::HashSet<usize> = alternative_indices
            .iter()
            .flat_map(|group| group.iter().copied())
            .collect();
        let binding_set: std::collections::HashSet<_> = binding_indices.iter().copied().collect();

        // Walk non-alternative branches first (body branches)
        for (idx, branch) in branches.iter().enumerate() {
            if !alternative_set.contains(&idx) && !binding_set.contains(&idx) {
                walk_nodes(self, branch);
            }
        }

        // Process each group of alternatives
        for alt_group in &alternative_indices {
            if alt_group.is_empty() {
                continue;
            }

            // Save stack state before processing this group
            let base_stack = self.stack.clone();
            let mut group_stacks: Vec<StackState> = Vec::new();

            for &alt_idx in alt_group {
                if let Some(branch) = branches.get(alt_idx) {
                    // Restore to base stack before this alternative
                    self.stack = base_stack.clone();

                    // Walk this alternative
                    walk_nodes(self, branch);

                    // Collect the resulting stack
                    group_stacks.push(self.stack.clone());
                }
            }

            // Merge all alternative stacks from this group
            if !group_stacks.is_empty() {
                self.stack = StackState::merge_all(&group_stacks);
            }
        }

        // Exit scope if we entered one
        if !binding_indices.is_empty() {
            self.exit_scope();
        }
    }

    /// Apply a stack effect, collecting constraints from consumed values.
    fn apply_command_effect(
        &mut self,
        lib: LibId,
        cmd: u16,
        effect: &StackEffect,
        span: Span,
    ) {
        let cmd_name = self.registry.get_command_name(lib, cmd);

        // Get input constraints and collect them
        let input_constraints = self.registry.get_input_constraints(lib, cmd);
        let consumes = effect.consumes().unwrap_or(0) as usize;

        for (i, constraint) in input_constraints.iter().enumerate().take(consumes) {
            let stack_pos = consumes - 1 - i;
            let origin = self.stack.origin_at(stack_pos);
            let requirement = self.type_constraint_to_requirement(constraint);
            self.collect_constraint(&origin, requirement, span, cmd_name);
        }

        // Apply the effect
        self.stack.apply_effect(effect, span);
    }
}

/// Result of traversal.
pub struct TraversalResult {
    pub symbols: SymbolTable,
    pub scopes: ScopeTree,
    pub diagnostics: Vec<Diagnostic>,
    pub constraints: Vec<Constraint>,
    pub substitution: Substitution,
    pub next_type_var: u32,
    /// Return origins for functions (captured during traversal for resolving Unknown types).
    pub return_origins: HashMap<String, Origin>,
    /// Stack snapshots at each command node for lowering.
    pub node_stacks: HashMap<Span, StackSnapshot>,
}

impl Visitor for Traverser<'_> {
    fn visit_node(&mut self, node: &Node) -> bool {
        // Handle List nodes specially - walk items for error detection but discard stack effects
        if let NodeKind::Composite(CompositeKind::List, branches) = &node.kind {
            // Walk items for error detection and variable references
            if let Some(items) = branches.first() {
                let saved = std::mem::take(&mut self.stack);
                walk_nodes(self, items);
                // Discard item stack effects - list captures them
                self.stack = saved;
            }
            self.stack.push(Type::Known(TypeId::LIST), Origin::Literal(node.span));
            return false;
        }

        // Handle Extended nodes specially to avoid walking binding branches
        // (binding branches contain [Integer, String] metadata that would pollute the stack)
        if let NodeKind::Composite(CompositeKind::Extended(lib, construct_id), branches) = &node.kind {
            let alternatives = self.registry.alternative_branches(*lib, *construct_id, branches.len());
            let bindings = self.registry.binding_branches(*lib, *construct_id, branches.len());

            if !alternatives.is_empty() {
                // Handle constructs with alternatives (IF/THEN/ELSE) with stack save/restore
                self.handle_extended_with_alternatives(*lib, *construct_id, branches, node);
                return false;
            } else if !bindings.is_empty() {
                // Handle constructs with bindings (FOR, -> locals) but no alternatives
                self.handle_extended_with_bindings(*lib, *construct_id, branches, node);
                return false;
            } else {
                // Simple Extended (no bindings, no alternatives) - just apply effect and walk
                let effect = self.registry.get_construct_effect(*lib, *construct_id);
                if !matches!(effect, StackEffect::Dynamic) {
                    self.stack.apply_effect(&effect, node.span);
                }
                // Walk all branches
                for branch in branches {
                    walk_nodes(self, branch);
                }
                return false;
            }
        }
        true // Continue with default walking
    }

    fn visit_integer(&mut self, _value: i64, node: &Node) {
        self.stack.push(Type::Known(TypeId::BINT), Origin::Literal(node.span));
    }

    fn visit_real(&mut self, _value: f64, node: &Node) {
        self.stack.push(Type::Known(TypeId::REAL), Origin::Literal(node.span));
    }

    fn visit_string(&mut self, value: &str, node: &Node) {
        // Check if this is part of a function definition pattern
        if self.patterns.contains_key(&node.span) {
            // Just record as pending name, don't push to stack
            self.pending_name = Some((value.to_string(), node.span));
            return;
        }

        self.pending_name = Some((value.to_string(), node.span));
        self.stack.push(Type::Known(TypeId::STRING), Origin::Literal(node.span));
    }

    fn visit_symbol(&mut self, sym: Symbol, node: &Node) {
        let name = self.interner.resolve(sym).to_string();

        // Add a read reference
        self.add_reference(name.clone(), node.span, ReferenceKind::Read);

        // Check if it's a known global function
        if self.globals.contains_key(&name) {
            self.handle_function_call(&name, node.span);
            return;
        }

        // Check if it's a known definition
        if let Some(def_id) = self.lookup_definition(&name) {
            let ty = self.symbols
                .get_definition(def_id)
                .and_then(|d| d.value_type.clone())
                .unwrap_or(Type::Unknown);

            self.stack.push(ty, Origin::Binding(def_id));
            return;
        }

        // Unknown symbol - dynamic effect
        self.stack.clear();
        self.stack.depth_known = false;
    }

    fn visit_local_ref(&mut self, index: usize, node: &Node) {
        if let Some(def_id) = self.lookup_local(index) {
            // Create a reference
            let name = self.symbols
                .get_definition(def_id)
                .map(|d| d.name.clone())
                .unwrap_or_default();
            let ref_obj = Reference::new(name, node.span, ReferenceKind::Read, self.current_scope);
            let ref_id = self.symbols.add_reference(ref_obj);
            self.symbols.resolve_reference(ref_id, def_id);

            // Mark definition as referenced
            if let Some(def) = self.symbols.get_definition_mut(def_id) {
                def.referenced = true;
            }

            let ty = self.symbols
                .get_definition(def_id)
                .and_then(|d| d.value_type.clone())
                .unwrap_or(Type::Unknown);

            self.stack.push(ty, Origin::Binding(def_id));
        } else {
            self.stack.push(Type::Unknown, Origin::Unknown);
        }
    }

    fn visit_command(&mut self, lib: LibId, cmd: u16, node: &Node) {
        // Check if this is part of a pattern (STO for function def)
        if let Some(Pattern::StoreDef { target_span: _ }) = self.patterns.get(&node.span) {
            // Skip - already handled in Phase 2
            self.pending_name = None;
            return;
        }

        // Store stack snapshot BEFORE applying any effects (for lowering)
        self.node_stacks.insert(node.span, StackSnapshot {
            tos: self.stack.type_at(0),
            nos: self.stack.type_at(1),
            depth: self.stack.depth(),
            depth_known: self.stack.depth_known,
        });

        // Handle binding effects (STO, RCL, etc.)
        if let Some(binding_kind) = self.registry.get_binding_effect(lib, cmd) {
            match binding_kind {
                BindingKind::Define => {
                    // STO - store to global
                    // Stack has: [value, name] with name on top
                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.stack.pop(); // pop name (STRING type)
                        let (value_ty, _value_origin) = self.stack.pop(); // pop value

                        // Create or update global definition
                        let existing = self.lookup_definition(&name);
                        if existing.is_none() {
                            let def = Definition::with_type(
                                name.clone(),
                                name_span,
                                DefinitionKind::Global,
                                self.current_scope,
                                value_ty,
                            );
                            let _def_id = self.add_definition(def);
                            self.add_reference(name, name_span, ReferenceKind::Write);
                        }
                    }
                    return;
                }
                BindingKind::Read => {
                    // RCL - read from global
                    if let Some((name, name_span)) = self.pending_name.take() {
                        self.stack.pop(); // name
                        self.add_reference(name.clone(), name_span, ReferenceKind::Read);

                        if let Some(def_id) = self.lookup_definition(&name) {
                            let ty = self.symbols
                                .get_definition(def_id)
                                .and_then(|d| d.value_type.clone())
                                .unwrap_or(Type::Unknown);
                            self.stack.push(ty, Origin::Binding(def_id));
                        } else {
                            self.stack.push(Type::Unknown, Origin::Unknown);
                        }
                    }
                    return;
                }
                _ => {}
            }
        }

        // Get stack effect and apply
        let tos = self.stack.type_at(0).as_known();
        let nos = self.stack.type_at(1).as_known();
        let effect = self.registry.get_command_effect(lib, cmd, tos, nos);
        self.apply_command_effect(lib, cmd, &effect, node.span);
    }

    fn visit_symbolic(&mut self, expr: &SymExpr, node: &Node) {
        // Check if it's a variable name (for STO pattern)
        if let SymExpr::Var(name) = expr {
            self.pending_name = Some((name.to_string(), node.span));
        }
        self.stack.push(Type::Known(TypeId::SYMBOLIC), Origin::Literal(node.span));
    }

    fn visit_program(&mut self, _body: &Branch, node: &Node) {
        // Check if this is a function definition
        let func_name = if let Some(Pattern::FunctionDef { name, .. }) = self.patterns.get(&node.span) {
            self.current_function = Some(name.clone());
            Some(name.clone())
        } else {
            None
        };

        // Enter program scope
        self.enter_scope(ScopeKind::Program, node.span);

        // If this is a function definition, pre-populate scope with Phase 2 param definitions
        // This ensures constraints are collected against the correct definition IDs
        if let Some(ref name) = func_name
            && let Some(info) = self.globals.get(name)
        {
            for &param_def_id in &info.param_def_ids {
                if let Some(def) = self.symbols.get_definition(param_def_id) {
                    let param_name = def.name.clone();
                    let local_index = def.local_index;

                    // Add to scope lookups
                    if let Some(scope_map) = self.scope_definitions.last_mut() {
                        scope_map.insert(param_name, param_def_id);
                    }
                    if let (Some(idx), Some(local_map)) = (local_index, self.local_index_to_def.last_mut()) {
                        local_map.insert(idx, param_def_id);
                    }
                }
            }
        }

        // Save current stack
        self.saved_stacks.push(std::mem::take(&mut self.stack));
        self.stack = StackState::new();
    }

    fn visit_program_post(&mut self, _body: &Branch, node: &Node) {
        // Capture return type and origin before exiting
        // If stack depth is unknown (dynamic operations), return Unknown
        let (return_ty, return_origin) = if !self.stack.depth_known || self.stack.depth() == 0 {
            (Type::Unknown, Origin::Unknown)
        } else {
            (self.stack.type_at(0), self.stack.origin_at(0))
        };

        // If this is a function, unify return type with TypeVar and store origin
        if let Some(ref func_name) = self.current_function
            && let Some(info) = self.globals.get(func_name) {
                self.substitution.unify(info.return_type_var, return_ty);
                // Store the return origin for later resolution in finalize_signatures
                self.return_origins.insert(func_name.clone(), return_origin);
            }

        self.current_function = None;

        // Exit scope
        self.exit_scope();

        // Restore saved stack
        if let Some(saved) = self.saved_stacks.pop() {
            self.stack = saved;
        }

        // Push program type
        self.stack.push(Type::Known(TypeId::PROGRAM), Origin::Literal(node.span));
    }

}

#[cfg(test)]
mod tests {
    // Basic tests will be added once mod.rs is complete
}
