//! Static analysis for RPL.
//!
//! This module provides static analysis capabilities for RPL programs,
//! working on the IR (intermediate representation) rather than raw tokens.
//!
//! # Architecture
//!
//! The analyzer uses a 4-phase architecture:
//!
//! 1. **Pattern Recognition**: Scan for multi-node patterns like function definitions
//! 2. **Global Collection**: Create preliminary definitions for global functions
//! 3. **Main Traversal**: Walk IR with parallel type/origin tracking
//! 4. **Constraint Resolution**: Solve type constraints using union-find
//!
//! ```text
//! Source → Parse → IR → Analysis → AnalysisResult
//!                       ↓
//!                  SymbolTable + ScopeTree + Diagnostics
//! ```
//!
//! # Features
//!
//! - **Symbol tracking**: Track variable definitions and references
//! - **Scope analysis**: Build scope trees for local bindings
//! - **Type inference**: Infer types from usage and propagate constraints
//! - **Diagnostics**: Report undefined variables, type errors, unused definitions
//!
//! # Example
//!
//! ```ignore
//! use rpl::analysis::analyze;
//!
//! let result = analyze(&nodes, &registry, &interner);
//! // result contains symbol table, scope tree, and diagnostics
//! ```

// Core infrastructure modules
mod context;
mod incremental;
mod result;
mod scopes;
mod symbols;
mod visitor;

// Analyzer phases
mod globals;
mod patterns;
mod resolve;
mod state;
mod traverse;
mod types;

// Re-exports: Core infrastructure
pub use context::{Context, EntryInfo};
pub use incremental::{line_edit_to_span_edit, IncrementalAnalysis, SpanEdit};
pub use result::{AnalysisResult, Diagnostic, DiagnosticKind, Severity};
pub use scopes::{Scope, ScopeId, ScopeKind, ScopeTree};
pub use symbols::{
    Definition, DefinitionId, DefinitionKind, Reference, ReferenceId, ReferenceKind, SymbolTable,
};
pub use visitor::{walk_node, walk_nodes, Visitor};

// Re-exports: Analyzer phases
pub use globals::{collect_globals, GlobalInfo, GlobalMap};
pub use patterns::{recognize_patterns, ParamInfo, Pattern, PatternMap};
pub use resolve::{finalize_signatures, resolve_constraints};
pub use state::{StackState, Substitution};
pub use traverse::{Traverser, TraversalResult};
pub use types::{Constraint, Origin, Requirement, StackSnapshot, Type, TypeVar};

use crate::core::Interner;
use crate::ir::Node;
use crate::registry::InterfaceRegistry;

/// Analyze IR nodes using the 4-phase analyzer.
///
/// This is the main entry point for static analysis.
///
/// # Arguments
///
/// * `nodes` - Parsed IR nodes from the parser
/// * `registry` - Library interface registry
/// * `interner` - Symbol interner
/// * `context` - External context (known project entries, etc.)
///
/// # Returns
///
/// An `AnalysisResult` containing the symbol table, scope tree, and diagnostics.
pub fn analyze(
    nodes: &[Node],
    registry: &InterfaceRegistry,
    interner: &Interner,
    context: &Context,
) -> AnalysisResult {
    // Phase 1: Pattern recognition
    let patterns = recognize_patterns(nodes, registry);

    // Phase 2: Global collection
    let mut symbols = SymbolTable::new();
    let (globals, next_type_var) = collect_globals(&patterns, &mut symbols, 0);

    // Phase 3: Main traversal
    let traverser = Traverser::new(
        registry,
        interner,
        &patterns,
        &globals,
        next_type_var,
        symbols,
    );
    let TraversalResult {
        mut symbols,
        scopes,
        mut diagnostics,
        constraints,
        mut substitution,
        return_origins,
        node_stacks,
        ..
    } = traverser.traverse(nodes);

    // Resolve TypeVars in definition value_types before constraint resolution.
    // This handles cases where a local is bound to a function call result (TypeVar)
    // and the function's return type is resolved later during traversal.
    let def_ids: Vec<_> = symbols.definitions().map(|d| d.id).collect();

    for def_id in def_ids {
        if let Some(def) = symbols.get_definition_mut(def_id)
            && let Some(ref ty) = def.value_type
            && ty.is_type_var()
        {
            let resolved = substitution.apply(ty);
            def.value_type = Some(resolved);
        }
    }

    // Phase 4: Constraint resolution
    let resolution_diagnostics = resolve_constraints(constraints, &mut symbols, &mut substitution);
    diagnostics.extend(resolution_diagnostics);

    // Finalize signatures with resolved types
    finalize_signatures(&mut symbols, &substitution, &return_origins);

    // Apply substitution to node_stacks so lowerer gets resolved types
    let node_stacks = node_stacks
        .into_iter()
        .map(|(span, snapshot)| {
            (
                span,
                StackSnapshot {
                    tos: substitution.apply(&snapshot.tos),
                    nos: substitution.apply(&snapshot.nos),
                    depth: snapshot.depth,
                    depth_known: snapshot.depth_known,
                },
            )
        })
        .collect();

    // Post-processing checks
    let post_diagnostics = post_analysis_checks(&symbols, context);
    diagnostics.extend(post_diagnostics);

    AnalysisResult {
        symbols,
        scopes,
        diagnostics,
        node_stacks,
    }
}

/// Run post-analysis checks for unused variables, etc.
fn post_analysis_checks(symbols: &SymbolTable, context: &Context) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Check for unresolved references (skip if known in context)
    for reference in symbols.unresolved_references() {
        if !context.is_known(&reference.name) {
            diagnostics.push(Diagnostic::undefined_variable(&reference.name, reference.span));
        }
    }

    // Check for unused local variables
    for def in symbols.unreferenced_definitions() {
        if matches!(def.kind, DefinitionKind::Local | DefinitionKind::LoopVar) {
            diagnostics.push(Diagnostic::unused_variable(&def.name, def.span));
        }
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Pos, Span};
    use crate::ir::Node;

    fn dummy_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    fn setup_test() -> (InterfaceRegistry, Interner) {
        let registry = InterfaceRegistry::new();
        let interner = Interner::new();
        (registry, interner)
    }

    #[test]
    fn analyze_empty() {
        let (registry, interner) = setup_test();
        let nodes: Vec<Node> = vec![];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn analyze_integer_literal() {
        let (registry, interner) = setup_test();
        let nodes = vec![Node::integer(42, dummy_span(0, 2))];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn analyze_real_literal() {
        let (registry, interner) = setup_test();
        let nodes = vec![Node::real(3.14, dummy_span(0, 4))];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn analyze_string_literal() {
        let (registry, interner) = setup_test();
        let nodes = vec![Node::string("hello", dummy_span(0, 7))];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn analyze_program() {
        let (registry, interner) = setup_test();
        let nodes = vec![Node::program(
            vec![Node::integer(1, dummy_span(3, 4))],
            dummy_span(0, 6),
        )];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn analyze_list() {
        let (registry, interner) = setup_test();
        let nodes = vec![Node::list(
            vec![
                Node::integer(1, dummy_span(2, 3)),
                Node::integer(2, dummy_span(4, 5)),
            ],
            dummy_span(0, 7),
        )];

        let result = analyze(&nodes, &registry, &interner, &Context::empty());

        assert!(result.diagnostics.is_empty());
    }
}
