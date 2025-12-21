//! Phase 1: Pattern recognition for multi-node constructs.
//!
//! This module scans the IR before traversal to identify patterns like:
//! - `<< body >> "name" STO` (function definitions)
//! - `value "name" STO` (store to global)
//!
//! Pre-recognizing patterns eliminates stateful pattern matching during
//! the main traversal, making the code cleaner and more predictable.

use std::collections::HashMap;

use crate::core::Span;
use crate::interface::BindingKind;
use crate::ir::{AtomKind, Branch, CompositeKind, Node, NodeKind};
use crate::registry::InterfaceRegistry;
use crate::symbolic::SymExpr;

/// Recognized multi-node patterns in the IR.
#[derive(Clone, Debug)]
pub enum Pattern {
    /// Function definition: `<< body >> "name" STO`
    FunctionDef {
        /// The function name.
        name: String,
        /// Span of the name node.
        name_span: Span,
        /// Span of the program node.
        body_span: Span,
        /// Number of parameters (from leading local bindings).
        param_count: usize,
        /// Parameter names and spans (for creating definitions).
        params: Vec<ParamInfo>,
    },
    /// The name node in a function definition (for marking as part of pattern).
    NameOfDef {
        /// Span of the associated program body.
        target_span: Span,
    },
    /// The STO command in a function definition (for skipping during traversal).
    StoreDef {
        /// Span of the associated program body.
        target_span: Span,
    },
}

/// Information about a function parameter.
#[derive(Clone, Debug)]
pub struct ParamInfo {
    /// Parameter name.
    pub name: String,
    /// Span of the parameter name.
    pub span: Span,
    /// Local index assigned during parsing.
    pub local_index: usize,
}

/// Map from node spans to recognized patterns.
pub type PatternMap = HashMap<Span, Pattern>;

/// Scan IR for multi-node patterns (Phase 1).
///
/// This identifies patterns before the main traversal begins, enabling:
/// - Function definitions to be recognized upfront
/// - Forward references to work correctly
/// - Cleaner traversal without stateful pattern matching
pub fn recognize_patterns(nodes: &[Node], registry: &InterfaceRegistry) -> PatternMap {
    let mut patterns = PatternMap::new();
    let mut i = 0;

    while i < nodes.len() {
        // Pattern: Program + Name + STO (function definition)
        if i + 2 < nodes.len()
            && let Some((pattern, name_pattern, sto_pattern)) =
                try_function_def(&nodes[i], &nodes[i + 1], &nodes[i + 2], registry)
            {
                let body_span = nodes[i].span;
                patterns.insert(body_span, pattern);
                patterns.insert(nodes[i + 1].span, name_pattern);
                patterns.insert(nodes[i + 2].span, sto_pattern);
                i += 3;
                continue;
            }

        // Recurse into composite nodes
        if let NodeKind::Composite(_, branches) = &nodes[i].kind {
            for branch in branches {
                let sub_patterns = recognize_patterns(branch, registry);
                patterns.extend(sub_patterns);
            }
        }

        i += 1;
    }

    patterns
}

/// Try to recognize a function definition pattern.
fn try_function_def(
    program_node: &Node,
    name_node: &Node,
    sto_node: &Node,
    registry: &InterfaceRegistry,
) -> Option<(Pattern, Pattern, Pattern)> {
    // Check: program_node is Program
    let body_branches = match &program_node.kind {
        NodeKind::Composite(CompositeKind::Program, branches) => branches,
        _ => return None,
    };

    // Check: name_node is a string or symbolic name
    let (name, name_span) = extract_name(name_node)?;

    // Check: sto_node is a command with Define binding effect
    if !is_define_command(sto_node, registry) {
        return None;
    }

    // Extract parameters from the program body
    let (param_count, params) = extract_program_params(body_branches, registry);

    let body_span = program_node.span;

    let main_pattern = Pattern::FunctionDef {
        name,
        name_span,
        body_span,
        param_count,
        params,
    };

    let name_pattern = Pattern::NameOfDef {
        target_span: body_span,
    };

    let sto_pattern = Pattern::StoreDef {
        target_span: body_span,
    };

    Some((main_pattern, name_pattern, sto_pattern))
}

/// Extract a name from a node that might be used with STO.
fn extract_name(node: &Node) -> Option<(String, Span)> {
    match &node.kind {
        NodeKind::Atom(AtomKind::String(s)) => Some((s.to_string(), node.span)),
        NodeKind::Atom(AtomKind::Symbolic(sym)) => {
            if let SymExpr::Var(s) = sym.as_ref() {
                Some((s.to_string(), node.span))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Check if a node is a command with BindingKind::Define effect.
fn is_define_command(node: &Node, registry: &InterfaceRegistry) -> bool {
    if let NodeKind::Atom(AtomKind::Command(lib, cmd)) = &node.kind {
        matches!(
            registry.get_binding_effect(*lib, *cmd),
            Some(BindingKind::Define)
        )
    } else {
        false
    }
}

/// Extract parameters from a program body.
///
/// Returns (param_count, param_info_list).
fn extract_program_params(body_branches: &[Branch], registry: &InterfaceRegistry) -> (usize, Vec<ParamInfo>) {
    // A program with parameters starts with a local binding construct (->)
    // Body structure: [[binding_construct, ...rest]]
    if body_branches.is_empty() || body_branches[0].is_empty() {
        return (0, vec![]);
    }

    let first_node = &body_branches[0][0];

    // Check if it's an Extended construct (local binding)
    let (lib, construct_id, inner_branches) = match &first_node.kind {
        NodeKind::Composite(CompositeKind::Extended(lib, construct_id), branches) => {
            (*lib, *construct_id, branches)
        }
        _ => return (0, vec![]),
    };

    // Get binding branches for this construct
    let binding_indices = registry.binding_branches(lib, construct_id, inner_branches.len());
    if binding_indices.is_empty() {
        return (0, vec![]);
    }

    // Extract parameter names from binding branches
    // Binding branches have format: [Integer(index), String(name)]
    let mut params = Vec::new();

    for &branch_idx in &binding_indices {
        if branch_idx >= inner_branches.len() {
            continue;
        }

        let binding = &inner_branches[branch_idx];
        if binding.len() >= 2 {
            let local_idx = if let NodeKind::Atom(AtomKind::Integer(n)) = &binding[0].kind {
                Some(*n as usize)
            } else {
                None
            };
            let name_info = if let NodeKind::Atom(AtomKind::String(s)) = &binding[1].kind {
                Some((s.to_string(), binding[1].span))
            } else {
                None
            };

            if let (Some(idx), Some((name, span))) = (local_idx, name_info) {
                params.push(ParamInfo {
                    name,
                    span,
                    local_index: idx,
                });
            }
        }
    }

    (params.len(), params)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn dummy_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn extract_string_name() {
        let node = Node::string("test", dummy_span(0, 6));
        let result = extract_name(&node);
        assert!(result.is_some());
        assert_eq!(result.unwrap().0, "test");
    }

    #[test]
    fn extract_symbolic_name() {
        use crate::symbolic::SymExpr;
        use std::sync::Arc;

        let node = Node::new(
            NodeKind::Atom(AtomKind::Symbolic(Arc::new(SymExpr::Var("foo".into())))),
            dummy_span(0, 4),
        );
        let result = extract_name(&node);
        assert!(result.is_some());
        assert_eq!(result.unwrap().0, "foo");
    }
}
