//! Phase 2: Collect global function definitions.
//!
//! This module processes patterns from Phase 1 to create preliminary
//! definitions for all global functions. This enables:
//! - Forward references (calling a function defined later)
//! - HM-style type inference (linking caller args to callee params)
//! - Consistent definition IDs across passes

use std::collections::HashMap;

use super::{
    patterns::{ParamInfo, Pattern, PatternMap},
    types::{Type, TypeVar},
};
use crate::{
    analysis::{Definition, DefinitionId, DefinitionKind, ScopeId, SymbolTable},
    core::Span,
    types::Signature,
};

/// Information about a global function collected in Phase 2.
#[derive(Clone, Debug)]
pub struct GlobalInfo {
    /// Definition ID in the symbol table.
    pub def_id: DefinitionId,
    /// Preliminary signature with TypeVars for unknown parts.
    pub signature: Signature,
    /// Type variable for the return type (for HM inference).
    pub return_type_var: TypeVar,
    /// Span of the program body.
    pub body_span: Span,
    /// Parameter definition IDs (pre-created for linking).
    pub param_def_ids: Vec<DefinitionId>,
}

/// Map from function name to global info.
pub type GlobalMap = HashMap<String, GlobalInfo>;

/// Collect global function definitions from patterns (Phase 2).
///
/// Creates preliminary Definition entries for all functions found in Phase 1.
/// Assigns fresh TypeVars for return types to enable constraint propagation.
///
/// Returns the GlobalMap and the next available type variable ID.
pub fn collect_globals(
    patterns: &PatternMap,
    symbols: &mut SymbolTable,
    initial_type_var: u32,
) -> (GlobalMap, u32) {
    let mut globals = GlobalMap::new();
    let mut next_type_var = initial_type_var;

    for pattern in patterns.values() {
        if let Pattern::FunctionDef {
            name,
            name_span,
            body_span,
            param_count,
            params,
        } = pattern
        {
            // Create parameter definitions
            let param_def_ids = create_param_definitions(params, symbols);

            // Create a fresh type variable for the return type
            let return_tv = TypeVar(next_type_var);
            next_type_var += 1;

            // Create preliminary signature
            // Inputs are Unknown (will be narrowed by constraints)
            let inputs: Vec<Type> = param_def_ids.iter().map(|_| Type::Unknown).collect();

            // Output is a TypeVar (will be unified with actual return type)
            let outputs = vec![Type::TypeVar(return_tv)];

            let sig = Signature::with_param_def_ids(inputs, outputs, param_def_ids.clone());

            // Create the function definition
            let mut def = Definition::with_type(
                name.clone(),
                *name_span,
                DefinitionKind::Global,
                ScopeId::root(),
                Type::program(),
            );
            def.arity = Some(*param_count);
            def.signature = Some(sig.clone());

            let def_id = symbols.add_definition(def);

            globals.insert(
                name.clone(),
                GlobalInfo {
                    def_id,
                    signature: sig,
                    return_type_var: return_tv,
                    body_span: *body_span,
                    param_def_ids,
                },
            );
        }
    }

    (globals, next_type_var)
}

/// Create parameter definitions from param info.
fn create_param_definitions(params: &[ParamInfo], symbols: &mut SymbolTable) -> Vec<DefinitionId> {
    params
        .iter()
        .map(|param| {
            let mut def = Definition::new(
                param.name.clone(),
                param.span,
                DefinitionKind::Local,
                ScopeId::root(), // Will be updated during traversal
            );
            def.local_index = Some(param.local_index);
            // Type is Unknown initially; will be narrowed by constraints
            def.value_type = Some(Type::Unknown);

            symbols.add_definition(def)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Pos;

    fn dummy_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn collect_empty_patterns() {
        let patterns = PatternMap::new();
        let mut symbols = SymbolTable::new();

        let (globals, next_tv) = collect_globals(&patterns, &mut symbols, 0);

        assert!(globals.is_empty());
        assert_eq!(next_tv, 0);
    }

    #[test]
    fn collect_function_def() {
        let mut patterns = PatternMap::new();

        patterns.insert(
            dummy_span(0, 20),
            Pattern::FunctionDef {
                name: "test".to_string(),
                name_span: dummy_span(22, 28),
                body_span: dummy_span(0, 20),
                param_count: 2,
                params: vec![
                    ParamInfo {
                        name: "x".to_string(),
                        span: dummy_span(5, 6),
                        local_index: 0,
                    },
                    ParamInfo {
                        name: "y".to_string(),
                        span: dummy_span(7, 8),
                        local_index: 1,
                    },
                ],
            },
        );

        let mut symbols = SymbolTable::new();
        let (globals, next_tv) = collect_globals(&patterns, &mut symbols, 0);

        assert_eq!(globals.len(), 1);
        assert!(globals.contains_key("test"));

        let info = &globals["test"];
        assert_eq!(info.param_def_ids.len(), 2);
        assert_eq!(info.return_type_var, TypeVar(0));

        // Next type var should be 1
        assert_eq!(next_tv, 1);
    }
}
