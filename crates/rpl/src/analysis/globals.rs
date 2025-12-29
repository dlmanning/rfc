//! Phase 2: Collect global function and variable definitions.
//!
//! This module processes patterns from Phase 1 to create preliminary
//! definitions for all global functions and variables. This enables:
//! - Forward references (calling a function defined later)
//! - Cross-file variable references (using a variable defined via STO elsewhere)
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

/// Information about a global variable collected from STO patterns.
#[derive(Clone, Debug)]
pub struct GlobalVarInfo {
    /// Definition ID in the symbol table.
    pub def_id: DefinitionId,
    /// Span of the variable name.
    pub name_span: Span,
    /// Source file where this variable was defined.
    pub defining_file: String,
    /// Directory path where the variable is stored.
    pub directory_path: Vec<String>,
    /// Type variable for HM inference.
    pub type_var: TypeVar,
}

/// Map from fully-qualified name to global variable info.
///
/// The key is the full path to the variable, e.g. "lib/data/ship_id".
/// For variables in the root directory, the key is just the name.
pub type GlobalVariableMap = HashMap<String, GlobalVarInfo>;

/// Build a fully-qualified key from a directory path and variable name.
pub fn make_global_var_key(path: &[String], name: &str) -> String {
    if path.is_empty() {
        name.to_string()
    } else {
        format!("{}/{}", path.join("/"), name)
    }
}

/// Collect global variable definitions from GlobalDefine patterns.
///
/// Creates Definition entries for all global variables found in Phase 1.
/// Assigns fresh TypeVars for type inference.
///
/// # Arguments
///
/// * `defines` - Vec of GlobalDefine patterns from collect_global_defines
/// * `symbols` - Symbol table to add definitions to
/// * `file_key` - Key of the file being processed (for diagnostics)
/// * `initial_type_var` - Starting type variable ID
///
/// Returns the GlobalVariableMap and the next available type variable ID.
pub fn collect_global_variables(
    defines: &[Pattern],
    symbols: &mut SymbolTable,
    file_key: &str,
    initial_type_var: u32,
) -> (GlobalVariableMap, u32) {
    let mut vars = GlobalVariableMap::new();
    let mut next_type_var = initial_type_var;

    for pattern in defines {
        if let Pattern::GlobalDefine {
            name,
            name_span,
            directory_path,
        } = pattern
        {
            let key = make_global_var_key(directory_path, name);

            // Skip if already defined (will be unified later)
            if vars.contains_key(&key) {
                continue;
            }

            // Create a fresh type variable for this variable
            let var_tv = TypeVar(next_type_var);
            next_type_var += 1;

            // Create the variable definition
            let def = Definition::with_type(
                name.clone(),
                *name_span,
                DefinitionKind::Global,
                ScopeId::root(),
                Type::TypeVar(var_tv),
            );

            let def_id = symbols.add_definition(def);

            vars.insert(
                key,
                GlobalVarInfo {
                    def_id,
                    name_span: *name_span,
                    defining_file: file_key.to_string(),
                    directory_path: directory_path.clone(),
                    type_var: var_tv,
                },
            );
        }
    }

    (vars, next_type_var)
}

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
            // Create parameter definitions (each gets a TypeVar)
            let param_def_ids = create_param_definitions(params, symbols, &mut next_type_var);

            // Create a fresh type variable for the return type
            let return_tv = TypeVar(next_type_var);
            next_type_var += 1;

            // Create preliminary signature
            // Inputs use the same TypeVars as parameter definitions
            let inputs: Vec<Type> = param_def_ids
                .iter()
                .filter_map(|&def_id| {
                    symbols
                        .get_definition(def_id)
                        .and_then(|d| d.value_type.clone())
                })
                .collect();

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
///
/// Each parameter gets a fresh TypeVar so constraints can resolve its type.
fn create_param_definitions(
    params: &[ParamInfo],
    symbols: &mut SymbolTable,
    next_type_var: &mut u32,
) -> Vec<DefinitionId> {
    params
        .iter()
        .map(|param| {
            // Create a fresh type variable for this parameter
            let param_tv = TypeVar(*next_type_var);
            *next_type_var += 1;

            let mut def = Definition::new(
                param.name.clone(),
                param.span,
                DefinitionKind::Local,
                ScopeId::root(), // Will be updated during traversal
            );
            def.local_index = Some(param.local_index);
            // Use TypeVar so constraints can resolve the type
            def.value_type = Some(Type::TypeVar(param_tv));

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
        // Return TypeVar is after the 2 param TypeVars (0, 1)
        assert_eq!(info.return_type_var, TypeVar(2));

        // Next type var should be 3 (2 params + 1 return)
        assert_eq!(next_tv, 3);
    }
}
