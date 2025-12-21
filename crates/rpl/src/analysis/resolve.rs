//! Phase 4: Constraint resolution using union-find.
//!
//! This module resolves type constraints collected during Phase 3:
//! 1. Build equivalence classes from Equal constraints (union-find)
//! 2. Group MustBe constraints by equivalence class
//! 3. Intersect requirements within each class
//! 4. Report conflicts as diagnostics
//! 5. Apply unified types to all definitions in class

use std::collections::HashMap;

use smallvec::SmallVec;

use super::{
    state::Substitution,
    types::{Constraint, Requirement, Type},
};
use crate::{
    analysis::{DefinitionId, Diagnostic, SymbolTable},
    core::{Span, TypeId},
};

/// Union-find structure for building type equivalence classes.
struct UnionFind {
    /// Parent pointers (def_id -> parent def_id).
    parent: HashMap<DefinitionId, DefinitionId>,
    /// Rank for union by rank optimization.
    rank: HashMap<DefinitionId, usize>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parent: HashMap::new(),
            rank: HashMap::new(),
        }
    }

    /// Initialize a set containing just this element.
    fn make_set(&mut self, id: DefinitionId) {
        self.parent.entry(id).or_insert(id);
        self.rank.entry(id).or_insert(0);
    }

    /// Find the root of the set containing this element.
    /// Uses path compression for efficiency.
    fn find(&mut self, id: DefinitionId) -> DefinitionId {
        let parent = *self.parent.get(&id).unwrap_or(&id);
        if parent != id {
            let root = self.find(parent);
            self.parent.insert(id, root); // Path compression
            root
        } else {
            id
        }
    }

    /// Union two sets. Uses union by rank.
    fn union(&mut self, a: DefinitionId, b: DefinitionId) {
        let root_a = self.find(a);
        let root_b = self.find(b);

        if root_a == root_b {
            return;
        }

        let rank_a = *self.rank.get(&root_a).unwrap_or(&0);
        let rank_b = *self.rank.get(&root_b).unwrap_or(&0);

        if rank_a < rank_b {
            self.parent.insert(root_a, root_b);
        } else if rank_a > rank_b {
            self.parent.insert(root_b, root_a);
        } else {
            self.parent.insert(root_b, root_a);
            *self.rank.entry(root_a).or_insert(0) += 1;
        }
    }

    /// Get all elements grouped by their root.
    fn get_classes(&mut self) -> HashMap<DefinitionId, Vec<DefinitionId>> {
        let ids: Vec<_> = self.parent.keys().copied().collect();
        let mut classes: HashMap<DefinitionId, Vec<DefinitionId>> = HashMap::new();

        for id in ids {
            let root = self.find(id);
            classes.entry(root).or_default().push(id);
        }

        classes
    }
}

/// MustBe constraint with location info.
struct MustBeInfo {
    def_id: DefinitionId,
    requirement: Requirement,
    span: Span,
    operation: String,
}

/// Resolve all constraints and update the symbol table.
///
/// Returns diagnostics for any type conflicts found.
pub fn resolve_constraints(
    constraints: Vec<Constraint>,
    symbols: &mut SymbolTable,
    substitution: &mut Substitution,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut uf = UnionFind::new();
    let mut must_be_constraints: Vec<MustBeInfo> = Vec::new();
    let mut called_with_types: HashMap<DefinitionId, SmallVec<[TypeId; 4]>> = HashMap::new();

    // Step 1: Process constraints
    for constraint in constraints {
        match constraint {
            Constraint::Equal {
                def_a,
                def_b,
                span: _,
            } => {
                uf.make_set(def_a);
                uf.make_set(def_b);
                uf.union(def_a, def_b);
            }
            Constraint::MustBe {
                def_id,
                requirement,
                span,
                operation,
            } => {
                uf.make_set(def_id);
                must_be_constraints.push(MustBeInfo {
                    def_id,
                    requirement,
                    span,
                    operation,
                });
            }
            Constraint::CalledWith {
                def_id,
                type_id,
                span: _,
            } => {
                uf.make_set(def_id);
                // Collect all types this parameter is called with (union them)
                let types = called_with_types.entry(def_id).or_default();
                if !types.contains(&type_id) {
                    types.push(type_id);
                }
            }
        }
    }

    // Step 1b: Convert CalledWith unions to MustBe constraints
    // If a parameter is called with multiple types, it must accept all of them (OneOf)
    for (def_id, types) in called_with_types {
        let requirement = if types.len() == 1 {
            Requirement::Exact(types[0])
        } else {
            Requirement::OneOf(types)
        };
        must_be_constraints.push(MustBeInfo {
            def_id,
            requirement,
            span: Span::default(), // No specific span for unioned constraint
            operation: "call".to_string(),
        });
    }

    // Step 2: Group MustBe constraints by equivalence class root
    let classes = uf.get_classes();
    let mut class_constraints: HashMap<DefinitionId, Vec<&MustBeInfo>> = HashMap::new();

    for info in &must_be_constraints {
        let root = uf.find(info.def_id);
        class_constraints.entry(root).or_default().push(info);
    }

    // Step 2b: Unify TypeVars within each equivalence class
    // When definitions are linked by Equal constraints, their TypeVars should be unified
    // so that resolving one resolves all of them
    for members in classes.values() {
        let mut representative_tv: Option<super::TypeVar> = None;
        for &member in members {
            if let Some(def) = symbols.get_definition(member)
                && let Some(Type::TypeVar(tv)) = &def.value_type {
                    if let Some(repr) = representative_tv {
                        // Unify this TypeVar with the representative
                        substitution.unify(*tv, Type::TypeVar(repr));
                    } else {
                        representative_tv = Some(*tv);
                    }
                }
        }
    }

    // Step 3: For each equivalence class, unify requirements
    for (root, constraints) in class_constraints {
        let members = classes.get(&root).cloned().unwrap_or_else(|| vec![root]);

        match unify_requirements(&constraints) {
            Ok(final_req) => {
                // Apply the unified requirement to all members of the class
                apply_requirement_to_class(&members, &final_req, symbols, substitution);
            }
            Err((first, second)) => {
                // Report type conflict
                let def_name = get_def_name(root, symbols);
                diagnostics.push(Diagnostic::type_mismatch(
                    &def_name,
                    second.span,
                    &format_requirement(&first.requirement),
                    &first.operation,
                    &format_requirement(&second.requirement),
                    &second.operation,
                    first.span,
                ));
            }
        }
    }

    diagnostics
}

/// Unify a list of MustBe constraints into a single requirement.
///
/// Returns Err with the two conflicting constraints if unification fails.
fn unify_requirements<'a>(
    constraints: &[&'a MustBeInfo],
) -> Result<Requirement, (&'a MustBeInfo, &'a MustBeInfo)> {
    if constraints.is_empty() {
        return Ok(Requirement::Any);
    }

    let mut result = constraints[0].requirement.clone();
    let first_constraint = constraints[0];

    for &constraint in &constraints[1..] {
        match result.intersect(&constraint.requirement) {
            Some(unified) => result = unified,
            None => return Err((first_constraint, constraint)),
        }
    }

    Ok(result)
}

/// Apply a requirement to all definitions in a class.
///
/// Also updates the substitution for any TypeVars in the definitions.
fn apply_requirement_to_class(
    members: &[DefinitionId],
    requirement: &Requirement,
    symbols: &mut SymbolTable,
    substitution: &mut Substitution,
) {
    let new_type = match requirement {
        Requirement::Exact(t) => Some(Type::Known(*t)),
        Requirement::OneOf(ts) => Some(Type::OneOf(ts.clone())),
        Requirement::Any => None,
    };

    if let Some(ty) = new_type {
        for &member in members {
            if let Some(def) = symbols.get_definition_mut(member) {
                // If the definition had a TypeVar, record the substitution
                if let Some(Type::TypeVar(tv)) = &def.value_type {
                    substitution.unify(*tv, ty.clone());
                }

                // Intersect with existing type if present
                def.value_type = Some(match &def.value_type {
                    Some(existing) => meet_types(existing, &ty),
                    None => ty.clone(),
                });
            }
        }
    }
}

/// Meet (intersect) two types.
fn meet_types(a: &Type, b: &Type) -> Type {
    match (a, b) {
        (Type::Unknown, other) | (other, Type::Unknown) => other.clone(),
        // TypeVar with concrete type: prefer concrete type
        (Type::TypeVar(_), other) | (other, Type::TypeVar(_)) => other.clone(),
        (Type::Known(t1), Type::Known(t2)) if t1 == t2 => Type::Known(*t1),
        (Type::Known(t1), Type::Known(_t2)) => Type::Known(*t1), // Conflict, keep first
        (Type::Known(t), Type::OneOf(_)) | (Type::OneOf(_), Type::Known(t)) => {
            // If it matches, narrow to t; if conflict, keep t anyway
            Type::Known(*t)
        }
        (Type::OneOf(ts1), Type::OneOf(ts2)) => {
            let intersection: smallvec::SmallVec<[TypeId; 4]> =
                ts1.iter().filter(|t| ts2.contains(t)).copied().collect();
            if intersection.is_empty() {
                a.clone() // Conflict, keep first
            } else if intersection.len() == 1 {
                Type::Known(intersection[0])
            } else {
                Type::OneOf(intersection)
            }
        }
    }
}

/// Get definition name for error messages.
fn get_def_name(def_id: DefinitionId, symbols: &SymbolTable) -> String {
    symbols
        .get_definition(def_id)
        .map(|d| d.name.clone())
        .unwrap_or_else(|| format!("def#{}", def_id.as_u32()))
}

/// Format a requirement for display.
fn format_requirement(req: &Requirement) -> String {
    match req {
        Requirement::Exact(t) => t.symbol().unwrap_or("?").to_string(),
        Requirement::OneOf(ts) => {
            let syms: Vec<_> = ts.iter().map(|t| t.symbol().unwrap_or("?")).collect();
            syms.join(" ∪ ")
        }
        Requirement::Any => "∀".to_string(),
    }
}

/// Finalize signatures by applying type variable substitutions.
///
/// Also resolves Unknown output types using return_origins: if the output type
/// resolved to Unknown but we know the origin was a binding (local variable),
/// we can use that variable's resolved type.
pub fn finalize_signatures(
    symbols: &mut SymbolTable,
    substitution: &Substitution,
    return_origins: &std::collections::HashMap<String, super::Origin>,
) {
    // Get all definition IDs with signatures
    let def_ids: Vec<_> = symbols
        .definitions()
        .filter(|d| d.signature.is_some())
        .map(|d| (d.id, d.name.clone()))
        .collect();

    for (def_id, def_name) in def_ids {
        // First, collect the information we need without holding a mutable borrow
        let updates = if let Some(def) = symbols.get_definition(def_id) {
            if let Some(ref sig) = def.signature {
                // Collect parameter types
                let param_types: Vec<_> = sig
                    .param_def_ids
                    .iter()
                    .map(|&pid| {
                        symbols
                            .get_definition(pid)
                            .and_then(|d| d.value_type.clone())
                    })
                    .collect();

                // Compute new outputs
                let new_outputs: Vec<_> = sig
                    .outputs
                    .iter()
                    .map(|ty| apply_substitution(ty, substitution))
                    .collect();

                Some((param_types, new_outputs))
            } else {
                None
            }
        } else {
            None
        };

        // Now apply the updates with a mutable borrow
        if let Some((param_types, mut new_outputs)) = updates {
            // If any output is Unknown or TypeVar, try to resolve from return origin
            if new_outputs.iter().any(|o| o.is_unknown() || o.is_type_var())
                && let Some(origin) = return_origins.get(&def_name)
            {
                // Get all def_ids from the origin (handles Phi nodes)
                let origin_def_ids = origin.all_def_ids();
                if !origin_def_ids.is_empty() {
                    // Try to resolve from the first binding's type
                    // If all bindings have the same type, use it; otherwise join them
                    let resolved_types: Vec<_> = origin_def_ids
                        .iter()
                        .filter_map(|&did| {
                            symbols
                                .get_definition(did)
                                .and_then(|d| d.value_type.clone())
                                .filter(|t| !t.is_unknown())
                        })
                        .collect();

                    if !resolved_types.is_empty() {
                        // Join all resolved types
                        let mut final_type = resolved_types[0].clone();
                        for ty in &resolved_types[1..] {
                            final_type = final_type.join(ty);
                        }

                        // Replace Unknown or TypeVar outputs with the resolved type
                        for output in &mut new_outputs {
                            if output.is_unknown() || output.is_type_var() {
                                *output = final_type.clone();
                            }
                        }
                    }
                }
            }

            if let Some(def) = symbols.get_definition_mut(def_id)
                && let Some(ref mut sig) = def.signature
            {
                // Apply parameter types
                for (i, param_ty) in param_types.into_iter().enumerate() {
                    if let Some(ty) = param_ty
                        && i < sig.inputs.len()
                    {
                        sig.inputs[i] = ty;
                    }
                }

                // Apply new outputs
                sig.outputs = new_outputs;
            }
        }
    }
}

/// Apply substitution to a Type, resolving TypeVars.
fn apply_substitution(ty: &Type, substitution: &Substitution) -> Type {
    match ty {
        Type::TypeVar(tv) => {
            if let Some(bound) = substitution.get(*tv) {
                bound.clone()
            } else {
                Type::Unknown
            }
        }
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{Definition, DefinitionKind, ScopeId},
        core::Pos,
    };

    fn dummy_span(start: u32, end: u32) -> Span {
        Span::new(Pos::new(start), Pos::new(end))
    }

    #[test]
    fn union_find_basic() {
        let mut uf = UnionFind::new();

        let a = DefinitionId::new(1);
        let b = DefinitionId::new(2);
        let c = DefinitionId::new(3);

        uf.make_set(a);
        uf.make_set(b);
        uf.make_set(c);

        // Initially all separate
        assert_ne!(uf.find(a), uf.find(b));
        assert_ne!(uf.find(b), uf.find(c));

        // Union a and b
        uf.union(a, b);
        assert_eq!(uf.find(a), uf.find(b));
        assert_ne!(uf.find(a), uf.find(c));

        // Union b and c (should transitively connect a, b, c)
        uf.union(b, c);
        assert_eq!(uf.find(a), uf.find(c));
    }

    #[test]
    fn resolve_compatible_constraints() {
        let mut symbols = SymbolTable::new();

        let def = Definition::new(
            "x".to_string(),
            dummy_span(0, 1),
            DefinitionKind::Local,
            ScopeId::root(),
        );
        let def_id = symbols.add_definition(def);

        let constraints = vec![
            Constraint::must_be(
                def_id,
                Requirement::OneOf(smallvec::smallvec![TypeId::BINT, TypeId::REAL]),
                dummy_span(10, 15),
                "+",
            ),
            Constraint::must_be(
                def_id,
                Requirement::Exact(TypeId::BINT),
                dummy_span(20, 25),
                "MOD",
            ),
        ];

        let mut subst = Substitution::new();
        let diagnostics = resolve_constraints(constraints, &mut symbols, &mut subst);

        // Should have no conflicts
        assert!(diagnostics.is_empty());

        // Type should be narrowed to BINT
        let resolved = symbols.get_definition(def_id).unwrap();
        assert_eq!(resolved.value_type, Some(Type::Known(TypeId::BINT)));
    }

    #[test]
    fn resolve_conflicting_constraints() {
        let mut symbols = SymbolTable::new();

        let def = Definition::new(
            "x".to_string(),
            dummy_span(0, 1),
            DefinitionKind::Local,
            ScopeId::root(),
        );
        let def_id = symbols.add_definition(def);

        let constraints = vec![
            Constraint::must_be(
                def_id,
                Requirement::Exact(TypeId::BINT),
                dummy_span(10, 15),
                "MOD",
            ),
            Constraint::must_be(
                def_id,
                Requirement::Exact(TypeId::STRING),
                dummy_span(20, 25),
                "SIZE",
            ),
        ];

        let mut subst = Substitution::new();
        let diagnostics = resolve_constraints(constraints, &mut symbols, &mut subst);

        // Should have a conflict
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("type conflict"));
    }

    #[test]
    fn equal_constraints_propagate() {
        let mut symbols = SymbolTable::new();

        let def_a = Definition::new(
            "a".to_string(),
            dummy_span(0, 1),
            DefinitionKind::Local,
            ScopeId::root(),
        );
        let def_b = Definition::new(
            "b".to_string(),
            dummy_span(2, 3),
            DefinitionKind::Local,
            ScopeId::root(),
        );

        let id_a = symbols.add_definition(def_a);
        let id_b = symbols.add_definition(def_b);

        let constraints = vec![
            // a and b are equal
            Constraint::equal(id_a, id_b, dummy_span(10, 15)),
            // a must be BINT
            Constraint::must_be(
                id_a,
                Requirement::Exact(TypeId::BINT),
                dummy_span(20, 25),
                "MOD",
            ),
        ];

        let mut subst = Substitution::new();
        let diagnostics = resolve_constraints(constraints, &mut symbols, &mut subst);

        assert!(diagnostics.is_empty());

        // Both should be BINT
        assert_eq!(
            symbols.get_definition(id_a).unwrap().value_type,
            Some(Type::Known(TypeId::BINT))
        );
        assert_eq!(
            symbols.get_definition(id_b).unwrap().value_type,
            Some(Type::Known(TypeId::BINT))
        );
    }
}
