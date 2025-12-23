//! Project index for cross-file analysis.
//!
//! Provides the `ProjectIndex` which holds analyzed information about all files
//! in a project, enabling cross-file type checking and LSP features.

use crate::error::LoadError;
use crate::loader;
use crate::manifest::Manifest;
use rpl::analysis::{AnalysisResult, ParamInfo};
use rpl::core::Interner;
use rpl::ir::{AtomKind, CompositeKind, Node, NodeKind};
use rpl::registry::InterfaceRegistry;
use rpl::types::Signature;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Information about a single project file.
#[derive(Debug)]
pub struct IndexEntry {
    /// The key/path in the project (e.g., "lib/square").
    pub key: String,

    /// Path to the source file.
    pub source_path: PathBuf,

    /// The source code.
    pub source: String,

    /// Parsed AST nodes.
    pub ast: Vec<Node>,

    /// The type of value this file produces (Program, List, Integer, etc.).
    pub value_type: ValueType,

    /// For programs: the inferred signature.
    pub signature: Option<Signature>,

    /// Full analysis result (symbols, diagnostics, etc.).
    pub analysis: Option<AnalysisResult>,
}

/// The type of value a project file produces.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Program,
    List,
    Integer,
    Real,
    String,
    Symbol,
    Other,
}

impl ValueType {
    /// Determine value type from a node.
    pub fn from_node(node: &Node) -> Self {
        match &node.kind {
            NodeKind::Composite(CompositeKind::Program, _) => ValueType::Program,
            NodeKind::Composite(CompositeKind::List, _) => ValueType::List,
            NodeKind::Atom(atom) => match atom {
                AtomKind::Integer(_) => ValueType::Integer,
                AtomKind::Real(_) => ValueType::Real,
                AtomKind::String(_) => ValueType::String,
                AtomKind::Symbol(_) => ValueType::Symbol,
                _ => ValueType::Other,
            },
            _ => ValueType::Other,
        }
    }

    /// Check if this is a program type.
    pub fn is_program(&self) -> bool {
        matches!(self, ValueType::Program)
    }
}

/// Index of all files in a project with their analysis results.
pub struct ProjectIndex {
    /// Project root directory.
    pub root: PathBuf,

    /// Project manifest.
    pub manifest: Manifest,

    /// Indexed entries by key.
    pub entries: HashMap<String, IndexEntry>,

    /// Interface registry for parsing.
    interfaces: InterfaceRegistry,

    /// Interner for symbols.
    interner: Interner,
}

impl ProjectIndex {
    /// Create a new empty project index.
    pub fn new(root: PathBuf, manifest: Manifest) -> Self {
        let mut interfaces = InterfaceRegistry::new();
        rpl_stdlib::register_interfaces(&mut interfaces);

        Self {
            root,
            manifest,
            entries: HashMap::new(),
            interfaces,
            interner: Interner::new(),
        }
    }

    /// Build a project index by analyzing all files.
    ///
    /// Uses global constraint resolution: all files are analyzed together so that
    /// type constraints from call sites properly narrow parameter types.
    pub fn build(project_dir: &Path) -> Result<Self, LoadError> {
        use rpl::analysis::{
            collect_globals, finalize_signatures, recognize_patterns, resolve_constraints,
            Constraint, GlobalMap, Pattern, PatternMap, Substitution, SymbolTable, Traverser,
        };

        // Load manifest
        let manifest_path = project_dir.join("project.toml");
        let manifest = Manifest::from_file(&manifest_path)?;

        let mut index = Self::new(project_dir.to_owned(), manifest.clone());

        // Collect files
        let file_paths = loader::collect_files(
            project_dir,
            &index.manifest.build.include,
            &index.manifest.build.exclude,
        )?;

        // =======================================================================
        // Phase 1: Parse all files and recognize patterns
        // =======================================================================
        struct FileData {
            key: String,
            source_path: PathBuf,
            source: String,
            nodes: Vec<Node>,
            patterns: PatternMap,
            value_type: ValueType,
        }

        let mut files: Vec<FileData> = Vec::new();

        for file_path in &file_paths {
            let key = loader::path_to_key(project_dir, file_path)?;
            let source = std::fs::read_to_string(file_path).map_err(|e| LoadError::Io {
                path: file_path.clone(),
                source: e,
            })?;

            // Parse
            let nodes = rpl::parse::parse(&source, &index.interfaces, &mut index.interner)
                .map_err(|e| LoadError::Eval {
                    path: file_path.clone(),
                    error: format!("parse error: {}", e.message),
                })?;

            // Check: must be exactly one top-level value
            if nodes.is_empty() {
                return Err(LoadError::NoValue {
                    path: file_path.clone(),
                });
            }
            if nodes.len() > 1 {
                return Err(LoadError::MultipleValues {
                    path: file_path.clone(),
                    count: nodes.len(),
                });
            }

            let value_type = ValueType::from_node(&nodes[0]);

            // Recognize patterns
            let mut patterns = recognize_patterns(&nodes, &index.interfaces);

            // For programs: inject synthetic FunctionDef pattern
            if value_type.is_program()
                && let NodeKind::Composite(CompositeKind::Program, branches) = &nodes[0].kind {
                    let (param_count, params) =
                        extract_program_params(branches, &index.interfaces);
                    patterns.insert(
                        nodes[0].span,
                        Pattern::FunctionDef {
                            name: key.clone(),
                            name_span: nodes[0].span,
                            body_span: nodes[0].span,
                            param_count,
                            params,
                        },
                    );
                }

            files.push(FileData {
                key,
                source_path: file_path.clone(),
                source,
                nodes,
                patterns,
                value_type,
            });
        }

        // =======================================================================
        // Phase 2: Collect globals from ALL patterns at once
        // =======================================================================
        // This creates shared TypeVars for all function parameters and returns,
        // enabling cross-file constraint propagation.
        let mut symbols = SymbolTable::new();
        let mut all_globals: GlobalMap = HashMap::new();
        let mut next_type_var = 0u32;

        for file in &files {
            if file.value_type.is_program() {
                let (file_globals, next_tv) =
                    collect_globals(&file.patterns, &mut symbols, next_type_var);
                next_type_var = next_tv;
                all_globals.extend(file_globals);
            }
        }

        // =======================================================================
        // Phase 3: Traverse all files, accumulating constraints
        // =======================================================================
        // Each file is traversed with the shared GlobalMap. Constraints from all
        // files are collected together for global resolution.
        let mut all_constraints: Vec<Constraint> = Vec::new();
        let mut all_return_origins: HashMap<String, rpl::analysis::Origin> = HashMap::new();
        let mut merged_substitution = Substitution::new();

        // Per-file results for building IndexEntries later
        struct PerFileAnalysis {
            diagnostics: Vec<rpl::analysis::Diagnostic>,
            node_stacks: HashMap<rpl::core::Span, rpl::analysis::StackSnapshot>,
        }
        let mut per_file_analysis: HashMap<String, PerFileAnalysis> = HashMap::new();

        for file in &files {
            if !file.value_type.is_program() {
                // Non-program files don't need traversal
                continue;
            }

            let traverser = Traverser::new(
                &index.interfaces,
                &index.interner,
                &file.patterns,
                &all_globals,
                next_type_var,
                symbols,
            );
            let result = traverser.traverse(&file.nodes);

            // Chain shared state
            symbols = result.symbols;
            next_type_var = result.next_type_var;
            all_constraints.extend(result.constraints);
            all_return_origins.extend(result.return_origins);
            merged_substitution.extend(result.substitution);

            // Store per-file results
            per_file_analysis.insert(
                file.key.clone(),
                PerFileAnalysis {
                    diagnostics: result.diagnostics,
                    node_stacks: result.node_stacks,
                },
            );
        }

        // =======================================================================
        // Phase 4: Resolve ALL constraints together
        // =======================================================================
        let _resolution_diagnostics =
            resolve_constraints(all_constraints, &mut symbols, &mut merged_substitution);

        // TODO: Attribute resolution diagnostics to correct files based on spans

        // =======================================================================
        // Phase 5: Finalize signatures
        // =======================================================================
        finalize_signatures(&mut symbols, &merged_substitution, &all_return_origins);

        // =======================================================================
        // Phase 6: Build IndexEntries
        // =======================================================================
        for file in files {
            let signature = if file.value_type.is_program() {
                symbols
                    .find_definitions_by_name(&file.key)
                    .next()
                    .and_then(|def| def.signature.clone())
            } else {
                None
            };

            let analysis = per_file_analysis.remove(&file.key).map(|pfa| {
                AnalysisResult {
                    symbols: SymbolTable::new(), // Per-file symbols not tracked separately
                    scopes: rpl::analysis::ScopeTree::new(),
                    diagnostics: pfa.diagnostics,
                    node_stacks: pfa.node_stacks,
                }
            });

            index.entries.insert(
                file.key.clone(),
                IndexEntry {
                    key: file.key,
                    source_path: file.source_path,
                    source: file.source,
                    ast: file.nodes,
                    value_type: file.value_type,
                    signature,
                    analysis,
                },
            );
        }

        Ok(index)
    }

    /// Get an entry by key.
    pub fn get(&self, key: &str) -> Option<&IndexEntry> {
        self.entries.get(key)
    }

    /// Get the signature for a project entry.
    pub fn get_signature(&self, key: &str) -> Option<&Signature> {
        self.entries.get(key).and_then(|e| e.signature.as_ref())
    }

    /// Build an analysis context from this project index.
    ///
    /// The context contains all project entries as known external names,
    /// allowing the analyzer to avoid "undefined variable" errors for
    /// references to other project files.
    pub fn to_context(&self) -> rpl::analysis::Context {
        use rpl::analysis::Context;

        let mut context = Context::new();

        for (key, entry) in &self.entries {
            match entry.value_type {
                ValueType::Program => {
                    context.add_program(key.clone(), entry.signature.clone());
                }
                _ => {
                    context.add_value(key.clone());
                }
            }
        }

        context
    }
}

/// Extract parameter info from a program body.
fn extract_program_params(
    body_branches: &[rpl::ir::Branch],
    interfaces: &InterfaceRegistry,
) -> (usize, Vec<ParamInfo>) {
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
    let binding_indices = interfaces
        .get(lib)
        .map(|i| i.binding_branches(construct_id, inner_branches.len()))
        .unwrap_or_default();
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

    #[test]
    fn value_type_from_program() {
        let mut interner = Interner::new();
        let mut interfaces = InterfaceRegistry::new();
        rpl_stdlib::register_interfaces(&mut interfaces);

        let nodes = rpl::parse::parse("<< DUP * >>", &interfaces, &mut interner).unwrap();
        assert_eq!(ValueType::from_node(&nodes[0]), ValueType::Program);
    }

    #[test]
    fn value_type_from_list() {
        let mut interner = Interner::new();
        let mut interfaces = InterfaceRegistry::new();
        rpl_stdlib::register_interfaces(&mut interfaces);

        let nodes = rpl::parse::parse("{ 1 2 3 }", &interfaces, &mut interner).unwrap();
        assert_eq!(ValueType::from_node(&nodes[0]), ValueType::List);
    }

    #[test]
    fn value_type_from_integer() {
        let mut interner = Interner::new();
        let mut interfaces = InterfaceRegistry::new();
        rpl_stdlib::register_interfaces(&mut interfaces);

        let nodes = rpl::parse::parse("42", &interfaces, &mut interner).unwrap();
        assert_eq!(ValueType::from_node(&nodes[0]), ValueType::Integer);
    }
}
