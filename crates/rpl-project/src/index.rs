//! Project index for cross-file analysis.
//!
//! Provides the `ProjectIndex` which holds analyzed information about all files
//! in a project, enabling cross-file type checking and LSP features.

use crate::error::LoadError;
use crate::loader;
use crate::manifest::Manifest;
use rpl::analysis::{
    AnalysisResult, Context, GlobalMap, GlobalVariableMap, ParamInfo, Pattern,
};
use rpl::core::Interner;
use rpl::ir::{AtomKind, CompositeKind, Node, NodeKind};
use rpl::registry::InterfaceRegistry;
use rpl::types::Signature;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

/// Compute a hash of content for cache invalidation.
fn hash_content(content: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

/// Information about a single project file.
#[derive(Debug)]
pub struct IndexEntry {
    /// The key/path in the project (e.g., "lib/square").
    pub key: String,

    /// Path to the source file.
    pub source_path: PathBuf,

    /// The source code.
    pub source: String,

    /// Hash of source content for cache invalidation.
    pub source_hash: u64,

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
    Bytes,
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

    /// Global function definitions collected from all files.
    ///
    /// These are functions defined via `<< >> 'name' STO` across the project.
    globals: rpl::analysis::GlobalMap,

    /// Global variables collected from all files.
    ///
    /// These are variables defined via STO across the project.
    /// The key is the fully-qualified path (e.g., "lib/data/ship_id").
    global_variables: GlobalVariableMap,

    /// Interface registry for parsing.
    interfaces: InterfaceRegistry,

    /// Interner for symbols.
    interner: Interner,

    /// Shared symbol table with all definitions across the project.
    symbols: rpl::analysis::SymbolTable,
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
            globals: GlobalMap::new(),
            global_variables: GlobalVariableMap::new(),
            interfaces,
            interner: Interner::new(),
            symbols: rpl::analysis::SymbolTable::new(),
        }
    }

    /// Get mutable access to the interface registry.
    ///
    /// Use this to register additional interfaces before calling `build`.
    pub fn interfaces_mut(&mut self) -> &mut InterfaceRegistry {
        &mut self.interfaces
    }

    /// Build a project index by analyzing all files.
    ///
    /// Uses global constraint resolution: all files are analyzed together so that
    /// type constraints from call sites properly narrow parameter types.
    pub fn build(project_dir: &Path) -> Result<Self, LoadError> {
        Self::build_with(project_dir, |_| {})
    }

    /// Build a project index with additional interface registration.
    ///
    /// The callback is invoked with a mutable reference to the interface registry
    /// before parsing begins, allowing registration of additional interfaces.
    pub fn build_with<F>(project_dir: &Path, register_interfaces: F) -> Result<Self, LoadError>
    where
        F: FnOnce(&mut InterfaceRegistry),
    {
        use rpl::analysis::{
            collect_global_defines, collect_global_variables, collect_globals,
            finalize_signatures, recognize_patterns, resolve_constraints, Constraint, GlobalMap,
            PatternMap, Substitution, SymbolTable, Traverser,
        };

        // Load manifest
        let manifest_path = project_dir.join("project.toml");
        let manifest = Manifest::from_file(&manifest_path)?;

        let mut index = Self::new(project_dir.to_owned(), manifest.clone());
        register_interfaces(&mut index.interfaces);

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
            global_defines: Vec<Pattern>,
            value_type: ValueType,
        }

        let mut files: Vec<FileData> = Vec::new();

        for file_path in &file_paths {
            let key = loader::path_to_key(project_dir, file_path)?;

            // Handle binary files (non-.rpl) separately
            let is_rpl = file_path
                .extension()
                .map(|e| e == "rpl")
                .unwrap_or(false);

            if !is_rpl {
                // Binary file - add to index as Bytes type without parsing
                index.entries.insert(
                    key.clone(),
                    IndexEntry {
                        key,
                        source_path: file_path.clone(),
                        source: String::new(),
                        source_hash: 0, // Binary files don't have source
                        ast: Vec::new(),
                        value_type: ValueType::Bytes,
                        signature: None,
                        analysis: None,
                    },
                );
                continue;
            }

            let source = std::fs::read_to_string(file_path).map_err(|e| LoadError::Io {
                path: file_path.clone(),
                source: e,
            })?;

            // Parse
            let nodes = rpl::parse::parse(&source, &index.interfaces, &mut index.interner)
                .map_err(|e| LoadError::Eval {
                    path: file_path.clone(),
                    source: e.into(),
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

            // Collect global variable definitions (STO patterns)
            let global_defines = collect_global_defines(&nodes, &index.interfaces);

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
                global_defines,
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
        // Phase 2.5: Collect global variables from ALL files
        // =======================================================================
        // This creates definitions for all global variables defined via STO,
        // enabling cross-file variable references.
        let mut all_global_vars: GlobalVariableMap = HashMap::new();
        let mut var_constraints: Vec<Constraint> = Vec::new();

        for file in &files {
            let (file_vars, next_tv) = collect_global_variables(
                &file.global_defines,
                &mut symbols,
                &file.key,
                next_type_var,
            );
            next_type_var = next_tv;

            // Merge variables, linking types if already exists
            for (key, info) in file_vars {
                if let Some(existing) = all_global_vars.get(&key) {
                    // Variable already defined - link types with Equal constraint
                    var_constraints.push(Constraint::Equal {
                        def_a: existing.def_id,
                        def_b: info.def_id,
                        span: info.name_span,
                    });
                } else {
                    all_global_vars.insert(key, info);
                }
            }
        }

        // Store in the index
        index.globals = all_globals.clone();
        index.global_variables = all_global_vars.clone();

        // =======================================================================
        // Phase 3: Traverse all files, accumulating constraints
        // =======================================================================
        // Each file is traversed with the shared GlobalMap. Constraints from all
        // files are collected together for global resolution.
        let mut all_constraints: Vec<Constraint> = var_constraints;
        let mut all_return_origins: HashMap<String, rpl::analysis::Origin> = HashMap::new();
        let mut merged_substitution = Substitution::new();

        // Build context from all entries for traversal
        // This allows the traverser to recognize project entries (like sprites.player)
        // Note: Must include entries from index.entries (which includes binary files)
        // as well as files (which are .rpl files being processed)
        let mut traversal_context = Context::new();
        // Add binary files and other early entries from index.entries
        for (key, entry) in &index.entries {
            match entry.value_type {
                ValueType::Program => {
                    traversal_context.add_program(key.clone(), None);
                }
                _ => {
                    traversal_context.add_value(key.clone());
                }
            }
        }
        // Add .rpl files from files vec
        for file in &files {
            match file.value_type {
                ValueType::Program => {
                    traversal_context.add_program(file.key.clone(), None);
                }
                _ => {
                    traversal_context.add_value(file.key.clone());
                }
            }
        }

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
                &all_global_vars,
                &traversal_context,
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

        // Resolve TypeVars in definition value_types after constraint resolution.
        // This ensures parameter types are properly resolved.
        let def_ids: Vec<_> = symbols.definitions().map(|d| d.id).collect();
        for def_id in def_ids {
            if let Some(def) = symbols.get_definition_mut(def_id)
                && let Some(ref ty) = def.value_type
                && ty.is_type_var()
            {
                let resolved = merged_substitution.apply(ty);
                def.value_type = Some(resolved);
            }
        }

        // =======================================================================
        // Phase 5: Finalize signatures
        // =======================================================================
        finalize_signatures(&mut symbols, &merged_substitution, &all_return_origins);

        // Update globals with resolved signatures from symbols
        // This ensures the GlobalMap reflects the resolved types after constraint resolution
        for (name, info) in &mut index.globals {
            if let Some(def) = symbols.find_definitions_by_name(name).next() {
                if let Some(sig) = def.signature.clone() {
                    info.signature = sig;
                }
            }
        }

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

            let source_hash = hash_content(&file.source);
            index.entries.insert(
                file.key.clone(),
                IndexEntry {
                    key: file.key,
                    source_path: file.source_path,
                    source: file.source,
                    source_hash,
                    ast: file.nodes,
                    value_type: file.value_type,
                    signature,
                    analysis,
                },
            );
        }

        // Store the shared symbol table
        index.symbols = symbols;

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

        // Add project entries (files)
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

        // Add global function definitions (from << >> 'name' STO patterns)
        for (name, info) in &self.globals {
            context.add_program(name.clone(), Some(info.signature.clone()));
        }

        // Add global variables defined via STO across the project
        for (key, _info) in &self.global_variables {
            // Use the full key (path/to/var) and also just the variable name
            context.add_value(key.clone());

            // Also add just the variable name for simple references
            if let Some(name) = key.rsplit('/').next() {
                if name != key {
                    context.add_value(name.to_string());
                }
            }
        }

        context
    }

    /// Get access to global variables.
    pub fn global_variables(&self) -> &GlobalVariableMap {
        &self.global_variables
    }

    /// Get access to global function definitions.
    pub fn globals(&self) -> &GlobalMap {
        &self.globals
    }

    /// Get access to the shared symbol table.
    ///
    /// This contains all definitions across all project files.
    pub fn symbols(&self) -> &rpl::analysis::SymbolTable {
        &self.symbols
    }

    /// Get the interface registry.
    pub fn interfaces(&self) -> &InterfaceRegistry {
        &self.interfaces
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Get the entry key for a file path.
    ///
    /// Returns None if the path is not within this project.
    pub fn key_for_path(&self, file_path: &Path) -> Option<String> {
        loader::path_to_key(&self.root, file_path).ok()
    }

    /// Check if cached analysis is valid for the given content.
    ///
    /// Returns true if the content hash matches the cached entry.
    pub fn is_cache_valid(&self, key: &str, content: &str) -> bool {
        self.entries
            .get(key)
            .is_some_and(|entry| entry.source_hash == hash_content(content))
    }

    /// Get cached analysis for a file if content matches.
    ///
    /// Returns None if content has changed (needs re-analysis).
    pub fn get_cached_analysis(&self, key: &str, content: &str) -> Option<&AnalysisResult> {
        let entry = self.entries.get(key)?;
        if entry.source_hash == hash_content(content) {
            entry.analysis.as_ref()
        } else {
            None
        }
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
