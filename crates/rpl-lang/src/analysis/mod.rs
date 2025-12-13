mod cache;
mod incremental;
mod patterns;
mod resolver;
mod result;
mod scopes;
mod state;
mod symbol_pass;
mod symbols;
pub mod token;
mod tokenizer;

pub use cache::AnalysisCache;
pub use incremental::{TextEdit, apply_edit, find_stable_point};
pub use patterns::{
    NameMatch, PatternDetector, PatternKind, PatternMatch, PatternRegistry, SymbolPattern,
    default_pattern_registry, detect_purge_pattern, detect_rcl_pattern, detect_sto_pattern,
};
pub use resolver::resolve_context;
pub use result::AnalysisResult;
pub use scopes::{Scope, ScopeId, ScopeKind, ScopeTree, build_scopes};
pub use state::{LineState, ParseState};
pub use symbol_pass::{resolve_references, run_symbol_pass};
pub use symbols::{
    Definition, DefinitionId, DefinitionKind, Reference, ReferenceId, ReferenceKind, SymbolTable,
};
pub use token::{ResolvedToken, TokenContext};
pub use tokenizer::Tokenizer;
