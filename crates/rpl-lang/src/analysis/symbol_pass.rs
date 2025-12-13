use rpl_core::{Interner, Span};
use rpl_source::SourceFile;

use super::patterns::{PatternKind, PatternMatch, PatternRegistry};
use super::scopes::{ScopeId, ScopeTree};
use super::symbols::{
    Definition, DefinitionId, DefinitionKind, Reference, ReferenceId, ReferenceKind, SymbolTable,
};
use super::token::ResolvedToken;

/// Run the symbol detection pass on tokens.
///
/// This scans tokens for symbol patterns (like `'name' STO`), records definitions
/// and references in the symbol table, and links tokens to their definitions/references.
pub fn run_symbol_pass(
    tokens: &mut [ResolvedToken],
    scopes: &ScopeTree,
    patterns: &PatternRegistry,
    source: &SourceFile,
    interner: &mut Interner,
) -> SymbolTable {
    let mut symbols = SymbolTable::new();
    let mut pos = 0;

    while pos < tokens.len() {
        // Try to match a pattern at this position
        if let Some(matched) = patterns.try_match(tokens, pos, source, interner) {
            process_match(&matched, tokens, scopes, &mut symbols);
            // Skip the tokens covered by this pattern
            pos = matched.token_range.1;
        } else {
            pos += 1;
        }
    }

    // Resolve references to definitions
    resolve_references(&mut symbols, scopes);

    symbols
}

/// Process a pattern match, creating definitions or references.
fn process_match(
    matched: &PatternMatch,
    tokens: &mut [ResolvedToken],
    scopes: &ScopeTree,
    symbols: &mut SymbolTable,
) {
    // Determine the scope at the pattern's location
    let scope_id = scopes.scope_at(matched.span.start());

    match matched.kind {
        PatternKind::GlobalDefine => {
            // Create a definition for each name in the pattern
            for name_match in &matched.names {
                let mut def = Definition::new(
                    DefinitionId::new(0), // Will be assigned by add_definition
                    name_match.name,
                    name_match.span,
                    DefinitionKind::GlobalVariable,
                    scope_id,
                );
                // Set the inferred value type if available
                def.value_type = matched.value_type;
                let def_id = symbols.add_definition(def);

                // Link the token to this definition
                link_token_to_definition(tokens, name_match.span, def_id);
            }
        }
        PatternKind::GlobalReference => {
            for name_match in &matched.names {
                let reference = Reference::new(
                    ReferenceId::new(0),
                    name_match.name,
                    name_match.span,
                    ReferenceKind::Read,
                    scope_id,
                );
                let ref_id = symbols.add_reference(reference);
                link_token_to_reference(tokens, name_match.span, ref_id);
            }
        }
        PatternKind::GlobalModify => {
            for name_match in &matched.names {
                let reference = Reference::new(
                    ReferenceId::new(0),
                    name_match.name,
                    name_match.span,
                    ReferenceKind::Modify,
                    scope_id,
                );
                let ref_id = symbols.add_reference(reference);
                link_token_to_reference(tokens, name_match.span, ref_id);
            }
        }
        PatternKind::GlobalDelete => {
            for name_match in &matched.names {
                let reference = Reference::new(
                    ReferenceId::new(0),
                    name_match.name,
                    name_match.span,
                    ReferenceKind::Delete,
                    scope_id,
                );
                let ref_id = symbols.add_reference(reference);
                link_token_to_reference(tokens, name_match.span, ref_id);
            }
        }
        PatternKind::LocalDefine => {
            for name_match in &matched.names {
                let def = Definition::new(
                    DefinitionId::new(0),
                    name_match.name,
                    name_match.span,
                    DefinitionKind::LocalVariable,
                    scope_id,
                );
                let def_id = symbols.add_definition(def);
                link_token_to_definition(tokens, name_match.span, def_id);
            }
        }
        PatternKind::LoopVariable => {
            for name_match in &matched.names {
                let def = Definition::new(
                    DefinitionId::new(0),
                    name_match.name,
                    name_match.span,
                    DefinitionKind::LoopVariable,
                    scope_id,
                );
                let def_id = symbols.add_definition(def);
                link_token_to_definition(tokens, name_match.span, def_id);
            }
        }
    }
}

/// Link a token at the given span to a definition.
fn link_token_to_definition(tokens: &mut [ResolvedToken], span: Span, def_id: DefinitionId) {
    for token in tokens.iter_mut() {
        if token.span == span {
            token.defines = Some(super::token::DefinitionId(def_id.as_u32()));
            break;
        }
    }
}

/// Link a token at the given span to a reference.
fn link_token_to_reference(tokens: &mut [ResolvedToken], span: Span, ref_id: ReferenceId) {
    for token in tokens.iter_mut() {
        if token.span == span {
            token.references = Some(super::token::ReferenceId(ref_id.as_u32()));
            break;
        }
    }
}

/// Resolve references to their definitions by walking the scope chain.
pub fn resolve_references(symbols: &mut SymbolTable, scopes: &ScopeTree) {
    // Collect reference info first to avoid borrow issues
    let ref_info: Vec<_> = symbols
        .references()
        .filter(|r| !r.is_resolved())
        .map(|r| (r.id, r.name, r.scope))
        .collect();

    for (ref_id, name, scope_id) in ref_info {
        // Walk scope chain looking for a definition
        if let Some(def_id) = find_definition_in_scope_chain(symbols, scopes, name, scope_id) {
            symbols.resolve_reference(ref_id, def_id);
        }
    }
}

/// Find a definition by walking up the scope chain.
fn find_definition_in_scope_chain(
    symbols: &SymbolTable,
    scopes: &ScopeTree,
    name: rpl_core::Symbol,
    start_scope: ScopeId,
) -> Option<DefinitionId> {
    for scope_id in scopes.ancestors(start_scope) {
        if let Some(def) = symbols.find_definition(name, scope_id) {
            return Some(def.id);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::patterns::{NameMatch, PatternMatch, SymbolPattern};
    use crate::analysis::scopes::ScopeKind;
    use rpl_core::Pos;
    use crate::library::LibraryId;
    use rpl_source::SourceId;
    use rpl_core::token::{SemanticKind, TokenInfo};

    fn make_token(start: u32, end: u32) -> ResolvedToken {
        ResolvedToken::new(
            Span::new(Pos::new(start), Pos::new(end)),
            LibraryId::new(1),
            TokenInfo::atom(1),
            SemanticKind::Command,
        )
    }

    fn make_source(text: &str) -> SourceFile {
        SourceFile::new(SourceId::new(0), "test.rpl".to_string(), text.to_string())
    }

    // Mock detector for 'name STO pattern: looks for two tokens where
    // first is a quoted name and second is "STO"
    fn mock_sto_detector(
        tokens: &[ResolvedToken],
        pos: usize,
        _source: &SourceFile,
        _interner: &mut Interner,
    ) -> Option<PatternMatch> {
        if pos + 1 >= tokens.len() {
            return None;
        }

        // In a real implementation, we'd check token content
        // For testing, we'll use a simple heuristic based on semantic kind
        let name_token = &tokens[pos];
        let cmd_token = &tokens[pos + 1];

        // Check if the pattern looks like 'name STO
        if name_token.semantic == SemanticKind::Variable
            && cmd_token.semantic == SemanticKind::Command
        {
            let span = Span::new(name_token.span.start(), cmd_token.span.end());

            Some(PatternMatch::new(
                PatternKind::GlobalDefine,
                span,
                vec![NameMatch::new(
                    rpl_core::Symbol::from_raw(0),
                    name_token.span,
                )],
                (pos, pos + 2),
            ))
        } else {
            None
        }
    }

    // Mock detector for 'name RCL pattern
    fn mock_rcl_detector(
        tokens: &[ResolvedToken],
        pos: usize,
        _source: &SourceFile,
        _interner: &mut Interner,
    ) -> Option<PatternMatch> {
        if pos + 1 >= tokens.len() {
            return None;
        }

        let name_token = &tokens[pos];
        let cmd_token = &tokens[pos + 1];

        // Check if pattern looks like a reference (Variable + Keyword for RCL)
        if name_token.semantic == SemanticKind::Variable
            && cmd_token.semantic == SemanticKind::Keyword
        {
            let span = Span::new(name_token.span.start(), cmd_token.span.end());

            Some(PatternMatch::new(
                PatternKind::GlobalReference,
                span,
                vec![NameMatch::new(
                    rpl_core::Symbol::from_raw(0),
                    name_token.span,
                )],
                (pos, pos + 2),
            ))
        } else {
            None
        }
    }

    #[test]
    fn run_symbol_pass_empty() {
        let mut tokens: Vec<ResolvedToken> = vec![];
        let scopes = ScopeTree::new();
        let patterns = PatternRegistry::new();
        let source = make_source("");
        let mut interner = Interner::new();

        let symbols = run_symbol_pass(&mut tokens, &scopes, &patterns, &source, &mut interner);

        assert_eq!(symbols.definition_count(), 0);
        assert_eq!(symbols.reference_count(), 0);
    }

    #[test]
    fn run_symbol_pass_no_patterns() {
        let mut tokens = vec![make_token(0, 3), make_token(4, 7)];
        let scopes = ScopeTree::new();
        let patterns = PatternRegistry::new();
        let source = make_source("abc def");
        let mut interner = Interner::new();

        let symbols = run_symbol_pass(&mut tokens, &scopes, &patterns, &source, &mut interner);

        assert_eq!(symbols.definition_count(), 0);
        assert_eq!(symbols.reference_count(), 0);
    }

    #[test]
    fn run_symbol_pass_detects_definition() {
        // Create tokens that look like 'x STO
        let mut tokens = vec![
            {
                let mut t = make_token(0, 3);
                t.semantic = SemanticKind::Variable;
                t
            },
            {
                let mut t = make_token(4, 7);
                t.semantic = SemanticKind::Command;
                t
            },
        ];

        let scopes = ScopeTree::new();
        let mut patterns = PatternRegistry::new();
        patterns.register(SymbolPattern::new(
            PatternKind::GlobalDefine,
            mock_sto_detector,
        ));

        let source = make_source("'x' STO");
        let mut interner = Interner::new();

        let symbols = run_symbol_pass(&mut tokens, &scopes, &patterns, &source, &mut interner);

        assert_eq!(symbols.definition_count(), 1);
        assert_eq!(symbols.reference_count(), 0);

        // Check the first token is linked to a definition
        assert!(tokens[0].defines.is_some());
    }

    #[test]
    fn run_symbol_pass_detects_reference() {
        // Create tokens that look like 'x RCL
        let mut tokens = vec![
            {
                let mut t = make_token(0, 3);
                t.semantic = SemanticKind::Variable;
                t
            },
            {
                let mut t = make_token(4, 7);
                t.semantic = SemanticKind::Keyword;
                t
            },
        ];

        let scopes = ScopeTree::new();
        let mut patterns = PatternRegistry::new();
        patterns.register(SymbolPattern::new(
            PatternKind::GlobalReference,
            mock_rcl_detector,
        ));

        let source = make_source("'x' RCL");
        let mut interner = Interner::new();

        let symbols = run_symbol_pass(&mut tokens, &scopes, &patterns, &source, &mut interner);

        assert_eq!(symbols.definition_count(), 0);
        assert_eq!(symbols.reference_count(), 1);

        // Check the first token is linked to a reference
        assert!(tokens[0].references.is_some());
    }

    #[test]
    fn resolve_references_links_to_definition() {
        let mut interner = Interner::new();
        let sym_x = interner.intern("x");

        let mut symbols = SymbolTable::new();
        let scopes = ScopeTree::new();

        // Add a definition
        let def_id = symbols.add_definition(Definition::new(
            DefinitionId::new(0),
            sym_x,
            Span::new(Pos::new(0), Pos::new(3)),
            DefinitionKind::GlobalVariable,
            scopes.root(),
        ));

        // Add an unresolved reference in the same scope
        let ref_id = symbols.add_reference(Reference::new(
            ReferenceId::new(0),
            sym_x,
            Span::new(Pos::new(10), Pos::new(13)),
            ReferenceKind::Read,
            scopes.root(),
        ));

        // Resolve references
        resolve_references(&mut symbols, &scopes);

        // Check reference is now resolved
        let reference = symbols.get_reference(ref_id).unwrap();
        assert!(reference.is_resolved());
        assert_eq!(reference.definition, Some(def_id));
    }

    #[test]
    fn resolve_references_inner_scope_shadows_outer() {
        let mut interner = Interner::new();
        let sym_x = interner.intern("x");

        let mut symbols = SymbolTable::new();
        let mut scopes = ScopeTree::new();

        // Create nested scopes
        let outer_scope = scopes.root();
        let inner_scope = scopes.add_scope(
            outer_scope,
            ScopeKind::Program,
            Span::new(Pos::new(10), Pos::new(100)),
        );

        // Add definition in outer scope
        let _outer_def_id = symbols.add_definition(Definition::new(
            DefinitionId::new(0),
            sym_x,
            Span::new(Pos::new(0), Pos::new(3)),
            DefinitionKind::GlobalVariable,
            outer_scope,
        ));

        // Add definition in inner scope (shadows outer)
        let inner_def_id = symbols.add_definition(Definition::new(
            DefinitionId::new(0),
            sym_x,
            Span::new(Pos::new(20), Pos::new(23)),
            DefinitionKind::LocalVariable,
            inner_scope,
        ));

        // Add reference in inner scope
        let ref_id = symbols.add_reference(Reference::new(
            ReferenceId::new(0),
            sym_x,
            Span::new(Pos::new(50), Pos::new(53)),
            ReferenceKind::Read,
            inner_scope,
        ));

        // Resolve
        resolve_references(&mut symbols, &scopes);

        // Reference should resolve to inner definition (shadowing)
        let reference = symbols.get_reference(ref_id).unwrap();
        assert!(reference.is_resolved());
        assert_eq!(reference.definition, Some(inner_def_id));
    }

    #[test]
    fn resolve_references_unresolved_stays_none() {
        let mut interner = Interner::new();
        let sym_x = interner.intern("x");
        let sym_y = interner.intern("y");

        let mut symbols = SymbolTable::new();
        let scopes = ScopeTree::new();

        // Add definition for 'x'
        symbols.add_definition(Definition::new(
            DefinitionId::new(0),
            sym_x,
            Span::new(Pos::new(0), Pos::new(3)),
            DefinitionKind::GlobalVariable,
            scopes.root(),
        ));

        // Add reference for 'y' (no definition exists)
        let ref_id = symbols.add_reference(Reference::new(
            ReferenceId::new(0),
            sym_y,
            Span::new(Pos::new(10), Pos::new(13)),
            ReferenceKind::Read,
            scopes.root(),
        ));

        // Resolve
        resolve_references(&mut symbols, &scopes);

        // Reference should remain unresolved
        let reference = symbols.get_reference(ref_id).unwrap();
        assert!(!reference.is_resolved());
        assert_eq!(reference.definition, None);
    }
}
