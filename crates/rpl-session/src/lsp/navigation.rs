use rpl_lang::analysis::{AnalysisResult, DefinitionKind, SymbolTable};
use rpl_core::{Interner, Pos, Span};
use rpl_source::SourceFile;

use super::completion::token_at_pos;

/// Result of a goto definition request.
#[derive(Clone, Debug)]
pub struct GotoResult {
    /// The span of the definition.
    pub span: Span,
}

impl GotoResult {
    /// Create a new goto result.
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

/// Result of a find references request.
#[derive(Clone, Debug)]
pub struct ReferenceResult {
    /// All reference locations.
    pub locations: Vec<Span>,
    /// Include the definition in results.
    pub include_definition: bool,
}

impl ReferenceResult {
    /// Create a new reference result.
    pub fn new(locations: Vec<Span>) -> Self {
        Self {
            locations,
            include_definition: false,
        }
    }

    /// Include the definition in results.
    pub fn with_definition(mut self) -> Self {
        self.include_definition = true;
        self
    }
}

/// Go to the definition of a symbol.
///
/// Note: Currently this is a stub. The actual implementation would need to:
/// 1. Look up the token's reference ID in the ResolvedToken
/// 2. Find the corresponding reference in the SymbolTable
/// 3. Follow the reference to its definition
///
/// The challenge is that ResolvedToken uses analysis::token::ReferenceId
/// while SymbolTable uses analysis::symbols::ReferenceId - these are
/// different types that would need to be unified or bridged.
pub fn goto_definition(
    analysis: &AnalysisResult,
    symbols: &SymbolTable,
    _source: &SourceFile,
    _interner: &Interner,
    pos: Pos,
) -> Option<GotoResult> {
    let token = token_at_pos(analysis, pos)?;

    // If this token has a defines field, it IS a definition
    // Look up the definition in the symbol table by matching spans
    if token.defines.is_some() {
        // This token is itself a definition, return its span
        return Some(GotoResult::new(token.span));
    }

    // If this token has a reference, try to find its target definition
    // by scanning symbol table for a reference at this span
    if token.references.is_some() {
        // Look through all references in the symbol table to find one at this span
        for reference in symbols.references() {
            if reference.span == token.span {
                // Found the reference, now get its definition
                if let Some(def_id) = reference.definition
                    && let Some(def) = symbols.get_definition(def_id)
                {
                    return Some(GotoResult::new(def.span));
                }
            }
        }
    }

    None
}

/// Symbol kind for document symbols.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DocumentSymbolKind {
    /// A global variable (may contain a program).
    Variable,
    /// A function (program stored in a variable).
    Function,
    /// A local variable.
    Local,
    /// A loop variable.
    LoopVariable,
}

/// A document symbol (for outline view).
#[derive(Clone, Debug)]
pub struct DocumentSymbol {
    /// The symbol name.
    pub name: String,
    /// The kind of symbol.
    pub kind: DocumentSymbolKind,
    /// The full range of the symbol (including the value).
    pub range: Span,
    /// The range of the symbol name.
    pub selection_range: Span,
    /// Optional detail (e.g., type info).
    pub detail: Option<String>,
}

impl DocumentSymbol {
    /// Create a new document symbol.
    pub fn new(name: String, kind: DocumentSymbolKind, range: Span, selection_range: Span) -> Self {
        Self {
            name,
            kind,
            range,
            selection_range,
            detail: None,
        }
    }

    /// Add a detail string.
    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }
}

/// Get document symbols for outline view.
pub fn document_symbols(
    symbols: &SymbolTable,
    interner: &Interner,
) -> Vec<DocumentSymbol> {
    let mut result = Vec::new();

    for def in symbols.definitions() {
        let name = interner.resolve(def.name).to_string();

        let (kind, detail) = match def.kind {
            DefinitionKind::GlobalVariable => {
                // Check if it stores a program
                let detail = def.value_type.map(|ty| {
                    match ty {
                        rpl_core::TypeId::PROGRAM => "Program".to_string(),
                        rpl_core::TypeId::REAL => "Real".to_string(),
                        rpl_core::TypeId::BINT => "Integer".to_string(),
                        rpl_core::TypeId::STRING => "String".to_string(),
                        rpl_core::TypeId::LIST => "List".to_string(),
                        _ => format!("{:?}", ty),
                    }
                });

                let kind = if def.value_type == Some(rpl_core::TypeId::PROGRAM) {
                    DocumentSymbolKind::Function
                } else {
                    DocumentSymbolKind::Variable
                };

                (kind, detail)
            }
            DefinitionKind::LocalVariable => (DocumentSymbolKind::Local, Some("local".to_string())),
            DefinitionKind::Parameter => (DocumentSymbolKind::Local, Some("parameter".to_string())),
            DefinitionKind::LoopVariable => (DocumentSymbolKind::LoopVariable, Some("loop".to_string())),
        };

        let mut symbol = DocumentSymbol::new(name, kind, def.span, def.span);
        if let Some(d) = detail {
            symbol = symbol.with_detail(d);
        }
        result.push(symbol);
    }

    result
}

/// Find all references to a symbol.
pub fn find_references(
    analysis: &AnalysisResult,
    symbols: &SymbolTable,
    _source: &SourceFile,
    _interner: &Interner,
    pos: Pos,
    include_definition: bool,
) -> Option<ReferenceResult> {
    let token = token_at_pos(analysis, pos)?;
    let mut locations = Vec::new();

    // First, determine which definition we're looking for
    let target_def_id = if token.defines.is_some() {
        // This token is a definition - find it in symbol table by span
        symbols
            .definitions()
            .find(|d| d.span == token.span)
            .map(|d| d.id)?
    } else if token.references.is_some() {
        // This token is a reference - find it in symbol table by span
        let reference = symbols.references().find(|r| r.span == token.span)?;
        reference.definition?
    } else {
        return None;
    };

    // Get the definition
    let def = symbols.get_definition(target_def_id)?;

    // Optionally include the definition
    if include_definition {
        locations.push(def.span);
    }

    // Find all references to this definition
    for reference in symbols.references_to(target_def_id) {
        locations.push(reference.span);
    }

    if locations.is_empty() {
        None
    } else {
        let mut result = ReferenceResult::new(locations);
        if include_definition {
            result = result.with_definition();
        }
        Some(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Pos, Span};

    #[test]
    fn goto_result_new() {
        let span = Span::new(Pos::new(0), Pos::new(5));
        let result = GotoResult::new(span);
        assert_eq!(result.span.start(), Pos::new(0));
        assert_eq!(result.span.end(), Pos::new(5));
    }

    #[test]
    fn reference_result_new() {
        let locations = vec![
            Span::new(Pos::new(0), Pos::new(5)),
            Span::new(Pos::new(10), Pos::new(15)),
        ];
        let result = ReferenceResult::new(locations);
        assert_eq!(result.locations.len(), 2);
        assert!(!result.include_definition);
    }

    #[test]
    fn reference_result_with_definition() {
        let locations = vec![Span::new(Pos::new(0), Pos::new(5))];
        let result = ReferenceResult::new(locations).with_definition();
        assert!(result.include_definition);
    }

    #[test]
    fn symbol_table_basic() {
        use rpl_lang::analysis::{Definition, DefinitionKind, Reference, ReferenceKind, ScopeId};
        use rpl_core::Symbol;

        let mut symbols = SymbolTable::new();
        let name = Symbol::from_raw(1);
        let scope = ScopeId::new(0);
        let def_span = Span::new(Pos::new(0), Pos::new(2));
        let ref_span = Span::new(Pos::new(10), Pos::new(12));

        // Add a definition
        let def = Definition::new(
            rpl_lang::analysis::DefinitionId::new(0),
            name,
            def_span,
            DefinitionKind::GlobalVariable,
            scope,
        );
        let def_id = symbols.add_definition(def);

        // Add a reference
        let reference = Reference::new(
            rpl_lang::analysis::ReferenceId::new(0),
            name,
            ref_span,
            ReferenceKind::Read,
            scope,
        );
        let ref_id = symbols.add_reference(reference);

        // Resolve the reference
        symbols.resolve_reference(ref_id, def_id);

        // Verify we can find references to the definition
        let refs: Vec<_> = symbols.references_to(def_id).collect();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].span, ref_span);
    }
}
