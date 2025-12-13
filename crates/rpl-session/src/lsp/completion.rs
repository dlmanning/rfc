use rpl_lang::analysis::{AnalysisResult, ResolvedToken, SymbolTable};
use rpl_core::{Interner, Pos};
use rpl_lang::library::LibraryRegistry;
use rpl_source::SourceFile;

/// Kind of completion item.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CompletionItemKind {
    Variable,
    Command,
    Keyword,
    Constant,
    Operator,
}

/// A completion suggestion.
#[derive(Clone, Debug)]
pub struct CompletionItem {
    /// Display label.
    pub label: String,
    /// Kind of item.
    pub kind: CompletionItemKind,
    /// Short detail string.
    pub detail: Option<String>,
    /// Full documentation.
    pub documentation: Option<String>,
    /// Text to insert (if different from label).
    pub insert_text: Option<String>,
    /// Sort priority (lower = higher priority).
    pub sort_priority: u32,
}

impl CompletionItem {
    /// Create a new completion item.
    pub fn new(label: impl Into<String>, kind: CompletionItemKind) -> Self {
        Self {
            label: label.into(),
            kind,
            detail: None,
            documentation: None,
            insert_text: None,
            sort_priority: 100,
        }
    }

    /// Set the detail.
    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }

    /// Set the documentation.
    pub fn with_documentation(mut self, doc: impl Into<String>) -> Self {
        self.documentation = Some(doc.into());
        self
    }

    /// Set the insert text.
    pub fn with_insert_text(mut self, text: impl Into<String>) -> Self {
        self.insert_text = Some(text.into());
        self
    }

    /// Set the sort priority.
    pub fn with_priority(mut self, priority: u32) -> Self {
        self.sort_priority = priority;
        self
    }
}

/// Get completions at a position.
pub fn complete(
    _analysis: &AnalysisResult,
    source: &SourceFile,
    _registry: &LibraryRegistry,
    symbols: &SymbolTable,
    interner: &Interner,
    pos: Pos,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Find the prefix at the cursor position
    let prefix = find_prefix(source, pos);

    // Add visible variables from the symbol table
    add_variables(&mut items, symbols, interner, &prefix);

    // Add commands from libraries
    add_library_commands(&mut items, &prefix);

    // Sort by priority, then alphabetically
    items.sort_by(|a, b| {
        a.sort_priority
            .cmp(&b.sort_priority)
            .then_with(|| a.label.cmp(&b.label))
    });

    items
}

/// Find the prefix being typed at the cursor position.
fn find_prefix(source: &SourceFile, pos: Pos) -> String {
    let text = source.source();
    let offset = pos.offset() as usize;

    if offset == 0 || offset > text.len() {
        return String::new();
    }

    // Scan backwards to find the start of the current word
    let bytes = text.as_bytes();
    let mut start = offset;

    while start > 0 {
        let ch = bytes[start - 1];
        if ch.is_ascii_whitespace() || ch == b'(' || ch == b')' || ch == b'[' || ch == b']' {
            break;
        }
        start -= 1;
    }

    text[start..offset].to_string()
}

/// Add visible variables to the completion list.
fn add_variables(
    items: &mut Vec<CompletionItem>,
    symbols: &SymbolTable,
    interner: &Interner,
    prefix: &str,
) {
    // Get all definitions from the symbol table
    for def in symbols.definitions() {
        let name = interner.resolve(def.name);
        if prefix.is_empty()
            || name
                .to_ascii_uppercase()
                .starts_with(&prefix.to_ascii_uppercase())
        {
            let item = CompletionItem::new(name, CompletionItemKind::Variable)
                .with_detail("variable")
                .with_priority(10); // Variables have high priority
            items.push(item);
        }
    }
}

/// Add commands from all libraries.
fn add_library_commands(items: &mut Vec<CompletionItem>, prefix: &str) {
    let prefix_upper = prefix.to_ascii_uppercase();

    // Add built-in stack commands
    let stack_commands = [
        ("DUP", "( a -- a a )", "Duplicate top of stack"),
        ("DROP", "( a -- )", "Remove top of stack"),
        ("SWAP", "( a b -- b a )", "Swap top two items"),
        ("OVER", "( a b -- a b a )", "Copy second item to top"),
        ("ROT", "( a b c -- b c a )", "Rotate top three items"),
        ("DEPTH", "( -- n )", "Push stack depth"),
        ("CLEAR", "( ... -- )", "Clear the stack"),
    ];

    for (name, stack, brief) in stack_commands {
        if prefix.is_empty() || name.starts_with(&prefix_upper) {
            let item = CompletionItem::new(name, CompletionItemKind::Command)
                .with_detail(stack)
                .with_documentation(brief)
                .with_priority(50);
            items.push(item);
        }
    }

    // Add arithmetic operators
    let operators = [
        ("+", "( a b -- a+b )", "Add two numbers"),
        ("-", "( a b -- a-b )", "Subtract two numbers"),
        ("*", "( a b -- a*b )", "Multiply two numbers"),
        ("/", "( a b -- a/b )", "Divide two numbers"),
        ("NEG", "( a -- -a )", "Negate a number"),
        ("ABS", "( a -- |a| )", "Absolute value"),
    ];

    for (name, stack, brief) in operators {
        let name_upper = name.to_ascii_uppercase();
        if prefix.is_empty() || name_upper.starts_with(&prefix_upper) {
            let item = CompletionItem::new(name, CompletionItemKind::Operator)
                .with_detail(stack)
                .with_documentation(brief)
                .with_priority(50);
            items.push(item);
        }
    }

    // Add keywords
    let keywords = [
        ("IF", "Conditional"),
        ("THEN", "End IF condition"),
        ("ELSE", "Alternative branch"),
        ("END", "End block"),
        ("FOR", "Counted loop"),
        ("NEXT", "End FOR loop"),
        ("WHILE", "Conditional loop"),
        ("REPEAT", "Loop body"),
        ("STO", "Store to variable"),
        ("RCL", "Recall variable"),
    ];

    for (name, brief) in keywords {
        if prefix.is_empty() || name.starts_with(&prefix_upper) {
            let item = CompletionItem::new(name, CompletionItemKind::Keyword)
                .with_documentation(brief)
                .with_priority(60);
            items.push(item);
        }
    }
}

/// Find the token at a given position.
pub fn token_at_pos(analysis: &AnalysisResult, pos: Pos) -> Option<&ResolvedToken> {
    analysis.tokens.iter().find(|t| t.span.contains(pos))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::Pos;
    use rpl_lang::library::LibraryRegistry;
    use rpl_stdlib::register_standard_libs;
    use rpl_source::SourceId;

    fn make_source(text: &str) -> SourceFile {
        SourceFile::new(SourceId::new(0), "test".into(), text.into())
    }

    fn make_analysis() -> AnalysisResult {
        AnalysisResult {
            source_id: SourceId::new(0),
            version: 0,
            tokens: vec![],
            lines: vec![],
            diagnostics: vec![],
        }
    }

    #[test]
    fn completion_item_new() {
        let item = CompletionItem::new("DUP", CompletionItemKind::Command);
        assert_eq!(item.label, "DUP");
        assert_eq!(item.kind, CompletionItemKind::Command);
        assert!(item.detail.is_none());
    }

    #[test]
    fn completion_item_builder() {
        let item = CompletionItem::new("DUP", CompletionItemKind::Command)
            .with_detail("( a -- a a )")
            .with_documentation("Duplicate top of stack")
            .with_priority(10);

        assert_eq!(item.detail, Some("( a -- a a )".to_string()));
        assert_eq!(
            item.documentation,
            Some("Duplicate top of stack".to_string())
        );
        assert_eq!(item.sort_priority, 10);
    }

    #[test]
    fn find_prefix_at_start() {
        let source = make_source("DUP DROP");
        let prefix = find_prefix(&source, Pos::new(0));
        assert_eq!(prefix, "");
    }

    #[test]
    fn find_prefix_mid_word() {
        let source = make_source("DUP DROP");
        let prefix = find_prefix(&source, Pos::new(2));
        assert_eq!(prefix, "DU");
    }

    #[test]
    fn find_prefix_after_space() {
        let source = make_source("DUP DROP");
        let prefix = find_prefix(&source, Pos::new(4));
        assert_eq!(prefix, "");
    }

    #[test]
    fn find_prefix_second_word() {
        let source = make_source("DUP DROP");
        let prefix = find_prefix(&source, Pos::new(6));
        assert_eq!(prefix, "DR");
    }

    #[test]
    fn complete_empty_returns_all() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("");
        let analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

        let items = complete(
            &analysis,
            &source,
            &registry,
            &symbols,
            &interner,
            Pos::new(0),
        );

        // Should have at least the built-in commands
        assert!(!items.is_empty());
        assert!(items.iter().any(|i| i.label == "DUP"));
        assert!(items.iter().any(|i| i.label == "+"));
    }

    #[test]
    fn complete_filters_by_prefix() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("D");
        let analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

        let items = complete(
            &analysis,
            &source,
            &registry,
            &symbols,
            &interner,
            Pos::new(1),
        );

        // Should have DUP, DROP, DEPTH but not SWAP
        assert!(items.iter().any(|i| i.label == "DUP"));
        assert!(items.iter().any(|i| i.label == "DROP"));
        assert!(items.iter().any(|i| i.label == "DEPTH"));
        assert!(!items.iter().any(|i| i.label == "SWAP"));
    }

    #[test]
    fn complete_case_insensitive() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        let source = make_source("du");
        let analysis = make_analysis();
        let symbols = SymbolTable::new();
        let interner = Interner::new();

        let items = complete(
            &analysis,
            &source,
            &registry,
            &symbols,
            &interner,
            Pos::new(2),
        );

        // Should match DUP even with lowercase prefix
        assert!(items.iter().any(|i| i.label == "DUP"));
    }
}
