//! Analysis and LSP tests.
//!
//! Tests for the static analysis and LSP features like
//! symbol tracking, hover, and semantic tokens.

use rpl::Session;

/// Helper to get analysis results for code
fn analyze(code: &str) -> rpl::analysis::AnalysisResult {
    let mut session = Session::new();
    let source_id = session.set_source("test.rpl", code);
    session.analyze(source_id).unwrap().clone()
}

/// Helper to get the substring from source at a span
fn span_text<'a>(source: &'a str, span: rpl::core::Span) -> &'a str {
    let start = span.start().offset() as usize;
    let end = span.end().offset() as usize;
    &source[start..end]
}

#[test]
fn local_binding_definitions_have_correct_spans() {
    let code = "-> a b << a b + >>";
    let result = analyze(code);

    // Should have 2 local definitions: a and b
    let defs: Vec<_> = result.symbols.definitions().collect();
    assert_eq!(defs.len(), 2, "Expected 2 definitions, got {}", defs.len());

    // Check that spans point to the actual variable names
    let a_def = defs.iter().find(|d| d.name == "a").expect("Definition 'a' not found");
    let b_def = defs.iter().find(|d| d.name == "b").expect("Definition 'b' not found");

    let a_text = span_text(code, a_def.span);
    let b_text = span_text(code, b_def.span);

    assert_eq!(a_text, "a", "Expected span for 'a' to contain 'a', got '{}'", a_text);
    assert_eq!(b_text, "b", "Expected span for 'b' to contain 'b', got '{}'", b_text);
}

#[test]
fn local_binding_references_have_correct_spans() {
    let code = "-> a b << a b + >>";
    let result = analyze(code);

    // Should have references to a and b in the body
    let refs: Vec<_> = result.symbols.references().collect();

    // Find references to 'a' and 'b'
    let a_refs: Vec<_> = refs.iter().filter(|r| r.name == "a").collect();
    let b_refs: Vec<_> = refs.iter().filter(|r| r.name == "b").collect();

    assert!(!a_refs.is_empty(), "Expected at least one reference to 'a'");
    assert!(!b_refs.is_empty(), "Expected at least one reference to 'b'");

    // Check spans point to actual variable uses
    for r in &a_refs {
        let text = span_text(code, r.span);
        assert_eq!(text, "a", "Reference span should contain 'a', got '{}'", text);
    }
    for r in &b_refs {
        let text = span_text(code, r.span);
        assert_eq!(text, "b", "Reference span should contain 'b', got '{}'", text);
    }
}

#[test]
fn local_variables_are_marked_as_referenced() {
    let code = "-> a b << a b + >>";
    let result = analyze(code);

    // All locals should be marked as referenced since they're used
    for def in result.symbols.definitions() {
        assert!(
            def.referenced,
            "Local '{}' should be marked as referenced",
            def.name
        );
    }

    // Should have no "unused variable" warnings
    let unused_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("unused") || d.message.contains("never used"))
        .collect();

    assert!(
        unused_warnings.is_empty(),
        "Should have no unused variable warnings, got: {:?}",
        unused_warnings
    );
}

#[test]
fn nested_local_binding_spans() {
    // Test with nested local bindings like in quicksort
    let code = "-> x << -> y << x y + >> >>";
    let result = analyze(code);

    let defs: Vec<_> = result.symbols.definitions().collect();

    // Should have x and y definitions
    let x_def = defs.iter().find(|d| d.name == "x");
    let y_def = defs.iter().find(|d| d.name == "y");

    assert!(x_def.is_some(), "Definition 'x' not found");
    assert!(y_def.is_some(), "Definition 'y' not found");

    let x_text = span_text(code, x_def.unwrap().span);
    let y_text = span_text(code, y_def.unwrap().span);

    assert_eq!(x_text, "x", "Span for 'x' should be 'x', got '{}'", x_text);
    assert_eq!(y_text, "y", "Span for 'y' should be 'y', got '{}'", y_text);
}

#[test]
fn multiple_locals_in_same_binding() {
    // Test -> less equal greater << ... >>
    let code = "-> less equal greater << less equal + greater + >>";
    let result = analyze(code);

    let defs: Vec<_> = result.symbols.definitions().collect();
    assert_eq!(defs.len(), 3, "Expected 3 definitions");

    for def in &defs {
        let text = span_text(code, def.span);
        assert_eq!(
            text, def.name,
            "Span for '{}' should match name, got '{}'",
            def.name, text
        );
    }

    // All should be referenced
    for def in &defs {
        assert!(def.referenced, "'{}' should be marked as referenced", def.name);
    }
}

#[test]
fn semantic_tokens_for_locals() {
    let mut session = Session::new();
    let source_id = session.set_source("test.rpl", "-> a b << a b + >>");

    // Verify analysis works
    let result = session.analyze(source_id).unwrap();

    // Check that definitions have non-overlapping spans
    let defs: Vec<_> = result.symbols.definitions().collect();
    for (i, def1) in defs.iter().enumerate() {
        for def2 in defs.iter().skip(i + 1) {
            let overlaps = def1.span.start() < def2.span.end()
                && def2.span.start() < def1.span.end();
            assert!(
                !overlaps,
                "Definitions '{}' and '{}' have overlapping spans: {:?} vs {:?}",
                def1.name, def2.name, def1.span, def2.span
            );
        }
    }

    // Check spans don't extend past their actual names
    for def in &defs {
        let len = def.span.len() as usize;
        assert_eq!(
            len, def.name.len(),
            "Span length {} doesn't match name length {} for '{}'",
            len, def.name.len(), def.name
        );
    }
}

#[test]
fn print_spans_for_debugging() {
    let code = "-> less equal greater << less equal + greater + >>";
    let result = analyze(code);

    println!("\nSource: {}", code);
    println!("Definitions:");
    for def in result.symbols.definitions() {
        let start = def.span.start().offset();
        let end = def.span.end().offset();
        let text = span_text(code, def.span);
        println!(
            "  {} @ {}..{} = '{}' (referenced: {})",
            def.name, start, end, text, def.referenced
        );
    }

    println!("\nReferences:");
    for r in result.symbols.references() {
        let start = r.span.start().offset();
        let end = r.span.end().offset();
        let text = span_text(code, r.span);
        println!("  {} @ {}..{} = '{}'", r.name, start, end, text);
    }
}

#[test]
fn semantic_tokens_quicksort_fixture() {
    use std::fs;

    // Load the actual quicksort fixture
    let code = fs::read_to_string("tests/programs/quicksort.rpl")
        .expect("Failed to read quicksort.rpl fixture");

    let mut session = Session::new();
    let source_id = session.set_source("quicksort.rpl", &code);

    // Get the analysis through session
    let result = session.analyze(source_id).unwrap();

    // Print all definitions and references for debugging
    println!("\nDefinitions in quicksort.rpl:");
    for def in result.symbols.definitions() {
        let start = def.span.start().offset();
        let end = def.span.end().offset();
        let text = span_text(&code, def.span);
        println!(
            "  '{}' @ {}..{} = '{}' (kind: {:?}, referenced: {})",
            def.name, start, end, text, def.kind, def.referenced
        );
    }

    // Verify LOCAL definitions have spans that match their names exactly
    // (Globals from STO have spans that include quotes, which is expected)
    for def in result.symbols.definitions() {
        let text = span_text(&code, def.span);
        if matches!(def.kind, rpl::analysis::DefinitionKind::Local | rpl::analysis::DefinitionKind::LoopVar) {
            assert_eq!(
                text, def.name,
                "Local definition '{}' has wrong span text '{}'",
                def.name, text
            );
        } else {
            // For globals, the span includes quotes (e.g., "partition3" STO)
            // The name should be the content without quotes
            let expected = format!("\"{}\"", def.name);
            assert_eq!(
                text, expected,
                "Global definition '{}' has wrong span text '{}', expected '{}'",
                def.name, text, expected
            );
        }
    }

    // Verify references have correct spans
    // For local refs, the span should match the name exactly
    // For global refs (from strings), the span includes quotes
    for r in result.symbols.references() {
        let text = span_text(&code, r.span);
        if text == r.name {
            // Exact match - good (locals, identifier-based refs)
            continue;
        }
        // Check if it's a quoted string
        let expected_quoted = format!("\"{}\"", r.name);
        assert!(
            text == expected_quoted || text == r.name,
            "Reference '{}' has wrong span text '{}', expected '{}' or '{}'",
            r.name, text, r.name, expected_quoted
        );
    }

    // Check specific locals from quicksort are found
    let local_names: Vec<_> = result
        .symbols
        .definitions()
        .filter(|d| matches!(d.kind, rpl::analysis::DefinitionKind::Local))
        .map(|d| d.name.as_str())
        .collect();

    // partition3 has: lst, piv, elem
    // quicksort has: lst, piv, less, equal, greater
    assert!(local_names.contains(&"lst"), "Should find local 'lst'");
    assert!(local_names.contains(&"piv"), "Should find local 'piv'");
    assert!(local_names.contains(&"elem"), "Should find local 'elem'");
    assert!(local_names.contains(&"less"), "Should find local 'less'");
    assert!(local_names.contains(&"equal"), "Should find local 'equal'");
    assert!(local_names.contains(&"greater"), "Should find local 'greater'");

    // Check loop variable 'i' from FOR loop (distinct LoopVar kind for reassignment warnings)
    let loop_var_names: Vec<_> = result
        .symbols
        .definitions()
        .filter(|d| matches!(d.kind, rpl::analysis::DefinitionKind::LoopVar))
        .map(|d| d.name.as_str())
        .collect();
    assert!(loop_var_names.contains(&"i"), "Should find loop variable 'i'");

    // All locals and loop vars should be marked as referenced (they're all used)
    let unreferenced: Vec<_> = result
        .symbols
        .definitions()
        .filter(|d| {
            matches!(
                d.kind,
                rpl::analysis::DefinitionKind::Local | rpl::analysis::DefinitionKind::LoopVar
            ) && !d.referenced
        })
        .map(|d| d.name.as_str())
        .collect();

    assert!(
        unreferenced.is_empty(),
        "All locals/loop vars should be referenced, but these aren't: {:?}",
        unreferenced
    );

    // Verify arity is correctly determined for global functions
    // partition3: << -> lst piv << ... >> >> -> arity 2
    // quicksort: << -> lst << ... >> >> -> arity 1
    let partition3 = result
        .symbols
        .definitions()
        .find(|d| d.name == "partition3")
        .expect("partition3 should be defined");
    assert_eq!(
        partition3.arity,
        Some(2),
        "partition3 should have arity 2, got {:?}",
        partition3.arity
    );

    let quicksort = result
        .symbols
        .definitions()
        .find(|d| d.name == "quicksort")
        .expect("quicksort should be defined");
    assert_eq!(
        quicksort.arity,
        Some(1),
        "quicksort should have arity 1, got {:?}",
        quicksort.arity
    );

    // Print arity info for debugging
    println!("\nFunction arities:");
    for def in result.symbols.definitions() {
        if def.arity.is_some() {
            println!("  '{}' arity: {:?}", def.name, def.arity);
        }
    }
}

#[test]
fn semantic_tokens_multiline() {
    use rpl::session::lsp::{encode_semantic_tokens, SemanticToken};
    use rpl::source::{SourceFile, SourceId};
    use rpl::core::Pos;

    // Multi-line code to test delta encoding
    let code = "-> a <<\n  a\n>>";

    let source_file = SourceFile::new(SourceId::new(0), "test.rpl".into(), code.to_string());

    // Create mock tokens at known positions
    // "a" definition is at offset 3 (line 1, col 4 in 1-indexed = line 0, col 3 in 0-indexed)
    // "a" reference is at offset 10 (line 2, col 3 in 1-indexed = line 1, col 2 in 0-indexed)
    let tokens = vec![
        SemanticToken {
            start: Pos::new(3), // first "a"
            length: 1,
            token_type: 7, // Parameter
            modifiers: 1,
        },
        SemanticToken {
            start: Pos::new(10), // second "a" (after newline + 2 spaces)
            length: 1,
            token_type: 6, // Variable
            modifiers: 0,
        },
    ];

    let encoded = encode_semantic_tokens(&tokens, &source_file);

    // Expected encoding (each token is 5 u32s):
    // Token 1: delta_line=0, delta_char=3, length=1, type=7, mods=1
    // Token 2: delta_line=1, delta_char=2, length=1, type=6, mods=0
    println!("Encoded tokens: {:?}", encoded);
    println!("Source: {:?}", code);

    // Verify the encoding
    assert_eq!(encoded.len(), 10, "Should have 2 tokens * 5 values");

    // First token
    assert_eq!(encoded[0], 0, "First token delta_line should be 0");
    assert_eq!(encoded[1], 3, "First token delta_char should be 3");
    assert_eq!(encoded[2], 1, "First token length should be 1");
    assert_eq!(encoded[3], 7, "First token type should be 7 (Parameter)");
    assert_eq!(encoded[4], 1, "First token modifiers should be 1");

    // Second token
    assert_eq!(encoded[5], 1, "Second token delta_line should be 1");
    assert_eq!(encoded[6], 2, "Second token delta_char should be 2");
    assert_eq!(encoded[7], 1, "Second token length should be 1");
    assert_eq!(encoded[8], 6, "Second token type should be 6 (Variable)");
    assert_eq!(encoded[9], 0, "Second token modifiers should be 0");
}
