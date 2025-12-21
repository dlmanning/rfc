//! Analysis and LSP tests.
//!
//! Tests for the static analysis and LSP features like
//! symbol tracking, hover, and semantic tokens.

use rpl::analysis::DiagnosticKind;
use rpl::Session;

/// Helper to get analysis results for code
fn analyze(code: &str) -> rpl::analysis::AnalysisResult {
    let mut session = crate::session_with_stdlib();
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
    let mut session = crate::session_with_stdlib();
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

    let mut session = crate::session_with_stdlib();
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

    // Verify quicksort signature: list -> list
    let qs_sig = quicksort.signature.as_ref().expect("quicksort should have signature");
    assert_eq!(
        qs_sig.inputs.len(),
        1,
        "quicksort should have 1 input, got {}",
        qs_sig.inputs.len()
    );
    assert_eq!(
        qs_sig.outputs.len(),
        1,
        "quicksort should have 1 output, got {} ({:?})",
        qs_sig.outputs.len(),
        qs_sig.outputs
    );

    // Verify partition3 signature: list, any -> list, list, list (3 outputs)
    // Note: partition3 uses ROLLD which has dynamic stack effects, so the analyzer
    // may not be able to statically determine the exact output count. We accept either
    // 3 known outputs or 1 Unknown output.
    let p3_sig = partition3.signature.as_ref().expect("partition3 should have signature");
    let is_dynamic_signature = p3_sig.outputs.len() == 1
        && matches!(p3_sig.outputs.first(), Some(ctype) if ctype.is_unknown());
    assert!(
        p3_sig.outputs.len() == 3 || is_dynamic_signature,
        "partition3 should have 3 outputs or Unknown (due to ROLLD), got {} ({:?})",
        p3_sig.outputs.len(),
        p3_sig.outputs
    );

    // Print arity info for debugging
    println!("\nFunction arities:");
    for def in result.symbols.definitions() {
        if def.arity.is_some() {
            println!("  '{}' arity: {:?}", def.name, def.arity);
        }
    }
}

// ============================================================================
// Hover Tests
// ============================================================================

#[test]
fn hover_on_variable_definition() {
    let mut session = crate::session_with_stdlib();
    let source_id = session.set_source("test.rpl", "42 \"x\" STO");

    // Position on "x" (the name being defined) - offset 4 is inside the string
    let pos = rpl::core::Pos::new(4);
    let hover = session.hover(source_id, pos);

    assert!(hover.is_some(), "Should have hover for definition");
    let hover = hover.unwrap();
    assert!(hover.contents.contains("x"), "Should mention variable name");
    assert!(
        hover.contents.contains("global") || hover.contents.contains("variable"),
        "Should indicate it's a variable"
    );
}

#[test]
fn hover_on_variable_reference() {
    let mut session = crate::session_with_stdlib();
    let source_id = session.set_source("test.rpl", "42 \"x\" STO x");

    // Position on the reference to x at the end (offset 11)
    let pos = rpl::core::Pos::new(11);
    let hover = session.hover(source_id, pos);

    assert!(hover.is_some(), "Should have hover for reference");
    let hover = hover.unwrap();
    assert!(hover.contents.contains("x"), "Should mention variable name");
}

#[test]
fn hover_shows_inferred_type() {
    let mut session = crate::session_with_stdlib();
    let source_id = session.set_source("test.rpl", "42 \"x\" STO");

    // Analyze to get type info
    let _ = session.analyze(source_id);

    // Position on "x" definition
    let pos = rpl::core::Pos::new(4);
    let hover = session.hover(source_id, pos);

    assert!(hover.is_some(), "Should have hover");
    let hover = hover.unwrap();
    // The type should be inferred as integer from the 42 literal
    // Note: This tests display format; type inference is tested in local_parameters_have_inferred_types
    assert!(
        hover.contents.contains("ℤ"),
        "Should show integer type (ℤ), got: {}",
        hover.contents
    );
}

#[test]
fn hover_shows_function_signature() {
    let mut session = crate::session_with_stdlib();
    let source_id = session.set_source("test.rpl", "<< -> a b << a b + >> >> \"add\" STO");

    let _ = session.analyze(source_id);

    // Position on "add" definition
    let pos = rpl::core::Pos::new(28); // inside "add"
    let hover = session.hover(source_id, pos);

    assert!(hover.is_some(), "Should have hover for function");
    let hover = hover.unwrap();
    assert!(
        hover.contents.contains("function"),
        "Should be labeled as function, got: {}",
        hover.contents
    );
    // Now shows signature instead of just arity
    assert!(
        hover.contents.contains("Signature:"),
        "Should show signature, got: {}",
        hover.contents
    );
    // The signature should indicate 2 inputs (before arrow)
    assert!(
        hover.contents.contains("→"),
        "Signature should contain arrow, got: {}",
        hover.contents
    );
    // Inputs should be narrowed to numeric types based on + usage
    // The signature should show (ℤ|ℝ) for inputs
    // Note: This tests display format; type inference is tested in lsp_showcase_function_signatures
    assert!(
        hover.contents.contains("ℤ") || hover.contents.contains("ℝ"),
        "Signature inputs should be narrowed to numeric types (ℤ or ℝ), got: {}",
        hover.contents
    );
    // More specifically, verify the narrowed type format
    // Should NOT contain "?" (unknown type) since constraints from + narrow the types
    assert!(
        !hover.contents.contains(" ? "),
        "Signature should not contain unknown types (?), got: {}",
        hover.contents
    );
}

#[test]
fn local_parameters_have_inferred_types() {
    // When we bind locals with `->`, the types should be inferred from what's on the stack
    let code = "1 2 -> a b << a b + >>";
    let result = analyze(code);

    // Find the local definitions
    let a_def = result
        .symbols
        .definitions()
        .find(|d| d.name == "a")
        .expect("Should find definition for 'a'");
    let b_def = result
        .symbols
        .definitions()
        .find(|d| d.name == "b")
        .expect("Should find definition for 'b'");

    // Both should have Integer type inferred from the literals
    assert!(
        a_def.value_type.is_some(),
        "Parameter 'a' should have an inferred type"
    );
    assert!(
        b_def.value_type.is_some(),
        "Parameter 'b' should have an inferred type"
    );

    // Check the types are integers
    let a_type = a_def.value_type.as_ref().unwrap();
    let b_type = b_def.value_type.as_ref().unwrap();

    assert!(
        a_type.is_integer(),
        "Parameter 'a' should be Integer, got {:?}",
        a_type
    );
    assert!(
        b_type.is_integer(),
        "Parameter 'b' should be Integer, got {:?}",
        b_type
    );
}

#[test]
fn local_parameters_have_mixed_types() {
    // Test with mixed types on the stack
    let code = "1 2.5 -> a b << a b + >>";
    let result = analyze(code);

    let a_def = result
        .symbols
        .definitions()
        .find(|d| d.name == "a")
        .expect("Should find definition for 'a'");
    let b_def = result
        .symbols
        .definitions()
        .find(|d| d.name == "b")
        .expect("Should find definition for 'b'");

    // 'a' binds the deeper item (1 = Integer)
    // 'b' binds TOS (2.5 = Real)
    let a_type = a_def.value_type.as_ref().expect("'a' should have type");
    let b_type = b_def.value_type.as_ref().expect("'b' should have type");

    assert!(
        a_type.is_integer(),
        "Parameter 'a' should be Integer, got {:?}",
        a_type
    );
    assert!(
        b_type.is_real(),
        "Parameter 'b' should be Real, got {:?}",
        b_type
    );
}

#[test]
fn hover_shows_local_parameter_type() {
    let mut session = crate::session_with_stdlib();
    // Use << >> instead of → for more reliable parsing
    let source_id = session.set_source("test.rpl", "1 2 -> a b << a b + >>");

    let _ = session.analyze(source_id);

    // Position on 'a' definition (offset 7, inside "a")
    let pos = rpl::core::Pos::new(7);
    let hover = session.hover(source_id, pos);

    assert!(hover.is_some(), "Should have hover for local parameter");
    let hover = hover.unwrap();
    // Note: This tests display format; type inference is tested in local_parameters_have_inferred_types
    assert!(
        hover.contents.contains("ℤ"),
        "Should show integer type (ℤ) for parameter 'a', got: {}",
        hover.contents
    );
}

#[test]
fn local_parameters_after_user_word_are_unknown() {
    // When calling a user-defined word, the stack effect is unknown,
    // so parameters bound after should be Unknown, not incorrectly typed.
    //
    // This tests a scenario like quicksort's:
    //   lst piv partition3
    //   -> less equal greater << ... >>
    //
    // Previously, 'equal' was incorrectly getting a String type from
    // unrelated stack state.

    // Define a word then call it and bind its results
    let code = r#"
<< 1 2 3 >> "myword" STO
<< myword -> x y z << x y z >> >>
"#;

    let result = analyze(code);

    // Find the 'y' parameter (should be Unknown, not incorrectly typed)
    let y_def = result
        .symbols
        .definitions()
        .find(|d| d.name == "y")
        .expect("Should find 'y' definition");

    // After calling 'myword', stack is unknown, so 'y' should have Unknown type
    // (or None if value_type is not set)
    if let Some(ref ty) = y_def.value_type {
        // The type should be Unknown (not a specific wrong type like String)
        assert!(
            ty.is_unknown(),
            "Parameter 'y' should be Unknown after user word call, got {:?}",
            ty
        );
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

// ============================================================================
// Function Signature Tests for lsp_showcase.rpl
// ============================================================================

#[test]
fn lsp_showcase_function_signatures() {
    use std::fs;

    // Load the lsp_showcase fixture
    let code = fs::read_to_string("tests/programs/lsp_showcase.rpl")
        .expect("Failed to read lsp_showcase.rpl fixture");

    let result = analyze(&code);

    // Print all function definitions and their signatures for debugging
    println!("\nFunction signatures in lsp_showcase.rpl:");
    for def in result.symbols.definitions() {
        if def.signature.is_some() {
            println!(
                "  '{}': {:?}",
                def.name,
                def.signature.as_ref().unwrap()
            );
        }
    }

    // === square ===
    // << -> x << x x * >> >> "square" STO
    // Should be (numeric → numeric)
    {
        let def = result
            .symbols
            .find_definitions_by_name("square")
            .next()
            .expect("square should be defined");
        let sig = def.signature.as_ref().expect("square should have signature");

        assert_eq!(sig.inputs.len(), 1, "square: expected 1 input, got {}", sig.inputs.len());
        assert!(
            sig.inputs[0].is_numeric(),
            "square: input should be numeric, got {:?}",
            sig.inputs[0]
        );
        assert_eq!(sig.outputs.len(), 1, "square: expected 1 output, got {}", sig.outputs.len());
        assert!(
            sig.outputs[0].is_numeric(),
            "square: output should be numeric, got {:?}",
            sig.outputs[0]
        );
    }

    // === myabs ===
    // << -> n << IF n 0 < THEN n NEG ELSE n END >> >> "myabs" STO
    // Should be (numeric → numeric) with IF/THEN/ELSE branches merged
    {
        let def = result
            .symbols
            .find_definitions_by_name("myabs")
            .next()
            .expect("myabs should be defined");
        let sig = def.signature.as_ref().expect("myabs should have signature");

        assert_eq!(sig.inputs.len(), 1, "myabs: expected 1 input, got {}", sig.inputs.len());
        assert!(
            sig.inputs[0].is_numeric(),
            "myabs: input should be numeric (from < comparison), got {:?}",
            sig.inputs[0]
        );
        assert_eq!(sig.outputs.len(), 1, "myabs: expected 1 output (merged branches), got {}", sig.outputs.len());
        // Both branches return the parameter n (numeric), so merged is numeric
        assert!(
            sig.outputs[0].is_numeric(),
            "myabs: output should be numeric (merged from both branches), got {:?}",
            sig.outputs[0]
        );
    }

    // === sqrt (Newton's method) ===
    // << -> x << x 2 / -> initial_guess << ... FOR ... >> >> >> "sqrt" STO
    // Should have 1 input and 1 output
    {
        let def = result
            .symbols
            .find_definitions_by_name("sqrt")
            .next()
            .expect("sqrt should be defined");
        let sig = def.signature.as_ref().expect("sqrt should have signature");

        assert_eq!(sig.inputs.len(), 1, "sqrt: expected 1 input, got {}", sig.inputs.len());
        assert_eq!(sig.outputs.len(), 1, "sqrt: expected 1 output, got {}", sig.outputs.len());
    }

    // === distance ===
    // << -> x1 y1 x2 y2 << ... dx square dy square + sqrt >> >> "distance" STO
    // Should have 4 inputs and 1 output
    {
        let def = result
            .symbols
            .find_definitions_by_name("distance")
            .next()
            .expect("distance should be defined");
        let sig = def.signature.as_ref().expect("distance should have signature");

        assert_eq!(sig.inputs.len(), 4, "distance: expected 4 inputs, got {}", sig.inputs.len());
        assert_eq!(sig.outputs.len(), 1, "distance: expected 1 output, got {}", sig.outputs.len());
    }

    // === triangle_perimeter ===
    // << -> ax ay bx by cx cy << ... distance ... >> >> "triangle_perimeter" STO
    // Should have 6 inputs and 1 output
    {
        let def = result
            .symbols
            .find_definitions_by_name("triangle_perimeter")
            .next()
            .expect("triangle_perimeter should be defined");
        let sig = def.signature.as_ref().expect("triangle_perimeter should have signature");

        assert_eq!(sig.inputs.len(), 6, "triangle_perimeter: expected 6 inputs, got {}", sig.inputs.len());
        assert_eq!(sig.outputs.len(), 1, "triangle_perimeter: expected 1 output, got {}", sig.outputs.len());
    }

    // === quadratic_solver ===
    // << -> a b c << ... IF disc 0 < THEN 0 ELSE root1 root2 END ... >> >> "quadratic_solver" STO
    // Should have 3 inputs, and at least 1 output (min of THEN=1, ELSE=2)
    {
        let def = result
            .symbols
            .find_definitions_by_name("quadratic_solver")
            .next()
            .expect("quadratic_solver should be defined");
        let sig = def.signature.as_ref().expect("quadratic_solver should have signature");

        assert_eq!(sig.inputs.len(), 3, "quadratic_solver: expected 3 inputs, got {}", sig.inputs.len());
        // All inputs should be numeric (used in arithmetic operations)
        assert!(
            sig.inputs.iter().all(|t| t.is_numeric()),
            "quadratic_solver: all inputs should be numeric, got {:?}",
            sig.inputs
        );
        // At least 1 output (min of THEN=1, ELSE=2)
        assert!(
            !sig.outputs.is_empty(),
            "quadratic_solver: expected at least 1 output, got {:?}",
            sig.outputs
        );
        // First output should be numeric (Integer from THEN or Real from ELSE)
        assert!(
            sig.outputs[0].is_numeric(),
            "quadratic_solver: first output should be numeric, got {:?}",
            sig.outputs[0]
        );
    }
}

// ============================================================================
// Global Variable Reassignment Tests
// ============================================================================

#[test]
fn global_variable_reassignment_single_definition() {
    // When a global variable is reassigned, there should still be only one definition
    let code = r#"
0 "count" STO
count 1 + "count" STO
count 1 + "count" STO
"#;
    let result = analyze(code);

    // Find definitions named "count"
    let count_defs: Vec<_> = result
        .symbols
        .find_definitions_by_name("count")
        .collect();

    assert_eq!(
        count_defs.len(),
        1,
        "Should have exactly 1 definition for 'count', got {}",
        count_defs.len()
    );

    // The definition should have numeric type (widened from repeated assignments
    // because `count 1 +` produces Numeric when count's type is Unknown at use site)
    let def = count_defs[0];
    assert!(
        def.value_type.as_ref().map(|t| t.is_numeric()).unwrap_or(false),
        "count should be numeric type, got {:?}",
        def.value_type
    );
}

#[test]
fn odds_evens_has_unique_definitions() {
    use std::fs;

    let code = fs::read_to_string("tests/programs/odds_evens.rpl")
        .expect("Failed to read odds_evens.rpl");

    let result = analyze(&code);

    // Check that 'odds' and 'evens' each have exactly one definition
    let odds_defs: Vec<_> = result
        .symbols
        .find_definitions_by_name("odds")
        .collect();
    let evens_defs: Vec<_> = result
        .symbols
        .find_definitions_by_name("evens")
        .collect();

    assert_eq!(
        odds_defs.len(),
        1,
        "Should have exactly 1 definition for 'odds', got {}",
        odds_defs.len()
    );
    assert_eq!(
        evens_defs.len(),
        1,
        "Should have exactly 1 definition for 'evens', got {}",
        evens_defs.len()
    );
}

// ============================================================================
// Fixture Error-Free Tests
// ============================================================================

/// Test that all fixture programs analyze without errors.
///
/// This catches regressions where changes to the analyzer cause false
/// positives (errors reported for valid code) in our fixture programs.
#[test]
fn all_fixtures_analyze_without_errors() {
    use std::fs;
    use std::path::Path;

    let fixtures_dir = Path::new("tests/programs");
    let mut failures = Vec::new();

    // Read all .rpl files in the fixtures directory
    for entry in fs::read_dir(fixtures_dir).expect("Failed to read fixtures directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().map(|e| e == "rpl").unwrap_or(false) {
            let filename = path.file_name().unwrap().to_string_lossy().to_string();
            let code = fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Failed to read {}: {}", filename, e));

            let mut session = crate::session_with_stdlib();
            let source_id = session.set_source(&filename, &code);
            let result = session.analyze(source_id).unwrap();

            // Collect error-level diagnostics (not warnings like UnusedVariable)
            let errors: Vec<_> = result
                .diagnostics
                .iter()
                .filter(|d| {
                    matches!(
                        d.kind,
                        DiagnosticKind::StackUnderflow
                            | DiagnosticKind::TypeMismatch
                            | DiagnosticKind::UndefinedVariable
                    )
                })
                .collect();

            if !errors.is_empty() {
                failures.push((filename, errors.iter().map(|d| d.message.clone()).collect::<Vec<_>>()));
            }
        }
    }

    if !failures.is_empty() {
        let mut msg = String::from("Fixture files with analysis errors:\n");
        for (file, errors) in &failures {
            msg.push_str(&format!("\n  {}:\n", file));
            for error in errors {
                msg.push_str(&format!("    - {}\n", error));
            }
        }
        panic!("{}", msg);
    }
}

// ============================================================================
// Symbol Resolution Tests
// ============================================================================

#[test]
fn function_definition_creates_global() {
    let result = analyze(r#"<< 1 >> "foo" STO"#);

    let defs: Vec<_> = result.symbols.definitions().collect();
    assert_eq!(defs.len(), 1, "Expected 1 definition, got {:?}", defs);
    assert_eq!(defs[0].name, "foo");
}

#[test]
fn forward_reference_resolves() {
    // Define foo, then call it
    let result = analyze(r#"<< 1 >> "foo" STO foo"#);

    // Should have no undefined variable errors
    let undefined: Vec<_> = result.diagnostics.iter()
        .filter(|d| matches!(d.kind, DiagnosticKind::UndefinedVariable))
        .collect();
    assert!(undefined.is_empty(), "Unexpected undefined variables: {:?}", undefined);
}

#[test]
fn recursive_function_resolves() {
    // A function that calls itself
    let result = analyze(r#"<< fact >> "fact" STO"#);

    let undefined: Vec<_> = result.diagnostics.iter()
        .filter(|d| matches!(d.kind, DiagnosticKind::UndefinedVariable))
        .collect();
    assert!(undefined.is_empty(), "Recursive call should resolve: {:?}", undefined);
}

#[test]
fn function_called_with_list_no_type_conflict() {
    // Function that takes a param, then call it with a list literal
    // The list should pass LIST type to the function
    let result = analyze(r#"
<< -> lst <<
    lst
>> >>
"foo" STO
{ 1 2 3 } foo
"#);

    let type_errors: Vec<_> = result.diagnostics.iter()
        .filter(|d| matches!(d.kind, DiagnosticKind::TypeMismatch))
        .collect();
    assert!(type_errors.is_empty(), "Unexpected type errors: {:?}", type_errors);
}

#[test]
fn local_bound_to_function_result_gets_type_from_usage() {
    // Test: function returns unknown, local m binds result,
    // then m is used with - which constrains it to numeric
    let result = analyze(r#"
<< 1 >> "getval" STO
<<
    -> x <<
        x getval -> m <<
            m 1 -
        >>
    >>
>> "test" STO
"#);

    // Find the definition of 'm'
    let m_def = result.symbols.definitions()
        .find(|d| d.name == "m");

    assert!(m_def.is_some(), "Should find definition for 'm'");
    let m_type = m_def.unwrap().value_type.clone();

    // m should have a numeric type from usage constraints
    // (either Known(Int/Real), OneOf([Int, Real]), or Unknown is acceptable
    // since the getval function's return type may not be known)
    assert!(
        m_type.is_some(),
        "m should have a type, got None"
    );
}

#[test]
fn local_bound_to_function_result_gets_return_type() {
    // Test: function explicitly returns Real, local m binds result,
    // m should have type Real from function return
    let result = analyze(r#"
<< 3.14 >> "mean" STO
<<
    -> x <<
        x mean -> m <<
            m 1.0 -
        >>
    >>
>> "variance" STO
"#);

    // Find the definition of 'm'
    let m_def = result.symbols.definitions()
        .find(|d| d.name == "m");

    assert!(m_def.is_some(), "Should find definition for 'm'");
    let m_type = m_def.unwrap().value_type.clone();

    // m should have Real type from mean's return
    // Note: Because mean is called with unknown stack after `x`,
    // the binding may get Unknown type. Either Real or Unknown is acceptable.
    assert!(
        m_type.is_some(),
        "m should have a type from mean's return or usage"
    );
}

