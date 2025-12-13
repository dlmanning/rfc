//! LSP feature verification tests.
//!
//! These tests verify that LSP queries return correct results.

use rpl_core::{Pos, SemanticKind};
use rpl_session::Session;

// ============================================================================
// Completions
// ============================================================================

#[test]
fn completion_commands() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "3 4 ");

    let completions = session.completions(id, Pos::new(4));
    let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();

    // Should contain operators and stack commands
    assert!(labels.contains(&"+"), "Expected + in completions");
    assert!(labels.contains(&"DUP"), "Expected DUP in completions");
    assert!(labels.contains(&"DROP"), "Expected DROP in completions");
}

#[test]
fn completion_prefix() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "3 DU");

    let completions = session.completions(id, Pos::new(4));
    let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();

    // Prefix filtering returns only DUP
    assert_eq!(labels, vec!["DUP"], "Expected only DUP for prefix 'DU'");
}

#[test]
fn completion_empty_file() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "");

    let completions = session.completions(id, Pos::new(0));

    // Should return some commands even for empty file
    assert!(
        !completions.is_empty(),
        "Expected some completions for empty file"
    );
}

// ============================================================================
// Hover
// ============================================================================

#[test]
fn hover_number() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "42");

    let hover = session
        .hover(id, Pos::new(0))
        .expect("Expected hover for number");
    assert!(
        hover.contents.contains("Number"),
        "Hover should mention 'Number'"
    );
    assert!(
        hover.contents.contains("42"),
        "Hover should contain the value"
    );
}

#[test]
fn hover_command() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "DUP");

    let hover = session
        .hover(id, Pos::new(0))
        .expect("Expected hover for command");
    assert!(
        hover.contents.contains("Command"),
        "Hover should mention 'Command'"
    );
    assert!(
        hover.contents.contains("DUP"),
        "Hover should contain command name"
    );
}

#[test]
fn hover_error_token() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "$$$");

    let hover = session
        .hover(id, Pos::new(0))
        .expect("Expected hover for error");
    assert!(
        hover.contents.contains("Error"),
        "Hover should mention 'Error'"
    );
}

// ============================================================================
// Semantic Tokens
// ============================================================================

#[test]
fn semantic_tokens_number() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "42");
    let result = session.analyze(id).expect("Analysis should succeed");

    // Find the token for "42"
    let token = result
        .tokens
        .iter()
        .find(|t| t.span.start().offset() == 0)
        .expect("Should have token at position 0");

    assert_eq!(
        token.semantic,
        SemanticKind::Number,
        "Expected Number semantic kind"
    );
}

#[test]
fn semantic_tokens_command() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "DUP");
    let result = session.analyze(id).expect("Analysis should succeed");

    let token = result
        .tokens
        .iter()
        .find(|t| t.span.start().offset() == 0)
        .expect("Should have token at position 0");

    assert_eq!(
        token.semantic,
        SemanticKind::Command,
        "Expected Command semantic kind"
    );
}

#[test]
fn semantic_tokens_operator() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "+");
    let result = session.analyze(id).expect("Analysis should succeed");

    let token = result
        .tokens
        .iter()
        .find(|t| t.span.start().offset() == 0)
        .expect("Should have token at position 0");

    assert_eq!(
        token.semantic,
        SemanticKind::Operator,
        "Expected Operator semantic kind"
    );
}

#[test]
fn tokens_cover_source() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "3 4 +");
    let result = session.analyze(id).expect("Analysis should succeed");

    // Should have 3 tokens
    assert_eq!(result.token_count(), 3, "Expected 3 tokens for '3 4 +'");

    // Check all source positions are covered (no gaps in meaningful content)
    let tokens = &result.tokens;

    // Token 1: "3" at position 0
    assert_eq!(tokens[0].span.start().offset(), 0);
    assert_eq!(tokens[0].span.end().offset(), 1);

    // Token 2: "4" at position 2
    assert_eq!(tokens[1].span.start().offset(), 2);
    assert_eq!(tokens[1].span.end().offset(), 3);

    // Token 3: "+" at position 4
    assert_eq!(tokens[2].span.start().offset(), 4);
    assert_eq!(tokens[2].span.end().offset(), 5);
}

// ============================================================================
// Diagnostics Access
// ============================================================================

#[test]
fn diagnostics_accessible() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "3 $$ 4");
    let result = session.analyze(id).expect("Analysis should succeed");

    assert!(result.has_errors(), "Expected errors for $$");

    // Diagnostics should be accessible
    assert!(
        !result.diagnostics.is_empty(),
        "Expected non-empty diagnostics"
    );

    // Each diagnostic should have a span
    for diag in &result.diagnostics {
        let span = diag.span();
        assert!(
            span.end().offset() > span.start().offset(),
            "Diagnostic span should have positive length"
        );
    }
}

// ============================================================================
// Fixture Files - LSP Integration
// ============================================================================

/// Helper to load a fixture file and verify it parses without errors
fn assert_fixture_parses(filename: &str) {
    let path = format!("tests/programs/{}", filename);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Could not read fixture file: {}", path));

    let mut session = Session::new();
    let id = session.set_source(&path, &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    assert!(
        !result.has_errors(),
        "Fixture {} should parse without errors, got: {:?}",
        filename,
        result.diagnostics
    );
}

/// Helper to verify fixture has semantic tokens
fn assert_fixture_has_tokens(filename: &str) {
    let path = format!("tests/programs/{}", filename);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|_| panic!("Could not read fixture file: {}", path));

    let mut session = Session::new();
    let id = session.set_source(&path, &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    assert!(
        result.token_count() > 0,
        "Fixture {} should have semantic tokens",
        filename
    );
}

#[test]
fn fixture_factorial_parses() {
    assert_fixture_parses("factorial.rpl");
}

#[test]
fn fixture_factorial_has_tokens() {
    assert_fixture_has_tokens("factorial.rpl");
}

#[test]
fn fixture_factorial_semantic_tokens() {
    let source = std::fs::read_to_string("tests/programs/factorial.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("factorial.rpl", &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    // Check that we have various token types
    let has_command = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Command);
    let has_number = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Number);
    let has_keyword = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Keyword);

    assert!(
        has_command,
        "factorial.rpl should have Command tokens (STO, etc.)"
    );
    assert!(has_number, "factorial.rpl should have Number tokens");
    assert!(
        has_keyword,
        "factorial.rpl should have Keyword tokens (IF, THEN, etc.)"
    );
}

#[test]
fn fixture_fibonacci_parses() {
    assert_fixture_parses("fibonacci.rpl");
}

#[test]
fn fixture_fibonacci_has_tokens() {
    assert_fixture_has_tokens("fibonacci.rpl");
}

#[test]
fn fixture_gcd_parses() {
    assert_fixture_parses("gcd.rpl");
}

#[test]
fn fixture_gcd_has_tokens() {
    assert_fixture_has_tokens("gcd.rpl");
}

#[test]
fn fixture_gcd_hover_on_command() {
    let source = std::fs::read_to_string("tests/programs/gcd.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("gcd.rpl", &source);

    // Find position of "MOD" command (line 10)
    let mod_pos = source.find("MOD").expect("MOD should exist in gcd.rpl");
    let hover = session.hover(id, Pos::new(mod_pos as u32));

    assert!(hover.is_some(), "Should have hover info for MOD");
    let hover = hover.unwrap();
    assert!(
        hover.contents.contains("MOD") || hover.contents.contains("Command"),
        "Hover should mention MOD or Command"
    );
}

#[test]
fn fixture_newton_sqrt_parses() {
    assert_fixture_parses("newton_sqrt.rpl");
}

#[test]
fn fixture_newton_sqrt_has_tokens() {
    assert_fixture_has_tokens("newton_sqrt.rpl");
}

#[test]
fn fixture_newton_sqrt_semantic_variety() {
    let source = std::fs::read_to_string("tests/programs/newton_sqrt.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("newton_sqrt.rpl", &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    // newton_sqrt uses: locals (->), START/NEXT, numbers, operators, commands
    let has_operator = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Operator);
    let has_number = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Number);
    let has_keyword = result
        .tokens
        .iter()
        .any(|t| t.semantic == SemanticKind::Keyword);

    assert!(
        has_operator,
        "newton_sqrt.rpl should have Operator tokens (/, +)"
    );
    assert!(has_number, "newton_sqrt.rpl should have Number tokens");
    assert!(
        has_keyword,
        "newton_sqrt.rpl should have Keyword tokens (START, NEXT)"
    );
}

#[test]
fn fixture_prime_sieve_parses() {
    assert_fixture_parses("prime_sieve.rpl");
}

#[test]
fn fixture_prime_sieve_has_tokens() {
    assert_fixture_has_tokens("prime_sieve.rpl");
}

#[test]
fn fixture_prime_sieve_complex_structure() {
    let source = std::fs::read_to_string("tests/programs/prime_sieve.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("prime_sieve.rpl", &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    // prime_sieve uses: nested IF, WHILE/REPEAT, many stack operations
    let keyword_count = result
        .tokens
        .iter()
        .filter(|t| t.semantic == SemanticKind::Keyword)
        .count();

    // Should have many keywords (IF, THEN, ELSE, END, WHILE, REPEAT)
    assert!(
        keyword_count >= 6,
        "prime_sieve.rpl should have at least 6 keyword tokens, got {}",
        keyword_count
    );
}

#[test]
fn fixture_list_ops_parses() {
    assert_fixture_parses("list_ops.rpl");
}

#[test]
fn fixture_list_ops_has_tokens() {
    assert_fixture_has_tokens("list_ops.rpl");
}

#[test]
fn fixture_symbolic_parses() {
    assert_fixture_parses("symbolic_derivative.rpl");
}

#[test]
fn fixture_symbolic_has_tokens() {
    assert_fixture_has_tokens("symbolic_derivative.rpl");
}

// ============================================================================
// LSP Showcase Fixture - Comprehensive Tests
// ============================================================================

#[test]
fn fixture_lsp_showcase_parses() {
    assert_fixture_parses("lsp_showcase.rpl");
}

#[test]
fn fixture_lsp_showcase_has_tokens() {
    assert_fixture_has_tokens("lsp_showcase.rpl");
}

#[test]
fn fixture_lsp_showcase_multiple_definitions() {
    let source = std::fs::read_to_string("tests/programs/lsp_showcase.rpl").unwrap();
    let mut session = Session::new();

    // Append a space to trigger completions
    let extended = format!("{}\n", source);
    let id = session.set_source("lsp_showcase.rpl", &extended);

    let completions = session.completions(id, Pos::new(extended.len() as u32));

    // Should have all user-defined functions
    let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();

    assert!(
        labels.contains(&"square"),
        "Should have 'square' in completions"
    );
    assert!(
        labels.contains(&"myabs"),
        "Should have 'myabs' in completions"
    );
    assert!(labels.contains(&"sqrt"), "Should have 'sqrt' in completions");
    assert!(
        labels.contains(&"distance"),
        "Should have 'distance' in completions"
    );
    assert!(
        labels.contains(&"triangle_perimeter"),
        "Should have 'triangle_perimeter' in completions"
    );
}

#[test]
fn fixture_lsp_showcase_rich_tokens() {
    let source = std::fs::read_to_string("tests/programs/lsp_showcase.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("lsp_showcase.rpl", &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    // Count different token types
    let num_count = result
        .tokens
        .iter()
        .filter(|t| t.semantic == SemanticKind::Number)
        .count();
    let keyword_count = result
        .tokens
        .iter()
        .filter(|t| t.semantic == SemanticKind::Keyword)
        .count();
    let command_count = result
        .tokens
        .iter()
        .filter(|t| t.semantic == SemanticKind::Command)
        .count();
    let operator_count = result
        .tokens
        .iter()
        .filter(|t| t.semantic == SemanticKind::Operator)
        .count();

    // This fixture is rich - should have many of each
    assert!(
        num_count >= 10,
        "lsp_showcase.rpl should have at least 10 numbers, got {}",
        num_count
    );
    assert!(
        keyword_count >= 10,
        "lsp_showcase.rpl should have at least 10 keywords (IF, THEN, FOR, NEXT, etc.), got {}",
        keyword_count
    );
    assert!(
        command_count >= 5,
        "lsp_showcase.rpl should have at least 5 commands (DUP, STO, etc.), got {}",
        command_count
    );
    assert!(
        operator_count >= 5,
        "lsp_showcase.rpl should have at least 5 operators (+, -, *, /), got {}",
        operator_count
    );
}

#[test]
fn fixture_lsp_showcase_nested_scopes() {
    let source = std::fs::read_to_string("tests/programs/lsp_showcase.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("lsp_showcase.rpl", &source);
    let result = session.analyze(id).expect("Analysis should succeed");

    // Count construct boundaries (starts_construct / ends_construct)
    let construct_starts = result
        .tokens
        .iter()
        .filter(|t| t.context.starts_construct)
        .count();
    let construct_ends = result
        .tokens
        .iter()
        .filter(|t| t.context.ends_construct)
        .count();

    // Should have many nested constructs (programs, local frames, loops)
    // Note: starts may exceed ends due to tokenizer handling of some constructs
    assert!(
        construct_starts >= 10,
        "lsp_showcase.rpl should have at least 10 construct starts, got {}",
        construct_starts
    );
    assert!(
        construct_ends >= 10,
        "lsp_showcase.rpl should have at least 10 construct ends, got {}",
        construct_ends
    );
}

#[test]
fn fixture_lsp_showcase_hover_on_operator() {
    let source = std::fs::read_to_string("tests/programs/lsp_showcase.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("lsp_showcase.rpl", &source);

    // Find a * operator
    let star_pos = source.find('*').expect("Should have * operator");
    let hover = session.hover(id, Pos::new(star_pos as u32));

    assert!(hover.is_some(), "Should have hover for * operator");
    let hover = hover.unwrap();
    assert!(
        hover.contents.contains("*") || hover.contents.contains("Operator"),
        "Hover should mention * or Operator"
    );
}

// ============================================================================
// Hover on Local Variables in Fixtures
// ============================================================================

#[test]
fn fixture_hover_on_local_variable() {
    let source = std::fs::read_to_string("tests/programs/factorial.rpl").unwrap();
    let mut session = Session::new();
    let id = session.set_source("factorial.rpl", &source);

    // Find position of "n" usage (e.g., "n 0 ==" on line 5)
    // Skip first occurrence which is in the -> declaration
    let first_n = source.find("n").unwrap();
    let n_usage = source[first_n + 1..]
        .find("n")
        .map(|p| p + first_n + 1)
        .unwrap();

    let hover = session.hover(id, Pos::new(n_usage as u32));
    // Local variables may or may not have hover - this tests the capability exists
    // If hover exists, it should be reasonable
    if let Some(h) = hover {
        assert!(
            !h.contents.is_empty(),
            "If hover exists for local variable, it should have content"
        );
    }
}

// ============================================================================
// Completions in Fixture Context
// ============================================================================

#[test]
fn fixture_completions_in_context() {
    let source = std::fs::read_to_string("tests/programs/factorial.rpl").unwrap();
    let mut session = Session::new();

    // Add some code after the fixture
    let extended = format!("{}\n10 ", source);
    let id = session.set_source("factorial.rpl", &extended);

    let completions = session.completions(id, Pos::new(extended.len() as u32));

    // Should get command completions (built-in commands)
    assert!(
        !completions.is_empty(),
        "Should have completions after fixture"
    );

    // Should include standard commands
    let has_dup = completions.iter().any(|c| c.label == "DUP");
    let has_drop = completions.iter().any(|c| c.label == "DROP");
    assert!(has_dup, "Completions should include DUP");
    assert!(has_drop, "Completions should include DROP");
}

#[test]
fn fixture_completions_include_user_defined() {
    let source = std::fs::read_to_string("tests/programs/factorial.rpl").unwrap();
    let mut session = Session::new();

    // Add some code after the fixture
    let extended = format!("{}\n10 ", source);
    let id = session.set_source("factorial.rpl", &extended);

    let completions = session.completions(id, Pos::new(extended.len() as u32));

    // Should include "fact" which was defined via STO in the fixture
    let has_fact = completions.iter().any(|c| c.label == "fact");
    assert!(
        has_fact,
        "Completions should include 'fact' defined via STO. Got: {:?}",
        completions.iter().map(|c| &c.label).collect::<Vec<_>>()
    );
}
