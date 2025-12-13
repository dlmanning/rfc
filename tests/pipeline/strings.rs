//! Tests for string operations.

use rpl_lang::Value;

use super::{assert_error, assert_stack_eq, eval_to_values, to_int, to_string};

// ============================================================================
// Strings
// ============================================================================

#[test]
fn string_empty() {
    let values = eval_to_values("\"\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_simple() {
    let values = eval_to_values("\"hello\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_with_spaces() {
    let values = eval_to_values("\"hello world\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello world");
}

#[test]
fn string_with_escape_newline() {
    let values = eval_to_values("\"hello\\nworld\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello\nworld");
}

#[test]
fn string_with_escape_quote() {
    let values = eval_to_values("\"say \\\"hi\\\"\"");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "say \"hi\"");
}

#[test]
fn string_size() {
    assert_stack_eq("\"hello\" SIZE", &[5.0]);
}

#[test]
fn string_size_empty() {
    assert_stack_eq("\"\" SIZE", &[0.0]);
}

#[test]
fn string_size_short() {
    assert_stack_eq("\"hi\" SIZE", &[2.0]);
}

#[test]
fn string_head() {
    let values = eval_to_values("\"hello\" HEAD");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "h");
}

#[test]
fn string_tail() {
    let values = eval_to_values("\"hello\" TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "ello");
}

#[test]
fn string_tail_to_empty() {
    let values = eval_to_values("\"x\" TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_sub() {
    // SUB: string start length -> substring (1-based)
    let values = eval_to_values("\"hello world\" 1 5 SUB");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_sub_middle() {
    let values = eval_to_values("\"hello world\" 7 5 SUB");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "world");
}

#[test]
fn string_pos_found() {
    // POS: string pattern -> position (1-based, 0 if not found)
    assert_stack_eq("\"hello world\" \"world\" POS", &[7.0]);
}

#[test]
fn string_pos_not_found() {
    assert_stack_eq("\"hello world\" \"foo\" POS", &[0.0]);
}

#[test]
fn string_pos_at_start() {
    assert_stack_eq("\"hello\" \"hel\" POS", &[1.0]);
}

#[test]
fn string_num() {
    // NUM: Convert string to number
    assert_stack_eq("\"42\" NUM", &[42.0]);
}

#[test]
fn string_num_float() {
    assert_stack_eq("\"3.15\" NUM", &[3.15]);
}

#[test]
fn string_str() {
    // STR: Convert number to string
    let values = eval_to_values("42 STR");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "42");
}

#[test]
fn string_chr() {
    // CHR: Character code to string
    let values = eval_to_values("65 CHR");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "A");
}

#[test]
fn string_asc() {
    // ASC: String to character code
    assert_stack_eq("\"A\" ASC", &[65.0]);
}

#[test]
fn string_chr_asc_roundtrip() {
    assert_stack_eq("65 CHR ASC", &[65.0]);
}

#[test]
fn string_multiple_on_stack() {
    let values = eval_to_values("\"hello\" \"world\"");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_string(&values[1]), "world");
}

#[test]
fn string_in_list() {
    let values = eval_to_values("{ \"a\" \"b\" \"c\" }");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
            assert_eq!(to_string(&elements[0]), "a");
            assert_eq!(to_string(&elements[1]), "b");
            assert_eq!(to_string(&elements[2]), "c");
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

// ============================================================================
// String Functions: →UTF8/UTF8→ (byte conversion)
// ============================================================================

#[test]
fn string_toutf8_ascii() {
    // →UTF8: Convert string to list of UTF-8 bytes
    let values = eval_to_values("\"ABC\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
            assert_eq!(to_int(&elements[0]), 65); // 'A'
            assert_eq!(to_int(&elements[1]), 66); // 'B'
            assert_eq!(to_int(&elements[2]), 67); // 'C'
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_toutf8_unicode() {
    // UTF-8 encoding of "é" is [195, 169]
    let values = eval_to_values("\"é\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 2);
            assert_eq!(to_int(&elements[0]), 195);
            assert_eq!(to_int(&elements[1]), 169);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_toutf8_empty() {
    let values = eval_to_values("\"\" →UTF8");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 0);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn string_fromutf8_ascii() {
    // UTF8→: Convert list of UTF-8 bytes to string
    let values = eval_to_values("{ 72 105 } UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "Hi");
}

#[test]
fn string_fromutf8_unicode() {
    // UTF-8 bytes for "é"
    let values = eval_to_values("{ 195 169 } UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "é");
}

#[test]
fn string_utf8_roundtrip() {
    let values = eval_to_values("\"hello\" →UTF8 UTF8→");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

// ============================================================================
// String Functions: SREV (reverse)
// ============================================================================

#[test]
fn string_srev_simple() {
    let values = eval_to_values("\"hello\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "olleh");
}

#[test]
fn string_srev_empty() {
    let values = eval_to_values("\"\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_srev_single_char() {
    let values = eval_to_values("\"x\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "x");
}

#[test]
fn string_srev_unicode() {
    // Reverse preserves Unicode characters
    let values = eval_to_values("\"café\" SREV");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "éfac");
}

// ============================================================================
// String Functions: NTOKENS, NTHTOKEN, NTHTOKENPOS (tokenization)
// ============================================================================

#[test]
fn string_ntokens_simple() {
    assert_stack_eq("\"hello world\" NTOKENS", &[2.0]);
}

#[test]
fn string_ntokens_multiple_spaces() {
    assert_stack_eq("\"a   b   c\" NTOKENS", &[3.0]);
}

#[test]
fn string_ntokens_empty() {
    assert_stack_eq("\"\" NTOKENS", &[0.0]);
}

#[test]
fn string_ntokens_whitespace_only() {
    assert_stack_eq("\"   \" NTOKENS", &[0.0]);
}

#[test]
fn string_ntokens_single_word() {
    assert_stack_eq("\"hello\" NTOKENS", &[1.0]);
}

#[test]
fn string_nthtoken_first() {
    let values = eval_to_values("\"hello world\" 1 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_nthtoken_second() {
    let values = eval_to_values("\"hello world\" 2 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "world");
}

#[test]
fn string_nthtoken_with_extra_spaces() {
    let values = eval_to_values("\"  a   b   c  \" 2 NTHTOKEN");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "b");
}

#[test]
fn string_nthtoken_out_of_range() {
    // Returns error for out-of-range index
    assert_error("\"hello world\" 5 NTHTOKEN", "not found");
}

#[test]
fn string_nthtokenpos_first() {
    // Returns start and end positions (1-based)
    assert_stack_eq("\"hello world\" 1 NTHTOKENPOS", &[1.0, 5.0]);
}

#[test]
fn string_nthtokenpos_second() {
    assert_stack_eq("\"hello world\" 2 NTHTOKENPOS", &[7.0, 11.0]);
}

#[test]
fn string_nthtokenpos_with_leading_space() {
    assert_stack_eq("\"  hello\" 1 NTHTOKENPOS", &[3.0, 7.0]);
}

#[test]
fn string_nthtokenpos_out_of_range() {
    // Returns error for out-of-range index
    assert_error("\"hello\" 5 NTHTOKENPOS", "not found");
}

// ============================================================================
// String Functions: TRIM, RTRIM
// ============================================================================

#[test]
fn string_trim_both_sides() {
    let values = eval_to_values("\"  hello  \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_leading_only() {
    let values = eval_to_values("\"   hello\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_trailing_only() {
    let values = eval_to_values("\"hello   \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_no_whitespace() {
    let values = eval_to_values("\"hello\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_trim_all_whitespace() {
    let values = eval_to_values("\"   \" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "");
}

#[test]
fn string_trim_tabs_and_newlines() {
    let values = eval_to_values("\"\\t\\nhello\\n\\t\" TRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_rtrim_trailing() {
    let values = eval_to_values("\"hello   \" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn string_rtrim_preserves_leading() {
    let values = eval_to_values("\"   hello   \" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "   hello");
}

#[test]
fn string_rtrim_no_trailing() {
    let values = eval_to_values("\"   hello\" RTRIM");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "   hello");
}

// ============================================================================
// String Functions: STRLENCP (code point length)
// ============================================================================

#[test]
fn string_strlencp_ascii() {
    assert_stack_eq("\"hello\" STRLENCP", &[5.0]);
}

#[test]
fn string_strlencp_unicode() {
    // "café" has 4 code points
    assert_stack_eq("\"café\" STRLENCP", &[4.0]);
}

#[test]
fn string_strlencp_empty() {
    assert_stack_eq("\"\" STRLENCP", &[0.0]);
}

#[test]
fn string_strlencp_emoji() {
    // Single emoji is one code point
    assert_stack_eq("\"😀\" STRLENCP", &[1.0]);
}

// ============================================================================
// String Functions: SREPL (search and replace)
// ============================================================================

#[test]
fn string_srepl_simple() {
    // SREPL: string search replace -> result count
    let values = eval_to_values("\"hello world\" \"world\" \"universe\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello universe");
    assert_eq!(to_int(&values[1]), 1);
}

#[test]
fn string_srepl_multiple() {
    let values = eval_to_values("\"aaa\" \"a\" \"b\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "bbb");
    assert_eq!(to_int(&values[1]), 3);
}

#[test]
fn string_srepl_not_found() {
    let values = eval_to_values("\"hello\" \"xyz\" \"abc\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_int(&values[1]), 0);
}

#[test]
fn string_srepl_empty_search() {
    // Empty search string: no replacement, 0 count
    let values = eval_to_values("\"hello\" \"\" \"x\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "hello");
    assert_eq!(to_int(&values[1]), 0);
}

#[test]
fn string_srepl_to_empty() {
    // Replace with empty string (deletion)
    let values = eval_to_values("\"hello world\" \" \" \"\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "helloworld");
    assert_eq!(to_int(&values[1]), 1);
}

#[test]
fn string_srepl_overlapping() {
    // Non-overlapping replacement
    let values = eval_to_values("\"aaa\" \"aa\" \"b\" SREPL");
    assert_eq!(values.len(), 2);
    assert_eq!(to_string(&values[0]), "ba");
    assert_eq!(to_int(&values[1]), 1);
}
