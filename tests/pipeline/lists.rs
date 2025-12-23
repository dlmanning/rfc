//! Tests for list operations.

use rpl::value::Value;

use super::{eval_to_values, list_to_reals};

// ============================================================================
// Lists
// ============================================================================

#[test]
fn list_empty() {
    let values = eval_to_values("{ }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), Vec::<f64>::new());
}

#[test]
fn list_single_element() {
    let values = eval_to_values("{ 42 }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![42.0]);
}

#[test]
fn list_multiple_elements() {
    let values = eval_to_values("{ 1 2 3 }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_with_expressions() {
    // Elements can be computed
    let values = eval_to_values("{ 1 2 + 3 4 * }");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![3.0, 12.0]); // 1+2=3, 3*4=12
}

#[test]
fn list_size() {
    use super::assert_stack_eq;
    assert_stack_eq("{ 1 2 3 } SIZE", &[3.0]);
}

#[test]
fn list_size_empty() {
    use super::assert_stack_eq;
    assert_stack_eq("{ } SIZE", &[0.0]);
}

#[test]
fn list_head() {
    use super::assert_stack_eq;
    assert_stack_eq("{ 1 2 3 } HEAD", &[1.0]);
}

#[test]
fn list_tail() {
    let values = eval_to_values("{ 1 2 3 } TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![2.0, 3.0]);
}

#[test]
fn list_tail_to_empty() {
    let values = eval_to_values("{ 1 } TAIL");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), Vec::<f64>::new());
}

#[test]
fn list_get() {
    use super::assert_stack_eq;
    // GET uses 1-based indexing
    assert_stack_eq("{ 10 20 30 } 2 GET", &[20.0]);
}

#[test]
fn list_get_first() {
    use super::assert_stack_eq;
    assert_stack_eq("{ 10 20 30 } 1 GET", &[10.0]);
}

#[test]
fn list_get_last() {
    use super::assert_stack_eq;
    assert_stack_eq("{ 10 20 30 } 3 GET", &[30.0]);
}

#[test]
fn list_put() {
    // PUT: list index value -> list'
    let values = eval_to_values("{ 1 2 3 } 2 99 PUT");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 99.0, 3.0]);
}

#[test]
fn list_revlist() {
    let values = eval_to_values("{ 1 2 3 } REVLIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![3.0, 2.0, 1.0]);
}

#[test]
fn list_to_explode() {
    use super::assert_stack_eq;
    // LIST→ explodes list onto stack and pushes count
    assert_stack_eq("{ 10 20 30 } LIST->", &[10.0, 20.0, 30.0, 3.0]);
}

#[test]
fn list_from_stack() {
    // →LIST: n elements count -> list
    let values = eval_to_values("10 20 30 3 ->LIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![10.0, 20.0, 30.0]);
}

#[test]
fn list_roundtrip() {
    // Explode and reconstruct
    let values = eval_to_values("{ 1 2 3 } LIST-> ->LIST");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_nested() {
    // Nested lists
    let values = eval_to_values("{ { 1 2 } { 3 4 } }");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(outer) => {
            assert_eq!(outer.len(), 2);
            assert_eq!(list_to_reals(&outer[0]), vec![1.0, 2.0]);
            assert_eq!(list_to_reals(&outer[1]), vec![3.0, 4.0]);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn list_head_of_nested() {
    // HEAD of nested list returns inner list
    let values = eval_to_values("{ { 1 2 } { 3 4 } } HEAD");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0]);
}

// ============================================================================
// List concatenation with +
// ============================================================================

#[test]
fn list_concat_two_lists() {
    // Two lists concatenated with +
    let values = eval_to_values("{ 1 2 } { 3 4 } +");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn list_append_element() {
    // List + element appends
    let values = eval_to_values("{ 1 2 } 3 +");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_prepend_element() {
    // Element + list prepends
    let values = eval_to_values("1 { 2 3 } +");
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_append_after_numeric_comparison() {
    // This tests that type inference doesn't incorrectly narrow
    // a variable's type based on comparison operations.
    // After `elem 2 <`, elem should still work for list append.
    let code = r#"
        { }
        1
        -> result elem <<
            elem 2 <
            IF THEN
                result elem +
            ELSE
                result
            END
        >>
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    // elem=1 which is < 2, so result + elem = {1}
    assert_eq!(list_to_reals(&values[0]), vec![1.0]);
}

#[test]
fn list_append_after_comparison() {
    // Simpler test: compare elem then append it to a list
    // This isolates the type inference issue where comparison
    // might incorrectly constrain elem to be numeric-only.
    let code = r#"
        << -> elem <<
            elem 5 <    @ compare (might constrain elem to numeric)
            DROP        @ discard comparison result
            { } elem +  @ append elem to empty list
        >> >>
        3 SWAP EVAL
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![3.0]);
}

#[test]
fn list_append_with_rot() {
    // Test ROT followed by + with list - simulates quicksort pattern
    // Stack: { } elem -> after ROT (with 3 items): varies
    let code = r#"
        { } { } { }
        3 -> elem <<
            ROT elem +
        >>
    "#;
    let values = eval_to_values(code);
    // After ROT on {a} {b} {c}: {b} {c} {a}
    // Then elem + appends to {a}: {b} {c} {a, elem}
    assert_eq!(values.len(), 3);
}

#[test]
fn list_append_with_rot_and_rolld() {
    // Full quicksort pattern: ROT elem + 3 ROLLD
    let code = r#"
        { } { } { }
        3 -> elem <<
            ROT elem + 3 ROLLD
        >>
    "#;
    let values = eval_to_values(code);
    // After this, {a} should have elem appended and be back at bottom
    assert_eq!(values.len(), 3);
    assert_eq!(list_to_reals(&values[0]), vec![3.0]);  // first list now has elem
}

#[test]
fn list_append_in_for_loop() {
    // Simulates the quicksort partition pattern with a FOR loop
    let code = r#"
        { } { } { }
        1 3 FOR i
            i -> elem <<
                ROT elem + 3 ROLLD
            >>
        NEXT
    "#;
    let values = eval_to_values(code);
    // All elements should be appended to the first list
    assert_eq!(values.len(), 3);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_append_in_for_loop_with_comparison() {
    // Simulates quicksort partition with comparison before append
    let code = r#"
        { } { } { }
        1 3 FOR i
            i -> elem <<
                elem 2 <
                IF THEN
                    ROT elem + 3 ROLLD
                END
            >>
        NEXT
    "#;
    let values = eval_to_values(code);
    // Only elem < 2 (i.e., 1) should be appended
    assert_eq!(values.len(), 3);
    assert_eq!(list_to_reals(&values[0]), vec![1.0]);
}

#[test]
fn list_append_elem_from_list_get() {
    // Most similar to quicksort: elem comes from list GET
    // This is where type inference might incorrectly narrow elem
    let code = r#"
        { 3 1 4 } -> lst <<
            { } { } { }
            1 lst SIZE FOR i
                lst i GET -> elem <<
                    elem 2 <
                    IF THEN
                        ROT elem + 3 ROLLD
                    END
                >>
            NEXT
        >>
    "#;
    let values = eval_to_values(code);
    // Only elem < 2 (i.e., 1) should be appended to first list
    assert_eq!(values.len(), 3);
    assert_eq!(list_to_reals(&values[0]), vec![1.0]);
}

#[test]
fn list_partition_function() {
    // Full partition function like in quicksort
    let code = r#"
        << -> lst piv <<
            { } { } { }
            1 lst SIZE FOR i
                lst i GET -> elem <<
                    elem piv <
                    IF THEN
                        ROT elem + 3 ROLLD
                    ELSE
                        elem piv ==
                        IF THEN
                            SWAP elem + SWAP
                        ELSE
                            elem +
                        END
                    END
                >>
            NEXT
        >> >>
        "partition3" STO

        { 3 1 4 1 5 } 2 partition3
    "#;
    let values = eval_to_values(code);
    // Should have 3 lists: less, equal, greater
    assert_eq!(values.len(), 3);
    // less: elements < 2 -> {1, 1}
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 1.0]);
}

#[test]
fn quicksort_full() {
    // Full quicksort implementation
    let code = r#"
        @ Partition
        << -> lst piv <<
            { } { } { }
            1 lst SIZE FOR i
                lst i GET -> elem <<
                    elem piv <
                    IF THEN
                        ROT elem + 3 ROLLD
                    ELSE
                        elem piv ==
                        IF THEN
                            SWAP elem + SWAP
                        ELSE
                            elem +
                        END
                    END
                >>
            NEXT
        >> >>
        "partition3" STO

        @ Quicksort
        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                lst 1 GET -> piv <<
                    lst piv partition3
                    -> less equal greater <<
                        less quicksort
                        equal +
                        greater quicksort +
                    >>
                >>
            END
        >> >>
        "quicksort" STO

        { 3 1 4 1 5 } quicksort
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 1.0, 3.0, 4.0, 5.0]);
}

#[test]
fn list_concat_with_locals_from_partition() {
    // Simulates the quicksort pattern: partition returns 3 lists,
    // bind them to locals, then recursive call + concatenation
    let code = r#"
        @ Simulated partition: just returns input split three ways
        << -> lst <<
            { } lst { }
        >> >>
        "partition" STO

        @ Recursive function with locals bound from partition result
        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                lst partition
                -> less equal greater <<
                    less process
                    equal +
                    greater process +
                >>
            END
        >> >>
        "process" STO

        { 1 2 } process
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

#[test]
fn list_concat_locals_simple() {
    // Even simpler: bind 2 lists to locals, then concatenate
    let code = r#"
        { 1 } { 2 }
        -> a b <<
            a b +
        >>
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0]);
}

#[test]
fn list_concat_locals_with_call() {
    // Bind lists to locals, call a function, then concatenate
    let code = r#"
        << -> x << x >> >>
        "id" STO

        { 1 } { 2 }
        -> a b <<
            a id b +
        >>
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0]);
}

#[test]
fn list_concat_locals_with_if_else() {
    // Add IF/THEN/ELSE to see if that affects type inference
    let code = r#"
        << -> x << x >> >>
        "id" STO

        { 1 } { 2 }
        -> a b <<
            1
            IF THEN
                a id b +
            ELSE
                b
            END
        >>
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0]);
}

#[test]
fn list_concat_recursive_with_if() {
    // Recursive function with IF/THEN/ELSE and list concatenation
    let code = r#"
        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                { 1 } { 2 }
                -> a b <<
                    a test b +
                >>
            END
        >> >>
        "test" STO

        { 1 2 3 } test
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

#[test]
fn list_concat_from_function_returning_3_lists() {
    // Function returns 3 lists that are bound to locals, then concatenated
    // Local bindings consume all bound values - c is used in body, not left on stack
    let code = r#"
        << { 1 } { 2 } { 3 } >>
        "three_lists" STO

        three_lists
        -> a b c <<
            a b + c +
        >>
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
    assert_eq!(list_to_reals(&values[0]), vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_concat_from_function_returning_3_lists_recursive() {
    // Same but with recursion
    let code = r#"
        << { 1 } { 2 } { 3 } >>
        "three_lists" STO

        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                three_lists
                -> a b c <<
                    a rec b + c rec +
                >>
            END
        >> >>
        "rec" STO

        { 1 2 } rec
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

#[test]
fn list_concat_after_recursive_call_minimal() {
    // Minimal case: recursive call result + local list
    // The issue: after `a rec`, the result is Unknown (recursive call)
    // Then `b +` should use CallLib, not I64Add
    let code = r#"
        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                { 1 } { 2 }
                -> a b <<
                    a rec b +
                >>
            END
        >> >>
        "rec" STO

        { 1 2 } rec
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

#[test]
fn list_concat_two_plus_operations() {
    // Two + operations in sequence: `a rec b + c rec +`
    // This is the pattern that fails in quicksort
    let code = r#"
        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                { 1 } { 2 } { 3 }
                -> a b c <<
                    a rec b + c rec +
                >>
            END
        >> >>
        "rec" STO

        { 1 2 } rec
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

#[test]
fn list_concat_two_plus_from_function() {
    // Same but lists come from a function call
    // THIS IS THE FAILING PATTERN
    let code = r#"
        << { 1 } { 2 } { 3 } >>
        "make_lists" STO

        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                make_lists
                -> a b c <<
                    a rec b + c rec +
                >>
            END
        >> >>
        "rec" STO

        { 1 2 } rec
    "#;
    let values = eval_to_values(code);
    assert_eq!(values.len(), 1);
}

/// Debug test to examine node_stacks for the failing case.
#[test]
fn debug_list_concat_types() {
    use rpl::analysis::analyze;
    use rpl::core::Interner;
    use rpl::parse::parse;
    use rpl::registry::InterfaceRegistry;

    let code = r#"
        << { 1 } { 2 } { 3 } >>
        "make_lists" STO

        << -> lst <<
            lst SIZE 1 <=
            IF THEN
                lst
            ELSE
                make_lists
                -> a b c <<
                    a rec b + c rec +
                >>
            END
        >> >>
        "rec" STO

        { 1 2 } rec
    "#;

    // Set up registries
    let mut interfaces = InterfaceRegistry::new();
    rpl_stdlib::register_interfaces(&mut interfaces);
    let mut interner = Interner::new();

    // Parse the code
    let nodes = parse(code, &interfaces, &mut interner).expect("parse failed");

    // Run analysis directly to see what happens
    let result = analyze(&nodes, &interfaces, &interner, &rpl::analysis::Context::empty());

    // Print all node_stacks with their types and what's at those positions
    println!("=== Node Stacks ({} entries) ===", result.node_stacks.len());
    let mut stacks: Vec<_> = result.node_stacks.iter().collect();
    stacks.sort_by_key(|(span, _)| span.start().offset());
    for (span, snapshot) in stacks {
        let start = span.start().offset() as usize;
        let end = span.end().offset() as usize;
        let snippet = if end <= code.len() {
            &code[start..end.min(start + 20)]
        } else {
            "???"
        };
        println!("Span {}..{}: tos={:?}, nos={:?}, src='{}'",
                 start, end,
                 snapshot.tos, snapshot.nos, snippet);
    }

    // Print definitions with their types
    println!("\n=== Definitions ===");
    for def in result.symbols.definitions() {
        println!("  {} (local={:?}): {:?}",
                 def.name, def.local_index, def.value_type);
    }

    // Find the + operator positions
    println!("\n=== + positions ===");
    for (i, c) in code.char_indices() {
        if c == '+' {
            println!("  '+' at byte {}", i);
        }
    }

    // What TypeId values mean
    println!("\n=== TypeId meanings ===");
    println!("TypeId(0) = BINT (integer)");
    println!("TypeId(1) = REAL");
    println!("TypeId(2) = STRING");
    println!("TypeId(5) = LIST");
    println!("TypeId(12) = BINT");

    // Check if any + has integer type - that's the bug!
    for (span, snapshot) in &result.node_stacks {
        let start = span.start().offset() as usize;
        let end = span.end().offset() as usize;
        if end <= code.len() && &code[start..end] == "+" {
            if snapshot.tos.is_integer() || snapshot.nos.is_integer() {
                println!("\n*** BUG FOUND: + at {}..{} has integer type! ***", start, end);
                println!("    tos={:?}, nos={:?}", snapshot.tos, snapshot.nos);
            }
        }
    }
}
