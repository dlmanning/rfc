//! Tests for list operations.

use super::{eval_to_values, list_to_reals};
use rpl_lang::Value;

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
