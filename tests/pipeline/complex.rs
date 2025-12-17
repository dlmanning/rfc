//! Tests for complex numbers.

use super::eval_to_values;

// ============================================================================
// Complex Numbers
// ============================================================================

#[test]
#[ignore = "HP-style complex literal syntax not yet implemented"]
fn complex_hp_style_literal() {
    // (3, 2) should create a complex number
    let values = eval_to_values("(3, 2)");
    assert_eq!(values.len(), 1);
    // rpl doesn't have Value::Object - complex numbers not yet supported
    panic!("Complex numbers not supported: {:?}", values[0]);
}
