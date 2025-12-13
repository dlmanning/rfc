//! Tests for complex numbers.

use rpl_lang::Value;

use super::eval_to_values;

// ============================================================================
// Complex Numbers
// ============================================================================

#[test]
fn complex_hp_style_literal() {
    // (3, 2) should create a complex number
    let values = eval_to_values("(3, 2)");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Object { type_id, data } => {
            assert_eq!(*type_id, rpl_core::TypeId::COMPLEX);
            // 4 words: re_hi, re_lo, im_hi, im_lo
            assert_eq!(data.len(), 4);
        }
        _ => panic!("Expected complex Object, got {:?}", values[0]),
    }
}
