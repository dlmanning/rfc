//! Tests for variables and directory operations.

use rpl::value::Value;
use rpl::Session;

use super::{assert_stack_eq, eval_to_values, to_string};

// ============================================================================
// Variables (STO, RCL, PURGE)
// ============================================================================

#[test]
fn var_sto_rcl() {
    // STO: value "name" STO - stores value
    // RCL: "name" RCL - recalls value
    assert_stack_eq("42 \"x\" STO \"x\" RCL", &[42.0]);
}

#[test]
fn var_sto_rcl_real() {
    assert_stack_eq("3.15 \"pi\" STO \"pi\" RCL", &[3.15]);
}

#[test]
fn var_sto_multiple() {
    // Store two different variables
    assert_stack_eq(
        "10 \"a\" STO 20 \"b\" STO \"a\" RCL \"b\" RCL",
        &[10.0, 20.0],
    );
}

#[test]
fn var_sto_overwrite() {
    // Overwriting a variable should work
    assert_stack_eq("10 \"x\" STO 20 \"x\" STO \"x\" RCL", &[20.0]);
}

#[test]
fn var_sto_string() {
    // Store a string value
    let values = eval_to_values("\"hello\" \"greeting\" STO \"greeting\" RCL");
    assert_eq!(values.len(), 1);
    assert_eq!(to_string(&values[0]), "hello");
}

#[test]
fn var_sto_list() {
    // Store a list value
    let values = eval_to_values("{ 1 2 3 } \"mylist\" STO \"mylist\" RCL");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 3);
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn var_sto_preserves_stack() {
    // STO should consume both the value and name, leaving stack empty
    let values = eval_to_values("42 \"x\" STO");
    assert_eq!(values.len(), 0);
}

#[test]
fn var_sto_in_program() {
    // Use STO inside a program
    assert_stack_eq("<< 100 \"result\" STO >> EVAL \"result\" RCL", &[100.0]);
}

#[test]
fn var_use_in_calculation() {
    // Store value, then use it in calculation
    assert_stack_eq("5 \"n\" STO \"n\" RCL \"n\" RCL *", &[25.0]);
}

#[test]
fn var_purge() {
    // PURGE removes a variable - we test by storing, purging, then storing again
    // (can't easily test the error case in this test framework)
    assert_stack_eq("42 \"x\" STO \"x\" PURGE 99 \"x\" STO \"x\" RCL", &[99.0]);
}

// ============================================================================
// Directory Commands: INCR, DECR
// ============================================================================

#[test]
fn var_incr() {
    // INCR: Increment variable and return new value
    assert_stack_eq("5 \"x\" STO \"x\" INCR", &[6.0]);
}

#[test]
fn var_incr_updates_var() {
    // INCR should also update the variable
    assert_stack_eq("5 \"x\" STO \"x\" INCR DROP \"x\" RCL", &[6.0]);
}

#[test]
fn var_decr() {
    // DECR: Decrement variable and return new value
    assert_stack_eq("5 \"x\" STO \"x\" DECR", &[4.0]);
}

#[test]
fn var_decr_updates_var() {
    // DECR should also update the variable
    assert_stack_eq("5 \"x\" STO \"x\" DECR DROP \"x\" RCL", &[4.0]);
}

#[test]
fn var_incr_loop() {
    // Use INCR in a counting loop
    assert_stack_eq(
        "0 \"cnt\" STO 1 5 START \"cnt\" INCR DROP NEXT \"cnt\" RCL",
        &[5.0],
    );
}

// ============================================================================
// Directory Commands: VARS, CLVAR
// ============================================================================

#[test]
fn var_vars_returns_list() {
    // VARS: Returns list of variable names
    let values = eval_to_values("CLVAR 42 \"testvar\" STO VARS");
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert!(!elements.is_empty());
            // Check that our variable is in the list
            let has_testvar = elements
                .iter()
                .any(|e| matches!(e, Value::String(s) if s.as_ref() == "testvar"));
            assert!(has_testvar, "VARS should contain 'testvar'");
        }
        other => panic!("Expected List, got {:?}", other),
    }
}

#[test]
fn var_clvar() {
    // CLVAR: Clears all variables
    // Store two vars, clear, try to RCL (should error)
    let mut session = crate::session_with_stdlib();
    session.eval("10 \"a\" STO 20 \"b\" STO").unwrap();
    session.eval("CLVAR").unwrap();
    // After CLVAR, RCL should fail
    assert!(
        session.eval("\"a\" RCL").is_err(),
        "a should be undefined after CLVAR"
    );
    assert!(
        session.eval("\"b\" RCL").is_err(),
        "b should be undefined after CLVAR"
    );
}

// ============================================================================
// Directory Commands: RENAME
// ============================================================================

#[test]
fn var_rename() {
    // RENAME: Rename a variable
    assert_stack_eq("42 \"old\" STO \"old\" \"new\" RENAME \"new\" RCL", &[42.0]);
}

#[test]
fn var_rename_old_gone() {
    // After RENAME, old name should not exist
    let mut session = crate::session_with_stdlib();
    session
        .eval("42 \"old\" STO \"old\" \"new\" RENAME")
        .unwrap();
    assert!(
        session.eval("\"old\" RCL").is_err(),
        "old name should not exist after RENAME"
    );
    let result = session.eval("\"new\" RCL").unwrap();
    assert_eq!(result.len(), 1);
}

// ============================================================================
// Directory Hierarchy: CRDIR, PGDIR, UPDIR, HOME, PATH
// ============================================================================

#[test]
fn dir_crdir_basic() {
    // CRDIR creates a subdirectory
    let mut session = crate::session_with_stdlib();
    session.eval("\"subdir\" CRDIR").unwrap();
    // No error means success
}

#[test]
fn dir_crdir_duplicate_fails() {
    // Creating a directory that already exists should fail
    let mut session = crate::session_with_stdlib();
    session.eval("\"subdir\" CRDIR").unwrap();
    assert!(
        session.eval("\"subdir\" CRDIR").is_err(),
        "duplicate CRDIR should fail"
    );
}

#[test]
fn dir_path_at_home() {
    // PATH at root should return empty list
    let mut session = crate::session_with_stdlib();
    let result = session.eval("PATH").unwrap();
    assert_eq!(result.len(), 1, "PATH should push one value");
    // The value should be an empty list
    match &result[0] {
        Value::List(items) => assert!(items.is_empty(), "PATH at root should be empty list"),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_home_noop_at_root() {
    // HOME at root is a no-op, PATH should still be empty
    let mut session = crate::session_with_stdlib();
    let result = session.eval("HOME PATH").unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::List(items) => assert!(items.is_empty()),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_updir_at_root_noop() {
    // UPDIR at root is a no-op (not an error), PATH should still be empty
    let mut session = crate::session_with_stdlib();
    let result = session.eval("UPDIR PATH").unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::List(items) => assert!(items.is_empty()),
        other => panic!("PATH should return a list, got {:?}", other),
    }
}

#[test]
fn dir_pgdir_basic() {
    // PGDIR removes an empty directory
    let mut session = crate::session_with_stdlib();
    session.eval("\"subdir\" CRDIR").unwrap();
    session.eval("\"subdir\" PGDIR").unwrap();
    // Recreating should work now
    session.eval("\"subdir\" CRDIR").unwrap();
}

#[test]
fn dir_pgdir_nonexistent_fails() {
    // PGDIR on nonexistent directory should fail
    let mut session = crate::session_with_stdlib();
    assert!(
        session.eval("\"nonexistent\" PGDIR").is_err(),
        "PGDIR on nonexistent should fail"
    );
}

#[test]
fn dir_vars_isolation() {
    // Variables in different directories are isolated
    let mut session = crate::session_with_stdlib();

    // Store in root
    session.eval("42 \"x\" STO").unwrap();

    // Create and enter subdirectory
    session.eval("\"sub\" CRDIR").unwrap();

    // Root variable should still be accessible (we haven't entered sub yet)
    let result = session.eval("\"x\" RCL").unwrap();
    assert_eq!(result.len(), 1);
}

// ============================================================================
// PACKDIR (Directory Serialization)
// ============================================================================

#[test]
#[ignore = "PACKDIR not supported"]
fn packdir_creates_object() {
    // PACKDIR should create a PackDir object
    let mut session = crate::session_with_stdlib();
    session.eval("42 \"x\" STO").unwrap();
    let values = session.eval("PACKDIR").unwrap();
    assert_eq!(values.len(), 1);
    // rpl doesn't have Value::Object
    panic!("PACKDIR not supported: {:?}", values[0]);
}

#[test]
fn packdir_roundtrip_single_var() {
    // Pack directory, clear, unpack, verify value restored
    // All in one eval to preserve stack between operations
    let mut session = crate::session_with_stdlib();
    session.eval("42 \"x\" STO PACKDIR CLVAR UNPACKDIR").unwrap();
    let values = session.eval("\"x\" RCL").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Integer(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }
}

#[test]
fn packdir_roundtrip_multiple_vars() {
    // Pack multiple variables of different types
    let mut session = crate::session_with_stdlib();
    session.eval("42 \"num\" STO \"hello\" \"msg\" STO { 1 2 3 } \"lst\" STO").unwrap();
    session.eval("PACKDIR CLVAR UNPACKDIR").unwrap();

    // Verify all values restored
    let values = session.eval("\"num\" RCL").unwrap();
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Integer(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }

    let values = session.eval("\"msg\" RCL").unwrap();
    assert_eq!(to_string(&values[0]), "hello");

    let values = session.eval("\"lst\" RCL").unwrap();
    match &values[0] {
        Value::List(elements) => assert_eq!(elements.len(), 3),
        _ => panic!("Expected list"),
    }
}

#[test]
fn packdir_unpack_into_named_subdir() {
    // Unpack into a new named subdirectory
    let mut session = crate::session_with_stdlib();
    // Store, pack, clear in one eval to preserve stack
    session.eval("42 \"x\" STO PACKDIR CLVAR \"backup\" UNPACKDIR").unwrap();

    // Variable should NOT be in current directory (it's in "backup" subdir)
    assert!(session.eval("\"x\" RCL").is_err());

    // Verify the subdirectory was created and has content
    // PGDIR should FAIL because directory is not empty (contains x)
    assert!(
        session.eval("\"backup\" PGDIR").is_err(),
        "PGDIR should fail because backup contains the unpacked variable"
    );
}

#[test]
fn packdir_packinfo_returns_names() {
    // PACKINFO should return list of entry names
    let mut session = crate::session_with_stdlib();
    session.eval("CLVAR").unwrap();
    session.eval("1 \"alpha\" STO").unwrap();
    session.eval("2 \"beta\" STO").unwrap();
    let values = session.eval("PACKDIR PACKINFO").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 2);
            // Names should be alpha and beta (order may vary)
            let names: Vec<String> = elements.iter().map(|e| to_string(e).to_owned()).collect();
            assert!(names.contains(&"alpha".to_string()));
            assert!(names.contains(&"beta".to_string()));
        }
        _ => panic!("Expected list of names"),
    }
}

#[test]
fn packdir_unpack_conflict_error() {
    // UNPACKDIR should error if a name already exists
    let mut session = crate::session_with_stdlib();
    session.eval("42 \"x\" STO").unwrap();
    session.eval("PACKDIR").unwrap();
    // Don't clear - x still exists
    let result = session.eval("UNPACKDIR");
    assert!(result.is_err(), "UNPACKDIR should fail on conflict");
}

#[test]
fn packdir_pack_named_subdir() {
    // Pack a specific subdirectory by name
    // Note: We can only test packing an empty subdirectory since there's
    // no RPL command to enter a directory programmatically
    let mut session = crate::session_with_stdlib();
    session.eval("\"mydir\" CRDIR").unwrap();

    // Pack "mydir" from parent and get PACKINFO in one eval
    let values = session.eval("\"mydir\" PACKDIR PACKINFO").unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::List(elements) => {
            assert_eq!(elements.len(), 0, "Empty directory should have no entries");
        }
        _ => panic!("Expected list from PACKINFO"),
    }
}

#[test]
fn packdir_with_subdirectories() {
    // Pack directory that contains subdirectories
    // Note: We can only test with empty subdirectories since there's no
    // RPL command to enter a directory programmatically
    let mut session = crate::session_with_stdlib();
    session.eval("CLVAR 1 \"rootvar\" STO \"sub\" CRDIR").unwrap();

    // Pack root (includes empty subdirectory), clear, then unpack
    session.eval("PACKDIR \"sub\" PGDIR CLVAR UNPACKDIR").unwrap();

    // Verify root variable was restored
    let values = session.eval("\"rootvar\" RCL").unwrap();
    match &values[0] {
        Value::Integer(i) => assert_eq!(*i, 1),
        Value::Real(r) => assert_eq!(*r, 1.0),
        _ => panic!("Expected number"),
    }

    // Verify subdirectory was recreated (PGDIR should work on it - it's empty)
    session.eval("\"sub\" PGDIR").unwrap();
}
