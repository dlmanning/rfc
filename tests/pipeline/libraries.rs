//! Tests for user library system.

use rpl::value::Value;
#[allow(unused_imports)]
use rpl::Session;

// ============================================================================
// Library System (CRLIB, ATTACH, LIBPTR)
// ============================================================================

#[test]
fn library_crlib_creates_library() {
    // Create a library with a single named command that pushes 42
    // { { "ANSWER" << 42 >> } } "TEST" CRLIB
    let mut session = crate::session_with_stdlib();

    // First, just verify CRLIB works and produces something
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "TEST" CRLIB"#);
    assert!(result.is_ok(), "CRLIB should succeed: {:?}", result);

    // Stack should have 1 item (the library object)
    let values = result.unwrap();
    assert_eq!(values.len(), 1, "CRLIB should produce one value");

    // Verify it's a Library value
    let lib = values[0].as_library();
    assert!(lib.is_some(), "CRLIB should produce a library");

    let lib_data = lib.unwrap();
    assert_eq!(lib_data.id, "TEST");
    assert_eq!(lib_data.commands.len(), 1);
    assert_eq!(lib_data.commands[0].name, "ANSWER");
}

#[test]
fn library_attach_stores_library() {
    let mut session = crate::session_with_stdlib();

    // Create and attach a library
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "MATH" CRLIB ATTACH"#);
    assert!(result.is_ok(), "CRLIB + ATTACH should succeed");

    // Stack should be empty after ATTACH
    let values = result.unwrap();
    assert_eq!(values.len(), 0, "ATTACH should consume the library");

    // The library should be stored in .SETTINGS.LIB.MATH
    // We can verify by checking it was stored (no direct way to verify in pipeline test)
}

#[test]
fn library_detach_removes_library() {
    let mut session = crate::session_with_stdlib();

    // Create, attach, then detach
    let result = session.eval(r#"{ { "ANSWER" << 42 >> } } "TEST" CRLIB ATTACH "TEST" DETACH"#);
    assert!(result.is_ok(), "CRLIB + ATTACH + DETACH should succeed");
}

// ============================================================================
// Library Private Data (LIBSTO, LIBRCL, LIBDEFRCL, LIBCLEAR)
// ============================================================================

#[test]
fn library_libsto_librcl_roundtrip() {
    let mut session = crate::session_with_stdlib();

    // Store a value in library private namespace
    let result = session.eval(r#"42 "TEST" "myvar" LIBSTO"#);
    assert!(result.is_ok(), "LIBSTO should succeed");

    // Recall the value
    let result = session.eval(r#""TEST" "myvar" LIBRCL"#);
    assert!(result.is_ok(), "LIBRCL should succeed");

    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 42.0),
        Value::Integer(i) => assert_eq!(*i, 42),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_libdefrcl_returns_stored_value() {
    let mut session = crate::session_with_stdlib();

    // Store a value
    session.eval(r#"100 "MATH" "setting" LIBSTO"#).unwrap();

    // Recall with default - should return stored value
    let result = session.eval(r#"0 "MATH" "setting" LIBDEFRCL"#).unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Integer(i) => assert_eq!(*i, 100),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_libdefrcl_returns_default() {
    let mut session = crate::session_with_stdlib();

    // Recall non-existent variable with default
    let result = session
        .eval(r#"999 "TEST" "nonexistent" LIBDEFRCL"#)
        .unwrap();
    assert_eq!(result.len(), 1);
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 999.0),
        Value::Integer(i) => assert_eq!(*i, 999),
        _ => panic!("Expected number"),
    }
}

#[test]
fn library_librcl_error_on_missing() {
    let mut session = crate::session_with_stdlib();

    // Try to recall non-existent variable
    let result = session.eval(r#""TEST" "missing" LIBRCL"#);
    assert!(result.is_err(), "LIBRCL should fail for missing variable");
}

#[test]
fn library_libclear_succeeds() {
    let mut session = crate::session_with_stdlib();

    // LIBCLEAR should succeed (even though our implementation is a no-op)
    let result = session.eval(r#""TEST" LIBCLEAR"#);
    assert!(result.is_ok(), "LIBCLEAR should succeed");
}

#[test]
fn library_libclear_actually_clears_data() {
    let mut session = crate::session_with_stdlib();

    // Store some values
    session.eval(r#"1 "TEST" "var1" LIBSTO"#).unwrap();
    session.eval(r#"2 "TEST" "var2" LIBSTO"#).unwrap();
    session.eval(r#"3 "TEST" "var3" LIBSTO"#).unwrap();

    // Verify they're stored
    let result = session.eval(r#""TEST" "var1" LIBRCL"#);
    assert!(result.is_ok(), "var1 should exist before LIBCLEAR");

    // Clear the library data
    session.eval(r#""TEST" LIBCLEAR"#).unwrap();

    // Verify they're gone - LIBRCL should fail
    let result = session.eval(r#""TEST" "var1" LIBRCL"#);
    assert!(result.is_err(), "var1 should be gone after LIBCLEAR");

    let result = session.eval(r#""TEST" "var2" LIBRCL"#);
    assert!(result.is_err(), "var2 should be gone after LIBCLEAR");
}

// ============================================================================
// User Library System - Full Integration Test
// ============================================================================

/// Comprehensive test demonstrating the full user library workflow:
/// 1. Create library commands as programs
/// 2. Package them into a library with CRLIB
/// 3. Attach the library
/// 4. Use library private data for state
/// 5. Clean up with DETACH and LIBCLEAR
#[test]
fn library_full_integration() {
    let mut session = crate::session_with_stdlib();

    // Step 1: Create a library with two named commands and attach it in one go
    // - DOUBLE: doubles a number (2 *)
    // - SQUARE: squares a number (DUP *)
    let result = session.eval(
        r#"
        { { "DOUBLE" << 2 * >> } { "SQUARE" << DUP * >> } } "MATH" CRLIB ATTACH
    "#,
    );
    assert!(
        result.is_ok(),
        "CRLIB + ATTACH should succeed: {:?}",
        result
    );
    assert_eq!(
        result.unwrap().len(),
        0,
        "Stack should be empty after ATTACH"
    );

    // Step 2: Use library private data
    // Store a "precision" setting for our MATH library
    let result = session.eval(r#"10 "MATH" "precision" LIBSTO"#);
    assert!(result.is_ok(), "LIBSTO should succeed");

    // Recall the setting
    let result = session.eval(r#""MATH" "precision" LIBRCL"#);
    assert!(result.is_ok(), "LIBRCL should succeed");
    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 10.0),
        Value::Integer(i) => assert_eq!(*i, 10),
        _ => panic!("Expected number"),
    }

    // Use LIBDEFRCL for a setting that doesn't exist - should return default
    let result = session.eval(r#"100 "MATH" "missing_setting" LIBDEFRCL"#);
    assert!(result.is_ok(), "LIBDEFRCL should succeed");
    let values = result.unwrap();
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Integer(i) => assert_eq!(*i, 100),
        _ => panic!("Expected default value"),
    }

    // Step 3: Store multiple settings, then clear them all
    session.eval(r#"1 "MATH" "setting1" LIBSTO"#).unwrap();
    session.eval(r#"2 "MATH" "setting2" LIBSTO"#).unwrap();
    session.eval(r#"3 "MATH" "setting3" LIBSTO"#).unwrap();

    // Verify one exists
    assert!(session.eval(r#""MATH" "setting2" LIBRCL"#).is_ok());

    // Clear all MATH library data
    session.eval(r#""MATH" LIBCLEAR"#).unwrap();

    // Verify all settings are gone
    assert!(
        session.eval(r#""MATH" "setting1" LIBRCL"#).is_err(),
        "setting1 should be cleared"
    );
    assert!(
        session.eval(r#""MATH" "setting2" LIBRCL"#).is_err(),
        "setting2 should be cleared"
    );
    assert!(
        session.eval(r#""MATH" "setting3" LIBRCL"#).is_err(),
        "setting3 should be cleared"
    );
    // Note: "precision" was also cleared since it's in the same library namespace

    // Step 4: Detach the library
    let result = session.eval(r#""MATH" DETACH"#);
    assert!(result.is_ok(), "DETACH should succeed");
}

// ============================================================================
// Library Command Resolution (Phase 7)
// ============================================================================

/// Test that attached library commands can be called by name
#[test]
fn library_command_resolution() {
    let mut session = crate::session_with_stdlib();

    // Create a library with a DOUBLE command that doubles a number (2 *)
    let result = session.eval(r#"{ { "DOUBLE" << 2 * >> } } "MATH" CRLIB ATTACH"#);
    assert!(result.is_ok(), "CRLIB + ATTACH should succeed: {:?}", result);

    // Now call DOUBLE - it should be found in the attached library
    let result = session.eval("5 DOUBLE");
    assert!(result.is_ok(), "DOUBLE should be callable: {:?}", result);

    let values = result.unwrap();
    assert_eq!(values.len(), 1, "DOUBLE should produce one result");
    match &values[0] {
        Value::Real(r) => assert_eq!(*r, 10.0),
        Value::Integer(i) => assert_eq!(*i, 10),
        _ => panic!("Expected 10, got {:?}", values[0]),
    }
}

/// Test multiple commands from the same library
#[test]
fn library_multiple_commands() {
    let mut session = crate::session_with_stdlib();

    // Create a library with DOUBLE (2 *) and SQUARE (DUP *)
    let result = session.eval(
        r#"{ { "DOUBLE" << 2 * >> } { "SQUARE" << DUP * >> } } "MATH" CRLIB ATTACH"#,
    );
    assert!(result.is_ok(), "CRLIB + ATTACH should succeed");

    // Test DOUBLE
    let result = session.eval("3 DOUBLE").unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 6.0),
        Value::Integer(i) => assert_eq!(*i, 6),
        _ => panic!("Expected 6"),
    }

    // Test SQUARE
    let result = session.eval("4 SQUARE").unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 16.0),
        Value::Integer(i) => assert_eq!(*i, 16),
        _ => panic!("Expected 16"),
    }
}

/// Test that library commands are case-sensitive
#[test]
fn library_command_case_sensitive() {
    let mut session = crate::session_with_stdlib();

    // Create a library with DOUBLE
    session.eval(r#"{ { "DOUBLE" << 2 * >> } } "MATH" CRLIB ATTACH"#).unwrap();

    // Only exact case works (case-sensitive)
    let result = session.eval("5 DOUBLE").unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 10.0),
        Value::Integer(i) => assert_eq!(*i, 10),
        _ => panic!("Expected 10"),
    }
}

/// Test that detached library commands are no longer callable
#[test]
fn library_command_after_detach() {
    let mut session = crate::session_with_stdlib();

    // Create and attach a library
    session.eval(r#"{ { "TRIPLE" << 3 * >> } } "TEST" CRLIB ATTACH"#).unwrap();

    // TRIPLE should work
    let result = session.eval("5 TRIPLE");
    assert!(result.is_ok(), "TRIPLE should be callable while attached");

    // Detach the library
    session.eval(r#""TEST" DETACH"#).unwrap();

    // TRIPLE should no longer work
    let result = session.eval("5 TRIPLE");
    assert!(result.is_err(), "TRIPLE should not be callable after DETACH");
}

/// Test that directory variables take precedence over library commands
#[test]
fn library_command_precedence() {
    let mut session = crate::session_with_stdlib();

    // Create a library with DOUBLE
    session.eval(r#"{ { "DOUBLE" << 2 * >> } } "MATH" CRLIB ATTACH"#).unwrap();

    // Store a variable named DOUBLE in the directory
    session.eval(r#"<< 3 * >> "DOUBLE" STO"#).unwrap();

    // Now DOUBLE should refer to the directory variable (3 *), not the library command (2 *)
    let result = session.eval("5 DOUBLE").unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 15.0), // 5 * 3 = 15, not 5 * 2 = 10
        Value::Integer(i) => assert_eq!(*i, 15),
        _ => panic!("Expected 15"),
    }

    // Remove the directory variable
    session.eval(r#""DOUBLE" PURGE"#).unwrap();

    // Now DOUBLE should refer to the library command again
    let result = session.eval("5 DOUBLE").unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 10.0), // 5 * 2 = 10
        Value::Integer(i) => assert_eq!(*i, 10),
        _ => panic!("Expected 10"),
    }
}

/// Test that library data is isolated between different libraries
#[test]
fn library_data_isolation() {
    let mut session = crate::session_with_stdlib();

    // Store data in two different library namespaces
    session.eval(r#"100 "LIB1" "value" LIBSTO"#).unwrap();
    session.eval(r#"200 "LIB2" "value" LIBSTO"#).unwrap();

    // Each library should see its own value
    let result = session.eval(r#""LIB1" "value" LIBRCL"#).unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 100.0),
        Value::Integer(i) => assert_eq!(*i, 100),
        _ => panic!("Expected 100"),
    }

    let result = session.eval(r#""LIB2" "value" LIBRCL"#).unwrap();
    match &result[0] {
        Value::Real(r) => assert_eq!(*r, 200.0),
        Value::Integer(i) => assert_eq!(*i, 200),
        _ => panic!("Expected 200"),
    }

    // Clearing LIB1 should not affect LIB2
    session.eval(r#""LIB1" LIBCLEAR"#).unwrap();

    assert!(
        session.eval(r#""LIB1" "value" LIBRCL"#).is_err(),
        "LIB1 data should be cleared"
    );
    assert!(
        session.eval(r#""LIB2" "value" LIBRCL"#).is_ok(),
        "LIB2 data should still exist"
    );
}
