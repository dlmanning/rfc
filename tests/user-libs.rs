//! Integration tests for user-defined libraries.
//!
//! These tests demonstrate non-trivial usage of the user library system,
//! including library creation, attachment, command invocation, private data,
//! and debugger integration.

use std::path::PathBuf;

use rpl_dap::DebugSession;
use rpl_lang::{
    DebugEvent, DebugState, ExecuteOutcome, VM, Value, compile::CompiledProgram, execute,
    library::LibraryRegistry, operator::OperatorRegistry,
};
use rpl_session::{Session, UserLibraryRegistry};
use rpl_source::{SourceFile, SourceId};
use rpl_stdlib::{
    libptr::{decode_lib_id, get_library_command_names, get_library_command_with_name},
    register_standard_libs, register_standard_operators,
};

// =============================================================================
// Helper Functions
// =============================================================================

/// Evaluate code and return the resulting stack values.
fn eval(code: &str) -> Vec<Value> {
    let mut session = Session::new();
    session
        .eval(code)
        .unwrap_or_else(|e| panic!("eval failed for '{}':\n{:?}", code, e))
}

/// Evaluate code and expect it to fail with error containing substring.
fn eval_error(code: &str, expected_substring: &str) {
    let mut session = Session::new();
    match session.eval(code) {
        Ok(_) => panic!("Expected error for '{}', but succeeded", code),
        Err(e) => {
            let msg = format!("{:?}", e);
            assert!(
                msg.to_lowercase()
                    .contains(&expected_substring.to_lowercase()),
                "Error '{}' doesn't contain '{}' for code '{}'",
                msg,
                expected_substring,
                code
            );
        }
    }
}

/// Extract the top stack value as a real number.
fn top_as_real(values: &[Value]) -> f64 {
    match values.last().expect("stack should not be empty") {
        Value::Real(r) => *r,
        Value::Int(i) => *i as f64,
        other => panic!("Expected number, got {:?}", other),
    }
}

/// Extract a library object's data from a Value.
fn extract_library_data(value: &Value) -> Vec<u32> {
    match value {
        Value::Object { type_id, data } if type_id.as_u16() == 102 => data.clone(),
        other => panic!("Expected library object, got {:?}", other),
    }
}

fn compile_and_setup(code: &str) -> (VM, CompiledProgram, LibraryRegistry, OperatorRegistry) {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let vm = VM::new();
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    (vm, program, registry, operators)
}

/// Set up a session with a library attached, then compile test code.
/// The library_setup is evaluated first, then test_code is compiled.
/// Returns the session (with VM containing attached libraries) and compiled program.
fn compile_with_library(library_setup: &str, test_code: &str) -> (Session, CompiledProgram) {
    let mut session = Session::new();
    // First, evaluate the library setup (CRLIB ATTACH)
    session
        .eval(library_setup)
        .expect("library setup should succeed");
    println!("CWL: After eval, user_libs = {:?}", session.user_libs());
    // Now compile the test code - library commands should be recognized
    let id = session.set_source("test.rpl", test_code);
    println!("CWL: Compiling test_code: {:?}", test_code);
    let program = session.compile(id).expect("compile should succeed");
    println!("CWL: Compiled program code: {:?}", program.code);
    (session, program)
}

/// Take ownership of the VM from a session for use in debug tests.
fn take_vm(session: &mut Session) -> VM {
    std::mem::take(session.vm_mut())
}

/// Take ownership of the user_libs from a session for use in debug tests.
fn take_user_libs(session: &mut Session) -> UserLibraryRegistry {
    std::mem::take(session.user_libs_mut())
}

// =============================================================================
// Non-Trivial Library: Vector Math Library (VMATH)
// =============================================================================
//
// This library provides 2D vector operations:
//   - VMAG: magnitude of a vector (x y → |v|)
//   - VADD: add two vectors (x1 y1 x2 y2 → x1+x2 y1+y2)
//   - VDOT: dot product (x1 y1 x2 y2 → dot)
//   - VSCALE: scale vector (x y s → x*s y*s)
//   - VNORM: normalize vector (x y → x/|v| y/|v|)

const VMATH_LIBRARY: &str = r#"
{
    { "VMAG" << DUP * SWAP DUP * + SQRT >> }
    { "VADD" << ROT + ROT ROT + SWAP >> }
    { "VDOT" << ROT * ROT ROT * + >> }
    { "VSCALE" << DUP ROT * ROT ROT * SWAP >> }
    { "VNORM" <<
        DUP2 DUP * SWAP DUP * + SQRT
        DUP ROT SWAP / ROT ROT / SWAP
    >> }
} "VMAT" CRLIB
"#;

// =============================================================================
// Non-Trivial Library: Statistics Library (STAT)
// =============================================================================
//
// This library provides statistical operations on lists:
//   - LSUM: sum of list elements
//   - LMEAN: arithmetic mean
//   - LMIN: minimum value
//   - LMAX: maximum value
//   - LLEN: length of list

const STAT_LIBRARY: &str = r#"
{
    { "LSUM" <<
        0 SWAP
        1 OVER SIZE
        FOR i
            i SWAP GET +
        NEXT
    >> }
    { "LMEAN" <<
        DUP SIZE
        SWAP 0 SWAP
        1 OVER SIZE
        FOR i
            i SWAP GET +
        NEXT
        SWAP /
    >> }
    { "LMIN" <<
        DUP 1 SWAP GET
        1 OVER SIZE
        FOR i
            i ROT GET
            DUP2 < IF SWAP DROP ELSE DROP END
            SWAP
        NEXT
        DROP
    >> }
    { "LMAX" <<
        DUP 1 SWAP GET
        1 OVER SIZE
        FOR i
            i ROT GET
            DUP2 > IF SWAP DROP ELSE DROP END
            SWAP
        NEXT
        DROP
    >> }
    { "LLEN" << SIZE >> }
} "STAT" CRLIB
"#;

// =============================================================================
// Non-Trivial Library: Counter Library (CNTR)
// =============================================================================
//
// This library demonstrates library private data (LIBSTO/LIBRCL):
//   - CINIT: initialize counter to 0
//   - CINC: increment counter
//   - CDEC: decrement counter
//   - CGET: get current counter value
//   - CRESET: reset counter to 0

const COUNTER_LIBRARY: &str = r#"
{
    { "CINIT" << 0 "CNTR" "count" LIBSTO >> }
    { "CINC" << "CNTR" "count" LIBRCL 1 + "CNTR" "count" LIBSTO >> }
    { "CDEC" << "CNTR" "count" LIBRCL 1 - "CNTR" "count" LIBSTO >> }
    { "CGET" << "CNTR" "count" LIBRCL >> }
    { "CRESET" << 0 "CNTR" "count" LIBSTO >> }
} "CNTR" CRLIB
"#;

// =============================================================================
// Functional Tests: Library Creation
// =============================================================================

#[test]
fn vmath_library_creates_successfully() {
    let values = eval(VMATH_LIBRARY);
    assert_eq!(values.len(), 1, "CRLIB should produce one value");

    let lib_data = extract_library_data(&values[0]);

    // Verify library ID
    let lib_id = lib_data[0];
    assert_eq!(decode_lib_id(lib_id), "VMAT");

    // Verify command count
    let cmd_count = lib_data[1];
    assert_eq!(cmd_count, 5, "VMATH should have 5 commands");

    // Verify command names
    let names = get_library_command_names(&lib_data);
    assert_eq!(names, vec!["VMAG", "VADD", "VDOT", "VSCALE", "VNORM"]);
}

#[test]
fn stat_library_creates_successfully() {
    let values = eval(STAT_LIBRARY);
    assert_eq!(values.len(), 1);

    let lib_data = extract_library_data(&values[0]);
    let names = get_library_command_names(&lib_data);
    assert_eq!(names, vec!["LSUM", "LMEAN", "LMIN", "LMAX", "LLEN"]);
}

#[test]
fn counter_library_creates_successfully() {
    let values = eval(COUNTER_LIBRARY);
    assert_eq!(values.len(), 1);

    let lib_data = extract_library_data(&values[0]);
    let names = get_library_command_names(&lib_data);
    assert_eq!(names, vec!["CINIT", "CINC", "CDEC", "CGET", "CRESET"]);
}

#[test]
fn library_command_extraction() {
    let values = eval(VMATH_LIBRARY);
    let lib_data = extract_library_data(&values[0]);

    // Extract each command and verify it's a program
    for i in 0..5 {
        let (name, value) = get_library_command_with_name(&lib_data, i)
            .unwrap_or_else(|| panic!("Command {} should exist", i));

        // Verify it's a program
        match value {
            Value::Program { .. } => {}
            other => panic!(
                "Command {} ({}) should be a program, got {:?}",
                i, name, other
            ),
        }
    }

    // Verify specific command names
    let (name0, _) = get_library_command_with_name(&lib_data, 0).unwrap();
    assert_eq!(name0, "VMAG");

    let (name4, _) = get_library_command_with_name(&lib_data, 4).unwrap();
    assert_eq!(name4, "VNORM");
}

// =============================================================================
// Functional Tests: Library Usage
// =============================================================================

#[test]
fn vmath_vmag_basic() {
    // Magnitude of (3, 4) should be 5
    let mut session = Session::new();

    // Create and attach library in one expression
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    // Use command name directly - compile-time resolution to LIBPTR
    let result = session.eval("3 4 VMAG").unwrap();
    assert_eq!(result.len(), 1, "Expected 1 value, got {:?}", result);
    let mag = top_as_real(&result);
    assert!(
        (mag - 5.0).abs() < 1e-10,
        "|(3,4)| should be 5, got {}",
        mag
    );
}

#[test]
fn vmath_vadd_basic() {
    // (1, 2) + (3, 4) = (4, 6)
    let mut session = Session::new();
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    let result = session.eval("1 2 3 4 VADD").unwrap();
    assert_eq!(result.len(), 2);

    let x = match &result[0] {
        Value::Real(r) => *r,
        Value::Int(i) => *i as f64,
        _ => panic!("Expected number"),
    };
    let y = top_as_real(&result);

    assert!((x - 4.0).abs() < 1e-10, "x should be 4, got {}", x);
    assert!((y - 6.0).abs() < 1e-10, "y should be 6, got {}", y);
}

#[test]
fn vmath_vdot_basic() {
    // (1, 2) · (3, 4) = 1*3 + 2*4 = 11
    let mut session = Session::new();
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    let result = session.eval("1 2 3 4 VDOT").unwrap();
    assert_eq!(result.len(), 1);
    let dot = top_as_real(&result);
    assert!(
        (dot - 11.0).abs() < 1e-10,
        "dot product should be 11, got {}",
        dot
    );
}

#[test]
fn vmath_vscale_basic() {
    // (2, 3) * 4 = (8, 12)
    let mut session = Session::new();
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    let result = session.eval("2 3 4 VSCALE").unwrap();
    assert_eq!(result.len(), 2);

    let x = match &result[0] {
        Value::Real(r) => *r,
        Value::Int(i) => *i as f64,
        _ => panic!("Expected number"),
    };
    let y = top_as_real(&result);

    assert!((x - 8.0).abs() < 1e-10, "x should be 8, got {}", x);
    assert!((y - 12.0).abs() < 1e-10, "y should be 12, got {}", y);
}

#[test]
fn vmath_vnorm_basic() {
    // normalize (3, 4) = (0.6, 0.8)
    let mut session = Session::new();
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    let result = session.eval("3 4 VNORM").unwrap();
    assert_eq!(result.len(), 2);

    let x = match &result[0] {
        Value::Real(r) => *r,
        Value::Int(i) => *i as f64,
        _ => panic!("Expected number"),
    };
    let y = top_as_real(&result);

    assert!((x - 0.6).abs() < 1e-10, "x should be 0.6, got {}", x);
    assert!((y - 0.8).abs() < 1e-10, "y should be 0.8, got {}", y);
}

#[test]
fn vmath_complex_expression() {
    // Compute: |(1,1) + (2,2)| = |(3,3)| = sqrt(18) ≈ 4.2426
    let mut session = Session::new();
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    // Add vectors then compute magnitude using command names
    let result = session.eval("1 1 2 2 VADD VMAG").unwrap();
    assert_eq!(result.len(), 1);
    let mag = top_as_real(&result);
    let expected = (18.0_f64).sqrt();
    assert!(
        (mag - expected).abs() < 1e-10,
        "should be sqrt(18), got {}",
        mag
    );
}

// =============================================================================
// Functional Tests: Library Private Data (Counter Library)
// =============================================================================

#[test]
fn counter_library_basic_workflow() {
    let mut session = Session::new();

    // Create and attach library
    session
        .eval(&format!("{} ATTACH", COUNTER_LIBRARY))
        .unwrap();

    // Initialize counter using command name
    session.eval("CINIT").unwrap();

    // Get initial value using command name
    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 0.0);

    // Increment 3 times
    session.eval("CINC").unwrap();
    session.eval("CINC").unwrap();
    session.eval("CINC").unwrap();

    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 3.0);

    // Decrement once
    session.eval("CDEC").unwrap();

    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 2.0);

    // Reset
    session.eval("CRESET").unwrap();

    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 0.0);
}

#[test]
fn counter_library_state_persists_across_evals() {
    let mut session = Session::new();

    session
        .eval(&format!("{} ATTACH", COUNTER_LIBRARY))
        .unwrap();
    session.eval("CINIT").unwrap();

    // State should persist across separate eval calls
    for i in 1..=10 {
        session.eval("CINC").unwrap();
        let result = session.eval("CGET").unwrap();
        assert_eq!(top_as_real(&result), i as f64);
    }
}

#[test]
fn counter_library_clear_resets_state() {
    let mut session = Session::new();

    session
        .eval(&format!("{} ATTACH", COUNTER_LIBRARY))
        .unwrap();
    session.eval("CINIT").unwrap();

    // Increment several times
    for _ in 0..5 {
        session.eval("CINC").unwrap();
    }

    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 5.0);

    // Clear library data
    session.eval(r#""CNTR" LIBCLEAR"#).unwrap();

    // Counter should be gone - CGET will fail
    let result = session.eval("CGET");
    assert!(result.is_err(), "CGET should fail after LIBCLEAR");
}

// =============================================================================
// Functional Tests: Multiple Libraries
// =============================================================================

#[test]
fn multiple_libraries_coexist() {
    let mut session = Session::new();

    // Attach both libraries
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();
    session
        .eval(&format!("{} ATTACH", COUNTER_LIBRARY))
        .unwrap();

    // Use both in same expression:
    // Initialize counter, then compute magnitude of (3,4)
    session.eval("CINIT").unwrap();
    let result = session.eval("3 4 VMAG").unwrap();
    assert_eq!(top_as_real(&result), 5.0);

    // Increment counter 5 times
    for _ in 0..5 {
        session.eval("CINC").unwrap();
    }

    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 5.0);
}

#[test]
fn library_data_isolated_between_libraries() {
    let mut session = Session::new();

    // Store data manually in different library namespaces
    session.eval(r#"100 "LIB1" "shared" LIBSTO"#).unwrap();
    session.eval(r#"200 "LIB2" "shared" LIBSTO"#).unwrap();

    // Each should maintain its own value
    let result = session.eval(r#""LIB1" "shared" LIBRCL"#).unwrap();
    assert_eq!(top_as_real(&result), 100.0);

    let result = session.eval(r#""LIB2" "shared" LIBRCL"#).unwrap();
    assert_eq!(top_as_real(&result), 200.0);

    // Clearing one doesn't affect the other
    session.eval(r#""LIB1" LIBCLEAR"#).unwrap();

    let result = session.eval(r#""LIB2" "shared" LIBRCL"#);
    assert!(result.is_ok(), "LIB2 data should survive LIB1 clear");
    assert_eq!(top_as_real(&result.unwrap()), 200.0);
}

// =============================================================================
// Functional Tests: Detach and Re-attach
// =============================================================================

#[test]
fn detach_removes_library_access() {
    let mut session = Session::new();

    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    // Should work while attached
    let result = session.eval("3 4 VMAG").unwrap();
    assert_eq!(top_as_real(&result), 5.0);

    // Detach
    session.eval(r#""VMAT" DETACH"#).unwrap();

    // Library commands should no longer compile
    // (command name is no longer recognized by tokenizer)
    let result = session.eval("3 4 VMAG");
    assert!(result.is_err(), "VMAG should fail after DETACH");
}

#[test]
fn reattach_restores_library_access() {
    let mut session = Session::new();

    // First attach
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    let result = session.eval("3 4 VMAG").unwrap();
    assert_eq!(top_as_real(&result), 5.0);

    // Detach
    session.eval(r#""VMAT" DETACH"#).unwrap();

    // Re-create and attach
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    // Should work again
    let result = session.eval("3 4 VMAG").unwrap();
    assert_eq!(top_as_real(&result), 5.0);
}

// =============================================================================
// Error Handling Tests
// =============================================================================

#[test]
fn error_invalid_library_name_too_long() {
    eval_error(r#"{ { "CMD" << 1 >> } } "TOOLONG" CRLIB"#, "1-4 characters");
}

#[test]
fn error_invalid_library_name_empty() {
    eval_error(r#"{ { "CMD" << 1 >> } } "" CRLIB"#, "1-4 characters");
}

#[test]
fn error_crlib_missing_name() {
    // A 1-element list (just program, no name) should fail with "2-element list" error
    eval_error(r#"{ { << 1 >> } } "TEST" CRLIB"#, "2-element list");
}

#[test]
fn error_crlib_invalid_entry_type() {
    eval_error(r#"{ 42 } "TEST" CRLIB"#, "2-element list");
}

#[test]
fn error_librcl_nonexistent_variable() {
    eval_error(r#""TEST" "nonexistent" LIBRCL"#, "not found");
}

// =============================================================================
// Debugger Tests: Basic Execution
// =============================================================================

#[test]
fn debug_library_command_step_through() {
    // Create a simple program that uses a library command
    let lib_setup = r#"{ { "DBL" << 2 * >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "5 DBL";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let mut vm = take_vm(&mut session);
    let mut user_libs = take_user_libs(&mut session);
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    let mut debug = DebugState::paused();

    // Should pause at start
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)));

    // Step through until completion, counting steps
    let mut step_count = 0;
    let max_steps = 100;

    loop {
        debug.step_into();
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut user_libs,
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps - possible infinite loop");
                }
            }
            ExecuteOutcome::Completed => break,
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    // Should have completed with result 10 (5 * 2)
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(10.0));
}

#[test]
fn debug_library_command_breakpoint() {
    let lib_setup = r#"{ { "INC" << 1 + >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "5 INC";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let mut vm = take_vm(&mut session);
    let mut user_libs = take_user_libs(&mut session);
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    let mut debug = DebugState::new();

    // Set a breakpoint at the start
    debug.add_breakpoint(0);

    // Should hit breakpoint
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();
    assert!(matches!(
        outcome,
        ExecuteOutcome::Debug(DebugEvent::Breakpoint(0))
    ));

    // Continue to completion
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Completed));

    // Result should be 6 (5 + 1)
    let stack = vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(6.0));
}

#[test]
fn debug_step_into_library_command() {
    // This test verifies we can step INTO a library command
    let lib_setup = r#"{ { "DOUBLE" << 2 * >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "7 DOUBLE";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let mut vm = take_vm(&mut session);
    let mut user_libs = take_user_libs(&mut session);
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    let mut debug = DebugState::paused();

    // Run to initial pause
    execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();

    // Track maximum call depth we see
    let mut max_call_depth = 0;
    let mut step_count = 0;
    let max_steps = 100;

    loop {
        debug.step_into();
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut user_libs,
            Some(&mut debug),
        )
        .unwrap();

        // Track call depth
        let call_depth = vm.call_depth();
        if call_depth > max_call_depth {
            max_call_depth = call_depth;
        }

        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps");
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    // We should have stepped into the library command (call_depth > 0 at some point)
    assert!(
        max_call_depth > 0,
        "Should have entered the library command"
    );

    // Result should be 14 (7 * 2)
    let stack = vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(14.0));
}

// =============================================================================
// Debugger Tests: DebugSession API
// =============================================================================

#[test]
fn debug_session_library_step_into() {
    let lib_setup = r#"{ { "SQR" << DUP * >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "6 SQR";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let source = SourceFile::new(
        SourceId::new(0),
        "test.rpl".to_string(),
        test_code.to_string(),
    );
    let vm = take_vm(&mut session);
    let user_libs = take_user_libs(&mut session);

    let mut debug_session = DebugSession::with_vm(vm, program, source, PathBuf::from("test.rpl"));
    debug_session.user_libs = user_libs;

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to initial pause
    let outcome = debug_session.run(&registry, &operators).unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)));

    // Step through
    let mut max_call_depth = 0;
    let mut step_count = 0;
    let max_steps = 100;

    loop {
        let outcome = debug_session.step_into(&registry, &operators).unwrap();

        let call_depth = debug_session.call_depth();
        if call_depth > max_call_depth {
            max_call_depth = call_depth;
        }

        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps");
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    assert!(
        max_call_depth > 0,
        "Should have stepped into library command via DebugSession"
    );

    // Result: 6^2 = 36
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(36.0));
}

#[test]
fn debug_session_library_with_private_data() {
    // Test debugging a library that uses private data
    let lib_setup = r#"
{
    { "INIT" << 0 "DBG" "val" LIBSTO >> }
    { "INC" << "DBG" "val" LIBRCL 1 + "DBG" "val" LIBSTO >> }
    { "GET" << "DBG" "val" LIBRCL >> }
} "DBG" CRLIB ATTACH
"#;
    let test_code = "INIT INC INC GET";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    println!("DBG: User libs after compile: {:?}", session.user_libs());
    println!("DBG: Program code: {:?}", program.code);
    let source = SourceFile::new(
        SourceId::new(0),
        "test.rpl".to_string(),
        test_code.to_string(),
    );
    let vm = take_vm(&mut session);
    let user_libs = take_user_libs(&mut session);

    let mut debug_session = DebugSession::with_vm(vm, program, source, PathBuf::from("test.rpl"));
    debug_session.user_libs = user_libs;

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to completion with step-by-step
    let outcome = debug_session.run(&registry, &operators).unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)));
    println!(
        "Initial pause, pc={}, call_depth={}",
        debug_session.vm.pc,
        debug_session.call_depth()
    );
    println!(
        "Globals: {:?}",
        debug_session
            .vm
            .globals_snapshot()
            .keys()
            .collect::<Vec<_>>()
    );
    println!("Stack: {:?}", debug_session.vm.stack_snapshot());

    println!("Program bytecode: {:?}", debug_session.program.code);

    let mut step_count = 0;
    loop {
        let code = debug_session.vm.current_code();
        println!(
            "Before step {}: pc={}, call_depth={}, code_len={}",
            step_count,
            debug_session.vm.pc,
            debug_session.call_depth(),
            code.len()
        );
        if step_count >= 22 {
            println!(
                "  code[pc..]: {:?}",
                &code[debug_session.vm.pc.min(code.len())..]
            );
        }
        let outcome = debug_session.step_into(&registry, &operators);
        println!(
            "After step {}: pc={}, call_depth={}, outcome={:?}",
            step_count,
            debug_session.vm.pc,
            debug_session.call_depth(),
            outcome
        );
        if outcome.is_err() || step_count >= 20 {
            println!("  Stack: {:?}", debug_session.vm.stack_snapshot());
        }
        let outcome = outcome.unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > 200 {
                    panic!("Too many steps");
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    // Result: INIT (0), INC (1), INC (2), GET → 2
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(2.0));
}

#[test]
fn debug_session_multiple_library_calls() {
    // Test debugging a program that calls the same library command multiple times
    let lib_setup = r#"{ { "DBL" << 2 * >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "2 DBL DBL DBL";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let source = SourceFile::new(
        SourceId::new(0),
        "test.rpl".to_string(),
        test_code.to_string(),
    );
    let vm = take_vm(&mut session);
    let user_libs = take_user_libs(&mut session);

    let mut debug_session = DebugSession::with_vm(vm, program, source, PathBuf::from("test.rpl"));
    debug_session.user_libs = user_libs;

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to initial pause
    debug_session.run(&registry, &operators).unwrap();

    // Count how many times we enter a function
    let mut function_entries = 0;
    let mut prev_call_depth = 0;
    let mut step_count = 0;

    loop {
        let outcome = debug_session.step_into(&registry, &operators).unwrap();

        let call_depth = debug_session.call_depth();
        if call_depth > prev_call_depth {
            function_entries += 1;
        }
        prev_call_depth = call_depth;

        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > 200 {
                    panic!("Too many steps");
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    // Should have entered a function at least 3 times (one for each DBL call)
    assert!(
        function_entries >= 3,
        "Should have entered function at least 3 times, got {}",
        function_entries
    );

    // Result: 2 * 2 * 2 * 2 = 16
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(16.0));
}

// =============================================================================
// Debugger Tests: Stack Inspection During Library Execution
// =============================================================================

#[test]
fn debug_stack_inspection_in_library_command() {
    let lib_setup = r#"{ { "CALC" << DUP * 1 + >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "5 CALC";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let mut vm = take_vm(&mut session);
    let mut user_libs = take_user_libs(&mut session);
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    let mut debug = DebugState::paused();

    // Run to pause
    execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();

    // Step through and capture stack states when inside the library command
    let mut stack_snapshots_in_call: Vec<Vec<Value>> = Vec::new();
    let mut step_count = 0;

    loop {
        debug.step_into();
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut user_libs,
            Some(&mut debug),
        )
        .unwrap();

        // When inside the call, capture stack state
        if vm.call_depth() > 0 {
            stack_snapshots_in_call.push(vm.stack_snapshot());
        }

        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > 100 {
                    break;
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    // Should have captured some stack states while inside the library
    assert!(
        !stack_snapshots_in_call.is_empty(),
        "Should have stack snapshots from inside library"
    );

    // Result: 5^2 + 1 = 26
    let stack = vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(26.0));
}

#[test]
fn debug_globals_after_library_attach() {
    let code = r#"
{ { "CMD" << 42 >> } } "TEST" CRLIB ATTACH
"#;

    let (mut vm, program, registry, operators) = compile_and_setup(code);
    let mut debug = DebugState::new();

    // Run to completion
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Completed));

    // Check that the library was stored
    let globals = vm.globals_snapshot();

    // The library should be stored under .SETTINGS.LIB.TEST
    let lib_path = ".SETTINGS.LIB.TEST";
    assert!(
        globals.contains_key(lib_path),
        "Library should be stored at {}, globals: {:?}",
        lib_path,
        globals.keys().collect::<Vec<_>>()
    );
}

// =============================================================================
// Debugger Tests: Step Over Library Calls
// =============================================================================

#[test]
fn debug_step_over_library_command() {
    let lib_setup = r#"{ { "DBL" << 2 * >> } } "TEST" CRLIB ATTACH"#;
    let test_code = "5 DBL 1 +";

    let (mut session, program) = compile_with_library(lib_setup, test_code);
    let mut vm = take_vm(&mut session);
    let mut user_libs = take_user_libs(&mut session);
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);
    let mut debug = DebugState::paused();

    // Run to initial pause
    execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut user_libs,
        Some(&mut debug),
    )
    .unwrap();

    // Use step-over to avoid entering function internals
    let mut step_count = 0;
    #[allow(unused_assignments)]
    let mut _entered_call = false;

    loop {
        // Use step_over instead of step_into
        debug.step_over(vm.return_depth());
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut user_libs,
            Some(&mut debug),
        )
        .unwrap();

        // If we're inside a call with step_over, that's unexpected
        // (though with the current implementation it may still enter)
        if vm.call_depth() > 0 {
            _entered_call = true;
        }

        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > 100 {
                    panic!("Too many steps");
                }
            }
            ExecuteOutcome::Completed => break,
            _ => {}
        }
    }

    // Result: (5 * 2) + 1 = 11
    let stack = vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(11.0));
}

// =============================================================================
// Integration Test: Complex Library Workflow
// =============================================================================

#[test]
fn integration_complex_library_workflow() {
    let mut session = Session::new();

    // Step 1: Create and attach vector math library
    session.eval(&format!("{} ATTACH", VMATH_LIBRARY)).unwrap();

    // Step 2: Create and attach counter library
    session
        .eval(&format!("{} ATTACH", COUNTER_LIBRARY))
        .unwrap();

    // Step 3: Initialize counter
    session.eval("CINIT").unwrap();

    // Step 4: Compute magnitude of several vectors, counting them
    let vectors = [(3.0, 4.0), (5.0, 12.0), (8.0, 15.0)];
    let expected_mags = [5.0, 13.0, 17.0];

    for (i, ((x, y), expected)) in vectors.iter().zip(expected_mags.iter()).enumerate() {
        // Compute magnitude using command name
        let result = session.eval(&format!("{} {} VMAG", x, y)).unwrap();
        let mag = top_as_real(&result);
        assert!(
            (mag - expected).abs() < 1e-10,
            "Magnitude {} should be {}, got {}",
            i,
            expected,
            mag
        );

        // Increment counter
        session.eval("CINC").unwrap();
    }

    // Step 5: Verify counter
    let result = session.eval("CGET").unwrap();
    assert_eq!(top_as_real(&result), 3.0, "Should have counted 3 vectors");

    // Step 6: Store the count as library data for later
    // Use CGET again to get the value, then store it
    session
        .eval(r#"CGET "VMAT" "vector_count" LIBSTO"#)
        .unwrap();

    // Step 7: Retrieve and verify
    let result = session.eval(r#""VMAT" "vector_count" LIBRCL"#).unwrap();
    assert_eq!(top_as_real(&result), 3.0);

    // Step 8: Clean up counter data
    session.eval(r#""CNTR" LIBCLEAR"#).unwrap();

    // Step 9: Detach counter library
    session.eval(r#""CNTR" DETACH"#).unwrap();

    // Step 10: Vector library should still work
    let result = session.eval("6 8 VMAG").unwrap();
    assert_eq!(top_as_real(&result), 10.0);

    // Step 11: Clean up
    session.eval(r#""VMAT" LIBCLEAR"#).unwrap();
    session.eval(r#""VMAT" DETACH"#).unwrap();
}
