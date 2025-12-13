//! Debug functionality integration tests.

use std::collections::HashMap;

use rpl_core::{Interner, Span, TypeId, make_prolog};
use rpl_lang::{
    DebugEvent, DebugState, ExecuteOutcome, VM, Value, compile::CompiledProgram, execute,
    library::LibraryRegistry, operator::OperatorRegistry,
};
use rpl_session::{Session, UserLibraryRegistry};
use rpl_stdlib::{register_standard_libs, register_standard_operators};

fn make_program(code: Vec<u32>) -> CompiledProgram {
    let len = code.len();
    CompiledProgram {
        code,
        spans: vec![Span::DUMMY; len],
        type_map: HashMap::new(),
        interner: Interner::new(),
    }
}

fn make_real_prolog(value: f64) -> Vec<u32> {
    let bits = value.to_bits();
    vec![
        make_prolog(TypeId::REAL.as_u16(), 2),
        (bits >> 32) as u32,
        bits as u32,
    ]
}

#[test]
fn execute_without_debug() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(42.0);
    code.extend(make_real_prolog(3.15));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        None,
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);
    assert_eq!(vm.depth(), 2);
}

#[test]
fn breakpoint_triggers() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));
    code.extend(make_real_prolog(3.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(3); // Break before second number

    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(3)));
    assert_eq!(vm.depth(), 1); // Only first number pushed
    assert_eq!(vm.pc, 3); // Stopped at breakpoint
}

#[test]
fn continue_after_breakpoint() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(3);

    // Hit breakpoint
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(3)));
    assert_eq!(vm.depth(), 1);

    // Continue
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);
    assert_eq!(vm.depth(), 2);
}

#[test]
fn step_into() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));
    code.extend(make_real_prolog(3.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::paused();

    // Should pause immediately
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused));
    assert_eq!(vm.pc, 0);
    assert_eq!(vm.depth(), 0);

    // Step into first instruction
    debug.step_into();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Step));
    assert_eq!(vm.depth(), 1); // First number pushed

    // Step into second instruction
    debug.step_into();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Step));
    assert_eq!(vm.depth(), 2); // Second number pushed

    // Step to completion
    debug.step_into();
    let _outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    // Either Step (if there was one more) or Completed
    assert_eq!(vm.depth(), 3);
}

#[test]
fn stack_inspection() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(3); // After first number

    execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();

    // Inspect stack
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(1.0));
}

#[test]
fn debug_with_session() {
    let mut session = Session::new();
    let id = session.set_source("test.rpl", "1 2 3 +");

    let program = session.compile(id).expect("compile should succeed");

    let mut vm = VM::new();
    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(0); // Break at very start

    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(0)));

    // Continue to completion
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Stack should have: 1, 5 (result of 2+3)
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 2);
}

#[test]
fn multiple_breakpoints() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));
    code.extend(make_real_prolog(3.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(0);
    debug.add_breakpoint(3);
    debug.add_breakpoint(6);

    // Hit first breakpoint
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(0)));
    assert_eq!(vm.depth(), 0);

    // Continue to second
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(3)));
    assert_eq!(vm.depth(), 1);

    // Continue to third
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(6)));
    assert_eq!(vm.depth(), 2);

    // Continue to end
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);
    assert_eq!(vm.depth(), 3);
}

#[test]
fn remove_breakpoint() {
    let mut vm = VM::new();
    let mut code = make_real_prolog(1.0);
    code.extend(make_real_prolog(2.0));

    let program = make_program(code);
    let registry = LibraryRegistry::new();
    let operators = OperatorRegistry::new();

    let mut debug = DebugState::new();
    debug.add_breakpoint(0);
    debug.add_breakpoint(3);

    // Hit first breakpoint
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(0)));

    // Remove second breakpoint and continue
    debug.remove_breakpoint(3);
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    // Should complete without hitting removed breakpoint
    assert_eq!(outcome, ExecuteOutcome::Completed);
}

// =============================================================================
// Fixture-based tests
// =============================================================================

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

#[test]
fn fixture_factorial_breakpoint_at_start() {
    let code = include_str!("programs/factorial.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::new();
    debug.add_breakpoint(0);

    // Should hit breakpoint at start
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(0)));

    // Continue to completion
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Stack should have 120 (5!)
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(120.0));
}

#[test]
fn fixture_factorial_step_through_start() {
    let code = include_str!("programs/factorial.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::paused();

    // Should pause at start
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused));
    assert_eq!(vm.pc, 0);

    // Step a few times
    for _ in 0..5 {
        debug.step_into();
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {}
            ExecuteOutcome::Completed => break,
            other => panic!("unexpected outcome: {:?}", other),
        }
    }

    // Now continue to completion
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Should still get correct result
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(120.0));
}

#[test]
fn fixture_fibonacci_run_to_completion() {
    let code = include_str!("programs/fibonacci.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::new();

    // Run without any breakpoints
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Stack should have 55 (fib(10))
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(55.0));
}

#[test]
fn fixture_gcd_breakpoint_and_inspect() {
    let code = include_str!("programs/gcd.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::new();
    debug.add_breakpoint(0);

    // Hit breakpoint
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(0)));

    // Stack should be empty at the very start
    let stack = vm.stack_snapshot();
    assert!(stack.is_empty());

    // Continue
    debug.continue_running();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Stack should have 6 (gcd(48, 18))
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(6.0));
}

#[test]
fn fixture_factorial_globals_inspection() {
    let code = include_str!("programs/factorial.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    // Run to completion
    let mut debug = DebugState::new();
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Completed);

    // Check that 'fact' was stored in globals
    let globals = vm.globals_snapshot();
    assert!(
        globals.contains_key("fact"),
        "globals should contain 'fact' function"
    );
}

#[test]
fn fixture_gcd_multiple_breakpoints() {
    let code = include_str!("programs/gcd.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::new();
    // Add breakpoints at several positions
    debug.add_breakpoint(0);
    debug.add_breakpoint(5);
    debug.add_breakpoint(10);

    let mut breakpoint_count = 0;

    loop {
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Breakpoint(_)) => {
                breakpoint_count += 1;
                debug.continue_running();
            }
            ExecuteOutcome::Completed => break,
            other => panic!("unexpected outcome: {:?}", other),
        }
    }

    // Should have hit at least the first breakpoint
    assert!(breakpoint_count >= 1, "should hit at least one breakpoint");

    // Result should still be correct
    let stack = vm.stack_snapshot();
    assert_eq!(stack.len(), 1);
    assert_eq!(stack[0].as_real(), Some(6.0));
}

#[test]
fn fixture_fibonacci_step_over_call() {
    let code = include_str!("programs/fibonacci.rpl");
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::paused();

    // Pause at start
    let outcome = execute(
        &mut vm,
        &program,
        &registry,
        &operators,
        &mut UserLibraryRegistry::new(),
        Some(&mut debug),
    )
    .unwrap();
    assert_eq!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused));

    // Use step-over for several steps
    // Step-over is tricky with recursive programs because we may enter calls
    // This test just verifies step-over works without crashing
    for i in 0..5 {
        debug.step_over(vm.return_depth());
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                // Step-over completed, continue
            }
            ExecuteOutcome::Completed => {
                // Completed early (small program) - that's fine
                return;
            }
            other => panic!("unexpected outcome on step {}: {:?}", i, other),
        }
    }

    // Continue to completion
    debug.continue_running();
    loop {
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(_) => debug.continue_running(),
            ExecuteOutcome::Completed => break,
        }
    }

    // Should get correct result: fib(10) = 55
    let stack = vm.stack_snapshot();
    assert!(!stack.is_empty(), "stack should not be empty");
    // The last value on stack should be 55.0
    assert_eq!(stack.last().unwrap().as_real(), Some(55.0));
}

/// Test that we can step INTO a stored function when it's called.
/// This is the key test for function debugging - we store a program,
/// then call it, and we should be able to step through its execution.
#[test]
fn step_into_stored_function() {
    // Create a simple program that stores a function and calls it:
    // :: 1 + ; "add_one" STO    -- store a function that adds 1
    // 5 add_one                   -- call it with 5, should get 6
    let code = ":: 1 + ; \"add_one\" STO 5 add_one";
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::paused();

    // Step through until we've stored the function and pushed 5
    // We need to get to the point where add_one is about to be called
    let mut step_count = 0;
    let max_steps = 20;
    let mut entered_function = false;

    loop {
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Paused) | ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps without completion");
                }

                let call_depth = vm.call_depth();

                // If we're inside the function (call_depth > 0), we've successfully stepped in!
                if call_depth > 0 {
                    entered_function = true;
                    // Continue to completion
                    debug.continue_running();
                    break;
                }

                debug.step_into();
            }
            ExecuteOutcome::Completed => {
                // If we completed without ever entering the function with call_depth > 0,
                // the stepping into functions is not working
                let stack = vm.stack_snapshot();
                if stack.last().unwrap().as_real() == Some(6.0) {
                    // Got correct result, but did we step into the function?
                    panic!(
                        "Program completed correctly but we never saw call_depth > 0 during stepping"
                    );
                }
                break;
            }
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    assert!(entered_function, "Should have entered the function");

    // Now continue to completion
    loop {
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(_) => debug.continue_running(),
            ExecuteOutcome::Completed => break,
        }
    }

    // Verify correct result
    let stack = vm.stack_snapshot();
    assert_eq!(
        stack.last().unwrap().as_real(),
        Some(6.0),
        "5 + 1 should equal 6"
    );
}

/// Test that step-into enters a function and we can inspect call_depth.
#[test]
fn step_into_function_call_depth() {
    // Store a simple function and call it
    let code = ":: 42 ; \"foo\" STO foo";
    let (mut vm, program, registry, operators) = compile_and_setup(code);

    let mut debug = DebugState::paused();
    let mut max_call_depth_seen = 0;
    let mut step_count = 0;
    let max_steps = 50;

    // Step through the entire program
    loop {
        let outcome = execute(
            &mut vm,
            &program,
            &registry,
            &operators,
            &mut UserLibraryRegistry::new(),
            Some(&mut debug),
        )
        .unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Paused) | ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps ({}), possible infinite loop", step_count);
                }

                let call_depth = vm.call_depth();
                if call_depth > max_call_depth_seen {
                    max_call_depth_seen = call_depth;
                }
                debug.step_into();
            }
            ExecuteOutcome::Completed => break,
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    // We should have seen call_depth > 0 at some point (when inside 'foo')
    assert!(
        max_call_depth_seen > 0,
        "Should have entered a function call (max_call_depth_seen={})",
        max_call_depth_seen
    );

    // Verify correct result
    let stack = vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(42.0));
}

/// Test step-into using the DebugSession like DAP server does.
/// This tests that the fix for resuming execution works when going through
/// the DebugSession API.
#[test]
fn step_into_via_debug_session() {
    use std::path::PathBuf;

    use rpl_dap::DebugSession;
    use rpl_source::{SourceFile, SourceId};

    let code = ":: 42 ; \"foo\" STO foo";
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let source = SourceFile::new(SourceId::new(0), "test.rpl".to_string(), code.to_string());

    let mut debug_session = DebugSession::new(program, source, PathBuf::from("test.rpl"));

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    let mut max_call_depth_seen = 0;
    let mut step_count = 0;
    let max_steps = 50;

    // First run to get initial pause
    let outcome = debug_session.run(&registry, &operators).unwrap();
    assert!(
        matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)),
        "Should start paused, got {:?}",
        outcome
    );

    // Step through the program
    loop {
        let outcome = debug_session.step_into(&registry, &operators).unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    panic!("Too many steps ({}), possible infinite loop", step_count);
                }

                let call_depth = debug_session.call_depth();
                if call_depth > max_call_depth_seen {
                    max_call_depth_seen = call_depth;
                }
            }
            ExecuteOutcome::Completed => break,
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    // We should have seen call_depth > 0 at some point (when inside 'foo')
    assert!(
        max_call_depth_seen > 0,
        "Should have entered a function call via DebugSession (max_call_depth_seen={})",
        max_call_depth_seen
    );

    // Verify correct result
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(42.0));
}

/// Test that breakpoints inside program definitions now work when the function is called.
///
/// Previously, breakpoints inside program definitions would not trigger because
/// the prolog was executed as a single unit. Now, with source-based breakpoints,
/// the breakpoint triggers when the stored function is called and executed.
#[test]
fn breakpoint_inside_program_triggers_on_call() {
    use std::path::PathBuf;

    use rpl_dap::DebugSession;
    use rpl_lang::find_pc_for_line;
    use rpl_source::{SourceFile, SourceId};

    // Multi-line program where line 2 contains "42" inside the program
    let code = "::\n  42\n; \"foo\" STO\nfoo";
    //          ^line 1  ^line 2    ^line 3   ^line 4

    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let source = SourceFile::new(SourceId::new(0), "test.rpl".to_string(), code.to_string());

    // Print bytecode and span info
    println!("Bytecode ({} words):", program.code.len());
    for (pc, word) in program.code.iter().enumerate() {
        let span = &program.spans[pc];
        let line = source.line_col(span.start()).line;
        println!("  [{}]: 0x{:08x} (line {})", pc, word, line);
    }

    // Check what PC each line maps to
    for line in 1..=4 {
        let pc = find_pc_for_line(&program, &source, line);
        println!("Line {} -> PC {:?}", line, pc);
    }

    let mut debug_session = DebugSession::new(program, source, PathBuf::from("test.rpl"));

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Set a breakpoint on line 2 (the "42" inside the program)
    let (bp_id, verified, actual_line) = debug_session.add_breakpoint(2);
    println!(
        "Breakpoint: id={}, verified={}, actual_line={:?}",
        bp_id, verified, actual_line
    );
    println!(
        "PC breakpoints: {:?}",
        debug_session.debug.breakpoints().collect::<Vec<_>>()
    );
    println!(
        "Source breakpoints: {:?}",
        debug_session.debug.source_breakpoints().collect::<Vec<_>>()
    );

    // Run and expect breakpoint to trigger when the function is called
    // Line 2 (the "42") is inside the program body, which means:
    // - During definition: the prolog is executed as one unit (doesn't trigger PC breakpoint)
    // - During call: the source breakpoint triggers because we're now executing the function body
    let outcome = debug_session
        .continue_running(&registry, &operators)
        .unwrap();
    println!("Outcome: {:?}", outcome);
    println!("VM PC after: {}", debug_session.vm.pc);
    println!("Call depth: {}", debug_session.vm.call_depth());

    // The breakpoint should trigger when the function is called
    assert!(
        matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Breakpoint(_))),
        "Breakpoint should trigger when the stored function is called. Got {:?}",
        outcome
    );

    // We should be inside the function (call_depth > 0)
    assert!(
        debug_session.vm.call_depth() > 0,
        "Should be inside the called function"
    );

    // Continue to completion
    let outcome = debug_session
        .continue_running(&registry, &operators)
        .unwrap();
    assert!(
        matches!(outcome, ExecuteOutcome::Completed),
        "Should complete after continuing. Got {:?}",
        outcome
    );

    // Verify correct result
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(42.0));
}

/// Test that debug info is stored with programs when running with source.
/// When we step into a called function, we should be able to access
/// its debug info for line mapping.
#[test]
fn debug_info_stored_with_program() {
    use std::path::PathBuf;

    use rpl_dap::DebugSession;
    use rpl_source::{SourceFile, SourceId};

    // Program that stores a function and calls it
    let code = ":: 42 ; \"foo\" STO foo";
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let source = SourceFile::new(SourceId::new(0), "test.rpl".to_string(), code.to_string());

    let mut debug_session = DebugSession::new(program, source, PathBuf::from("test.rpl"));

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to initial pause
    let outcome = debug_session.run(&registry, &operators).unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)));

    // Step until we're inside the function (call_depth > 0)
    let mut found_debug_info = false;
    let mut step_count = 0;
    let max_steps = 50;

    loop {
        let outcome = debug_session.step_into(&registry, &operators).unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    break;
                }

                // When inside the function, check if we have debug info
                if debug_session.vm.call_depth() > 0 {
                    // The VM should have debug info set from execute_with_source
                    if debug_session.vm.has_debug_info() {
                        found_debug_info = true;
                    }
                    // Continue to completion after finding debug info
                    break;
                }
            }
            ExecuteOutcome::Completed => break,
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    assert!(
        found_debug_info,
        "VM should have debug info when running via DebugSession"
    );

    // Continue to completion
    loop {
        let outcome = debug_session
            .continue_running(&registry, &operators)
            .unwrap();
        match outcome {
            ExecuteOutcome::Debug(_) => {}
            ExecuteOutcome::Completed => break,
        }
    }

    // Verify result
    let stack = debug_session.vm.stack_snapshot();
    assert_eq!(stack.last().unwrap().as_real(), Some(42.0));
}

/// Test that stored programs have debug info attached when created with source.
/// This is crucial for breakpoints to work inside called functions.
#[test]
fn stored_program_has_debug_info() {
    use std::path::PathBuf;

    use rpl_dap::DebugSession;
    use rpl_source::{SourceFile, SourceId};

    // Program that stores a function
    let code = ":: 42 ; \"foo\" STO";
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let source = SourceFile::new(SourceId::new(0), "test.rpl".to_string(), code.to_string());

    let mut debug_session = DebugSession::new(program, source, PathBuf::from("test.rpl"));

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to completion
    let outcome = debug_session
        .continue_running(&registry, &operators)
        .unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Completed));

    // Check the stored program has debug info
    let globals = debug_session.vm.globals_snapshot();
    let foo = globals.get("foo").expect("foo should be stored");

    match foo {
        Value::Program { code, debug_info } => {
            println!("Program code length: {}", code.len());
            println!("Debug info: {:?}", debug_info);
            assert!(
                debug_info.is_some(),
                "Stored program should have debug info attached"
            );
            let info = debug_info.as_ref().unwrap();
            assert!(!info.spans.is_empty(), "Debug info should have spans");
            assert!(info.source.is_some(), "Debug info should have source");
            assert!(
                info.source_name.is_some(),
                "Debug info should have source name"
            );
        }
        other => panic!("Expected Program, got {:?}", other),
    }
}

/// Test that debug info is available when we step into a function.
/// This verifies the full flow: program is stored with debug info,
/// then when called, the debug info is pushed to the call stack.
#[test]
fn step_into_function_has_debug_info_in_call_frame() {
    use std::path::PathBuf;

    use rpl_dap::DebugSession;
    use rpl_lang::ReturnEntry;
    use rpl_source::{SourceFile, SourceId};

    // Program that stores a function and calls it
    let code = ":: 42 ; \"foo\" STO foo";
    let mut session = Session::new();
    let id = session.set_source("test.rpl", code);
    let program = session.compile(id).expect("compile should succeed");
    let source = SourceFile::new(SourceId::new(0), "test.rpl".to_string(), code.to_string());

    let mut debug_session = DebugSession::new(program, source, PathBuf::from("test.rpl"));

    let mut registry = LibraryRegistry::new();
    register_standard_libs(&mut registry);
    let mut operators = OperatorRegistry::new();
    register_standard_operators(&mut operators);

    // Run to initial pause
    let outcome = debug_session.run(&registry, &operators).unwrap();
    assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Paused)));

    // Step until we're inside the function (call_depth > 0)
    let mut found_debug_info_in_frame = false;
    let mut step_count = 0;
    let max_steps = 50;

    loop {
        let outcome = debug_session.step_into(&registry, &operators).unwrap();
        match outcome {
            ExecuteOutcome::Debug(DebugEvent::Step) => {
                step_count += 1;
                if step_count > max_steps {
                    break;
                }

                // When inside the function, check if the call frame has debug info
                if debug_session.vm.call_depth() > 0 {
                    println!(
                        "Inside function at step {}, call_depth = {}",
                        step_count,
                        debug_session.vm.call_depth()
                    );
                    println!("  Current PC = {}", debug_session.vm.pc);

                    // Check the return stack for debug info
                    let return_stack = debug_session.vm.return_stack_snapshot();
                    for entry in return_stack.iter() {
                        if let ReturnEntry::Call {
                            name, debug_info, ..
                        } = entry
                        {
                            println!(
                                "  Call frame: name={:?}, has_debug_info={}",
                                name,
                                debug_info.is_some()
                            );
                            if let Some(dbg) = debug_info {
                                println!("    Debug info spans: {:?}", dbg.spans);
                                println!("    Source: {:?}", dbg.source.as_ref().map(|s| s.len()));
                                found_debug_info_in_frame = true;
                            }
                        }
                    }
                    break;
                }
            }
            ExecuteOutcome::Completed => break,
            other => panic!("Unexpected outcome: {:?}", other),
        }
    }

    assert!(
        found_debug_info_in_frame,
        "Call frame should have debug info when stepping into stored function"
    );
}
