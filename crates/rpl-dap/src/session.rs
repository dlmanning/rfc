//! Debug session management.
//!
//! Manages the state of a debug session including the VM, compiled program,
//! debug state, and source mapping.

use std::{collections::HashMap, path::PathBuf};

use rpl::{
    CompiledProgram, DebugState, ExecuteOutcome, Pos, ReturnEntry, SourceFile, debug_helpers,
    registry::Registry, vm::Vm,
};

/// A debug session manages the state of debugging a single RPL program.
pub struct DebugSession {
    /// The virtual machine instance.
    pub vm: Vm,

    /// The compiled program being debugged.
    pub program: CompiledProgram,

    /// Debug state (breakpoints, stepping mode, etc.).
    pub debug: DebugState,

    /// The source file being debugged.
    pub source: SourceFile,

    /// Path to the source file.
    pub source_path: PathBuf,

    /// The registry for libraries and operators.
    pub registry: Registry,

    /// Mapping from DAP breakpoint IDs to bytecode PCs.
    breakpoint_map: HashMap<i64, usize>,

    /// Next available breakpoint ID.
    next_breakpoint_id: i64,

    /// Whether the session has started execution.
    pub started: bool,
}

impl DebugSession {
    /// Create a new debug session.
    pub fn new(program: CompiledProgram, source: SourceFile, source_path: PathBuf) -> Self {
        Self::with_vm(Vm::new(), program, source, source_path)
    }

    /// Create a new debug session with an existing VM.
    pub fn with_vm(
        vm: Vm,
        program: CompiledProgram,
        source: SourceFile,
        source_path: PathBuf,
    ) -> Self {
        let mut debug = DebugState::new();
        debug.pause(); // Start paused

        Self {
            vm,
            program,
            debug,
            source,
            source_path,
            registry: Registry::with_core(),
            breakpoint_map: HashMap::new(),
            next_breakpoint_id: 1,
            started: false,
        }
    }

    /// Add a breakpoint at a source line.
    ///
    /// Returns the breakpoint ID and whether it was verified (mapped to bytecode).
    pub fn add_breakpoint(&mut self, line: u32) -> (i64, bool, Option<u32>) {
        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        // Try to find bytecode position for this line in the main program
        if let Some(pc) = debug_helpers::find_pc_for_line(&self.program, &self.source, line) {
            // Add PC-based breakpoint
            self.debug.add_breakpoint(pc);
            self.breakpoint_map.insert(id, pc);

            // Also add source-based breakpoint using line range (works inside called functions)
            if let Some(start_offset) = self.source.line_start(line) {
                let end_offset = self
                    .source
                    .line_start(line + 1)
                    .unwrap_or(self.source.source().len() as u32);
                self.debug
                    .add_source_line_breakpoint(start_offset, end_offset);
            }

            // Get the actual line for this PC (might differ from requested)
            let actual_line = debug_helpers::line_for_pc(&self.program, &self.source, pc);
            (id, true, actual_line)
        } else {
            // Line not in main program - might be inside a nested program (e.g., << ... >>)
            // Add a source-based breakpoint using the line's range
            if let Some(start_offset) = self.source.line_start(line) {
                let end_offset = self
                    .source
                    .line_start(line + 1)
                    .unwrap_or(self.source.source().len() as u32);
                self.debug
                    .add_source_line_breakpoint(start_offset, end_offset);
                (id, true, Some(line))
            } else {
                // Invalid line number
                (id, false, None)
            }
        }
    }

    /// Clear all breakpoints.
    pub fn clear_breakpoints(&mut self) {
        self.debug.clear_breakpoints();
        self.breakpoint_map.clear();
    }

    /// Execute until the next pause point (breakpoint, step, or completion).
    pub fn run(&mut self) -> Result<ExecuteOutcome, rpl::vm::VmError> {
        self.started = true;
        self.vm
            .execute_debug(&self.program, &self.registry, &mut self.debug)
    }

    /// Continue execution.
    pub fn continue_running(&mut self) -> Result<ExecuteOutcome, rpl::vm::VmError> {
        self.debug.continue_running();
        self.run()
    }

    /// Step into the next instruction.
    pub fn step_into(&mut self) -> Result<ExecuteOutcome, rpl::vm::VmError> {
        self.debug.step_into();
        self.run()
    }

    /// Step over the next instruction (don't descend into calls).
    pub fn step_over(&mut self) -> Result<ExecuteOutcome, rpl::vm::VmError> {
        self.debug.step_over(self.vm.call_depth());
        self.run()
    }

    /// Step out of the current call.
    pub fn step_out(&mut self) -> Result<ExecuteOutcome, rpl::vm::VmError> {
        self.debug.step_out(self.vm.call_depth());
        self.run()
    }

    /// Get the current source line (1-indexed).
    pub fn current_line(&self) -> Option<u32> {
        if self.vm.call_depth() > 0 {
            // Inside a nested call - use the nested program's source map
            if let Some(entry) = self.vm.return_stack().last() {
                let ReturnEntry::Call { program, .. } = entry;
                // Get source offset from the nested program's source map
                if let Some(offset) = program.source_offset_for_pc(self.vm.pc) {
                    // Convert offset to line number
                    let line_col = self.source.line_col(Pos::new(offset));
                    return Some(line_col.line);
                }
            }
            None
        } else {
            debug_helpers::line_for_pc(&self.program, &self.source, self.vm.pc)
        }
    }

    /// Get the source line for a given PC in the main program.
    pub fn line_for_pc(&self, pc: usize) -> Option<u32> {
        debug_helpers::line_for_pc(&self.program, &self.source, pc)
    }

    /// Get the current call depth (0 = main program).
    pub fn call_depth(&self) -> usize {
        self.vm.call_depth()
    }

    /// Get the name of the current function being executed.
    pub fn current_function_name(&self) -> Option<String> {
        for entry in self.vm.return_stack().iter().rev() {
            let ReturnEntry::Call { name, .. } = entry;
            if name.is_some() {
                return name.clone();
            }
        }
        None
    }

    /// Get the bytecode of the currently executing program.
    ///
    /// This returns the bytecode for the innermost program being executed,
    /// which may be a nested program (inside a function call) or the main program.
    pub fn current_bytecode(&self) -> &[u8] {
        if let Some(entry) = self.vm.return_stack().last() {
            let ReturnEntry::Call { program, .. } = entry;
            &program.code
        } else {
            &self.program.code
        }
    }

    /// Get the current PC (program counter) for disassembly reference.
    pub fn current_pc(&self) -> usize {
        self.vm.pc
    }
}

#[cfg(test)]
mod tests {
    use rpl::{DebugEvent, ExecuteOutcome, Session, SourceId};

    use super::*;

    fn create_session(source: &str) -> DebugSession {
        let source_file =
            SourceFile::new(SourceId::new(0), "test.rpl".to_string(), source.to_string());
        let mut rpl_session = Session::new();
        let program = rpl_session.compile(source).expect("Failed to compile");
        DebugSession::new(program, source_file, PathBuf::from("test.rpl"))
    }

    #[test]
    fn breakpoint_in_nested_program() {
        // Program with a nested function
        // Line 1: <<
        // Line 2:     -> a b
        // Line 3:     <<
        // Line 4:         a b +
        // Line 5:     >>
        // Line 6: >> 'add' STO
        // Line 7: 3 4 add
        let source = r#"<<
    -> a b
    <<
        a b +
    >>
>> 'add' STO
3 4 add"#;

        let mut session = create_session(source);

        // Set breakpoint on line 4 (a b +) - inside the nested program
        let (_id, verified, actual_line) = session.add_breakpoint(4);
        assert!(verified, "Breakpoint should be verified");
        assert_eq!(actual_line, Some(4));

        // Continue execution - should stop at the breakpoint
        session.debug.continue_running();
        let outcome = session.run().expect("Execution failed");

        // Should have stopped at a breakpoint
        assert!(
            matches!(
                outcome,
                ExecuteOutcome::Debug(DebugEvent::Breakpoint { .. })
            ),
            "Should stop at breakpoint, got {:?}",
            outcome
        );

        // Should be inside the nested call
        assert!(session.call_depth() > 0, "Should be inside nested call");

        // Current line should be 4
        assert_eq!(session.current_line(), Some(4), "Should be at line 4");
    }

    #[test]
    fn step_through_nested_program() {
        let source = r#"<<
    -> x
    <<
        x 2 *
    >>
>> 'double' STO
5 double"#;

        let mut session = create_session(source);

        // Session starts paused - step through the program
        let mut found_nested = false;
        for _ in 0..50 {
            let outcome = session.step_into();
            if outcome.is_err() {
                break;
            }
            if let Ok(ExecuteOutcome::Completed) = outcome {
                break;
            }
            if session.call_depth() > 0 {
                found_nested = true;
                // Should be able to get current line inside nested
                let line = session.current_line();
                assert!(
                    line.is_some(),
                    "Should have line info inside nested program"
                );
                break;
            }
        }
        assert!(found_nested, "Should have entered nested program");
    }

    #[test]
    fn gcd_breakpoint() {
        // Test breakpoints with gcd.rpl-style code
        let source = r#"@ GCD
<< -> a b <<
    IF b 0 ==
    THEN
        a
    ELSE
        b
        a b MOD
        gcd
    END
>> >>
"gcd" STO
48 18 gcd"#;

        let mut session = create_session(source);

        // Set breakpoint on line 3 ("IF b 0 ==") - inside the nested program
        // Line 3 in 1-indexed is "    IF b 0 =="
        let (_id, verified, _actual_line) = session.add_breakpoint(3);
        println!(
            "Breakpoint on line 3: verified={}, actual={:?}",
            verified, _actual_line
        );

        // Debug: print breakpoint info
        for (start, end) in session.debug.source_line_breakpoints() {
            println!("Source line breakpoint: {}..{}", start, end);
        }

        // Start execution (initially paused, will hit Entry)
        let outcome = session.run().expect("Run failed");
        println!("First run outcome: {:?}", outcome);
        println!(
            "Call depth: {}, line: {:?}",
            session.call_depth(),
            session.current_line()
        );

        // Continue past Entry - should hit our breakpoint
        let outcome = session.continue_running().expect("Continue failed");
        println!("After continue: {:?}", outcome);
        println!(
            "Call depth: {}, line: {:?}",
            session.call_depth(),
            session.current_line()
        );

        // Verify we hit a breakpoint (not completed)
        assert!(
            matches!(
                outcome,
                ExecuteOutcome::Debug(DebugEvent::Breakpoint { .. })
            ),
            "Should stop at breakpoint, got {:?}",
            outcome
        );

        // Should be inside nested call
        assert!(
            session.call_depth() > 0,
            "Should be inside nested call, depth={}",
            session.call_depth()
        );
    }

    #[test]
    fn gcd_breakpoint_exact_file() {
        // Use the exact same format as gcd.rpl
        let source = "@ Greatest Common Divisor using Euclidean algorithm
@ Usage: 48 18 gcd -> 6

<< -> a b <<
    IF b 0 ==
    THEN
        a
    ELSE
        b
        a b MOD
        gcd
    END
>> >>
\"gcd\" STO

@ Test: gcd(48, 18) = 6
48 18 gcd
";

        let mut session = create_session(source);

        // Print line offsets
        println!("Source length: {}", source.len());
        for i in 1..=18 {
            if let Some(start) = session.source.line_start(i) {
                let end = session
                    .source
                    .line_start(i + 1)
                    .unwrap_or(source.len() as u32);
                let line_text = &source[start as usize..end as usize];
                println!(
                    "Line {}: offset {}..{}: {:?}",
                    i,
                    start,
                    end,
                    line_text.trim_end_matches('\n')
                );
            }
        }

        // Set breakpoint on line 5 ("    IF b 0 ==")
        let (_id, verified, _actual_line) = session.add_breakpoint(5);
        println!(
            "\nBreakpoint on line 5: verified={}, actual={:?}",
            verified, _actual_line
        );

        // Debug: print breakpoint info
        for (start, end) in session.debug.source_line_breakpoints() {
            println!("Source line breakpoint range: {}..{}", start, end);
        }

        // Start execution (initially paused, will hit Entry)
        let outcome = session.run().expect("Run failed");
        println!("\nFirst run (Entry): {:?}", outcome);
        println!(
            "PC: {}, Call depth: {}, line: {:?}",
            session.current_pc(),
            session.call_depth(),
            session.current_line()
        );

        // Continue past Entry - should eventually hit our breakpoint inside gcd
        let outcome = session.continue_running().expect("Continue failed");
        println!("\nAfter continue: {:?}", outcome);
        println!(
            "PC: {}, Call depth: {}, line: {:?}",
            session.current_pc(),
            session.call_depth(),
            session.current_line()
        );

        match &outcome {
            ExecuteOutcome::Completed => {
                println!(
                    "Program completed! Stack: {:?}",
                    session.vm.stack_contents()
                );
                panic!("Should have hit breakpoint, not completed");
            }
            ExecuteOutcome::Debug(event) => {
                println!("Stopped at: {:?}", event);
            }
        }

        // Should be inside nested call
        assert!(
            session.call_depth() > 0,
            "Should be inside nested call, depth={}",
            session.call_depth()
        );
    }

    #[test]
    fn gcd_breakpoint_on_mod_line() {
        // Test breakpoint on "a b MOD" line (line 10) inside nested program
        let source = "@ Greatest Common Divisor using Euclidean algorithm
@ Usage: 48 18 gcd -> 6

<< -> a b <<
    IF b 0 ==
    THEN
        a
    ELSE
        b
        a b MOD
        gcd
    END
>> >>
\"gcd\" STO

@ Test: gcd(48, 18) = 6
48 18 gcd
";

        let mut session = create_session(source);

        // Set breakpoint on line 10 ("        a b MOD")
        let (_id, verified, _actual_line) = session.add_breakpoint(10);
        assert!(verified, "Breakpoint should be verified");

        // Run to Entry
        let outcome = session.run().expect("Run failed");
        assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Entry)));

        // Continue - should hit breakpoint on line 10
        let outcome = session.continue_running().expect("Continue failed");
        assert!(
            matches!(
                outcome,
                ExecuteOutcome::Debug(DebugEvent::Breakpoint { .. })
            ),
            "Should hit breakpoint, got {:?}",
            outcome
        );
        assert_eq!(session.current_line(), Some(10), "Should be on line 10");
        assert!(session.call_depth() > 0, "Should be inside nested call");
    }

    #[test]
    fn prime_sieve_call_depth() {
        // Check call depth inside prime_sieve's nested program structure
        let source = "<< -> n <<
    IF n 2 <
    THEN
        0
    ELSE
        1
    END
>> >>
\"is_prime\" STO
17 is_prime
";

        let mut session = create_session(source);

        // Print line info
        println!("Source lines:");
        for i in 1..=12 {
            if let Some(start) = session.source.line_start(i) {
                let end = session
                    .source
                    .line_start(i + 1)
                    .unwrap_or(source.len() as u32);
                let line_text = &source[start as usize..std::cmp::min(end as usize, source.len())];
                println!(
                    "Line {}: offset {}..{}: {:?}",
                    i,
                    start,
                    end,
                    line_text.trim_end_matches('\n')
                );
            }
        }

        // Set breakpoint on line 2 (IF n 2 <) - inside the inner program
        let (id, verified, actual_line) = session.add_breakpoint(2);
        println!(
            "\nBreakpoint on line 2: id={}, verified={}, actual={:?}",
            id, verified, actual_line
        );

        // Print breakpoint ranges
        println!("Breakpoint ranges:");
        for (start, end) in session.debug.source_line_breakpoints() {
            println!("  Source line breakpoint: {}..{}", start, end);
        }

        // Run and check if breakpoint is hit using continue_running (not step_into)
        let mut outcome = session.run().expect("Run failed");
        println!("\nEntry: {:?}, depth: {}", outcome, session.call_depth());

        // Now use continue_running to see if breakpoint fires
        for i in 1..=5 {
            if matches!(outcome, ExecuteOutcome::Completed) {
                println!("Completed at iteration {}", i);
                break;
            }
            outcome = session.continue_running().expect("Continue failed");
            println!(
                "Continue {}: {:?}, depth={}, pc={}, line={:?}",
                i,
                outcome,
                session.call_depth(),
                session.current_pc(),
                session.current_line()
            );

            // If inside a call, show source offset
            if session.call_depth() > 0
                && let Some(entry) = session.vm.return_stack().last()
            {
                let rpl::ReturnEntry::Call { program, .. } = entry;
                if let Some(offset) = program.source_offset_for_pc(session.current_pc()) {
                    println!("  -> nested program source offset: {}", offset);
                }
            }
        }
    }

    #[test]
    fn gcd_recursive_breakpoints() {
        // Test that breakpoints hit on each recursive call
        let source = "@ GCD
<< -> a b <<
    IF b 0 ==
    THEN
        a
    ELSE
        b
        a b MOD
        gcd
    END
>> >>
\"gcd\" STO
48 18 gcd
";

        let mut session = create_session(source);

        // Set breakpoint on line 3 ("    IF b 0 ==")
        session.add_breakpoint(3);

        // Run until completion, counting breakpoint hits
        let mut hits = 0;
        let mut last_outcome = session.run().expect("Run failed");

        for _ in 1..=10 {
            if matches!(last_outcome, ExecuteOutcome::Completed) {
                break;
            }
            last_outcome = session.continue_running().expect("Continue failed");
            if matches!(
                last_outcome,
                ExecuteOutcome::Debug(DebugEvent::Breakpoint { .. })
            ) {
                hits += 1;
            }
        }

        // GCD(48, 18) requires 4 recursive calls, so we expect 4 breakpoint hits
        assert_eq!(hits, 4, "Should hit breakpoint 4 times for gcd(48, 18)");
    }

    #[test]
    fn instruction_pointer_reference_format() {
        // Test with the actual prime_sieve.rpl to debug the line mismatch
        let source = r#"@ Check if a number is prime using trial division
@ Usage: 17 is_prime -> 1

<< -> n <<
    IF n 2 <
    THEN
        0
    ELSE
        IF n 2 ==
        THEN
            1
        ELSE
            @ Stack: (empty)
            @ We'll keep: is_prime divisor
            1 2
            WHILE DUP DUP * n <= REPEAT
                @ Stack: is_prime divisor
                n OVER MOD 0 ==
                IF THEN
                    @ Found a divisor - set is_prime to 0
                    @ Stack: is_prime divisor
                    SWAP DROP 0 SWAP
                    @ Stack: 0 divisor
                END
                1 +
            END
            DROP  @ drop the divisor, leave is_prime
        END
    END
>> >>
"is_prime" STO

@ Test: 18 is NOT prime (divisible by 2)
18 is_prime
"#;

        let mut session = create_session(source);

        // Print line info for reference
        println!("Source lines of interest:");
        for line_num in [18, 19, 22, 23] {
            if let Some(start) = session.source.line_start(line_num) {
                let end = session
                    .source
                    .line_start(line_num + 1)
                    .unwrap_or(source.len() as u32);
                let line_text = &source[start as usize..std::cmp::min(end as usize, source.len())];
                println!(
                    "  Line {}: offset {}..{}: {:?}",
                    line_num,
                    start,
                    end,
                    line_text.trim_end_matches('\n')
                );
            }
        }

        // Set breakpoint on line 22 (SWAP DROP 0 SWAP) - inside the inner program
        let (bp_id, verified, actual) = session.add_breakpoint(22);
        println!(
            "\nBreakpoint on line 22: id={}, verified={}, actual={:?}",
            bp_id, verified, actual
        );

        // Print breakpoint ranges
        println!("Breakpoint ranges:");
        for (start, end) in session.debug.source_line_breakpoints() {
            println!("  Source line breakpoint: {}..{}", start, end);
        }

        // Run to entry
        let outcome = session.run().expect("Run failed");
        assert!(matches!(outcome, ExecuteOutcome::Debug(DebugEvent::Entry)));

        // Continue to hit breakpoint
        let outcome = session.continue_running().expect("Continue failed");
        println!("\nAfter continue: {:?}", outcome);

        // Check if we hit the breakpoint
        if let ExecuteOutcome::Debug(DebugEvent::Breakpoint { pc }) = outcome {
            let call_depth = session.call_depth();
            let current_pc = session.current_pc();
            let current_line = session.current_line();

            println!("Breakpoint hit!");
            println!("  call_depth: {}", call_depth);
            println!("  current_pc (vm.pc): {}", current_pc);
            println!("  current_line: {:?}", current_line);
            println!("  breakpoint event pc: {}", pc);

            // Check if current_pc matches the breakpoint event pc
            assert_eq!(
                current_pc, pc,
                "current_pc should match breakpoint event pc"
            );

            // Now check what source offset the nested program reports for this PC
            if let Some(entry) = session.vm.return_stack().last() {
                let ReturnEntry::Call { program, .. } = entry;
                if let Some(offset) = program.source_offset_for_pc(current_pc) {
                    let line_col = session.source.line_col(Pos::new(offset));
                    println!("  source_offset_for_pc({}): {}", current_pc, offset);
                    println!(
                        "  line_col for offset {}: line={}, col={}",
                        offset, line_col.line, line_col.col
                    );
                }

                // Disassemble around the current PC
                println!("\nDisassembly around PC {}:", current_pc);
                let start_pc = current_pc.saturating_sub(10);
                let instructions = rpl::vm::disasm::disassemble(&program.code, start_pc, 20);
                for instr in &instructions {
                    let marker = if instr.pc == current_pc { ">>>" } else { "   " };
                    if let Some(offset) = program.source_offset_for_pc(instr.pc) {
                        let line = session.source.line_col(Pos::new(offset)).line;
                        println!(
                            "{} 0x{:04X}: {} [line {}]",
                            marker, instr.pc, instr.text, line
                        );
                    } else {
                        println!("{} 0x{:04X}: {}", marker, instr.pc, instr.text);
                    }
                }
            }
            // Now simulate what handle_stack_trace does
            println!("\nSimulating stack trace response:");
            let call_depth = session.call_depth();
            let current_pc = session.current_pc();
            let ipr = format!("0x{:02X}{:06X}", call_depth, current_pc);
            println!("  instruction_pointer_reference: {}", ipr);

            // Decode it back
            let full_addr = usize::from_str_radix(&ipr[2..], 16).unwrap();
            let decoded_depth = (full_addr >> 24) as usize;
            let decoded_pc = full_addr & 0x00FFFFFF;
            println!(
                "  Decoded: depth={}, pc={} (0x{:04X})",
                decoded_depth, decoded_pc, decoded_pc
            );

            // Check consistency
            assert_eq!(call_depth, decoded_depth, "Depth encoding mismatch");
            assert_eq!(current_pc, decoded_pc, "PC encoding mismatch");

            // The disassembly at decoded_pc should be line 22
            if let Some(entry) = session.vm.return_stack().last() {
                let ReturnEntry::Call { program, .. } = entry;
                if let Some(offset) = program.source_offset_for_pc(decoded_pc) {
                    let line = session.source.line_col(Pos::new(offset)).line;
                    println!("  Line for decoded PC: {}", line);
                    assert_eq!(line, 22, "Decoded PC should map to line 22");
                }
            }
        } else {
            println!("Did not hit breakpoint, got: {:?}", outcome);
        }
    }
}
