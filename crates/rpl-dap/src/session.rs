//! Debug session management.
//!
//! Manages the state of a debug session including the VM, compiled program,
//! debug state, and source mapping.

use std::collections::HashMap;
use std::path::PathBuf;

use rpl_lang::{DebugState, ExecuteOutcome, RuntimeError, VM, execute_with_source};
use rpl_session::{CompiledProgram, LibraryRegistry, OperatorRegistry, SourceFile, UserLibraryRegistry};

/// A debug session manages the state of debugging a single RPL program.
pub struct DebugSession {
    /// The virtual machine instance.
    pub vm: VM,

    /// The compiled program being debugged.
    pub program: CompiledProgram,

    /// Debug state (breakpoints, stepping mode, etc.).
    pub debug: DebugState,

    /// The source file being debugged.
    pub source: SourceFile,

    /// Path to the source file.
    pub source_path: PathBuf,

    /// User library registry for attached libraries.
    pub user_libs: UserLibraryRegistry,

    /// Mapping from DAP breakpoint IDs to bytecode PCs.
    breakpoint_map: HashMap<i64, usize>,

    /// Next available breakpoint ID.
    next_breakpoint_id: i64,

    /// Whether the session has started execution.
    pub started: bool,
}

impl DebugSession {
    /// Create a new debug session.
    pub fn new(
        program: CompiledProgram,
        source: SourceFile,
        source_path: PathBuf,
    ) -> Self {
        Self::with_vm(VM::new(), program, source, source_path)
    }

    /// Create a new debug session with an existing VM.
    ///
    /// This is useful when debugging code that depends on state set up
    /// in a previous execution (e.g., attached user libraries).
    pub fn with_vm(
        vm: VM,
        program: CompiledProgram,
        source: SourceFile,
        source_path: PathBuf,
    ) -> Self {
        Self {
            vm,
            program,
            debug: DebugState::paused(), // Start paused
            source,
            source_path,
            user_libs: UserLibraryRegistry::new(),
            breakpoint_map: HashMap::new(),
            next_breakpoint_id: 1,
            started: false,
        }
    }

    /// Add a breakpoint at a source line.
    ///
    /// Returns the breakpoint ID and whether it was verified (mapped to bytecode).
    pub fn add_breakpoint(&mut self, line: u32) -> (i64, bool, Option<u32>) {
        use rpl_lang::find_pc_for_line;

        let id = self.next_breakpoint_id;
        self.next_breakpoint_id += 1;

        // Try to find bytecode position for this line
        if let Some(pc) = find_pc_for_line(&self.program, &self.source, line) {
            // Add PC-based breakpoint (for main program)
            self.debug.add_breakpoint(pc);
            self.breakpoint_map.insert(id, pc);

            // Also add source-based breakpoint (works inside called functions)
            // Get the source byte offset for this PC
            if let Some(span) = self.program.spans.get(pc) {
                self.debug.add_source_breakpoint(span.start().offset());
            }

            // Get the actual line for this PC (might differ from requested)
            let actual_line = rpl_lang::line_for_pc(&self.program, &self.source, pc);
            (id, true, actual_line)
        } else {
            // Breakpoint couldn't be verified
            (id, false, None)
        }
    }

    /// Clear all breakpoints.
    pub fn clear_breakpoints(&mut self) {
        self.debug.clear_breakpoints();
        self.breakpoint_map.clear();
    }

    /// Execute until the next pause point (breakpoint, step, or completion).
    pub fn run(
        &mut self,
        registry: &LibraryRegistry,
        operators: &OperatorRegistry,
    ) -> Result<ExecuteOutcome, RuntimeError> {
        self.started = true;
        execute_with_source(
            &mut self.vm,
            &self.program,
            registry,
            operators,
            &mut self.user_libs,
            Some(&mut self.debug),
            Some(self.source.source()),
            Some(&self.source_path.to_string_lossy()),
        )
    }

    /// Continue execution.
    pub fn continue_running(
        &mut self,
        registry: &LibraryRegistry,
        operators: &OperatorRegistry,
    ) -> Result<ExecuteOutcome, RuntimeError> {
        self.debug.continue_running();
        self.run(registry, operators)
    }

    /// Step into the next instruction.
    pub fn step_into(
        &mut self,
        registry: &LibraryRegistry,
        operators: &OperatorRegistry,
    ) -> Result<ExecuteOutcome, RuntimeError> {
        self.debug.step_into();
        self.run(registry, operators)
    }

    /// Step over the next instruction (don't descend into calls).
    pub fn step_over(
        &mut self,
        registry: &LibraryRegistry,
        operators: &OperatorRegistry,
    ) -> Result<ExecuteOutcome, RuntimeError> {
        self.debug.step_over(self.vm.call_depth());
        self.run(registry, operators)
    }

    /// Step out of the current call.
    pub fn step_out(
        &mut self,
        registry: &LibraryRegistry,
        operators: &OperatorRegistry,
    ) -> Result<ExecuteOutcome, RuntimeError> {
        self.debug.step_out(self.vm.call_depth());
        self.run(registry, operators)
    }

    /// Get the current source line (1-indexed).
    ///
    /// Returns None if we're inside a called function (call_depth > 0) since
    /// called functions don't have source span information in the main program.
    pub fn current_line(&self) -> Option<u32> {
        if self.vm.call_depth() > 0 {
            // Inside a called function - no source mapping available
            None
        } else {
            rpl_lang::line_for_pc(&self.program, &self.source, self.vm.pc)
        }
    }

    /// Get the source line for a given PC in the main program.
    pub fn line_for_pc(&self, pc: usize) -> Option<u32> {
        rpl_lang::line_for_pc(&self.program, &self.source, pc)
    }

    /// Get the current call depth (0 = main program).
    pub fn call_depth(&self) -> usize {
        self.vm.call_depth()
    }

    /// Get the name of the current function being executed.
    ///
    /// Returns None if in the main program.
    pub fn current_function_name(&self) -> Option<String> {
        // Look at the most recent Call entry on the return stack
        use rpl_lang::ReturnEntry;
        for entry in self.vm.return_stack_snapshot().iter().rev() {
            if let ReturnEntry::Call { name, .. } = entry {
                return name.clone();
            }
        }
        None
    }
}
