//! Debug support for the RPL VM.
//!
//! Provides breakpoints, stepping, and execution control for debugging.

use std::collections::HashSet;

/// Debug execution mode.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DebugMode {
    /// Run until breakpoint or completion.
    Running,
    /// Paused, waiting for command.
    Paused,
    /// Execute one instruction then pause.
    StepInto,
    /// Execute until return stack depth decreases (exit current call).
    StepOut,
    /// Execute until same or lower return stack depth (step over calls).
    StepOver,
}

/// Event that caused execution to pause.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DebugEvent {
    /// Hit a breakpoint at this PC.
    Breakpoint(usize),
    /// Completed a step operation.
    Step,
    /// Started in paused mode.
    Paused,
}

/// Debug state for controlling execution.
#[derive(Debug)]
pub struct DebugState {
    /// Breakpoints by bytecode position (in main program only).
    breakpoints: HashSet<usize>,

    /// Breakpoints by source byte offset (works inside called functions too).
    source_breakpoints: HashSet<u32>,

    /// Current execution mode.
    mode: DebugMode,

    /// For step-out: target return stack depth to reach.
    step_out_depth: Option<usize>,

    /// For step-over: return stack depth when step started.
    step_over_depth: Option<usize>,

    /// Track if we've executed at least one instruction since last pause.
    /// Used to prevent immediately re-triggering breakpoint at current PC.
    executed_since_pause: bool,

    /// PC where we last paused (to avoid re-triggering same breakpoint).
    last_pause_pc: Option<usize>,

    /// Call depth where we last paused (breakpoints only trigger at depth 0).
    last_pause_depth: Option<usize>,

    /// Source offset where we last paused (to avoid re-triggering).
    last_pause_source_offset: Option<u32>,
}

impl DebugState {
    /// Create a new debug state in running mode.
    pub fn new() -> Self {
        Self {
            breakpoints: HashSet::new(),
            source_breakpoints: HashSet::new(),
            mode: DebugMode::Running,
            step_out_depth: None,
            step_over_depth: None,
            executed_since_pause: true,
            last_pause_pc: None,
            last_pause_depth: None,
            last_pause_source_offset: None,
        }
    }

    /// Create a new debug state that starts paused.
    pub fn paused() -> Self {
        Self {
            breakpoints: HashSet::new(),
            source_breakpoints: HashSet::new(),
            mode: DebugMode::Paused,
            step_out_depth: None,
            step_over_depth: None,
            executed_since_pause: false,
            last_pause_pc: None,
            last_pause_depth: None,
            last_pause_source_offset: None,
        }
    }

    /// Add a breakpoint at a bytecode position (main program only).
    pub fn add_breakpoint(&mut self, pc: usize) {
        self.breakpoints.insert(pc);
    }

    /// Add a breakpoint at a source byte offset (works inside called functions).
    pub fn add_source_breakpoint(&mut self, offset: u32) {
        self.source_breakpoints.insert(offset);
    }

    /// Remove a breakpoint by PC.
    pub fn remove_breakpoint(&mut self, pc: usize) {
        self.breakpoints.remove(&pc);
    }

    /// Remove a source breakpoint by offset.
    pub fn remove_source_breakpoint(&mut self, offset: u32) {
        self.source_breakpoints.remove(&offset);
    }

    /// Check if a breakpoint exists at a PC position.
    pub fn has_breakpoint(&self, pc: usize) -> bool {
        self.breakpoints.contains(&pc)
    }

    /// Check if a source breakpoint exists at an offset.
    pub fn has_source_breakpoint(&self, offset: u32) -> bool {
        self.source_breakpoints.contains(&offset)
    }

    /// Get all PC breakpoint positions.
    pub fn breakpoints(&self) -> impl Iterator<Item = usize> + '_ {
        self.breakpoints.iter().copied()
    }

    /// Get all source breakpoint offsets.
    pub fn source_breakpoints(&self) -> impl Iterator<Item = u32> + '_ {
        self.source_breakpoints.iter().copied()
    }

    /// Clear all breakpoints (both PC and source).
    pub fn clear_breakpoints(&mut self) {
        self.breakpoints.clear();
        self.source_breakpoints.clear();
    }

    /// Get the current mode.
    pub fn mode(&self) -> DebugMode {
        self.mode
    }

    /// Set the execution mode.
    pub fn set_mode(&mut self, mode: DebugMode) {
        self.mode = mode;
        // Reset tracking when mode changes
        self.executed_since_pause = false;
    }

    /// Continue running until next breakpoint.
    pub fn continue_running(&mut self) {
        self.mode = DebugMode::Running;
        self.executed_since_pause = false;
        self.step_out_depth = None;
        self.step_over_depth = None;
    }

    /// Step into the next instruction.
    pub fn step_into(&mut self) {
        self.mode = DebugMode::StepInto;
        self.executed_since_pause = false; // Will pause after next instruction
        self.step_out_depth = None;
        self.step_over_depth = None;
    }

    /// Step over the current instruction (don't descend into calls).
    pub fn step_over(&mut self, current_return_depth: usize) {
        self.mode = DebugMode::StepOver;
        self.step_over_depth = Some(current_return_depth);
        self.executed_since_pause = false; // Will pause after stepping
        self.step_out_depth = None;
    }

    /// Step out of the current call.
    pub fn step_out(&mut self, current_return_depth: usize) {
        self.mode = DebugMode::StepOut;
        self.step_out_depth = Some(current_return_depth.saturating_sub(1));
        self.executed_since_pause = false; // Will pause after return
        self.step_over_depth = None;
    }

    /// Check if we should pause before executing at this PC.
    ///
    /// The `call_depth` parameter indicates the current function call depth (0 = main program).
    /// The optional `source_offset` allows checking source-based breakpoints (works at all depths).
    /// PC-based breakpoints only trigger at call_depth 0.
    /// Stepping works at all call depths.
    ///
    /// Returns `Some(event)` if we should pause, `None` to continue.
    pub fn check(&mut self, pc: usize, call_depth: usize) -> Option<DebugEvent> {
        self.check_with_source(pc, call_depth, None)
    }

    /// Check if we should pause, with optional source offset for source-based breakpoints.
    pub fn check_with_source(
        &mut self,
        pc: usize,
        call_depth: usize,
        source_offset: Option<u32>,
    ) -> Option<DebugEvent> {
        match self.mode {
            DebugMode::Paused => {
                // Already paused, don't re-trigger
                if !self.executed_since_pause {
                    self.last_pause_pc = Some(pc);
                    self.last_pause_depth = Some(call_depth);
                    self.last_pause_source_offset = source_offset;
                    return Some(DebugEvent::Paused);
                }
                None
            }

            DebugMode::Running => {
                // Check source-based breakpoints first (works at all call depths)
                if let Some(offset) = source_offset {
                    let same_source_location = self.last_pause_source_offset == Some(offset);
                    if !same_source_location && self.source_breakpoints.contains(&offset) {
                        self.mode = DebugMode::Paused;
                        self.executed_since_pause = false;
                        self.last_pause_pc = Some(pc);
                        self.last_pause_depth = Some(call_depth);
                        self.last_pause_source_offset = Some(offset);
                        return Some(DebugEvent::Breakpoint(pc));
                    }
                }

                // PC-based breakpoints only apply to the main program (call_depth == 0)
                // This prevents false triggers when nested functions have their own PC=0
                let at_main_program = call_depth == 0;
                let same_location = self.last_pause_pc == Some(pc)
                    && self.last_pause_depth == Some(call_depth);

                if at_main_program && self.breakpoints.contains(&pc) && !same_location {
                    self.mode = DebugMode::Paused;
                    self.executed_since_pause = false;
                    self.last_pause_pc = Some(pc);
                    self.last_pause_depth = Some(call_depth);
                    self.last_pause_source_offset = source_offset;
                    Some(DebugEvent::Breakpoint(pc))
                } else {
                    // Clear last_pause when we move to a different location
                    if !same_location {
                        self.last_pause_pc = None;
                        self.last_pause_depth = None;
                    }
                    if source_offset != self.last_pause_source_offset {
                        self.last_pause_source_offset = None;
                    }
                    None
                }
            }

            DebugMode::StepInto => {
                // Pause after any instruction (works at all call depths)
                if self.executed_since_pause {
                    self.mode = DebugMode::Paused;
                    self.executed_since_pause = false;
                    self.last_pause_pc = Some(pc);
                    self.last_pause_depth = Some(call_depth);
                    Some(DebugEvent::Step)
                } else {
                    None
                }
            }

            DebugMode::StepOver => {
                // Pause when we're at same or lower call depth (works at all depths)
                if let Some(target_depth) = self.step_over_depth {
                    if self.executed_since_pause && call_depth <= target_depth {
                        self.mode = DebugMode::Paused;
                        self.executed_since_pause = false;
                        self.step_over_depth = None;
                        self.last_pause_pc = Some(pc);
                        self.last_pause_depth = Some(call_depth);
                        Some(DebugEvent::Step)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            DebugMode::StepOut => {
                // Pause when call depth is less than when we started
                if let Some(target_depth) = self.step_out_depth {
                    if self.executed_since_pause && call_depth <= target_depth {
                        self.mode = DebugMode::Paused;
                        self.executed_since_pause = false;
                        self.step_out_depth = None;
                        self.last_pause_pc = Some(pc);
                        self.last_pause_depth = Some(call_depth);
                        Some(DebugEvent::Step)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    /// Mark that an instruction was executed. Call after each instruction.
    pub fn mark_executed(&mut self) {
        self.executed_since_pause = true;
    }
}

impl Default for DebugState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_state_new() {
        let state = DebugState::new();
        assert_eq!(state.mode(), DebugMode::Running);
        assert!(!state.has_breakpoint(0));
    }

    #[test]
    fn debug_state_paused() {
        let state = DebugState::paused();
        assert_eq!(state.mode(), DebugMode::Paused);
    }

    #[test]
    fn add_remove_breakpoint() {
        let mut state = DebugState::new();

        state.add_breakpoint(10);
        assert!(state.has_breakpoint(10));
        assert!(!state.has_breakpoint(20));

        state.add_breakpoint(20);
        assert!(state.has_breakpoint(10));
        assert!(state.has_breakpoint(20));

        state.remove_breakpoint(10);
        assert!(!state.has_breakpoint(10));
        assert!(state.has_breakpoint(20));
    }

    #[test]
    fn clear_breakpoints() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);
        state.add_breakpoint(20);
        state.add_breakpoint(30);

        state.clear_breakpoints();
        assert!(!state.has_breakpoint(10));
        assert!(!state.has_breakpoint(20));
        assert!(!state.has_breakpoint(30));
    }

    #[test]
    fn check_breakpoint_triggers() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);

        // Should not trigger at other positions
        assert!(state.check(5, 0).is_none());

        // Should trigger at breakpoint
        let event = state.check(10, 0);
        assert_eq!(event, Some(DebugEvent::Breakpoint(10)));
        assert_eq!(state.mode(), DebugMode::Paused);
    }

    #[test]
    fn check_paused_mode() {
        let mut state = DebugState::paused();

        // Should return Paused event
        let event = state.check(0, 0);
        assert_eq!(event, Some(DebugEvent::Paused));
    }

    #[test]
    fn step_into() {
        let mut state = DebugState::new();
        state.step_into();

        // First check should NOT pause (need to execute first)
        assert!(state.check(0, 0).is_none());

        // After executing, should pause on next check
        state.mark_executed();
        let event = state.check(3, 0); // At next instruction
        assert_eq!(event, Some(DebugEvent::Step));
        assert_eq!(state.mode(), DebugMode::Paused);
    }

    #[test]
    fn step_over() {
        let mut state = DebugState::new();
        state.step_over(1); // At depth 1

        // Should not pause at deeper depth
        state.mark_executed();
        assert!(state.check(5, 2).is_none());

        // Should pause when back at same or lower depth
        let event = state.check(10, 1);
        assert_eq!(event, Some(DebugEvent::Step));
    }

    #[test]
    fn step_out() {
        let mut state = DebugState::new();
        state.step_out(2); // At depth 2, target is 1

        // Should not pause at same depth
        state.mark_executed();
        assert!(state.check(5, 2).is_none());

        // Should pause when at lower depth
        let event = state.check(10, 1);
        assert_eq!(event, Some(DebugEvent::Step));
    }

    #[test]
    fn continue_after_breakpoint() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);

        // Hit breakpoint
        let event = state.check(10, 0);
        assert_eq!(event, Some(DebugEvent::Breakpoint(10)));

        // Continue running
        state.continue_running();

        // Should not immediately re-trigger at same PC
        assert!(state.check(10, 0).is_none());

        // But should trigger if we come back later
        state.mark_executed();
        // Move away first
        assert!(state.check(20, 0).is_none());
        // Now coming back should trigger
        let event = state.check(10, 0);
        assert_eq!(event, Some(DebugEvent::Breakpoint(10)));
    }

    #[test]
    fn breakpoints_iterator() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);
        state.add_breakpoint(20);
        state.add_breakpoint(30);

        let bps: HashSet<usize> = state.breakpoints().collect();
        assert!(bps.contains(&10));
        assert!(bps.contains(&20));
        assert!(bps.contains(&30));
        assert_eq!(bps.len(), 3);
    }
}
