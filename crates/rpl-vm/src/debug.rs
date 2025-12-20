//! Debug state for VM debugging support.
//!
//! This module provides types for managing breakpoints, stepping modes,
//! and debug events during VM execution.

use std::collections::HashSet;

/// Debug mode indicating current execution state.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum DebugMode {
    /// Normal execution, no debugging active.
    #[default]
    Running,
    /// Paused at a breakpoint or after a step.
    Paused,
    /// Step into: stop at next instruction.
    StepInto,
    /// Step over: stop after current call returns or at next instruction at same depth.
    StepOver,
    /// Step out: stop when returning to a lower call depth.
    StepOut,
}

/// Debug event that caused execution to pause.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DebugEvent {
    /// Hit a breakpoint.
    Breakpoint {
        /// PC where breakpoint was hit.
        pc: usize,
    },
    /// Completed a step operation.
    Step {
        /// PC where step completed.
        pc: usize,
    },
    /// Entry point (first instruction).
    Entry,
}

/// Debug state for managing breakpoints and stepping.
#[derive(Clone, Debug, Default)]
pub struct DebugState {
    /// Whether debugging is enabled. When false, all debug checks are skipped.
    enabled: bool,
    /// PC-based breakpoints.
    breakpoints: HashSet<usize>,
    /// Source line breakpoints as (start_offset, end_offset) ranges.
    /// A breakpoint hits if the source offset falls within any range.
    source_line_breakpoints: Vec<(u32, u32)>,
    /// Current debug mode.
    mode: DebugMode,
    /// Target depth for step out (stop when call_depth < this).
    step_out_depth: Option<usize>,
    /// Target depth for step over (stop when call_depth <= this).
    step_over_depth: Option<usize>,
    /// Whether we've executed at least one instruction since last pause.
    executed_since_pause: bool,
    /// PC where we last paused (to avoid pausing twice at same location).
    last_pause_pc: Option<usize>,
    /// Source offset where we last paused.
    last_pause_source_offset: Option<u32>,
    /// Source line breakpoint range we last stopped at (to avoid multiple hits per line).
    last_pause_breakpoint_range: Option<(u32, u32)>,
}

impl DebugState {
    /// Create a new debug state.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new debug state with debugging enabled.
    pub fn enabled() -> Self {
        Self {
            enabled: true,
            ..Self::default()
        }
    }

    /// Get the current debug mode.
    pub fn mode(&self) -> DebugMode {
        self.mode
    }

    /// Check if debugging is enabled.
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Enable debugging.
    pub fn enable(&mut self) {
        self.enabled = true;
    }

    /// Disable debugging. All debug checks will be skipped.
    pub fn disable(&mut self) {
        self.enabled = false;
    }

    /// Toggle debugging on/off. Returns the new state.
    pub fn toggle(&mut self) -> bool {
        self.enabled = !self.enabled;
        self.enabled
    }

    /// Check if debugging is active (enabled and not just running).
    pub fn is_debugging(&self) -> bool {
        self.enabled
            && (self.mode != DebugMode::Running
                || !self.breakpoints.is_empty()
                || !self.source_line_breakpoints.is_empty())
    }

    // === Breakpoint Management ===

    /// Add a PC-based breakpoint.
    pub fn add_breakpoint(&mut self, pc: usize) {
        self.breakpoints.insert(pc);
    }

    /// Remove a PC-based breakpoint.
    pub fn remove_breakpoint(&mut self, pc: usize) {
        self.breakpoints.remove(&pc);
    }

    /// Check if a PC-based breakpoint exists.
    pub fn has_breakpoint(&self, pc: usize) -> bool {
        self.breakpoints.contains(&pc)
    }

    /// Add a source line breakpoint with a range.
    /// The breakpoint will hit if the source offset is >= start and < end.
    pub fn add_source_line_breakpoint(&mut self, start_offset: u32, end_offset: u32) {
        // Avoid duplicates
        if !self.source_line_breakpoints.contains(&(start_offset, end_offset)) {
            self.source_line_breakpoints.push((start_offset, end_offset));
        }
    }

    /// Add a source offset breakpoint (legacy API, uses offset as single point).
    pub fn add_source_breakpoint(&mut self, offset: u32) {
        self.add_source_line_breakpoint(offset, offset + 1);
    }

    /// Remove a source line breakpoint.
    pub fn remove_source_line_breakpoint(&mut self, start_offset: u32, end_offset: u32) {
        self.source_line_breakpoints.retain(|&(s, e)| s != start_offset || e != end_offset);
    }

    /// Check if a source offset falls within any source line breakpoint.
    pub fn has_source_breakpoint(&self, offset: u32) -> bool {
        self.source_line_breakpoints.iter().any(|&(start, end)| offset >= start && offset < end)
    }

    /// Clear all breakpoints.
    pub fn clear_breakpoints(&mut self) {
        self.breakpoints.clear();
        self.source_line_breakpoints.clear();
    }

    /// Get all PC breakpoints.
    pub fn breakpoints(&self) -> impl Iterator<Item = usize> + '_ {
        self.breakpoints.iter().copied()
    }

    /// Get all source line breakpoints as (start, end) ranges.
    pub fn source_line_breakpoints(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.source_line_breakpoints.iter().copied()
    }

    // === Stepping Control ===

    /// Continue normal execution.
    pub fn continue_running(&mut self) {
        self.mode = DebugMode::Running;
        self.step_out_depth = None;
        self.step_over_depth = None;
        self.executed_since_pause = false;
        // DON'T clear last_pause_pc/source_offset here!
        // We need them to avoid re-triggering the same breakpoint immediately.
        // They will be cleared in mark_executed() after we've moved past the breakpoint.
    }

    /// Step into: stop at next instruction.
    pub fn step_into(&mut self) {
        self.mode = DebugMode::StepInto;
        self.step_out_depth = None;
        self.step_over_depth = None;
        self.executed_since_pause = false;
    }

    /// Step over: stop at next instruction at same or lower call depth.
    pub fn step_over(&mut self, current_depth: usize) {
        self.mode = DebugMode::StepOver;
        self.step_over_depth = Some(current_depth);
        self.step_out_depth = None;
        self.executed_since_pause = false;
    }

    /// Step out: stop when returning to a lower call depth.
    pub fn step_out(&mut self, current_depth: usize) {
        self.mode = DebugMode::StepOut;
        self.step_out_depth = Some(current_depth);
        self.step_over_depth = None;
        self.executed_since_pause = false;
    }

    /// Pause execution immediately.
    pub fn pause(&mut self) {
        self.mode = DebugMode::Paused;
    }

    // === Execution Hooks ===

    /// Check if execution should pause at the current location.
    ///
    /// Call this before executing each instruction.
    /// Returns Some(event) if we should pause, None to continue.
    /// Returns None immediately if debugging is disabled.
    pub fn check(
        &mut self,
        pc: usize,
        call_depth: usize,
        source_offset: Option<u32>,
    ) -> Option<DebugEvent> {
        // Skip all checks if debugging is disabled
        if !self.enabled {
            return None;
        }

        // Check for initial entry pause (paused before any execution and no prior pause)
        // This handles the case where we start debugging in paused mode
        if self.mode == DebugMode::Paused
            && !self.executed_since_pause
            && self.last_pause_pc.is_none()
        {
            self.last_pause_pc = Some(pc);
            self.last_pause_source_offset = source_offset;
            return Some(DebugEvent::Entry);
        }

        // Check PC breakpoint
        if self.breakpoints.contains(&pc) {
            // Don't break twice at same PC
            if self.last_pause_pc != Some(pc) {
                self.mode = DebugMode::Paused;
                self.last_pause_pc = Some(pc);
                self.last_pause_source_offset = source_offset;
                return Some(DebugEvent::Breakpoint { pc });
            }
        }

        // Check source line breakpoint (uses range matching)
        if let Some(offset) = source_offset {
            // Find which breakpoint range this offset falls into
            if let Some(&(start, end)) = self.source_line_breakpoints
                .iter()
                .find(|&&(s, e)| offset >= s && offset < e)
            {
                // Don't break twice in same breakpoint range (same line)
                if self.last_pause_breakpoint_range != Some((start, end)) {
                    self.mode = DebugMode::Paused;
                    self.last_pause_pc = Some(pc);
                    self.last_pause_source_offset = Some(offset);
                    self.last_pause_breakpoint_range = Some((start, end));
                    return Some(DebugEvent::Breakpoint { pc });
                }
            }
        }

        // Check stepping modes (only after executing at least one instruction)
        if self.executed_since_pause {
            match self.mode {
                DebugMode::StepInto => {
                    self.mode = DebugMode::Paused;
                    self.last_pause_pc = Some(pc);
                    self.last_pause_source_offset = source_offset;
                    return Some(DebugEvent::Step { pc });
                }
                DebugMode::StepOver => {
                    if let Some(target_depth) = self.step_over_depth
                        && call_depth <= target_depth
                    {
                        self.mode = DebugMode::Paused;
                        self.last_pause_pc = Some(pc);
                        self.last_pause_source_offset = source_offset;
                        return Some(DebugEvent::Step { pc });
                    }
                }
                DebugMode::StepOut => {
                    if let Some(target_depth) = self.step_out_depth
                        && call_depth < target_depth
                    {
                        self.mode = DebugMode::Paused;
                        self.last_pause_pc = Some(pc);
                        self.last_pause_source_offset = source_offset;
                        return Some(DebugEvent::Step { pc });
                    }
                }
                DebugMode::Running | DebugMode::Paused => {}
            }
        }

        None
    }

    /// Mark that an instruction was executed.
    ///
    /// Call this after each instruction executes successfully.
    /// The source_offset is used to track when we leave a breakpoint's line.
    pub fn mark_executed_with_offset(&mut self, source_offset: Option<u32>) {
        self.executed_since_pause = true;
        // After executing an instruction, we've moved past the last pause location.
        // Clear PC-based tracking so we can hit PC breakpoints again on loops.
        self.last_pause_pc = None;
        self.last_pause_source_offset = None;

        // For line-based breakpoints, only clear the range when we've LEFT the line.
        // This prevents multiple hits on the same line while allowing re-hits after loops.
        if let Some((start, end)) = self.last_pause_breakpoint_range {
            if let Some(offset) = source_offset {
                // If current offset is outside the range, we've left the line
                if offset < start || offset >= end {
                    self.last_pause_breakpoint_range = None;
                }
            } else {
                // No source offset (e.g., internal code) - clear the range
                self.last_pause_breakpoint_range = None;
            }
        }
    }

    /// Mark that an instruction was executed (legacy API).
    ///
    /// Call this after each instruction executes successfully.
    pub fn mark_executed(&mut self) {
        // Without offset info, we must clear the breakpoint range
        self.mark_executed_with_offset(None);
    }

    /// Reset pause tracking for a new execution.
    pub fn reset_pause_tracking(&mut self) {
        self.last_pause_pc = None;
        self.last_pause_source_offset = None;
        self.last_pause_breakpoint_range = None;
        self.executed_since_pause = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_state_new() {
        let state = DebugState::new();
        assert_eq!(state.mode(), DebugMode::Running);
        assert!(!state.is_debugging());
    }

    #[test]
    fn add_pc_breakpoint() {
        let mut state = DebugState::enabled();
        state.add_breakpoint(10);
        assert!(state.has_breakpoint(10));
        assert!(!state.has_breakpoint(20));
        assert!(state.is_debugging());
    }

    #[test]
    fn remove_pc_breakpoint() {
        let mut state = DebugState::enabled();
        state.add_breakpoint(10);
        state.remove_breakpoint(10);
        assert!(!state.has_breakpoint(10));
    }

    #[test]
    fn add_source_breakpoint() {
        let mut state = DebugState::enabled();
        state.add_source_breakpoint(100);
        assert!(state.has_source_breakpoint(100));
        assert!(!state.has_source_breakpoint(200));
        assert!(state.is_debugging());
    }

    #[test]
    fn clear_breakpoints() {
        let mut state = DebugState::enabled();
        state.add_breakpoint(10);
        state.add_source_breakpoint(100);
        state.clear_breakpoints();
        assert!(!state.has_breakpoint(10));
        assert!(!state.has_source_breakpoint(100));
    }

    #[test]
    fn step_into() {
        let mut state = DebugState::enabled();
        state.step_into();
        assert_eq!(state.mode(), DebugMode::StepInto);

        // First check returns None (haven't executed yet)
        assert!(state.check(0, 0, None).is_none());

        // Mark executed
        state.mark_executed();

        // Next check returns Step event
        let event = state.check(1, 0, None);
        assert!(matches!(event, Some(DebugEvent::Step { pc: 1 })));
        assert_eq!(state.mode(), DebugMode::Paused);
    }

    #[test]
    fn step_over_same_depth() {
        let mut state = DebugState::enabled();
        state.step_over(1);
        assert_eq!(state.mode(), DebugMode::StepOver);

        state.mark_executed();

        // At same depth, should stop
        let event = state.check(1, 1, None);
        assert!(matches!(event, Some(DebugEvent::Step { .. })));
    }

    #[test]
    fn step_over_deeper_depth() {
        let mut state = DebugState::enabled();
        state.step_over(1);

        state.mark_executed();

        // At deeper depth, should continue
        let event = state.check(1, 2, None);
        assert!(event.is_none());
        assert_eq!(state.mode(), DebugMode::StepOver);
    }

    #[test]
    fn step_out() {
        let mut state = DebugState::enabled();
        state.step_out(2);
        assert_eq!(state.mode(), DebugMode::StepOut);

        state.mark_executed();

        // At same depth, continue
        assert!(state.check(1, 2, None).is_none());

        // At lower depth, stop
        let event = state.check(2, 1, None);
        assert!(matches!(event, Some(DebugEvent::Step { .. })));
    }

    #[test]
    fn breakpoint_hit() {
        let mut state = DebugState::enabled();
        state.add_breakpoint(10);

        // Should hit breakpoint
        let event = state.check(10, 0, None);
        assert!(matches!(event, Some(DebugEvent::Breakpoint { pc: 10 })));

        // Should not hit twice at same location
        let event = state.check(10, 0, None);
        assert!(event.is_none());
    }

    #[test]
    fn source_breakpoint_hit() {
        let mut state = DebugState::enabled();
        state.add_source_breakpoint(50);

        // Should hit at source offset
        let event = state.check(10, 0, Some(50));
        assert!(matches!(event, Some(DebugEvent::Breakpoint { .. })));

        // Should not hit twice
        let event = state.check(11, 0, Some(50));
        assert!(event.is_none());
    }

    #[test]
    fn continue_running() {
        let mut state = DebugState::enabled();
        state.step_into();
        state.mark_executed();
        state.check(0, 0, None); // Causes pause

        state.continue_running();
        assert_eq!(state.mode(), DebugMode::Running);
    }

    #[test]
    fn continue_after_breakpoint_does_not_retrigger() {
        let mut state = DebugState::enabled();
        state.add_breakpoint(10);

        // First check triggers breakpoint
        let event = state.check(10, 0, None);
        assert!(matches!(event, Some(DebugEvent::Breakpoint { pc: 10 })));
        assert_eq!(state.mode(), DebugMode::Paused);

        // User clicks continue
        state.continue_running();
        assert_eq!(state.mode(), DebugMode::Running);

        // Resume at same PC - should NOT retrigger breakpoint (haven't executed yet)
        let event = state.check(10, 0, None);
        assert!(event.is_none(), "Should not retrigger breakpoint at same PC");

        // Execute the instruction at PC 10
        state.mark_executed();

        // Now at PC 11, no breakpoint there
        let event = state.check(11, 0, None);
        assert!(event.is_none());

        // If we loop back to PC 10, we SHOULD hit breakpoint again
        let event = state.check(10, 0, None);
        assert!(matches!(event, Some(DebugEvent::Breakpoint { pc: 10 })));
    }

    #[test]
    fn pause() {
        let mut state = DebugState::new();
        state.pause();
        assert_eq!(state.mode(), DebugMode::Paused);
    }

    #[test]
    fn debug_state_disabled_by_default() {
        let state = DebugState::new();
        assert!(!state.is_enabled());
    }

    #[test]
    fn debug_state_enabled_constructor() {
        let state = DebugState::enabled();
        assert!(state.is_enabled());
    }

    #[test]
    fn toggle_debug() {
        let mut state = DebugState::new();
        assert!(!state.is_enabled());

        let enabled = state.toggle();
        assert!(enabled);
        assert!(state.is_enabled());

        let enabled = state.toggle();
        assert!(!enabled);
        assert!(!state.is_enabled());
    }

    #[test]
    fn enable_disable() {
        let mut state = DebugState::new();

        state.enable();
        assert!(state.is_enabled());

        state.disable();
        assert!(!state.is_enabled());
    }

    #[test]
    fn disabled_skips_breakpoints() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);

        // Disabled: should not hit breakpoint
        let event = state.check(10, 0, None);
        assert!(event.is_none());

        // Enable and try again
        state.enable();
        let event = state.check(10, 0, None);
        assert!(matches!(event, Some(DebugEvent::Breakpoint { pc: 10 })));
    }

    #[test]
    fn disabled_skips_stepping() {
        let mut state = DebugState::new();
        state.step_into();
        state.mark_executed();

        // Disabled: should not step
        let event = state.check(1, 0, None);
        assert!(event.is_none());

        // Enable and try again
        state.enable();
        let event = state.check(1, 0, None);
        assert!(matches!(event, Some(DebugEvent::Step { pc: 1 })));
    }

    #[test]
    fn is_debugging_respects_enabled() {
        let mut state = DebugState::new();
        state.add_breakpoint(10);

        // Has breakpoint but disabled
        assert!(!state.is_debugging());

        // Enable - now is debugging
        state.enable();
        assert!(state.is_debugging());
    }
}
