//! Application state for the REPL.

use rpl_core::TypeId;
use rpl_lang::Value;
use rpl_session::Session;

use super::{
    commands::{CommandResult, try_execute_command},
    input::{History, HistorySearch, KillRing, default_history_path},
    plot_decoder::decompile_plot,
    ui::{MultilineEditor, PlotViewState},
};

/// Which pane currently has focus.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Focus {
    /// Editor pane has focus (default).
    #[default]
    Editor,
    /// Stack sidebar has focus (for selection).
    Stack,
}

/// Application state.
pub struct App {
    /// The RPL session (compiler + VM).
    pub session: Session,
    /// Current focus (which pane receives input).
    pub focus: Focus,
    /// Multiline editor (always visible).
    pub editor: MultilineEditor,
    /// Kill ring for yank operations.
    pub kill_ring: KillRing,
    /// Command history with persistence.
    pub history: History,
    /// Incremental history search state (when Ctrl-R is active).
    pub history_search: Option<HistorySearch>,
    /// Selected stack level (1-indexed like HP 48, None = new input mode).
    pub selected_level: Option<usize>,
    /// Whether editor content has been modified since loading.
    pub editor_dirty: bool,
    /// Stack scroll offset (for viewing deeper stack levels).
    pub stack_scroll: usize,
    /// Last error message (if any).
    pub last_error: Option<String>,
    /// Whether the app is still running.
    pub running: bool,
    /// Plot viewer state (None = not viewing a plot).
    pub plot_view: Option<PlotViewState>,
}

impl App {
    /// Create a new App with default state.
    pub fn new() -> Self {
        // Set up history with file persistence
        let mut history = match default_history_path() {
            Some(path) => History::with_file(1000, path),
            None => History::new(1000),
        };

        // Load existing history (ignore errors)
        let _ = history.load();

        Self {
            session: Session::new(),
            focus: Focus::Editor,
            editor: MultilineEditor::new(),
            kill_ring: KillRing::default(),
            history,
            history_search: None,
            selected_level: None,
            editor_dirty: false,
            stack_scroll: 0,
            last_error: None,
            running: true,
            plot_view: None,
        }
    }

    /// Save history to file (call on exit).
    pub fn save_history(&self) {
        let _ = self.history.save();
    }

    // ========================================================================
    // Execution
    // ========================================================================

    /// Execute the editor content.
    ///
    /// Uses Session::eval_repl() which preserves the stack between evaluations.
    /// Returns true if the REPL should quit.
    pub fn run_editor(&mut self) -> bool {
        // Cancel any active search
        self.history_search = None;

        let content = self.editor.content();
        self.history.reset_navigation();
        self.kill_ring.reset_yank();

        if content.trim().is_empty() {
            return false;
        }

        // Check for meta-commands first
        match try_execute_command(self, &content) {
            CommandResult::Quit => {
                return true;
            }
            CommandResult::Handled => {
                // Add to history even for commands
                self.history.add(content);
                self.editor.clear();
                self.editor_dirty = false;
                return false;
            }
            CommandResult::NotACommand => {
                // Continue to execute as RPL code
            }
        }

        // Add to history
        self.history.add(content.clone());

        // Compile and execute, preserving the stack
        match self.session.eval_repl(&content) {
            Ok(()) => {
                self.last_error = None;
                self.editor.clear();
                self.editor_dirty = false;
                // Clear selection after successful execution
                self.selected_level = None;
                self.plot_view = None;
            }
            Err(e) => {
                self.last_error = Some(format!("{}", e));
            }
        }

        false
    }

    /// Apply editor content to replace the selected stack item.
    pub fn apply_edit(&mut self) -> Result<(), String> {
        let Some(level) = self.selected_level else {
            return Err("No item selected".to_string());
        };

        let content = self.editor.content();
        if content.trim().is_empty() {
            return Err("Editor is empty".to_string());
        }

        // Save current stack depth
        let stack = self.session.vm().stack();
        let original_depth = stack.len();
        if level > original_depth {
            return Err("Selected level no longer exists".to_string());
        }

        // Evaluate the editor content
        match self.session.eval_repl(&content) {
            Ok(()) => {
                // Check that exactly one item was produced
                let new_depth = self.session.vm().stack().len();
                if new_depth == original_depth + 1 {
                    // Swap the new item with the selected level
                    // Pop the new value, remove the old one, push new one in place
                    // For now, simple approach: just leave the new item on top
                    // TODO: Actually replace at the selected level
                    self.last_error = None;
                    self.editor_dirty = false;
                    self.history.add(content);
                    Ok(())
                } else if new_depth == original_depth {
                    // Command consumed something but produced nothing
                    Err("Edit produced no result".to_string())
                } else {
                    Err(format!(
                        "Edit produced {} items (expected 1)",
                        new_depth.saturating_sub(original_depth)
                    ))
                }
            }
            Err(e) => {
                self.last_error = Some(format!("{}", e));
                Err(format!("{}", e))
            }
        }
    }

    /// Replace the selected stack item with the result of evaluating editor content.
    /// Returns Ok(true) if the REPL should quit.
    pub fn replace_selected(&mut self) -> Result<bool, String> {
        let Some(level) = self.selected_level else {
            return Err("No item selected".to_string());
        };

        let content = self.editor.content();
        if content.trim().is_empty() {
            return Err("Editor is empty".to_string());
        }

        // Check for meta-commands first
        match try_execute_command(self, &content) {
            CommandResult::Quit => return Ok(true),
            CommandResult::Handled => {
                self.history.add(content);
                self.editor.clear();
                self.editor_dirty = false;
                self.selected_level = None;
                return Ok(false);
            }
            CommandResult::NotACommand => {}
        }

        // Save current stack state
        let original_depth = self.session.vm().stack().len();
        if level > original_depth {
            return Err("Selected level no longer exists".to_string());
        }

        // Evaluate the editor content
        match self.session.eval_repl(&content) {
            Ok(()) => {
                let new_depth = self.session.vm().stack().len();
                let produced = new_depth.saturating_sub(original_depth);

                if produced == 1 {
                    // Success: new item is at level 1, now we need to:
                    // 1. Remove the old item at (level + 1) since we added one
                    // 2. The new item stays at level 1
                    let target_level = level + 1; // Old item shifted up by 1
                    if target_level <= new_depth {
                        // Use ROLL to bring old item to top, then DROP it
                        let roll_cmd = format!("{} ROLL DROP", target_level);
                        let _ = self.session.eval_repl(&roll_cmd);
                    }

                    self.last_error = None;
                    self.editor_dirty = false;
                    self.history.add(content);

                    // Re-select level 1 to show the new item and update preview
                    self.select_level(1);
                    Ok(false)
                } else if produced == 0 {
                    Err("Edit produced no result".to_string())
                } else {
                    Err(format!("Edit produced {} items (expected 1)", produced))
                }
            }
            Err(e) => {
                self.last_error = Some(format!("{}", e));
                Err(format!("{}", e))
            }
        }
    }

    /// Revert editor to the selected item's original content.
    pub fn revert_edit(&mut self) {
        if let Some(level) = self.selected_level {
            self.select_level(level);
        } else {
            self.editor.clear();
            self.editor_dirty = false;
        }
    }

    // ========================================================================
    // Stack selection
    // ========================================================================

    /// Select a stack level for viewing/editing.
    pub fn select_level(&mut self, level: usize) {
        let stack = self.session.vm().stack();
        if level < 1 || level > stack.len() {
            return;
        }

        self.selected_level = Some(level);
        let value = &stack[stack.len() - level];

        // Load content into editor
        let formatted = format_for_edit(value);
        self.editor = MultilineEditor::with_content(&formatted);
        self.editor_dirty = false;

        // If it's a plot, set up plot view state
        if let Value::Object { type_id, data } = value {
            if *type_id == TypeId::PLOT {
                self.plot_view = Some(PlotViewState::new(data.clone()));
            } else {
                self.plot_view = None;
            }
        } else {
            self.plot_view = None;
        }
    }

    /// Deselect and go to new input mode.
    pub fn deselect(&mut self) {
        self.selected_level = None;
        self.editor.clear();
        self.editor_dirty = false;
        self.plot_view = None;
    }

    /// Move selection up (to higher level / older item).
    pub fn select_up(&mut self) {
        let stack_len = self.session.vm().stack().len();
        if stack_len == 0 {
            return;
        }

        let new_level = match self.selected_level {
            None => 1, // Start at level 1
            Some(current) if current < stack_len => current + 1,
            Some(_) => return, // Already at max
        };
        self.select_level(new_level);
    }

    /// Move selection down (to lower level / newer item).
    pub fn select_down(&mut self) {
        match self.selected_level {
            None => {} // Already deselected
            Some(1) => self.deselect(),
            Some(current) => self.select_level(current - 1),
        }
    }

    /// Check if an item is selected.
    pub fn has_selection(&self) -> bool {
        self.selected_level.is_some()
    }

    /// Check if the selected item is visualizable (shows preview).
    pub fn should_show_preview(&self) -> bool {
        if let Some(level) = self.selected_level {
            let stack = self.session.vm().stack();
            if let Some(value) = stack.get(stack.len().saturating_sub(level)) {
                return matches!(
                    value,
                    Value::Object { type_id, .. } if *type_id == TypeId::PLOT
                );
            }
        }
        false
    }

    // ========================================================================
    // History navigation (for editor)
    // ========================================================================

    /// Navigate to previous history entry.
    pub fn history_prev(&mut self) {
        self.kill_ring.reset_yank();
        let current = self.editor.content();
        if let Some(entry) = self.history.prev(&current) {
            self.editor = MultilineEditor::with_content(entry);
            self.editor_dirty = true;
        }
    }

    /// Navigate to next history entry.
    pub fn history_next(&mut self) {
        self.kill_ring.reset_yank();
        if let Some(entry) = self.history.next() {
            self.editor = MultilineEditor::with_content(entry);
            self.editor_dirty = true;
        }
    }

    // ========================================================================
    // History search (Ctrl-R)
    // ========================================================================

    /// Start incremental history search.
    pub fn start_history_search(&mut self) {
        self.history_search = Some(HistorySearch::new());
    }

    /// Cancel history search and restore input.
    pub fn cancel_history_search(&mut self) {
        self.history_search = None;
    }

    /// Accept the current search result and exit search mode.
    pub fn accept_history_search(&mut self) {
        if let Some(ref search) = self.history_search
            && let Some(entry) = search.current_entry(&self.history)
        {
            self.editor = MultilineEditor::with_content(entry);
            self.editor_dirty = true;
        }
        self.history_search = None;
    }

    /// Add a character to the search query.
    pub fn search_push_char(&mut self, c: char) {
        if let Some(ref mut search) = self.history_search {
            search.push_char(c, &self.history);
        }
    }

    /// Remove a character from the search query.
    pub fn search_pop_char(&mut self) {
        if let Some(ref mut search) = self.history_search {
            search.pop_char(&self.history);
        }
    }

    /// Move to the next search match (older).
    pub fn search_next_match(&mut self) {
        if let Some(ref mut search) = self.history_search {
            search.next_match();
        }
    }

    /// Move to the previous search match (newer).
    pub fn search_prev_match(&mut self) {
        if let Some(ref mut search) = self.history_search {
            search.prev_match();
        }
    }

    /// Check if in search mode.
    pub fn in_search_mode(&self) -> bool {
        self.history_search.is_some()
    }

    // ========================================================================
    // Plot viewer (for preview pane)
    // ========================================================================

    /// Update plot preview from editor content (live preview).
    pub fn update_plot_preview(&mut self) {
        // This could be called after running editor content to update preview
        if let Some(level) = self.selected_level {
            let stack = self.session.vm().stack();
            if let Some(Value::Object { type_id, data }) =
                stack.get(stack.len().saturating_sub(level))
                && *type_id == TypeId::PLOT
            {
                self.plot_view = Some(PlotViewState::new(data.clone()));
            }
        }
    }
}

impl Default for App {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a value for editing in the editor.
fn format_for_edit(value: &Value) -> String {
    match value {
        Value::Int(i) => i.to_string(),
        Value::Real(r) => {
            if r.fract() == 0.0 && r.abs() < 1e15 {
                format!("{}.", *r as i64)
            } else {
                format!("{}", r)
            }
        }
        Value::Bool(b) => if *b { "1" } else { "0" }.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Symbol(s) => format!("'{:?}", s),
        Value::List(items) => {
            let formatted: Vec<String> = items.iter().map(format_for_edit).collect();
            format!("{{ {} }}", formatted.join(" "))
        }
        Value::Program { debug_info, .. } => {
            // Return the program source if available
            if let Some(info) = debug_info
                && let Some(source) = &info.source
            {
                return source.clone();
            }
            "# Program (no source available)".to_string()
        }
        Value::Object { type_id, data } if *type_id == TypeId::PLOT => {
            // Decompile plot bytecode back to editable source
            decompile_plot(data)
        }
        Value::Object { type_id, data } if *type_id == TypeId::COMPLEX => {
            // Format complex number for editing
            format_complex_for_edit(data)
        }
        Value::Object { type_id, .. } => match type_id.name() {
            Some(name) => format!("# {} object (not editable)", name),
            None => format!("# Object({}) (not editable)", type_id.as_u16()),
        },
    }
}

/// Format a complex number for editing.
fn format_complex_for_edit(data: &[u32]) -> String {
    if data.len() == 4 {
        // Rectangular: (re, im)
        let re = decode_f64(data[0], data[1]);
        let im = decode_f64(data[2], data[3]);
        format!("({}, {})", re, im)
    } else if data.len() == 5 {
        // Polar: (mag∠angle) or (mag∠angle°)
        let mag = decode_f64(data[0], data[1]);
        let angle = decode_f64(data[2], data[3]);
        let mode = data[4] as u8;
        if mode == 1 {
            format!("({}∠{}°)", mag, angle)
        } else {
            format!("({}∠{})", mag, angle)
        }
    } else {
        "# Complex (invalid format)".to_string()
    }
}

/// Decode two u32 words to an f64.
fn decode_f64(hi: u32, lo: u32) -> f64 {
    let bits = ((hi as u64) << 32) | (lo as u64);
    f64::from_bits(bits)
}
