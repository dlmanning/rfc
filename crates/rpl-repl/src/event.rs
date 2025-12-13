//! Event loop and input handling.

use std::io;
use std::time::Duration;

use crossterm::event::{self, Event, KeyCode, KeyEventKind, KeyModifiers};
use ratatui::DefaultTerminal;

use super::app::{App, Focus};
use super::ui;

/// Run the main event loop.
pub fn run_event_loop(app: &mut App) -> io::Result<()> {
    let mut terminal = ratatui::init();
    let result = run_loop(&mut terminal, app);
    ratatui::restore();
    result
}

fn run_loop(terminal: &mut DefaultTerminal, app: &mut App) -> io::Result<()> {
    while app.running {
        // Draw UI
        terminal.draw(|frame| {
            ui::render(frame, app);
        })?;

        // Poll for events with timeout
        if event::poll(Duration::from_millis(100))?
            && let Event::Key(key) = event::read()?
        {
            // Only handle key press events (not release on Windows)
            if key.kind != KeyEventKind::Press {
                continue;
            }
            handle_key(app, key.modifiers, key.code);
        }
    }

    // Save history on exit
    app.save_history();

    Ok(())
}

fn handle_key(app: &mut App, modifiers: KeyModifiers, code: KeyCode) {
    // Handle search mode first (intercepts all keys)
    if app.in_search_mode() {
        handle_search_key(app, modifiers, code);
        return;
    }

    // Global keys (work regardless of focus)
    match (modifiers, code) {
        // Exit on Ctrl-D when editor is empty
        (KeyModifiers::CONTROL, KeyCode::Char('d')) if app.editor.is_empty() => {
            app.running = false;
            return;
        }
        // History search
        (KeyModifiers::CONTROL, KeyCode::Char('r')) => {
            app.start_history_search();
            return;
        }
        // Soft menu keys (F1-F6) - always available
        (KeyModifiers::NONE, KeyCode::F(n)) if (1..=6).contains(&n) => {
            handle_soft_key(app, n);
            return;
        }
        _ => {}
    }

    // Route based on focus
    match app.focus {
        Focus::Editor => handle_editor_key(app, modifiers, code),
        Focus::Stack => handle_stack_key(app, modifiers, code),
    }
}

fn handle_search_key(app: &mut App, modifiers: KeyModifiers, code: KeyCode) {
    match (modifiers, code) {
        // Accept search result and exit
        (KeyModifiers::NONE, KeyCode::Enter) => {
            app.accept_history_search();
        }
        // Cancel search
        (KeyModifiers::NONE, KeyCode::Esc) | (KeyModifiers::CONTROL, KeyCode::Char('g')) => {
            app.cancel_history_search();
        }
        // Cycle to next match (older)
        (KeyModifiers::CONTROL, KeyCode::Char('r')) => {
            app.search_next_match();
        }
        // Cycle to previous match (newer)
        (KeyModifiers::CONTROL, KeyCode::Char('s')) => {
            app.search_prev_match();
        }
        // Delete character from search query
        (KeyModifiers::NONE, KeyCode::Backspace) | (KeyModifiers::CONTROL, KeyCode::Char('h')) => {
            app.search_pop_char();
        }
        // Add character to search query
        (KeyModifiers::NONE | KeyModifiers::SHIFT, KeyCode::Char(c)) => {
            app.search_push_char(c);
        }
        _ => {}
    }
}

fn handle_editor_key(app: &mut App, modifiers: KeyModifiers, code: KeyCode) {
    // Clear error on typing
    if app.last_error.is_some() {
        match code {
            KeyCode::Char(_) | KeyCode::Backspace | KeyCode::Enter => {
                app.last_error = None;
            }
            _ => {}
        }
    }

    match (modifiers, code) {
        // Switch focus to stack
        (KeyModifiers::NONE, KeyCode::Tab) => {
            app.focus = Focus::Stack;
        }

        // Execute/Run (Ctrl+Enter)
        (KeyModifiers::CONTROL, KeyCode::Enter) => {
            if app.run_editor() {
                app.running = false;
            }
        }

        // Apply changes to selected item (Alt+Enter)
        (KeyModifiers::ALT, KeyCode::Enter) if app.has_selection() => {
            if let Err(e) = app.apply_edit() {
                app.last_error = Some(e);
            }
        }

        // Shift+Enter: always insert newline (for multiline input)
        (KeyModifiers::SHIFT, KeyCode::Enter) => {
            app.editor.insert_newline();
            app.editor_dirty = true;
        }

        // Enter behavior: smart execute vs newline
        (KeyModifiers::NONE, KeyCode::Enter) => {
            let content = app.editor.content();
            let trimmed = content.trim();

            // Check if we're in an incomplete block (program, list, etc.)
            let open_programs = trimmed.matches("<<").count();
            let close_programs = trimmed.matches(">>").count();
            let open_lists = trimmed.matches('{').count();
            let close_lists = trimmed.matches('}').count();

            let is_incomplete = open_programs > close_programs || open_lists > close_lists;

            if trimmed.is_empty() {
                // Empty input: do nothing or clear
            } else if is_incomplete {
                // Incomplete block: insert newline
                app.editor.insert_newline();
                app.editor_dirty = true;
            } else if app.has_selection() {
                // Editing existing item: replace it
                match app.replace_selected() {
                    Ok(should_quit) if should_quit => app.running = false,
                    Ok(_) => {}
                    Err(e) => app.last_error = Some(e),
                }
            } else {
                // New input: execute normally
                if app.run_editor() {
                    app.running = false;
                }
            }
        }

        // Cursor movement
        (KeyModifiers::NONE, KeyCode::Up) | (KeyModifiers::CONTROL, KeyCode::Char('p')) => {
            app.editor.move_up();
        }
        (KeyModifiers::NONE, KeyCode::Down) | (KeyModifiers::CONTROL, KeyCode::Char('n')) => {
            app.editor.move_down();
        }
        (KeyModifiers::NONE, KeyCode::Left) | (KeyModifiers::CONTROL, KeyCode::Char('b')) => {
            app.editor.move_left();
        }
        (KeyModifiers::NONE, KeyCode::Right) | (KeyModifiers::CONTROL, KeyCode::Char('f')) => {
            app.editor.move_right();
        }
        (KeyModifiers::CONTROL, KeyCode::Char('a')) | (KeyModifiers::NONE, KeyCode::Home) => {
            app.editor.move_to_line_start();
        }
        (KeyModifiers::CONTROL, KeyCode::Char('e')) | (KeyModifiers::NONE, KeyCode::End) => {
            app.editor.move_to_line_end();
        }

        // Editing
        (KeyModifiers::NONE, KeyCode::Backspace) | (KeyModifiers::CONTROL, KeyCode::Char('h')) => {
            app.editor.backspace();
            app.editor_dirty = true;
        }
        (KeyModifiers::NONE, KeyCode::Delete) | (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
            app.editor.delete();
            app.editor_dirty = true;
        }

        // Kill operations
        (KeyModifiers::CONTROL, KeyCode::Char('k')) => {
            let killed = app.editor.kill_to_end();
            app.kill_ring.push(killed);
            app.editor_dirty = true;
        }
        (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
            let killed = app.editor.kill_to_start();
            app.kill_ring.push(killed);
            app.editor_dirty = true;
        }

        // Yank
        (KeyModifiers::CONTROL, KeyCode::Char('y')) => {
            if let Some(text) = app.kill_ring.yank() {
                app.editor.insert_str(text);
                app.editor_dirty = true;
            }
        }

        // Clear (Ctrl-C)
        (KeyModifiers::CONTROL, KeyCode::Char('c')) => {
            app.editor.clear();
            app.last_error = None;
            app.editor_dirty = false;
            if app.has_selection() {
                app.deselect();
            }
        }

        // Character input
        (KeyModifiers::NONE | KeyModifiers::SHIFT, KeyCode::Char(c)) => {
            app.editor.insert_char(c);
            app.editor_dirty = true;
        }

        _ => {}
    }
}

fn handle_stack_key(app: &mut App, modifiers: KeyModifiers, code: KeyCode) {
    match (modifiers, code) {
        // Switch focus to editor
        (KeyModifiers::NONE, KeyCode::Tab) => {
            app.focus = Focus::Editor;
        }

        // Navigate stack selection
        (KeyModifiers::NONE, KeyCode::Up) | (KeyModifiers::NONE, KeyCode::Char('k')) => {
            app.select_up();
        }
        (KeyModifiers::NONE, KeyCode::Down) | (KeyModifiers::NONE, KeyCode::Char('j')) => {
            app.select_down();
        }

        // Select with Enter - move to editor to work with selected item
        (KeyModifiers::NONE, KeyCode::Enter) => {
            app.focus = Focus::Editor;
        }

        // Quick actions on selected item
        (KeyModifiers::NONE, KeyCode::Char('d')) | (KeyModifiers::NONE, KeyCode::Delete) => {
            // DROP selected item
            if let Some(level) = app.selected_level {
                // Execute DROP at that level
                // For now, just drop level 1 if that's selected
                if level == 1 {
                    let _ = app.session.eval_repl("DROP");
                    app.deselect();
                }
            }
        }

        (KeyModifiers::NONE, KeyCode::Char('D')) => {
            // DUP selected item to level 1
            if app.selected_level == Some(1) {
                let _ = app.session.eval_repl("DUP");
            }
        }

        // Deselect
        (KeyModifiers::NONE, KeyCode::Esc) => {
            app.deselect();
            app.focus = Focus::Editor;
        }

        // Number keys for quick level selection
        (KeyModifiers::NONE, KeyCode::Char(c)) if c.is_ascii_digit() => {
            let level = c.to_digit(10).unwrap() as usize;
            if level >= 1 {
                app.select_level(level);
            }
        }

        _ => {}
    }
}

/// Handle soft menu key press (F1-F6).
fn handle_soft_key(app: &mut App, key: u8) {
    // Menu changes based on context
    let command = match (app.focus, app.has_selection()) {
        (Focus::Editor, false) => {
            // New input mode
            match key {
                1 => "run",   // RUN (will be handled specially)
                2 => "CLEAR", // CLEAR
                3 => ":vars", // VAR
                4 => "",      // HIST - not implemented
                5 => "",      // MODE - not implemented
                6 => ":help", // HELP
                _ => "",
            }
        }
        (Focus::Editor, true) => {
            // Editing existing item
            match key {
                1 => "run",    // RUN
                2 => "apply",  // APPLY
                3 => "revert", // REVERT
                4 => "DROP",   // DROP
                5 => "DUP",    // DUP
                6 => ":help",  // HELP
                _ => "",
            }
        }
        (Focus::Stack, _) => {
            // Stack navigation
            match key {
                1 => "edit", // EDIT - switch to editor
                2 => "",     // VIEW - already viewing
                3 => "DROP", // DROP
                4 => "DUP",  // DUP
                5 => "SWAP", // SWAP
                6 => ":help",
                _ => "",
            }
        }
    };

    match command {
        "run" => {
            if app.run_editor() {
                app.running = false;
            }
        }
        "apply" => {
            if let Err(e) = app.apply_edit() {
                app.last_error = Some(e);
            }
        }
        "revert" => {
            app.revert_edit();
        }
        "edit" => {
            app.focus = Focus::Editor;
        }
        "" => {} // Not implemented
        cmd if cmd.starts_with(':') => {
            // Meta-command - execute directly
            let content = app.editor.content();
            app.editor = super::ui::MultilineEditor::with_content(cmd);
            let _ = app.run_editor();
            // Restore if it was a query command
            if !content.is_empty() {
                app.editor = super::ui::MultilineEditor::with_content(&content);
            }
        }
        cmd => {
            // Execute as RPL command
            let _ = app.session.eval_repl(cmd);
        }
    }
}
