//! RPL TUI - Interactive calculator interface.
//!
//! A full-featured terminal UI for the RPL language with:
//! - Split-pane display (stack, variables, history, input)
//! - Multi-line editor mode (Ctrl-O)
//! - Readline-style editing with history

fn main() {
    if let Err(e) = rpl_tui::run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
