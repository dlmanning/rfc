//! RPL REPL - Interactive Read-Eval-Print Loop.
//!
//! A full-featured terminal UI for the RPL language with:
//! - Split-pane display (stack, variables, history, input)
//! - Multi-line editor mode (Ctrl-O)
//! - Readline-style editing with history

fn main() {
    if let Err(e) = rpl_repl::run() {
        eprintln!("REPL error: {}", e);
        std::process::exit(1);
    }
}
