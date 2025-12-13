//! RPL REPL - Interactive Read-Eval-Print Loop.
//!
//! This module provides a terminal-based REPL for the RPL language using ratatui.

mod app;
mod commands;
mod event;
pub mod input;
pub mod plot_decoder;
pub mod ui;

pub use app::App;

use std::io;

/// Run the REPL.
pub fn run() -> io::Result<()> {
    let mut app = App::new();
    event::run_event_loop(&mut app)
}
