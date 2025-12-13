//! Input handling module.
//!
//! This module contains:
//! - readline.rs - Full readline-like editing
//! - kill_ring.rs - Kill ring for Ctrl-K/Y operations
//! - history.rs - History persistence and search

mod history;
mod kill_ring;
mod readline;

pub use history::{default_history_path, History, HistorySearch};
pub use kill_ring::KillRing;
pub use readline::InputLine;
