//! SR5 Fantasy Console
//!
//! The `libraries::interfaces` module is always available for IDE/analysis use.
//! Other modules require the `runtime` feature.

pub mod libraries;

#[cfg(feature = "runtime")]
pub mod console;
#[cfg(feature = "runtime")]
pub mod hardware;
#[cfg(feature = "runtime")]
pub mod renderer;

pub use libraries::interfaces::register_interfaces;

#[cfg(feature = "runtime")]
pub use console::Console;
#[cfg(feature = "runtime")]
pub use hardware::{HardwareRef, SCREEN_HEIGHT, SCREEN_WIDTH, Sr5Hardware};
#[cfg(feature = "runtime")]
pub use libraries::register_sr5_libs;
