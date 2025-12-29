//! SR5 libraries.
//!
//! The `interfaces` submodule is always available.
//! The library implementations require the `runtime` feature.

pub mod interfaces;

#[cfg(feature = "runtime")]
mod graphics;
#[cfg(feature = "runtime")]
mod input;
#[cfg(feature = "runtime")]
mod sprites;
#[cfg(feature = "runtime")]
mod system;
#[cfg(feature = "runtime")]
mod tiles;
#[cfg(feature = "runtime")]
mod ui;

#[cfg(feature = "runtime")]
use std::sync::{Arc, Mutex};

#[cfg(feature = "runtime")]
pub use graphics::Sr5GraphicsLib;
#[cfg(feature = "runtime")]
pub use input::Sr5InputLib;
#[cfg(feature = "runtime")]
pub use sprites::Sr5SpritesLib;
#[cfg(feature = "runtime")]
pub use system::Sr5SystemLib;
#[cfg(feature = "runtime")]
pub use tiles::Sr5TilesLib;
#[cfg(feature = "runtime")]
pub use ui::Sr5UiLib;

#[cfg(feature = "runtime")]
use crate::hardware::Sr5Hardware;
#[cfg(feature = "runtime")]
use rpl::Session;

/// Register all SR5 libraries with the session.
#[cfg(feature = "runtime")]
pub fn register_sr5_libs(session: &mut Session, hardware: Arc<Mutex<Sr5Hardware>>) {
    // Register graphics library
    session
        .interfaces_mut()
        .add(Sr5GraphicsLib::new(hardware.clone()));
    session
        .lowerers_mut()
        .add(Sr5GraphicsLib::new(hardware.clone()));
    session
        .executors_mut()
        .add(Sr5GraphicsLib::new(hardware.clone()));

    // Register input library
    session
        .interfaces_mut()
        .add(Sr5InputLib::new(hardware.clone()));
    session
        .lowerers_mut()
        .add(Sr5InputLib::new(hardware.clone()));
    session
        .executors_mut()
        .add(Sr5InputLib::new(hardware.clone()));

    // Register sprites library
    session
        .interfaces_mut()
        .add(Sr5SpritesLib::new(hardware.clone()));
    session
        .lowerers_mut()
        .add(Sr5SpritesLib::new(hardware.clone()));
    session
        .executors_mut()
        .add(Sr5SpritesLib::new(hardware.clone()));

    // Register tiles library
    session
        .interfaces_mut()
        .add(Sr5TilesLib::new(hardware.clone()));
    session
        .lowerers_mut()
        .add(Sr5TilesLib::new(hardware.clone()));
    session
        .executors_mut()
        .add(Sr5TilesLib::new(hardware.clone()));

    // Register system library
    session
        .interfaces_mut()
        .add(Sr5SystemLib::new(hardware.clone()));
    session
        .lowerers_mut()
        .add(Sr5SystemLib::new(hardware.clone()));
    session
        .executors_mut()
        .add(Sr5SystemLib::new(hardware.clone()));

    // Register UI library (overrides stdlib UI with hardware-backed implementation)
    session
        .interfaces_mut()
        .add(Sr5UiLib::new(hardware.clone()));
    session.lowerers_mut().add(Sr5UiLib::new(hardware.clone()));
    session.executors_mut().add(Sr5UiLib::new(hardware));

    // Register plot library from rpl-vector-plot crate
    rpl_vector_plot::register_vector_plot_lib(session);
}
