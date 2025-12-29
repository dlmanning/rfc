use std::path::Path;
use std::sync::{Arc, Mutex};

use rpl::Session;
use rpl_project::{Manifest, Project};

use crate::hardware::{HardwareRef, Sr5Hardware};
use crate::libraries::register_sr5_libs;

/// SR5 Console - wraps VM session and hardware state
pub struct Console {
    /// RPL session for compilation and execution
    pub session: Session,

    /// Hardware state (shared with libraries)
    pub hardware: HardwareRef,

    /// Loaded project manifest (if any)
    pub manifest: Option<Manifest>,
}

impl Console {
    pub fn new() -> Self {
        let hardware = Arc::new(Mutex::new(Sr5Hardware::new()));
        let session = Self::create_session(hardware.clone());

        Self {
            session,
            hardware,
            manifest: None,
        }
    }

    /// Create a new session with stdlib and SR5 libraries registered.
    fn create_session(hardware: HardwareRef) -> Session {
        let mut session = Session::new();

        // Register stdlib
        rpl_stdlib::register_interfaces(session.interfaces_mut());
        rpl_stdlib::register_lowerers(session.lowerers_mut());
        rpl_stdlib::register_executors(session.executors_mut());

        // Register SR5-specific libraries
        register_sr5_libs(&mut session, hardware);

        session
    }

    /// Load a project from a directory containing `project.toml`.
    pub fn load_project(&mut self, project_dir: impl AsRef<Path>) -> Result<(), String> {
        // Take our configured session and use it to load the project
        let session = std::mem::replace(
            &mut self.session,
            Self::create_session(self.hardware.clone()),
        );

        let project = Project::load_into(session, project_dir)
            .map_err(|e| format!("Failed to load project: {}", e))?;

        // Put session and manifest back
        self.session = project.session;
        self.manifest = Some(project.manifest.clone());

        Ok(())
    }

    /// Initialize the project by calling the `init` entry point if it exists.
    pub fn init(&mut self) -> Result<(), String> {
        if self.manifest.is_none() {
            return Err("No project loaded".to_string());
        }

        // Call init if it exists
        if self.session.vm().directory.lookup("init").is_some() {
            self.session
                .eval("'init' RCL EVAL")
                .map_err(|e| format!("{}", e))?;
        }

        Ok(())
    }

    /// Call the `update` entry point (called each frame before draw).
    pub fn update(&mut self) -> Result<(), String> {
        if self.session.vm().directory.lookup("update").is_some() {
            self.session
                .eval("'update' RCL EVAL")
                .map_err(|e| format!("{}", e))?;
        }
        Ok(())
    }

    /// Call the `draw` entry point (called each frame after update).
    /// Clears framebuffer, then executes draw.rpl for sprites/UI.
    /// Note: Tile layers are rendered by GPU, not here.
    pub fn draw(&mut self) -> Result<(), String> {
        if let Ok(mut hw) = self.hardware.lock() {
            // Clear framebuffer (tile layers rendered by GPU separately)
            hw.clear(0);
        }

        // Execute draw.rpl for sprites and dynamic elements
        if self.session.vm().directory.lookup("draw").is_some() {
            self.session
                .eval("'draw' RCL EVAL")
                .map_err(|e| format!("{}", e))?;
        }
        Ok(())
    }

    /// Draw the SR5 logo directly to the framebuffer.
    pub fn draw_logo(&mut self) {
        const LOGO_PNG: &[u8] = include_bytes!("../sr5.png");
        if let Ok(mut hw) = self.hardware.lock() {
            hw.blit_png_centered(LOGO_PNG);
        }
    }

    /// Update input state from window events
    pub fn update_input(&mut self, buttons: u16) {
        if let Ok(mut hw) = self.hardware.lock() {
            hw.input.update(buttons);
        }
    }

    /// Advance to next frame
    pub fn next_frame(&mut self, delta_ms: u64) {
        if let Ok(mut hw) = self.hardware.lock() {
            hw.next_frame(delta_ms);
        }
    }

    /// Get the framebuffer rendered to RGBA for display
    pub fn render(&self, output: &mut [u8]) {
        if let Ok(hw) = self.hardware.lock() {
            hw.render_to_rgba(output);
        }
    }
}

impl Default for Console {
    fn default() -> Self {
        Self::new()
    }
}
