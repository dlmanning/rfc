//! Input handling for SR5.

/// Button bit flags
pub mod buttons {
    pub const A: u16 = 0x0001;
    pub const B: u16 = 0x0002;
    pub const X: u16 = 0x0004;
    pub const Y: u16 = 0x0008;
    pub const L: u16 = 0x0010;
    pub const R: u16 = 0x0020;
    pub const START: u16 = 0x0040;
    pub const SELECT: u16 = 0x0080;
    pub const UP: u16 = 0x0100;
    pub const DOWN: u16 = 0x0200;
    pub const LEFT: u16 = 0x0400;
    pub const RIGHT: u16 = 0x0800;
    pub const ENTER: u16 = 0x1000;
    pub const MODE: u16 = 0x2000;
    pub const VARS: u16 = 0x4000;
}

/// Input state tracking
#[derive(Debug, Default)]
pub struct InputState {
    /// Current button state (bitmask)
    pub current: u16,
    /// Previous frame's button state
    pub previous: u16,
}

impl InputState {
    /// Update input state for new frame
    pub fn update(&mut self, new_state: u16) {
        self.previous = self.current;
        self.current = new_state;
    }

    /// Get buttons that are currently held
    pub fn held(&self) -> u16 {
        self.current
    }

    /// Get buttons that were just pressed this frame
    pub fn pressed(&self) -> u16 {
        self.current & !self.previous
    }

    /// Get buttons that were just released this frame
    pub fn released(&self) -> u16 {
        !self.current & self.previous
    }
}
