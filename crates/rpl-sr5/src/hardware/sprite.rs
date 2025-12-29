//! Sprite system for SR5.

/// A sprite stored in VRAM.
#[derive(Clone)]
pub struct Sprite {
    pub width: u16,
    pub height: u16,
    /// RGBA data, 4 bytes per pixel
    pub data: Vec<u8>,
}

/// Options for sprite rendering.
#[derive(Clone, Copy, Debug)]
pub struct SpriteOptions {
    /// X scale (8.8 fixed point, 256 = 1.0)
    pub scale_x: i32,
    /// Y scale (8.8 fixed point, 256 = 1.0)
    pub scale_y: i32,
    /// Rotation angle in degrees
    pub angle: i32,
    /// Flip horizontally
    pub flip_h: bool,
    /// Flip vertically
    pub flip_v: bool,
}

impl Default for SpriteOptions {
    fn default() -> Self {
        Self {
            scale_x: 256,
            scale_y: 256,
            angle: 0,
            flip_h: false,
            flip_v: false,
        }
    }
}

impl SpriteOptions {
    /// Create options with rotation.
    pub fn rotated(angle: i32) -> Self {
        Self {
            angle,
            ..Default::default()
        }
    }

    /// Create options from flip flags (1=H, 2=V, 3=both).
    pub fn from_flags(flags: u8) -> Self {
        Self {
            flip_h: flags & 1 != 0,
            flip_v: flags & 2 != 0,
            ..Default::default()
        }
    }
}

/// Sprite storage bank.
pub struct SpriteBank {
    /// Sparse array of sprites (None = free slot)
    sprites: Vec<Option<Sprite>>,
    /// Hint for next free slot
    next_id: usize,
}

impl SpriteBank {
    pub fn new() -> Self {
        Self {
            sprites: Vec::new(),
            next_id: 0,
        }
    }

    /// Allocate a new sprite, returns its ID.
    pub fn alloc(&mut self, sprite: Sprite) -> usize {
        // Find a free slot starting from hint
        for i in self.next_id..self.sprites.len() {
            if self.sprites[i].is_none() {
                self.sprites[i] = Some(sprite);
                self.next_id = i + 1;
                return i;
            }
        }
        // No free slot, append
        let id = self.sprites.len();
        self.sprites.push(Some(sprite));
        self.next_id = id + 1;
        id
    }

    /// Free a sprite slot.
    pub fn free(&mut self, id: usize) {
        if id < self.sprites.len() {
            self.sprites[id] = None;
            if id < self.next_id {
                self.next_id = id;
            }
        }
    }

    /// Get a sprite by ID.
    pub fn get(&self, id: usize) -> Option<&Sprite> {
        self.sprites.get(id).and_then(|s| s.as_ref())
    }
}

impl Default for SpriteBank {
    fn default() -> Self {
        Self::new()
    }
}
