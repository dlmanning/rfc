//! Space Robot 5 Hardware
//!
//! Screen: Configurable resolution (default 400x240), 15-bit color (32768 colors)
//! RAM: 8 MB, VRAM: 1 MB, Audio RAM: 512 KB, Save RAM: 256 KB

use std::sync::{Arc, Mutex};

mod input;
mod sprite;
mod tiles;

pub use input::{buttons, InputState};
pub use sprite::{Sprite, SpriteBank, SpriteOptions};
pub use tiles::{
    TileLayer, TileMap, TileMapBank, TileSheet, TileSheetBank, NUM_TILE_LAYERS,
};

/// Shared hardware reference for libraries to access.
pub type HardwareRef = Arc<Mutex<Sr5Hardware>>;

/// Default screen width.
pub const DEFAULT_SCREEN_WIDTH: u32 = 400;
/// Default screen height.
pub const DEFAULT_SCREEN_HEIGHT: u32 = 240;

// Legacy aliases for compatibility
pub const SCREEN_WIDTH: u32 = DEFAULT_SCREEN_WIDTH;
pub const SCREEN_HEIGHT: u32 = DEFAULT_SCREEN_HEIGHT;

// Planned memory sizes (not yet allocated)
#[allow(dead_code)]
pub const RAM_SIZE: usize = 8 * 1024 * 1024; // 8 MB
#[allow(dead_code)]
pub const VRAM_SIZE: usize = 1024 * 1024; // 1 MB
#[allow(dead_code)]
pub const AUDIO_RAM_SIZE: usize = 512 * 1024; // 512 KB
#[allow(dead_code)]
pub const SAVE_RAM_SIZE: usize = 256 * 1024; // 256 KB

/// Convert RGB555 (15-bit) to RGBA8888 for display.
/// Color 0 is treated as transparent for GPU compositing.
#[inline]
pub fn rgb555_to_rgba(color: u16) -> [u8; 4] {
    // Color 0 is transparent (used as clear color for compositing)
    if color == 0 {
        return [0, 0, 0, 0];
    }
    let r = ((color & 0x7C00) >> 10) as u8;
    let g = ((color & 0x03E0) >> 5) as u8;
    let b = (color & 0x001F) as u8;
    // Expand 5-bit to 8-bit: (val << 3) | (val >> 2)
    [
        (r << 3) | (r >> 2),
        (g << 3) | (g >> 2),
        (b << 3) | (b >> 2),
        255,
    ]
}

/// Convert RGB components (0-31 each) to RGB555
#[inline]
pub fn rgb_to_555(r: u8, g: u8, b: u8) -> u16 {
    ((r as u16 & 0x1F) << 10) | ((g as u16 & 0x1F) << 5) | (b as u16 & 0x1F)
}

/// Static background image (rendered behind tile layers).
#[derive(Clone)]
pub struct Background {
    /// Width in pixels
    pub width: u32,
    /// Height in pixels
    pub height: u32,
    /// RGBA pixel data
    pub data: Vec<u8>,
    /// Generation counter (incremented when background changes)
    pub generation: u32,
}

/// SR5 Hardware state
pub struct Sr5Hardware {
    /// Screen width in pixels
    pub width: u32,

    /// Screen height in pixels
    pub height: u32,

    /// Framebuffer for direct pixel rendering (RGB555)
    pub framebuffer: Vec<u16>,

    /// Static background image (rendered behind tile layers)
    pub background: Option<Background>,

    /// Sprite storage
    pub sprites: SpriteBank,

    /// Tile sheet storage
    pub tilesheets: TileSheetBank,

    /// Tilemap storage
    pub tilemaps: TileMapBank,

    /// Tile layers (rendered back to front: 0, 1, 2, 3)
    pub layers: [TileLayer; NUM_TILE_LAYERS],

    /// Input state
    pub input: InputState,

    /// Frame counter (wraps at u64::MAX)
    pub frame_count: u64,

    /// Milliseconds since boot
    pub ticks: u64,

    /// VSYNC request flag (set by VSYNC command, cleared by Console)
    pub vsync_requested: bool,
}

impl Sr5Hardware {
    /// Create hardware with default resolution (400x240).
    pub fn new() -> Self {
        Self::with_resolution(DEFAULT_SCREEN_WIDTH, DEFAULT_SCREEN_HEIGHT)
    }

    /// Create hardware with custom resolution.
    pub fn with_resolution(width: u32, height: u32) -> Self {
        Self {
            width,
            height,
            framebuffer: vec![0u16; (width * height) as usize],
            background: None,
            sprites: SpriteBank::new(),
            tilesheets: TileSheetBank::new(),
            tilemaps: TileMapBank::new(),
            layers: Default::default(),
            input: InputState::default(),
            frame_count: 0,
            ticks: 0,
            vsync_requested: false,
        }
    }

    /// Set the background image.
    pub fn set_background(&mut self, width: u32, height: u32, data: Vec<u8>) {
        let generation = self.background.as_ref().map_or(1, |bg| bg.generation.wrapping_add(1));
        self.background = Some(Background {
            width,
            height,
            data,
            generation,
        });
    }

    /// Clear the background image.
    pub fn clear_background(&mut self) {
        self.background = None;
    }

    /// Set screen resolution. Only valid before first frame (in init).
    pub fn set_resolution(&mut self, width: u32, height: u32) -> Result<(), String> {
        if self.frame_count > 0 {
            return Err("SCREEN must be called in init (before first frame)".into());
        }
        self.width = width;
        self.height = height;
        self.framebuffer = vec![0u16; (width * height) as usize];
        Ok(())
    }

    /// Get current screen dimensions.
    pub fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    /// Clear the framebuffer to a color
    pub fn clear(&mut self, color: u16) {
        self.framebuffer.fill(color);
    }

    /// Plot a pixel at (x, y) with RGB555 color
    pub fn plot(&mut self, x: i32, y: i32, color: u16) {
        if x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32 {
            let idx = (y as usize) * (self.width as usize) + (x as usize);
            self.framebuffer[idx] = color;
        }
    }

    /// Get pixel at (x, y)
    pub fn get_pixel(&self, x: i32, y: i32) -> Option<u16> {
        if x >= 0 && x < self.width as i32 && y >= 0 && y < self.height as i32 {
            let idx = (y as usize) * (self.width as usize) + (x as usize);
            Some(self.framebuffer[idx])
        } else {
            None
        }
    }

    /// Render framebuffer to RGBA pixel buffer (for display)
    pub fn render_to_rgba(&self, output: &mut [u8]) {
        debug_assert!(output.len() >= (self.width * self.height * 4) as usize);
        for (i, &pixel) in self.framebuffer.iter().enumerate() {
            let rgba = rgb555_to_rgba(pixel);
            let offset = i * 4;
            output[offset..offset + 4].copy_from_slice(&rgba);
        }
    }

    /// Advance frame counter and update ticks
    pub fn next_frame(&mut self, delta_ms: u64) {
        self.frame_count = self.frame_count.wrapping_add(1);
        self.ticks = self.ticks.wrapping_add(delta_ms);
    }

    // ========================================================================
    // Sprite Rendering
    // ========================================================================

    /// Draw a sprite at (x, y) with alpha blending.
    pub fn blit_sprite(&mut self, id: usize, x: i32, y: i32) {
        self.blit_sprite_ex(id, x, y, SpriteOptions::default());
    }

    /// Draw a sprite with flip flags (1=H, 2=V, 3=both).
    pub fn blit_sprite_flip(&mut self, id: usize, x: i32, y: i32, flags: u8) {
        self.blit_sprite_ex(id, x, y, SpriteOptions::from_flags(flags));
    }

    /// Draw a sprite scaled (scale is 8.8 fixed point, 256 = 1.0).
    pub fn blit_sprite_scaled(&mut self, id: usize, x: i32, y: i32, scale_x: i32, scale_y: i32) {
        self.blit_sprite_ex(
            id,
            x,
            y,
            SpriteOptions {
                scale_x,
                scale_y,
                ..Default::default()
            },
        );
    }

    /// Draw a sprite rotated (angle in degrees).
    pub fn blit_sprite_rotated(&mut self, id: usize, x: i32, y: i32, angle: i32) {
        self.blit_sprite_ex(id, x, y, SpriteOptions::rotated(angle));
    }

    /// Draw a sprite with full control via options.
    pub fn blit_sprite_ex(&mut self, id: usize, x: i32, y: i32, opts: SpriteOptions) {
        // Clone sprite to avoid borrow conflict
        let Some(sprite) = self.sprites.get(id).cloned() else {
            return;
        };

        // Simple case: no rotation, 1:1 scale
        if opts.angle == 0 && opts.scale_x == 256 && opts.scale_y == 256 {
            self.blit_simple(&sprite, x, y, &opts);
            return;
        }

        // Scaled case (no rotation)
        if opts.angle == 0 {
            self.blit_scaled(&sprite, x, y, &opts);
            return;
        }

        // Full rotation + scale
        self.blit_rotated(&sprite, x, y, &opts);
    }

    /// Simple blit without rotation or scaling.
    fn blit_simple(&mut self, sprite: &Sprite, x: i32, y: i32, opts: &SpriteOptions) {
        let sw = sprite.width as i32;
        let sh = sprite.height as i32;

        for sy in 0..sh {
            for sx in 0..sw {
                let src_x = if opts.flip_h { sw - 1 - sx } else { sx };
                let src_y = if opts.flip_v { sh - 1 - sy } else { sy };
                let src_idx = ((src_y * sw + src_x) * 4) as usize;

                let r = sprite.data[src_idx];
                let g = sprite.data[src_idx + 1];
                let b = sprite.data[src_idx + 2];
                let a = sprite.data[src_idx + 3];

                if a > 0 {
                    self.blend_pixel(x + sx, y + sy, r, g, b, a);
                }
            }
        }
    }

    /// Scaled blit using nearest-neighbor sampling.
    fn blit_scaled(&mut self, sprite: &Sprite, x: i32, y: i32, opts: &SpriteOptions) {
        let sw = sprite.width as i32;
        let sh = sprite.height as i32;
        let dw = (sw * opts.scale_x) >> 8;
        let dh = (sh * opts.scale_y) >> 8;

        for dy in 0..dh {
            for dx in 0..dw {
                // Map destination to source (nearest neighbor)
                let mut src_x = (dx << 8) / opts.scale_x;
                let mut src_y = (dy << 8) / opts.scale_y;

                if opts.flip_h {
                    src_x = sw - 1 - src_x;
                }
                if opts.flip_v {
                    src_y = sh - 1 - src_y;
                }

                if src_x >= 0 && src_x < sw && src_y >= 0 && src_y < sh {
                    let src_idx = ((src_y * sw + src_x) * 4) as usize;
                    let r = sprite.data[src_idx];
                    let g = sprite.data[src_idx + 1];
                    let b = sprite.data[src_idx + 2];
                    let a = sprite.data[src_idx + 3];

                    if a > 0 {
                        self.blend_pixel(x + dx, y + dy, r, g, b, a);
                    }
                }
            }
        }
    }

    /// Rotated blit with scaling.
    fn blit_rotated(&mut self, sprite: &Sprite, x: i32, y: i32, opts: &SpriteOptions) {
        let sw = sprite.width as i32;
        let sh = sprite.height as i32;

        // Compute sin/cos (angle in degrees)
        let rad = (opts.angle as f32) * std::f32::consts::PI / 180.0;
        let cos_a = rad.cos();
        let sin_a = rad.sin();

        // Scaled dimensions
        let dw = (sw * opts.scale_x) >> 8;
        let dh = (sh * opts.scale_y) >> 8;

        // Center of sprite
        let cx = dw / 2;
        let cy = dh / 2;

        // Bounding box for rotated sprite
        let corners = [
            (-cx, -cy),
            (dw - cx, -cy),
            (-cx, dh - cy),
            (dw - cx, dh - cy),
        ];
        let mut min_x = i32::MAX;
        let mut max_x = i32::MIN;
        let mut min_y = i32::MAX;
        let mut max_y = i32::MIN;

        for (px, py) in corners {
            let rx = (px as f32 * cos_a - py as f32 * sin_a) as i32;
            let ry = (px as f32 * sin_a + py as f32 * cos_a) as i32;
            min_x = min_x.min(rx);
            max_x = max_x.max(rx);
            min_y = min_y.min(ry);
            max_y = max_y.max(ry);
        }

        // Iterate over bounding box
        for dy in min_y..=max_y {
            for dx in min_x..=max_x {
                // Reverse rotation to find source pixel
                let ux = (dx as f32 * cos_a + dy as f32 * sin_a) + cx as f32;
                let uy = (-dx as f32 * sin_a + dy as f32 * cos_a) + cy as f32;

                // Map to source coordinates
                let mut src_x = ((ux as i32) << 8) / opts.scale_x;
                let mut src_y = ((uy as i32) << 8) / opts.scale_y;

                if opts.flip_h {
                    src_x = sw - 1 - src_x;
                }
                if opts.flip_v {
                    src_y = sh - 1 - src_y;
                }

                if src_x >= 0 && src_x < sw && src_y >= 0 && src_y < sh {
                    let src_idx = ((src_y * sw + src_x) * 4) as usize;
                    let r = sprite.data[src_idx];
                    let g = sprite.data[src_idx + 1];
                    let b = sprite.data[src_idx + 2];
                    let a = sprite.data[src_idx + 3];

                    if a > 0 {
                        self.blend_pixel(x + dx, y + dy, r, g, b, a);
                    }
                }
            }
        }
    }

    /// Blend a single RGBA pixel onto the framebuffer.
    fn blend_pixel(&mut self, x: i32, y: i32, r: u8, g: u8, b: u8, a: u8) {
        if x < 0 || x >= self.width as i32 || y < 0 || y >= self.height as i32 {
            return;
        }

        let idx = (y as usize) * (self.width as usize) + (x as usize);

        if a == 255 {
            // Fully opaque - direct write
            let r5 = (r >> 3) as u16;
            let g5 = (g >> 3) as u16;
            let b5 = (b >> 3) as u16;
            self.framebuffer[idx] = (r5 << 10) | (g5 << 5) | b5;
        } else {
            // Alpha blend
            let dst = self.framebuffer[idx];
            let dst_rgba = rgb555_to_rgba(dst);

            let alpha = a as u16;
            let inv_alpha = 255 - alpha;

            let out_r = ((r as u16 * alpha + dst_rgba[0] as u16 * inv_alpha) / 255) as u8;
            let out_g = ((g as u16 * alpha + dst_rgba[1] as u16 * inv_alpha) / 255) as u8;
            let out_b = ((b as u16 * alpha + dst_rgba[2] as u16 * inv_alpha) / 255) as u8;

            let r5 = (out_r >> 3) as u16;
            let g5 = (out_g >> 3) as u16;
            let b5 = (out_b >> 3) as u16;
            self.framebuffer[idx] = (r5 << 10) | (g5 << 5) | b5;
        }
    }

    // ========================================================================
    // PNG Blitting
    // ========================================================================

    /// Decode and blit a PNG centered on screen.
    pub fn blit_png_centered(&mut self, png_data: &[u8]) {
        let decoder = png::Decoder::new(png_data);
        let mut reader = match decoder.read_info() {
            Ok(r) => r,
            Err(_) => return,
        };

        let mut buf = vec![0; reader.output_buffer_size()];
        let info = match reader.next_frame(&mut buf) {
            Ok(i) => i,
            Err(_) => return,
        };

        let img_width = info.width as i32;
        let img_height = info.height as i32;
        let x_offset = (self.width as i32 - img_width) / 2;
        let y_offset = (self.height as i32 - img_height) / 2;

        match info.color_type {
            png::ColorType::Rgba => {
                for y in 0..img_height {
                    for x in 0..img_width {
                        let idx = ((y * img_width + x) * 4) as usize;
                        let a = buf[idx + 3];
                        if a > 0 {
                            let color = rgb8_to_555(buf[idx], buf[idx + 1], buf[idx + 2]);
                            self.plot(x_offset + x, y_offset + y, color);
                        }
                    }
                }
            }
            png::ColorType::Rgb => {
                for y in 0..img_height {
                    for x in 0..img_width {
                        let idx = ((y * img_width + x) * 3) as usize;
                        let color = rgb8_to_555(buf[idx], buf[idx + 1], buf[idx + 2]);
                        self.plot(x_offset + x, y_offset + y, color);
                    }
                }
            }
            _ => {}
        }
    }
}

/// Convert 8-bit RGB to RGB555.
#[inline]
fn rgb8_to_555(r: u8, g: u8, b: u8) -> u16 {
    let r5 = (r >> 3) as u16;
    let g5 = (g >> 3) as u16;
    let b5 = (b >> 3) as u16;
    (r5 << 10) | (g5 << 5) | b5
}

impl Default for Sr5Hardware {
    fn default() -> Self {
        Self::new()
    }
}
