//! RGBA color type.

/// RGBA color with 8-bit components.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    /// Create a new color with explicit RGBA components.
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    /// Create an opaque color (alpha = 255).
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 255 }
    }

    /// Unpack a 32-bit RGBA value (0xRRGGBBAA format).
    pub const fn from_packed(rgba: u32) -> Self {
        Self {
            r: ((rgba >> 24) & 0xFF) as u8,
            g: ((rgba >> 16) & 0xFF) as u8,
            b: ((rgba >> 8) & 0xFF) as u8,
            a: (rgba & 0xFF) as u8,
        }
    }

    /// Pack into a 32-bit RGBA value (0xRRGGBBAA format).
    pub const fn to_packed(self) -> u32 {
        ((self.r as u32) << 24)
            | ((self.g as u32) << 16)
            | ((self.b as u32) << 8)
            | (self.a as u32)
    }

    // Common colors
    pub const WHITE: Color = Color::rgb(255, 255, 255);
    pub const BLACK: Color = Color::rgb(0, 0, 0);
    pub const TRANSPARENT: Color = Color::new(0, 0, 0, 0);
    pub const RED: Color = Color::rgb(255, 0, 0);
    pub const GREEN: Color = Color::rgb(0, 255, 0);
    pub const BLUE: Color = Color::rgb(0, 0, 255);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pack_unpack_roundtrip() {
        let colors = [
            Color::WHITE,
            Color::BLACK,
            Color::TRANSPARENT,
            Color::new(0x12, 0x34, 0x56, 0x78),
        ];
        for color in colors {
            let packed = color.to_packed();
            let unpacked = Color::from_packed(packed);
            assert_eq!(color, unpacked);
        }
    }

    #[test]
    fn test_packed_format() {
        let color = Color::new(0xAA, 0xBB, 0xCC, 0xDD);
        assert_eq!(color.to_packed(), 0xAABBCCDD);
    }
}
