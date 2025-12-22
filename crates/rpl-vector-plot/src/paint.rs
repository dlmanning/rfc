//! Paint and stroke styles.

use crate::types::{Color, Point};

/// A gradient color stop.
#[derive(Clone, Debug, PartialEq)]
pub struct GradientStop {
    pub offset: f32, // 0.0 to 1.0
    pub color: Color,
}

impl GradientStop {
    pub fn new(offset: f32, color: Color) -> Self {
        Self { offset, color }
    }
}

/// Paint style for fills.
#[derive(Clone, Debug, PartialEq)]
pub enum Paint {
    /// Solid color fill.
    Solid(Color),
    /// Linear gradient.
    LinearGradient {
        start: Point,
        end: Point,
        stops: Vec<GradientStop>,
    },
    /// Radial gradient.
    RadialGradient {
        center: Point,
        radius: f32,
        stops: Vec<GradientStop>,
    },
}

impl Default for Paint {
    fn default() -> Self {
        Paint::Solid(Color::BLACK)
    }
}

impl From<Color> for Paint {
    fn from(c: Color) -> Self {
        Paint::Solid(c)
    }
}

impl Paint {
    /// Create a solid color paint.
    pub fn solid(color: Color) -> Self {
        Paint::Solid(color)
    }

    /// Create a linear gradient.
    pub fn linear_gradient(start: Point, end: Point, stops: Vec<GradientStop>) -> Self {
        Paint::LinearGradient { start, end, stops }
    }

    /// Create a radial gradient.
    pub fn radial_gradient(center: Point, radius: f32, stops: Vec<GradientStop>) -> Self {
        Paint::RadialGradient { center, radius, stops }
    }
}

/// Stroke style.
#[derive(Clone, Debug, PartialEq)]
pub struct Stroke {
    pub width: f32,
    pub color: Color,
    pub line_cap: LineCap,
    pub line_join: LineJoin,
}

impl Default for Stroke {
    fn default() -> Self {
        Self {
            width: 1.0,
            color: Color::BLACK,
            line_cap: LineCap::Butt,
            line_join: LineJoin::Miter,
        }
    }
}

impl Stroke {
    pub fn new(color: Color, width: f32) -> Self {
        Self {
            width,
            color,
            line_cap: LineCap::Butt,
            line_join: LineJoin::Miter,
        }
    }

    pub fn with_cap(mut self, cap: LineCap) -> Self {
        self.line_cap = cap;
        self
    }

    pub fn with_join(mut self, join: LineJoin) -> Self {
        self.line_join = join;
        self
    }
}

/// Line cap style.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum LineCap {
    #[default]
    Butt,
    Round,
    Square,
}

/// Line join style.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum LineJoin {
    #[default]
    Miter,
    Round,
    Bevel,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paint_solid() {
        let p = Paint::solid(Color::RED);
        assert!(matches!(p, Paint::Solid(c) if c == Color::RED));
    }

    #[test]
    fn stroke_default() {
        let s = Stroke::default();
        assert_eq!(s.width, 1.0);
        assert_eq!(s.color, Color::BLACK);
    }

    #[test]
    fn gradient_stop() {
        let stop = GradientStop::new(0.5, Color::BLUE);
        assert_eq!(stop.offset, 0.5);
        assert_eq!(stop.color, Color::BLUE);
    }
}
