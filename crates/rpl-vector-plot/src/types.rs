//! Core geometric types.

/// A point in 2D space.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
#[repr(C)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Point {
    pub const ZERO: Point = Point { x: 0.0, y: 0.0 };

    #[inline]
    pub const fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    /// Distance to another point.
    #[inline]
    pub fn distance(self, other: Point) -> f32 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }

    /// Linear interpolation between self and other.
    #[inline]
    pub fn lerp(self, other: Point, t: f32) -> Point {
        Point {
            x: self.x + (other.x - self.x) * t,
            y: self.y + (other.y - self.y) * t,
        }
    }
}

impl From<(f32, f32)> for Point {
    fn from((x, y): (f32, f32)) -> Self {
        Self { x, y }
    }
}

/// An axis-aligned rectangle.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
#[repr(C)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub w: f32,
    pub h: f32,
}

impl Rect {
    pub const ZERO: Rect = Rect { x: 0.0, y: 0.0, w: 0.0, h: 0.0 };
    pub const EMPTY: Rect = Rect { x: f32::INFINITY, y: f32::INFINITY, w: 0.0, h: 0.0 };

    #[inline]
    pub const fn new(x: f32, y: f32, w: f32, h: f32) -> Self {
        Self { x, y, w, h }
    }

    /// Create a rect from min/max coordinates.
    #[inline]
    pub fn from_bounds(min_x: f32, min_y: f32, max_x: f32, max_y: f32) -> Self {
        Self {
            x: min_x,
            y: min_y,
            w: max_x - min_x,
            h: max_y - min_y,
        }
    }

    #[inline]
    pub fn min_x(self) -> f32 { self.x }
    #[inline]
    pub fn min_y(self) -> f32 { self.y }
    #[inline]
    pub fn max_x(self) -> f32 { self.x + self.w }
    #[inline]
    pub fn max_y(self) -> f32 { self.y + self.h }

    #[inline]
    pub fn center(self) -> Point {
        Point::new(self.x + self.w * 0.5, self.y + self.h * 0.5)
    }

    /// Check if the rect is the EMPTY sentinel (used for bounding box computation).
    #[inline]
    pub fn is_empty(self) -> bool {
        // Only the sentinel is considered empty, not zero-area rects at valid positions
        self.x.is_infinite()
    }

    /// Expand rect to include a point.
    #[inline]
    pub fn include_point(&mut self, p: Point) {
        if self.is_empty() {
            self.x = p.x;
            self.y = p.y;
            self.w = 0.0;
            self.h = 0.0;
        } else {
            let min_x = self.x.min(p.x);
            let min_y = self.y.min(p.y);
            let max_x = self.max_x().max(p.x);
            let max_y = self.max_y().max(p.y);
            self.x = min_x;
            self.y = min_y;
            self.w = max_x - min_x;
            self.h = max_y - min_y;
        }
    }

    /// Union of two rects.
    #[inline]
    pub fn union(self, other: Rect) -> Rect {
        if self.is_empty() { return other; }
        if other.is_empty() { return self; }
        Rect::from_bounds(
            self.min_x().min(other.min_x()),
            self.min_y().min(other.min_y()),
            self.max_x().max(other.max_x()),
            self.max_y().max(other.max_y()),
        )
    }

    /// Check if point is inside rect.
    #[inline]
    pub fn contains(self, p: Point) -> bool {
        p.x >= self.x && p.x <= self.max_x() && p.y >= self.y && p.y <= self.max_y()
    }
}

/// RGBA color with 8-bit components.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    pub const WHITE: Color = Color::rgb(255, 255, 255);
    pub const BLACK: Color = Color::rgb(0, 0, 0);
    pub const TRANSPARENT: Color = Color::new(0, 0, 0, 0);
    pub const RED: Color = Color::rgb(255, 0, 0);
    pub const GREEN: Color = Color::rgb(0, 255, 0);
    pub const BLUE: Color = Color::rgb(0, 0, 255);

    #[inline]
    pub const fn new(r: u8, g: u8, b: u8, a: u8) -> Self {
        Self { r, g, b, a }
    }

    #[inline]
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 255 }
    }

    /// Unpack from 0xRRGGBBAA format.
    #[inline]
    pub const fn from_packed(rgba: u32) -> Self {
        Self {
            r: ((rgba >> 24) & 0xFF) as u8,
            g: ((rgba >> 16) & 0xFF) as u8,
            b: ((rgba >> 8) & 0xFF) as u8,
            a: (rgba & 0xFF) as u8,
        }
    }

    /// Pack to 0xRRGGBBAA format.
    #[inline]
    pub const fn to_packed(self) -> u32 {
        ((self.r as u32) << 24)
            | ((self.g as u32) << 16)
            | ((self.b as u32) << 8)
            | (self.a as u32)
    }
}

/// 2D affine transformation matrix.
///
/// Stored as `[a, b, c, d, e, f]` representing:
/// ```text
/// | a  b  e |
/// | c  d  f |
/// | 0  0  1 |
/// ```
///
/// Transform a point: `x' = a*x + b*y + e`, `y' = c*x + d*y + f`
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Transform([f32; 6]);

impl Default for Transform {
    fn default() -> Self {
        Self::IDENTITY
    }
}

impl Transform {
    pub const IDENTITY: Transform = Transform([1.0, 0.0, 0.0, 1.0, 0.0, 0.0]);

    #[inline]
    pub const fn new(a: f32, b: f32, c: f32, d: f32, e: f32, f: f32) -> Self {
        Self([a, b, c, d, e, f])
    }

    #[inline]
    pub fn translate(dx: f32, dy: f32) -> Self {
        Self([1.0, 0.0, 0.0, 1.0, dx, dy])
    }

    #[inline]
    pub fn scale(sx: f32, sy: f32) -> Self {
        Self([sx, 0.0, 0.0, sy, 0.0, 0.0])
    }

    #[inline]
    pub fn rotate(angle: f32) -> Self {
        let (sin, cos) = angle.sin_cos();
        Self([cos, -sin, sin, cos, 0.0, 0.0])
    }

    /// Compose two transforms: self * other
    #[inline]
    pub fn then(self, other: Transform) -> Transform {
        let [a1, b1, c1, d1, e1, f1] = self.0;
        let [a2, b2, c2, d2, e2, f2] = other.0;
        Transform([
            a1 * a2 + b1 * c2,
            a1 * b2 + b1 * d2,
            c1 * a2 + d1 * c2,
            c1 * b2 + d1 * d2,
            e1 * a2 + f1 * c2 + e2,
            e1 * b2 + f1 * d2 + f2,
        ])
    }

    /// Apply transform to a point.
    #[inline]
    pub fn apply(self, p: Point) -> Point {
        let [a, b, c, d, e, f] = self.0;
        Point::new(a * p.x + b * p.y + e, c * p.x + d * p.y + f)
    }

    /// Get the matrix components.
    #[inline]
    pub fn as_array(&self) -> &[f32; 6] {
        &self.0
    }

    /// Check if this is the identity transform.
    #[inline]
    pub fn is_identity(&self) -> bool {
        *self == Self::IDENTITY
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn point_distance() {
        let a = Point::new(0.0, 0.0);
        let b = Point::new(3.0, 4.0);
        assert!((a.distance(b) - 5.0).abs() < 1e-6);
    }

    #[test]
    fn rect_include_point() {
        let mut r = Rect::EMPTY;
        r.include_point(Point::new(10.0, 20.0));
        r.include_point(Point::new(30.0, 40.0));
        assert_eq!(r.x, 10.0);
        assert_eq!(r.y, 20.0);
        assert_eq!(r.w, 20.0);
        assert_eq!(r.h, 20.0);
    }

    #[test]
    fn color_pack_unpack() {
        let c = Color::new(0xAA, 0xBB, 0xCC, 0xDD);
        assert_eq!(c.to_packed(), 0xAABBCCDD);
        assert_eq!(Color::from_packed(0xAABBCCDD), c);
    }

    #[test]
    fn transform_identity() {
        let t = Transform::IDENTITY;
        let p = Point::new(10.0, 20.0);
        assert_eq!(t.apply(p), p);
    }

    #[test]
    fn transform_translate() {
        let t = Transform::translate(5.0, 10.0);
        let p = Point::new(1.0, 2.0);
        assert_eq!(t.apply(p), Point::new(6.0, 12.0));
    }

    #[test]
    fn transform_scale() {
        let t = Transform::scale(2.0, 3.0);
        let p = Point::new(10.0, 10.0);
        assert_eq!(t.apply(p), Point::new(20.0, 30.0));
    }

    #[test]
    fn transform_compose() {
        let t1 = Transform::translate(10.0, 0.0);
        let t2 = Transform::scale(2.0, 2.0);
        let composed = t1.then(t2);
        let p = Point::new(5.0, 5.0);
        // First translate: (15, 5), then scale: (30, 10)
        assert_eq!(composed.apply(p), Point::new(30.0, 10.0));
    }
}
