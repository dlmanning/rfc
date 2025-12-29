//! SR5 Plot renderer implementation.
//!
//! Implements the Renderer trait to render Plot objects
//! to the SR5 framebuffer.

use crate::hardware::Sr5Hardware;
use rpl_vector_plot::{Paint, Point, Renderer, Stroke, Transform};

// ============================================================================
// Drawing primitives
// ============================================================================

/// Draw a line using Bresenham's algorithm
pub fn draw_line(hw: &mut Sr5Hardware, x1: i32, y1: i32, x2: i32, y2: i32, color: u16) {
    let dx = (x2 - x1).abs();
    let dy = -(y2 - y1).abs();
    let sx = if x1 < x2 { 1 } else { -1 };
    let sy = if y1 < y2 { 1 } else { -1 };
    let mut err = dx + dy;

    let mut x = x1;
    let mut y = y1;

    loop {
        hw.plot(x, y, color);
        if x == x2 && y == y2 {
            break;
        }
        let e2 = 2 * err;
        if e2 >= dy {
            err += dy;
            x += sx;
        }
        if e2 <= dx {
            err += dx;
            y += sy;
        }
    }
}

/// Draw a circle using midpoint algorithm
pub fn draw_circle(hw: &mut Sr5Hardware, cx: i32, cy: i32, r: i32, color: u16, filled: bool) {
    if r <= 0 {
        hw.plot(cx, cy, color);
        return;
    }

    let mut x = r;
    let mut y = 0;
    let mut err = 1 - r;

    while x >= y {
        if filled {
            // Draw horizontal lines
            for px in cx - x..=cx + x {
                hw.plot(px, cy + y, color);
                hw.plot(px, cy - y, color);
            }
            for px in cx - y..=cx + y {
                hw.plot(px, cy + x, color);
                hw.plot(px, cy - x, color);
            }
        } else {
            // Draw 8 octant points
            hw.plot(cx + x, cy + y, color);
            hw.plot(cx - x, cy + y, color);
            hw.plot(cx + x, cy - y, color);
            hw.plot(cx - x, cy - y, color);
            hw.plot(cx + y, cy + x, color);
            hw.plot(cx - y, cy + x, color);
            hw.plot(cx + y, cy - x, color);
            hw.plot(cx - y, cy - x, color);
        }

        y += 1;
        if err < 0 {
            err += 2 * y + 1;
        } else {
            x -= 1;
            err += 2 * (y - x + 1);
        }
    }
}

// ============================================================================
// Plot Renderer
// ============================================================================

/// Shape in a path
enum PathShape {
    Line(i32, i32, i32, i32), // x1, y1, x2, y2
    Circle(i32, i32, i32),    // cx, cy, r
}

/// SR5 renderer - implements Renderer to render to framebuffer
pub struct Sr5Renderer<'a> {
    hw: &'a mut Sr5Hardware,
    shapes: Vec<PathShape>,
    pos: (i32, i32),
    start_pos: (i32, i32),
}

impl<'a> Sr5Renderer<'a> {
    pub fn new(hw: &'a mut Sr5Hardware) -> Self {
        Self {
            hw,
            shapes: Vec::new(),
            pos: (0, 0),
            start_pos: (0, 0),
        }
    }
}

/// Convert RGBA (0-255 each) to RGB555
fn rgba_to_555(r: u8, g: u8, b: u8, _a: u8) -> u16 {
    let r5 = (r >> 3) as u16;
    let g5 = (g >> 3) as u16;
    let b5 = (b >> 3) as u16;
    (r5 << 10) | (g5 << 5) | b5
}

/// Extract color from paint
fn paint_to_555(paint: &Paint) -> u16 {
    match paint {
        Paint::Solid(c) => rgba_to_555(c.r, c.g, c.b, c.a),
        Paint::LinearGradient { stops, .. } | Paint::RadialGradient { stops, .. } => {
            // Use first color of gradient as fallback
            stops
                .first()
                .map(|s| rgba_to_555(s.color.r, s.color.g, s.color.b, s.color.a))
                .unwrap_or(0x7FFF)
        }
    }
}

impl Renderer for Sr5Renderer<'_> {
    fn move_to(&mut self, p: Point) {
        self.pos = (p.x.round() as i32, p.y.round() as i32);
        self.start_pos = self.pos;
    }

    fn line_to(&mut self, p: Point) {
        let (x1, y1) = self.pos;
        let (x2, y2) = (p.x.round() as i32, p.y.round() as i32);
        self.shapes.push(PathShape::Line(x1, y1, x2, y2));
        self.pos = (x2, y2);
    }

    fn quad_to(&mut self, _ctrl: Point, end: Point) {
        // Simplified: just draw line to end point
        let (x1, y1) = self.pos;
        let (x2, y2) = (end.x.round() as i32, end.y.round() as i32);
        self.shapes.push(PathShape::Line(x1, y1, x2, y2));
        self.pos = (x2, y2);
    }

    fn cubic_to(&mut self, _c1: Point, _c2: Point, end: Point) {
        // Simplified: just draw line to end point
        let (x1, y1) = self.pos;
        let (x2, y2) = (end.x.round() as i32, end.y.round() as i32);
        self.shapes.push(PathShape::Line(x1, y1, x2, y2));
        self.pos = (x2, y2);
    }

    fn arc(&mut self, center: Point, radius: f32, _start_angle: f32, _sweep_angle: f32) {
        // For now, draw full circle (ignoring angles)
        self.shapes.push(PathShape::Circle(
            center.x.round() as i32,
            center.y.round() as i32,
            radius.round() as i32,
        ));
    }

    fn close_path(&mut self) {
        // Draw line back to start if not already there
        let (x1, y1) = self.pos;
        let (x2, y2) = self.start_pos;
        if x1 != x2 || y1 != y2 {
            self.shapes.push(PathShape::Line(x1, y1, x2, y2));
        }
        self.pos = self.start_pos;
    }

    fn fill(&mut self, paint: &Paint) {
        let color = paint_to_555(paint);
        for shape in &self.shapes {
            match shape {
                PathShape::Line(x1, y1, x2, y2) => {
                    draw_line(self.hw, *x1, *y1, *x2, *y2, color);
                }
                PathShape::Circle(cx, cy, r) => {
                    draw_circle(self.hw, *cx, *cy, *r, color, true);
                }
            }
        }
        self.shapes.clear();
    }

    fn stroke(&mut self, stroke: &Stroke) {
        let color = rgba_to_555(stroke.color.r, stroke.color.g, stroke.color.b, stroke.color.a);
        for shape in &self.shapes {
            match shape {
                PathShape::Line(x1, y1, x2, y2) => {
                    draw_line(self.hw, *x1, *y1, *x2, *y2, color);
                }
                PathShape::Circle(cx, cy, r) => {
                    draw_circle(self.hw, *cx, *cy, *r, color, false);
                }
            }
        }
        self.shapes.clear();
    }

    fn push_transform(&mut self, _t: &Transform) {
        // Transform not yet implemented
    }

    fn pop_transform(&mut self) {
        // Transform not yet implemented
    }

    fn text(&mut self, _pos: Point, _text: &str, _size: f32) {
        // Text rendering not yet implemented
    }

    fn clear_path(&mut self) {
        self.shapes.clear();
    }
}
