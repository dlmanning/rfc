//! Path representation and commands.

use crate::types::{Point, Rect};

/// A path command.
#[derive(Clone, Debug, PartialEq)]
pub enum PathCmd {
    /// Move to a point (starts a new subpath).
    MoveTo(Point),
    /// Line to a point.
    LineTo(Point),
    /// Quadratic Bézier curve.
    QuadTo { ctrl: Point, end: Point },
    /// Cubic Bézier curve.
    CubicTo { c1: Point, c2: Point, end: Point },
    /// Arc (circular).
    Arc {
        center: Point,
        radius: f32,
        start_angle: f32,
        sweep_angle: f32,
    },
    /// Close the current subpath.
    Close,
}

/// A path consisting of multiple commands.
#[derive(Clone, Debug, Default)]
pub struct Path {
    cmds: Vec<PathCmd>,
    bounds: Rect,
    current: Point,
    subpath_start: Point,
}

impl Path {
    /// Create an empty path.
    pub fn new() -> Self {
        Self {
            cmds: Vec::new(),
            bounds: Rect::EMPTY,
            current: Point::ZERO,
            subpath_start: Point::ZERO,
        }
    }

    /// Get the path commands.
    pub fn commands(&self) -> &[PathCmd] {
        &self.cmds
    }

    /// Get the bounding box of the path.
    pub fn bounds(&self) -> Rect {
        self.bounds
    }

    /// Check if the path is empty.
    pub fn is_empty(&self) -> bool {
        self.cmds.is_empty()
    }

    /// Clear the path.
    pub fn clear(&mut self) {
        self.cmds.clear();
        self.bounds = Rect::EMPTY;
        self.current = Point::ZERO;
        self.subpath_start = Point::ZERO;
    }

    /// Move to a point (starts a new subpath).
    pub fn move_to(&mut self, p: Point) {
        self.cmds.push(PathCmd::MoveTo(p));
        self.bounds.include_point(p);
        self.current = p;
        self.subpath_start = p;
    }

    /// Line to a point.
    pub fn line_to(&mut self, p: Point) {
        self.cmds.push(PathCmd::LineTo(p));
        self.bounds.include_point(p);
        self.current = p;
    }

    /// Quadratic Bézier curve.
    pub fn quad_to(&mut self, ctrl: Point, end: Point) {
        self.cmds.push(PathCmd::QuadTo { ctrl, end });
        // Conservative bounds: include control point and end point
        self.bounds.include_point(ctrl);
        self.bounds.include_point(end);
        self.current = end;
    }

    /// Cubic Bézier curve.
    pub fn cubic_to(&mut self, c1: Point, c2: Point, end: Point) {
        self.cmds.push(PathCmd::CubicTo { c1, c2, end });
        // Conservative bounds: include all control points
        self.bounds.include_point(c1);
        self.bounds.include_point(c2);
        self.bounds.include_point(end);
        self.current = end;
    }

    /// Arc (circular).
    pub fn arc(&mut self, center: Point, radius: f32, start_angle: f32, sweep_angle: f32) {
        self.cmds.push(PathCmd::Arc {
            center,
            radius,
            start_angle,
            sweep_angle,
        });
        // Conservative bounds: use full circle bounds
        self.bounds.include_point(Point::new(center.x - radius, center.y - radius));
        self.bounds.include_point(Point::new(center.x + radius, center.y + radius));
        // Update current point to arc endpoint
        let end_angle = start_angle + sweep_angle;
        self.current = Point::new(
            center.x + radius * end_angle.cos(),
            center.y + radius * end_angle.sin(),
        );
    }

    /// Close the current subpath.
    pub fn close(&mut self) {
        self.cmds.push(PathCmd::Close);
        self.current = self.subpath_start;
    }

    /// Add a circle as a subpath.
    pub fn circle(&mut self, center: Point, radius: f32) {
        use std::f32::consts::PI;
        self.move_to(Point::new(center.x + radius, center.y));
        self.arc(center, radius, 0.0, 2.0 * PI);
        self.close();
    }

    /// Add a rectangle as a subpath.
    pub fn rect(&mut self, x: f32, y: f32, w: f32, h: f32) {
        self.move_to(Point::new(x, y));
        self.line_to(Point::new(x + w, y));
        self.line_to(Point::new(x + w, y + h));
        self.line_to(Point::new(x, y + h));
        self.close();
    }

    /// Add an ellipse as a subpath.
    pub fn ellipse(&mut self, center: Point, rx: f32, ry: f32) {
        // Approximate ellipse with 4 cubic Béziers
        // Magic number for cubic approximation of quarter circle
        const K: f32 = 0.552_284_7;

        let cx = center.x;
        let cy = center.y;
        let kx = K * rx;
        let ky = K * ry;

        self.move_to(Point::new(cx + rx, cy));
        self.cubic_to(
            Point::new(cx + rx, cy + ky),
            Point::new(cx + kx, cy + ry),
            Point::new(cx, cy + ry),
        );
        self.cubic_to(
            Point::new(cx - kx, cy + ry),
            Point::new(cx - rx, cy + ky),
            Point::new(cx - rx, cy),
        );
        self.cubic_to(
            Point::new(cx - rx, cy - ky),
            Point::new(cx - kx, cy - ry),
            Point::new(cx, cy - ry),
        );
        self.cubic_to(
            Point::new(cx + kx, cy - ry),
            Point::new(cx + rx, cy - ky),
            Point::new(cx + rx, cy),
        );
        self.close();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_bounds() {
        let mut path = Path::new();
        path.move_to(Point::new(10.0, 20.0));
        path.line_to(Point::new(30.0, 40.0));

        let b = path.bounds();
        assert_eq!(b.x, 10.0);
        assert_eq!(b.y, 20.0);
        assert_eq!(b.w, 20.0);
        assert_eq!(b.h, 20.0);
    }

    #[test]
    fn path_circle() {
        let mut path = Path::new();
        path.circle(Point::new(50.0, 50.0), 25.0);

        let b = path.bounds();
        assert_eq!(b.x, 25.0);
        assert_eq!(b.y, 25.0);
        assert_eq!(b.w, 50.0);
        assert_eq!(b.h, 50.0);
    }

    #[test]
    fn path_rect() {
        let mut path = Path::new();
        path.rect(10.0, 20.0, 100.0, 50.0);

        assert_eq!(path.commands().len(), 5); // move + 3 lines + close
        let b = path.bounds();
        assert_eq!(b.x, 10.0);
        assert_eq!(b.y, 20.0);
        assert_eq!(b.w, 100.0);
        assert_eq!(b.h, 50.0);
    }
}
