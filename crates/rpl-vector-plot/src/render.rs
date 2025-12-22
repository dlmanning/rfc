//! Renderer trait and rendering logic.

use crate::element::{Element, ElementKind};
use crate::paint::{Paint, Stroke};
use crate::path::PathCmd;
use crate::plot::Plot;
use crate::types::{Point, Transform};

/// Trait for rendering plots.
///
/// Implement this trait to render plots to your target (screen, canvas, SVG, etc.).
///
/// # Path Model
///
/// Path commands are issued in order:
/// - `move_to` starts a new subpath
/// - `line_to`, `quad_to`, `cubic_to`, `arc` add segments
/// - `close_path` closes the current subpath
///
/// After path commands, `fill` or `stroke` (or both) will be called to render.
pub trait Renderer {
    /// Move to a point (starts a new subpath).
    fn move_to(&mut self, p: Point);

    /// Line to a point.
    fn line_to(&mut self, p: Point);

    /// Quadratic Bézier curve.
    fn quad_to(&mut self, ctrl: Point, end: Point);

    /// Cubic Bézier curve.
    fn cubic_to(&mut self, c1: Point, c2: Point, end: Point);

    /// Arc (circular).
    fn arc(&mut self, center: Point, radius: f32, start_angle: f32, sweep_angle: f32);

    /// Close the current subpath.
    fn close_path(&mut self);

    /// Fill the current path.
    fn fill(&mut self, paint: &Paint);

    /// Stroke the current path.
    fn stroke(&mut self, stroke: &Stroke);

    /// Clear the current path without rendering.
    fn clear_path(&mut self);

    /// Push a transform onto the transform stack.
    fn push_transform(&mut self, t: &Transform);

    /// Pop a transform from the stack.
    fn pop_transform(&mut self);

    /// Render text at a position.
    fn text(&mut self, pos: Point, text: &str, size: f32);
}

/// Render a plot using the given renderer.
pub fn render<R: Renderer>(plot: &Plot, renderer: &mut R) {
    for elem in plot.elements() {
        render_element(renderer, elem);
    }
}

fn render_element<R: Renderer>(renderer: &mut R, elem: &Element) {
    // Push transform if not identity
    let has_transform = !elem.transform.is_identity();
    if has_transform {
        renderer.push_transform(&elem.transform);
    }

    match &elem.kind {
        ElementKind::Path { path, fill, stroke } => {
            // Emit path commands
            for cmd in path.commands() {
                match cmd {
                    PathCmd::MoveTo(p) => renderer.move_to(*p),
                    PathCmd::LineTo(p) => renderer.line_to(*p),
                    PathCmd::QuadTo { ctrl, end } => renderer.quad_to(*ctrl, *end),
                    PathCmd::CubicTo { c1, c2, end } => renderer.cubic_to(*c1, *c2, *end),
                    PathCmd::Arc { center, radius, start_angle, sweep_angle } => {
                        renderer.arc(*center, *radius, *start_angle, *sweep_angle);
                    }
                    PathCmd::Close => renderer.close_path(),
                }
            }

            // Fill and/or stroke
            if let Some(paint) = fill {
                renderer.fill(paint);
            }
            if let Some(s) = stroke {
                renderer.stroke(s);
            }

            // Clear path for next element
            renderer.clear_path();
        }
        ElementKind::Text { pos, text, size } => {
            renderer.text(*pos, text, *size);
        }
        ElementKind::Group { children } => {
            for child in children {
                render_element(renderer, child);
            }
        }
    }

    if has_transform {
        renderer.pop_transform();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Color;

    /// Mock renderer that records all calls.
    #[derive(Default)]
    struct MockRenderer {
        calls: Vec<String>,
    }

    impl Renderer for MockRenderer {
        fn move_to(&mut self, p: Point) {
            self.calls.push(format!("move_to({}, {})", p.x, p.y));
        }

        fn line_to(&mut self, p: Point) {
            self.calls.push(format!("line_to({}, {})", p.x, p.y));
        }

        fn quad_to(&mut self, ctrl: Point, end: Point) {
            self.calls.push(format!(
                "quad_to({}, {}, {}, {})",
                ctrl.x, ctrl.y, end.x, end.y
            ));
        }

        fn cubic_to(&mut self, c1: Point, c2: Point, end: Point) {
            self.calls.push(format!(
                "cubic_to({}, {}, {}, {}, {}, {})",
                c1.x, c1.y, c2.x, c2.y, end.x, end.y
            ));
        }

        fn arc(&mut self, center: Point, radius: f32, start_angle: f32, sweep_angle: f32) {
            self.calls.push(format!(
                "arc({}, {}, {}, {}, {})",
                center.x, center.y, radius, start_angle, sweep_angle
            ));
        }

        fn close_path(&mut self) {
            self.calls.push("close_path".to_string());
        }

        fn fill(&mut self, _paint: &Paint) {
            self.calls.push("fill".to_string());
        }

        fn stroke(&mut self, _stroke: &Stroke) {
            self.calls.push("stroke".to_string());
        }

        fn clear_path(&mut self) {
            self.calls.push("clear_path".to_string());
        }

        fn push_transform(&mut self, _t: &Transform) {
            self.calls.push("push_transform".to_string());
        }

        fn pop_transform(&mut self) {
            self.calls.push("pop_transform".to_string());
        }

        fn text(&mut self, pos: Point, text: &str, size: f32) {
            self.calls.push(format!("text({}, {}, \"{}\", {})", pos.x, pos.y, text, size));
        }
    }

    #[test]
    fn render_empty_plot() {
        let plot = Plot::new();
        let mut renderer = MockRenderer::default();
        render(&plot, &mut renderer);
        assert!(renderer.calls.is_empty());
    }

    #[test]
    fn render_circle() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        plot.add_circle(50.0, 50.0, 25.0);

        let mut renderer = MockRenderer::default();
        render(&plot, &mut renderer);

        // Should have path commands + fill + clear
        assert!(renderer.calls.iter().any(|c| c.starts_with("move_to")));
        assert!(renderer.calls.iter().any(|c| c == "fill"));
        assert!(renderer.calls.iter().any(|c| c == "clear_path"));
    }

    #[test]
    fn render_text() {
        let mut plot = Plot::new();
        plot.add_text(10.0, 20.0, "Hello".to_string(), 12.0);

        let mut renderer = MockRenderer::default();
        render(&plot, &mut renderer);

        assert!(renderer.calls.iter().any(|c| c.contains("text")));
        assert!(renderer.calls.iter().any(|c| c.contains("Hello")));
    }

    #[test]
    fn render_with_transform() {
        let mut plot = Plot::new();

        // Create element with transform
        let mut elem = Element::new(ElementKind::text(
            Point::new(0.0, 0.0),
            "Test".to_string(),
            10.0,
        ));
        elem.transform = Transform::translate(100.0, 100.0);
        plot.add_anonymous(elem);

        let mut renderer = MockRenderer::default();
        render(&plot, &mut renderer);

        assert!(renderer.calls.iter().any(|c| c == "push_transform"));
        assert!(renderer.calls.iter().any(|c| c == "pop_transform"));
    }
}
