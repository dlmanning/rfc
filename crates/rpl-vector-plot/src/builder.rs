//! Fluent builder for constructing plots.

use crate::element::{Element, ElementKind};
use crate::paint::{Paint, Stroke};
use crate::path::Path;
use crate::plot::Plot;
use crate::types::{Color, Point, Transform};

/// Graphics state for the builder.
#[derive(Clone, Debug)]
struct GraphicsState {
    fill: Option<Paint>,
    stroke: Option<Stroke>,
    transform: Transform,
}

impl Default for GraphicsState {
    fn default() -> Self {
        Self {
            fill: Some(Paint::Solid(Color::BLACK)),
            stroke: None,
            transform: Transform::IDENTITY,
        }
    }
}

/// Builder for constructing plots fluently.
///
/// # Example
///
/// ```ignore
/// let plot = PlotBuilder::new()
///     .set_fill(Color::RED)
///     .circle(50.0, 50.0, 25.0)
///     .fill()
///     .set_stroke(Color::BLACK, 2.0)
///     .rect(10.0, 10.0, 80.0, 80.0)
///     .stroke()
///     .build();
/// ```
#[derive(Debug)]
pub struct PlotBuilder {
    plot: Plot,
    path: Path,
    state: GraphicsState,
    state_stack: Vec<GraphicsState>,
    group_stack: Vec<Vec<Element>>,
}

impl Default for PlotBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl PlotBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            plot: Plot::new(),
            path: Path::new(),
            state: GraphicsState::default(),
            state_stack: Vec::new(),
            group_stack: Vec::new(),
        }
    }

    /// Build the final plot.
    pub fn build(self) -> Plot {
        self.plot
    }

    // ========================================================================
    // Path operations
    // ========================================================================

    /// Move to a point (starts a new subpath).
    pub fn move_to(&mut self, x: f32, y: f32) -> &mut Self {
        self.path.move_to(Point::new(x, y));
        self
    }

    /// Line to a point.
    pub fn line_to(&mut self, x: f32, y: f32) -> &mut Self {
        self.path.line_to(Point::new(x, y));
        self
    }

    /// Quadratic Bézier curve.
    pub fn quad_to(&mut self, cx: f32, cy: f32, x: f32, y: f32) -> &mut Self {
        self.path.quad_to(Point::new(cx, cy), Point::new(x, y));
        self
    }

    /// Cubic Bézier curve.
    pub fn cubic_to(&mut self, c1x: f32, c1y: f32, c2x: f32, c2y: f32, x: f32, y: f32) -> &mut Self {
        self.path.cubic_to(
            Point::new(c1x, c1y),
            Point::new(c2x, c2y),
            Point::new(x, y),
        );
        self
    }

    /// Arc (circular).
    pub fn arc(&mut self, cx: f32, cy: f32, r: f32, start: f32, sweep: f32) -> &mut Self {
        self.path.arc(Point::new(cx, cy), r, start, sweep);
        self
    }

    /// Close the current subpath.
    pub fn close(&mut self) -> &mut Self {
        self.path.close();
        self
    }

    // ========================================================================
    // Shape shortcuts (add to current path)
    // ========================================================================

    /// Add a circle to the current path.
    pub fn circle(&mut self, cx: f32, cy: f32, r: f32) -> &mut Self {
        self.path.circle(Point::new(cx, cy), r);
        self
    }

    /// Add a rectangle to the current path.
    pub fn rect(&mut self, x: f32, y: f32, w: f32, h: f32) -> &mut Self {
        self.path.rect(x, y, w, h);
        self
    }

    /// Add an ellipse to the current path.
    pub fn ellipse(&mut self, cx: f32, cy: f32, rx: f32, ry: f32) -> &mut Self {
        self.path.ellipse(Point::new(cx, cy), rx, ry);
        self
    }

    // ========================================================================
    // Finalize path
    // ========================================================================

    /// Fill the current path and clear it.
    pub fn fill(&mut self) -> &mut Self {
        if !self.path.is_empty() {
            let path = std::mem::take(&mut self.path);
            let elem = Element::new(ElementKind::Path {
                path,
                fill: self.state.fill.clone(),
                stroke: None,
            }).with_transform(self.state.transform);
            self.add_element(elem);
        }
        self
    }

    /// Stroke the current path and clear it.
    pub fn stroke(&mut self) -> &mut Self {
        if !self.path.is_empty() {
            let path = std::mem::take(&mut self.path);
            let elem = Element::new(ElementKind::Path {
                path,
                fill: None,
                stroke: self.state.stroke.clone(),
            }).with_transform(self.state.transform);
            self.add_element(elem);
        }
        self
    }

    /// Fill and stroke the current path, then clear it.
    pub fn fill_and_stroke(&mut self) -> &mut Self {
        if !self.path.is_empty() {
            let path = std::mem::take(&mut self.path);
            let elem = Element::new(ElementKind::Path {
                path,
                fill: self.state.fill.clone(),
                stroke: self.state.stroke.clone(),
            }).with_transform(self.state.transform);
            self.add_element(elem);
        }
        self
    }

    // ========================================================================
    // Text
    // ========================================================================

    /// Add text at a position.
    pub fn text(&mut self, x: f32, y: f32, text: &str, size: f32) -> &mut Self {
        let elem = Element::new(ElementKind::text(
            Point::new(x, y),
            text.to_string(),
            size,
        )).with_transform(self.state.transform);
        self.add_element(elem);
        self
    }

    // ========================================================================
    // Style
    // ========================================================================

    /// Set the fill color.
    pub fn set_fill(&mut self, color: Color) -> &mut Self {
        self.state.fill = Some(Paint::Solid(color));
        self
    }

    /// Set the fill paint (for gradients).
    pub fn set_fill_paint(&mut self, paint: Paint) -> &mut Self {
        self.state.fill = Some(paint);
        self
    }

    /// Clear the fill (no fill).
    pub fn no_fill(&mut self) -> &mut Self {
        self.state.fill = None;
        self
    }

    /// Set the stroke color and width.
    pub fn set_stroke(&mut self, color: Color, width: f32) -> &mut Self {
        self.state.stroke = Some(Stroke::new(color, width));
        self
    }

    /// Set the stroke width only.
    pub fn set_stroke_width(&mut self, width: f32) -> &mut Self {
        if let Some(ref mut stroke) = self.state.stroke {
            stroke.width = width;
        } else {
            self.state.stroke = Some(Stroke::new(Color::BLACK, width));
        }
        self
    }

    /// Set the stroke color only.
    pub fn set_stroke_color(&mut self, color: Color) -> &mut Self {
        if let Some(ref mut stroke) = self.state.stroke {
            stroke.color = color;
        } else {
            self.state.stroke = Some(Stroke::new(color, 1.0));
        }
        self
    }

    /// Clear the stroke (no stroke).
    pub fn no_stroke(&mut self) -> &mut Self {
        self.state.stroke = None;
        self
    }

    // ========================================================================
    // Transform
    // ========================================================================

    /// Translate the coordinate system.
    pub fn translate(&mut self, dx: f32, dy: f32) -> &mut Self {
        self.state.transform = self.state.transform.then(Transform::translate(dx, dy));
        self
    }

    /// Rotate the coordinate system (angle in radians).
    pub fn rotate(&mut self, angle: f32) -> &mut Self {
        self.state.transform = self.state.transform.then(Transform::rotate(angle));
        self
    }

    /// Scale the coordinate system.
    pub fn scale(&mut self, sx: f32, sy: f32) -> &mut Self {
        self.state.transform = self.state.transform.then(Transform::scale(sx, sy));
        self
    }

    /// Reset transform to identity.
    pub fn identity(&mut self) -> &mut Self {
        self.state.transform = Transform::IDENTITY;
        self
    }

    // ========================================================================
    // State stack
    // ========================================================================

    /// Save the current graphics state.
    pub fn save(&mut self) -> &mut Self {
        self.state_stack.push(self.state.clone());
        self
    }

    /// Restore the previous graphics state.
    pub fn restore(&mut self) -> &mut Self {
        if let Some(state) = self.state_stack.pop() {
            self.state = state;
        }
        self
    }

    // ========================================================================
    // Grouping
    // ========================================================================

    /// Begin a new group.
    pub fn begin_group(&mut self) -> &mut Self {
        self.group_stack.push(Vec::new());
        self
    }

    /// End the current group.
    pub fn end_group(&mut self) -> &mut Self {
        if let Some(children) = self.group_stack.pop() {
            let elem = Element::new(ElementKind::group_with(children))
                .with_transform(self.state.transform);
            self.add_element(elem);
        }
        self
    }

    // ========================================================================
    // Internal
    // ========================================================================

    fn add_element(&mut self, element: Element) {
        if let Some(group) = self.group_stack.last_mut() {
            group.push(element);
        } else {
            self.plot.add_anonymous(element);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builder_simple() {
        let mut builder = PlotBuilder::new();
        builder.set_fill(Color::RED);
        builder.circle(50.0, 50.0, 25.0);
        builder.fill();
        let plot = builder.build();

        assert_eq!(plot.len(), 1);
    }

    #[test]
    fn builder_multiple_shapes() {
        let mut builder = PlotBuilder::new();
        builder.set_fill(Color::RED);
        builder.circle(50.0, 50.0, 25.0);
        builder.fill();
        builder.set_fill(Color::BLUE);
        builder.rect(0.0, 0.0, 100.0, 100.0);
        builder.fill();
        let plot = builder.build();

        assert_eq!(plot.len(), 2);
    }

    #[test]
    fn builder_stroke() {
        let mut builder = PlotBuilder::new();
        builder.set_stroke(Color::BLACK, 2.0);
        builder.move_to(0.0, 0.0);
        builder.line_to(100.0, 100.0);
        builder.stroke();
        let plot = builder.build();

        assert_eq!(plot.len(), 1);
    }

    #[test]
    fn builder_save_restore() {
        let mut builder = PlotBuilder::new();
        builder.set_fill(Color::RED);
        builder.save();
        builder.set_fill(Color::BLUE);
        builder.restore();

        // Should be back to RED
        assert!(matches!(builder.state.fill, Some(Paint::Solid(c)) if c == Color::RED));
    }

    #[test]
    fn builder_group() {
        let mut builder = PlotBuilder::new();
        builder.begin_group();
        builder.set_fill(Color::RED);
        builder.circle(10.0, 10.0, 5.0);
        builder.fill();
        builder.circle(20.0, 20.0, 5.0);
        builder.fill();
        builder.end_group();
        let plot = builder.build();

        assert_eq!(plot.len(), 1); // One group
        if let ElementKind::Group { children } = &plot.elements()[0].kind {
            assert_eq!(children.len(), 2); // Two circles in group
        } else {
            panic!("Expected a group");
        }
    }
}
