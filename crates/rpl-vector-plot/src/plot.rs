//! The Plot scene graph.

use crate::element::{Element, ElementId, ElementKind};
use crate::paint::{Paint, Stroke};
use crate::path::Path;
use crate::types::{Color, Point, Rect};

/// A complete vector plot (scene graph).
#[derive(Clone, Debug)]
pub struct Plot {
    /// Root-level elements.
    elements: Vec<Element>,
    /// Next available element ID.
    next_id: u32,
    /// Cached bounding box of all elements.
    bounds: Rect,
    /// Optional viewbox (logical coordinate space).
    viewbox: Option<Rect>,
    /// Current fill color for new shapes.
    fill_color: Color,
    /// Current stroke color for new shapes.
    stroke_color: Color,
    /// Current stroke width for new shapes.
    stroke_width: f32,
}

impl Default for Plot {
    fn default() -> Self {
        Self::new()
    }
}

impl Plot {
    /// Create an empty plot.
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            next_id: 1,
            bounds: Rect::EMPTY,
            viewbox: None,
            fill_color: Color::BLACK,
            stroke_color: Color::BLACK,
            stroke_width: 1.0,
        }
    }

    /// Get the current fill color.
    pub fn fill_color(&self) -> Color {
        self.fill_color
    }

    /// Set the fill color for subsequent shapes.
    pub fn set_fill_color(&mut self, color: Color) {
        self.fill_color = color;
    }

    /// Get the current stroke color.
    pub fn stroke_color(&self) -> Color {
        self.stroke_color
    }

    /// Set the stroke color for subsequent shapes.
    pub fn set_stroke_color(&mut self, color: Color) {
        self.stroke_color = color;
    }

    /// Get the current stroke width.
    pub fn stroke_width(&self) -> f32 {
        self.stroke_width
    }

    /// Set the stroke width for subsequent shapes.
    pub fn set_stroke_width(&mut self, width: f32) {
        self.stroke_width = width;
    }

    /// Get the bounding box of all elements.
    pub fn bounds(&self) -> Rect {
        self.bounds
    }

    /// Get the viewbox, if set.
    pub fn viewbox(&self) -> Option<Rect> {
        self.viewbox
    }

    /// Set the viewbox.
    pub fn set_viewbox(&mut self, viewbox: Rect) {
        self.viewbox = Some(viewbox);
    }

    /// Get the root elements.
    pub fn elements(&self) -> &[Element] {
        &self.elements
    }

    /// Get the root elements mutably.
    pub fn elements_mut(&mut self) -> &mut Vec<Element> {
        &mut self.elements
    }

    /// Get the number of root elements.
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Check if the plot is empty.
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Allocate a new element ID.
    pub fn alloc_id(&mut self) -> ElementId {
        let id = ElementId::new(self.next_id);
        self.next_id += 1;
        id
    }

    /// Add an element and return its ID.
    pub fn add(&mut self, mut element: Element) -> ElementId {
        let id = self.alloc_id();
        element.id = Some(id);
        self.bounds = self.bounds.union(element.bounds());
        self.elements.push(element);
        id
    }

    /// Add an element without allocating an ID.
    pub fn add_anonymous(&mut self, element: Element) {
        self.bounds = self.bounds.union(element.bounds());
        self.elements.push(element);
    }

    /// Find an element by ID (searches recursively).
    #[allow(clippy::collapsible_if)]
    pub fn get(&self, id: ElementId) -> Option<&Element> {
        fn find_in(elements: &[Element], id: ElementId) -> Option<&Element> {
            for elem in elements {
                if elem.id == Some(id) {
                    return Some(elem);
                }
                if let ElementKind::Group { children } = &elem.kind {
                    if let Some(found) = find_in(children, id) {
                        return Some(found);
                    }
                }
            }
            None
        }
        find_in(&self.elements, id)
    }

    /// Find an element by ID mutably (searches recursively).
    #[allow(clippy::collapsible_if)]
    pub fn get_mut(&mut self, id: ElementId) -> Option<&mut Element> {
        fn find_in_mut(elements: &mut [Element], id: ElementId) -> Option<&mut Element> {
            for elem in elements {
                if elem.id == Some(id) {
                    return Some(elem);
                }
                if let ElementKind::Group { children } = &mut elem.kind {
                    if let Some(found) = find_in_mut(children, id) {
                        return Some(found);
                    }
                }
            }
            None
        }
        find_in_mut(&mut self.elements, id)
    }

    /// Recalculate bounds from all elements.
    pub fn recalculate_bounds(&mut self) {
        self.bounds = self.elements.iter().fold(Rect::EMPTY, |acc, elem| {
            acc.union(elem.bounds())
        });
    }

    // ========================================================================
    // Convenience methods for adding common shapes
    // ========================================================================

    /// Add a filled circle using the current fill color.
    pub fn add_circle(&mut self, cx: f32, cy: f32, r: f32) -> ElementId {
        let mut path = Path::new();
        path.circle(Point::new(cx, cy), r);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(self.fill_color))))
    }

    /// Add a filled circle with a specific color.
    pub fn add_circle_colored(&mut self, cx: f32, cy: f32, r: f32, fill: Color) -> ElementId {
        let mut path = Path::new();
        path.circle(Point::new(cx, cy), r);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(fill))))
    }

    /// Add a stroked circle.
    pub fn add_circle_stroked(&mut self, cx: f32, cy: f32, r: f32, stroke: Stroke) -> ElementId {
        let mut path = Path::new();
        path.circle(Point::new(cx, cy), r);
        self.add(Element::new(ElementKind::stroked_path(path, stroke)))
    }

    /// Add a filled rectangle using the current fill color.
    pub fn add_rect(&mut self, x: f32, y: f32, w: f32, h: f32) -> ElementId {
        let mut path = Path::new();
        path.rect(x, y, w, h);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(self.fill_color))))
    }

    /// Add a filled rectangle with a specific color.
    pub fn add_rect_colored(&mut self, x: f32, y: f32, w: f32, h: f32, fill: Color) -> ElementId {
        let mut path = Path::new();
        path.rect(x, y, w, h);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(fill))))
    }

    /// Add a stroked rectangle.
    pub fn add_rect_stroked(&mut self, x: f32, y: f32, w: f32, h: f32, stroke: Stroke) -> ElementId {
        let mut path = Path::new();
        path.rect(x, y, w, h);
        self.add(Element::new(ElementKind::stroked_path(path, stroke)))
    }

    /// Add a filled ellipse using the current fill color.
    pub fn add_ellipse(&mut self, cx: f32, cy: f32, rx: f32, ry: f32) -> ElementId {
        let mut path = Path::new();
        path.ellipse(Point::new(cx, cy), rx, ry);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(self.fill_color))))
    }

    /// Add a filled ellipse with a specific color.
    pub fn add_ellipse_colored(&mut self, cx: f32, cy: f32, rx: f32, ry: f32, fill: Color) -> ElementId {
        let mut path = Path::new();
        path.ellipse(Point::new(cx, cy), rx, ry);
        self.add(Element::new(ElementKind::filled_path(path, Paint::Solid(fill))))
    }

    /// Add a line.
    pub fn add_line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, stroke: Stroke) -> ElementId {
        let mut path = Path::new();
        path.move_to(Point::new(x1, y1));
        path.line_to(Point::new(x2, y2));
        self.add(Element::new(ElementKind::stroked_path(path, stroke)))
    }

    /// Add text.
    pub fn add_text(&mut self, x: f32, y: f32, text: String, size: f32) -> ElementId {
        self.add(Element::new(ElementKind::text(Point::new(x, y), text, size)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plot_empty() {
        let plot = Plot::new();
        assert!(plot.is_empty());
        assert!(plot.bounds().is_empty());
    }

    #[test]
    fn plot_add_circle() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        let id = plot.add_circle(50.0, 50.0, 25.0);

        assert_eq!(plot.len(), 1);
        assert!(plot.get(id).is_some());

        let b = plot.bounds();
        assert_eq!(b.x, 25.0);
        assert_eq!(b.y, 25.0);
        assert_eq!(b.w, 50.0);
        assert_eq!(b.h, 50.0);
    }

    #[test]
    fn plot_add_multiple() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        plot.add_rect(0.0, 0.0, 50.0, 50.0);
        plot.set_fill_color(Color::BLUE);
        plot.add_rect(100.0, 100.0, 50.0, 50.0);

        assert_eq!(plot.len(), 2);

        let b = plot.bounds();
        assert_eq!(b.x, 0.0);
        assert_eq!(b.y, 0.0);
        assert_eq!(b.w, 150.0);
        assert_eq!(b.h, 150.0);
    }

    #[test]
    fn plot_get_by_id() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        let id1 = plot.add_circle(10.0, 10.0, 5.0);
        plot.set_fill_color(Color::BLUE);
        let id2 = plot.add_rect(50.0, 50.0, 20.0, 20.0);

        assert!(plot.get(id1).is_some());
        assert!(plot.get(id2).is_some());
        assert!(plot.get(ElementId::new(999)).is_none());
    }

    #[test]
    fn plot_alloc_id_increments() {
        let mut plot = Plot::new();
        let id1 = plot.alloc_id();
        let id2 = plot.alloc_id();
        let id3 = plot.alloc_id();

        assert_eq!(id1.as_u32(), 1);
        assert_eq!(id2.as_u32(), 2);
        assert_eq!(id3.as_u32(), 3);
    }
}
