//! Scene graph elements.

use crate::paint::{Paint, Stroke};
use crate::path::Path;
use crate::types::{Point, Rect, Transform};

/// Unique identifier for an element.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ElementId(pub u32);

impl ElementId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_u32(self) -> u32 {
        self.0
    }
}

/// A drawable element in the scene graph.
#[derive(Clone, Debug)]
pub struct Element {
    pub id: Option<ElementId>,
    pub kind: ElementKind,
    pub transform: Transform,
}

impl Element {
    /// Create a new element with no ID.
    pub fn new(kind: ElementKind) -> Self {
        Self {
            id: None,
            kind,
            transform: Transform::IDENTITY,
        }
    }

    /// Create a new element with an ID.
    pub fn with_id(id: ElementId, kind: ElementKind) -> Self {
        Self {
            id: Some(id),
            kind,
            transform: Transform::IDENTITY,
        }
    }

    /// Set the transform.
    pub fn with_transform(mut self, transform: Transform) -> Self {
        self.transform = transform;
        self
    }

    /// Get the bounding box of this element (in local coordinates).
    pub fn bounds(&self) -> Rect {
        match &self.kind {
            ElementKind::Path { path, stroke, .. } => {
                let mut b = path.bounds();
                // Expand bounds by stroke width if stroked
                if let Some(s) = stroke {
                    let hw = s.width * 0.5;
                    b.x -= hw;
                    b.y -= hw;
                    b.w += s.width;
                    b.h += s.width;
                }
                b
            }
            ElementKind::Text { pos, size, text } => {
                // Rough estimate: assume each character is about 0.6 * size wide
                let width = text.len() as f32 * size * 0.6;
                Rect::new(pos.x, pos.y - size, width, *size)
            }
            ElementKind::Group { children } => {
                children.iter().fold(Rect::EMPTY, |acc, child| {
                    acc.union(child.bounds())
                })
            }
        }
    }
}

/// The kind of element.
#[derive(Clone, Debug)]
pub enum ElementKind {
    /// A path with optional fill and stroke.
    Path {
        path: Path,
        fill: Option<Paint>,
        stroke: Option<Stroke>,
    },
    /// Text at a position.
    Text {
        pos: Point,
        text: String,
        size: f32,
    },
    /// A group of child elements.
    Group {
        children: Vec<Element>,
    },
}

impl ElementKind {
    /// Create a filled path element.
    pub fn filled_path(path: Path, paint: Paint) -> Self {
        Self::Path {
            path,
            fill: Some(paint),
            stroke: None,
        }
    }

    /// Create a stroked path element.
    pub fn stroked_path(path: Path, stroke: Stroke) -> Self {
        Self::Path {
            path,
            fill: None,
            stroke: Some(stroke),
        }
    }

    /// Create a path with both fill and stroke.
    pub fn filled_and_stroked_path(path: Path, paint: Paint, stroke: Stroke) -> Self {
        Self::Path {
            path,
            fill: Some(paint),
            stroke: Some(stroke),
        }
    }

    /// Create a text element.
    pub fn text(pos: Point, text: String, size: f32) -> Self {
        Self::Text { pos, text, size }
    }

    /// Create an empty group.
    pub fn group() -> Self {
        Self::Group { children: Vec::new() }
    }

    /// Create a group with children.
    pub fn group_with(children: Vec<Element>) -> Self {
        Self::Group { children }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::Path;
    use crate::types::Color;

    #[test]
    fn element_id() {
        let id = ElementId::new(42);
        assert_eq!(id.as_u32(), 42);
    }

    #[test]
    fn element_path_bounds() {
        let mut path = Path::new();
        path.rect(10.0, 20.0, 100.0, 50.0);

        let elem = Element::new(ElementKind::filled_path(path, Paint::Solid(Color::RED)));
        let b = elem.bounds();
        assert_eq!(b.x, 10.0);
        assert_eq!(b.y, 20.0);
        assert_eq!(b.w, 100.0);
        assert_eq!(b.h, 50.0);
    }

    #[test]
    fn element_path_with_stroke_bounds() {
        let mut path = Path::new();
        path.rect(10.0, 20.0, 100.0, 50.0);

        let stroke = Stroke::new(Color::BLACK, 4.0);
        let elem = Element::new(ElementKind::stroked_path(path, stroke));
        let b = elem.bounds();
        // Bounds expanded by stroke width / 2 = 2.0
        assert_eq!(b.x, 8.0);
        assert_eq!(b.y, 18.0);
        assert_eq!(b.w, 104.0);
        assert_eq!(b.h, 54.0);
    }

    #[test]
    fn element_group_bounds() {
        let mut path1 = Path::new();
        path1.rect(0.0, 0.0, 50.0, 50.0);
        let elem1 = Element::new(ElementKind::filled_path(path1, Paint::Solid(Color::RED)));

        let mut path2 = Path::new();
        path2.rect(100.0, 100.0, 50.0, 50.0);
        let elem2 = Element::new(ElementKind::filled_path(path2, Paint::Solid(Color::BLUE)));

        let group = Element::new(ElementKind::group_with(vec![elem1, elem2]));
        let b = group.bounds();
        assert_eq!(b.x, 0.0);
        assert_eq!(b.y, 0.0);
        assert_eq!(b.w, 150.0);
        assert_eq!(b.h, 150.0);
    }
}
