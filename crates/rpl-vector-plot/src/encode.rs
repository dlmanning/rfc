//! Encode a Plot to bytes.

use crate::element::{Element, ElementKind};
use crate::paint::{LineCap, LineJoin, Paint, Stroke};
use crate::path::{Path, PathCmd};
use crate::plot::Plot;
use crate::types::{Color, Point, Transform};

/// Magic header: "VPL" + version byte.
pub const MAGIC: [u8; 4] = [b'V', b'P', b'L', 0x01];

// Command bytes
mod cmd {
    // Structural
    pub const END: u8 = 0x00;
    pub const ELEMENT: u8 = 0x01;
    #[allow(dead_code)] // Used when group encoding is implemented
    pub const GROUP_START: u8 = 0x02;
    #[allow(dead_code)] // Used when group encoding is implemented
    pub const GROUP_END: u8 = 0x03;

    // Path commands
    pub const PATH_START: u8 = 0x10;
    pub const PATH_END: u8 = 0x11;
    pub const MOVE_TO: u8 = 0x12;
    pub const LINE_TO: u8 = 0x13;
    pub const QUAD_TO: u8 = 0x14;
    pub const CUBIC_TO: u8 = 0x15;
    pub const ARC: u8 = 0x16;
    pub const CLOSE: u8 = 0x17;

    // Element kinds
    pub const KIND_PATH: u8 = 0x20;
    pub const KIND_TEXT: u8 = 0x21;
    pub const KIND_GROUP: u8 = 0x22;

    // Paint
    pub const PAINT_NONE: u8 = 0x30;
    pub const PAINT_SOLID: u8 = 0x31;
    pub const PAINT_LINEAR: u8 = 0x32;
    pub const PAINT_RADIAL: u8 = 0x33;

    // Stroke
    pub const STROKE_NONE: u8 = 0x40;
    pub const STROKE_PRESENT: u8 = 0x41;

    // Transform
    pub const TRANSFORM_IDENTITY: u8 = 0x50;
    pub const TRANSFORM_MATRIX: u8 = 0x51;

    // Metadata
    pub const VIEWBOX: u8 = 0x60;
    pub const GRAPHICS_STATE: u8 = 0x61;
}

/// Encode a plot to bytes.
pub fn encode(plot: &Plot) -> Vec<u8> {
    let mut out = Vec::with_capacity(256);

    // Magic header
    out.extend_from_slice(&MAGIC);

    // Optional viewbox
    if let Some(vb) = plot.viewbox() {
        out.push(cmd::VIEWBOX);
        write_f32(&mut out, vb.x);
        write_f32(&mut out, vb.y);
        write_f32(&mut out, vb.w);
        write_f32(&mut out, vb.h);
    }

    // Graphics state (fill color, stroke color, stroke width)
    out.push(cmd::GRAPHICS_STATE);
    write_color(&mut out, plot.fill_color());
    write_color(&mut out, plot.stroke_color());
    write_f32(&mut out, plot.stroke_width());

    // Elements
    for elem in plot.elements() {
        encode_element(&mut out, elem);
    }

    // End marker
    out.push(cmd::END);

    out
}

fn encode_element(out: &mut Vec<u8>, elem: &Element) {
    out.push(cmd::ELEMENT);

    // Element ID (0 if none)
    let id = elem.id.map(|id| id.as_u32()).unwrap_or(0);
    write_u32(out, id);

    // Transform
    encode_transform(out, &elem.transform);

    // Kind
    match &elem.kind {
        ElementKind::Path { path, fill, stroke } => {
            out.push(cmd::KIND_PATH);
            encode_path(out, path);
            encode_paint(out, fill.as_ref());
            encode_stroke(out, stroke.as_ref());
        }
        ElementKind::Text { pos, text, size } => {
            out.push(cmd::KIND_TEXT);
            write_f32(out, pos.x);
            write_f32(out, pos.y);
            write_f32(out, *size);
            write_string(out, text);
        }
        ElementKind::Group { children } => {
            out.push(cmd::KIND_GROUP);
            write_u32(out, children.len() as u32);
            for child in children {
                encode_element(out, child);
            }
        }
    }
}

fn encode_transform(out: &mut Vec<u8>, t: &Transform) {
    if t.is_identity() {
        out.push(cmd::TRANSFORM_IDENTITY);
    } else {
        out.push(cmd::TRANSFORM_MATRIX);
        for &v in t.as_array() {
            write_f32(out, v);
        }
    }
}

fn encode_path(out: &mut Vec<u8>, path: &Path) {
    out.push(cmd::PATH_START);
    for cmd in path.commands() {
        match cmd {
            PathCmd::MoveTo(p) => {
                out.push(cmd::MOVE_TO);
                write_point(out, *p);
            }
            PathCmd::LineTo(p) => {
                out.push(cmd::LINE_TO);
                write_point(out, *p);
            }
            PathCmd::QuadTo { ctrl, end } => {
                out.push(cmd::QUAD_TO);
                write_point(out, *ctrl);
                write_point(out, *end);
            }
            PathCmd::CubicTo { c1, c2, end } => {
                out.push(cmd::CUBIC_TO);
                write_point(out, *c1);
                write_point(out, *c2);
                write_point(out, *end);
            }
            PathCmd::Arc { center, radius, start_angle, sweep_angle } => {
                out.push(cmd::ARC);
                write_point(out, *center);
                write_f32(out, *radius);
                write_f32(out, *start_angle);
                write_f32(out, *sweep_angle);
            }
            PathCmd::Close => {
                out.push(cmd::CLOSE);
            }
        }
    }
    out.push(cmd::PATH_END);
}

fn encode_paint(out: &mut Vec<u8>, paint: Option<&Paint>) {
    match paint {
        None => {
            out.push(cmd::PAINT_NONE);
        }
        Some(Paint::Solid(color)) => {
            out.push(cmd::PAINT_SOLID);
            write_color(out, *color);
        }
        Some(Paint::LinearGradient { start, end, stops }) => {
            out.push(cmd::PAINT_LINEAR);
            write_point(out, *start);
            write_point(out, *end);
            write_u32(out, stops.len() as u32);
            for stop in stops {
                write_f32(out, stop.offset);
                write_color(out, stop.color);
            }
        }
        Some(Paint::RadialGradient { center, radius, stops }) => {
            out.push(cmd::PAINT_RADIAL);
            write_point(out, *center);
            write_f32(out, *radius);
            write_u32(out, stops.len() as u32);
            for stop in stops {
                write_f32(out, stop.offset);
                write_color(out, stop.color);
            }
        }
    }
}

fn encode_stroke(out: &mut Vec<u8>, stroke: Option<&Stroke>) {
    match stroke {
        None => {
            out.push(cmd::STROKE_NONE);
        }
        Some(s) => {
            out.push(cmd::STROKE_PRESENT);
            write_f32(out, s.width);
            write_color(out, s.color);
            out.push(match s.line_cap {
                LineCap::Butt => 0,
                LineCap::Round => 1,
                LineCap::Square => 2,
            });
            out.push(match s.line_join {
                LineJoin::Miter => 0,
                LineJoin::Round => 1,
                LineJoin::Bevel => 2,
            });
        }
    }
}

// ============================================================================
// Write helpers
// ============================================================================

fn write_f32(out: &mut Vec<u8>, v: f32) {
    out.extend_from_slice(&v.to_le_bytes());
}

fn write_u32(out: &mut Vec<u8>, v: u32) {
    out.extend_from_slice(&v.to_le_bytes());
}

fn write_point(out: &mut Vec<u8>, p: Point) {
    write_f32(out, p.x);
    write_f32(out, p.y);
}

fn write_color(out: &mut Vec<u8>, c: Color) {
    out.push(c.r);
    out.push(c.g);
    out.push(c.b);
    out.push(c.a);
}

fn write_string(out: &mut Vec<u8>, s: &str) {
    let bytes = s.as_bytes();
    write_u32(out, bytes.len() as u32);
    out.extend_from_slice(bytes);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Rect;

    #[test]
    fn encode_empty_plot() {
        let plot = Plot::new();
        let bytes = encode(&plot);

        assert_eq!(&bytes[0..4], &MAGIC);
        // After magic: graphics state (1 + 4 + 4 + 4 = 13 bytes), then END
        assert_eq!(bytes[4], cmd::GRAPHICS_STATE);
        assert_eq!(*bytes.last().unwrap(), cmd::END);
    }

    #[test]
    fn encode_with_viewbox() {
        let mut plot = Plot::new();
        plot.set_viewbox(Rect::new(0.0, 0.0, 100.0, 100.0));
        let bytes = encode(&plot);

        assert_eq!(&bytes[0..4], &MAGIC);
        assert_eq!(bytes[4], cmd::VIEWBOX);
    }

    #[test]
    fn encode_circle() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        plot.add_circle(50.0, 50.0, 25.0);
        let bytes = encode(&plot);

        assert!(bytes.len() > 5);
        assert_eq!(&bytes[0..4], &MAGIC);
    }
}
