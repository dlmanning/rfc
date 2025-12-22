//! Decode a Plot from bytes.

use crate::element::{Element, ElementId, ElementKind};
use crate::encode::MAGIC;
use crate::paint::{GradientStop, LineCap, LineJoin, Paint, Stroke};
use crate::path::Path;
use crate::plot::Plot;
use crate::types::{Color, Point, Rect, Transform};

/// Decode error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecodeError {
    /// Invalid magic header.
    InvalidMagic,
    /// Unexpected end of data.
    UnexpectedEnd,
    /// Invalid command byte.
    InvalidCommand(u8),
    /// Invalid UTF-8 string.
    InvalidString,
    /// Other error.
    Other(String),
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecodeError::InvalidMagic => write!(f, "invalid magic header"),
            DecodeError::UnexpectedEnd => write!(f, "unexpected end of data"),
            DecodeError::InvalidCommand(c) => write!(f, "invalid command: 0x{c:02X}"),
            DecodeError::InvalidString => write!(f, "invalid UTF-8 string"),
            DecodeError::Other(s) => write!(f, "{s}"),
        }
    }
}

impl std::error::Error for DecodeError {}

// Command bytes (must match encode.rs)
mod cmd {
    pub const END: u8 = 0x00;
    pub const ELEMENT: u8 = 0x01;

    pub const PATH_START: u8 = 0x10;
    pub const PATH_END: u8 = 0x11;
    pub const MOVE_TO: u8 = 0x12;
    pub const LINE_TO: u8 = 0x13;
    pub const QUAD_TO: u8 = 0x14;
    pub const CUBIC_TO: u8 = 0x15;
    pub const ARC: u8 = 0x16;
    pub const CLOSE: u8 = 0x17;

    pub const KIND_PATH: u8 = 0x20;
    pub const KIND_TEXT: u8 = 0x21;
    pub const KIND_GROUP: u8 = 0x22;

    pub const PAINT_NONE: u8 = 0x30;
    pub const PAINT_SOLID: u8 = 0x31;
    pub const PAINT_LINEAR: u8 = 0x32;
    pub const PAINT_RADIAL: u8 = 0x33;

    pub const STROKE_NONE: u8 = 0x40;
    pub const STROKE_PRESENT: u8 = 0x41;

    pub const TRANSFORM_IDENTITY: u8 = 0x50;
    pub const TRANSFORM_MATRIX: u8 = 0x51;

    pub const VIEWBOX: u8 = 0x60;
    pub const GRAPHICS_STATE: u8 = 0x61;
}

/// Decoder state.
struct Decoder<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Decoder<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn read_u8(&mut self) -> Result<u8, DecodeError> {
        if self.pos >= self.data.len() {
            return Err(DecodeError::UnexpectedEnd);
        }
        let v = self.data[self.pos];
        self.pos += 1;
        Ok(v)
    }

    fn read_u32(&mut self) -> Result<u32, DecodeError> {
        if self.pos + 4 > self.data.len() {
            return Err(DecodeError::UnexpectedEnd);
        }
        let bytes: [u8; 4] = self.data[self.pos..self.pos + 4].try_into().unwrap();
        self.pos += 4;
        Ok(u32::from_le_bytes(bytes))
    }

    fn read_f32(&mut self) -> Result<f32, DecodeError> {
        if self.pos + 4 > self.data.len() {
            return Err(DecodeError::UnexpectedEnd);
        }
        let bytes: [u8; 4] = self.data[self.pos..self.pos + 4].try_into().unwrap();
        self.pos += 4;
        Ok(f32::from_le_bytes(bytes))
    }

    fn read_point(&mut self) -> Result<Point, DecodeError> {
        let x = self.read_f32()?;
        let y = self.read_f32()?;
        Ok(Point::new(x, y))
    }

    fn read_color(&mut self) -> Result<Color, DecodeError> {
        let r = self.read_u8()?;
        let g = self.read_u8()?;
        let b = self.read_u8()?;
        let a = self.read_u8()?;
        Ok(Color::new(r, g, b, a))
    }

    fn read_string(&mut self) -> Result<String, DecodeError> {
        let len = self.read_u32()? as usize;
        if self.pos + len > self.data.len() {
            return Err(DecodeError::UnexpectedEnd);
        }
        let bytes = &self.data[self.pos..self.pos + len];
        self.pos += len;
        std::str::from_utf8(bytes)
            .map(|s| s.to_string())
            .map_err(|_| DecodeError::InvalidString)
    }
}

/// Decode a plot from bytes.
pub fn decode(bytes: &[u8]) -> Result<Plot, DecodeError> {
    if bytes.len() < MAGIC.len() {
        return Err(DecodeError::InvalidMagic);
    }
    if bytes[0..4] != MAGIC {
        return Err(DecodeError::InvalidMagic);
    }

    let mut decoder = Decoder::new(&bytes[4..]);
    let mut plot = Plot::new();
    let mut max_id = 0u32;

    loop {
        let cmd = decoder.read_u8()?;
        match cmd {
            cmd::END => break,
            cmd::VIEWBOX => {
                let x = decoder.read_f32()?;
                let y = decoder.read_f32()?;
                let w = decoder.read_f32()?;
                let h = decoder.read_f32()?;
                plot.set_viewbox(Rect::new(x, y, w, h));
            }
            cmd::GRAPHICS_STATE => {
                let fill_color = decoder.read_color()?;
                let stroke_color = decoder.read_color()?;
                let stroke_width = decoder.read_f32()?;
                plot.set_fill_color(fill_color);
                plot.set_stroke_color(stroke_color);
                plot.set_stroke_width(stroke_width);
            }
            cmd::ELEMENT => {
                let elem = decode_element(&mut decoder, &mut max_id)?;
                plot.add_anonymous(elem);
            }
            _ => return Err(DecodeError::InvalidCommand(cmd)),
        }
    }

    Ok(plot)
}

fn decode_element(decoder: &mut Decoder, max_id: &mut u32) -> Result<Element, DecodeError> {
    let id_val = decoder.read_u32()?;
    let id = if id_val == 0 {
        None
    } else {
        *max_id = (*max_id).max(id_val);
        Some(ElementId::new(id_val))
    };

    let transform = decode_transform(decoder)?;

    let kind_cmd = decoder.read_u8()?;
    let kind = match kind_cmd {
        cmd::KIND_PATH => {
            let path = decode_path(decoder)?;
            let fill = decode_paint(decoder)?;
            let stroke = decode_stroke(decoder)?;
            ElementKind::Path { path, fill, stroke }
        }
        cmd::KIND_TEXT => {
            let x = decoder.read_f32()?;
            let y = decoder.read_f32()?;
            let size = decoder.read_f32()?;
            let text = decoder.read_string()?;
            ElementKind::Text {
                pos: Point::new(x, y),
                text,
                size,
            }
        }
        cmd::KIND_GROUP => {
            let count = decoder.read_u32()? as usize;
            let mut children = Vec::with_capacity(count);
            for _ in 0..count {
                // Each child is preceded by ELEMENT marker
                let elem_cmd = decoder.read_u8()?;
                if elem_cmd != cmd::ELEMENT {
                    return Err(DecodeError::InvalidCommand(elem_cmd));
                }
                children.push(decode_element(decoder, max_id)?);
            }
            ElementKind::Group { children }
        }
        _ => return Err(DecodeError::InvalidCommand(kind_cmd)),
    };

    Ok(Element {
        id,
        kind,
        transform,
    })
}

fn decode_transform(decoder: &mut Decoder) -> Result<Transform, DecodeError> {
    let cmd = decoder.read_u8()?;
    match cmd {
        cmd::TRANSFORM_IDENTITY => Ok(Transform::IDENTITY),
        cmd::TRANSFORM_MATRIX => {
            let a = decoder.read_f32()?;
            let b = decoder.read_f32()?;
            let c = decoder.read_f32()?;
            let d = decoder.read_f32()?;
            let e = decoder.read_f32()?;
            let f = decoder.read_f32()?;
            Ok(Transform::new(a, b, c, d, e, f))
        }
        _ => Err(DecodeError::InvalidCommand(cmd)),
    }
}

fn decode_path(decoder: &mut Decoder) -> Result<Path, DecodeError> {
    let start_cmd = decoder.read_u8()?;
    if start_cmd != cmd::PATH_START {
        return Err(DecodeError::InvalidCommand(start_cmd));
    }

    let mut path = Path::new();

    loop {
        let cmd = decoder.read_u8()?;
        match cmd {
            cmd::PATH_END => break,
            cmd::MOVE_TO => {
                let p = decoder.read_point()?;
                path.move_to(p);
            }
            cmd::LINE_TO => {
                let p = decoder.read_point()?;
                path.line_to(p);
            }
            cmd::QUAD_TO => {
                let ctrl = decoder.read_point()?;
                let end = decoder.read_point()?;
                path.quad_to(ctrl, end);
            }
            cmd::CUBIC_TO => {
                let c1 = decoder.read_point()?;
                let c2 = decoder.read_point()?;
                let end = decoder.read_point()?;
                path.cubic_to(c1, c2, end);
            }
            cmd::ARC => {
                let center = decoder.read_point()?;
                let radius = decoder.read_f32()?;
                let start_angle = decoder.read_f32()?;
                let sweep_angle = decoder.read_f32()?;
                path.arc(center, radius, start_angle, sweep_angle);
            }
            cmd::CLOSE => {
                path.close();
            }
            _ => return Err(DecodeError::InvalidCommand(cmd)),
        }
    }

    Ok(path)
}

fn decode_paint(decoder: &mut Decoder) -> Result<Option<Paint>, DecodeError> {
    let cmd = decoder.read_u8()?;
    match cmd {
        cmd::PAINT_NONE => Ok(None),
        cmd::PAINT_SOLID => {
            let color = decoder.read_color()?;
            Ok(Some(Paint::Solid(color)))
        }
        cmd::PAINT_LINEAR => {
            let start = decoder.read_point()?;
            let end = decoder.read_point()?;
            let count = decoder.read_u32()? as usize;
            let mut stops = Vec::with_capacity(count);
            for _ in 0..count {
                let offset = decoder.read_f32()?;
                let color = decoder.read_color()?;
                stops.push(GradientStop::new(offset, color));
            }
            Ok(Some(Paint::LinearGradient { start, end, stops }))
        }
        cmd::PAINT_RADIAL => {
            let center = decoder.read_point()?;
            let radius = decoder.read_f32()?;
            let count = decoder.read_u32()? as usize;
            let mut stops = Vec::with_capacity(count);
            for _ in 0..count {
                let offset = decoder.read_f32()?;
                let color = decoder.read_color()?;
                stops.push(GradientStop::new(offset, color));
            }
            Ok(Some(Paint::RadialGradient { center, radius, stops }))
        }
        _ => Err(DecodeError::InvalidCommand(cmd)),
    }
}

fn decode_stroke(decoder: &mut Decoder) -> Result<Option<Stroke>, DecodeError> {
    let cmd = decoder.read_u8()?;
    match cmd {
        cmd::STROKE_NONE => Ok(None),
        cmd::STROKE_PRESENT => {
            let width = decoder.read_f32()?;
            let color = decoder.read_color()?;
            let cap_byte = decoder.read_u8()?;
            let join_byte = decoder.read_u8()?;

            let line_cap = match cap_byte {
                0 => LineCap::Butt,
                1 => LineCap::Round,
                2 => LineCap::Square,
                _ => LineCap::Butt,
            };
            let line_join = match join_byte {
                0 => LineJoin::Miter,
                1 => LineJoin::Round,
                2 => LineJoin::Bevel,
                _ => LineJoin::Miter,
            };

            Ok(Some(Stroke {
                width,
                color,
                line_cap,
                line_join,
            }))
        }
        _ => Err(DecodeError::InvalidCommand(cmd)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::encode::encode;

    #[test]
    fn decode_empty_plot() {
        let plot = Plot::new();
        let bytes = encode(&plot);
        let decoded = decode(&bytes).unwrap();

        assert!(decoded.is_empty());
    }

    #[test]
    fn decode_invalid_magic() {
        let bytes = [0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode(&bytes);
        assert!(matches!(result, Err(DecodeError::InvalidMagic)));
    }

    #[test]
    fn roundtrip_circle() {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        plot.add_circle(50.0, 50.0, 25.0);

        let bytes = encode(&plot);
        let decoded = decode(&bytes).unwrap();

        assert_eq!(decoded.len(), 1);

        // Check bounds match
        let orig_bounds = plot.bounds();
        let decoded_bounds = decoded.bounds();
        assert!((orig_bounds.x - decoded_bounds.x).abs() < 0.001);
        assert!((orig_bounds.y - decoded_bounds.y).abs() < 0.001);
    }

    #[test]
    fn roundtrip_with_viewbox() {
        let mut plot = Plot::new();
        plot.set_viewbox(Rect::new(0.0, 0.0, 100.0, 100.0));
        plot.set_fill_color(Color::BLUE);
        plot.add_rect(10.0, 10.0, 80.0, 80.0);

        let bytes = encode(&plot);
        let decoded = decode(&bytes).unwrap();

        assert!(decoded.viewbox().is_some());
        let vb = decoded.viewbox().unwrap();
        assert_eq!(vb.x, 0.0);
        assert_eq!(vb.y, 0.0);
        assert_eq!(vb.w, 100.0);
        assert_eq!(vb.h, 100.0);
    }

    #[test]
    fn roundtrip_text() {
        let mut plot = Plot::new();
        plot.add_text(10.0, 20.0, "Hello".to_string(), 12.0);

        let bytes = encode(&plot);
        let decoded = decode(&bytes).unwrap();

        assert_eq!(decoded.len(), 1);
        if let ElementKind::Text { pos, text, size } = &decoded.elements()[0].kind {
            assert_eq!(pos.x, 10.0);
            assert_eq!(pos.y, 20.0);
            assert_eq!(text, "Hello");
            assert_eq!(*size, 12.0);
        } else {
            panic!("Expected text element");
        }
    }
}
