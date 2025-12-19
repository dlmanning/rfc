//! Plot renderer trait and dispatch logic.
//!
//! This module provides:
//! - [`PlotRenderer`] trait for implementing custom renderers
//! - [`render_plot`] function to decode and dispatch plot commands
//!
//! # Graphics State Model
//!
//! The renderer maintains:
//! - Current position (for path commands)
//! - Stroke color and fill color
//! - Line width
//! - Font settings
//! - Transform matrix
//! - State stack (for push/pop operations)
//!
//! # Coordinate System
//!
//! Coordinates are floating-point values. The origin and orientation
//! depend on the renderer implementation, but typically:
//! - Origin at top-left
//! - X increases rightward
//! - Y increases downward

use crate::{color::Color, commands::*, encoding::*, error::DecodeError};

/// Trait for rendering Plot objects.
///
/// Implement this trait to render plots to your target (screen, canvas, SVG, etc.).
///
/// # Shape Accumulation
///
/// Shape commands (`circle`, `rect`, etc.) add shapes to an internal path.
/// Call `fill()` or `stroke()` to render and clear the accumulated shapes.
pub trait PlotRenderer {
    /// Move the current point without drawing.
    fn move_to(&mut self, x: f64, y: f64);

    /// Draw a line from the current point to (x, y).
    fn line_to(&mut self, x: f64, y: f64);

    /// Add a circle to the current path.
    fn circle(&mut self, cx: f64, cy: f64, radius: f64);

    /// Add a rectangle to the current path.
    fn rect(&mut self, x: f64, y: f64, width: f64, height: f64);

    /// Add an ellipse to the current path.
    fn ellipse(&mut self, cx: f64, cy: f64, rx: f64, ry: f64);

    /// Add an arc to the current path.
    fn arc(&mut self, cx: f64, cy: f64, radius: f64, start_angle: f64, end_angle: f64);

    /// Add a cubic Bézier curve to the current path.
    ///
    /// Control points: (x1, y1) and (x2, y2). End point: (x3, y3).
    fn bezier(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64);

    /// Draw a single pixel at (x, y).
    fn pixel(&mut self, x: f64, y: f64);

    /// Draw text at the given position.
    fn text(&mut self, x: f64, y: f64, text: &str);

    /// Stroke the current path with the stroke color, then clear the path.
    fn stroke(&mut self);

    /// Fill the current path with the fill color, then clear the path.
    fn fill(&mut self);

    /// Set the line width for stroking.
    fn set_line_width(&mut self, width: f64);

    /// Set the stroke color (packed RGBA: 0xRRGGBBAA).
    fn set_stroke_color(&mut self, rgba: u32);

    /// Set the fill color (packed RGBA: 0xRRGGBBAA).
    fn set_fill_color(&mut self, rgba: u32);

    /// Set the font for text rendering.
    fn set_font(&mut self, size: f64, name: &str);

    /// Reset the transformation matrix to identity.
    fn identity(&mut self);

    /// Apply a 2D affine transformation matrix.
    ///
    /// Matrix layout: `[a b; c d]` with translation `[e; f]`
    fn transform(&mut self, a: f64, b: f64, c: f64, d: f64, e: f64, f: f64);

    /// Scale the coordinate system.
    fn scale(&mut self, sx: f64, sy: f64);

    /// Rotate the coordinate system (angle in radians).
    fn rotate(&mut self, angle: f64);

    /// Translate the coordinate system.
    fn translate(&mut self, dx: f64, dy: f64);

    /// Push the current graphics state onto the stack.
    fn push_state(&mut self);

    /// Pop the graphics state from the stack.
    fn pop_state(&mut self);

    /// Set the clipping region to the current path.
    fn clip(&mut self);
}

/// Render a Plot byte stream using the given renderer.
///
/// Decodes commands from `bytes` and dispatches them to the renderer.
/// Stops at `CMD_END` or end of data.
///
/// # Errors
///
/// Returns `DecodeError::InvalidMagic` if the blob doesn't have a valid Plot header.
/// Returns `DecodeError::UnexpectedEnd` if the data ends mid-command.
/// Returns `DecodeError::InvalidNumber` if number decoding fails.
/// Returns `DecodeError::InvalidString` if string decoding fails.
pub fn render_plot<R: PlotRenderer>(bytes: &[u8], renderer: &mut R) -> Result<(), DecodeError> {
    // Validate and skip magic header
    let commands = validate_plot_blob(bytes).map_err(|_| DecodeError::InvalidMagic)?;
    let mut offset = 0;

    while offset < commands.len() {
        let cmd = commands[offset];
        offset += 1;

        match cmd {
            CMD_END => break,

            CMD_MOVETO => {
                let (x, y, consumed) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.move_to(x, y);
            }

            CMD_LINETO => {
                let (x, y, consumed) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.line_to(x, y);
            }

            CMD_CIRCLE => {
                let (cx, cy, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (r, c2) = decode_coord(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                renderer.circle(cx, cy, r);
            }

            CMD_RECT => {
                let (x, y, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (w, h, c2) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                renderer.rect(x, y, w, h);
            }

            CMD_ELLIPSE => {
                let (cx, cy, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (rx, ry, c2) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                renderer.ellipse(cx, cy, rx, ry);
            }

            CMD_ARC => {
                let (cx, cy, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (r, c2) = decode_coord(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                let (start, end, c3) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c3;
                renderer.arc(cx, cy, r, start, end);
            }

            CMD_BEZIER => {
                let (x1, y1, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (x2, y2, c2) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                let (x3, y3, c3) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c3;
                renderer.bezier(x1, y1, x2, y2, x3, y3);
            }

            CMD_PIXEL => {
                let (x, y, consumed) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.pixel(x, y);
            }

            CMD_TEXT => {
                let (x, y, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (text, c2) =
                    decode_string(&commands[offset..]).ok_or(DecodeError::InvalidString)?;
                offset += c2;
                renderer.text(x, y, &text);
            }

            CMD_FILL => {
                renderer.fill();
            }

            CMD_STROKE => {
                renderer.stroke();
            }

            CMD_LINEWIDTH => {
                let (w, consumed) =
                    decode_coord(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.set_line_width(w);
            }

            CMD_COLOR => {
                let (r, g, b, a, consumed) =
                    decode_rgba(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                let color = Color::new(r, g, b, a);
                renderer.set_stroke_color(color.to_packed());
            }

            CMD_FILLCOLOR => {
                let (r, g, b, a, consumed) =
                    decode_rgba(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                let color = Color::new(r, g, b, a);
                renderer.set_fill_color(color.to_packed());
            }

            CMD_FONT => {
                let (size, c1) =
                    decode_coord(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (name, c2) =
                    decode_string(&commands[offset..]).ok_or(DecodeError::InvalidString)?;
                offset += c2;
                renderer.set_font(size, &name);
            }

            CMD_IDENTITY => {
                renderer.identity();
            }

            CMD_TRANSFORM => {
                let (a, b, c1) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c1;
                let (c, d, c2) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c2;
                let (e, f, c3) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += c3;
                renderer.transform(a, b, c, d, e, f);
            }

            CMD_SCALE => {
                let (sx, sy, consumed) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.scale(sx, sy);
            }

            CMD_ROTATE => {
                let (angle, consumed) =
                    decode_coord(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.rotate(angle);
            }

            CMD_TRANSLATE => {
                let (dx, dy, consumed) =
                    decode_two_coords(&commands[offset..]).ok_or(DecodeError::UnexpectedEnd)?;
                offset += consumed;
                renderer.translate(dx, dy);
            }

            CMD_PUSH => {
                renderer.push_state();
            }

            CMD_POP => {
                renderer.pop_state();
            }

            CMD_CLIP => {
                renderer.clip();
            }

            _ => {
                // Unknown command - skip (for forward compatibility)
            }
        }
    }

    Ok(())
}

// Helper functions for decoding

fn decode_coord(bytes: &[u8]) -> Option<(f64, usize)> {
    let (fp, consumed) = decode_number(bytes)?;
    Some((from_fixed_point(fp as i32), consumed))
}

fn decode_two_coords(bytes: &[u8]) -> Option<(f64, f64, usize)> {
    let (x, c1) = decode_coord(bytes)?;
    let (y, c2) = decode_coord(&bytes[c1..])?;
    Some((x, y, c1 + c2))
}

fn decode_rgba(bytes: &[u8]) -> Option<(u8, u8, u8, u8, usize)> {
    let mut offset = 0;
    let (r, c1) = decode_number(bytes)?;
    offset += c1;
    let (g, c2) = decode_number(&bytes[offset..])?;
    offset += c2;
    let (b, c3) = decode_number(&bytes[offset..])?;
    offset += c3;
    let (a, c4) = decode_number(&bytes[offset..])?;
    offset += c4;
    Some((r as u8, g as u8, b as u8, a as u8, offset))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Mock renderer that records all method calls.
    #[derive(Default)]
    struct MockRenderer {
        calls: Vec<String>,
    }

    impl PlotRenderer for MockRenderer {
        fn move_to(&mut self, x: f64, y: f64) {
            self.calls.push(format!("move_to({x}, {y})"));
        }
        fn line_to(&mut self, x: f64, y: f64) {
            self.calls.push(format!("line_to({x}, {y})"));
        }
        fn circle(&mut self, cx: f64, cy: f64, r: f64) {
            self.calls.push(format!("circle({cx}, {cy}, {r})"));
        }
        fn rect(&mut self, x: f64, y: f64, w: f64, h: f64) {
            self.calls.push(format!("rect({x}, {y}, {w}, {h})"));
        }
        fn ellipse(&mut self, cx: f64, cy: f64, rx: f64, ry: f64) {
            self.calls.push(format!("ellipse({cx}, {cy}, {rx}, {ry})"));
        }
        fn arc(&mut self, cx: f64, cy: f64, r: f64, start: f64, end: f64) {
            self.calls
                .push(format!("arc({cx}, {cy}, {r}, {start}, {end})"));
        }
        fn bezier(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64) {
            self.calls
                .push(format!("bezier({x1}, {y1}, {x2}, {y2}, {x3}, {y3})"));
        }
        fn pixel(&mut self, x: f64, y: f64) {
            self.calls.push(format!("pixel({x}, {y})"));
        }
        fn text(&mut self, x: f64, y: f64, text: &str) {
            self.calls.push(format!("text({x}, {y}, \"{text}\")"));
        }
        fn stroke(&mut self) {
            self.calls.push("stroke()".into());
        }
        fn fill(&mut self) {
            self.calls.push("fill()".into());
        }
        fn set_line_width(&mut self, w: f64) {
            self.calls.push(format!("set_line_width({w})"));
        }
        fn set_stroke_color(&mut self, rgba: u32) {
            self.calls.push(format!("set_stroke_color(0x{rgba:08X})"));
        }
        fn set_fill_color(&mut self, rgba: u32) {
            self.calls.push(format!("set_fill_color(0x{rgba:08X})"));
        }
        fn set_font(&mut self, size: f64, name: &str) {
            self.calls.push(format!("set_font({size}, \"{name}\")"));
        }
        fn identity(&mut self) {
            self.calls.push("identity()".into());
        }
        fn transform(&mut self, a: f64, b: f64, c: f64, d: f64, e: f64, f: f64) {
            self.calls
                .push(format!("transform({a}, {b}, {c}, {d}, {e}, {f})"));
        }
        fn scale(&mut self, sx: f64, sy: f64) {
            self.calls.push(format!("scale({sx}, {sy})"));
        }
        fn rotate(&mut self, angle: f64) {
            self.calls.push(format!("rotate({angle})"));
        }
        fn translate(&mut self, dx: f64, dy: f64) {
            self.calls.push(format!("translate({dx}, {dy})"));
        }
        fn push_state(&mut self) {
            self.calls.push("push_state()".into());
        }
        fn pop_state(&mut self) {
            self.calls.push("pop_state()".into());
        }
        fn clip(&mut self) {
            self.calls.push("clip()".into());
        }
    }

    /// Build a plot blob with magic header and given commands.
    fn build_plot(commands: &[u8]) -> Vec<u8> {
        let mut bytes = PLOT_MAGIC.to_vec();
        bytes.extend_from_slice(commands);
        bytes
    }

    /// Build a simple plot with a circle.
    fn build_circle_plot(x: f64, y: f64, r: f64) -> Vec<u8> {
        let mut commands = vec![CMD_CIRCLE];
        commands.extend(encode_number(to_fixed_point(x) as i64));
        commands.extend(encode_number(to_fixed_point(y) as i64));
        commands.extend(encode_number(to_fixed_point(r) as i64));
        commands.push(CMD_END);
        build_plot(&commands)
    }

    #[test]
    fn test_render_circle() {
        let plot = build_circle_plot(100.0, 100.0, 50.0);
        let mut renderer = MockRenderer::default();
        render_plot(&plot, &mut renderer).unwrap();
        assert_eq!(renderer.calls.len(), 1);
        assert_eq!(renderer.calls[0], "circle(100, 100, 50)");
    }

    #[test]
    fn test_render_fill_stroke() {
        let plot = build_plot(&[CMD_FILL, CMD_STROKE, CMD_END]);
        let mut renderer = MockRenderer::default();
        render_plot(&plot, &mut renderer).unwrap();
        assert_eq!(renderer.calls, vec!["fill()", "stroke()"]);
    }

    #[test]
    fn test_render_state_commands() {
        let plot = build_plot(&[CMD_PUSH, CMD_IDENTITY, CMD_POP, CMD_END]);
        let mut renderer = MockRenderer::default();
        render_plot(&plot, &mut renderer).unwrap();
        assert_eq!(
            renderer.calls,
            vec!["push_state()", "identity()", "pop_state()"]
        );
    }

    #[test]
    fn test_render_empty_plot() {
        let plot = build_plot(&[CMD_END]);
        let mut renderer = MockRenderer::default();
        render_plot(&plot, &mut renderer).unwrap();
        assert!(renderer.calls.is_empty());
    }

    #[test]
    fn test_render_no_end_marker() {
        // Should handle gracefully without CMD_END
        let plot = build_plot(&[CMD_FILL]);
        let mut renderer = MockRenderer::default();
        render_plot(&plot, &mut renderer).unwrap();
        assert_eq!(renderer.calls, vec!["fill()"]);
    }

    #[test]
    fn test_render_truncated_data_fails() {
        // Circle command without any coordinate data
        let plot = build_plot(&[CMD_CIRCLE]);
        let mut renderer = MockRenderer::default();
        let result = render_plot(&plot, &mut renderer);
        assert!(result.is_err());
    }

    #[test]
    fn test_render_invalid_magic_fails() {
        // Random bytes without magic header
        let plot = vec![0x00, 0x01, 0x02, 0x03, CMD_END];
        let mut renderer = MockRenderer::default();
        let result = render_plot(&plot, &mut renderer);
        assert_eq!(result, Err(DecodeError::InvalidMagic));
    }

    #[test]
    fn test_render_empty_blob_fails() {
        let plot = vec![];
        let mut renderer = MockRenderer::default();
        let result = render_plot(&plot, &mut renderer);
        assert_eq!(result, Err(DecodeError::InvalidMagic));
    }
}
