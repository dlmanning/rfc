//! Plot renderer trait and dispatch logic.
//!
//! This module defines the renderer protocol that allows Plot objects to be
//! rendered to any target (framebuffer, bitmap, SVG, etc.) by implementing
//! the `PlotRenderer` trait.

use super::commands::*;
use super::encoding::*;
use super::pack_rgba;

/// Trait for rendering Plot objects.
///
/// Implementors receive decoded drawing commands and write pixels to their
/// target surface. The renderer doesn't need to understand the Plot byte
/// stream format - it just handles high-level drawing operations.
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
    fn bezier(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64);

    /// Draw a single pixel.
    fn pixel(&mut self, x: f64, y: f64);

    /// Draw text at the given position.
    fn text(&mut self, x: f64, y: f64, text: &str);

    /// Stroke the current path with the current stroke color.
    fn stroke(&mut self);

    /// Fill the current path with the current fill color.
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
/// This function walks the Plot byte stream, decodes each command, and
/// dispatches to the appropriate method on the renderer.
pub fn render_plot<R: PlotRenderer>(bytes: &[u8], renderer: &mut R) {
    let mut offset = 0;

    while offset < bytes.len() {
        let cmd = bytes[offset];
        offset += 1;

        match cmd {
            CMD_END => break,

            CMD_MOVETO => {
                if let Some((x, y, consumed)) = decode_two_coords(&bytes[offset..]) {
                    offset += consumed;
                    renderer.move_to(x, y);
                }
            }

            CMD_LINETO => {
                if let Some((x, y, consumed)) = decode_two_coords(&bytes[offset..]) {
                    offset += consumed;
                    renderer.line_to(x, y);
                }
            }

            CMD_CIRCLE => {
                if let Some((cx, cy, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((r, c2)) = decode_coord(&bytes[offset..]) {
                        offset += c2;
                        renderer.circle(cx, cy, r);
                    }
                }
            }

            CMD_RECT => {
                if let Some((x, y, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((w, h, c2)) = decode_two_coords(&bytes[offset..]) {
                        offset += c2;
                        renderer.rect(x, y, w, h);
                    }
                }
            }

            CMD_ELLIPSE => {
                if let Some((cx, cy, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((rx, ry, c2)) = decode_two_coords(&bytes[offset..]) {
                        offset += c2;
                        renderer.ellipse(cx, cy, rx, ry);
                    }
                }
            }

            CMD_ARC => {
                if let Some((cx, cy, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((r, c2)) = decode_coord(&bytes[offset..]) {
                        offset += c2;
                        if let Some((start, end, c3)) = decode_two_coords(&bytes[offset..]) {
                            offset += c3;
                            renderer.arc(cx, cy, r, start, end);
                        }
                    }
                }
            }

            CMD_BEZIER => {
                if let Some((x1, y1, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((x2, y2, c2)) = decode_two_coords(&bytes[offset..]) {
                        offset += c2;
                        if let Some((x3, y3, c3)) = decode_two_coords(&bytes[offset..]) {
                            offset += c3;
                            renderer.bezier(x1, y1, x2, y2, x3, y3);
                        }
                    }
                }
            }

            CMD_PIXEL => {
                if let Some((x, y, consumed)) = decode_two_coords(&bytes[offset..]) {
                    offset += consumed;
                    renderer.pixel(x, y);
                }
            }

            CMD_TEXT => {
                if let Some((x, y, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((text, c2)) = decode_string(&bytes[offset..]) {
                        offset += c2;
                        renderer.text(x, y, &text);
                    }
                }
            }

            CMD_FILL => {
                renderer.fill();
            }

            CMD_STROKE => {
                renderer.stroke();
            }

            CMD_LINEWIDTH => {
                if let Some((w, consumed)) = decode_coord(&bytes[offset..]) {
                    offset += consumed;
                    renderer.set_line_width(w);
                }
            }

            CMD_COLOR => {
                if let Some((r, g, b, a, consumed)) = decode_rgba(&bytes[offset..]) {
                    offset += consumed;
                    renderer.set_stroke_color(pack_rgba(r, g, b, a));
                }
            }

            CMD_FILLCOLOR => {
                if let Some((r, g, b, a, consumed)) = decode_rgba(&bytes[offset..]) {
                    offset += consumed;
                    renderer.set_fill_color(pack_rgba(r, g, b, a));
                }
            }

            CMD_FONT => {
                if let Some((size, c1)) = decode_coord(&bytes[offset..]) {
                    offset += c1;
                    if let Some((name, c2)) = decode_string(&bytes[offset..]) {
                        offset += c2;
                        renderer.set_font(size, &name);
                    }
                }
            }

            CMD_IDENTITY => {
                renderer.identity();
            }

            CMD_TRANSFORM => {
                if let Some((a, b, c1)) = decode_two_coords(&bytes[offset..]) {
                    offset += c1;
                    if let Some((c, d, c2)) = decode_two_coords(&bytes[offset..]) {
                        offset += c2;
                        if let Some((e, f, c3)) = decode_two_coords(&bytes[offset..]) {
                            offset += c3;
                            renderer.transform(a, b, c, d, e, f);
                        }
                    }
                }
            }

            CMD_SCALE => {
                if let Some((sx, sy, consumed)) = decode_two_coords(&bytes[offset..]) {
                    offset += consumed;
                    renderer.scale(sx, sy);
                }
            }

            CMD_ROTATE => {
                if let Some((angle, consumed)) = decode_coord(&bytes[offset..]) {
                    offset += consumed;
                    renderer.rotate(angle);
                }
            }

            CMD_TRANSLATE => {
                if let Some((dx, dy, consumed)) = decode_two_coords(&bytes[offset..]) {
                    offset += consumed;
                    renderer.translate(dx, dy);
                }
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
                // Unknown command - skip
                // In a more robust implementation, we'd have a way to skip
                // unknown commands based on their argument count
            }
        }
    }
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
