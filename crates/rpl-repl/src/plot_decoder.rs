//! Plot bytecode decoder for rendering.
//!
//! Decodes plot objects into drawable commands for the TUI viewer.

use rpl_core::Word;
use rpl_stdlib::plot::{
    commands::*,
    encoding::{decode_number, decode_string, from_fixed_point, words_to_bytes},
};

/// A decoded plot command ready for rendering.
#[derive(Debug, Clone)]
pub enum PlotCommand {
    MoveTo {
        x: f64,
        y: f64,
    },
    LineTo {
        x: f64,
        y: f64,
    },
    Circle {
        x: f64,
        y: f64,
        radius: f64,
    },
    Rect {
        x: f64,
        y: f64,
        width: f64,
        height: f64,
    },
    Ellipse {
        x: f64,
        y: f64,
        rx: f64,
        ry: f64,
    },
    Arc {
        x: f64,
        y: f64,
        radius: f64,
        start: f64,
        end: f64,
    },
    Bezier {
        x1: f64,
        y1: f64,
        x2: f64,
        y2: f64,
        x3: f64,
        y3: f64,
    },
    Pixel {
        x: f64,
        y: f64,
    },
    Color {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    FillColor {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    LineWidth {
        width: f64,
    },
    Text {
        x: f64,
        y: f64,
        text: String,
    },
    Fill,
    Stroke,
    PushState,
    PopState,
    Identity,
    Translate {
        dx: f64,
        dy: f64,
    },
    Scale {
        sx: f64,
        sy: f64,
    },
    Rotate {
        angle: f64,
    },
    Clip,
}

/// Iterator that decodes plot bytecode into commands.
pub struct PlotDecoder {
    bytes: Vec<u8>,
    pos: usize,
}

impl PlotDecoder {
    /// Create a new decoder from plot object data (Word array).
    pub fn new(words: &[Word]) -> Self {
        Self {
            bytes: words_to_bytes(words),
            pos: 0,
        }
    }

    /// Decode a coordinate (Q8.24 fixed-point).
    fn decode_coord(&mut self) -> Option<f64> {
        let (val, consumed) = decode_number(&self.bytes[self.pos..])?;
        self.pos += consumed;
        Some(from_fixed_point(val as i32))
    }

    /// Decode a color component (0-255 integer).
    fn decode_color_component(&mut self) -> Option<u8> {
        let (val, consumed) = decode_number(&self.bytes[self.pos..])?;
        self.pos += consumed;
        Some(val.clamp(0, 255) as u8)
    }

    /// Decode a string.
    fn decode_str(&mut self) -> Option<String> {
        let (s, consumed) = decode_string(&self.bytes[self.pos..])?;
        self.pos += consumed;
        Some(s)
    }

    /// Calculate the bounding box of all drawable elements.
    /// Returns (min_x, max_x, min_y, max_y).
    pub fn bounds(words: &[Word]) -> (f64, f64, f64, f64) {
        let mut min_x = f64::MAX;
        let mut max_x = f64::MIN;
        let mut min_y = f64::MAX;
        let mut max_y = f64::MIN;

        let mut update_point = |x: f64, y: f64| {
            min_x = min_x.min(x);
            max_x = max_x.max(x);
            min_y = min_y.min(y);
            max_y = max_y.max(y);
        };

        let decoder = PlotDecoder::new(words);
        for cmd in decoder {
            match cmd {
                PlotCommand::MoveTo { x, y }
                | PlotCommand::LineTo { x, y }
                | PlotCommand::Pixel { x, y } => {
                    update_point(x, y);
                }
                PlotCommand::Circle { x, y, radius } => {
                    update_point(x - radius, y - radius);
                    update_point(x + radius, y + radius);
                }
                PlotCommand::Rect {
                    x,
                    y,
                    width,
                    height,
                } => {
                    update_point(x, y);
                    update_point(x + width, y + height);
                }
                PlotCommand::Ellipse { x, y, rx, ry } => {
                    update_point(x - rx, y - ry);
                    update_point(x + rx, y + ry);
                }
                PlotCommand::Arc { x, y, radius, .. } => {
                    // Conservative bounds for arc
                    update_point(x - radius, y - radius);
                    update_point(x + radius, y + radius);
                }
                PlotCommand::Bezier {
                    x1,
                    y1,
                    x2,
                    y2,
                    x3,
                    y3,
                } => {
                    // Include all control points
                    update_point(x1, y1);
                    update_point(x2, y2);
                    update_point(x3, y3);
                }
                PlotCommand::Text { x, y, .. } => {
                    update_point(x, y);
                }
                _ => {}
            }
        }

        // Handle empty plot or no geometric commands
        if min_x > max_x || min_y > max_y {
            (0.0, 100.0, 0.0, 100.0)
        } else {
            // Add small margin
            let margin_x = (max_x - min_x) * 0.05;
            let margin_y = (max_y - min_y) * 0.05;
            (
                min_x - margin_x,
                max_x + margin_x,
                min_y - margin_y,
                max_y + margin_y,
            )
        }
    }
}

impl Iterator for PlotDecoder {
    type Item = PlotCommand;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.bytes.len() {
            return None;
        }

        let cmd_byte = self.bytes[self.pos];
        self.pos += 1;

        // Check for end marker
        if cmd_byte == CMD_END {
            return None;
        }

        // Skip non-command bytes
        if !is_command(cmd_byte) {
            return self.next();
        }

        match cmd_byte {
            CMD_MOVETO => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                Some(PlotCommand::MoveTo { x, y })
            }
            CMD_LINETO => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                Some(PlotCommand::LineTo { x, y })
            }
            CMD_CIRCLE => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                let radius = self.decode_coord()?;
                Some(PlotCommand::Circle { x, y, radius })
            }
            CMD_RECT => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                let width = self.decode_coord()?;
                let height = self.decode_coord()?;
                Some(PlotCommand::Rect {
                    x,
                    y,
                    width,
                    height,
                })
            }
            CMD_ELLIPSE => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                let rx = self.decode_coord()?;
                let ry = self.decode_coord()?;
                Some(PlotCommand::Ellipse { x, y, rx, ry })
            }
            CMD_ARC => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                let radius = self.decode_coord()?;
                let start = self.decode_coord()?;
                let end = self.decode_coord()?;
                Some(PlotCommand::Arc {
                    x,
                    y,
                    radius,
                    start,
                    end,
                })
            }
            CMD_BEZIER => {
                let x1 = self.decode_coord()?;
                let y1 = self.decode_coord()?;
                let x2 = self.decode_coord()?;
                let y2 = self.decode_coord()?;
                let x3 = self.decode_coord()?;
                let y3 = self.decode_coord()?;
                Some(PlotCommand::Bezier {
                    x1,
                    y1,
                    x2,
                    y2,
                    x3,
                    y3,
                })
            }
            CMD_PIXEL => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                Some(PlotCommand::Pixel { x, y })
            }
            CMD_COLOR => {
                let r = self.decode_color_component()?;
                let g = self.decode_color_component()?;
                let b = self.decode_color_component()?;
                let a = self.decode_color_component()?;
                Some(PlotCommand::Color { r, g, b, a })
            }
            CMD_FILLCOLOR => {
                let r = self.decode_color_component()?;
                let g = self.decode_color_component()?;
                let b = self.decode_color_component()?;
                let a = self.decode_color_component()?;
                Some(PlotCommand::FillColor { r, g, b, a })
            }
            CMD_LINEWIDTH => {
                let width = self.decode_coord()?;
                Some(PlotCommand::LineWidth { width })
            }
            CMD_TEXT => {
                let x = self.decode_coord()?;
                let y = self.decode_coord()?;
                let text = self.decode_str()?;
                Some(PlotCommand::Text { x, y, text })
            }
            CMD_FONT => {
                // Skip font size and name - we don't use them for TUI rendering
                let _size = self.decode_coord()?;
                let _name = self.decode_str()?;
                self.next() // Skip and get next command
            }
            CMD_FILL => Some(PlotCommand::Fill),
            CMD_STROKE => Some(PlotCommand::Stroke),
            CMD_PUSH => Some(PlotCommand::PushState),
            CMD_POP => Some(PlotCommand::PopState),
            CMD_IDENTITY => Some(PlotCommand::Identity),
            CMD_TRANSLATE => {
                let dx = self.decode_coord()?;
                let dy = self.decode_coord()?;
                Some(PlotCommand::Translate { dx, dy })
            }
            CMD_SCALE => {
                let sx = self.decode_coord()?;
                let sy = self.decode_coord()?;
                Some(PlotCommand::Scale { sx, sy })
            }
            CMD_ROTATE => {
                let angle = self.decode_coord()?;
                Some(PlotCommand::Rotate { angle })
            }
            CMD_TRANSFORM => {
                // Skip 6 matrix values - we don't support full transforms
                for _ in 0..6 {
                    self.decode_coord()?;
                }
                self.next() // Skip and get next command
            }
            CMD_CLIP => Some(PlotCommand::Clip),
            _ => self.next(), // Skip unknown commands
        }
    }
}

/// Decompile plot bytecode back to RPL source code.
pub fn decompile_plot(words: &[Word]) -> String {
    let decoder = PlotDecoder::new(words);
    let mut lines = vec!["BEGINPLOT".to_string()];

    for cmd in decoder {
        let line = match cmd {
            PlotCommand::MoveTo { x, y } => format!("  {} {} MOVETO", fmt_num(x), fmt_num(y)),
            PlotCommand::LineTo { x, y } => format!("  {} {} LINETO", fmt_num(x), fmt_num(y)),
            PlotCommand::Circle { x, y, radius } => {
                format!("  {} {} {} CIRCLE", fmt_num(x), fmt_num(y), fmt_num(radius))
            }
            PlotCommand::Rect {
                x,
                y,
                width,
                height,
            } => {
                format!(
                    "  {} {} {} {} RECT",
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(width),
                    fmt_num(height)
                )
            }
            PlotCommand::Ellipse { x, y, rx, ry } => {
                format!(
                    "  {} {} {} {} ELLIPSE",
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(rx),
                    fmt_num(ry)
                )
            }
            PlotCommand::Arc {
                x,
                y,
                radius,
                start,
                end,
            } => {
                format!(
                    "  {} {} {} {} {} ARC",
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(radius),
                    fmt_num(start),
                    fmt_num(end)
                )
            }
            PlotCommand::Bezier {
                x1,
                y1,
                x2,
                y2,
                x3,
                y3,
            } => {
                format!(
                    "  {} {} {} {} {} {} BEZIER",
                    fmt_num(x1),
                    fmt_num(y1),
                    fmt_num(x2),
                    fmt_num(y2),
                    fmt_num(x3),
                    fmt_num(y3)
                )
            }
            PlotCommand::Pixel { x, y } => format!("  {} {} PIXEL", fmt_num(x), fmt_num(y)),
            PlotCommand::Color { r, g, b, a } => {
                format!("  {} {} {} {} COLOR", r, g, b, a)
            }
            PlotCommand::FillColor { r, g, b, a } => {
                format!("  {} {} {} {} FILLCOLOR", r, g, b, a)
            }
            PlotCommand::LineWidth { width } => format!("  {} LINEWIDTH", fmt_num(width)),
            PlotCommand::Text { x, y, text } => {
                format!("  {} {} \"{}\" TEXT", fmt_num(x), fmt_num(y), text)
            }
            PlotCommand::Fill => "  FILL".to_string(),
            PlotCommand::Stroke => "  STROKE".to_string(),
            PlotCommand::PushState => "  PUSHSTATE".to_string(),
            PlotCommand::PopState => "  POPSTATE".to_string(),
            PlotCommand::Identity => "  IDENTITY".to_string(),
            PlotCommand::Translate { dx, dy } => {
                format!("  {} {} TRANSLATE", fmt_num(dx), fmt_num(dy))
            }
            PlotCommand::Scale { sx, sy } => format!("  {} {} SCALE", fmt_num(sx), fmt_num(sy)),
            PlotCommand::Rotate { angle } => format!("  {} ROTATE", fmt_num(angle)),
            PlotCommand::Clip => "  CLIP".to_string(),
        };
        lines.push(line);
    }

    lines.push("ENDPLOT".to_string());
    lines.join("\n")
}

/// Format a number nicely (avoid unnecessary decimals).
fn fmt_num(n: f64) -> String {
    if n.fract().abs() < 0.0001 {
        format!("{}", n as i64)
    } else {
        format!("{:.2}", n)
    }
}

#[cfg(test)]
mod tests {
    use rpl_stdlib::plot::encoding::{bytes_to_words, encode_number, to_fixed_point};

    use super::*;

    fn make_simple_plot() -> Vec<Word> {
        let mut bytes = Vec::new();

        // MOVETO 10, 20
        bytes.push(CMD_MOVETO);
        bytes.extend(encode_number(to_fixed_point(10.0) as i64));
        bytes.extend(encode_number(to_fixed_point(20.0) as i64));

        // LINETO 30, 40
        bytes.push(CMD_LINETO);
        bytes.extend(encode_number(to_fixed_point(30.0) as i64));
        bytes.extend(encode_number(to_fixed_point(40.0) as i64));

        // CIRCLE 50, 50, 10
        bytes.push(CMD_CIRCLE);
        bytes.extend(encode_number(to_fixed_point(50.0) as i64));
        bytes.extend(encode_number(to_fixed_point(50.0) as i64));
        bytes.extend(encode_number(to_fixed_point(10.0) as i64));

        // END
        bytes.push(CMD_END);

        bytes_to_words(&bytes)
    }

    #[test]
    fn decode_simple_plot() {
        let words = make_simple_plot();
        let decoder = PlotDecoder::new(&words);
        let commands: Vec<_> = decoder.collect();

        assert_eq!(commands.len(), 3);

        match &commands[0] {
            PlotCommand::MoveTo { x, y } => {
                assert!((x - 10.0).abs() < 0.001);
                assert!((y - 20.0).abs() < 0.001);
            }
            _ => panic!("Expected MoveTo"),
        }

        match &commands[1] {
            PlotCommand::LineTo { x, y } => {
                assert!((x - 30.0).abs() < 0.001);
                assert!((y - 40.0).abs() < 0.001);
            }
            _ => panic!("Expected LineTo"),
        }

        match &commands[2] {
            PlotCommand::Circle { x, y, radius } => {
                assert!((x - 50.0).abs() < 0.001);
                assert!((y - 50.0).abs() < 0.001);
                assert!((radius - 10.0).abs() < 0.001);
            }
            _ => panic!("Expected Circle"),
        }
    }

    #[test]
    fn bounds_calculation() {
        let words = make_simple_plot();
        let (min_x, max_x, min_y, max_y) = PlotDecoder::bounds(&words);

        // MOVETO 10,20 -> LINETO 30,40 -> CIRCLE center 50,50 radius 10
        // Expected bounds: x=[10, 60], y=[20, 60] (circle extends to 60)
        assert!(min_x < 11.0); // ~10 with margin
        assert!(max_x > 59.0); // ~60 with margin
        assert!(min_y < 21.0); // ~20 with margin
        assert!(max_y > 59.0); // ~60 with margin
    }
}
