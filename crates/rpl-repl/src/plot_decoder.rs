//! Plot bytecode decoder for rendering.
//!
//! Decodes plot objects into drawable commands for the TUI viewer.
//! Uses the stdlib's PlotRenderer trait for decoding.

use rpl_core::Word;
use rpl_stdlib::plot::{
    encoding::words_to_bytes,
    render::{render_plot, PlotRenderer},
    unpack_rgba,
};

/// A decoded plot command ready for rendering.
#[derive(Debug, Clone)]
pub enum PlotCommand {
    MoveTo { x: f64, y: f64 },
    LineTo { x: f64, y: f64 },
    Circle { x: f64, y: f64, radius: f64 },
    Rect { x: f64, y: f64, width: f64, height: f64 },
    Ellipse { x: f64, y: f64, rx: f64, ry: f64 },
    Arc { x: f64, y: f64, radius: f64, start: f64, end: f64 },
    Bezier { x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64 },
    Pixel { x: f64, y: f64 },
    Color { r: u8, g: u8, b: u8, a: u8 },
    FillColor { r: u8, g: u8, b: u8, a: u8 },
    LineWidth { width: f64 },
    Text { x: f64, y: f64, text: String },
    Font { size: f64, name: String },
    Fill,
    Stroke,
    PushState,
    PopState,
    Identity,
    Transform { a: f64, b: f64, c: f64, d: f64, e: f64, f: f64 },
    Translate { dx: f64, dy: f64 },
    Scale { sx: f64, sy: f64 },
    Rotate { angle: f64 },
    Clip,
}

/// Collector that implements PlotRenderer to gather commands into a Vec.
struct CommandCollector {
    commands: Vec<PlotCommand>,
}

impl CommandCollector {
    fn new() -> Self {
        Self { commands: Vec::new() }
    }
}

impl PlotRenderer for CommandCollector {
    fn move_to(&mut self, x: f64, y: f64) {
        self.commands.push(PlotCommand::MoveTo { x, y });
    }

    fn line_to(&mut self, x: f64, y: f64) {
        self.commands.push(PlotCommand::LineTo { x, y });
    }

    fn circle(&mut self, cx: f64, cy: f64, radius: f64) {
        self.commands.push(PlotCommand::Circle { x: cx, y: cy, radius });
    }

    fn rect(&mut self, x: f64, y: f64, width: f64, height: f64) {
        self.commands.push(PlotCommand::Rect { x, y, width, height });
    }

    fn ellipse(&mut self, cx: f64, cy: f64, rx: f64, ry: f64) {
        self.commands.push(PlotCommand::Ellipse { x: cx, y: cy, rx, ry });
    }

    fn arc(&mut self, cx: f64, cy: f64, radius: f64, start_angle: f64, end_angle: f64) {
        self.commands.push(PlotCommand::Arc {
            x: cx,
            y: cy,
            radius,
            start: start_angle,
            end: end_angle,
        });
    }

    fn bezier(&mut self, x1: f64, y1: f64, x2: f64, y2: f64, x3: f64, y3: f64) {
        self.commands.push(PlotCommand::Bezier { x1, y1, x2, y2, x3, y3 });
    }

    fn pixel(&mut self, x: f64, y: f64) {
        self.commands.push(PlotCommand::Pixel { x, y });
    }

    fn text(&mut self, x: f64, y: f64, text: &str) {
        self.commands.push(PlotCommand::Text { x, y, text: text.to_string() });
    }

    fn stroke(&mut self) {
        self.commands.push(PlotCommand::Stroke);
    }

    fn fill(&mut self) {
        self.commands.push(PlotCommand::Fill);
    }

    fn set_line_width(&mut self, width: f64) {
        self.commands.push(PlotCommand::LineWidth { width });
    }

    fn set_stroke_color(&mut self, rgba: u32) {
        let (r, g, b, a) = unpack_rgba(rgba);
        self.commands.push(PlotCommand::Color { r, g, b, a });
    }

    fn set_fill_color(&mut self, rgba: u32) {
        let (r, g, b, a) = unpack_rgba(rgba);
        self.commands.push(PlotCommand::FillColor { r, g, b, a });
    }

    fn set_font(&mut self, size: f64, name: &str) {
        self.commands.push(PlotCommand::Font { size, name: name.to_string() });
    }

    fn identity(&mut self) {
        self.commands.push(PlotCommand::Identity);
    }

    fn transform(&mut self, a: f64, b: f64, c: f64, d: f64, e: f64, f: f64) {
        self.commands.push(PlotCommand::Transform { a, b, c, d, e, f });
    }

    fn scale(&mut self, sx: f64, sy: f64) {
        self.commands.push(PlotCommand::Scale { sx, sy });
    }

    fn rotate(&mut self, angle: f64) {
        self.commands.push(PlotCommand::Rotate { angle });
    }

    fn translate(&mut self, dx: f64, dy: f64) {
        self.commands.push(PlotCommand::Translate { dx, dy });
    }

    fn push_state(&mut self) {
        self.commands.push(PlotCommand::PushState);
    }

    fn pop_state(&mut self) {
        self.commands.push(PlotCommand::PopState);
    }

    fn clip(&mut self) {
        self.commands.push(PlotCommand::Clip);
    }
}

/// Decode plot bytecode into a list of commands.
pub fn decode_plot(words: &[Word]) -> Vec<PlotCommand> {
    let bytes = words_to_bytes(words);
    let mut collector = CommandCollector::new();
    render_plot(&bytes, &mut collector);
    collector.commands
}

/// Calculate the bounding box of all drawable elements.
/// Returns (min_x, max_x, min_y, max_y).
pub fn plot_bounds(words: &[Word]) -> (f64, f64, f64, f64) {
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

    for cmd in decode_plot(words) {
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
            PlotCommand::Rect { x, y, width, height } => {
                update_point(x, y);
                update_point(x + width, y + height);
            }
            PlotCommand::Ellipse { x, y, rx, ry } => {
                update_point(x - rx, y - ry);
                update_point(x + rx, y + ry);
            }
            PlotCommand::Arc { x, y, radius, .. } => {
                update_point(x - radius, y - radius);
                update_point(x + radius, y + radius);
            }
            PlotCommand::Bezier { x1, y1, x2, y2, x3, y3 } => {
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

    if min_x > max_x || min_y > max_y {
        (0.0, 100.0, 0.0, 100.0)
    } else {
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

/// Decompile plot bytecode back to RPL source code.
pub fn decompile_plot(words: &[Word]) -> String {
    let mut lines = vec!["BEGINPLOT".to_string()];

    for cmd in decode_plot(words) {
        let line = match cmd {
            PlotCommand::MoveTo { x, y } => format!("  {} {} MOVETO", fmt_num(x), fmt_num(y)),
            PlotCommand::LineTo { x, y } => format!("  {} {} LINETO", fmt_num(x), fmt_num(y)),
            PlotCommand::Circle { x, y, radius } => {
                format!("  {} {} {} CIRCLE", fmt_num(x), fmt_num(y), fmt_num(radius))
            }
            PlotCommand::Rect { x, y, width, height } => {
                format!("  {} {} {} {} RECT", fmt_num(x), fmt_num(y), fmt_num(width), fmt_num(height))
            }
            PlotCommand::Ellipse { x, y, rx, ry } => {
                format!("  {} {} {} {} ELLIPSE", fmt_num(x), fmt_num(y), fmt_num(rx), fmt_num(ry))
            }
            PlotCommand::Arc { x, y, radius, start, end } => {
                format!("  {} {} {} {} {} ARC", fmt_num(x), fmt_num(y), fmt_num(radius), fmt_num(start), fmt_num(end))
            }
            PlotCommand::Bezier { x1, y1, x2, y2, x3, y3 } => {
                format!("  {} {} {} {} {} {} BEZIER", fmt_num(x1), fmt_num(y1), fmt_num(x2), fmt_num(y2), fmt_num(x3), fmt_num(y3))
            }
            PlotCommand::Pixel { x, y } => format!("  {} {} PIXEL", fmt_num(x), fmt_num(y)),
            PlotCommand::Color { r, g, b, a } => format!("  {} {} {} {} COLOR", r, g, b, a),
            PlotCommand::FillColor { r, g, b, a } => format!("  {} {} {} {} FILLCOLOR", r, g, b, a),
            PlotCommand::LineWidth { width } => format!("  {} LINEWIDTH", fmt_num(width)),
            PlotCommand::Text { x, y, ref text } => format!("  {} {} \"{}\" TEXT", fmt_num(x), fmt_num(y), text),
            PlotCommand::Font { size, ref name } => format!("  {} \"{}\" FONT", fmt_num(size), name),
            PlotCommand::Fill => "  FILL".to_string(),
            PlotCommand::Stroke => "  STROKE".to_string(),
            PlotCommand::PushState => "  PUSHSTATE".to_string(),
            PlotCommand::PopState => "  POPSTATE".to_string(),
            PlotCommand::Identity => "  IDENTITY".to_string(),
            PlotCommand::Transform { a, b, c, d, e, f } => {
                format!("  {} {} {} {} {} {} TRANSFORM", fmt_num(a), fmt_num(b), fmt_num(c), fmt_num(d), fmt_num(e), fmt_num(f))
            }
            PlotCommand::Translate { dx, dy } => format!("  {} {} TRANSLATE", fmt_num(dx), fmt_num(dy)),
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
    use rpl_stdlib::plot::{
        commands::CMD_END,
        encoding::{bytes_to_words, encode_number, to_fixed_point},
    };

    use super::*;

    fn make_simple_plot() -> Vec<Word> {
        use rpl_stdlib::plot::commands::{CMD_CIRCLE, CMD_LINETO, CMD_MOVETO};

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
    fn test_decode_simple_plot() {
        let words = make_simple_plot();
        let commands = decode_plot(&words);

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
    fn test_bounds_calculation() {
        let words = make_simple_plot();
        let (min_x, max_x, min_y, max_y) = plot_bounds(&words);

        // MOVETO 10,20 -> LINETO 30,40 -> CIRCLE center 50,50 radius 10
        // Expected bounds: x=[10, 60], y=[20, 60] (circle extends to 60)
        assert!(min_x < 11.0); // ~10 with margin
        assert!(max_x > 59.0); // ~60 with margin
        assert!(min_y < 21.0); // ~20 with margin
        assert!(max_y > 59.0); // ~60 with margin
    }
}
