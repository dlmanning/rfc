//! Plot bytecode decoder for rendering.
//!
//! Decodes plot objects into drawable commands for the TUI viewer.
//! Uses rpl-vector-plot's Renderer trait for decoding.

use rpl_vector_plot::{decode, render, Color, Paint, Point, Renderer, Stroke};

/// A decoded plot command ready for rendering.
#[derive(Debug, Clone)]
pub enum PlotCommand {
    MoveTo { x: f64, y: f64 },
    LineTo { x: f64, y: f64 },
    QuadTo { cx: f64, cy: f64, x: f64, y: f64 },
    CubicTo { c1x: f64, c1y: f64, c2x: f64, c2y: f64, x: f64, y: f64 },
    Arc { cx: f64, cy: f64, radius: f64, start: f64, sweep: f64 },
    ClosePath,
    Fill { r: u8, g: u8, b: u8, a: u8 },
    Stroke { r: u8, g: u8, b: u8, a: u8, width: f64 },
    Text { x: f64, y: f64, text: String, size: f64 },
    PushTransform { a: f64, b: f64, c: f64, d: f64, e: f64, f: f64 },
    PopTransform,
}

/// Collector that implements Renderer to gather commands into a Vec.
struct CommandCollector {
    commands: Vec<PlotCommand>,
}

impl CommandCollector {
    fn new() -> Self {
        Self { commands: Vec::new() }
    }
}

impl Renderer for CommandCollector {
    fn move_to(&mut self, p: Point) {
        self.commands.push(PlotCommand::MoveTo { x: p.x as f64, y: p.y as f64 });
    }

    fn line_to(&mut self, p: Point) {
        self.commands.push(PlotCommand::LineTo { x: p.x as f64, y: p.y as f64 });
    }

    fn quad_to(&mut self, ctrl: Point, end: Point) {
        self.commands.push(PlotCommand::QuadTo {
            cx: ctrl.x as f64,
            cy: ctrl.y as f64,
            x: end.x as f64,
            y: end.y as f64,
        });
    }

    fn cubic_to(&mut self, c1: Point, c2: Point, end: Point) {
        self.commands.push(PlotCommand::CubicTo {
            c1x: c1.x as f64,
            c1y: c1.y as f64,
            c2x: c2.x as f64,
            c2y: c2.y as f64,
            x: end.x as f64,
            y: end.y as f64,
        });
    }

    fn arc(&mut self, center: Point, radius: f32, start_angle: f32, sweep_angle: f32) {
        self.commands.push(PlotCommand::Arc {
            cx: center.x as f64,
            cy: center.y as f64,
            radius: radius as f64,
            start: start_angle as f64,
            sweep: sweep_angle as f64,
        });
    }

    fn close_path(&mut self) {
        self.commands.push(PlotCommand::ClosePath);
    }

    fn fill(&mut self, paint: &Paint) {
        let color = match paint {
            Paint::Solid(c) => *c,
            Paint::LinearGradient { stops, .. } | Paint::RadialGradient { stops, .. } => {
                // Use first color of gradient as fallback
                stops.first().map(|s| s.color).unwrap_or(Color::BLACK)
            }
        };
        self.commands.push(PlotCommand::Fill {
            r: color.r,
            g: color.g,
            b: color.b,
            a: color.a,
        });
    }

    fn stroke(&mut self, stroke: &Stroke) {
        self.commands.push(PlotCommand::Stroke {
            r: stroke.color.r,
            g: stroke.color.g,
            b: stroke.color.b,
            a: stroke.color.a,
            width: stroke.width as f64,
        });
    }

    fn push_transform(&mut self, t: &rpl_vector_plot::Transform) {
        let [a, b, c, d, e, f] = *t.as_array();
        self.commands.push(PlotCommand::PushTransform {
            a: a as f64,
            b: b as f64,
            c: c as f64,
            d: d as f64,
            e: e as f64,
            f: f as f64,
        });
    }

    fn pop_transform(&mut self) {
        self.commands.push(PlotCommand::PopTransform);
    }

    fn text(&mut self, pos: Point, text: &str, size: f32) {
        self.commands.push(PlotCommand::Text {
            x: pos.x as f64,
            y: pos.y as f64,
            text: text.to_string(),
            size: size as f64,
        });
    }

    fn clear_path(&mut self) {
        // No-op for command collection - we don't track path state
    }
}

/// Decode plot bytecode into a list of commands.
pub fn decode_plot(bytes: &[u8]) -> Vec<PlotCommand> {
    match decode(bytes) {
        Ok(plot) => {
            let mut collector = CommandCollector::new();
            render(&plot, &mut collector);
            collector.commands
        }
        Err(_) => Vec::new(),
    }
}

/// Calculate the bounding box of all drawable elements.
/// Returns (min_x, max_x, min_y, max_y).
pub fn plot_bounds(bytes: &[u8]) -> (f64, f64, f64, f64) {
    match decode(bytes) {
        Ok(plot) => {
            let b = plot.bounds();
            if b.is_empty() {
                (0.0, 100.0, 0.0, 100.0)
            } else {
                let margin_x = b.w as f64 * 0.05;
                let margin_y = b.h as f64 * 0.05;
                (
                    b.x as f64 - margin_x,
                    (b.x + b.w) as f64 + margin_x,
                    b.y as f64 - margin_y,
                    (b.y + b.h) as f64 + margin_y,
                )
            }
        }
        Err(_) => (0.0, 100.0, 0.0, 100.0),
    }
}

/// Decompile plot bytecode back to RPL source code.
pub fn decompile_plot(bytes: &[u8]) -> String {
    let mut lines = vec!["NEWPLOT".to_string()];

    for cmd in decode_plot(bytes) {
        let line = match cmd {
            PlotCommand::MoveTo { x, y } => format!("  {} {} MOVETO", fmt_num(x), fmt_num(y)),
            PlotCommand::LineTo { x, y } => format!("  {} {} LINETO", fmt_num(x), fmt_num(y)),
            PlotCommand::QuadTo { cx, cy, x, y } => {
                format!("  {} {} {} {} QUADTO", fmt_num(cx), fmt_num(cy), fmt_num(x), fmt_num(y))
            }
            PlotCommand::CubicTo { c1x, c1y, c2x, c2y, x, y } => {
                format!(
                    "  {} {} {} {} {} {} CUBICTO",
                    fmt_num(c1x), fmt_num(c1y), fmt_num(c2x), fmt_num(c2y), fmt_num(x), fmt_num(y)
                )
            }
            PlotCommand::Arc { cx, cy, radius, start, sweep } => {
                format!(
                    "  {} {} {} {} {} ARC",
                    fmt_num(cx), fmt_num(cy), fmt_num(radius), fmt_num(start), fmt_num(sweep)
                )
            }
            PlotCommand::ClosePath => "  CLOSEPATH".to_string(),
            PlotCommand::Fill { r, g, b, a } => {
                format!("  {} {} {} {} RGBA FILLCOLOR FILL", r, g, b, a)
            }
            PlotCommand::Stroke { r, g, b, a, width } => {
                format!("  {} {} {} {} RGBA {} STROKECOLOR STROKE", r, g, b, a, fmt_num(width))
            }
            PlotCommand::Text { x, y, text, size } => {
                format!("  {} {} \"{}\" {} TEXT", fmt_num(x), fmt_num(y), text, fmt_num(size))
            }
            PlotCommand::PushTransform { a, b, c, d, e, f } => {
                format!(
                    "  ; transform [{} {} {} {} {} {}]",
                    fmt_num(a), fmt_num(b), fmt_num(c), fmt_num(d), fmt_num(e), fmt_num(f)
                )
            }
            PlotCommand::PopTransform => "  ; pop transform".to_string(),
        };
        lines.push(line);
    }

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
    use super::*;
    use rpl_vector_plot::{encode, Plot};

    fn make_simple_plot() -> Vec<u8> {
        let mut plot = Plot::new();
        plot.set_fill_color(Color::RED);
        plot.add_circle(50.0, 50.0, 10.0);
        encode(&plot)
    }

    #[test]
    fn test_decode_simple_plot() {
        let bytes = make_simple_plot();
        let commands = decode_plot(&bytes);

        // Should have path commands for the circle (arc) + fill
        assert!(!commands.is_empty());
    }

    #[test]
    fn test_bounds_calculation() {
        let bytes = make_simple_plot();
        let (min_x, max_x, min_y, max_y) = plot_bounds(&bytes);

        // Circle at (50, 50) with radius 10: bounds should be ~[40, 60] x [40, 60]
        assert!(min_x < 41.0);
        assert!(max_x > 59.0);
        assert!(min_y < 41.0);
        assert!(max_y > 59.0);
    }
}
