//! Plot viewer overlay for displaying plot objects.

use ratatui::prelude::*;
use ratatui::symbols::Marker;
use ratatui::widgets::canvas::{Canvas, Circle, Line as CanvasLine};
use ratatui::widgets::{Block, Borders, Paragraph};

use crate::plot_decoder::{decode_plot, plot_bounds, PlotCommand};

/// 2D affine transform matrix (row-major: [[a, b, tx], [c, d, ty]]).
#[derive(Clone, Copy, Debug)]
struct Transform {
    a: f64,  // scale x / cos(theta)
    b: f64,  // -sin(theta) / shear x
    c: f64,  // sin(theta) / shear y
    d: f64,  // scale y / cos(theta)
    tx: f64, // translate x
    ty: f64, // translate y
}

impl Default for Transform {
    fn default() -> Self {
        Self::identity()
    }
}

impl Transform {
    /// Identity transform (no change).
    fn identity() -> Self {
        Self {
            a: 1.0,
            b: 0.0,
            c: 0.0,
            d: 1.0,
            tx: 0.0,
            ty: 0.0,
        }
    }

    /// Create from 6 components [a, b, c, d, e, f].
    fn from_components(a: f64, b: f64, c: f64, d: f64, e: f64, f: f64) -> Self {
        Self {
            a,
            b,
            c,
            d,
            tx: e,
            ty: f,
        }
    }

    /// Apply this transform to a point.
    fn apply(&self, x: f64, y: f64) -> (f64, f64) {
        (
            self.a * x + self.b * y + self.tx,
            self.c * x + self.d * y + self.ty,
        )
    }

    /// Get the average scale factor (for scaling radii).
    fn average_scale(&self) -> f64 {
        let sx = (self.a * self.a + self.c * self.c).sqrt();
        let sy = (self.b * self.b + self.d * self.d).sqrt();
        (sx + sy) / 2.0
    }
}

/// Graphics state for push/pop.
#[derive(Clone, Debug)]
struct GraphicsState {
    transform: Transform,
    stroke_color: Color,
    fill_color: Color,
}

/// Plot viewer state.
#[derive(Clone, Debug)]
pub struct PlotViewState {
    /// Raw plot data (bytes).
    plot_data: Vec<u8>,
    /// Computed bounds (min_x, max_x, min_y, max_y).
    bounds: (f64, f64, f64, f64),
    /// Pan offset (dx, dy).
    pan: (f64, f64),
    /// Zoom level (1.0 = fit to view).
    zoom: f64,
}

impl PlotViewState {
    /// Create a new plot view state from plot data.
    pub fn new(plot_data: Vec<u8>) -> Self {
        let bounds = plot_bounds(&plot_data);
        Self {
            plot_data,
            bounds,
            pan: (0.0, 0.0),
            zoom: 1.0,
        }
    }

    /// Get the view bounds adjusted for pan and zoom.
    pub fn view_bounds(&self) -> (f64, f64, f64, f64) {
        let (min_x, max_x, min_y, max_y) = self.bounds;
        let width = max_x - min_x;
        let height = max_y - min_y;
        let center_x = (min_x + max_x) / 2.0 + self.pan.0;
        let center_y = (min_y + max_y) / 2.0 + self.pan.1;

        let view_width = width / self.zoom;
        let view_height = height / self.zoom;

        (
            center_x - view_width / 2.0,
            center_x + view_width / 2.0,
            center_y - view_height / 2.0,
            center_y + view_height / 2.0,
        )
    }

    /// Pan step size (10% of view).
    fn pan_step(&self) -> f64 {
        let (min_x, max_x, _, _) = self.bounds;
        (max_x - min_x) * 0.1 / self.zoom
    }

    /// Pan left.
    pub fn pan_left(&mut self) {
        self.pan.0 -= self.pan_step();
    }

    /// Pan right.
    pub fn pan_right(&mut self) {
        self.pan.0 += self.pan_step();
    }

    /// Pan up.
    pub fn pan_up(&mut self) {
        self.pan.1 += self.pan_step();
    }

    /// Pan down.
    pub fn pan_down(&mut self) {
        self.pan.1 -= self.pan_step();
    }

    /// Zoom in.
    pub fn zoom_in(&mut self) {
        self.zoom = (self.zoom * 1.2).min(10.0);
    }

    /// Zoom out.
    pub fn zoom_out(&mut self) {
        self.zoom = (self.zoom / 1.2).max(0.1);
    }

    /// Reset view to fit bounds.
    pub fn reset_view(&mut self) {
        self.pan = (0.0, 0.0);
        self.zoom = 1.0;
    }

    /// Get the plot data for iteration.
    pub fn plot_data(&self) -> &[u8] {
        &self.plot_data
    }
}

/// Render the plot viewer in the given area.
pub fn render_plot_view(frame: &mut Frame, state: &PlotViewState, area: Rect) {
    // Create the outer block
    let block = Block::default()
        .title(" Plot ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Cyan));

    let inner = block.inner(area);
    frame.render_widget(block, area);

    // Reserve space for help text at bottom (1 line)
    let canvas_area = Rect::new(inner.x, inner.y, inner.width, inner.height.saturating_sub(1));
    let help_area = Rect::new(
        inner.x,
        inner.y + inner.height.saturating_sub(1),
        inner.width,
        1,
    );

    // Get view bounds
    let (view_min_x, view_max_x, view_min_y, view_max_y) = state.view_bounds();

    // Create and render the canvas
    let canvas = Canvas::default()
        .marker(Marker::Braille)
        .x_bounds([view_min_x, view_max_x])
        .y_bounds([view_min_y, view_max_y])
        .paint(|ctx| {
            let commands = decode_plot(state.plot_data());

            // Graphics state
            let mut transform = Transform::identity();
            let mut stroke_color = Color::White;
            let mut fill_color = Color::White;
            let mut current_pos = (0.0, 0.0);
            let mut path_start = (0.0, 0.0);
            let mut state_stack: Vec<GraphicsState> = Vec::new();

            // Path buffer for fill operations
            let mut path: Vec<(f64, f64)> = Vec::new();

            for cmd in commands {
                match cmd {
                    PlotCommand::MoveTo { x, y } => {
                        let (tx, ty) = transform.apply(x, y);
                        current_pos = (tx, ty);
                        path_start = (tx, ty);
                        path.clear();
                        path.push((tx, ty));
                    }
                    PlotCommand::LineTo { x, y } => {
                        let (tx, ty) = transform.apply(x, y);
                        ctx.draw(&CanvasLine {
                            x1: current_pos.0,
                            y1: current_pos.1,
                            x2: tx,
                            y2: ty,
                            color: stroke_color,
                        });
                        current_pos = (tx, ty);
                        path.push((tx, ty));
                    }
                    PlotCommand::QuadTo { cx, cy, x, y } => {
                        // Approximate quadratic bezier with line segments
                        let (tcx, tcy) = transform.apply(cx, cy);
                        let (tx, ty) = transform.apply(x, y);
                        let steps = 8;
                        for i in 1..=steps {
                            let t = i as f64 / steps as f64;
                            let u = 1.0 - t;
                            let px = u * u * current_pos.0 + 2.0 * u * t * tcx + t * t * tx;
                            let py = u * u * current_pos.1 + 2.0 * u * t * tcy + t * t * ty;
                            ctx.draw(&CanvasLine {
                                x1: current_pos.0,
                                y1: current_pos.1,
                                x2: px,
                                y2: py,
                                color: stroke_color,
                            });
                            current_pos = (px, py);
                            path.push((px, py));
                        }
                    }
                    PlotCommand::CubicTo { c1x, c1y, c2x, c2y, x, y } => {
                        // Approximate cubic bezier with line segments
                        let (tc1x, tc1y) = transform.apply(c1x, c1y);
                        let (tc2x, tc2y) = transform.apply(c2x, c2y);
                        let (tx, ty) = transform.apply(x, y);
                        let steps = 12;
                        for i in 1..=steps {
                            let t = i as f64 / steps as f64;
                            let u = 1.0 - t;
                            let px = u * u * u * current_pos.0
                                + 3.0 * u * u * t * tc1x
                                + 3.0 * u * t * t * tc2x
                                + t * t * t * tx;
                            let py = u * u * u * current_pos.1
                                + 3.0 * u * u * t * tc1y
                                + 3.0 * u * t * t * tc2y
                                + t * t * t * ty;
                            ctx.draw(&CanvasLine {
                                x1: current_pos.0,
                                y1: current_pos.1,
                                x2: px,
                                y2: py,
                                color: stroke_color,
                            });
                            current_pos = (px, py);
                            path.push((px, py));
                        }
                    }
                    PlotCommand::Arc { cx, cy, radius, start, sweep } => {
                        let (tcx, tcy) = transform.apply(cx, cy);
                        let scaled_radius = radius * transform.average_scale();

                        // For small sweeps, draw arc; for full circles, draw circle
                        if sweep.abs() >= std::f64::consts::PI * 1.9 {
                            ctx.draw(&Circle {
                                x: tcx,
                                y: tcy,
                                radius: scaled_radius,
                                color: stroke_color,
                            });
                        } else {
                            // Draw arc with line segments
                            let steps = ((sweep.abs() * 8.0) as i32).max(4);
                            let mut prev_x = tcx + scaled_radius * start.cos();
                            let mut prev_y = tcy + scaled_radius * start.sin();
                            for i in 1..=steps {
                                let angle = start + sweep * (i as f64 / steps as f64);
                                let ax = tcx + scaled_radius * angle.cos();
                                let ay = tcy + scaled_radius * angle.sin();
                                ctx.draw(&CanvasLine {
                                    x1: prev_x,
                                    y1: prev_y,
                                    x2: ax,
                                    y2: ay,
                                    color: stroke_color,
                                });
                                prev_x = ax;
                                prev_y = ay;
                            }
                        }

                        // Add circle approximation to path for fill
                        path.clear();
                        let segments = 32;
                        for i in 0..=segments {
                            let angle = start + sweep * (i as f64 / segments as f64);
                            let px = tcx + scaled_radius * angle.cos();
                            let py = tcy + scaled_radius * angle.sin();
                            path.push((px, py));
                        }
                    }
                    PlotCommand::ClosePath => {
                        // Draw line back to start
                        if current_pos != path_start {
                            ctx.draw(&CanvasLine {
                                x1: current_pos.0,
                                y1: current_pos.1,
                                x2: path_start.0,
                                y2: path_start.1,
                                color: stroke_color,
                            });
                        }
                        current_pos = path_start;
                        path.push(path_start);
                    }
                    PlotCommand::Fill { r, g, b, .. } => {
                        fill_color = Color::Rgb(r, g, b);
                        // Fill the current path
                        if path.len() >= 3 {
                            fill_polygon(ctx, &path, fill_color);
                        }
                        path.clear();
                    }
                    PlotCommand::Stroke { r, g, b, .. } => {
                        stroke_color = Color::Rgb(r, g, b);
                        // Stroke the current path (already drawn via LineTo etc.)
                        path.clear();
                    }
                    PlotCommand::Text { x, y, ref text, .. } => {
                        let (tx, ty) = transform.apply(x, y);
                        ctx.print(tx, ty, text.clone().fg(stroke_color));
                    }
                    PlotCommand::PushTransform { a, b, c, d, e, f } => {
                        state_stack.push(GraphicsState {
                            transform,
                            stroke_color,
                            fill_color,
                        });
                        transform = Transform::from_components(a, b, c, d, e, f);
                    }
                    PlotCommand::PopTransform => {
                        if let Some(state) = state_stack.pop() {
                            transform = state.transform;
                            stroke_color = state.stroke_color;
                            fill_color = state.fill_color;
                        }
                    }
                }
            }
        });

    frame.render_widget(canvas, canvas_area);

    // Render help text
    let help = Paragraph::new(Line::from(vec![
        Span::styled("Arrows", Style::default().fg(Color::Yellow)),
        Span::raw(":pan  "),
        Span::styled("+/-", Style::default().fg(Color::Yellow)),
        Span::raw(":zoom  "),
        Span::styled("r", Style::default().fg(Color::Yellow)),
        Span::raw(":reset  "),
        Span::styled("ESC", Style::default().fg(Color::Yellow)),
        Span::raw(":close"),
    ]))
    .alignment(Alignment::Center)
    .style(Style::default().fg(Color::DarkGray));

    frame.render_widget(help, help_area);
}

/// Fill a polygon using scanline algorithm (simplified).
fn fill_polygon(ctx: &mut ratatui::widgets::canvas::Context, points: &[(f64, f64)], color: Color) {
    if points.len() < 3 {
        return;
    }

    // Find y bounds
    let min_y = points.iter().map(|p| p.1).fold(f64::INFINITY, f64::min);
    let max_y = points.iter().map(|p| p.1).fold(f64::NEG_INFINITY, f64::max);

    // Scanline fill with limited resolution for TUI
    let steps = 50.min((max_y - min_y).abs() as i32);
    if steps <= 0 {
        return;
    }

    for i in 0..=steps {
        let y = min_y + (max_y - min_y) * (i as f64 / steps as f64);

        // Find intersections with polygon edges
        let mut intersections = Vec::new();
        for j in 0..points.len() {
            let (x1, y1) = points[j];
            let (x2, y2) = points[(j + 1) % points.len()];

            if (y1 <= y && y < y2) || (y2 <= y && y < y1) {
                let x = x1 + (y - y1) * (x2 - x1) / (y2 - y1);
                intersections.push(x);
            }
        }

        intersections.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        // Draw horizontal lines between pairs
        for pair in intersections.chunks(2) {
            if pair.len() == 2 {
                ctx.draw(&CanvasLine {
                    x1: pair[0],
                    y1: y,
                    x2: pair[1],
                    y2: y,
                    color,
                });
            }
        }
    }
}
