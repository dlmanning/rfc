//! Plot viewer overlay for displaying plot objects.

use ratatui::prelude::*;
use ratatui::symbols::Marker;
use ratatui::widgets::canvas::{Canvas, Circle, Line as CanvasLine};
use ratatui::widgets::{Block, Borders, Paragraph};

use rpl_core::Word;

use crate::plot_decoder::{PlotCommand, PlotDecoder};

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

    /// Apply this transform to a point.
    fn apply(&self, x: f64, y: f64) -> (f64, f64) {
        (
            self.a * x + self.b * y + self.tx,
            self.c * x + self.d * y + self.ty,
        )
    }

    /// Apply this transform to a vector (no translation).
    fn apply_vector(&self, dx: f64, dy: f64) -> (f64, f64) {
        (self.a * dx + self.b * dy, self.c * dx + self.d * dy)
    }

    /// Concatenate with another transform (self * other).
    fn concat(&self, other: &Transform) -> Transform {
        Transform {
            a: self.a * other.a + self.b * other.c,
            b: self.a * other.b + self.b * other.d,
            c: self.c * other.a + self.d * other.c,
            d: self.c * other.b + self.d * other.d,
            tx: self.a * other.tx + self.b * other.ty + self.tx,
            ty: self.c * other.tx + self.d * other.ty + self.ty,
        }
    }

    /// Create a translation transform.
    fn translate(dx: f64, dy: f64) -> Self {
        Self {
            a: 1.0,
            b: 0.0,
            c: 0.0,
            d: 1.0,
            tx: dx,
            ty: dy,
        }
    }

    /// Create a scale transform.
    fn scale(sx: f64, sy: f64) -> Self {
        Self {
            a: sx,
            b: 0.0,
            c: 0.0,
            d: sy,
            tx: 0.0,
            ty: 0.0,
        }
    }

    /// Create a rotation transform (angle in radians).
    fn rotate(angle: f64) -> Self {
        let cos = angle.cos();
        let sin = angle.sin();
        Self {
            a: cos,
            b: -sin,
            c: sin,
            d: cos,
            tx: 0.0,
            ty: 0.0,
        }
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
    /// Raw plot data (words).
    plot_data: Vec<Word>,
    /// Computed bounds (min_x, max_x, min_y, max_y).
    bounds: (f64, f64, f64, f64),
    /// Pan offset (dx, dy).
    pan: (f64, f64),
    /// Zoom level (1.0 = fit to view).
    zoom: f64,
}

impl PlotViewState {
    /// Create a new plot view state from plot data.
    pub fn new(plot_data: Vec<Word>) -> Self {
        let bounds = PlotDecoder::bounds(&plot_data);
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
    pub fn plot_data(&self) -> &[Word] {
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
            let decoder = PlotDecoder::new(state.plot_data());

            // Graphics state
            let mut transform = Transform::identity();
            let mut stroke_color = Color::White;
            let mut fill_color = Color::White;
            let mut current_pos = (0.0, 0.0);
            let mut path_start = (0.0, 0.0);
            let mut state_stack: Vec<GraphicsState> = Vec::new();

            // Path buffer for fill operations
            let mut path: Vec<(f64, f64)> = Vec::new();

            for cmd in decoder {
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
                    PlotCommand::Circle { x, y, radius } => {
                        let (cx, cy) = transform.apply(x, y);
                        let scaled_radius = radius * transform.average_scale();
                        ctx.draw(&Circle {
                            x: cx,
                            y: cy,
                            radius: scaled_radius,
                            color: stroke_color,
                        });
                        // Add circle approximation to path for FILL
                        path.clear();
                        let segments = 32;
                        for i in 0..=segments {
                            let angle = 2.0 * std::f64::consts::PI * (i as f64) / (segments as f64);
                            let px = cx + scaled_radius * angle.cos();
                            let py = cy + scaled_radius * angle.sin();
                            path.push((px, py));
                        }
                    }
                    PlotCommand::Rect { x, y, width, height } => {
                        // Transform all four corners for proper rotated rectangles
                        let corners = [
                            transform.apply(x, y),
                            transform.apply(x + width, y),
                            transform.apply(x + width, y + height),
                            transform.apply(x, y + height),
                        ];

                        // Draw as four lines (handles rotation/skew)
                        for i in 0..4 {
                            let (ax, ay) = corners[i];
                            let (bx, by) = corners[(i + 1) % 4];
                            ctx.draw(&CanvasLine {
                                x1: ax, y1: ay,
                                x2: bx, y2: by,
                                color: stroke_color,
                            });
                        }
                        // Add corners to path for FILL
                        path.clear();
                        for corner in corners {
                            path.push(corner);
                        }
                        path.push(corners[0]); // Close the path
                    }
                    PlotCommand::Ellipse { x, y, rx, ry } => {
                        let (cx, cy) = transform.apply(x, y);
                        // Draw proper ellipse with line segments
                        draw_ellipse(ctx, cx, cy, rx, ry, &transform, stroke_color);
                    }
                    PlotCommand::Arc { x, y, radius, start, end } => {
                        let (cx, cy) = transform.apply(x, y);
                        let scaled_radius = radius * transform.average_scale();
                        draw_arc(ctx, cx, cy, scaled_radius, start, end, stroke_color);
                    }
                    PlotCommand::Bezier { x1, y1, x2, y2, x3, y3 } => {
                        let tp0 = current_pos;
                        let tp1 = transform.apply(x1, y1);
                        let tp2 = transform.apply(x2, y2);
                        let tp3 = transform.apply(x3, y3);
                        draw_bezier(ctx, tp0, tp1, tp2, tp3, stroke_color);
                        current_pos = tp3;
                        path.push(tp3);
                    }
                    PlotCommand::Pixel { x, y } => {
                        let (px, py) = transform.apply(x, y);
                        let pixel_size = (view_max_x - view_min_x) / (canvas_area.width as f64 * 2.0);
                        ctx.draw(&Circle {
                            x: px,
                            y: py,
                            radius: pixel_size,
                            color: stroke_color,
                        });
                    }
                    PlotCommand::Color { r, g, b, .. } => {
                        stroke_color = Color::Rgb(r, g, b);
                    }
                    PlotCommand::FillColor { r, g, b, .. } => {
                        fill_color = Color::Rgb(r, g, b);
                    }
                    PlotCommand::Text { x, y, text } => {
                        let (tx, ty) = transform.apply(x, y);
                        ctx.print(tx, ty, text.fg(stroke_color));
                    }
                    PlotCommand::Fill => {
                        // Fill the current path with the fill color
                        if path.len() >= 3 {
                            fill_polygon(ctx, &path, fill_color);
                        }
                    }
                    PlotCommand::Stroke => {
                        // Close and stroke the current path
                        if path.len() >= 2 && path_start != current_pos {
                            ctx.draw(&CanvasLine {
                                x1: current_pos.0,
                                y1: current_pos.1,
                                x2: path_start.0,
                                y2: path_start.1,
                                color: stroke_color,
                            });
                        }
                    }
                    PlotCommand::PushState => {
                        state_stack.push(GraphicsState {
                            transform,
                            stroke_color,
                            fill_color,
                        });
                    }
                    PlotCommand::PopState => {
                        if let Some(state) = state_stack.pop() {
                            transform = state.transform;
                            stroke_color = state.stroke_color;
                            fill_color = state.fill_color;
                        }
                    }
                    PlotCommand::Identity => {
                        transform = Transform::identity();
                    }
                    PlotCommand::Translate { dx, dy } => {
                        transform = transform.concat(&Transform::translate(dx, dy));
                    }
                    PlotCommand::Scale { sx, sy } => {
                        transform = transform.concat(&Transform::scale(sx, sy));
                    }
                    PlotCommand::Rotate { angle } => {
                        transform = transform.concat(&Transform::rotate(angle));
                    }
                    PlotCommand::LineWidth { .. } | PlotCommand::Clip => {
                        // Not supported in Braille rendering
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

/// Draw an arc using line segments.
fn draw_arc(
    ctx: &mut ratatui::widgets::canvas::Context<'_>,
    cx: f64,
    cy: f64,
    radius: f64,
    start: f64,
    end: f64,
    color: Color,
) {
    let segments = 20;
    let step = (end - start) / segments as f64;

    let mut prev_x = cx + radius * start.cos();
    let mut prev_y = cy + radius * start.sin();

    for i in 1..=segments {
        let angle = start + step * i as f64;
        let x = cx + radius * angle.cos();
        let y = cy + radius * angle.sin();

        ctx.draw(&CanvasLine {
            x1: prev_x,
            y1: prev_y,
            x2: x,
            y2: y,
            color,
        });

        prev_x = x;
        prev_y = y;
    }
}

/// Draw a cubic bezier curve using line segments.
fn draw_bezier(
    ctx: &mut ratatui::widgets::canvas::Context<'_>,
    p0: (f64, f64),
    p1: (f64, f64),
    p2: (f64, f64),
    p3: (f64, f64),
    color: Color,
) {
    let segments = 20;

    let mut prev = p0;

    for i in 1..=segments {
        let t = i as f64 / segments as f64;
        let t2 = t * t;
        let t3 = t2 * t;
        let mt = 1.0 - t;
        let mt2 = mt * mt;
        let mt3 = mt2 * mt;

        let x = mt3 * p0.0 + 3.0 * mt2 * t * p1.0 + 3.0 * mt * t2 * p2.0 + t3 * p3.0;
        let y = mt3 * p0.1 + 3.0 * mt2 * t * p1.1 + 3.0 * mt * t2 * p2.1 + t3 * p3.1;

        ctx.draw(&CanvasLine {
            x1: prev.0,
            y1: prev.1,
            x2: x,
            y2: y,
            color,
        });

        prev = (x, y);
    }
}

/// Draw an ellipse using line segments, respecting transform.
fn draw_ellipse(
    ctx: &mut ratatui::widgets::canvas::Context<'_>,
    cx: f64,
    cy: f64,
    rx: f64,
    ry: f64,
    transform: &Transform,
    color: Color,
) {
    let segments = 32;
    let step = std::f64::consts::TAU / segments as f64;

    // Get transformed radii vectors
    let (rx_tx, rx_ty) = transform.apply_vector(rx, 0.0);
    let (ry_tx, ry_ty) = transform.apply_vector(0.0, ry);

    let mut prev_x = cx + rx_tx;
    let mut prev_y = cy + rx_ty;

    for i in 1..=segments {
        let angle = step * i as f64;
        let cos = angle.cos();
        let sin = angle.sin();

        // Point on ellipse in local coords, then transform the offset
        let dx = rx_tx * cos + ry_tx * sin;
        let dy = rx_ty * cos + ry_ty * sin;

        let x = cx + dx;
        let y = cy + dy;

        ctx.draw(&CanvasLine {
            x1: prev_x,
            y1: prev_y,
            x2: x,
            y2: y,
            color,
        });

        prev_x = x;
        prev_y = y;
    }
}

/// Fill a polygon using horizontal scanlines.
/// This is a simple implementation that works for convex and simple concave polygons.
fn fill_polygon(
    ctx: &mut ratatui::widgets::canvas::Context<'_>,
    vertices: &[(f64, f64)],
    color: Color,
) {
    if vertices.len() < 3 {
        return;
    }

    // Find bounding box
    let mut min_y = f64::MAX;
    let mut max_y = f64::MIN;
    for &(_, y) in vertices {
        min_y = min_y.min(y);
        max_y = max_y.max(y);
    }

    // Number of scanlines (adjust density based on polygon size)
    let height = max_y - min_y;
    let num_lines = (height * 2.0).clamp(10.0, 100.0) as usize;
    let step = height / num_lines as f64;

    // For each scanline, find intersections with polygon edges
    for i in 0..=num_lines {
        let y = min_y + step * i as f64;
        let mut intersections: Vec<f64> = Vec::new();

        // Check each edge
        for j in 0..vertices.len() {
            let (x1, y1) = vertices[j];
            let (x2, y2) = vertices[(j + 1) % vertices.len()];

            // Check if scanline crosses this edge
            if (y1 <= y && y < y2) || (y2 <= y && y < y1) {
                // Calculate x intersection
                let t = (y - y1) / (y2 - y1);
                let x = x1 + t * (x2 - x1);
                intersections.push(x);
            }
        }

        // Sort intersections and draw horizontal lines between pairs
        intersections.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

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
