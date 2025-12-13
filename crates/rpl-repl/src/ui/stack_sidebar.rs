//! Compact stack sidebar with selection support.

use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Paragraph};

use crate::app::{App, Focus};

use super::stack_view::{format_value, value_style};

/// Render the stack sidebar with selection indicator.
pub fn render_stack_sidebar(frame: &mut Frame, app: &App, area: Rect) {
    let border_color = if app.focus == Focus::Stack {
        Color::Cyan
    } else {
        Color::DarkGray
    };

    let block = Block::default()
        .title(" Stack ")
        .borders(Borders::ALL)
        .border_style(Style::default().fg(border_color));

    let inner = block.inner(area);
    frame.render_widget(block, area);

    let stack = app.session.vm().stack();
    let visible_height = inner.height as usize;
    let width = inner.width as usize;

    if stack.is_empty() {
        // Show empty message
        let empty_msg = Paragraph::new("(empty)")
            .style(Style::default().fg(Color::DarkGray))
            .alignment(Alignment::Center);
        frame.render_widget(empty_msg, inner);
        return;
    }

    // Build lines for visible stack items
    let mut lines: Vec<Line> = Vec::new();
    let stack_len = stack.len();

    // Calculate which levels to show
    // Level 1 should be at the bottom, with scrolling for large stacks
    let scroll = app.stack_scroll;
    let _start_level = (scroll + 1).min(stack_len);
    let end_level = (scroll + visible_height).min(stack_len);

    // We want to show from bottom up, with level 1 at the bottom
    // If we have more items than can fit, show the most recent ones
    for row in 0..visible_height {
        // Row 0 is at top, but we want level numbers to increase from bottom
        // So row 0 = highest level shown, row (height-1) = lowest level shown (closest to 1)
        let level = if stack_len <= visible_height {
            // All items fit: row 0 = level stack_len, bottom = level 1
            stack_len.saturating_sub(row)
        } else {
            // Scrolled view
            (end_level).saturating_sub(row)
        };

        if level < 1 || level > stack_len {
            // Empty row
            lines.push(Line::from(""));
            continue;
        }

        let value = &stack[stack_len - level];
        let is_selected = app.selected_level == Some(level);

        // Format: "►N: value" or " N: value"
        let prefix = if is_selected { "►" } else { " " };
        let level_str = format!("{}:", level);

        // Truncate value to fit
        let formatted = format_value(value);
        let available_width = width.saturating_sub(prefix.len() + level_str.len() + 1);
        let display_value = if formatted.len() > available_width {
            format!("{}…", &formatted[..available_width.saturating_sub(1)])
        } else {
            formatted
        };

        let style = if is_selected {
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)
        } else {
            value_style(value)
        };

        let line = Line::from(vec![
            Span::styled(prefix, if is_selected { style } else { Style::default() }),
            Span::styled(level_str, Style::default().fg(Color::DarkGray)),
            Span::raw(" "),
            Span::styled(display_value, style),
        ]);

        lines.push(line);
    }

    let paragraph = Paragraph::new(lines);
    frame.render_widget(paragraph, inner);
}
