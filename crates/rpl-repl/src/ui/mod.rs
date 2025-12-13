//! UI rendering for the REPL (IDE-style three-column layout).

mod layout;
pub mod multiline_editor;
pub mod plot_view;
mod stack_sidebar;
mod stack_view;
mod status_bar;

use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Paragraph};

use super::app::{App, Focus};
use layout::AppLayout;

pub use multiline_editor::MultilineEditor;
pub use plot_view::PlotViewState;

/// Render the entire UI (IDE-style three-column layout).
pub fn render(frame: &mut Frame, app: &App) {
    let show_preview = app.should_show_preview();
    let layout = AppLayout::compute(frame.area(), show_preview);

    render_header(frame, app, layout.header_area);
    render_editor(frame, app, layout.editor_area);

    if let Some(preview_area) = layout.preview_area
        && let Some(ref plot_state) = app.plot_view
    {
        plot_view::render_plot_view(frame, plot_state, preview_area);
    }

    stack_sidebar::render_stack_sidebar(frame, app, layout.stack_area);
    render_menu(frame, app, layout.menu_area);

    // Render search overlay if active
    if app.in_search_mode() {
        render_search_overlay(frame, app);
    }
}

/// Render the header line (path, angle mode, flags).
fn render_header(frame: &mut Frame, app: &App, area: Rect) {
    let path = app.session.vm().dir_path();
    let path_str = if path.is_empty() {
        "{ HOME }".to_string()
    } else {
        format!("{{ HOME {} }}", path.join(" "))
    };

    // Count variables in current directory
    let var_count = app.session.vm().globals_snapshot().len();
    let subdir_count: usize = app.session.vm().subdir_names().count();

    let header = Line::from(vec![
        Span::styled(&path_str, Style::default().fg(Color::Cyan)),
        Span::raw("  "),
        Span::styled(
            format!("{}v {}d", var_count, subdir_count),
            Style::default().fg(Color::DarkGray),
        ),
        // Right-align the mode indicator
        Span::raw(" ".repeat(area.width.saturating_sub(path_str.len() as u16 + 15) as usize)),
        Span::styled("RAD", Style::default().fg(Color::Yellow)),
        Span::raw(" "),
        Span::styled("PRG", Style::default().fg(Color::DarkGray)),
    ]);

    let paragraph = Paragraph::new(header);
    frame.render_widget(paragraph, area);
}

/// Render the editor pane (multiline editor).
fn render_editor(frame: &mut Frame, app: &App, area: Rect) {
    let title = if let Some(level) = app.selected_level {
        format!(" Level {} ", level)
    } else {
        " Input ".to_string()
    };

    let border_color = if app.focus == Focus::Editor {
        Color::Green
    } else {
        Color::DarkGray
    };

    let dirty_indicator = if app.editor_dirty { " *" } else { "" };

    let block = Block::default()
        .title(format!("{}{}", title, dirty_indicator))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(border_color));

    let inner = block.inner(area);
    frame.render_widget(block, area);

    // Show error if present
    if let Some(ref err) = app.last_error {
        let error_line = Line::from(vec![
            Span::styled(
                "Error: ",
                Style::default().fg(Color::Red).add_modifier(Modifier::BOLD),
            ),
            Span::styled(err.as_str(), Style::default().fg(Color::Red)),
        ]);
        let error_para = Paragraph::new(error_line);

        // Reserve first line for error
        if inner.height > 1 {
            let error_area = Rect {
                height: 1,
                ..inner
            };
            frame.render_widget(error_para, error_area);

            // Render editor content below error
            let editor_area = Rect {
                y: inner.y + 1,
                height: inner.height - 1,
                ..inner
            };
            render_editor_content(frame, app, editor_area);
        }
    } else {
        render_editor_content(frame, app, inner);
    }
}

/// Render the editor content with cursor.
fn render_editor_content(frame: &mut Frame, app: &App, area: Rect) {
    let content = app.editor.content();
    let lines: Vec<&str> = content.lines().collect();
    let visible_lines = area.height as usize;

    // Get cursor position
    let (cursor_row, _) = app.editor.cursor_pos();
    let cursor_col = app.editor.cursor_display_col(); // Use character count, not bytes

    // Calculate scroll offset to keep cursor visible
    let scroll_offset = if cursor_row >= visible_lines {
        cursor_row - visible_lines + 1
    } else {
        0
    };

    // Build visible lines
    let mut display_lines: Vec<Line> = Vec::new();
    for (i, line) in lines.iter().enumerate().skip(scroll_offset).take(visible_lines) {
        let line_num = i + 1;
        let line_num_str = format!("{:3} ", line_num);

        display_lines.push(Line::from(vec![
            Span::styled(line_num_str, Style::default().fg(Color::DarkGray)),
            Span::raw(*line),
        ]));
    }

    // Pad with empty lines if needed
    while display_lines.len() < visible_lines {
        let line_num = scroll_offset + display_lines.len() + 1;
        let line_num_str = format!("{:3} ", line_num);
        display_lines.push(Line::from(vec![
            Span::styled(line_num_str, Style::default().fg(Color::DarkGray)),
        ]));
    }

    let paragraph = Paragraph::new(display_lines);
    frame.render_widget(paragraph, area);

    // Position cursor if editor is focused
    if app.focus == Focus::Editor {
        let cursor_x = area.x + 4 + cursor_col as u16; // 4 for line number prefix
        let cursor_y = area.y + (cursor_row - scroll_offset) as u16;
        if cursor_y < area.y + area.height {
            frame.set_cursor_position((cursor_x, cursor_y));
        }
    }
}

/// Render the search overlay (Ctrl+R history search).
fn render_search_overlay(frame: &mut Frame, app: &App) {
    if let Some(ref search) = app.history_search {
        let area = frame.area();

        // Render at bottom of screen, above menu
        let search_area = Rect {
            x: area.x + 1,
            y: area.y + area.height.saturating_sub(3),
            width: area.width.saturating_sub(2),
            height: 2,
        };

        let query = search.query();
        let match_info = if search.has_matches() {
            format!(" ({}/{})", search.current_match_index(), search.match_count())
        } else if !query.is_empty() {
            " (no match)".to_string()
        } else {
            String::new()
        };

        let prompt = format!("(search)`{}'{}: ", query, match_info);
        let result = search.current_entry(&app.history).unwrap_or("");

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(Color::Yellow));

        let inner = block.inner(search_area);
        frame.render_widget(block, search_area);

        let search_line = Line::from(vec![
            Span::styled(&prompt, Style::default().fg(Color::Yellow)),
            Span::raw(result),
        ]);

        let paragraph = Paragraph::new(search_line);
        frame.render_widget(paragraph, inner);

        // Position cursor at end of query
        let cursor_x = inner.x + "(search)`".len() as u16 + query.chars().count() as u16;
        let cursor_y = inner.y;
        frame.set_cursor_position((cursor_x, cursor_y));
    }
}

/// Render the soft menu bar (context-sensitive function keys).
fn render_menu(frame: &mut Frame, app: &App, area: Rect) {
    // Calculate menu item width (divide screen into 6 sections)
    let item_width = area.width / 6;

    // Menu items depend on context
    let menu_items = match (app.focus, app.has_selection()) {
        (Focus::Editor, false) => {
            // New input mode
            vec!["RUN", "CLEAR", "VAR", "HIST", "MODE", "HELP"]
        }
        (Focus::Editor, true) => {
            // Editing existing item
            vec!["RUN", "APPLY", "REVERT", "DROP", "DUP", "HELP"]
        }
        (Focus::Stack, _) => {
            // Stack navigation
            vec!["EDIT", "VIEW", "DROP", "DUP", "SWAP", "HELP"]
        }
    };

    let mut spans: Vec<Span> = Vec::new();

    for (i, item) in menu_items.iter().enumerate() {
        // Add separator
        if i > 0 {
            spans.push(Span::styled("|", Style::default().fg(Color::DarkGray)));
        }

        // Center the item text in its cell
        let text_len = item.len();
        let cell_width = if i == menu_items.len() - 1 {
            (area.width - (item_width * 5) - 5) as usize // Last cell gets remaining width
        } else {
            (item_width - 1) as usize
        };
        let padding_left = (cell_width.saturating_sub(text_len)) / 2;
        let padding_right = cell_width.saturating_sub(text_len).saturating_sub(padding_left);

        spans.push(Span::raw(" ".repeat(padding_left)));
        spans.push(Span::styled(
            *item,
            Style::default().fg(Color::White).add_modifier(Modifier::BOLD),
        ));
        spans.push(Span::raw(" ".repeat(padding_right)));
    }

    let menu_line = Line::from(spans);
    let paragraph = Paragraph::new(menu_line).style(Style::default().bg(Color::DarkGray));
    frame.render_widget(paragraph, area);
}
