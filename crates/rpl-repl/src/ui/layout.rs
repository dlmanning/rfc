//! Screen layout for the REPL (IDE-style three-column).

use ratatui::layout::{Constraint, Direction, Layout, Rect};

/// Computed layout areas for the REPL.
pub struct AppLayout {
    /// Header/status line at top.
    pub header_area: Rect,
    /// Editor pane (left side - always visible).
    pub editor_area: Rect,
    /// Preview pane (middle - only when viewing plots/etc).
    pub preview_area: Option<Rect>,
    /// Stack sidebar (right side - compact view).
    pub stack_area: Rect,
    /// Soft menu keys at bottom.
    pub menu_area: Rect,
}

impl AppLayout {
    /// Compute layout areas from the frame size.
    ///
    /// IDE-style layout:
    /// ```text
    /// ┌─────────────────────────────────────────────┐
    /// │ { HOME }                          RAD  PRG  │  <- Header (1 line)
    /// ├─────────────────┬─────────────┬─────────────┤
    /// │                 │             │ 4: 3.14159  │
    /// │  [Editor]       │  [Preview]  │ 3: "hello"  │  <- Main area
    /// │                 │             │►2: <Plot>   │     Editor | Preview | Stack
    /// │  BEGINPLOT      │    ○        │ 1: 42       │     ► = selected
    /// │  50 50 CIRCLE   │             │             │
    /// │  ENDPLOT        │             │             │
    /// │                 │             │             │
    /// ├─────────────────┴─────────────┴─────────────┤
    /// │ RUN  │APPLY │REVERT│ NEW  │ DROP │ HELP    │  <- Soft menu
    /// └─────────────────────────────────────────────┘
    /// ```
    ///
    /// Without preview (non-visual items):
    /// ```text
    /// ┌─────────────────────────────────────────────┐
    /// │ { HOME }                          RAD  PRG  │
    /// ├───────────────────────────────┬─────────────┤
    /// │                               │ 4: 3.14159  │
    /// │  [Editor]                     │ 3: "hello"  │
    /// │                               │ 2: 42       │
    /// │  1 2 +                        │►1: 7        │
    /// │                               │             │
    /// ├───────────────────────────────┴─────────────┤
    /// │ RUN  │CLEAR │ VAR  │ HIST │ MODE │ HELP    │
    /// └─────────────────────────────────────────────┘
    /// ```
    pub fn compute(frame_size: Rect, show_preview: bool) -> Self {
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Header
                Constraint::Min(6),    // Main content area
                Constraint::Length(1), // Menu
            ])
            .split(frame_size);

        let (editor_area, preview_area, stack_area) = if show_preview {
            // Three-column layout: Editor | Preview | Stack
            let content_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(40), // Editor
                    Constraint::Percentage(35), // Preview
                    Constraint::Percentage(25), // Stack
                ])
                .split(main_chunks[1]);
            (content_chunks[0], Some(content_chunks[1]), content_chunks[2])
        } else {
            // Two-column layout: Editor | Stack
            let content_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(70), // Editor
                    Constraint::Percentage(30), // Stack
                ])
                .split(main_chunks[1]);
            (content_chunks[0], None, content_chunks[1])
        };

        Self {
            header_area: main_chunks[0],
            editor_area,
            preview_area,
            stack_area,
            menu_area: main_chunks[2],
        }
    }
}
