//! Stack value formatting and styling.

use ratatui::style::{Color, Style};

use rpl::value::Value;

/// Format a value for display.
pub fn format_value(value: &Value) -> String {
    match value {
        Value::Integer(i) => i.to_string(),
        Value::Real(r) => format_real(*r),
        Value::String(s) => format!("\"{}\"", escape_string(s)),
        Value::List(items) => {
            if items.is_empty() {
                "{ }".to_string()
            } else if items.len() <= 5 {
                let inner: Vec<String> = items.iter().map(format_value).collect();
                format!("{{ {} }}", inner.join(" "))
            } else {
                let first_few: Vec<String> = items.iter().take(3).map(format_value).collect();
                format!("{{ {} ... }} ({} items)", first_few.join(" "), items.len())
            }
        }
        Value::Program(data) => {
            let has_source = data.source_map.as_ref().is_some_and(|sm| !sm.source.is_empty());
            let debug_marker = if has_source { " [source]" } else { "" };
            format!("<< {} bytes{} >>", data.code.len(), debug_marker)
        }
        Value::Symbolic(expr) => format!("'{}", expr),
        Value::Library(lib) => format!("<Library {}>", lib.id),
        Value::Bytes(data) => format!("<Bytes {} bytes>", data.len()),
    }
}

/// Get the style for a value type.
pub fn value_style(value: &Value) -> Style {
    match value {
        Value::Integer(_) | Value::Real(_) => Style::default().fg(Color::Yellow),
        Value::String(_) => Style::default().fg(Color::Green),
        Value::List(_) => Style::default().fg(Color::Blue),
        Value::Program { .. } => Style::default().fg(Color::Magenta),
        Value::Symbolic(_) => Style::default().fg(Color::Cyan),
        Value::Library(_) => Style::default().fg(Color::Red),
        Value::Bytes(_) => Style::default().fg(Color::Red),
    }
}

/// Format a real number.
fn format_real(r: f64) -> String {
    if r.is_nan() {
        "NaN".to_string()
    } else if r.is_infinite() {
        if r > 0.0 { "Inf" } else { "-Inf" }.to_string()
    } else if r.fract() == 0.0 && r.abs() < 1e15 {
        // Integer-like, show with trailing dot
        format!("{}.", r as i64)
    } else {
        format!("{}", r)
    }
}

/// Escape special characters in a string for display.
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c if c.is_control() => result.push_str(&format!("\\x{:02x}", c as u32)),
            c => result.push(c),
        }
    }
    result
}
