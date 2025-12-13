//! Stack value formatting and styling.

use ratatui::style::{Color, Style};

use rpl_core::TypeId;
use rpl_session::Value;

/// Format a value for display.
pub fn format_value(value: &Value) -> String {
    match value {
        Value::Real(r) => format_real(*r),
        Value::Int(i) => i.to_string(),
        Value::Bool(b) => if *b { "1" } else { "0" }.to_string(),
        Value::String(s) => format!("\"{}\"", escape_string(s)),
        Value::Symbol(sym) => format!("'{:?}", sym),
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
        Value::Program { code, debug_info } => {
            let debug_marker = if debug_info.is_some() { " [debug]" } else { "" };
            format!("<< {} words{} >>", code.len(), debug_marker)
        }
        Value::Object { type_id, data } if *type_id == TypeId::COMPLEX => {
            format_complex(data)
        }
        Value::Object { type_id, data } => {
            match type_id.name() {
                Some(name) => format!("<{} size={}>", name, data.len()),
                None => format!("<Object({}) size={}>", type_id.as_u16(), data.len()),
            }
        }
    }
}

/// Format a complex number for display.
fn format_complex(data: &[u32]) -> String {
    if data.len() == 4 {
        // Rectangular: (re, im)
        let re = decode_f64(data[0], data[1]);
        let im = decode_f64(data[2], data[3]);
        format!("({}, {})", format_real(re), format_real(im))
    } else if data.len() == 5 {
        // Polar: (mag∠angle) or (mag∠angle°)
        let mag = decode_f64(data[0], data[1]);
        let angle = decode_f64(data[2], data[3]);
        let mode = data[4] as u8;
        if mode == 1 {
            format!("({}∠{}°)", format_real(mag), format_real(angle))
        } else {
            format!("({}∠{})", format_real(mag), format_real(angle))
        }
    } else {
        "<Complex invalid>".to_string()
    }
}

/// Decode two u32 words to an f64.
fn decode_f64(hi: u32, lo: u32) -> f64 {
    let bits = ((hi as u64) << 32) | (lo as u64);
    f64::from_bits(bits)
}

/// Format a value in short form (for variable display).
#[allow(dead_code)]
pub fn format_value_short(value: &Value) -> String {
    match value {
        Value::Real(r) => format_real(*r),
        Value::Int(i) => i.to_string(),
        Value::Bool(b) => if *b { "1" } else { "0" }.to_string(),
        Value::String(s) if s.len() <= 20 => format!("\"{}\"", escape_string(s)),
        Value::String(s) => format!("\"{}...\"", escape_string(&s[..17])),
        Value::Symbol(sym) => format!("'{:?}", sym),
        Value::List(items) if items.is_empty() => "{ }".to_string(),
        Value::List(items) => format!("{{...}} ({})", items.len()),
        Value::Program { code, .. } => format!("<<{}>>", code.len()),
        Value::Object { type_id, .. } => {
            match type_id.name() {
                Some(name) => format!("<{}>", name),
                None => format!("<Object({})>", type_id.as_u16()),
            }
        }
    }
}

/// Get the style for a value type.
pub fn value_style(value: &Value) -> Style {
    match value {
        Value::Real(_) | Value::Int(_) => Style::default().fg(Color::Yellow),
        Value::Bool(_) => Style::default().fg(Color::Magenta),
        Value::String(_) => Style::default().fg(Color::Green),
        Value::Symbol(_) => Style::default().fg(Color::Cyan),
        Value::List(_) => Style::default().fg(Color::Blue),
        Value::Program { .. } => Style::default().fg(Color::Magenta),
        Value::Object { .. } => Style::default().fg(Color::Red),
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
