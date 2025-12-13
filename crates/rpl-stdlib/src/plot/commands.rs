//! Command byte constants for plot objects.
//!
//! Commands occupy the range 0x60-0x7F.

// Path commands
pub const CMD_MOVETO: u8 = 0x6D;      // 'm' - x, y
pub const CMD_LINETO: u8 = 0x6C;      // 'l' - x, y

// Shape commands
pub const CMD_ARC: u8 = 0x61;         // 'a' - x, y, r, start, end
pub const CMD_BEZIER: u8 = 0x62;      // 'b' - x1, y1, x2, y2, x3, y3
pub const CMD_CIRCLE: u8 = 0x63;      // 'c' - x, y, r
pub const CMD_ELLIPSE: u8 = 0x65;     // 'e' - x, y, rx, ry
pub const CMD_PIXEL: u8 = 0x70;       // 'p' - x, y
pub const CMD_RECT: u8 = 0x72;        // 'r' - x, y, w, h

// Path operations
pub const CMD_FILL: u8 = 0x66;        // 'f' - (no params)
pub const CMD_STROKE: u8 = 0x73;      // 's' - (no params)

// Style commands
pub const CMD_FILLCOLOR: u8 = 0x67;   // 'g' - r, g, b, a
pub const CMD_COLOR: u8 = 0x6B;       // 'k' - r, g, b, a (stroke color)
pub const CMD_LINEWIDTH: u8 = 0x77;   // 'w' - width

// Text commands
pub const CMD_FONT: u8 = 0x6E;        // 'n' - size, name(string)
pub const CMD_TEXT: u8 = 0x74;        // 't' - x, y, string

// Transform commands
pub const CMD_IDENTITY: u8 = 0x69;    // 'i' - (no params)
pub const CMD_POP: u8 = 0x6F;         // 'o' - (no params)
pub const CMD_PUSH: u8 = 0x75;        // 'u' - (no params)
pub const CMD_TRANSLATE: u8 = 0x76;   // 'v' - dx, dy
pub const CMD_TRANSFORM: u8 = 0x78;   // 'x' - a, b, c, d, e, f (2x3 matrix)
pub const CMD_ROTATE: u8 = 0x79;      // 'y' - angle
pub const CMD_SCALE: u8 = 0x7A;       // 'z' - sx, sy

// Clipping
pub const CMD_CLIP: u8 = 0x7C;        // '|' - (no params, clip to current path)

// End marker
pub const CMD_END: u8 = 0x7E;         // '~' - end of plot

/// Check if a byte is a valid command.
pub fn is_command(b: u8) -> bool {
    (0x60..=0x7F).contains(&b)
}

/// Get the name of a command for display.
#[allow(dead_code)]
pub fn command_name(cmd: u8) -> &'static str {
    match cmd {
        CMD_MOVETO => "MOVETO",
        CMD_LINETO => "LINETO",
        CMD_ARC => "ARC",
        CMD_BEZIER => "BEZIER",
        CMD_CIRCLE => "CIRCLE",
        CMD_ELLIPSE => "ELLIPSE",
        CMD_PIXEL => "PIXEL",
        CMD_RECT => "RECT",
        CMD_FILL => "FILL",
        CMD_STROKE => "STROKE",
        CMD_FILLCOLOR => "FILLCOLOR",
        CMD_COLOR => "COLOR",
        CMD_LINEWIDTH => "LINEWIDTH",
        CMD_FONT => "FONT",
        CMD_TEXT => "TEXT",
        CMD_IDENTITY => "IDENTITY",
        CMD_POP => "POPSTATE",
        CMD_PUSH => "PUSHSTATE",
        CMD_TRANSLATE => "TRANSLATE",
        CMD_TRANSFORM => "TRANSFORM",
        CMD_ROTATE => "ROTATE",
        CMD_SCALE => "SCALE",
        CMD_CLIP => "CLIP",
        CMD_END => "END",
        _ => "UNKNOWN",
    }
}

/// Count the number of commands in a byte stream.
pub fn count_commands(bytes: &[u8]) -> usize {
    bytes.iter().filter(|&&b| is_command(b)).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn command_range() {
        assert!(is_command(0x60));
        assert!(is_command(0x7F));
        assert!(is_command(CMD_MOVETO));
        assert!(is_command(CMD_END));
        assert!(!is_command(0x5F));
        assert!(!is_command(0x80));
    }

    #[test]
    fn command_names() {
        assert_eq!(command_name(CMD_MOVETO), "MOVETO");
        assert_eq!(command_name(CMD_LINETO), "LINETO");
        assert_eq!(command_name(CMD_CIRCLE), "CIRCLE");
    }
}
