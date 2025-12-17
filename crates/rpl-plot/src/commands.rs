//! Command byte constants for plot objects.
//!
//! Each command is a single byte using ASCII mnemonics for easy debugging.
//! Commands are followed by their arguments encoded as variable-length numbers.
//!
//! # Command Categories
//!
//! - **Path commands**: `MOVETO`, `LINETO` - define paths
//! - **Shape commands**: `CIRCLE`, `RECT`, `ELLIPSE`, `ARC`, `BEZIER`, `PIXEL`
//! - **Path operations**: `FILL`, `STROKE` - render current shapes
//! - **Style commands**: `COLOR`, `FILLCOLOR`, `LINEWIDTH`
//! - **Text commands**: `FONT`, `TEXT`
//! - **Transform commands**: `TRANSLATE`, `ROTATE`, `SCALE`, `TRANSFORM`, `IDENTITY`
//! - **State commands**: `PUSH`, `POP`, `CLIP`

// ============================================================================
// Path Commands
// ============================================================================

/// Move to position without drawing. Args: x, y
pub const CMD_MOVETO: u8 = 0x6D; // 'm'

/// Draw line from current position. Args: x, y
pub const CMD_LINETO: u8 = 0x6C; // 'l'

// ============================================================================
// Shape Commands
// ============================================================================

/// Add arc to path. Args: cx, cy, r, start_angle, end_angle
pub const CMD_ARC: u8 = 0x61; // 'a'

/// Add cubic bezier curve. Args: x1, y1, x2, y2, x3, y3
pub const CMD_BEZIER: u8 = 0x62; // 'b'

/// Add circle to path. Args: cx, cy, r
pub const CMD_CIRCLE: u8 = 0x63; // 'c'

/// Add ellipse to path. Args: cx, cy, rx, ry
pub const CMD_ELLIPSE: u8 = 0x65; // 'e'

/// Draw single pixel. Args: x, y
pub const CMD_PIXEL: u8 = 0x70; // 'p'

/// Add rectangle to path. Args: x, y, w, h
pub const CMD_RECT: u8 = 0x72; // 'r'

// ============================================================================
// Path Operations
// ============================================================================

/// Fill current shapes with fill color. Args: (none)
pub const CMD_FILL: u8 = 0x66; // 'f'

/// Stroke current shapes with stroke color. Args: (none)
pub const CMD_STROKE: u8 = 0x73; // 's'

// ============================================================================
// Style Commands
// ============================================================================

/// Set fill color. Args: r, g, b, a
pub const CMD_FILLCOLOR: u8 = 0x67; // 'g'

/// Set stroke color. Args: r, g, b, a
pub const CMD_COLOR: u8 = 0x6B; // 'k'

/// Set line width for strokes. Args: width
pub const CMD_LINEWIDTH: u8 = 0x77; // 'w'

// ============================================================================
// Text Commands
// ============================================================================

/// Set font for text. Args: size, name (string)
pub const CMD_FONT: u8 = 0x6E; // 'n'

/// Draw text at position. Args: x, y, text (string)
pub const CMD_TEXT: u8 = 0x74; // 't'

// ============================================================================
// Transform Commands
// ============================================================================

/// Reset transform to identity. Args: (none)
pub const CMD_IDENTITY: u8 = 0x69; // 'i'

/// Translate coordinate system. Args: dx, dy
pub const CMD_TRANSLATE: u8 = 0x76; // 'v'

/// Apply 2x3 affine transform matrix. Args: a, b, c, d, e, f
pub const CMD_TRANSFORM: u8 = 0x78; // 'x'

/// Rotate coordinate system. Args: angle (radians)
pub const CMD_ROTATE: u8 = 0x79; // 'y'

/// Scale coordinate system. Args: sx, sy
pub const CMD_SCALE: u8 = 0x7A; // 'z'

// ============================================================================
// State Commands
// ============================================================================

/// Push graphics state onto stack. Args: (none)
pub const CMD_PUSH: u8 = 0x75; // 'u'

/// Pop graphics state from stack. Args: (none)
pub const CMD_POP: u8 = 0x6F; // 'o'

/// Set clipping region to current path. Args: (none)
pub const CMD_CLIP: u8 = 0x7C; // '|'

// ============================================================================
// Control
// ============================================================================

/// End of plot marker. Args: (none)
pub const CMD_END: u8 = 0x7E; // '~'
