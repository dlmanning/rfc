//! Plot library for scalable vector graphics.
//!
//! Provides commands to build Plot blobs using a functional Blob->Blob style.
//! Each command takes a Blob, appends drawing commands, and returns the modified Blob.

use std::sync::{Arc, OnceLock};

use rpl::{
    interface::InterfaceSpec,
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
    Span,
};

use crate::commands::*;
use crate::encoding::{encode_number, encode_string, to_fixed_point};

/// Interface declaration for the Plot library.
const INTERFACE: &str = include_str!("plot.rpli");

/// Get the interface specification (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid plot interface"))
}

/// Plot library ID.
pub const PLOT_LIB_ID: LibId = 88;

// ============================================================================
// Helper functions
// ============================================================================

/// Pop a Blob from the stack and return it as a mutable Vec<u8>.
fn pop_blob(ctx: &mut ExecuteContext) -> Result<Vec<u8>, String> {
    match ctx.pop()? {
        Value::Bytes(arc) => Ok(arc.to_vec()),
        other => Err(format!("Expected Blob, got {}", other.type_name())),
    }
}

/// Push a Blob onto the stack.
fn push_blob(ctx: &mut ExecuteContext, bytes: Vec<u8>) -> Result<(), String> {
    ctx.push(Value::Bytes(Arc::from(bytes.into_boxed_slice())))
}

/// Pop a number from the stack as f64.
fn pop_number(ctx: &mut ExecuteContext) -> Result<f64, String> {
    match ctx.pop()? {
        Value::Real(r) => Ok(r),
        Value::Integer(i) => Ok(i as f64),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

/// Pop an integer from the stack.
fn pop_integer(ctx: &mut ExecuteContext) -> Result<i64, String> {
    match ctx.pop()? {
        Value::Integer(i) => Ok(i),
        Value::Real(r) => Ok(r as i64),
        other => Err(format!("Expected integer, got {}", other.type_name())),
    }
}

/// Pop a string from the stack.
fn pop_string(ctx: &mut ExecuteContext) -> Result<String, String> {
    match ctx.pop()? {
        Value::String(s) => Ok(s.to_string()),
        other => Err(format!("Expected string, got {}", other.type_name())),
    }
}

/// Pack RGBA components into a 32-bit integer (0xRRGGBBAA format).
fn pack_rgba(r: u8, g: u8, b: u8, a: u8) -> u32 {
    ((r as u32) << 24) | ((g as u32) << 16) | ((b as u32) << 8) | (a as u32)
}

/// Unpack a 32-bit RGBA color into components.
#[allow(dead_code)]
fn unpack_rgba(rgba: u32) -> (u8, u8, u8, u8) {
    let r = ((rgba >> 24) & 0xFF) as u8;
    let g = ((rgba >> 16) & 0xFF) as u8;
    let b = ((rgba >> 8) & 0xFF) as u8;
    let a = (rgba & 0xFF) as u8;
    (r, g, b, a)
}

/// Convert a Real color value to 0-255 range.
fn real_to_color(r: f64) -> u8 {
    if r > 1.0 {
        r.clamp(0.0, 255.0) as u8
    } else {
        (r * 255.0).clamp(0.0, 255.0) as u8
    }
}

/// Pop a color component (handles 0-1 or 0-255 range).
fn pop_color(ctx: &mut ExecuteContext) -> Result<u8, String> {
    match ctx.pop()? {
        Value::Real(r) => Ok(real_to_color(r)),
        Value::Integer(i) => Ok(i.clamp(0, 255) as u8),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

// ============================================================================
// Blob building helpers
// ============================================================================

/// Append a command byte to the blob.
fn append_byte(blob: &mut Vec<u8>, b: u8) {
    blob.push(b);
}

/// Append a coordinate (as fixed-point) to the blob.
fn append_coord(blob: &mut Vec<u8>, f: f64) {
    let fp = to_fixed_point(f);
    blob.extend(encode_number(fp as i64));
}

/// Append a number to the blob.
fn append_number(blob: &mut Vec<u8>, n: i64) {
    blob.extend(encode_number(n));
}

/// Append a string to the blob.
fn append_string(blob: &mut Vec<u8>, s: &str) {
    blob.extend(encode_string(s));
}

// ============================================================================
// Library Implementation
// ============================================================================

/// Plot library for RPL.
#[derive(Clone, Copy)]
pub struct PlotLib;

// Command IDs
mod cmd {
    pub const NEWPLOT: u16 = 0;
    pub const MOVETO: u16 = 1;
    pub const LINETO: u16 = 2;
    pub const CIRCLE: u16 = 3;
    pub const RECT: u16 = 4;
    pub const ELLIPSE: u16 = 5;
    pub const ARC: u16 = 6;
    pub const BEZIER: u16 = 7;
    pub const PIXEL: u16 = 8;
    pub const TEXT: u16 = 9;
    pub const FILL: u16 = 10;
    pub const STROKE: u16 = 11;
    pub const CLIP: u16 = 12;
    pub const LINEWIDTH: u16 = 13;
    pub const COLOR: u16 = 14;
    pub const FILLCOLOR: u16 = 15;
    pub const FONT: u16 = 16;
    pub const IDENTITY: u16 = 17;
    pub const TRANSFORM: u16 = 18;
    pub const SCALE: u16 = 19;
    pub const ROTATE: u16 = 20;
    pub const TRANSLATE: u16 = 21;
    pub const PUSHSTATE: u16 = 22;
    pub const POPSTATE: u16 = 23;
    pub const RGBA: u16 = 24;
}

impl LibraryLowerer for PlotLib {
    fn id(&self) -> LibId {
        PLOT_LIB_ID
    }

    fn lower_command(
        &self,
        cmd_id: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(PLOT_LIB_ID, cmd_id);
        Ok(())
    }
}

impl LibraryExecutor for PlotLib {
    fn id(&self) -> LibId {
        PLOT_LIB_ID
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::NEWPLOT => {
                // Create new empty blob with magic header
                let blob = PLOT_MAGIC.to_vec();
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::MOVETO => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_MOVETO);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::LINETO => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_LINETO);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::CIRCLE => {
                let r = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_CIRCLE);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                append_coord(&mut blob, r);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::RECT => {
                let h = pop_number(ctx)?;
                let w = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_RECT);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                append_coord(&mut blob, w);
                append_coord(&mut blob, h);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::ELLIPSE => {
                let ry = pop_number(ctx)?;
                let rx = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_ELLIPSE);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                append_coord(&mut blob, rx);
                append_coord(&mut blob, ry);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::ARC => {
                let end_angle = pop_number(ctx)?;
                let start_angle = pop_number(ctx)?;
                let r = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_ARC);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                append_coord(&mut blob, r);
                append_coord(&mut blob, start_angle);
                append_coord(&mut blob, end_angle);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::BEZIER => {
                let y3 = pop_number(ctx)?;
                let x3 = pop_number(ctx)?;
                let y2 = pop_number(ctx)?;
                let x2 = pop_number(ctx)?;
                let y1 = pop_number(ctx)?;
                let x1 = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_BEZIER);
                append_coord(&mut blob, x1);
                append_coord(&mut blob, y1);
                append_coord(&mut blob, x2);
                append_coord(&mut blob, y2);
                append_coord(&mut blob, x3);
                append_coord(&mut blob, y3);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::PIXEL => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_PIXEL);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::TEXT => {
                let text = pop_string(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_TEXT);
                append_coord(&mut blob, x);
                append_coord(&mut blob, y);
                append_string(&mut blob, &text);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::FILL => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_FILL);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::STROKE => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_STROKE);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::CLIP => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_CLIP);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::LINEWIDTH => {
                let width = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_LINEWIDTH);
                append_coord(&mut blob, width);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::COLOR => {
                let color = pop_integer(ctx)? as u32;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_COLOR);
                append_number(&mut blob, color as i64);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::FILLCOLOR => {
                let color = pop_integer(ctx)? as u32;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_FILLCOLOR);
                append_number(&mut blob, color as i64);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::FONT => {
                let name = pop_string(ctx)?;
                let size = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_FONT);
                append_coord(&mut blob, size);
                append_string(&mut blob, &name);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::IDENTITY => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_IDENTITY);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::TRANSFORM => {
                let f = pop_number(ctx)?;
                let e = pop_number(ctx)?;
                let d = pop_number(ctx)?;
                let c = pop_number(ctx)?;
                let b = pop_number(ctx)?;
                let a = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_TRANSFORM);
                append_coord(&mut blob, a);
                append_coord(&mut blob, b);
                append_coord(&mut blob, c);
                append_coord(&mut blob, d);
                append_coord(&mut blob, e);
                append_coord(&mut blob, f);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::SCALE => {
                let sy = pop_number(ctx)?;
                let sx = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_SCALE);
                append_coord(&mut blob, sx);
                append_coord(&mut blob, sy);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::ROTATE => {
                let angle = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_ROTATE);
                append_coord(&mut blob, angle);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::TRANSLATE => {
                let dy = pop_number(ctx)?;
                let dx = pop_number(ctx)?;
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_TRANSLATE);
                append_coord(&mut blob, dx);
                append_coord(&mut blob, dy);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::PUSHSTATE => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_PUSH);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::POPSTATE => {
                let mut blob = pop_blob(ctx)?;
                append_byte(&mut blob, CMD_POP);
                push_blob(ctx, blob)?;
                Ok(())
            }

            cmd::RGBA => {
                // Pure function: a b c d -> Int (no blob involved)
                let a = pop_color(ctx)?;
                let b = pop_color(ctx)?;
                let g = pop_color(ctx)?;
                let r = pop_color(ctx)?;
                let color = pack_rgba(r, g, b, a);
                ctx.push(Value::Integer(color as i64))?;
                Ok(())
            }

            _ => Err(format!("Unknown plot command: {}", ctx.cmd)),
        }
    }
}

/// Register the plot library with a session.
pub fn register_plot_lib(session: &mut rpl::Session) {
    session.interfaces_mut().add(interface().clone());
    session.lowerers_mut().add(PlotLib);
    session.executors_mut().add(PlotLib);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl::libs::LibraryInterface;
    use rpl::types::CStack;

    #[test]
    fn plot_lib_id() {
        assert_eq!(interface().id(), 88);
    }

    #[test]
    fn plot_lib_name() {
        assert_eq!(interface().name(), "Plot");
    }

    #[test]
    fn plot_lib_has_expected_commands() {
        let commands = interface().commands();
        let names: Vec<&str> = commands
            .iter()
            .flat_map(|c| c.names.iter().map(|s| s.as_str()))
            .collect();

        assert!(names.contains(&"NEWPLOT"));
        assert!(names.contains(&"CIRCLE"));
        assert!(names.contains(&"RECT"));
        assert!(names.contains(&"FILL"));
        assert!(names.contains(&"STROKE"));
        assert!(names.contains(&"RGBA"));
    }

    #[test]
    fn plot_lib_command_count() {
        let commands = interface().commands();
        // 25 commands total
        assert_eq!(commands.len(), 25);
    }

    #[test]
    fn test_pack_rgba() {
        assert_eq!(pack_rgba(0xFF, 0x00, 0x00, 0xFF), 0xFF0000FF); // Red
        assert_eq!(pack_rgba(0x00, 0xFF, 0x00, 0xFF), 0x00FF00FF); // Green
        assert_eq!(pack_rgba(0x00, 0x00, 0xFF, 0xFF), 0x0000FFFF); // Blue
        assert_eq!(pack_rgba(0xAA, 0xBB, 0xCC, 0xDD), 0xAABBCCDD);
    }

    #[test]
    fn test_unpack_rgba() {
        let (r, g, b, a) = unpack_rgba(0xAABBCCDD);
        assert_eq!(r, 0xAA);
        assert_eq!(g, 0xBB);
        assert_eq!(b, 0xCC);
        assert_eq!(a, 0xDD);
    }

    #[test]
    fn test_rgba_roundtrip() {
        for rgba in [0x00000000, 0xFFFFFFFF, 0xFF0000FF, 0x12345678] {
            let (r, g, b, a) = unpack_rgba(rgba);
            assert_eq!(pack_rgba(r, g, b, a), rgba);
        }
    }

    #[test]
    fn test_real_to_color_fractional() {
        assert_eq!(real_to_color(0.0), 0);
        assert_eq!(real_to_color(1.0), 255);
        assert_eq!(real_to_color(0.5), 127);
    }

    #[test]
    fn test_real_to_color_byte_range() {
        assert_eq!(real_to_color(128.0), 128);
        assert_eq!(real_to_color(255.0), 255);
        assert_eq!(real_to_color(300.0), 255); // Clamped
    }

    #[test]
    fn test_command_effects() {
        let empty_stack = CStack::new();

        // NEWPLOT: 0 in, 1 out (Blob)
        let effect = interface().command_effect(cmd::NEWPLOT, &empty_stack);
        assert_eq!(effect.consumes(), Some(0));
        assert_eq!(effect.produces(), Some(1));

        // CIRCLE: 4 in (Blob + 3 args), 1 out (Blob)
        let effect = interface().command_effect(cmd::CIRCLE, &empty_stack);
        assert_eq!(effect.consumes(), Some(4));
        assert_eq!(effect.produces(), Some(1));

        // FILL: 1 in (Blob), 1 out (Blob)
        let effect = interface().command_effect(cmd::FILL, &empty_stack);
        assert_eq!(effect.consumes(), Some(1));
        assert_eq!(effect.produces(), Some(1));

        // RGBA: 4 in, 1 out (Int) - no blob
        let effect = interface().command_effect(cmd::RGBA, &empty_stack);
        assert_eq!(effect.consumes(), Some(4));
        assert_eq!(effect.produces(), Some(1));
    }
}
