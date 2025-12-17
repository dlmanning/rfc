//! Plot library for scalable vector graphics.
//!
//! Provides commands to build Plot objects that store drawing commands
//! as a compact byte stream for resolution-independent rendering.

use std::cell::RefCell;
use std::sync::Arc;

use rpl::{
    TypeId,
    ir::LibId,
    libs::{CommandInfo, ExecuteContext, ExecuteResult, Library, LibraryExecutor, LibraryLowerer, StackEffect},
    lower::LowerError,
    value::Value,
};

use crate::commands::*;
use crate::encoding::{encode_number, encode_string, to_fixed_point};

/// Plot library ID.
pub const PLOT_LIB_ID: LibId = 88;

/// State for a plot being constructed.
#[derive(Default)]
struct PlotState {
    bytes: Vec<u8>,
}

impl PlotState {
    fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    fn push_byte(&mut self, b: u8) {
        self.bytes.push(b);
    }

    fn push_coord(&mut self, f: f64) {
        let fp = to_fixed_point(f);
        self.bytes.extend(encode_number(fp as i64));
    }

    fn push_number(&mut self, n: i64) {
        self.bytes.extend(encode_number(n));
    }

    fn push_string(&mut self, s: &str) {
        self.bytes.extend(encode_string(s));
    }

    fn finalize(mut self) -> Vec<u8> {
        self.bytes.push(CMD_END);
        self.bytes
    }
}

// Thread-local storage for current plot state
thread_local! {
    static CURRENT_PLOT: RefCell<Option<PlotState>> = const { RefCell::new(None) };
}

fn with_plot_state<F, R>(f: F) -> Result<R, String>
where
    F: FnOnce(&mut PlotState) -> R,
{
    CURRENT_PLOT.with(|cell| {
        let mut borrow = cell.borrow_mut();
        match borrow.as_mut() {
            Some(state) => Ok(f(state)),
            None => Err("No plot in progress. Use BEGINPLOT first.".to_string()),
        }
    })
}

fn take_plot_state() -> Result<PlotState, String> {
    CURRENT_PLOT.with(|cell| {
        cell.borrow_mut()
            .take()
            .ok_or_else(|| "No plot in progress. Use BEGINPLOT first.".to_string())
    })
}

fn set_plot_state(state: PlotState) -> Result<(), String> {
    CURRENT_PLOT.with(|cell| {
        let mut borrow = cell.borrow_mut();
        if borrow.is_some() {
            return Err("Plot already in progress. Use ENDPLOT first.".to_string());
        }
        *borrow = Some(state);
        Ok(())
    })
}

/// Pack RGBA components into a 32-bit integer (0xRRGGBBAA format).
fn pack_rgba(r: u8, g: u8, b: u8, a: u8) -> u32 {
    ((r as u32) << 24) | ((g as u32) << 16) | ((b as u32) << 8) | (a as u32)
}

/// Unpack a 32-bit RGBA color into components.
fn unpack_rgba(rgba: u32) -> (u8, u8, u8, u8) {
    let r = ((rgba >> 24) & 0xFF) as u8;
    let g = ((rgba >> 16) & 0xFF) as u8;
    let b = ((rgba >> 8) & 0xFF) as u8;
    let a = (rgba & 0xFF) as u8;
    (r, g, b, a)
}

/// Convert a Real color value to 0-255 range.
fn real_to_color(r: f64) -> i64 {
    if r > 1.0 {
        r.clamp(0.0, 255.0) as i64
    } else {
        (r * 255.0).clamp(0.0, 255.0) as i64
    }
}

/// Pop a number from the stack as f64.
fn pop_number(ctx: &mut ExecuteContext) -> Result<f64, String> {
    match ctx.pop() {
        Ok(Value::Real(r)) => Ok(r),
        Ok(Value::Integer(i)) => Ok(i as f64),
        Ok(_) => Err("Expected number".to_string()),
        Err(e) => Err(e),
    }
}

/// Pop a color component (handles 0-1 or 0-255 range).
fn pop_color(ctx: &mut ExecuteContext) -> Result<u8, String> {
    match ctx.pop() {
        Ok(Value::Real(r)) => Ok(real_to_color(r) as u8),
        Ok(Value::Integer(i)) => Ok(i.clamp(0, 255) as u8),
        Ok(_) => Err("Expected number".to_string()),
        Err(e) => Err(e),
    }
}

/// Plot library for RPL.
pub struct PlotLib;

// Command IDs
mod cmd {
    pub const BEGINPLOT: u16 = 0;
    pub const ENDPLOT: u16 = 1;
    pub const MOVETO: u16 = 2;
    pub const LINETO: u16 = 3;
    pub const CIRCLE: u16 = 4;
    pub const RECT: u16 = 5;
    pub const ELLIPSE: u16 = 6;
    pub const ARC: u16 = 7;
    pub const BEZIER: u16 = 8;
    pub const PIXEL: u16 = 9;
    pub const TEXT: u16 = 10;
    pub const FILL: u16 = 11;
    pub const STROKE: u16 = 12;
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
    pub const CLIP: u16 = 24;
    pub const RGBA: u16 = 25;
}

impl Library for PlotLib {
    fn id(&self) -> LibId {
        PLOT_LIB_ID
    }

    fn name(&self) -> &'static str {
        "Plot"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("BEGINPLOT", PLOT_LIB_ID, cmd::BEGINPLOT, 0, 0),
            CommandInfo::with_effect("ENDPLOT", PLOT_LIB_ID, cmd::ENDPLOT, 0, 1),
            CommandInfo::with_effect("MOVETO", PLOT_LIB_ID, cmd::MOVETO, 2, 0),
            CommandInfo::with_effect("LINETO", PLOT_LIB_ID, cmd::LINETO, 2, 0),
            CommandInfo::with_effect("CIRCLE", PLOT_LIB_ID, cmd::CIRCLE, 3, 0),
            CommandInfo::with_effect("RECT", PLOT_LIB_ID, cmd::RECT, 4, 0),
            CommandInfo::with_effect("ELLIPSE", PLOT_LIB_ID, cmd::ELLIPSE, 4, 0),
            CommandInfo::with_effect("ARC", PLOT_LIB_ID, cmd::ARC, 5, 0),
            CommandInfo::with_effect("BEZIER", PLOT_LIB_ID, cmd::BEZIER, 6, 0),
            CommandInfo::with_effect("PIXEL", PLOT_LIB_ID, cmd::PIXEL, 2, 0),
            CommandInfo::with_effect("TEXT", PLOT_LIB_ID, cmd::TEXT, 3, 0),
            CommandInfo::with_effect("FILL", PLOT_LIB_ID, cmd::FILL, 0, 0),
            CommandInfo::with_effect("STROKE", PLOT_LIB_ID, cmd::STROKE, 0, 0),
            CommandInfo::with_effect("LINEWIDTH", PLOT_LIB_ID, cmd::LINEWIDTH, 1, 0),
            CommandInfo::with_effect("COLOR", PLOT_LIB_ID, cmd::COLOR, 1, 0),
            CommandInfo::with_effect("FILLCOLOR", PLOT_LIB_ID, cmd::FILLCOLOR, 1, 0),
            CommandInfo::with_effect("FONT", PLOT_LIB_ID, cmd::FONT, 2, 0),
            CommandInfo::with_effect("IDENTITY", PLOT_LIB_ID, cmd::IDENTITY, 0, 0),
            CommandInfo::with_effect("TRANSFORM", PLOT_LIB_ID, cmd::TRANSFORM, 6, 0),
            CommandInfo::with_effect("SCALE", PLOT_LIB_ID, cmd::SCALE, 2, 0),
            CommandInfo::with_effect("ROTATE", PLOT_LIB_ID, cmd::ROTATE, 1, 0),
            CommandInfo::with_effect("TRANSLATE", PLOT_LIB_ID, cmd::TRANSLATE, 2, 0),
            CommandInfo::with_effect("PUSHSTATE", PLOT_LIB_ID, cmd::PUSHSTATE, 0, 0),
            CommandInfo::with_effect("POPSTATE", PLOT_LIB_ID, cmd::POPSTATE, 0, 0),
            CommandInfo::with_effect("CLIP", PLOT_LIB_ID, cmd::CLIP, 0, 0),
            CommandInfo::with_effect("RGBA", PLOT_LIB_ID, cmd::RGBA, 4, 1),
        ]
    }
}

impl LibraryLowerer for PlotLib {
    fn lower_composite(
        &self,
        _id: u16,
        _branches: &[rpl::ir::Branch],
        _span: rpl::Span,
        _ctx: &mut rpl::lower::LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "Plot library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd_id: u16,
        _span: rpl::Span,
        ctx: &mut rpl::lower::LowerContext,
    ) -> Result<StackEffect, LowerError> {
        ctx.output.emit_call_lib(PLOT_LIB_ID, cmd_id);
        Ok(match cmd_id {
            cmd::BEGINPLOT => StackEffect::fixed(0, &[]),
            cmd::ENDPLOT => StackEffect::fixed(0, &[None]),
            cmd::MOVETO | cmd::LINETO | cmd::PIXEL => StackEffect::fixed(2, &[]),
            cmd::CIRCLE => StackEffect::fixed(3, &[]),
            cmd::RECT | cmd::ELLIPSE => StackEffect::fixed(4, &[]),
            cmd::ARC => StackEffect::fixed(5, &[]),
            cmd::BEZIER | cmd::TRANSFORM => StackEffect::fixed(6, &[]),
            cmd::TEXT => StackEffect::fixed(3, &[]),
            cmd::FILL | cmd::STROKE | cmd::IDENTITY | cmd::PUSHSTATE | cmd::POPSTATE | cmd::CLIP => {
                StackEffect::fixed(0, &[])
            }
            cmd::LINEWIDTH | cmd::ROTATE | cmd::COLOR | cmd::FILLCOLOR => StackEffect::fixed(1, &[]),
            cmd::FONT | cmd::SCALE | cmd::TRANSLATE => StackEffect::fixed(2, &[]),
            cmd::RGBA => StackEffect::fixed(4, &[Some(TypeId::BINT)]),
            _ => StackEffect::Dynamic,
        })
    }
}

impl LibraryExecutor for PlotLib {
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::BEGINPLOT => {
                set_plot_state(PlotState::new())?;
                Ok(())
            }

            cmd::ENDPLOT => {
                let state = take_plot_state()?;
                let bytes = state.finalize();
                ctx.push(Value::Bytes(Arc::from(bytes.into_boxed_slice())))?;
                Ok(())
            }

            cmd::MOVETO => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_MOVETO);
                    state.push_coord(x);
                    state.push_coord(y);
                })?;
                Ok(())
            }

            cmd::LINETO => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_LINETO);
                    state.push_coord(x);
                    state.push_coord(y);
                })?;
                Ok(())
            }

            cmd::CIRCLE => {
                let r = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_CIRCLE);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(r);
                })?;
                Ok(())
            }

            cmd::RECT => {
                let h = pop_number(ctx)?;
                let w = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_RECT);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(w);
                    state.push_coord(h);
                })?;
                Ok(())
            }

            cmd::ELLIPSE => {
                let ry = pop_number(ctx)?;
                let rx = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_ELLIPSE);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(rx);
                    state.push_coord(ry);
                })?;
                Ok(())
            }

            cmd::ARC => {
                let end_angle = pop_number(ctx)?;
                let start_angle = pop_number(ctx)?;
                let r = pop_number(ctx)?;
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_ARC);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(r);
                    state.push_coord(start_angle);
                    state.push_coord(end_angle);
                })?;
                Ok(())
            }

            cmd::BEZIER => {
                let y3 = pop_number(ctx)?;
                let x3 = pop_number(ctx)?;
                let y2 = pop_number(ctx)?;
                let x2 = pop_number(ctx)?;
                let y1 = pop_number(ctx)?;
                let x1 = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_BEZIER);
                    state.push_coord(x1);
                    state.push_coord(y1);
                    state.push_coord(x2);
                    state.push_coord(y2);
                    state.push_coord(x3);
                    state.push_coord(y3);
                })?;
                Ok(())
            }

            cmd::PIXEL => {
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_PIXEL);
                    state.push_coord(x);
                    state.push_coord(y);
                })?;
                Ok(())
            }

            cmd::TEXT => {
                let text = match ctx.pop() {
                    Ok(Value::String(s)) => s.to_string(),
                    Ok(_) => return Err("TEXT: expected string".to_string()),
                    Err(e) => return Err(e),
                };
                let y = pop_number(ctx)?;
                let x = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_TEXT);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_string(&text);
                })?;
                Ok(())
            }

            cmd::FILL => {
                with_plot_state(|state| state.push_byte(CMD_FILL))?;
                Ok(())
            }

            cmd::STROKE => {
                with_plot_state(|state| state.push_byte(CMD_STROKE))?;
                Ok(())
            }

            cmd::LINEWIDTH => {
                let w = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_LINEWIDTH);
                    state.push_coord(w);
                })?;
                Ok(())
            }

            cmd::COLOR => {
                let rgba = match ctx.pop() {
                    Ok(Value::Integer(i)) => i as u32,
                    Ok(_) => return Err("COLOR: expected packed RGBA integer".to_string()),
                    Err(e) => return Err(e),
                };
                let (r, g, b, a) = unpack_rgba(rgba);
                with_plot_state(|state| {
                    state.push_byte(CMD_COLOR);
                    state.push_number(r as i64);
                    state.push_number(g as i64);
                    state.push_number(b as i64);
                    state.push_number(a as i64);
                })?;
                Ok(())
            }

            cmd::FILLCOLOR => {
                let rgba = match ctx.pop() {
                    Ok(Value::Integer(i)) => i as u32,
                    Ok(_) => return Err("FILLCOLOR: expected packed RGBA integer".to_string()),
                    Err(e) => return Err(e),
                };
                let (r, g, b, a) = unpack_rgba(rgba);
                with_plot_state(|state| {
                    state.push_byte(CMD_FILLCOLOR);
                    state.push_number(r as i64);
                    state.push_number(g as i64);
                    state.push_number(b as i64);
                    state.push_number(a as i64);
                })?;
                Ok(())
            }

            cmd::FONT => {
                let name = match ctx.pop() {
                    Ok(Value::String(s)) => s.to_string(),
                    Ok(_) => return Err("FONT: expected string".to_string()),
                    Err(e) => return Err(e),
                };
                let size = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_FONT);
                    state.push_coord(size);
                    state.push_string(&name);
                })?;
                Ok(())
            }

            cmd::IDENTITY => {
                with_plot_state(|state| state.push_byte(CMD_IDENTITY))?;
                Ok(())
            }

            cmd::TRANSFORM => {
                let f = pop_number(ctx)?;
                let e = pop_number(ctx)?;
                let d = pop_number(ctx)?;
                let c = pop_number(ctx)?;
                let b = pop_number(ctx)?;
                let a = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_TRANSFORM);
                    state.push_coord(a);
                    state.push_coord(b);
                    state.push_coord(c);
                    state.push_coord(d);
                    state.push_coord(e);
                    state.push_coord(f);
                })?;
                Ok(())
            }

            cmd::SCALE => {
                let sy = pop_number(ctx)?;
                let sx = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_SCALE);
                    state.push_coord(sx);
                    state.push_coord(sy);
                })?;
                Ok(())
            }

            cmd::ROTATE => {
                let angle = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_ROTATE);
                    state.push_coord(angle);
                })?;
                Ok(())
            }

            cmd::TRANSLATE => {
                let dy = pop_number(ctx)?;
                let dx = pop_number(ctx)?;
                with_plot_state(|state| {
                    state.push_byte(CMD_TRANSLATE);
                    state.push_coord(dx);
                    state.push_coord(dy);
                })?;
                Ok(())
            }

            cmd::PUSHSTATE => {
                with_plot_state(|state| state.push_byte(CMD_PUSH))?;
                Ok(())
            }

            cmd::POPSTATE => {
                with_plot_state(|state| state.push_byte(CMD_POP))?;
                Ok(())
            }

            cmd::CLIP => {
                with_plot_state(|state| state.push_byte(CMD_CLIP))?;
                Ok(())
            }

            cmd::RGBA => {
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
    let registry = session.registry_mut();
    let lib = PlotLib;
    for cmd in lib.commands() {
        registry.register_command(cmd.name, cmd.lib_id, cmd.cmd_id);
    }
    registry.register_lowerer(Box::new(PlotLib));
    registry.register_executor(Box::new(PlotLib));
}
