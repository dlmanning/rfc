//! Plot library for scalable vector graphics.
//!
//! Plot objects store drawing commands as a compact byte stream, enabling
//! resolution-independent graphics that can be rendered at any size.

pub mod commands;
pub mod encoding;

use std::cell::RefCell;

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::{TypeId, Word};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

use commands::*;
use encoding::*;

/// Library ID for Plot.
pub const PLOT_LIB_ID: LibraryId = LibraryId::new(88);

/// Plot library implementation.
pub struct PlotLib;

impl PlotLib {
    // Command IDs for RPL commands
    const CMD_BEGINPLOT: u16 = 0;
    const CMD_EDITPLOT: u16 = 1;
    const CMD_ENDPLOT: u16 = 2;
    const CMD_MOVETO: u16 = 3;
    const CMD_LINETO: u16 = 4;
    const CMD_CIRCLE: u16 = 5;
    const CMD_RECT: u16 = 6;
    const CMD_ELLIPSE: u16 = 7;
    const CMD_ARC: u16 = 8;
    const CMD_BEZIER: u16 = 9;
    const CMD_PIXEL: u16 = 10;
    const CMD_TEXT: u16 = 11;
    const CMD_FILL: u16 = 12;
    const CMD_STROKE: u16 = 13;
    const CMD_LINEWIDTH: u16 = 14;
    const CMD_COLOR: u16 = 15;
    const CMD_FILLCOLOR: u16 = 16;
    const CMD_FONT: u16 = 17;
    const CMD_IDENTITY: u16 = 18;
    const CMD_TRANSFORM: u16 = 19;
    const CMD_SCALE: u16 = 20;
    const CMD_ROTATE: u16 = 21;
    const CMD_TRANSLATE: u16 = 22;
    const CMD_PUSHSTATE: u16 = 23;
    const CMD_POPSTATE: u16 = 24;
    const CMD_CLIP: u16 = 25;

    /// Get command ID from name (case-insensitive).
    fn command_id(name: &str) -> Option<u16> {
        match name.to_ascii_uppercase().as_str() {
            "BEGINPLOT" => Some(Self::CMD_BEGINPLOT),
            "EDITPLOT" => Some(Self::CMD_EDITPLOT),
            "ENDPLOT" => Some(Self::CMD_ENDPLOT),
            "MOVETO" => Some(Self::CMD_MOVETO),
            "LINETO" => Some(Self::CMD_LINETO),
            "CIRCLE" => Some(Self::CMD_CIRCLE),
            "RECT" => Some(Self::CMD_RECT),
            "ELLIPSE" => Some(Self::CMD_ELLIPSE),
            "ARC" => Some(Self::CMD_ARC),
            "BEZIER" => Some(Self::CMD_BEZIER),
            "PIXEL" => Some(Self::CMD_PIXEL),
            "TEXT" => Some(Self::CMD_TEXT),
            "FILL" => Some(Self::CMD_FILL),
            "STROKE" => Some(Self::CMD_STROKE),
            "LINEWIDTH" => Some(Self::CMD_LINEWIDTH),
            "COLOR" => Some(Self::CMD_COLOR),
            "FILLCOLOR" => Some(Self::CMD_FILLCOLOR),
            "FONT" => Some(Self::CMD_FONT),
            "IDENTITY" => Some(Self::CMD_IDENTITY),
            "TRANSFORM" => Some(Self::CMD_TRANSFORM),
            "SCALE" => Some(Self::CMD_SCALE),
            "ROTATE" => Some(Self::CMD_ROTATE),
            "TRANSLATE" => Some(Self::CMD_TRANSLATE),
            "PUSHSTATE" => Some(Self::CMD_PUSHSTATE),
            "POPSTATE" => Some(Self::CMD_POPSTATE),
            "CLIP" => Some(Self::CMD_CLIP),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_BEGINPLOT => Some("BEGINPLOT"),
            Self::CMD_EDITPLOT => Some("EDITPLOT"),
            Self::CMD_ENDPLOT => Some("ENDPLOT"),
            Self::CMD_MOVETO => Some("MOVETO"),
            Self::CMD_LINETO => Some("LINETO"),
            Self::CMD_CIRCLE => Some("CIRCLE"),
            Self::CMD_RECT => Some("RECT"),
            Self::CMD_ELLIPSE => Some("ELLIPSE"),
            Self::CMD_ARC => Some("ARC"),
            Self::CMD_BEZIER => Some("BEZIER"),
            Self::CMD_PIXEL => Some("PIXEL"),
            Self::CMD_TEXT => Some("TEXT"),
            Self::CMD_FILL => Some("FILL"),
            Self::CMD_STROKE => Some("STROKE"),
            Self::CMD_LINEWIDTH => Some("LINEWIDTH"),
            Self::CMD_COLOR => Some("COLOR"),
            Self::CMD_FILLCOLOR => Some("FILLCOLOR"),
            Self::CMD_FONT => Some("FONT"),
            Self::CMD_IDENTITY => Some("IDENTITY"),
            Self::CMD_TRANSFORM => Some("TRANSFORM"),
            Self::CMD_SCALE => Some("SCALE"),
            Self::CMD_ROTATE => Some("ROTATE"),
            Self::CMD_TRANSLATE => Some("TRANSLATE"),
            Self::CMD_PUSHSTATE => Some("PUSHSTATE"),
            Self::CMD_POPSTATE => Some("POPSTATE"),
            Self::CMD_CLIP => Some("CLIP"),
            _ => None,
        }
    }
}

/// State for a plot being constructed.
#[derive(Default)]
struct PlotState {
    bytes: Vec<u8>,
}

impl PlotState {
    fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    fn from_bytes(bytes: Vec<u8>) -> Self {
        Self { bytes }
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

impl Library for PlotLib {
    fn id(&self) -> LibraryId {
        PLOT_LIB_ID
    }

    fn name(&self) -> &'static str {
        "Plot"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if Self::command_id(text).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        if let Some(cmd) = Self::command_id(text) {
            ctx.emit_opcode(PLOT_LIB_ID.as_u16(), cmd);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_BEGINPLOT => {
                if let Err(e) = set_plot_state(PlotState::new()) {
                    return ExecuteResult::Error(e);
                }
                ExecuteResult::Ok
            }

            Self::CMD_EDITPLOT => {
                // Pop plot object from stack and start editing
                let plot = match ctx.pop() {
                    Ok(Value::Object { type_id, data }) if type_id == TypeId::PLOT => {
                        words_to_bytes(&data)
                    }
                    Ok(_) => return ExecuteResult::Error("EDITPLOT: expected Plot object".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Remove trailing END marker if present
                let bytes = if plot.last() == Some(&CMD_END) {
                    plot[..plot.len() - 1].to_vec()
                } else {
                    plot
                };

                if let Err(e) = set_plot_state(PlotState::from_bytes(bytes)) {
                    return ExecuteResult::Error(e);
                }
                ExecuteResult::Ok
            }

            Self::CMD_ENDPLOT => {
                let state = match take_plot_state() {
                    Ok(s) => s,
                    Err(e) => return ExecuteResult::Error(e),
                };

                let bytes = state.finalize();
                let data = bytes_to_words(&bytes);

                if ctx
                    .push(Value::Object {
                        type_id: TypeId::PLOT,
                        data,
                    })
                    .is_err()
                {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }

            Self::CMD_MOVETO => {
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("MOVETO: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("MOVETO: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_MOVETO);
                    state.push_coord(x);
                    state.push_coord(y);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_LINETO => {
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("LINETO: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("LINETO: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_LINETO);
                    state.push_coord(x);
                    state.push_coord(y);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_CIRCLE => {
                let r = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("CIRCLE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("CIRCLE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("CIRCLE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_CIRCLE);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(r);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_RECT => {
                let h = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("RECT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let w = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("RECT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("RECT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("RECT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_RECT);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(w);
                    state.push_coord(h);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_ELLIPSE => {
                let ry = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ELLIPSE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let rx = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ELLIPSE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ELLIPSE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ELLIPSE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_ELLIPSE);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(rx);
                    state.push_coord(ry);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_ARC => {
                let end_angle = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ARC: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let start_angle = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ARC: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let r = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ARC: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ARC: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ARC: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_ARC);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_coord(r);
                    state.push_coord(start_angle);
                    state.push_coord(end_angle);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_BEZIER => {
                let y3 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x3 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y2 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x2 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y1 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x1 = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("BEZIER: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_BEZIER);
                    state.push_coord(x1);
                    state.push_coord(y1);
                    state.push_coord(x2);
                    state.push_coord(y2);
                    state.push_coord(x3);
                    state.push_coord(y3);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_PIXEL => {
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("PIXEL: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("PIXEL: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_PIXEL);
                    state.push_coord(x);
                    state.push_coord(y);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_TEXT => {
                let text = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("TEXT: expected string".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let y = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TEXT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TEXT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_TEXT);
                    state.push_coord(x);
                    state.push_coord(y);
                    state.push_string(&text);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_FILL => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_FILL);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_STROKE => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_STROKE);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_LINEWIDTH => {
                let w = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("LINEWIDTH: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_LINEWIDTH);
                    state.push_coord(w);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_COLOR => {
                let a = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("COLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let b = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("COLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let g = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("COLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let r = match ctx.pop() {
                    Ok(Value::Real(rv)) => (rv * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("COLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_COLOR);
                    state.push_number(r);
                    state.push_number(g);
                    state.push_number(b);
                    state.push_number(a);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_FILLCOLOR => {
                let a = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("FILLCOLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let b = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("FILLCOLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let g = match ctx.pop() {
                    Ok(Value::Real(r)) => (r * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("FILLCOLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let r = match ctx.pop() {
                    Ok(Value::Real(rv)) => (rv * 255.0).clamp(0.0, 255.0) as i64,
                    Ok(Value::Int(i)) => i.clamp(0, 255),
                    Ok(_) => return ExecuteResult::Error("FILLCOLOR: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_FILLCOLOR);
                    state.push_number(r);
                    state.push_number(g);
                    state.push_number(b);
                    state.push_number(a);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_FONT => {
                let name = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("FONT: expected string".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let size = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("FONT: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_FONT);
                    state.push_coord(size);
                    state.push_string(&name);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_IDENTITY => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_IDENTITY);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_TRANSFORM => {
                let f = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let e = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let d = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let c = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let b = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSFORM: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_TRANSFORM);
                    state.push_coord(a);
                    state.push_coord(b);
                    state.push_coord(c);
                    state.push_coord(d);
                    state.push_coord(e);
                    state.push_coord(f);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_SCALE => {
                let sy = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("SCALE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let sx = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("SCALE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_SCALE);
                    state.push_coord(sx);
                    state.push_coord(sy);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_ROTATE => {
                let angle = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("ROTATE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_ROTATE);
                    state.push_coord(angle);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_TRANSLATE => {
                let dy = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSLATE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let dx = match ctx.pop() {
                    Ok(Value::Real(r)) => r,
                    Ok(Value::Int(i)) => i as f64,
                    Ok(_) => return ExecuteResult::Error("TRANSLATE: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match with_plot_state(|state| {
                    state.push_byte(CMD_TRANSLATE);
                    state.push_coord(dx);
                    state.push_coord(dy);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_PUSHSTATE => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_PUSH);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_POPSTATE => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_POP);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            Self::CMD_CLIP => {
                match with_plot_state(|state| {
                    state.push_byte(CMD_CLIP);
                }) {
                    Ok(_) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(e),
                }
            }

            _ => ExecuteResult::Error(format!("Unknown plot command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for PLOT prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::PLOT.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read(); // consume prolog

                    // Read the data words and convert to bytes
                    let data: Vec<Word> = (0..size).filter_map(|_| ctx.read()).collect();
                    let bytes = words_to_bytes(&data);
                    let cmd_count = count_commands(&bytes);

                    ctx.write(&format!("«Plot: {} cmds»", cmd_count));
                    return DecompileResult::Ok;
                }
                DecompileResult::Unknown
            }
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token.to_ascii_uppercase().as_str() {
            "BEGINPLOT" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "EDITPLOT" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "ENDPLOT" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
            "MOVETO" | "LINETO" | "PIXEL" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "CIRCLE" => StackEffect::Fixed {
                consumes: 3,
                produces: 0,
            },
            "RECT" | "ELLIPSE" => StackEffect::Fixed {
                consumes: 4,
                produces: 0,
            },
            "ARC" => StackEffect::Fixed {
                consumes: 5,
                produces: 0,
            },
            "BEZIER" => StackEffect::Fixed {
                consumes: 6,
                produces: 0,
            },
            "TEXT" => StackEffect::Fixed {
                consumes: 3,
                produces: 0,
            },
            "FILL" | "STROKE" | "IDENTITY" | "PUSHSTATE" | "POPSTATE" | "CLIP" => {
                StackEffect::Fixed {
                    consumes: 0,
                    produces: 0,
                }
            }
            "LINEWIDTH" | "ROTATE" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "COLOR" | "FILLCOLOR" => StackEffect::Fixed {
                consumes: 4,
                produces: 0,
            },
            "FONT" | "SCALE" | "TRANSLATE" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "TRANSFORM" => StackEffect::Fixed {
                consumes: 6,
                produces: 0,
            },
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::compile::OutputBuffer;
    use rpl_lang::VM;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &'_ mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn probe_beginplot() {
        let interner = Interner::new();
        let lib = PlotLib;
        let ctx = make_probe_ctx("BEGINPLOT", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = PlotLib;
        let ctx = make_probe_ctx("beginplot", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_all_commands() {
        let interner = Interner::new();
        let lib = PlotLib;

        for cmd in &[
            "BEGINPLOT",
            "ENDPLOT",
            "MOVETO",
            "LINETO",
            "CIRCLE",
            "RECT",
            "FILL",
            "STROKE",
        ] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                cmd
            );
        }
    }

    #[test]
    fn compile_beginplot() {
        let lib = PlotLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(9));

        let mut ctx = CompileContext::new(span, "BEGINPLOT", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
    }

    #[test]
    fn execute_beginplot_endplot() {
        // Clear any existing state
        CURRENT_PLOT.with(|cell| *cell.borrow_mut() = None);

        let lib = PlotLib;
        let mut vm = VM::new();

        // BEGINPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_BEGINPLOT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // ENDPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_ENDPLOT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // Should have a plot on the stack
        assert_eq!(vm.depth(), 1);
        let val = vm.pop().unwrap();
        match val {
            Value::Object { type_id, data } => {
                assert_eq!(type_id, TypeId::PLOT);
                // Should contain at least the END marker
                let bytes = words_to_bytes(&data);
                assert!(!bytes.is_empty());
                assert_eq!(bytes.last(), Some(&CMD_END));
            }
            _ => panic!("Expected Plot object"),
        }
    }

    #[test]
    fn execute_drawing_commands() {
        // Clear any existing state
        CURRENT_PLOT.with(|cell| *cell.borrow_mut() = None);

        let lib = PlotLib;
        let mut vm = VM::new();

        // BEGINPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_BEGINPLOT);
        lib.execute(&mut ctx);

        // 10 20 MOVETO
        vm.push(Value::Real(10.0)).unwrap();
        vm.push(Value::Real(20.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_MOVETO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // 30 40 LINETO
        vm.push(Value::Real(30.0)).unwrap();
        vm.push(Value::Real(40.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_LINETO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // STROKE
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_STROKE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // ENDPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_ENDPLOT);
        lib.execute(&mut ctx);

        // Check the result
        let val = vm.pop().unwrap();
        match val {
            Value::Object { type_id, data } => {
                assert_eq!(type_id, TypeId::PLOT);
                let bytes = words_to_bytes(&data);
                // Should have MOVETO, LINETO, STROKE, END
                assert!(bytes.contains(&CMD_MOVETO));
                assert!(bytes.contains(&CMD_LINETO));
                assert!(bytes.contains(&CMD_STROKE));
                assert_eq!(bytes.last(), Some(&CMD_END));
            }
            _ => panic!("Expected Plot object"),
        }
    }

    #[test]
    fn error_without_beginplot() {
        // Clear any existing state
        CURRENT_PLOT.with(|cell| *cell.borrow_mut() = None);

        let lib = PlotLib;
        let mut vm = VM::new();

        vm.push(Value::Real(10.0)).unwrap();
        vm.push(Value::Real(20.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_MOVETO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }
}
