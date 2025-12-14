//! Plot library for scalable vector graphics.
//!
//! Plot objects store drawing commands as a compact byte stream, enabling
//! resolution-independent graphics that can be rendered at any size.

pub mod commands;
pub mod encoding;
pub mod render;

use std::cell::RefCell;

use rpl_core::{TypeId, Word};
use rpl_lang::library::{ExecuteOk, StackEffect};
use rpl_lang::Value;

use commands::*;
use encoding::*;

/// Convert a Real color value to 0-255 range.
/// Values > 1.0 are treated as direct 0-255 values.
/// Values 0.0-1.0 are treated as normalized (multiplied by 255).
fn real_to_color(r: f64) -> i64 {
    if r > 1.0 {
        r.clamp(0.0, 255.0) as i64
    } else {
        (r * 255.0).clamp(0.0, 255.0) as i64
    }
}

/// Pack RGBA components into a 32-bit integer (0xRRGGBBAA format).
pub fn pack_rgba(r: u8, g: u8, b: u8, a: u8) -> u32 {
    ((r as u32) << 24) | ((g as u32) << 16) | ((b as u32) << 8) | (a as u32)
}

/// Unpack a 32-bit RGBA color into components.
pub fn unpack_rgba(rgba: u32) -> (u8, u8, u8, u8) {
    let r = ((rgba >> 24) & 0xFF) as u8;
    let g = ((rgba >> 16) & 0xFF) as u8;
    let b = ((rgba >> 8) & 0xFF) as u8;
    let a = (rgba & 0xFF) as u8;
    (r, g, b, a)
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

rpl_macros::define_library! {
    pub library PlotLib(88, "Plot");

    commands {
        BEGINPLOT | "beginplot" (0 -> 0) "Begin a new plot" {
            if let Err(e) = set_plot_state(PlotState::new()) {
                return Err(e);
            }
            Ok(ExecuteOk::Ok)
        }

        EDITPLOT | "editplot" (1 -> 0) "Edit an existing plot" {
            let plot = match ctx.pop() {
                Ok(Value::Object { type_id, data }) if type_id == TypeId::PLOT => {
                    words_to_bytes(&data)
                }
                Ok(_) => return Err("EDITPLOT: expected Plot object".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let bytes = if plot.last() == Some(&CMD_END) {
                plot[..plot.len() - 1].to_vec()
            } else {
                plot
            };

            if let Err(e) = set_plot_state(PlotState::from_bytes(bytes)) {
                return Err(e);
            }
            Ok(ExecuteOk::Ok)
        }

        ENDPLOT | "endplot" (0 -> 1) "End plot and push to stack" {
            let state = match take_plot_state() {
                Ok(s) => s,
                Err(e) => return Err(e),
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
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        MOVETO | "moveto" (2 -> 0) "Move to position" {
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("MOVETO: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("MOVETO: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_MOVETO);
                state.push_coord(x);
                state.push_coord(y);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        LINETO | "lineto" (2 -> 0) "Draw line to position" {
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("LINETO: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("LINETO: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_LINETO);
                state.push_coord(x);
                state.push_coord(y);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        CIRCLE | "circle" (3 -> 0) "Draw circle" {
            let r = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("CIRCLE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("CIRCLE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("CIRCLE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_CIRCLE);
                state.push_coord(x);
                state.push_coord(y);
                state.push_coord(r);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        RECT | "rect" (4 -> 0) "Draw rectangle" {
            let h = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("RECT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let w = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("RECT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("RECT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("RECT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_RECT);
                state.push_coord(x);
                state.push_coord(y);
                state.push_coord(w);
                state.push_coord(h);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        ELLIPSE | "ellipse" (4 -> 0) "Draw ellipse" {
            let ry = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ELLIPSE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let rx = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ELLIPSE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ELLIPSE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ELLIPSE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_ELLIPSE);
                state.push_coord(x);
                state.push_coord(y);
                state.push_coord(rx);
                state.push_coord(ry);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        ARC | "arc" (5 -> 0) "Draw arc" {
            let end_angle = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ARC: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let start_angle = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ARC: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let r = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ARC: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ARC: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ARC: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_ARC);
                state.push_coord(x);
                state.push_coord(y);
                state.push_coord(r);
                state.push_coord(start_angle);
                state.push_coord(end_angle);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        BEZIER | "bezier" (6 -> 0) "Draw bezier curve" {
            let y3 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x3 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y2 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x2 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y1 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x1 = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("BEZIER: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
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
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        PIXEL | "pixel" (2 -> 0) "Draw pixel" {
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("PIXEL: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("PIXEL: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_PIXEL);
                state.push_coord(x);
                state.push_coord(y);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        TEXT | "text" (3 -> 0) "Draw text" {
            let text = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("TEXT: expected string".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let y = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TEXT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let x = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TEXT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_TEXT);
                state.push_coord(x);
                state.push_coord(y);
                state.push_string(&text);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        FILL | "fill" (0 -> 0) "Fill current path" {
            match with_plot_state(|state| {
                state.push_byte(CMD_FILL);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        STROKE | "stroke" (0 -> 0) "Stroke current path" {
            match with_plot_state(|state| {
                state.push_byte(CMD_STROKE);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        LINEWIDTH | "linewidth" (1 -> 0) "Set line width" {
            let w = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("LINEWIDTH: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_LINEWIDTH);
                state.push_coord(w);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        COLOR | "color" (1 -> 0) "Set stroke color (packed RGBA)" {
            let (r, g, b, a) = match ctx.pop() {
                Ok(Value::Int(i)) => unpack_rgba(i as u32),
                Ok(_) => return Err("COLOR: expected packed RGBA integer".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_COLOR);
                state.push_number(r as i64);
                state.push_number(g as i64);
                state.push_number(b as i64);
                state.push_number(a as i64);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        FILLCOLOR | "fillcolor" (1 -> 0) "Set fill color (packed RGBA)" {
            let (r, g, b, a) = match ctx.pop() {
                Ok(Value::Int(i)) => unpack_rgba(i as u32),
                Ok(_) => return Err("FILLCOLOR: expected packed RGBA integer".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_FILLCOLOR);
                state.push_number(r as i64);
                state.push_number(g as i64);
                state.push_number(b as i64);
                state.push_number(a as i64);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        FONT | "font" (2 -> 0) "Set font size and name" {
            let name = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("FONT: expected string".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let size = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("FONT: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_FONT);
                state.push_coord(size);
                state.push_string(&name);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        IDENTITY | "identity" (0 -> 0) "Reset transform to identity" {
            match with_plot_state(|state| {
                state.push_byte(CMD_IDENTITY);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        TRANSFORM | "transform" (6 -> 0) "Apply transform matrix" {
            let f = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let e = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let d = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let c = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let b = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSFORM: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
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
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        SCALE | "scale" (2 -> 0) "Scale transform" {
            let sy = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("SCALE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let sx = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("SCALE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_SCALE);
                state.push_coord(sx);
                state.push_coord(sy);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        ROTATE | "rotate" (1 -> 0) "Rotate transform" {
            let angle = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("ROTATE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_ROTATE);
                state.push_coord(angle);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        TRANSLATE | "translate" (2 -> 0) "Translate transform" {
            let dy = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSLATE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let dx = match ctx.pop() {
                Ok(Value::Real(r)) => r,
                Ok(Value::Int(i)) => i as f64,
                Ok(_) => return Err("TRANSLATE: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match with_plot_state(|state| {
                state.push_byte(CMD_TRANSLATE);
                state.push_coord(dx);
                state.push_coord(dy);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        PUSHSTATE | "pushstate" (0 -> 0) "Push graphics state" {
            match with_plot_state(|state| {
                state.push_byte(CMD_PUSH);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        POPSTATE | "popstate" (0 -> 0) "Pop graphics state" {
            match with_plot_state(|state| {
                state.push_byte(CMD_POP);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        CLIP | "clip" (0 -> 0) "Clip to current path" {
            match with_plot_state(|state| {
                state.push_byte(CMD_CLIP);
            }) {
                Ok(_) => Ok(ExecuteOk::Ok),
                Err(e) => Err(e),
            }
        }

        RGBA | "rgba" (4 -> 1) "Pack RGBA components into color" {
            let a = match ctx.pop() {
                Ok(Value::Real(r)) => real_to_color(r) as u8,
                Ok(Value::Int(i)) => i.clamp(0, 255) as u8,
                Ok(_) => return Err("RGBA: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let b = match ctx.pop() {
                Ok(Value::Real(r)) => real_to_color(r) as u8,
                Ok(Value::Int(i)) => i.clamp(0, 255) as u8,
                Ok(_) => return Err("RGBA: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let g = match ctx.pop() {
                Ok(Value::Real(r)) => real_to_color(r) as u8,
                Ok(Value::Int(i)) => i.clamp(0, 255) as u8,
                Ok(_) => return Err("RGBA: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let r = match ctx.pop() {
                Ok(Value::Real(rv)) => real_to_color(rv) as u8,
                Ok(Value::Int(i)) => i.clamp(0, 255) as u8,
                Ok(_) => return Err("RGBA: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let color = pack_rgba(r, g, b, a);
            if ctx.push(Value::Int(color as i64)).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }
    }

    custom decompile {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::PLOT.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read();

                    let data: Vec<Word> = (0..size).filter_map(|_| ctx.read()).collect();
                    let bytes = words_to_bytes(&data);
                    let cmd_count = count_commands(&bytes);

                    ctx.write(&format!("«Plot: {} cmds»", cmd_count));
                    return rpl_lang::library::DecompileResult::Ok;
                }
                rpl_lang::library::DecompileResult::Unknown
            }
            DecompileMode::Call(cmd) => {
                let name = match cmd {
                    Self::CMD_BEGINPLOT => "BEGINPLOT",
                    Self::CMD_EDITPLOT => "EDITPLOT",
                    Self::CMD_ENDPLOT => "ENDPLOT",
                    Self::CMD_MOVETO => "MOVETO",
                    Self::CMD_LINETO => "LINETO",
                    Self::CMD_CIRCLE => "CIRCLE",
                    Self::CMD_RECT => "RECT",
                    Self::CMD_ELLIPSE => "ELLIPSE",
                    Self::CMD_ARC => "ARC",
                    Self::CMD_BEZIER => "BEZIER",
                    Self::CMD_PIXEL => "PIXEL",
                    Self::CMD_TEXT => "TEXT",
                    Self::CMD_FILL => "FILL",
                    Self::CMD_STROKE => "STROKE",
                    Self::CMD_LINEWIDTH => "LINEWIDTH",
                    Self::CMD_COLOR => "COLOR",
                    Self::CMD_FILLCOLOR => "FILLCOLOR",
                    Self::CMD_FONT => "FONT",
                    Self::CMD_IDENTITY => "IDENTITY",
                    Self::CMD_TRANSFORM => "TRANSFORM",
                    Self::CMD_SCALE => "SCALE",
                    Self::CMD_ROTATE => "ROTATE",
                    Self::CMD_TRANSLATE => "TRANSLATE",
                    Self::CMD_PUSHSTATE => "PUSHSTATE",
                    Self::CMD_POPSTATE => "POPSTATE",
                    Self::CMD_CLIP => "CLIP",
                    Self::CMD_RGBA => "RGBA",
                    _ => return rpl_lang::library::DecompileResult::Unknown,
                };
                ctx.write(name);
                rpl_lang::library::DecompileResult::Ok
            }
        }
    }

    custom stack_effect {
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
                consumes: 1,
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
            "RGBA" => StackEffect::Fixed {
                consumes: 4,
                produces: 1,
            },
            _ => StackEffect::Dynamic,
        }
    }
}

/// Library ID for Plot.
pub const PLOT_LIB_ID: rpl_lang::library::LibraryId = PlotLib::ID;

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::compile::OutputBuffer;
    use rpl_lang::library::{CompileContext, CompileResult, ExecuteContext, Library, ProbeContext, ProbeResult};
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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        // ENDPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_ENDPLOT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        let _ = lib.execute(&mut ctx);

        // 10 20 MOVETO
        vm.push(Value::Real(10.0)).unwrap();
        vm.push(Value::Real(20.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_MOVETO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        // 30 40 LINETO
        vm.push(Value::Real(30.0)).unwrap();
        vm.push(Value::Real(40.0)).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_LINETO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        // STROKE
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_STROKE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        // ENDPLOT
        let mut ctx = make_exec_ctx(&mut vm, PlotLib::CMD_ENDPLOT);
        let _ = lib.execute(&mut ctx);

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
        assert!(matches!(result, Err(_)));
    }
}
