//! RPL library integration.

use std::sync::{Arc, OnceLock};

use rpl::{
    Span,
    interface::InterfaceSpec,
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

use crate::{decode::decode, encode::encode, plot::Plot, types::Color};

/// Interface declaration.
const INTERFACE: &str = include_str!("vector-plot.rpli");

/// Get the interface specification (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid vector-plot interface"))
}

/// Vector plot library ID.
pub const VECTOR_PLOT_LIB_ID: LibId = 88;

// Command IDs
mod cmd {
    // Construction
    pub const VPNEW: u16 = 0;

    // Path commands
    pub const VPM: u16 = 10;
    pub const VPL: u16 = 11;
    pub const VPQ: u16 = 12;
    pub const VPC: u16 = 13;
    pub const VPA: u16 = 14;
    pub const VPZ: u16 = 15;

    // Shapes
    pub const VPCIRCLE: u16 = 20;
    pub const VPRECT: u16 = 21;
    pub const VPELLIPSE: u16 = 22;

    // Finalize
    pub const VPFILL: u16 = 30;
    pub const VPSTROKE: u16 = 31;
    pub const VPFILLSTROKE: u16 = 32;

    // Style
    pub const VPFILLC: u16 = 40;
    pub const VPSTROKEC: u16 = 41;
    pub const VPWIDTH: u16 = 42;

    // Transform
    pub const VPTRANS: u16 = 50;
    pub const VPROTATE: u16 = 51;
    pub const VPSCALE: u16 = 52;
    pub const VPIDENT: u16 = 53;

    // State
    pub const VPSAVE: u16 = 60;
    pub const VPRESTORE: u16 = 61;

    // Groups
    pub const VPGROUP: u16 = 70;
    pub const VPENDGRP: u16 = 71;

    // Text
    pub const VPTEXT: u16 = 80;

    // Queries
    pub const VPBOUNDS: u16 = 90;
    pub const VPCOUNT: u16 = 91;

    // Utility
    pub const VPRGBA: u16 = 99;
}

// ============================================================================
// Helper functions
// ============================================================================

fn pop_blob(ctx: &mut ExecuteContext) -> Result<Vec<u8>, String> {
    match ctx.pop()? {
        Value::Bytes(arc) => Ok(arc.to_vec()),
        other => Err(format!("Expected Blob, got {}", other.type_name())),
    }
}

fn push_blob(ctx: &mut ExecuteContext, bytes: Vec<u8>) -> Result<(), String> {
    ctx.push(Value::Bytes(Arc::from(bytes.into_boxed_slice())))
}

fn pop_number(ctx: &mut ExecuteContext) -> Result<f64, String> {
    match ctx.pop()? {
        Value::Real(r) => Ok(r),
        Value::Integer(i) => Ok(i as f64),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

fn pop_integer(ctx: &mut ExecuteContext) -> Result<i64, String> {
    match ctx.pop()? {
        Value::Integer(i) => Ok(i),
        Value::Real(r) => Ok(r as i64),
        other => Err(format!("Expected integer, got {}", other.type_name())),
    }
}

fn pop_string(ctx: &mut ExecuteContext) -> Result<String, String> {
    match ctx.pop()? {
        Value::String(s) => Ok(s.to_string()),
        other => Err(format!("Expected string, got {}", other.type_name())),
    }
}

fn pop_color(ctx: &mut ExecuteContext) -> Result<u8, String> {
    match ctx.pop()? {
        Value::Real(r) => {
            if r > 1.0 {
                Ok(r.clamp(0.0, 255.0) as u8)
            } else {
                Ok((r * 255.0).clamp(0.0, 255.0) as u8)
            }
        }
        Value::Integer(i) => Ok(i.clamp(0, 255) as u8),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

fn decode_plot(bytes: &[u8]) -> Result<Plot, String> {
    decode(bytes).map_err(|e| e.to_string())
}

fn encode_plot(plot: &Plot) -> Vec<u8> {
    encode(plot)
}

/// Decode, mutate, encode pattern.
macro_rules! with_plot {
    ($ctx:expr, |$plot:ident| $body:block) => {{
        let bytes = pop_blob($ctx)?;
        #[allow(unused_mut)]
        let mut $plot = decode_plot(&bytes)?;
        $body
        push_blob($ctx, encode_plot(&$plot))?;
        Ok(ExecuteAction::ok())
    }};
}

// ============================================================================
// Library Implementation
// ============================================================================

/// Vector plot library for RPL.
#[derive(Clone, Copy)]
pub struct VectorPlotLib;

impl LibraryLowerer for VectorPlotLib {
    fn id(&self) -> LibId {
        VECTOR_PLOT_LIB_ID
    }

    fn lower_command(
        &self,
        cmd_id: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(VECTOR_PLOT_LIB_ID, cmd_id);
        Ok(())
    }
}

impl LibraryExecutor for VectorPlotLib {
    fn id(&self) -> LibId {
        VECTOR_PLOT_LIB_ID
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            // Construction
            cmd::VPNEW => {
                let plot = Plot::new();
                push_blob(ctx, encode_plot(&plot))?;
                Ok(ExecuteAction::ok())
            }

            // Path commands
            cmd::VPM => {
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    // For path commands, we need a builder-based approach
                    // For now, this is a placeholder - full implementation needs
                    // tracking current path state in the Plot
                    let _ = (x, y);
                })
            }

            cmd::VPL => {
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    let _ = (x, y);
                })
            }

            cmd::VPQ => {
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                let cy = pop_number(ctx)? as f32;
                let cx = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    let _ = (cx, cy, x, y);
                })
            }

            cmd::VPC => {
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                let c2y = pop_number(ctx)? as f32;
                let c2x = pop_number(ctx)? as f32;
                let c1y = pop_number(ctx)? as f32;
                let c1x = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    let _ = (c1x, c1y, c2x, c2y, x, y);
                })
            }

            cmd::VPA => {
                let sweep = pop_number(ctx)? as f32;
                let start = pop_number(ctx)? as f32;
                let r = pop_number(ctx)? as f32;
                let cy = pop_number(ctx)? as f32;
                let cx = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    let _ = (cx, cy, r, start, sweep);
                })
            }

            cmd::VPZ => {
                with_plot!(ctx, |plot| {
                    // Close path
                })
            }

            // Shapes - use current fill color from plot
            cmd::VPCIRCLE => {
                let r = pop_number(ctx)? as f32;
                let cy = pop_number(ctx)? as f32;
                let cx = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    plot.add_circle(cx, cy, r);
                })
            }

            cmd::VPRECT => {
                let h = pop_number(ctx)? as f32;
                let w = pop_number(ctx)? as f32;
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    plot.add_rect(x, y, w, h);
                })
            }

            cmd::VPELLIPSE => {
                let ry = pop_number(ctx)? as f32;
                let rx = pop_number(ctx)? as f32;
                let cy = pop_number(ctx)? as f32;
                let cx = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    plot.add_ellipse(cx, cy, rx, ry);
                })
            }

            // Finalize (placeholders for now)
            cmd::VPFILL | cmd::VPSTROKE | cmd::VPFILLSTROKE => {
                with_plot!(ctx, |plot| {
                    // Finalize current path - needs builder state
                })
            }

            // Style - set colors and stroke width
            cmd::VPFILLC => {
                let packed = pop_integer(ctx)? as u32;
                let r = ((packed >> 24) & 0xFF) as u8;
                let g = ((packed >> 16) & 0xFF) as u8;
                let b = ((packed >> 8) & 0xFF) as u8;
                let a = (packed & 0xFF) as u8;
                with_plot!(ctx, |plot| {
                    plot.set_fill_color(Color::new(r, g, b, a));
                })
            }

            cmd::VPSTROKEC => {
                let width = pop_number(ctx)? as f32;
                let packed = pop_integer(ctx)? as u32;
                let r = ((packed >> 24) & 0xFF) as u8;
                let g = ((packed >> 16) & 0xFF) as u8;
                let b = ((packed >> 8) & 0xFF) as u8;
                let a = (packed & 0xFF) as u8;
                with_plot!(ctx, |plot| {
                    plot.set_stroke_color(Color::new(r, g, b, a));
                    plot.set_stroke_width(width);
                })
            }

            cmd::VPWIDTH => {
                let width = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    plot.set_stroke_width(width);
                })
            }

            // Transform (placeholders)
            cmd::VPTRANS | cmd::VPROTATE | cmd::VPSCALE | cmd::VPIDENT => {
                // Pop appropriate args
                if ctx.cmd == cmd::VPTRANS || ctx.cmd == cmd::VPSCALE {
                    let _ = pop_number(ctx)?;
                    let _ = pop_number(ctx)?;
                } else if ctx.cmd == cmd::VPROTATE {
                    let _ = pop_number(ctx)?;
                }
                with_plot!(ctx, |plot| {
                    // Apply transform
                })
            }

            // State
            cmd::VPSAVE | cmd::VPRESTORE => {
                with_plot!(ctx, |plot| {
                    // Save/restore state
                })
            }

            // Groups
            cmd::VPGROUP | cmd::VPENDGRP => {
                with_plot!(ctx, |plot| {
                    // Begin/end group
                })
            }

            // Text
            cmd::VPTEXT => {
                let size = pop_number(ctx)? as f32;
                let text = pop_string(ctx)?;
                let y = pop_number(ctx)? as f32;
                let x = pop_number(ctx)? as f32;
                with_plot!(ctx, |plot| {
                    plot.add_text(x, y, text.clone(), size);
                })
            }

            // Queries
            cmd::VPBOUNDS => {
                let bytes = pop_blob(ctx)?;
                let plot = decode_plot(&bytes)?;
                let b = plot.bounds();
                let values: Vec<Value> = vec![
                    Value::Real(b.x as f64),
                    Value::Real(b.y as f64),
                    Value::Real((b.x + b.w) as f64),
                    Value::Real((b.y + b.h) as f64),
                ];
                let list = Value::List(Arc::from(values.into_boxed_slice()));
                ctx.push(list)?;
                Ok(ExecuteAction::ok())
            }

            cmd::VPCOUNT => {
                let bytes = pop_blob(ctx)?;
                let plot = decode_plot(&bytes)?;
                ctx.push(Value::Integer(plot.len() as i64))?;
                Ok(ExecuteAction::ok())
            }

            // Color utility
            cmd::VPRGBA => {
                let a = pop_color(ctx)?;
                let b = pop_color(ctx)?;
                let g = pop_color(ctx)?;
                let r = pop_color(ctx)?;
                let packed =
                    ((r as u32) << 24) | ((g as u32) << 16) | ((b as u32) << 8) | (a as u32);
                ctx.push(Value::Integer(packed as i64))?;
                Ok(ExecuteAction::ok())
            }

            _ => Err(format!("Unknown vector-plot command: {}", ctx.cmd)),
        }
    }
}

/// Register the vector plot library with a session.
pub fn register_vector_plot_lib(session: &mut rpl::Session) {
    session.interfaces_mut().add(interface().clone());
    session.lowerers_mut().add(VectorPlotLib);
    session.executors_mut().add(VectorPlotLib);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vector_plot_lib_id() {
        assert_eq!(interface().id(), 88);
    }

    #[test]
    fn vector_plot_lib_name() {
        assert_eq!(interface().name(), "VectorPlot");
    }

    #[test]
    fn vector_plot_has_commands() {
        let commands = interface().commands();
        let names: Vec<&str> = commands
            .iter()
            .flat_map(|c| c.names.iter().map(|s| s.as_str()))
            .collect();

        assert!(names.contains(&"NEWPLOT"));
        assert!(names.contains(&"CIRCLE"));
        assert!(names.contains(&"RECT"));
        assert!(names.contains(&"FILL"));
        assert!(names.contains(&"RGBA"));
    }
}
