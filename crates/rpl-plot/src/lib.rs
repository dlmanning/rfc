//! # rpl-plot - Scalable Vector Graphics for RPL
//!
//! This crate provides a compact byte stream format for storing
//! resolution-independent vector graphics commands.
//!
//! ## Overview
//!
//! - **Building plots**: Use the RPL commands (`BEGINPLOT`, `CIRCLE`, etc.)
//! - **Rendering plots**: Implement [`PlotRenderer`] for your target
//! - **Format**: Commands are encoded as single bytes followed by
//!   variable-length operands in Q16.16 fixed-point format
//!
//! ## Plot Format
//!
//! A plot is a sequence of drawing commands encoded as bytes:
//!
//! | Command |  Byte  | Arguments  |
//! |---------|--------|------------|
//! | MOVETO  | `0x6D` | x, y       |
//! | LINETO  | `0x6C` | x, y       |
//! | CIRCLE  | `0x63` | x, y, r    |
//! | RECT    | `0x72` | x, y, w, h |
//! | FILL    | `0x66` | (none)     |
//! | STROKE  | `0x73` | (none)     |
//! | END     | `0x7E` | (none)     |
//!
//! Coordinates use Q16.16 fixed-point encoding for precision.
//!
//! ## Example
//!
//! ```ignore
//! use rpl_plot::{render_plot, PlotRenderer, Color};
//!
//! // Implement PlotRenderer for your target
//! struct MyRenderer { /* ... */ }
//!
//! impl PlotRenderer for MyRenderer {
//!     fn circle(&mut self, cx: f64, cy: f64, r: f64) {
//!         // Draw circle at (cx, cy) with radius r
//!     }
//!     // ... implement other methods
//! }
//!
//! // Render plot data
//! let plot_bytes: &[u8] = /* ... */;
//! let mut renderer = MyRenderer::new();
//! render_plot(plot_bytes, &mut renderer);
//! ```

pub mod color;
pub mod commands;
pub mod encoding;
pub mod error;
pub mod library;
pub mod render;

pub use color::Color;
pub use commands::*;
pub use encoding::{
    decode_number, decode_string, encode_number, encode_string, from_fixed_point, to_fixed_point,
};
pub use error::{DecodeError, PlotError};
pub use library::{PLOT_LIB_ID, PlotLib, register_plot_lib};
pub use render::{PlotRenderer, render_plot};
