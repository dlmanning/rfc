//! # rpl-vector-plot - Vector Graphics for RPL
//!
//! A typed vector graphics library with scene graph support.
//!
//! ## Features
//!
//! - **Typed representation**: `Plot` struct with `Element`s, not just bytes
//! - **f32 coordinates**: GPU-native, compact storage
//! - **Scene graph**: Groups, element IDs, hierarchical structure
//! - **Incremental bounds**: Bounding box tracked as elements are added
//! - **Round-trip serialization**: `encode` ↔ `decode`
//!
//! ## Architecture
//!
//! ```text
//! RPL Stack          Rust
//! ─────────          ────
//! Blob ◄──── encode ◄──── Plot { elements, bounds }
//!      ────► decode ────►
//! ```
//!
//! On the RPL stack, plots are stored as `Blob`. Internally, commands
//! decode the blob to a typed `Plot`, mutate it, and encode back.

mod builder;
mod decode;
mod element;
mod encode;
mod library;
mod paint;
mod path;
mod plot;
mod render;
mod types;

pub use builder::PlotBuilder;
pub use decode::{DecodeError, decode};
pub use element::{Element, ElementId, ElementKind};
pub use encode::encode;
pub use library::{VECTOR_PLOT_LIB_ID, VectorPlotLib, register_vector_plot_lib};
pub use paint::{GradientStop, Paint, Stroke};
pub use path::{Path, PathCmd};
pub use plot::Plot;
pub use render::{Renderer, render};
pub use types::{Color, Point, Rect, Transform};
