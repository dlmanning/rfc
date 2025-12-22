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

mod types;
mod path;
mod paint;
mod element;
mod plot;
mod builder;
mod encode;
mod decode;
mod render;
mod library;

pub use types::{Point, Rect, Color, Transform};
pub use path::{Path, PathCmd};
pub use paint::{Paint, Stroke, GradientStop};
pub use element::{Element, ElementKind, ElementId};
pub use plot::Plot;
pub use builder::PlotBuilder;
pub use encode::encode;
pub use decode::{decode, DecodeError};
pub use render::{Renderer, render};
pub use library::{VectorPlotLib, VECTOR_PLOT_LIB_ID, register_vector_plot_lib};
