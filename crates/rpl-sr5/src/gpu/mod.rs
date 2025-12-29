//! GPU rendering for SR5.
//!
//! This module provides GPU-based rendering using wgpu:
//! - [`GpuState`]: Surface management and final compositing
//! - [`GpuTileRenderer`]: Tile layer rendering with transforms
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
//! │   Tile Layers   │───▶│  GpuTileRenderer │───▶│  Output Texture │
//! │(with transforms)|    │  (WGSL shader)   │    │   (RGBA8)       │
//! └─────────────────┘    └──────────────────┘    └────────┬────────┘
//!                                                         │
//!                                                         ▼
//! ┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
//! │  CPU Sprites    │───▶│  GpuState (blit) │───▶│  Screen Surface │
//! │  (framebuffer)  │    │  (alpha blend)   │    │                 │
//! └─────────────────┘    └──────────────────┘    └─────────────────┘
//! ```
//!
//! # Tile Layer Features
//!
//! - **Scale transforms**: Layers can be scaled for parallax depth effects
//!   (e.g., distant stars appear smaller at 0.5x scale)
//! - **Rotation transforms**: Mode 7-style rotation effects
//! - **Wrap mode**: Seamless tiling for infinite scrolling backgrounds
//! - **Multi-layer compositing**: Up to 4 independent tile layers
//!
//! # Shader Pipeline
//!
//! The tile layer shader performs inverse transforms to map screen pixels
//! back to tilemap coordinates:
//!
//! 1. Screen position → centered coordinates
//! 2. Apply inverse rotation
//! 3. Apply inverse scale
//! 4. Add scroll offset → world position
//! 5. Calculate tile index from tilemap
//! 6. Sample tilesheet texture
//!
//! # Usage
//!
//! Tile layers are configured via RPL commands:
//! - `LAYER`: Assign tilemap and tilesheet to a layer
//! - `SCROLL`: Set layer scroll offset
//! - `LSCALE`: Set layer scale (0.5 = half size, 2.0 = double)
//! - `LROT`: Set layer rotation in degrees
//! - `LWRAP`: Enable/disable seamless wrapping

mod state;
mod tile_renderer;

pub use state::GpuState;
pub use tile_renderer::GpuTileRenderer;
