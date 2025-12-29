//! GPU tile layer renderer implementation.
//!
//! This module contains the [`GpuTileRenderer`] which renders tile layers
//! to an offscreen texture using a WGSL fragment shader.
//!
//! # Resource Management
//!
//! The renderer caches GPU resources for tilesheets and tilemaps:
//! - **Tilesheet textures**: RGBA8 textures uploaded from PNG tile graphics
//! - **Tilemap buffers**: Storage buffers containing tile indices
//! - **Layer uniforms**: Per-layer transform and configuration data
//!
//! Resources are synced lazily when layers are rendered.

use std::sync::{Arc, Mutex};
use wgpu::util::DeviceExt;

use crate::hardware::{NUM_TILE_LAYERS, Sr5Hardware};

/// Uniform buffer layout for a single tile layer.
///
/// This struct is uploaded to the GPU and accessed by the tile layer shader.
/// It contains all per-layer configuration including transforms.
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct LayerUniforms {
    /// Screen dimensions in pixels
    screen_size: [f32; 2],
    /// Scroll offset in pixels
    scroll: [f32; 2],
    /// Scale factors
    scale: [f32; 2],
    /// Rotation in radians
    rotation: f32,
    /// Wrap mode (0 = clamp, 1 = wrap)
    wrap: u32,
    /// Tile size in pixels (8 or 16)
    tile_size: f32,
    /// Tilemap dimensions
    map_width: u32,
    map_height: u32,
    /// Tilesheet grid dimensions
    sheet_cols: u32,
    sheet_rows: u32,
    /// Layer visibility
    visible: u32,
}

/// GPU-accelerated tile layer renderer.
///
/// Renders up to 4 tile layers to an offscreen texture with support for
/// scroll, scale, rotation, and wrap transforms. The output texture is
/// then composited with the CPU-rendered sprite framebuffer.
///
/// # Example
///
/// ```ignore
/// // Create renderer with wgpu device
/// let renderer = GpuTileRenderer::new(&device);
///
/// // Each frame, render tile layers
/// renderer.render(&device, &queue, &hardware);
///
/// // Use renderer.output_view for compositing
/// ```
pub struct GpuTileRenderer {
    /// Render pipeline for tile layers
    pipeline: wgpu::RenderPipeline,
    /// Texture sampler
    sampler: wgpu::Sampler,
    /// Vertex buffer (fullscreen quad)
    vertex_buffer: wgpu::Buffer,
    /// Output texture (layers rendered here, kept alive for view)
    _output_texture: wgpu::Texture,
    /// Output texture view
    pub output_view: wgpu::TextureView,
    /// Bind group layout for per-layer resources
    layer_bind_group_layout: wgpu::BindGroupLayout,
    /// Cached tilesheet textures (indexed by tilesheet ID)
    tilesheet_textures: Vec<Option<(wgpu::Texture, wgpu::TextureView)>>,
    /// Tilesheet bank generation when last synced
    tilesheet_generations: Vec<Option<u32>>,
    /// Cached tilemap buffers (indexed by tilemap ID)
    tilemap_buffers: Vec<Option<wgpu::Buffer>>,
    /// Tilemap generation when last synced
    tilemap_generations: Vec<Option<u32>>,
    /// Per-layer uniform buffers
    layer_uniform_buffers: [wgpu::Buffer; NUM_TILE_LAYERS],
    /// Per-layer bind groups (created when layer is configured)
    layer_bind_groups: [Option<wgpu::BindGroup>; NUM_TILE_LAYERS],
    /// (tilesheet_id, tilemap_id) when bind group was last created
    layer_bind_group_keys: [Option<(usize, usize)>; NUM_TILE_LAYERS],
    /// Screen width in pixels
    screen_width: u32,
    /// Screen height in pixels
    screen_height: u32,
}

// Fullscreen quad vertices (position + uv)
#[repr(C)]
#[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
struct Vertex {
    position: [f32; 2],
    uv: [f32; 2],
}

const QUAD_VERTICES: [Vertex; 6] = [
    Vertex {
        position: [-1.0, -1.0],
        uv: [0.0, 1.0],
    },
    Vertex {
        position: [1.0, -1.0],
        uv: [1.0, 1.0],
    },
    Vertex {
        position: [1.0, 1.0],
        uv: [1.0, 0.0],
    },
    Vertex {
        position: [-1.0, -1.0],
        uv: [0.0, 1.0],
    },
    Vertex {
        position: [1.0, 1.0],
        uv: [1.0, 0.0],
    },
    Vertex {
        position: [-1.0, 1.0],
        uv: [0.0, 0.0],
    },
];

const TILE_SHADER: &str = include_str!("shaders/tile.wgsl");

impl GpuTileRenderer {
    /// Create a new GPU tile renderer with the given screen dimensions.
    pub fn new(device: &wgpu::Device, screen_width: u32, screen_height: u32) -> Self {
        // Create shader module
        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Tile Layer Shader"),
            source: wgpu::ShaderSource::Wgsl(TILE_SHADER.into()),
        });

        // Create bind group layout
        let layer_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Tile Layer Bind Group Layout"),
                entries: &[
                    // Uniforms
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                    // Tilesheet texture
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    // Sampler
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                    // Tilemap storage buffer
                    wgpu::BindGroupLayoutEntry {
                        binding: 3,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Storage { read_only: true },
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                ],
            });

        // Create pipeline layout
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Tile Layer Pipeline Layout"),
            bind_group_layouts: &[&layer_bind_group_layout],
            immediate_size: 0,
        });

        // Create render pipeline
        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Tile Layer Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<Vertex>() as wgpu::BufferAddress,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &[
                        wgpu::VertexAttribute {
                            offset: 0,
                            shader_location: 0,
                            format: wgpu::VertexFormat::Float32x2,
                        },
                        wgpu::VertexAttribute {
                            offset: 8,
                            shader_location: 1,
                            format: wgpu::VertexFormat::Float32x2,
                        },
                    ],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        // Create sampler
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Tile Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // Create vertex buffer
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Tile Vertex Buffer"),
            contents: bytemuck::cast_slice(&QUAD_VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        });

        // Create output texture
        let output_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Tile Layer Output"),
            size: wgpu::Extent3d {
                width: screen_width,
                height: screen_height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });

        let output_view = output_texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Create per-layer uniform buffers
        let layer_uniform_buffers = std::array::from_fn(|i| {
            device.create_buffer(&wgpu::BufferDescriptor {
                label: Some(&format!("Layer {} Uniforms", i)),
                size: std::mem::size_of::<LayerUniforms>() as u64,
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            })
        });

        Self {
            pipeline,
            sampler,
            vertex_buffer,
            _output_texture: output_texture,
            output_view,
            layer_bind_group_layout,
            tilesheet_textures: Vec::new(),
            tilesheet_generations: Vec::new(),
            tilemap_buffers: Vec::new(),
            tilemap_generations: Vec::new(),
            layer_uniform_buffers,
            layer_bind_groups: Default::default(),
            layer_bind_group_keys: Default::default(),
            screen_width,
            screen_height,
        }
    }

    /// Ensure tilesheet texture exists and is up to date.
    /// Returns true if the texture was created/updated.
    #[allow(clippy::too_many_arguments)]
    pub fn sync_tilesheet(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        id: usize,
        data: Option<&[u8]>,
        width: u32,
        height: u32,
        bank_generation: u32,
    ) -> bool {
        // Extend vectors if needed
        while self.tilesheet_textures.len() <= id {
            self.tilesheet_textures.push(None);
            self.tilesheet_generations.push(None);
        }

        // Check if already synced at this generation
        if self.tilesheet_generations[id] == Some(bank_generation) {
            return false;
        }

        // Need data to create texture
        let Some(data) = data else {
            return false;
        };

        // Create texture
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some(&format!("Tilesheet {}", id)),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            data,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(width * 4),
                rows_per_image: Some(height),
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        self.tilesheet_textures[id] = Some((texture, view));
        self.tilesheet_generations[id] = Some(bank_generation);
        true
    }

    /// Ensure tilemap buffer exists and is up to date.
    /// Returns true if the buffer was created/updated.
    pub fn sync_tilemap(
        &mut self,
        device: &wgpu::Device,
        id: usize,
        tiles: Option<&[u16]>,
        generation: u32,
    ) -> bool {
        // Extend vectors if needed
        while self.tilemap_buffers.len() <= id {
            self.tilemap_buffers.push(None);
            self.tilemap_generations.push(None);
        }

        // Check if already synced at this generation
        if self.tilemap_generations[id] == Some(generation) {
            return false;
        }

        // Need data to create buffer
        let Some(tiles) = tiles else {
            return false;
        };

        // Convert u16 to u32 for shader compatibility
        let tiles_u32: Vec<u32> = tiles.iter().map(|&t| t as u32).collect();

        let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some(&format!("Tilemap {}", id)),
            contents: bytemuck::cast_slice(&tiles_u32),
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
        });

        self.tilemap_buffers[id] = Some(buffer);
        self.tilemap_generations[id] = Some(generation);
        true
    }

    /// Update bind group for a layer.
    /// Only recreates if tilesheet or tilemap changed.
    pub fn update_layer_bind_group(
        &mut self,
        device: &wgpu::Device,
        layer_idx: usize,
        tilesheet_id: usize,
        tilemap_id: usize,
        force: bool,
    ) {
        // Check if bind group already valid for these resources
        let key = (tilesheet_id, tilemap_id);
        if !force && self.layer_bind_group_keys[layer_idx] == Some(key) {
            return;
        }

        let Some(Some((_, tilesheet_view))) = self.tilesheet_textures.get(tilesheet_id) else {
            return;
        };
        let Some(Some(tilemap_buffer)) = self.tilemap_buffers.get(tilemap_id) else {
            return;
        };

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some(&format!("Layer {} Bind Group", layer_idx)),
            layout: &self.layer_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: self.layer_uniform_buffers[layer_idx].as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(tilesheet_view),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 3,
                    resource: tilemap_buffer.as_entire_binding(),
                },
            ],
        });

        self.layer_bind_groups[layer_idx] = Some(bind_group);
        self.layer_bind_group_keys[layer_idx] = Some(key);
    }

    /// Render all tile layers to the output texture.
    ///
    /// Returns an error if the hardware lock is poisoned.
    pub fn render(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        hardware: &Arc<Mutex<Sr5Hardware>>,
    ) -> Result<(), String> {
        // Collect layer data while holding the lock briefly
        // Only clone data when generation has changed
        let layer_data: Vec<_> = {
            let hw = hardware.lock().map_err(|_| "Hardware lock poisoned")?;
            let sheet_bank_gen = hw.tilesheets.generation;

            (0..NUM_TILE_LAYERS)
                .filter_map(|layer_idx| {
                    let layer = &hw.layers[layer_idx];
                    let tilemap_id = layer.tilemap_id?;
                    let tilesheet_id = layer.tilesheet_id?;
                    let sheet = hw.tilesheets.get(tilesheet_id)?;
                    let map = hw.tilemaps.get(tilemap_id)?;

                    // Check if we need to sync tilesheet (bank generation changed)
                    let need_sheet_sync = self
                        .tilesheet_generations
                        .get(tilesheet_id)
                        .is_none_or(|g| *g != Some(sheet_bank_gen));

                    // Check if we need to sync tilemap (map generation changed)
                    let need_map_sync = self
                        .tilemap_generations
                        .get(tilemap_id)
                        .is_none_or(|g| *g != Some(map.generation));

                    Some((
                        layer_idx,
                        tilemap_id,
                        tilesheet_id,
                        // Only clone if needed
                        if need_sheet_sync {
                            Some(sheet.data.clone())
                        } else {
                            None
                        },
                        sheet.width,
                        sheet.height,
                        sheet.tile_size,
                        sheet.cols,
                        sheet.rows,
                        sheet_bank_gen,
                        // Only clone if needed
                        if need_map_sync {
                            Some(map.tiles.clone())
                        } else {
                            None
                        },
                        map.width,
                        map.height,
                        map.generation,
                        // Layer config (Copy types)
                        layer.scroll_x,
                        layer.scroll_y,
                        layer.scale_x,
                        layer.scale_y,
                        layer.rotation,
                        layer.wrap,
                        layer.visible,
                    ))
                })
                .collect()
        };
        // Lock released here

        // Sync resources and update uniforms (lock not held)
        for (
            layer_idx,
            tilemap_id,
            tilesheet_id,
            sheet_data,
            sheet_width,
            sheet_height,
            tile_size,
            sheet_cols,
            sheet_rows,
            sheet_bank_gen,
            map_tiles,
            map_width,
            map_height,
            map_generation,
            scroll_x,
            scroll_y,
            scale_x,
            scale_y,
            rotation,
            wrap,
            visible,
        ) in layer_data
        {
            let sheet_updated = self.sync_tilesheet(
                device,
                queue,
                tilesheet_id,
                sheet_data.as_deref(),
                sheet_width as u32,
                sheet_height as u32,
                sheet_bank_gen,
            );

            let map_updated =
                self.sync_tilemap(device, tilemap_id, map_tiles.as_deref(), map_generation);

            let uniforms = LayerUniforms {
                screen_size: [self.screen_width as f32, self.screen_height as f32],
                scroll: [scroll_x as f32, scroll_y as f32],
                scale: [scale_x, scale_y],
                rotation: rotation.to_radians(),
                wrap: if wrap { 1 } else { 0 },
                tile_size: tile_size as f32,
                map_width: map_width as u32,
                map_height: map_height as u32,
                sheet_cols: sheet_cols as u32,
                sheet_rows: sheet_rows as u32,
                visible: if visible { 1 } else { 0 },
            };

            queue.write_buffer(
                &self.layer_uniform_buffers[layer_idx],
                0,
                bytemuck::bytes_of(&uniforms),
            );

            // Only recreate bind group if resources changed
            let force_bind_group = sheet_updated || map_updated;
            self.update_layer_bind_group(
                device,
                layer_idx,
                tilesheet_id,
                tilemap_id,
                force_bind_group,
            );
        }

        // Create command encoder
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Tile Layer Encoder"),
        });

        // Render pass
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Tile Layer Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &self.output_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });

            render_pass.set_pipeline(&self.pipeline);
            render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));

            // Render each layer (back to front)
            for layer_idx in 0..NUM_TILE_LAYERS {
                if let Some(bind_group) = &self.layer_bind_groups[layer_idx] {
                    render_pass.set_bind_group(0, bind_group, &[]);
                    render_pass.draw(0..6, 0..1);
                }
            }
        }

        queue.submit(std::iter::once(encoder.finish()));

        Ok(())
    }
}
