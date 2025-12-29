//! GPU state management for SR5.
//!
//! Handles wgpu initialization, surface management, and the final compositing
//! pass that blits tile layers and CPU framebuffer to the screen.

use std::sync::Arc;
use winit::window::Window;

use super::GpuTileRenderer;
use crate::console::Console;

/// GPU state for rendering.
///
/// Manages the wgpu device, surface, and compositing pipeline that combines
/// GPU-rendered tile layers with the CPU-rendered sprite framebuffer.
pub struct GpuState {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    config: wgpu::SurfaceConfiguration,
    blit_pipeline: wgpu::RenderPipeline,
    blit_bind_group_layout: wgpu::BindGroupLayout,
    blit_sampler: wgpu::Sampler,
    tile_renderer: GpuTileRenderer,
    /// CPU framebuffer texture (for sprites)
    cpu_texture: wgpu::Texture,
    /// Cached bind group for tile layer output
    tile_blit_bind_group: wgpu::BindGroup,
    /// Cached bind group for CPU framebuffer
    cpu_blit_bind_group: wgpu::BindGroup,
    /// Background texture (static image behind tile layers)
    background_texture: Option<wgpu::Texture>,
    /// Cached bind group for background
    background_bind_group: Option<wgpu::BindGroup>,
    /// Generation when background was last synced
    background_generation: Option<u32>,
    /// Screen width in pixels
    screen_width: u32,
    /// Screen height in pixels
    screen_height: u32,
}

impl GpuState {
    /// Create a new GPU state for the given window and screen dimensions.
    pub fn new(window: Arc<Window>, screen_width: u32, screen_height: u32) -> Result<Self, String> {
        let size = window.inner_size();

        // Create wgpu instance
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        // Create surface
        let surface = instance
            .create_surface(window)
            .map_err(|e| format!("Failed to create surface: {}", e))?;

        // Request adapter
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .map_err(|e| format!("Failed to find GPU adapter: {}", e))?;

        // Request device
        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("SR5 Device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
            memory_hints: Default::default(),
            experimental_features: Default::default(),
            trace: Default::default(),
        }))
        .map_err(|e| format!("Failed to create device: {}", e))?;

        // Configure surface
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps
            .formats
            .iter()
            .find(|f| f.is_srgb())
            .copied()
            .unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Create blit shader for final output
        let blit_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Blit Shader"),
            source: wgpu::ShaderSource::Wgsl(BLIT_SHADER.into()),
        });

        // Bind group layout for blit
        let blit_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Blit Bind Group Layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });

        let blit_pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Blit Pipeline Layout"),
            bind_group_layouts: &[&blit_bind_group_layout],
            immediate_size: 0,
        });

        let blit_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Blit Pipeline"),
            layout: Some(&blit_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &blit_shader,
                entry_point: Some("vs_main"),
                buffers: &[],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &blit_shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: surface_format,
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

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Blit Sampler"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // Create tile renderer
        let tile_renderer = GpuTileRenderer::new(&device, screen_width, screen_height);

        // Create CPU framebuffer texture
        let cpu_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("CPU Framebuffer"),
            size: wgpu::Extent3d {
                width: screen_width,
                height: screen_height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        let cpu_texture_view = cpu_texture.create_view(&wgpu::TextureViewDescriptor::default());

        // Create cached bind groups (these reference stable resources)
        let tile_blit_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Tile Blit Bind Group"),
            layout: &blit_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&tile_renderer.output_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        let cpu_blit_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("CPU Blit Bind Group"),
            layout: &blit_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&cpu_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        Ok(Self {
            surface,
            device,
            queue,
            config,
            blit_pipeline,
            blit_bind_group_layout,
            blit_sampler: sampler,
            tile_renderer,
            cpu_texture,
            tile_blit_bind_group,
            cpu_blit_bind_group,
            background_texture: None,
            background_bind_group: None,
            background_generation: None,
            screen_width,
            screen_height,
        })
    }

    /// Handle window resize.
    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.config.width = new_size.width;
            self.config.height = new_size.height;
            self.surface.configure(&self.device, &self.config);
        }
    }

    /// Sync background texture if changed.
    fn sync_background(&mut self, console: &Console) {
        let hw = match console.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return,
        };

        // Check if background exists and needs sync
        let Some(bg) = &hw.background else {
            // No background - clear cached resources
            if self.background_texture.is_some() {
                self.background_texture = None;
                self.background_bind_group = None;
                self.background_generation = None;
            }
            return;
        };

        // Check if already synced at this generation
        if self.background_generation == Some(bg.generation) {
            return;
        }

        // Create/update texture
        let texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Background"),
            size: wgpu::Extent3d {
                width: bg.width,
                height: bg.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        self.queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &bg.data,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(bg.width * 4),
                rows_per_image: Some(bg.height),
            },
            wgpu::Extent3d {
                width: bg.width,
                height: bg.height,
                depth_or_array_layers: 1,
            },
        );

        let texture_view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Background Blit Bind Group"),
            layout: &self.blit_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&self.blit_sampler),
                },
            ],
        });

        self.background_texture = Some(texture);
        self.background_bind_group = Some(bind_group);
        self.background_generation = Some(bg.generation);
    }

    /// Render a frame.
    ///
    /// Composites GPU tile layers with the CPU framebuffer and presents to screen.
    /// Render order: background → tile layers → CPU framebuffer (sprites).
    pub fn render(&mut self, console: &Console, cpu_frame: &[u8]) -> Result<(), String> {
        // Sync background texture if changed
        self.sync_background(console);

        // Render GPU tile layers
        self.tile_renderer
            .render(&self.device, &self.queue, &console.hardware)?;

        // Upload CPU framebuffer
        self.queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &self.cpu_texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            cpu_frame,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(self.screen_width * 4),
                rows_per_image: Some(self.screen_height),
            },
            wgpu::Extent3d {
                width: self.screen_width,
                height: self.screen_height,
                depth_or_array_layers: 1,
            },
        );

        let output = self
            .surface
            .get_current_texture()
            .map_err(|e| format!("Failed to get surface texture: {}", e))?;
        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Render Encoder"),
            });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Blit Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                    depth_slice: None,
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });

            render_pass.set_pipeline(&self.blit_pipeline);

            // Draw background first (static image behind everything)
            if let Some(bg_bind_group) = &self.background_bind_group {
                render_pass.set_bind_group(0, bg_bind_group, &[]);
                render_pass.draw(0..6, 0..1);
            }

            // Draw tile layers (with alpha blending over background)
            render_pass.set_bind_group(0, &self.tile_blit_bind_group, &[]);
            render_pass.draw(0..6, 0..1);

            // Draw CPU framebuffer on top (sprites with alpha blending)
            render_pass.set_bind_group(0, &self.cpu_blit_bind_group, &[]);
            render_pass.draw(0..6, 0..1);
        }

        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();

        Ok(())
    }
}

const BLIT_SHADER: &str = include_str!("shaders/blit.wgsl");
