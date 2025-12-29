//! SR5 application state and event handling.

use std::{path::PathBuf, sync::Arc, time::Instant};

use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, KeyEvent, WindowEvent},
    event_loop::ActiveEventLoop,
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
};

use crate::console::Console;
use crate::gpu::GpuState;
use crate::hardware::{DEFAULT_SCREEN_HEIGHT, DEFAULT_SCREEN_WIDTH, buttons};

const SCALE: u32 = 2;

/// SR5 application state.
pub struct Sr5App {
    window: Option<Arc<Window>>,
    gpu: Option<GpuState>,
    console: Console,
    last_frame: Instant,
    button_state: u16,
    project_path: Option<PathBuf>,
    project_loaded: bool,
    cpu_frame: Vec<u8>,
}

impl Sr5App {
    /// Create a new SR5 application.
    pub fn new(project_path: Option<PathBuf>) -> Self {
        Self {
            window: None,
            gpu: None,
            console: Console::new(),
            last_frame: Instant::now(),
            button_state: 0,
            project_path,
            project_loaded: false,
            // Initial buffer with default size, will be resized if SCREEN called
            cpu_frame: vec![0u8; (DEFAULT_SCREEN_WIDTH * DEFAULT_SCREEN_HEIGHT * 4) as usize],
        }
    }

    fn draw(&mut self) {
        // Render CPU framebuffer (sprites, etc.)
        self.console.render(&mut self.cpu_frame);

        if let Some(gpu) = &mut self.gpu
            && let Err(e) = gpu.render(&self.console, &self.cpu_frame)
        {
            eprintln!("Render error: {:?}", e);
        }
    }

    fn update(&mut self) {
        let now = Instant::now();
        let delta = now.duration_since(self.last_frame);
        self.last_frame = now;

        // Update input state
        self.console.update_input(self.button_state);

        // Advance frame
        self.console.next_frame(delta.as_millis() as u64);

        // Call project entry points if a project is loaded
        if self.project_loaded {
            if let Err(e) = self.console.update() {
                eprintln!("Error in update: {}", e);
            }
            if let Err(e) = self.console.draw() {
                eprintln!("Error in draw: {}", e);
            }
        }
    }

    fn handle_key(&mut self, key: KeyCode, pressed: bool) {
        let button = match key {
            KeyCode::KeyZ => buttons::A,
            KeyCode::KeyX => buttons::B,
            KeyCode::KeyA => buttons::X,
            KeyCode::KeyS => buttons::Y,
            KeyCode::KeyQ => buttons::L,
            KeyCode::KeyW => buttons::R,
            KeyCode::Enter => buttons::START,
            KeyCode::Backspace => buttons::SELECT,
            KeyCode::ArrowUp => buttons::UP,
            KeyCode::ArrowDown => buttons::DOWN,
            KeyCode::ArrowLeft => buttons::LEFT,
            KeyCode::ArrowRight => buttons::RIGHT,
            KeyCode::Space => buttons::ENTER,
            KeyCode::Tab => buttons::MODE,
            KeyCode::Escape => buttons::VARS,
            _ => return,
        };

        if pressed {
            self.button_state |= button;
        } else {
            self.button_state &= !button;
        }
    }
}

impl ApplicationHandler for Sr5App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }

        // Create window with default size (may resize after init)
        let default_size =
            LogicalSize::new(DEFAULT_SCREEN_WIDTH * SCALE, DEFAULT_SCREEN_HEIGHT * SCALE);
        let attrs = Window::default_attributes()
            .with_title("Space Robot 5")
            .with_inner_size(default_size)
            .with_resizable(false);

        let window = match event_loop.create_window(attrs) {
            Ok(w) => Arc::new(w),
            Err(e) => {
                eprintln!("Failed to create window: {}", e);
                event_loop.exit();
                return;
            }
        };

        // Load project and run init BEFORE creating GPU
        // This allows SCREEN command to set resolution
        if let Some(project_path) = &self.project_path {
            let path = project_path.clone();
            if let Err(e) = self.console.load_project(&path) {
                eprintln!("Failed to load project: {}", e);
                self.console.draw_logo();
            } else {
                println!("Loaded project: {:?}", path);
                // Call init entry point - SCREEN command can run here
                if let Err(e) = self.console.init() {
                    eprintln!("Error in init: {}", e);
                }
                self.project_loaded = true;
            }
        } else {
            // Draw SR5 logo
            self.console.draw_logo();
        }

        // Get final resolution from hardware (may have changed via SCREEN command)
        let (width, height) = self.console.hardware.lock().unwrap().dimensions();

        // Resize window to match resolution if different from default
        if width != DEFAULT_SCREEN_WIDTH || height != DEFAULT_SCREEN_HEIGHT {
            let _ = window.request_inner_size(LogicalSize::new(width * SCALE, height * SCALE));
        }

        // Create GPU state with final resolution
        let gpu = match GpuState::new(window.clone(), width, height) {
            Ok(gpu) => gpu,
            Err(e) => {
                eprintln!("GPU initialization failed: {}", e);
                event_loop.exit();
                return;
            }
        };

        // Resize cpu_frame buffer if resolution changed
        let frame_size = (width * height * 4) as usize;
        if self.cpu_frame.len() != frame_size {
            self.cpu_frame = vec![0u8; frame_size];
        }

        self.window = Some(window);
        self.gpu = Some(gpu);
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::Resized(physical_size) => {
                if let Some(gpu) = &mut self.gpu {
                    gpu.resize(physical_size);
                }
            }
            WindowEvent::KeyboardInput {
                event:
                    KeyEvent {
                        physical_key: PhysicalKey::Code(keycode),
                        state,
                        ..
                    },
                ..
            } => {
                self.handle_key(keycode, state == ElementState::Pressed);
            }
            WindowEvent::RedrawRequested => {
                self.update();
                self.draw();
            }
            _ => {}
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }
}
