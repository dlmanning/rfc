mod app;
mod console;
mod gpu;
mod hardware;
mod libraries;
mod renderer;

use std::{env, path::PathBuf};

use app::Sr5App;
use winit::event_loop::{ControlFlow, EventLoop};

fn main() {
    let args: Vec<String> = env::args().collect();

    let project_path = if args.len() > 1 {
        Some(PathBuf::from(&args[1]))
    } else {
        None
    };

    println!("Space Robot 5 - Fantasy Console");
    if let Some(path) = &project_path {
        println!("Loading project: {:?}", path);
    }
    println!();
    println!("Controls:");
    println!("  Arrow keys: D-pad");
    println!("  Z: A, X: B, A: X, S: Y");
    println!("  Q: L, W: R");
    println!("  Enter: Start, Backspace: Select");
    println!();

    let event_loop = match EventLoop::new() {
        Ok(el) => el,
        Err(e) => {
            eprintln!("Failed to create event loop: {}", e);
            return;
        }
    };
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = Sr5App::new(project_path);
    if let Err(e) = event_loop.run_app(&mut app) {
        eprintln!("Event loop error: {}", e);
    }
}
