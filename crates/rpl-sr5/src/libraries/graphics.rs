use std::sync::{Arc, Mutex};

use rpl::{
    Span,
    ir::{Branch, LibId},
    libs::{
        CommandInfo, ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor,
        LibraryInterface, LibraryLowerer,
    },
    lower::{LowerContext, LowerError},
    value::Value,
};

use png::ColorType;

use crate::hardware::{Sr5Hardware, rgb_to_555};
use crate::renderer::Sr5Renderer;
use rpl_vector_plot::{decode, render};

/// SR5 Graphics library - CLS, PGET, RGB, RENDER, BGLOAD
#[derive(Clone)]
pub struct Sr5GraphicsLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// SR5 Graphics library ID.
pub const SR5_GRAPHICS_LIB: LibId = 200;

impl Sr5GraphicsLib {
    const CMD_CLS: u16 = 0;
    const CMD_PGET: u16 = 1;
    const CMD_RGB: u16 = 2;
    const CMD_RENDER: u16 = 3;
    const CMD_BGLOAD: u16 = 4;
    const CMD_BGCLR: u16 = 5;

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }
}

impl LibraryInterface for Sr5GraphicsLib {
    fn id(&self) -> LibId {
        SR5_GRAPHICS_LIB
    }

    fn name(&self) -> &'static str {
        "SR5Graphics"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("CLS", SR5_GRAPHICS_LIB, Self::CMD_CLS, 1, 0),
            CommandInfo::with_effect("PGET", SR5_GRAPHICS_LIB, Self::CMD_PGET, 2, 1),
            CommandInfo::with_effect("RGB", SR5_GRAPHICS_LIB, Self::CMD_RGB, 3, 1),
            CommandInfo::with_effect("RENDER", SR5_GRAPHICS_LIB, Self::CMD_RENDER, 1, 0),
            CommandInfo::with_effect("BGLOAD", SR5_GRAPHICS_LIB, Self::CMD_BGLOAD, 1, 0),
            CommandInfo::with_effect("BGCLR", SR5_GRAPHICS_LIB, Self::CMD_BGCLR, 0, 0),
        ]
    }
}

impl LibraryLowerer for Sr5GraphicsLib {
    fn id(&self) -> LibId {
        SR5_GRAPHICS_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "SR5 Graphics library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SR5_GRAPHICS_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for Sr5GraphicsLib {
    fn id(&self) -> LibId {
        SR5_GRAPHICS_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let mut hw = match self.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return Err("Hardware lock poisoned".into()),
        };

        match ctx.cmd {
            Self::CMD_CLS => {
                let color = match ctx.pop() {
                    Ok(Value::Real(v)) => v as u16,
                    Ok(Value::Integer(v)) => v as u16,
                    Ok(_) => return Err("CLS: expected number".into()),
                    Err(_) => return Err("CLS: stack underflow".into()),
                };
                hw.clear(color);
                Ok(ExecuteAction::ok())
            }
            Self::CMD_PGET => {
                let y = match ctx.pop() {
                    Ok(Value::Real(v)) => v as i32,
                    Ok(Value::Integer(v)) => v as i32,
                    Ok(_) => return Err("PGET: expected number for y".into()),
                    Err(_) => return Err("PGET: stack underflow".into()),
                };
                let x = match ctx.pop() {
                    Ok(Value::Real(v)) => v as i32,
                    Ok(Value::Integer(v)) => v as i32,
                    Ok(_) => return Err("PGET: expected number for x".into()),
                    Err(_) => return Err("PGET: stack underflow".into()),
                };
                let color = hw.get_pixel(x, y).unwrap_or(0);
                ctx.push(Value::Integer(color as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_RGB => {
                let b = pop_number(ctx, "RGB")? as u8;
                let g = pop_number(ctx, "RGB")? as u8;
                let r = pop_number(ctx, "RGB")? as u8;
                let color = rgb_to_555(r, g, b);
                ctx.push(Value::Integer(color as i64))?;
                Ok(ExecuteAction::ok())
            }
            Self::CMD_RENDER => {
                // Accept plot data as Bytes (when plot library stores as raw bytes)
                let bytes = match ctx.pop() {
                    Ok(Value::Bytes(data)) => data.to_vec(),
                    Ok(_) => return Err("RENDER: expected Bytes (plot data)".into()),
                    Err(_) => return Err("RENDER: stack underflow".into()),
                };
                let plot = decode(&bytes).map_err(|e| format!("RENDER: {}", e))?;
                let mut renderer = Sr5Renderer::new(&mut hw);
                render(&plot, &mut renderer);
                Ok(ExecuteAction::ok())
            }
            Self::CMD_BGLOAD => {
                // bytes --
                let bytes = match ctx.pop() {
                    Ok(Value::Bytes(data)) => data,
                    Ok(_) => return Err("BGLOAD: expected Bytes".into()),
                    Err(_) => return Err("BGLOAD: stack underflow".into()),
                };

                // Decode PNG (with automatic expansion of indexed/palette images)
                let mut decoder = png::Decoder::new(bytes.as_ref());
                decoder.set_transformations(png::Transformations::EXPAND);
                let mut reader = decoder.read_info().map_err(|e| format!("BGLOAD: {}", e))?;

                let mut buf = vec![0; reader.output_buffer_size()];
                let info = reader
                    .next_frame(&mut buf)
                    .map_err(|e| format!("BGLOAD: {}", e))?;

                let width = info.width;
                let height = info.height;

                // Convert to RGBA if needed
                let rgba_data = match info.color_type {
                    ColorType::Rgba => buf[..info.buffer_size()].to_vec(),
                    ColorType::Rgb => {
                        let rgb = &buf[..info.buffer_size()];
                        let mut rgba = Vec::with_capacity((width as usize) * (height as usize) * 4);
                        for chunk in rgb.chunks(3) {
                            rgba.push(chunk[0]);
                            rgba.push(chunk[1]);
                            rgba.push(chunk[2]);
                            rgba.push(255);
                        }
                        rgba
                    }
                    ColorType::GrayscaleAlpha => {
                        let ga = &buf[..info.buffer_size()];
                        let mut rgba = Vec::with_capacity((width as usize) * (height as usize) * 4);
                        for chunk in ga.chunks(2) {
                            rgba.push(chunk[0]);
                            rgba.push(chunk[0]);
                            rgba.push(chunk[0]);
                            rgba.push(chunk[1]);
                        }
                        rgba
                    }
                    ColorType::Grayscale => {
                        let g = &buf[..info.buffer_size()];
                        let mut rgba = Vec::with_capacity((width as usize) * (height as usize) * 4);
                        for &v in g {
                            rgba.push(v);
                            rgba.push(v);
                            rgba.push(v);
                            rgba.push(255);
                        }
                        rgba
                    }
                    ColorType::Indexed => {
                        return Err("BGLOAD: indexed color not supported".into());
                    }
                };

                hw.set_background(width, height, rgba_data);
                Ok(ExecuteAction::ok())
            }
            Self::CMD_BGCLR => {
                hw.clear_background();
                Ok(ExecuteAction::ok())
            }
            _ => Err(format!("Unknown graphics command: {}", ctx.cmd)),
        }
    }
}

/// Pop a number from the stack.
fn pop_number(ctx: &mut ExecuteContext, cmd: &str) -> Result<i64, String> {
    match ctx.pop() {
        Ok(Value::Real(v)) => Ok(v as i64),
        Ok(Value::Integer(v)) => Ok(v),
        Ok(other) => Err(format!("{}: expected number, got {:?}", cmd, other)),
        Err(_) => Err(format!("{}: stack underflow", cmd)),
    }
}
