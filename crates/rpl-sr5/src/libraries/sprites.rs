//! SR5 Sprite Library
//!
//! Commands for loading and drawing sprites with alpha blending.

use std::sync::{Arc, Mutex};

use png::ColorType;
use rpl::{
    Span,
    ir::{Branch, LibId},
    libs::{CommandInfo, ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryInterface, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};

use crate::hardware::{Sprite, SpriteOptions, Sr5Hardware};

/// SR5 Sprites library - PNGLOAD, SPRLOAD, SPRFREE, SPR, SPRFLIP, etc.
#[derive(Clone)]
pub struct Sr5SpritesLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// SR5 Sprites library ID.
pub const SR5_SPRITES_LIB: LibId = 203;

impl Sr5SpritesLib {
    // Loading commands
    const CMD_PNGLOAD: u16 = 0;   // bytes -- id w h
    const CMD_SPRLOAD: u16 = 1;   // data w h -- id
    const CMD_SPRFREE: u16 = 2;   // id --

    // Drawing commands
    const CMD_SPR: u16 = 3;       // id x y --
    const CMD_SPRFLIP: u16 = 4;   // id x y flags --
    const CMD_SPRSCALE: u16 = 5;  // id x y sx sy --
    const CMD_SPRROT: u16 = 6;    // id x y angle --
    const CMD_SPREX: u16 = 7;     // id x y sx sy angle flags --

    // Info commands
    const CMD_SPRW: u16 = 8;      // id -- width
    const CMD_SPRH: u16 = 9;      // id -- height

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }
}

impl LibraryInterface for Sr5SpritesLib {
    fn id(&self) -> LibId {
        SR5_SPRITES_LIB
    }

    fn name(&self) -> &'static str {
        "SR5Sprites"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            // Loading
            CommandInfo::with_effect("PNGLOAD", SR5_SPRITES_LIB, Self::CMD_PNGLOAD, 1, 3),
            CommandInfo::with_effect("SPRLOAD", SR5_SPRITES_LIB, Self::CMD_SPRLOAD, 3, 1),
            CommandInfo::with_effect("SPRFREE", SR5_SPRITES_LIB, Self::CMD_SPRFREE, 1, 0),
            // Drawing
            CommandInfo::with_effect("SPR", SR5_SPRITES_LIB, Self::CMD_SPR, 3, 0),
            CommandInfo::with_effect("SPRFLIP", SR5_SPRITES_LIB, Self::CMD_SPRFLIP, 4, 0),
            CommandInfo::with_effect("SPRSCALE", SR5_SPRITES_LIB, Self::CMD_SPRSCALE, 5, 0),
            CommandInfo::with_effect("SPRROT", SR5_SPRITES_LIB, Self::CMD_SPRROT, 4, 0),
            CommandInfo::with_effect("SPREX", SR5_SPRITES_LIB, Self::CMD_SPREX, 7, 0),
            // Info
            CommandInfo::with_effect("SPRW", SR5_SPRITES_LIB, Self::CMD_SPRW, 1, 1),
            CommandInfo::with_effect("SPRH", SR5_SPRITES_LIB, Self::CMD_SPRH, 1, 1),
        ]
    }
}

impl LibraryLowerer for Sr5SpritesLib {
    fn id(&self) -> LibId {
        SR5_SPRITES_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "SR5 Sprites library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SR5_SPRITES_LIB, cmd);
        Ok(())
    }
}

impl LibraryExecutor for Sr5SpritesLib {
    fn id(&self) -> LibId {
        SR5_SPRITES_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let mut hw = match self.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return Err("Hardware lock poisoned".into()),
        };

        match ctx.cmd {
            Self::CMD_PNGLOAD => {
                // bytes -- id w h
                let bytes = match ctx.pop() {
                    Ok(Value::Bytes(data)) => data,
                    Ok(_) => return Err("PNGLOAD: expected Bytes".into()),
                    Err(_) => return Err("PNGLOAD: stack underflow".into()),
                };

                // Decode PNG (with automatic expansion of indexed/palette images)
                let mut decoder = png::Decoder::new(bytes.as_ref());
                // Expand indexed/palette images to RGB(A)
                decoder.set_transformations(png::Transformations::EXPAND);
                let mut reader = decoder.read_info().map_err(|e| format!("PNGLOAD: {}", e))?;

                let mut buf = vec![0; reader.output_buffer_size()];
                let info = reader.next_frame(&mut buf).map_err(|e| format!("PNGLOAD: {}", e))?;

                let width = info.width as u16;
                let height = info.height as u16;

                // Convert to RGBA if needed
                let rgba_data = match info.color_type {
                    ColorType::Rgba => buf[..info.buffer_size()].to_vec(),
                    ColorType::Rgb => {
                        // Convert RGB to RGBA
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
                        return Err("PNGLOAD: indexed color not supported".into());
                    }
                };

                let sprite = Sprite {
                    width,
                    height,
                    data: rgba_data,
                };

                let id = hw.sprites.alloc(sprite);

                ctx.push(Value::Integer(id as i64))?;
                ctx.push(Value::Integer(width as i64))?;
                ctx.push(Value::Integer(height as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRLOAD => {
                // data w h -- id
                let h = pop_int(ctx, "SPRLOAD")? as u16;
                let w = pop_int(ctx, "SPRLOAD")? as u16;
                let data = match ctx.pop() {
                    Ok(Value::Bytes(data)) => data.to_vec(),
                    Ok(_) => return Err("SPRLOAD: expected Bytes".into()),
                    Err(_) => return Err("SPRLOAD: stack underflow".into()),
                };

                let expected_size = (w as usize) * (h as usize) * 4;
                if data.len() != expected_size {
                    return Err(format!(
                        "SPRLOAD: expected {} bytes for {}x{} RGBA, got {}",
                        expected_size, w, h, data.len()
                    ));
                }

                let sprite = Sprite {
                    width: w,
                    height: h,
                    data,
                };

                let id = hw.sprites.alloc(sprite);
                ctx.push(Value::Integer(id as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRFREE => {
                // id --
                let id = pop_int(ctx, "SPRFREE")? as usize;
                hw.sprites.free(id);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPR => {
                // id x y --
                let y = pop_int(ctx, "SPR")? as i32;
                let x = pop_int(ctx, "SPR")? as i32;
                let id = pop_int(ctx, "SPR")? as usize;
                hw.blit_sprite(id, x, y);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRFLIP => {
                // id x y flags --
                let flags = pop_int(ctx, "SPRFLIP")? as u8;
                let y = pop_int(ctx, "SPRFLIP")? as i32;
                let x = pop_int(ctx, "SPRFLIP")? as i32;
                let id = pop_int(ctx, "SPRFLIP")? as usize;
                hw.blit_sprite_flip(id, x, y, flags);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRSCALE => {
                // id x y sx sy --
                let sy = pop_int(ctx, "SPRSCALE")? as i32;
                let sx = pop_int(ctx, "SPRSCALE")? as i32;
                let y = pop_int(ctx, "SPRSCALE")? as i32;
                let x = pop_int(ctx, "SPRSCALE")? as i32;
                let id = pop_int(ctx, "SPRSCALE")? as usize;
                hw.blit_sprite_scaled(id, x, y, sx, sy);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRROT => {
                // id x y angle --
                let angle = pop_int(ctx, "SPRROT")? as i32;
                let y = pop_int(ctx, "SPRROT")? as i32;
                let x = pop_int(ctx, "SPRROT")? as i32;
                let id = pop_int(ctx, "SPRROT")? as usize;
                hw.blit_sprite_rotated(id, x, y, angle);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPREX => {
                // id x y sx sy angle flags --
                let flags = pop_int(ctx, "SPREX")? as u8;
                let angle = pop_int(ctx, "SPREX")? as i32;
                let scale_y = pop_int(ctx, "SPREX")? as i32;
                let scale_x = pop_int(ctx, "SPREX")? as i32;
                let y = pop_int(ctx, "SPREX")? as i32;
                let x = pop_int(ctx, "SPREX")? as i32;
                let id = pop_int(ctx, "SPREX")? as usize;
                let opts = SpriteOptions {
                    scale_x,
                    scale_y,
                    angle,
                    flip_h: flags & 1 != 0,
                    flip_v: flags & 2 != 0,
                };
                hw.blit_sprite_ex(id, x, y, opts);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRW => {
                // id -- width
                let id = pop_int(ctx, "SPRW")? as usize;
                let width = hw.sprites.get(id).map(|s| s.width as i64).unwrap_or(0);
                ctx.push(Value::Integer(width))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SPRH => {
                // id -- height
                let id = pop_int(ctx, "SPRH")? as usize;
                let height = hw.sprites.get(id).map(|s| s.height as i64).unwrap_or(0);
                ctx.push(Value::Integer(height))?;
                Ok(ExecuteAction::ok())
            }

            _ => Err(format!("Unknown sprites command: {}", ctx.cmd)),
        }
    }
}

/// Pop an integer from the stack.
fn pop_int(ctx: &mut ExecuteContext, cmd: &str) -> Result<i64, String> {
    match ctx.pop() {
        Ok(Value::Real(v)) => Ok(v as i64),
        Ok(Value::Integer(v)) => Ok(v),
        Ok(other) => Err(format!("{}: expected number, got {:?}", cmd, other)),
        Err(_) => Err(format!("{}: stack underflow", cmd)),
    }
}
