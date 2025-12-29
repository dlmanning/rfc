//! SR5 Tile Layer Library
//!
//! Commands for tile-based background layers.

use std::sync::{Arc, Mutex};

use png::ColorType;
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

use crate::hardware::{NUM_TILE_LAYERS, Sr5Hardware, TileMap, TileSheet};

/// SR5 Tiles library
#[derive(Clone)]
pub struct Sr5TilesLib {
    hardware: Arc<Mutex<Sr5Hardware>>,
}

/// SR5 Tiles library ID.
pub const SR5_TILES_LIB: LibId = 204;

impl Sr5TilesLib {
    // Tile sheet commands
    const CMD_TSLOAD: u16 = 0; // bytes tilesize -- id
    const CMD_TSFREE: u16 = 1; // id --

    // Tilemap commands
    const CMD_TMNEW: u16 = 2; // w h -- id
    const CMD_TMLOAD: u16 = 3; // bytes -- id
    const CMD_TMFREE: u16 = 4; // id --
    const CMD_TSET: u16 = 5; // tile x y map --
    const CMD_TGET: u16 = 6; // x y map -- tile

    // Layer commands
    const CMD_LAYER: u16 = 7; // map sheet layer --
    const CMD_SCROLL: u16 = 8; // x y layer --
    const CMD_LSHOW: u16 = 9; // visible layer --
    const CMD_LWRAP: u16 = 10; // wrap layer --
    const CMD_LSCALE: u16 = 11; // scale layer -- (uniform scale)
    const CMD_LROT: u16 = 12; // angle layer -- (rotation in degrees)

    pub fn new(hardware: Arc<Mutex<Sr5Hardware>>) -> Self {
        Self { hardware }
    }
}

impl LibraryInterface for Sr5TilesLib {
    fn id(&self) -> LibId {
        SR5_TILES_LIB
    }

    fn name(&self) -> &'static str {
        "SR5Tiles"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            // Tile sheet
            CommandInfo::with_effect("TSLOAD", SR5_TILES_LIB, Self::CMD_TSLOAD, 2, 1),
            CommandInfo::with_effect("TSFREE", SR5_TILES_LIB, Self::CMD_TSFREE, 1, 0),
            // Tilemap
            CommandInfo::with_effect("TMNEW", SR5_TILES_LIB, Self::CMD_TMNEW, 2, 1),
            CommandInfo::with_effect("TMLOAD", SR5_TILES_LIB, Self::CMD_TMLOAD, 1, 1),
            CommandInfo::with_effect("TMFREE", SR5_TILES_LIB, Self::CMD_TMFREE, 1, 0),
            CommandInfo::with_effect("TSET", SR5_TILES_LIB, Self::CMD_TSET, 4, 0),
            CommandInfo::with_effect("TGET", SR5_TILES_LIB, Self::CMD_TGET, 3, 1),
            // Layer
            CommandInfo::with_effect("LAYER", SR5_TILES_LIB, Self::CMD_LAYER, 3, 0),
            CommandInfo::with_effect("SCROLL", SR5_TILES_LIB, Self::CMD_SCROLL, 3, 0),
            CommandInfo::with_effect("LSHOW", SR5_TILES_LIB, Self::CMD_LSHOW, 2, 0),
            CommandInfo::with_effect("LWRAP", SR5_TILES_LIB, Self::CMD_LWRAP, 2, 0),
            CommandInfo::with_effect("LSCALE", SR5_TILES_LIB, Self::CMD_LSCALE, 2, 0),
            CommandInfo::with_effect("LROT", SR5_TILES_LIB, Self::CMD_LROT, 2, 0),
        ]
    }
}

impl LibraryLowerer for Sr5TilesLib {
    fn id(&self) -> LibId {
        SR5_TILES_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError {
            message: "SR5 Tiles library has no composites".into(),
            span: None,
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(SR5_TILES_LIB, cmd);
        Ok(())
    }
}

/// Helper to pop an integer from the stack.
fn pop_int(ctx: &mut ExecuteContext, cmd: &str) -> Result<i64, String> {
    match ctx.pop() {
        Ok(Value::Integer(i)) => Ok(i),
        Ok(Value::Real(r)) => Ok(r as i64),
        Ok(other) => Err(format!("{}: expected number, got {:?}", cmd, other)),
        Err(_) => Err(format!("{}: stack underflow", cmd)),
    }
}

/// Helper to pop a float from the stack.
fn pop_float(ctx: &mut ExecuteContext, cmd: &str) -> Result<f32, String> {
    match ctx.pop() {
        Ok(Value::Real(r)) => Ok(r as f32),
        Ok(Value::Integer(i)) => Ok(i as f32),
        Ok(other) => Err(format!("{}: expected number, got {:?}", cmd, other)),
        Err(_) => Err(format!("{}: stack underflow", cmd)),
    }
}

impl LibraryExecutor for Sr5TilesLib {
    fn id(&self) -> LibId {
        SR5_TILES_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let mut hw = match self.hardware.lock() {
            Ok(guard) => guard,
            Err(_) => return Err("Hardware lock poisoned".into()),
        };

        match ctx.cmd {
            Self::CMD_TSLOAD => {
                // bytes tilesize -- id
                let tile_size = pop_int(ctx, "TSLOAD")? as u8;
                let bytes = match ctx.pop() {
                    Ok(Value::Bytes(b)) => b,
                    Ok(_) => return Err("TSLOAD requires bytes".into()),
                    Err(_) => return Err("TSLOAD: stack underflow".into()),
                };

                // Validate tile size
                if tile_size != 8 && tile_size != 16 {
                    return Err("TSLOAD tile size must be 8 or 16".into());
                }

                // Decode PNG
                let decoder = png::Decoder::new(bytes.as_ref());
                let mut reader = decoder
                    .read_info()
                    .map_err(|e| format!("PNG error: {}", e))?;
                let mut buf = vec![0; reader.output_buffer_size()];
                let info = reader
                    .next_frame(&mut buf)
                    .map_err(|e| format!("PNG error: {}", e))?;

                let width = info.width as u16;
                let height = info.height as u16;

                // Convert to RGBA
                let data = match info.color_type {
                    ColorType::Rgba => buf[..(width as usize * height as usize * 4)].to_vec(),
                    ColorType::Rgb => {
                        let pixels = width as usize * height as usize;
                        let mut rgba = Vec::with_capacity(pixels * 4);
                        for i in 0..pixels {
                            rgba.push(buf[i * 3]);
                            rgba.push(buf[i * 3 + 1]);
                            rgba.push(buf[i * 3 + 2]);
                            rgba.push(255);
                        }
                        rgba
                    }
                    _ => return Err("TSLOAD requires RGB or RGBA PNG".into()),
                };

                // Calculate tile grid
                let cols = width / tile_size as u16;
                let rows = height / tile_size as u16;

                let sheet = TileSheet {
                    tile_size,
                    cols,
                    rows,
                    data,
                    width,
                    height,
                };

                let id = hw.tilesheets.alloc(sheet);
                ctx.push(Value::Integer(id as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TSFREE => {
                // id --
                let id = pop_int(ctx, "TSFREE")? as usize;
                hw.tilesheets.free(id);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TMNEW => {
                // w h -- id
                let h = pop_int(ctx, "TMNEW")? as u16;
                let w = pop_int(ctx, "TMNEW")? as u16;
                let map = TileMap::new(w, h);
                let id = hw.tilemaps.alloc(map);
                ctx.push(Value::Integer(id as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TMLOAD => {
                // bytes -- id
                let bytes = match ctx.pop() {
                    Ok(Value::Bytes(b)) => b,
                    Ok(_) => return Err("TMLOAD requires bytes".into()),
                    Err(_) => return Err("TMLOAD: stack underflow".into()),
                };

                if bytes.len() < 4 {
                    return Err("TMLOAD: invalid tilemap data".into());
                }

                // Parse: u16 width, u16 height, then u16 tiles
                let w = u16::from_le_bytes([bytes[0], bytes[1]]);
                let h = u16::from_le_bytes([bytes[2], bytes[3]]);
                let expected_size = 4 + (w as usize * h as usize * 2);

                if bytes.len() < expected_size {
                    return Err("TMLOAD: tilemap data too short".into());
                }

                let mut tiles = Vec::with_capacity(w as usize * h as usize);
                for i in 0..(w as usize * h as usize) {
                    let offset = 4 + i * 2;
                    let tile = u16::from_le_bytes([bytes[offset], bytes[offset + 1]]);
                    tiles.push(tile);
                }

                let map = TileMap {
                    width: w,
                    height: h,
                    tiles,
                    generation: 0,
                };
                let id = hw.tilemaps.alloc(map);
                ctx.push(Value::Integer(id as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TMFREE => {
                // id --
                let id = pop_int(ctx, "TMFREE")? as usize;
                hw.tilemaps.free(id);
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TSET => {
                // tile x y map --
                let map_id = pop_int(ctx, "TSET")? as usize;
                let y = pop_int(ctx, "TSET")? as u16;
                let x = pop_int(ctx, "TSET")? as u16;
                let tile = pop_int(ctx, "TSET")? as u16;

                if let Some(map) = hw.tilemaps.get_mut(map_id) {
                    map.set(x, y, tile);
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_TGET => {
                // x y map -- tile
                let map_id = pop_int(ctx, "TGET")? as usize;
                let y = pop_int(ctx, "TGET")? as u16;
                let x = pop_int(ctx, "TGET")? as u16;

                let tile = hw.tilemaps.get(map_id).map(|m| m.get(x, y)).unwrap_or(0);
                ctx.push(Value::Integer(tile as i64))?;
                Ok(ExecuteAction::ok())
            }

            Self::CMD_LAYER => {
                // map sheet layer --
                let layer_idx = pop_int(ctx, "LAYER")? as usize;
                let sheet_id = pop_int(ctx, "LAYER")? as usize;
                let map_id = pop_int(ctx, "LAYER")? as usize;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].tilemap_id = Some(map_id);
                    hw.layers[layer_idx].tilesheet_id = Some(sheet_id);
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_SCROLL => {
                // x y layer --
                let layer_idx = pop_int(ctx, "SCROLL")? as usize;
                let y = pop_int(ctx, "SCROLL")? as i32;
                let x = pop_int(ctx, "SCROLL")? as i32;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].scroll_x = x;
                    hw.layers[layer_idx].scroll_y = y;
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_LSHOW => {
                // visible layer --
                let layer_idx = pop_int(ctx, "LSHOW")? as usize;
                let visible = pop_int(ctx, "LSHOW")? != 0;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].visible = visible;
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_LWRAP => {
                // wrap layer --
                let layer_idx = pop_int(ctx, "LWRAP")? as usize;
                let wrap = pop_int(ctx, "LWRAP")? != 0;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].wrap = wrap;
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_LSCALE => {
                // scale layer --
                let layer_idx = pop_int(ctx, "LSCALE")? as usize;
                let scale = pop_float(ctx, "LSCALE")?;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].scale_x = scale;
                    hw.layers[layer_idx].scale_y = scale;
                }
                Ok(ExecuteAction::ok())
            }

            Self::CMD_LROT => {
                // angle layer --
                let layer_idx = pop_int(ctx, "LROT")? as usize;
                let angle = pop_float(ctx, "LROT")?;

                if layer_idx < NUM_TILE_LAYERS {
                    hw.layers[layer_idx].rotation = angle;
                }
                Ok(ExecuteAction::ok())
            }

            _ => Err(format!("Unknown SR5 Tiles command: {}", ctx.cmd)),
        }
    }
}
