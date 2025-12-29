// Tile layer shader - renders tilemaps with scroll, scale, rotation, and wrap.

struct Uniforms {
    screen_size: vec2<f32>,
    scroll: vec2<f32>,
    scale: vec2<f32>,
    rotation: f32,
    wrap: u32,
    tile_size: f32,
    map_width: u32,
    map_height: u32,
    sheet_cols: u32,
    sheet_rows: u32,
    visible: u32,
}

@group(0) @binding(0) var<uniform> uniforms: Uniforms;
@group(0) @binding(1) var tilesheet: texture_2d<f32>;
@group(0) @binding(2) var tile_sampler: sampler;
@group(0) @binding(3) var<storage, read> tilemap: array<u32>;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) uv: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4<f32>(in.position, 0.0, 1.0);
    out.uv = in.uv;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    if (uniforms.visible == 0u) {
        discard;
    }

    // Screen position in pixels
    let screen_pos = in.uv * uniforms.screen_size;

    // Apply inverse transform to get world position
    // Center of rotation is screen center
    let center = uniforms.screen_size * 0.5;
    let centered = screen_pos - center;

    // Inverse rotation
    let cos_r = cos(-uniforms.rotation);
    let sin_r = sin(-uniforms.rotation);
    let rotated = vec2<f32>(
        centered.x * cos_r - centered.y * sin_r,
        centered.x * sin_r + centered.y * cos_r
    );

    // Inverse scale
    let scaled = rotated / uniforms.scale;

    // Add scroll offset
    let world_pos = scaled + center + uniforms.scroll;

    // Calculate tile coordinates
    let tile_size = uniforms.tile_size;
    var tile_x = i32(floor(world_pos.x / tile_size));
    var tile_y = i32(floor(world_pos.y / tile_size));

    let map_w = i32(uniforms.map_width);
    let map_h = i32(uniforms.map_height);

    // Handle wrapping or clipping
    if (uniforms.wrap != 0u) {
        // Proper modulo for negative numbers
        tile_x = ((tile_x % map_w) + map_w) % map_w;
        tile_y = ((tile_y % map_h) + map_h) % map_h;
    } else {
        if (tile_x < 0 || tile_x >= map_w || tile_y < 0 || tile_y >= map_h) {
            discard;
        }
    }

    // Lookup tile index from tilemap
    let tile_idx = tilemap[u32(tile_y) * uniforms.map_width + u32(tile_x)];
    if (tile_idx == 0u) {
        discard;  // Tile 0 = transparent
    }

    // Calculate UV in tilesheet (tile_idx 1 maps to sheet index 0)
    let sheet_tile_idx = tile_idx - 1u;
    let sheet_tile_x = sheet_tile_idx % uniforms.sheet_cols;
    let sheet_tile_y = sheet_tile_idx / uniforms.sheet_cols;

    // Local UV within the tile (0..1)
    let local_uv = fract(world_pos / tile_size);

    // Calculate final UV in tilesheet texture
    let tile_uv = (vec2<f32>(f32(sheet_tile_x), f32(sheet_tile_y)) + local_uv) /
                  vec2<f32>(f32(uniforms.sheet_cols), f32(uniforms.sheet_rows));

    let color = textureSample(tilesheet, tile_sampler, tile_uv);

    // Discard fully transparent pixels
    if (color.a < 0.01) {
        discard;
    }

    return color;
}
