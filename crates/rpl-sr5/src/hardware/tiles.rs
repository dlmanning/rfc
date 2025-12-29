//! Tile system for SR5.

/// Number of tile layers
pub const NUM_TILE_LAYERS: usize = 4;

/// A tile sheet (grid of tiles sliced from a PNG).
#[derive(Clone)]
pub struct TileSheet {
    /// Tile size (8 or 16)
    pub tile_size: u8,
    /// Number of tile columns in the sheet
    pub cols: u16,
    /// Number of tile rows in the sheet
    pub rows: u16,
    /// RGBA pixel data for the entire sheet
    pub data: Vec<u8>,
    /// Sheet width in pixels
    pub width: u16,
    /// Sheet height in pixels
    pub height: u16,
}

/// A tilemap (2D grid of tile indices).
#[derive(Clone)]
pub struct TileMap {
    /// Width in tiles
    pub width: u16,
    /// Height in tiles
    pub height: u16,
    /// Tile indices (row-major order)
    pub tiles: Vec<u16>,
    /// Generation counter (incremented on modification)
    pub generation: u32,
}

impl TileMap {
    /// Create a new empty tilemap.
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            width,
            height,
            tiles: vec![0; (width as usize) * (height as usize)],
            generation: 0,
        }
    }

    /// Get tile at position.
    pub fn get(&self, x: u16, y: u16) -> u16 {
        if x < self.width && y < self.height {
            self.tiles[(y as usize) * (self.width as usize) + (x as usize)]
        } else {
            0
        }
    }

    /// Set tile at position.
    pub fn set(&mut self, x: u16, y: u16, tile: u16) {
        if x < self.width && y < self.height {
            self.tiles[(y as usize) * (self.width as usize) + (x as usize)] = tile;
            self.generation = self.generation.wrapping_add(1);
        }
    }
}

/// A tile layer configuration.
#[derive(Clone)]
pub struct TileLayer {
    /// TileMap ID (None = layer not configured)
    pub tilemap_id: Option<usize>,
    /// TileSheet ID (None = layer not configured)
    pub tilesheet_id: Option<usize>,
    /// Horizontal scroll offset
    pub scroll_x: i32,
    /// Vertical scroll offset
    pub scroll_y: i32,
    /// Layer visibility
    pub visible: bool,
    /// Wrap scrolling (tilemap repeats infinitely)
    pub wrap: bool,
    /// Horizontal scale (1.0 = normal, 0.5 = half size)
    pub scale_x: f32,
    /// Vertical scale (1.0 = normal, 0.5 = half size)
    pub scale_y: f32,
    /// Rotation in degrees
    pub rotation: f32,
}

impl Default for TileLayer {
    fn default() -> Self {
        Self {
            tilemap_id: None,
            tilesheet_id: None,
            scroll_x: 0,
            scroll_y: 0,
            visible: false,
            wrap: false,
            scale_x: 1.0,
            scale_y: 1.0,
            rotation: 0.0,
        }
    }
}

/// Storage bank for tile sheets.
pub struct TileSheetBank {
    sheets: Vec<Option<TileSheet>>,
    next_id: usize,
    /// Generation counter (incremented on alloc/free)
    pub generation: u32,
}

impl TileSheetBank {
    pub fn new() -> Self {
        Self {
            sheets: Vec::new(),
            next_id: 0,
            generation: 0,
        }
    }

    pub fn alloc(&mut self, sheet: TileSheet) -> usize {
        self.generation = self.generation.wrapping_add(1);
        for i in self.next_id..self.sheets.len() {
            if self.sheets[i].is_none() {
                self.sheets[i] = Some(sheet);
                self.next_id = i + 1;
                return i;
            }
        }
        let id = self.sheets.len();
        self.sheets.push(Some(sheet));
        self.next_id = id + 1;
        id
    }

    pub fn free(&mut self, id: usize) {
        if id < self.sheets.len() {
            self.sheets[id] = None;
            self.generation = self.generation.wrapping_add(1);
            if id < self.next_id {
                self.next_id = id;
            }
        }
    }

    pub fn get(&self, id: usize) -> Option<&TileSheet> {
        self.sheets.get(id).and_then(|s| s.as_ref())
    }
}

impl Default for TileSheetBank {
    fn default() -> Self {
        Self::new()
    }
}

/// Storage bank for tilemaps.
pub struct TileMapBank {
    maps: Vec<Option<TileMap>>,
    next_id: usize,
    /// Generation counter (incremented on alloc/free)
    pub generation: u32,
}

impl TileMapBank {
    pub fn new() -> Self {
        Self {
            maps: Vec::new(),
            next_id: 0,
            generation: 0,
        }
    }

    pub fn alloc(&mut self, map: TileMap) -> usize {
        self.generation = self.generation.wrapping_add(1);
        for i in self.next_id..self.maps.len() {
            if self.maps[i].is_none() {
                self.maps[i] = Some(map);
                self.next_id = i + 1;
                return i;
            }
        }
        let id = self.maps.len();
        self.maps.push(Some(map));
        self.next_id = id + 1;
        id
    }

    pub fn free(&mut self, id: usize) {
        if id < self.maps.len() {
            self.maps[id] = None;
            self.generation = self.generation.wrapping_add(1);
            if id < self.next_id {
                self.next_id = id;
            }
        }
    }

    pub fn get(&self, id: usize) -> Option<&TileMap> {
        self.maps.get(id).and_then(|s| s.as_ref())
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut TileMap> {
        self.maps.get_mut(id).and_then(|s| s.as_mut())
    }
}

impl Default for TileMapBank {
    fn default() -> Self {
        Self::new()
    }
}
