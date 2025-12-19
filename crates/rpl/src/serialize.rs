//! Value serialization for rpl.
//!
//! Provides functions to serialize any rpl Value to bytes and deserialize bytes
//! back to Values. Used by PACKDIR and user libraries.
//!
//! ## Wire Format
//!
//! Each value is prefixed with a 1-byte type tag. Tag bit 0x10 indicates "has debug info".
//!
//! | Tag | Type | Payload |
//! |-----|------|---------|
//! | 0x01 | Integer | 8 bytes (i64 LE) |
//! | 0x02 | Real | 8 bytes (f64 LE) |
//! | 0x03 | String | u32 len + UTF-8 bytes |
//! | 0x04 | List | u32 count + serialized elements |
//! | 0x05 | Program | strings + bytecode (no debug) |
//! | 0x15 | Program | strings + bytecode + source map |
//! | 0x06 | Symbolic | u32 len + string representation |
//! | 0x07 | Library | library data (no debug) |
//! | 0x17 | Library | library data + source maps |
//!
//! ## Source Maps
//!
//! Source maps include the source code itself, allowing error messages to show
//! the actual code even when deserializing standalone library files.
//!
//! Format: `source_len: u32 + source: UTF-8 + span_count: u16 + spans...`

use std::fmt;
use std::sync::Arc;

use crate::core::{Pos, Span};

use crate::value::{LibraryCommand, LibraryData, ProgramData, Value};
use crate::vm::directory::DirNode;

// Type tags
const TAG_INTEGER: u8 = 0x01;
const TAG_REAL: u8 = 0x02;
const TAG_STRING: u8 = 0x03;
const TAG_LIST: u8 = 0x04;
const TAG_PROGRAM: u8 = 0x05;
const TAG_SYMBOLIC: u8 = 0x06;
const TAG_LIBRARY: u8 = 0x07;
const TAG_BYTES: u8 = 0x08;

/// Bit flag indicating debug info is present.
const DEBUG_FLAG: u8 = 0x10;

/// Error type for serialization/deserialization failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SerializeError {
    /// Unexpected end of input.
    UnexpectedEnd,
    /// Invalid type tag.
    InvalidTag(u8),
    /// Invalid UTF-8 in string data.
    InvalidUtf8,
}

impl fmt::Display for SerializeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEnd => write!(f, "Unexpected end of data"),
            Self::InvalidTag(tag) => write!(f, "Invalid type tag: 0x{:02x}", tag),
            Self::InvalidUtf8 => write!(f, "Invalid UTF-8 string data"),
        }
    }
}

impl std::error::Error for SerializeError {}

/// Options for serialization.
#[derive(Clone, Copy, Default)]
pub struct SerializeOptions {
    /// Include source maps (debug info) for programs.
    pub include_source_maps: bool,
}

/// Source map: maps bytecode offsets to source spans, with the source code included.
///
/// Including the source allows error messages to show the actual code even when
/// deserializing standalone library files.
#[derive(Clone, Debug, Default)]
pub struct SourceMap {
    /// The source code that spans reference into.
    pub source: String,
    /// Bytecode offsets where spans start.
    pub offsets: Vec<u32>,
    /// Source spans (parallel with offsets).
    pub spans: Vec<Span>,
}

impl SourceMap {
    /// Create a new empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a source map with source code.
    pub fn with_source(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            offsets: Vec::new(),
            spans: Vec::new(),
        }
    }

    /// Check if the source map is empty.
    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    /// Get a slice of source code for a span.
    pub fn get_source_slice(&self, span: &Span) -> Option<&str> {
        let start = span.start().offset() as usize;
        let end = span.end().offset() as usize;
        if end <= self.source.len() && start <= end {
            Some(&self.source[start..end])
        } else {
            None
        }
    }
}

/// Extended program data with optional source map.
#[derive(Clone, Debug)]
pub struct ProgramWithDebug {
    /// The program data.
    pub program: Arc<ProgramData>,
    /// Optional source map.
    pub source_map: Option<SourceMap>,
}

/// Serialize a Value to bytes (without debug info).
pub fn serialize_value(value: &Value) -> Vec<u8> {
    serialize_value_with_options(value, SerializeOptions::default())
}

/// Serialize a Value to bytes with options.
pub fn serialize_value_with_options(value: &Value, opts: SerializeOptions) -> Vec<u8> {
    let mut buf = Vec::new();
    serialize_into(&mut buf, value, &opts);
    buf
}

/// Serialize a Value into an existing buffer.
fn serialize_into(buf: &mut Vec<u8>, value: &Value, opts: &SerializeOptions) {
    match value {
        Value::Integer(i) => {
            buf.push(TAG_INTEGER);
            buf.extend(&i.to_le_bytes());
        }
        Value::Real(r) => {
            buf.push(TAG_REAL);
            buf.extend(&r.to_le_bytes());
        }
        Value::String(s) => {
            buf.push(TAG_STRING);
            write_string(buf, s);
        }
        Value::List(items) => {
            buf.push(TAG_LIST);
            write_u32(buf, items.len() as u32);
            for item in items.iter() {
                serialize_into(buf, item, opts);
            }
        }
        Value::Program(prog) => {
            // Set debug flag if program has source map and we want to include it
            let has_debug = opts.include_source_maps && prog.source_map.is_some();
            buf.push(if has_debug { TAG_PROGRAM | DEBUG_FLAG } else { TAG_PROGRAM });
            serialize_program_data(buf, prog, opts);
        }
        Value::Symbolic(expr) => {
            buf.push(TAG_SYMBOLIC);
            let s = format!("{}", expr);
            write_string(buf, &s);
        }
        Value::Library(lib) => {
            buf.push(TAG_LIBRARY);
            serialize_library_data(buf, lib, opts);
        }
        Value::Bytes(data) => {
            buf.push(TAG_BYTES);
            write_u32(buf, data.len() as u32);
            buf.extend(data.iter());
        }
    }
}

/// Serialize program data (rodata + bytecode, optionally with source map).
fn serialize_program_data(buf: &mut Vec<u8>, prog: &ProgramData, opts: &SerializeOptions) {
    // Rodata section
    write_u32(buf, prog.rodata.len() as u32);
    buf.extend(prog.rodata.iter());

    // Bytecode
    write_u32(buf, prog.code.len() as u32);
    buf.extend(prog.code.iter());

    // Source map (if present and requested)
    if opts.include_source_maps && let Some(ref source_map) = prog.source_map {
        serialize_source_map(buf, source_map);
    }
}

/// Serialize library data.
///
/// Format:
/// - lib_id_len: u8
/// - lib_id: UTF-8 bytes
/// - cmd_count: u16 (LE)
/// - per command:
///   - name_len: u8
///   - name: UTF-8 bytes
///   - rodata_len: u32 (LE)
///   - rodata: bytes
///   - code_len: u32 (LE)
///   - code: bytes
fn serialize_library_data(buf: &mut Vec<u8>, lib: &LibraryData, _opts: &SerializeOptions) {
    // Library ID (u8 len + string)
    buf.push(lib.id.len() as u8);
    buf.extend(lib.id.as_bytes());

    // Command count
    write_u16(buf, lib.commands.len() as u16);

    // Each command
    for cmd in &lib.commands {
        // Command name (u8 len + string)
        buf.push(cmd.name.len() as u8);
        buf.extend(cmd.name.as_bytes());

        // Rodata section
        write_u32(buf, cmd.rodata.len() as u32);
        buf.extend(cmd.rodata.iter());

        // Bytecode
        write_u32(buf, cmd.code.len() as u32);
        buf.extend(cmd.code.iter());
    }
}

/// Deserialize a Value from bytes.
/// Returns (value, bytes_consumed) on success.
pub fn deserialize_value(bytes: &[u8]) -> Result<(Value, usize), SerializeError> {
    if bytes.is_empty() {
        return Err(SerializeError::UnexpectedEnd);
    }

    let tag = bytes[0];
    let base_tag = tag & !DEBUG_FLAG; // Strip debug flag
    let _has_debug = (tag & DEBUG_FLAG) != 0;
    let rest = &bytes[1..];

    match base_tag {
        TAG_INTEGER => {
            if rest.len() < 8 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let i = i64::from_le_bytes(rest[..8].try_into().unwrap());
            Ok((Value::Integer(i), 9))
        }
        TAG_REAL => {
            if rest.len() < 8 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let r = f64::from_le_bytes(rest[..8].try_into().unwrap());
            Ok((Value::Real(r), 9))
        }
        TAG_STRING => {
            let (s, consumed) = read_string(rest)?;
            Ok((Value::string(s), 1 + consumed))
        }
        TAG_LIST => {
            if rest.len() < 4 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let count = read_u32(rest)? as usize;
            let mut offset = 4;
            let mut items = Vec::with_capacity(count);

            for _ in 0..count {
                let (item, consumed) = deserialize_value(&rest[offset..])?;
                items.push(item);
                offset += consumed;
            }

            Ok((Value::list(items), 1 + offset))
        }
        TAG_PROGRAM => {
            let (prog, consumed) = deserialize_program_data(rest, _has_debug)?;
            Ok((Value::Program(Arc::new(prog)), 1 + consumed))
        }
        TAG_SYMBOLIC => {
            let (s, consumed) = read_string(rest)?;
            // Store as a variable reference with the full expression string
            // This preserves the expression text for later re-parsing if needed
            Ok((Value::symbolic(crate::symbolic::SymExpr::var(s)), 1 + consumed))
        }
        TAG_LIBRARY => {
            let (lib, consumed) = deserialize_library_data(rest, _has_debug)?;
            Ok((Value::Library(Arc::new(lib)), 1 + consumed))
        }
        TAG_BYTES => {
            if rest.len() < 4 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let len = read_u32(rest)? as usize;
            if rest.len() < 4 + len {
                return Err(SerializeError::UnexpectedEnd);
            }
            let data: Vec<u8> = rest[4..4 + len].to_vec();
            Ok((Value::bytes(data), 1 + 4 + len))
        }
        _ => Err(SerializeError::InvalidTag(tag)),
    }
}

/// Deserialize program data.
fn deserialize_program_data(bytes: &[u8], has_debug: bool) -> Result<(ProgramData, usize), SerializeError> {
    let mut offset = 0;

    // Rodata section
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }
    let rodata_len = read_u32(bytes)? as usize;
    offset += 4;

    if bytes.len() < offset + rodata_len {
        return Err(SerializeError::UnexpectedEnd);
    }
    let rodata: Vec<u8> = bytes[offset..offset + rodata_len].to_vec();
    offset += rodata_len;

    // Bytecode
    if bytes.len() < offset + 4 {
        return Err(SerializeError::UnexpectedEnd);
    }
    let code_len = read_u32(&bytes[offset..])? as usize;
    offset += 4;

    if bytes.len() < offset + code_len {
        return Err(SerializeError::UnexpectedEnd);
    }
    let code: Vec<u8> = bytes[offset..offset + code_len].to_vec();
    offset += code_len;

    // Source map (if debug flag set)
    let source_map = if has_debug {
        let (sm, consumed) = deserialize_source_map(&bytes[offset..])?;
        offset += consumed;
        Some(sm)
    } else {
        None
    };

    let prog = if let Some(sm) = source_map {
        ProgramData::with_source_map(code, rodata, sm)
    } else {
        ProgramData::with_rodata(code, rodata)
    };

    Ok((prog, offset))
}

/// Deserialize library data.
fn deserialize_library_data(bytes: &[u8], _has_debug: bool) -> Result<(LibraryData, usize), SerializeError> {
    let mut offset = 0;

    // Library ID (u8 len + string)
    if bytes.is_empty() {
        return Err(SerializeError::UnexpectedEnd);
    }
    let id_len = bytes[0] as usize;
    offset += 1;

    if bytes.len() < offset + id_len {
        return Err(SerializeError::UnexpectedEnd);
    }
    let id = std::str::from_utf8(&bytes[offset..offset + id_len])
        .map_err(|_| SerializeError::InvalidUtf8)?
        .to_string();
    offset += id_len;

    // Command count
    if bytes.len() < offset + 2 {
        return Err(SerializeError::UnexpectedEnd);
    }
    let cmd_count = read_u16(&bytes[offset..])? as usize;
    offset += 2;

    let mut commands = Vec::with_capacity(cmd_count);

    for _ in 0..cmd_count {
        // Command name (u8 len + string)
        if bytes.len() < offset + 1 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let name_len = bytes[offset] as usize;
        offset += 1;

        if bytes.len() < offset + name_len {
            return Err(SerializeError::UnexpectedEnd);
        }
        let name = std::str::from_utf8(&bytes[offset..offset + name_len])
            .map_err(|_| SerializeError::InvalidUtf8)?
            .to_string();
        offset += name_len;

        // Rodata section
        if bytes.len() < offset + 4 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let rodata_len = read_u32(&bytes[offset..])? as usize;
        offset += 4;

        if bytes.len() < offset + rodata_len {
            return Err(SerializeError::UnexpectedEnd);
        }
        let rodata: Vec<u8> = bytes[offset..offset + rodata_len].to_vec();
        offset += rodata_len;

        // Bytecode
        if bytes.len() < offset + 4 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let code_len = read_u32(&bytes[offset..])? as usize;
        offset += 4;

        if bytes.len() < offset + code_len {
            return Err(SerializeError::UnexpectedEnd);
        }
        let code: Vec<u8> = bytes[offset..offset + code_len].to_vec();
        offset += code_len;

        commands.push(LibraryCommand::new(name, code, rodata));
    }

    Ok((LibraryData::with_commands(id, commands), offset))
}

/// Serialize a source map (includes source code).
///
/// Format:
/// - source_len: u32 (LE)
/// - source: UTF-8 bytes
/// - span_count: u16 (LE)
/// - per span:
///   - bytecode_offset: u32 (LE)
///   - source_start: u32 (LE)
///   - source_end: u32 (LE)
pub fn serialize_source_map(buf: &mut Vec<u8>, source_map: &SourceMap) {
    // Source code
    write_string(buf, &source_map.source);

    // Spans
    write_u16(buf, source_map.offsets.len() as u16);
    for (i, &bc_offset) in source_map.offsets.iter().enumerate() {
        write_u32(buf, bc_offset);
        write_u32(buf, source_map.spans[i].start().offset());
        write_u32(buf, source_map.spans[i].end().offset());
    }
}

/// Deserialize a source map (includes source code).
fn deserialize_source_map(bytes: &[u8]) -> Result<(SourceMap, usize), SerializeError> {
    let mut offset = 0;

    // Source code
    let (source, consumed) = read_string(bytes)?;
    offset += consumed;

    // Span count
    if bytes.len() < offset + 2 {
        return Err(SerializeError::UnexpectedEnd);
    }
    let span_count = read_u16(&bytes[offset..])? as usize;
    offset += 2;

    let mut source_map = SourceMap::with_source(source);
    source_map.offsets.reserve(span_count);
    source_map.spans.reserve(span_count);

    for _ in 0..span_count {
        if bytes.len() < offset + 12 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let bc_offset = read_u32(&bytes[offset..])?;
        let start = read_u32(&bytes[offset + 4..])?;
        let end = read_u32(&bytes[offset + 8..])?;
        offset += 12;

        source_map.offsets.push(bc_offset);
        source_map.spans.push(Span::new(Pos::new(start), Pos::new(end)));
    }

    Ok((source_map, offset))
}

// ============================================================================
// Helper functions
// ============================================================================

fn write_u16(buf: &mut Vec<u8>, val: u16) {
    buf.extend(&val.to_le_bytes());
}

fn write_u32(buf: &mut Vec<u8>, val: u32) {
    buf.extend(&val.to_le_bytes());
}

fn read_u16(bytes: &[u8]) -> Result<u16, SerializeError> {
    if bytes.len() < 2 {
        return Err(SerializeError::UnexpectedEnd);
    }
    Ok(u16::from_le_bytes(bytes[..2].try_into().unwrap()))
}

fn read_u32(bytes: &[u8]) -> Result<u32, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }
    Ok(u32::from_le_bytes(bytes[..4].try_into().unwrap()))
}

/// Write string with u32 length prefix.
fn write_string(buf: &mut Vec<u8>, s: &str) {
    write_u32(buf, s.len() as u32);
    buf.extend(s.as_bytes());
}

/// Write string with u16 length prefix.
fn write_string_u16(buf: &mut Vec<u8>, s: &str) {
    write_u16(buf, s.len() as u16);
    buf.extend(s.as_bytes());
}

/// Read string with u32 length prefix.
fn read_string(bytes: &[u8]) -> Result<(String, usize), SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let len = read_u32(bytes)? as usize;

    if bytes.len() < 4 + len {
        return Err(SerializeError::UnexpectedEnd);
    }

    let s = std::str::from_utf8(&bytes[4..4 + len])
        .map_err(|_| SerializeError::InvalidUtf8)?
        .to_string();

    Ok((s, 4 + len))
}

/// Read string with u16 length prefix.
fn read_string_u16(bytes: &[u8]) -> Result<(String, usize), SerializeError> {
    if bytes.len() < 2 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let len = read_u16(bytes)? as usize;

    if bytes.len() < 2 + len {
        return Err(SerializeError::UnexpectedEnd);
    }

    let s = std::str::from_utf8(&bytes[2..2 + len])
        .map_err(|_| SerializeError::InvalidUtf8)?
        .to_string();

    Ok((s, 2 + len))
}

// ============================================================================
// Directory packing (PACKDIR/UNPACKDIR)
// ============================================================================

/// Entry kind in packed directory.
const ENTRY_VAR: u8 = 0;
const ENTRY_SUBDIR: u8 = 1;

/// Pack a directory node into bytes.
///
/// Format:
/// - entry_count: u32
/// - per entry:
///   - kind: u8 (0=var, 1=subdir)
///   - name_len: u16 + name: UTF-8
///   - if var: serialized value
///   - if subdir: recursive packdir data
pub fn pack_directory(node: &DirNode) -> Vec<u8> {
    let mut buf = Vec::new();
    pack_directory_into(&mut buf, node);
    buf
}

fn pack_directory_into(buf: &mut Vec<u8>, node: &DirNode) {
    // Count total entries (vars + subdirs)
    let var_count = node.var_count();
    let subdir_count = node.subdir_count();
    let total = var_count + subdir_count;
    write_u32(buf, total as u32);

    // Write variables
    for (name, value) in node.vars_iter() {
        buf.push(ENTRY_VAR);
        write_string_u16(buf, name);
        serialize_into(buf, value, &SerializeOptions::default());
    }

    // Write subdirectories (recursively)
    for (name, subnode) in node.subdirs_iter() {
        buf.push(ENTRY_SUBDIR);
        write_string_u16(buf, name);
        pack_directory_into(buf, subnode);
    }
}

/// Get entry names from packed directory without fully unpacking.
pub fn packinfo(bytes: &[u8]) -> Result<Vec<String>, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let entry_count = read_u32(bytes)? as usize;
    let mut offset = 4;
    let mut names = Vec::with_capacity(entry_count);

    for _ in 0..entry_count {
        if bytes.len() < offset + 1 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let kind = bytes[offset];
        offset += 1;

        // Read name
        let (name, consumed) = read_string_u16(&bytes[offset..])?;
        names.push(name);
        offset += consumed;

        // Skip the payload
        match kind {
            ENTRY_VAR => {
                // Deserialize to skip (we need to know the length)
                let (_, consumed) = deserialize_value(&bytes[offset..])?;
                offset += consumed;
            }
            ENTRY_SUBDIR => {
                // Recursively skip subdirectory
                let consumed = skip_packed_directory(&bytes[offset..])?;
                offset += consumed;
            }
            _ => return Err(SerializeError::InvalidTag(kind)),
        }
    }

    Ok(names)
}

/// Skip a packed directory, returning bytes consumed.
fn skip_packed_directory(bytes: &[u8]) -> Result<usize, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let entry_count = read_u32(bytes)? as usize;
    let mut offset = 4;

    for _ in 0..entry_count {
        if bytes.len() < offset + 1 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let kind = bytes[offset];
        offset += 1;

        // Skip name
        let (_, consumed) = read_string_u16(&bytes[offset..])?;
        offset += consumed;

        // Skip payload
        match kind {
            ENTRY_VAR => {
                let (_, consumed) = deserialize_value(&bytes[offset..])?;
                offset += consumed;
            }
            ENTRY_SUBDIR => {
                let consumed = skip_packed_directory(&bytes[offset..])?;
                offset += consumed;
            }
            _ => return Err(SerializeError::InvalidTag(kind)),
        }
    }

    Ok(offset)
}

/// Unpack bytes into a directory node.
///
/// Returns an error if any name conflicts with existing entries in the node.
pub fn unpack_directory(bytes: &[u8], node: &mut DirNode) -> Result<usize, SerializeError> {
    unpack_directory_inner(bytes, node, false)
}

/// Unpack bytes into a directory node, optionally checking for conflicts.
fn unpack_directory_inner(
    bytes: &[u8],
    node: &mut DirNode,
    skip_conflict_check: bool,
) -> Result<usize, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }

    let entry_count = read_u32(bytes)? as usize;
    let mut offset = 4;

    // First pass: check for conflicts (unless skipped)
    if !skip_conflict_check {
        let mut check_offset = 4;
        for _ in 0..entry_count {
            if bytes.len() < check_offset + 1 {
                return Err(SerializeError::UnexpectedEnd);
            }
            let kind = bytes[check_offset];
            check_offset += 1;

            let (name, consumed) = read_string_u16(&bytes[check_offset..])?;
            check_offset += consumed;

            if node.name_exists(&name) {
                return Err(SerializeError::InvalidUtf8); // Reuse error for conflict
            }

            // Skip payload
            match kind {
                ENTRY_VAR => {
                    let (_, consumed) = deserialize_value(&bytes[check_offset..])?;
                    check_offset += consumed;
                }
                ENTRY_SUBDIR => {
                    let consumed = skip_packed_directory(&bytes[check_offset..])?;
                    check_offset += consumed;
                }
                _ => return Err(SerializeError::InvalidTag(kind)),
            }
        }
    }

    // Second pass: actually unpack
    for _ in 0..entry_count {
        if bytes.len() < offset + 1 {
            return Err(SerializeError::UnexpectedEnd);
        }
        let kind = bytes[offset];
        offset += 1;

        let (name, consumed) = read_string_u16(&bytes[offset..])?;
        offset += consumed;

        match kind {
            ENTRY_VAR => {
                let (value, consumed) = deserialize_value(&bytes[offset..])?;
                offset += consumed;
                node.store(name, value);
            }
            ENTRY_SUBDIR => {
                let subnode = node.ensure_subdir(&name);
                let consumed = unpack_directory_inner(&bytes[offset..], subnode, true)?;
                offset += consumed;
            }
            _ => return Err(SerializeError::InvalidTag(kind)),
        }
    }

    Ok(offset)
}

/// Error type for unpack conflicts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnpackConflictError {
    pub name: String,
}

impl fmt::Display for UnpackConflictError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNPACKDIR: name '{}' already exists", self.name)
    }
}

/// Unpack bytes into a directory node, returning conflict error with name.
pub fn unpack_directory_checked(
    bytes: &[u8],
    node: &mut DirNode,
) -> Result<usize, Result<SerializeError, UnpackConflictError>> {
    if bytes.len() < 4 {
        return Err(Ok(SerializeError::UnexpectedEnd));
    }

    let entry_count = read_u32(bytes).map_err(Ok)? as usize;
    let mut offset = 4;

    // First pass: check for conflicts
    let mut check_offset = 4;
    for _ in 0..entry_count {
        if bytes.len() < check_offset + 1 {
            return Err(Ok(SerializeError::UnexpectedEnd));
        }
        let kind = bytes[check_offset];
        check_offset += 1;

        let (name, consumed) = read_string_u16(&bytes[check_offset..]).map_err(Ok)?;
        check_offset += consumed;

        if node.name_exists(&name) {
            return Err(Err(UnpackConflictError { name }));
        }

        // Skip payload
        match kind {
            ENTRY_VAR => {
                let (_, consumed) = deserialize_value(&bytes[check_offset..]).map_err(Ok)?;
                check_offset += consumed;
            }
            ENTRY_SUBDIR => {
                let consumed = skip_packed_directory(&bytes[check_offset..]).map_err(Ok)?;
                check_offset += consumed;
            }
            _ => return Err(Ok(SerializeError::InvalidTag(kind))),
        }
    }

    // Second pass: actually unpack
    for _ in 0..entry_count {
        if bytes.len() < offset + 1 {
            return Err(Ok(SerializeError::UnexpectedEnd));
        }
        let kind = bytes[offset];
        offset += 1;

        let (name, consumed) = read_string_u16(&bytes[offset..]).map_err(Ok)?;
        offset += consumed;

        match kind {
            ENTRY_VAR => {
                let (value, consumed) = deserialize_value(&bytes[offset..]).map_err(Ok)?;
                offset += consumed;
                node.store(name, value);
            }
            ENTRY_SUBDIR => {
                let subnode = node.ensure_subdir(&name);
                let consumed = unpack_directory_inner(&bytes[offset..], subnode, true)
                    .map_err(Ok)?;
                offset += consumed;
            }
            _ => return Err(Ok(SerializeError::InvalidTag(kind))),
        }
    }

    Ok(offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(value: Value) -> Value {
        let bytes = serialize_value(&value);
        let (result, consumed) = deserialize_value(&bytes).unwrap();
        assert_eq!(consumed, bytes.len());
        result
    }

    #[test]
    fn test_integer() {
        let result = round_trip(Value::Integer(42));
        assert_eq!(result, Value::Integer(42));

        let result = round_trip(Value::Integer(-1000000));
        assert_eq!(result, Value::Integer(-1000000));

        let result = round_trip(Value::Integer(i64::MAX));
        assert_eq!(result, Value::Integer(i64::MAX));

        let result = round_trip(Value::Integer(i64::MIN));
        assert_eq!(result, Value::Integer(i64::MIN));
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_real() {
        let result = round_trip(Value::Real(3.14159));
        match result {
            Value::Real(r) => assert!((r - 3.14159).abs() < 1e-10),
            _ => panic!("expected real"),
        }

        // Infinity
        let result = round_trip(Value::Real(f64::INFINITY));
        match result {
            Value::Real(r) => assert!(r.is_infinite() && r.is_sign_positive()),
            _ => panic!("expected real"),
        }

        // Negative infinity
        let result = round_trip(Value::Real(f64::NEG_INFINITY));
        match result {
            Value::Real(r) => assert!(r.is_infinite() && r.is_sign_negative()),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn test_string() {
        let result = round_trip(Value::string("hello"));
        assert_eq!(result, Value::string("hello"));

        // Empty string
        let result = round_trip(Value::string(""));
        assert_eq!(result, Value::string(""));

        // Unicode
        let result = round_trip(Value::string("héllo 世界 🎉"));
        assert_eq!(result, Value::string("héllo 世界 🎉"));
    }

    #[test]
    fn test_list_empty() {
        let result = round_trip(Value::list(Vec::<Value>::new()));
        match result {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_list_primitives() {
        let list = Value::list(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
        let result = round_trip(list);
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Integer(1));
                assert_eq!(items[1], Value::Integer(2));
                assert_eq!(items[2], Value::Integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_list_mixed() {
        let list = Value::list(vec![
            Value::Integer(42),
            Value::Real(3.14),
            Value::string("hello"),
        ]);
        let result = round_trip(list);
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::Integer(42));
                match &items[1] {
                    Value::Real(r) => assert!((r - 3.14).abs() < 1e-10),
                    _ => panic!("expected real"),
                }
                assert_eq!(items[2], Value::string("hello"));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_list_nested() {
        let inner = Value::list(vec![Value::Integer(1), Value::Integer(2)]);
        let outer = Value::list(vec![inner, Value::Integer(3)]);
        let result = round_trip(outer);

        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                match &items[0] {
                    Value::List(inner_items) => {
                        assert_eq!(inner_items.len(), 2);
                        assert_eq!(inner_items[0], Value::Integer(1));
                        assert_eq!(inner_items[1], Value::Integer(2));
                    }
                    _ => panic!("expected nested list"),
                }
                assert_eq!(items[1], Value::Integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn test_program_empty() {
        let prog = Value::program(Vec::<u8>::new());
        let result = round_trip(prog);

        match result {
            Value::Program(data) => {
                assert!(data.code.is_empty());
                assert!(data.rodata.is_empty());
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn test_program_with_code() {
        let prog = Value::program(vec![0x12, 0x34, 0x56, 0x78]);
        let result = round_trip(prog);

        match result {
            Value::Program(data) => {
                assert_eq!(data.code.as_ref(), &[0x12, 0x34, 0x56, 0x78]);
                assert!(data.rodata.is_empty());
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn test_program_with_rodata() {
        let prog = Value::program_with_rodata(
            vec![0xAB, 0xCD],
            vec![b'f', b'o', b'o', b'b', b'a', b'r'],  // "foobar" as rodata
        );
        let result = round_trip(prog);

        match result {
            Value::Program(data) => {
                assert_eq!(data.code.as_ref(), &[0xAB, 0xCD]);
                assert_eq!(data.rodata.as_ref(), b"foobar");
                assert!(data.source_map.is_none());
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn test_program_with_source_map() {
        // Create a program with source map
        let source = "2 * DUP +";
        let mut source_map = SourceMap::with_source(source);
        source_map.offsets.push(0);
        source_map.spans.push(Span::new(Pos::new(0), Pos::new(1)));  // "2"
        source_map.offsets.push(3);
        source_map.spans.push(Span::new(Pos::new(2), Pos::new(3)));  // "*"

        let prog = Value::program_with_source_map(
            vec![0x01, 0x02, 0x03],
            vec![b'x'],  // rodata containing "x"
            source_map,
        );

        // Serialize with debug info
        let opts = SerializeOptions { include_source_maps: true };
        let bytes = serialize_value_with_options(&prog, opts);

        // Check that debug flag is set (TAG_PROGRAM | DEBUG_FLAG = 0x15)
        assert_eq!(bytes[0], TAG_PROGRAM | DEBUG_FLAG);

        // Deserialize
        let (result, consumed) = deserialize_value(&bytes).unwrap();
        assert_eq!(consumed, bytes.len());

        match result {
            Value::Program(data) => {
                assert_eq!(data.code.as_ref(), &[0x01, 0x02, 0x03]);
                assert_eq!(data.rodata.as_ref(), b"x");

                // Verify source map was preserved
                assert!(data.source_map.is_some());
                let sm = data.source_map.as_ref().unwrap();
                assert_eq!(sm.source, "2 * DUP +");
                assert_eq!(sm.offsets.len(), 2);
                assert_eq!(sm.offsets[0], 0);
                assert_eq!(sm.offsets[1], 3);
                assert_eq!(sm.get_source_slice(&sm.spans[0]), Some("2"));
                assert_eq!(sm.get_source_slice(&sm.spans[1]), Some("*"));
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn test_program_without_debug_flag() {
        // Create a program with source map but serialize without debug flag
        let source = "test";
        let mut source_map = SourceMap::with_source(source);
        source_map.offsets.push(0);
        source_map.spans.push(Span::new(Pos::new(0), Pos::new(4)));

        let prog = Value::program_with_source_map(
            vec![0xFF],
            Vec::<u8>::new(),  // empty rodata
            source_map,
        );

        // Serialize WITHOUT debug info
        let opts = SerializeOptions { include_source_maps: false };
        let bytes = serialize_value_with_options(&prog, opts);

        // Check that debug flag is NOT set
        assert_eq!(bytes[0], TAG_PROGRAM);

        // Deserialize - source map should be gone
        let (result, _) = deserialize_value(&bytes).unwrap();
        match result {
            Value::Program(data) => {
                assert_eq!(data.code.as_ref(), &[0xFF]);
                assert!(data.source_map.is_none(), "source map should not be present when debug flag is off");
            }
            _ => panic!("expected program"),
        }
    }

    #[test]
    fn test_symbolic() {
        use crate::symbolic::SymExpr;

        let expr = SymExpr::var("X");
        let result = round_trip(Value::symbolic(expr));

        match result {
            Value::Symbolic(e) => {
                assert_eq!(format!("{}", e), "X");
            }
            _ => panic!("expected symbolic"),
        }
    }

    #[test]
    fn test_source_map_roundtrip() {
        let source = "1 2 + DUP * SWAP DROP";
        let mut source_map = SourceMap::with_source(source);
        source_map.offsets.push(0);
        source_map.spans.push(Span::new(Pos::new(0), Pos::new(1)));   // "1"
        source_map.offsets.push(5);
        source_map.spans.push(Span::new(Pos::new(4), Pos::new(5)));   // "+"

        let mut buf = Vec::new();
        serialize_source_map(&mut buf, &source_map);

        let (result, consumed) = deserialize_source_map(&buf).unwrap();
        assert_eq!(consumed, buf.len());
        assert_eq!(result.source, source);
        assert_eq!(result.offsets.len(), 2);
        assert_eq!(result.offsets[0], 0);
        assert_eq!(result.offsets[1], 5);
        assert_eq!(result.spans[0].start().offset(), 0);
        assert_eq!(result.spans[0].end().offset(), 1);
        assert_eq!(result.spans[1].start().offset(), 4);
        assert_eq!(result.spans[1].end().offset(), 5);

        // Test get_source_slice
        assert_eq!(result.get_source_slice(&result.spans[0]), Some("1"));
        assert_eq!(result.get_source_slice(&result.spans[1]), Some("+"));
    }

    #[test]
    fn test_error_unexpected_end() {
        let bytes = [TAG_REAL]; // Missing 8 bytes
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::UnexpectedEnd)));
    }

    #[test]
    fn test_error_invalid_tag() {
        let bytes = [0xFF, 0, 0, 0];
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::InvalidTag(0xFF))));
    }

    #[test]
    fn test_error_invalid_utf8() {
        let mut bytes = vec![TAG_STRING];
        bytes.extend(&4u32.to_le_bytes()); // length = 4
        bytes.extend(&[0xFF, 0xFE, 0xFF, 0xFE]); // invalid UTF-8
        let result = deserialize_value(&bytes);
        assert!(matches!(result, Err(SerializeError::InvalidUtf8)));
    }

    #[test]
    fn test_error_empty() {
        let result = deserialize_value(&[]);
        assert!(matches!(result, Err(SerializeError::UnexpectedEnd)));
    }

    #[test]
    fn test_library_roundtrip() {
        use crate::value::{LibraryCommand, LibraryData};

        let lib = LibraryData::with_commands(
            "TEST",
            vec![
                LibraryCommand::new("DBL", vec![0x01, 0x02], Vec::<u8>::new()),
                LibraryCommand::new(
                    "TRIPLE",
                    vec![0x03, 0x04, 0x05],
                    vec![b'x'],  // rodata containing "x"
                ),
            ],
        );
        let value = Value::library(lib);
        let result = round_trip(value);

        match result {
            Value::Library(lib) => {
                assert_eq!(lib.id, "TEST");
                assert_eq!(lib.commands.len(), 2);
                assert_eq!(lib.commands[0].name, "DBL");
                assert_eq!(lib.commands[0].code.as_ref(), &[0x01, 0x02]);
                assert!(lib.commands[0].rodata.is_empty());
                assert_eq!(lib.commands[1].name, "TRIPLE");
                assert_eq!(lib.commands[1].code.as_ref(), &[0x03, 0x04, 0x05]);
                assert_eq!(lib.commands[1].rodata.as_ref(), b"x");
            }
            _ => panic!("expected library"),
        }
    }

    #[test]
    fn test_library_empty_commands() {
        use crate::value::LibraryData;

        let lib = LibraryData::new("EMPTY");
        let value = Value::library(lib);
        let result = round_trip(value);

        match result {
            Value::Library(lib) => {
                assert_eq!(lib.id, "EMPTY");
                assert!(lib.commands.is_empty());
            }
            _ => panic!("expected library"),
        }
    }

    #[test]
    fn test_bytes_roundtrip() {
        let data = vec![0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD];
        let result = round_trip(Value::bytes(data.clone()));

        match result {
            Value::Bytes(b) => {
                assert_eq!(b.as_ref(), &data[..]);
            }
            _ => panic!("expected bytes"),
        }
    }

    #[test]
    fn test_bytes_empty() {
        let result = round_trip(Value::bytes(Vec::<u8>::new()));

        match result {
            Value::Bytes(b) => {
                assert!(b.is_empty());
            }
            _ => panic!("expected bytes"),
        }
    }
}
