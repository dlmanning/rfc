//! Library for user library pointers (LIBPTR).
//!
//! This module provides support for user-defined libraries:
//! - LIBPTR objects that reference commands in attached libraries
//! - LIBRARY objects that contain packaged command collections
//!
//! ## Storage Locations
//!
//! Attached libraries are stored in: `.SETTINGS.LIB.<LIBID>`
//! Private library data is stored in: `.SETTINGS.LIBDATA.<LIBID>.<varname>`
//!
//! ## LIBPTR Format
//!
//! A LIBPTR is a 2-word object:
//! ```text
//! Word 0: Library ID (4-character identifier encoded as u32)
//! Word 1: Command index within the library
//! ```
//!
//! ## LIBRARY Format
//!
//! A library contains:
//! ```text
//! Word 0: Library ID
//! Word 1: Command count (N)
//! Words 2..N+1: Offset table (offset from start to each command entry)
//! Words N+2...: Command entries (name, token info, help, object)
//! ```

use std::collections::HashMap;

use rpl_core::{TypeId, Word};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::Value;

// ============================================================================
// User Library Registry (for compile-time command resolution)
// ============================================================================

/// Metadata about an attached user library (for compile-time resolution).
///
/// This stores only the command names, not the actual library objects.
/// The actual objects remain in the VM's global directory.
#[derive(Debug, Clone)]
pub struct AttachedLibraryMeta {
    /// The library ID (encoded 1-4 character name)
    pub lib_id: u32,
    /// Command names indexed by command index
    pub commands: Vec<String>,
}

/// Registry of attached user libraries for compile-time command resolution.
///
/// This registry is shared between the compiler (for resolving command names
/// to LIBPTR objects) and the VM (for populating during ATTACH/DETACH).
#[derive(Debug, Default)]
pub struct UserLibraryRegistry {
    /// Map from library ID to metadata
    attached: HashMap<u32, AttachedLibraryMeta>,
}

impl UserLibraryRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            attached: HashMap::new(),
        }
    }

    /// Register an attached library's command names.
    ///
    /// Called when ATTACH is executed.
    pub fn register(&mut self, lib_id: u32, commands: Vec<String>) {
        self.attached.insert(
            lib_id,
            AttachedLibraryMeta { lib_id, commands },
        );
    }

    /// Unregister a library (called when DETACH is executed).
    pub fn unregister(&mut self, lib_id: u32) {
        self.attached.remove(&lib_id);
    }

    /// Look up a command by name across all attached libraries.
    ///
    /// Returns `Some((lib_id, cmd_index))` if found, `None` otherwise.
    /// The lookup is case-insensitive.
    pub fn lookup_command(&self, name: &str) -> Option<(u32, u32)> {
        let upper = name.to_ascii_uppercase();
        for meta in self.attached.values() {
            for (index, cmd_name) in meta.commands.iter().enumerate() {
                if cmd_name.to_ascii_uppercase() == upper {
                    return Some((meta.lib_id, index as u32));
                }
            }
        }
        None
    }

    /// Get command name for decompilation.
    ///
    /// Returns the command name if the library is attached and the index is valid.
    pub fn get_command_name(&self, lib_id: u32, index: u32) -> Option<&str> {
        self.attached
            .get(&lib_id)
            .and_then(|meta| meta.commands.get(index as usize))
            .map(|s| s.as_str())
    }

    /// Check if a library is attached.
    pub fn is_attached(&self, lib_id: u32) -> bool {
        self.attached.contains_key(&lib_id)
    }

    /// Get all attached library IDs.
    pub fn attached_libraries(&self) -> impl Iterator<Item = u32> + '_ {
        self.attached.keys().copied()
    }
}

/// Library for user library pointers.
pub struct LibPtrLib;

impl LibPtrLib {
    /// Library ID for libptr.
    pub const ID: LibraryId = LibraryId::new(102);

    // Command IDs
    const CMD_CRLIB: u16 = 0;
    const CMD_ATTACH: u16 = 1;
    const CMD_DETACH: u16 = 2;
    const CMD_LIBMENU: u16 = 3;
    const CMD_LIBSTO: u16 = 4;
    const CMD_LIBRCL: u16 = 5;
    const CMD_LIBDEFRCL: u16 = 6;
    const CMD_LIBCLEAR: u16 = 7;
}

/// Encode a library ID from a string (1-4 ASCII alphanumeric characters).
///
/// The ID is stored in little-endian order, so "MATH" becomes:
/// byte 0 = 'M', byte 1 = 'A', byte 2 = 'T', byte 3 = 'H'
pub fn encode_lib_id(name: &str) -> Result<u32, &'static str> {
    if name.is_empty() || name.len() > 4 {
        return Err("Library ID must be 1-4 characters");
    }
    for c in name.chars() {
        if !c.is_ascii_alphanumeric() {
            return Err("Library ID must be alphanumeric");
        }
    }
    let mut id: u32 = 0;
    for (i, byte) in name.bytes().enumerate() {
        id |= (byte as u32) << (i * 8);
    }
    Ok(id)
}

/// Decode a library ID to a string.
pub fn decode_lib_id(id: u32) -> String {
    let mut s = String::with_capacity(4);
    for i in 0..4 {
        let byte = ((id >> (i * 8)) & 0xFF) as u8;
        if byte != 0 {
            s.push(byte as char);
        }
    }
    s
}

/// Create a LIBPTR value.
pub fn make_libptr(lib_id: u32, cmd_index: u32) -> Value {
    Value::Object {
        type_id: TypeId::LIBPTR,
        data: vec![lib_id, cmd_index],
    }
}

/// Extract library ID and command index from a LIBPTR value.
pub fn parse_libptr(value: &Value) -> Option<(u32, u32)> {
    match value {
        Value::Object { type_id, data } if *type_id == TypeId::LIBPTR && data.len() >= 2 => {
            Some((data[0], data[1]))
        }
        _ => None,
    }
}

/// Get the storage path for an attached library.
pub fn lib_storage_path(lib_id: u32) -> String {
    format!(".SETTINGS.LIB.{}", decode_lib_id(lib_id))
}

/// Get the storage path for library private data.
pub fn lib_data_path(lib_id: u32, var_name: &str) -> String {
    format!(".SETTINGS.LIBDATA.{}.{}", decode_lib_id(lib_id), var_name)
}

/// Encode a command name into words (length-prefixed, word-aligned).
///
/// Format: [length_in_bytes: u32] [packed_bytes...]
/// Bytes are packed little-endian, 4 bytes per word, padded with zeros.
fn encode_command_name(name: &str) -> Vec<Word> {
    let bytes = name.as_bytes();
    let len = bytes.len();
    let word_count = len.div_ceil(4);

    let mut result = Vec::with_capacity(1 + word_count);
    result.push(len as u32);

    for chunk in bytes.chunks(4) {
        let mut word: u32 = 0;
        for (i, &byte) in chunk.iter().enumerate() {
            word |= (byte as u32) << (i * 8);
        }
        result.push(word);
    }

    result
}

/// Decode a command name from words starting at the given offset.
///
/// Returns (name, words_consumed) or None if invalid.
fn decode_command_name(data: &[Word], offset: usize) -> Option<(String, usize)> {
    if offset >= data.len() {
        return None;
    }

    let len = data[offset] as usize;
    let word_count = len.div_ceil(4);

    if offset + 1 + word_count > data.len() {
        return None;
    }

    let mut bytes = Vec::with_capacity(len);
    for i in 0..word_count {
        let word = data[offset + 1 + i];
        for j in 0..4 {
            if bytes.len() < len {
                bytes.push(((word >> (j * 8)) & 0xFF) as u8);
            }
        }
    }

    String::from_utf8(bytes).ok().map(|s| (s, 1 + word_count))
}

/// Build a LIBRARY object from named commands.
///
/// Input: A list of (name, program) pairs
/// Output: A LIBRARY Value with the proper internal format
///
/// Library format:
/// - Word 0: Library ID
/// - Word 1: Command count
/// - Words 2..N+1: Offset table (offset from start to each command entry)
/// - Words N+2...: Command entries, each containing:
///     - Name length (1 word, in bytes)
///     - Name bytes (packed into words, padded to word boundary)
///     - Program prolog + bytecode
pub fn build_library(lib_id: u32, commands: &[(String, Value)]) -> Result<Value, &'static str> {
    let cmd_count = commands.len();

    // First pass: serialize each command (name + program) and calculate offsets
    let mut serialized_commands: Vec<Vec<Word>> = Vec::with_capacity(cmd_count);

    for (name, program) in commands {
        let mut cmd_data = encode_command_name(name);

        match program {
            Value::Program { code, .. } => {
                // Serialize as a PROGRAM prolog + data
                let prolog = rpl_core::make_prolog(TypeId::PROGRAM.as_u16(), code.len() as u16);
                cmd_data.push(prolog);
                cmd_data.extend(code.iter());
            }
            Value::Object { type_id, data } if *type_id == TypeId::PROGRAM => {
                // Already an object - serialize with prolog
                let prolog = rpl_core::make_prolog(type_id.as_u16(), data.len() as u16);
                cmd_data.push(prolog);
                cmd_data.extend(data.iter());
            }
            _ => {
                return Err("CRLIB requires a list of programs");
            }
        }

        serialized_commands.push(cmd_data);
    }

    // Calculate header size: lib_id + cmd_count + offset_table
    let header_size = 2 + cmd_count;

    // Calculate offsets for each command
    let mut offsets = Vec::with_capacity(cmd_count);
    let mut current_offset = header_size;
    for cmd_data in &serialized_commands {
        offsets.push(current_offset as u32);
        current_offset += cmd_data.len();
    }

    // Build the library data
    let mut lib_data = Vec::with_capacity(current_offset);
    lib_data.push(lib_id);
    lib_data.push(cmd_count as u32);
    lib_data.extend(offsets);
    for cmd_data in serialized_commands {
        lib_data.extend(cmd_data);
    }

    Ok(Value::Object {
        type_id: TypeId::LIBRARY,
        data: lib_data,
    })
}

/// Parse a library object and extract the command name and program at a given index.
///
/// Library format:
/// - Word 0: Library ID
/// - Word 1: Command count
/// - Words 2..N+1: Offset table
/// - Words N+2...: Command entries (name + program each)
///
/// Returns (name, program) if found.
pub fn get_library_command_with_name(lib_data: &[Word], cmd_index: u32) -> Option<(String, Value)> {
    if lib_data.len() < 2 {
        return None;
    }

    let _lib_id = lib_data[0];
    let cmd_count = lib_data[1] as usize;

    if cmd_index as usize >= cmd_count {
        return None;
    }

    // Offset table starts at word 2
    if lib_data.len() < 2 + cmd_count {
        return None;
    }

    let offset = lib_data[2 + cmd_index as usize] as usize;

    if offset >= lib_data.len() {
        return None;
    }

    // Parse command name
    let (name, name_words) = decode_command_name(lib_data, offset)?;

    // Parse program object after the name
    let prog_offset = offset + name_words;
    if prog_offset >= lib_data.len() {
        return None;
    }

    let remaining = &lib_data[prog_offset..];
    if remaining.is_empty() {
        return None;
    }

    // Check if it's a prolog word
    if rpl_core::is_prolog(remaining[0]) {
        let type_id_raw = rpl_core::extract_type(remaining[0]);
        let size = rpl_core::extract_size(remaining[0]) as usize;

        if remaining.len() < 1 + size {
            return None;
        }

        let obj_data: Vec<Word> = remaining[1..1 + size].to_vec();
        let type_id = TypeId::new(type_id_raw);

        // If it's a PROGRAM, return as Value::Program for proper execution
        let value = if type_id == TypeId::PROGRAM {
            Value::program(obj_data)
        } else {
            Value::Object {
                type_id,
                data: obj_data,
            }
        };
        return Some((name, value));
    }

    None
}

/// Parse a library object and extract the command at a given index.
///
/// This is a convenience wrapper around `get_library_command_with_name`
/// that discards the name.
pub fn get_library_command(lib_data: &[Word], cmd_index: u32) -> Option<Value> {
    get_library_command_with_name(lib_data, cmd_index).map(|(_, program)| program)
}

/// Get all command names from a library.
pub fn get_library_command_names(lib_data: &[Word]) -> Vec<String> {
    if lib_data.len() < 2 {
        return Vec::new();
    }

    let cmd_count = lib_data[1] as usize;
    let mut names = Vec::with_capacity(cmd_count);

    for i in 0..cmd_count {
        if let Some((name, _)) = get_library_command_with_name(lib_data, i as u32) {
            names.push(name);
        }
    }

    names
}

impl Library for LibPtrLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "LibPtr"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        // First, check built-in commands
        let len = match upper.as_str() {
            "CRLIB" => 5,
            "ATTACH" => 6,
            "DETACH" => 6,
            "LIBMENU" => 7,
            "LIBSTO" => 6,
            "LIBRCL" => 6,
            "LIBDEFRCL" => 9,
            "LIBCLEAR" => 8,
            _ => 0, // Not a built-in command, check user libraries below
        };

        if len > 0 && text.len() == len {
            return ProbeResult::Match {
                info: TokenInfo::atom(len as u8),
                semantic: SemanticKind::Command,
            };
        }

        // Check if token matches a user library command
        if let Some(registry) = ctx.user_lib_registry()
            && registry.lookup_command(text).is_some() {
                return ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Command,
                };
            }

        ProbeResult::NoMatch
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        // First, check built-in commands
        let cmd = match upper.as_str() {
            "CRLIB" => Some(Self::CMD_CRLIB),
            "ATTACH" => Some(Self::CMD_ATTACH),
            "DETACH" => Some(Self::CMD_DETACH),
            "LIBMENU" => Some(Self::CMD_LIBMENU),
            "LIBSTO" => Some(Self::CMD_LIBSTO),
            "LIBRCL" => Some(Self::CMD_LIBRCL),
            "LIBDEFRCL" => Some(Self::CMD_LIBDEFRCL),
            "LIBCLEAR" => Some(Self::CMD_LIBCLEAR),
            _ => None,
        };

        if let Some(cmd) = cmd {
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            return CompileResult::Ok;
        }

        // Check if token is a user library command
        if let Some(registry) = ctx.user_lib_registry()
            && let Some((lib_id, cmd_index)) = registry.lookup_command(text) {
                // Emit LIBPTR prolog: [prolog][lib_id][cmd_index]
                ctx.emit_prolog(TypeId::LIBPTR.as_u16(), 2);
                ctx.emit(lib_id);
                ctx.emit(cmd_index);
                return CompileResult::Ok;
            }

        CompileResult::NoMatch
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_ATTACH => {
                // Pop library object from stack
                let lib_obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Validate it's a library and extract lib_id and data
                let (lib_id, lib_data) = match &lib_obj {
                    Value::Object { type_id, data }
                        if *type_id == TypeId::LIBRARY && !data.is_empty() =>
                    {
                        (data[0], data.clone())
                    }
                    _ => return ExecuteResult::Error("Expected library object".into()),
                };

                // Extract command names for compile-time resolution
                let command_names = get_library_command_names(&lib_data);

                // Register the library's command names
                ctx.user_libs_mut().register(lib_id, command_names);

                // Store in .SETTINGS.LIB.<LIBID>
                let path = lib_storage_path(lib_id);
                ctx.store(path, lib_obj);

                ExecuteResult::Ok
            }

            Self::CMD_DETACH => {
                // Pop library ID from stack
                let lib_id = match ctx.pop() {
                    Ok(Value::String(s)) => match encode_lib_id(&s) {
                        Ok(id) => id,
                        Err(e) => return ExecuteResult::Error(e.into()),
                    },
                    Ok(_) => return ExecuteResult::Error("Expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Unregister the library's command names
                ctx.user_libs_mut().unregister(lib_id);

                // Remove from .SETTINGS.LIB.<LIBID>
                let path = lib_storage_path(lib_id);
                ctx.purge(&path);

                ExecuteResult::Ok
            }

            Self::CMD_CRLIB => {
                // CRLIB: ( { { "name" <<prog>> } ... } 'LIBID' → library )
                // Pop library ID string
                let lib_id_str = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("CRLIB: expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Encode the library ID
                let lib_id = match encode_lib_id(&lib_id_str) {
                    Ok(id) => id,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Pop the list of command entries
                let entries = match ctx.pop() {
                    Ok(Value::List(items)) => items,
                    Ok(_) => return ExecuteResult::Error("CRLIB: expected list of command entries".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Parse each entry as { "name" <<program>> }
                let mut commands: Vec<(String, Value)> = Vec::with_capacity(entries.len());
                for (i, entry) in entries.iter().enumerate() {
                    match entry {
                        Value::List(pair) if pair.len() == 2 => {
                            // Extract name
                            let name = match &pair[0] {
                                Value::String(s) => s.clone(),
                                _ => return ExecuteResult::Error(
                                    format!("CRLIB: entry {} must have string name as first element", i)
                                ),
                            };
                            // Extract program
                            let program = pair[1].clone();
                            match &program {
                                Value::Program { .. } => {}
                                Value::Object { type_id, .. } if *type_id == TypeId::PROGRAM => {}
                                _ => return ExecuteResult::Error(
                                    format!("CRLIB: entry {} must have program as second element", i)
                                ),
                            }
                            commands.push((name, program));
                        }
                        _ => return ExecuteResult::Error(
                            format!("CRLIB: entry {} must be a 2-element list {{ \"name\" <<program>> }}", i)
                        ),
                    }
                }

                // Build the library
                match build_library(lib_id, &commands) {
                    Ok(library) => {
                        if ctx.push(library).is_err() {
                            return ExecuteResult::Error("Stack overflow".into());
                        }
                        ExecuteResult::Ok
                    }
                    Err(e) => ExecuteResult::Error(e.into()),
                }
            }

            Self::CMD_LIBMENU => {
                // LIBMENU: ( 'LIBID' n → )
                // Display library commands as a soft-key menu.
                // Requires command names in library format (not yet implemented).
                ExecuteResult::Error("LIBMENU not yet implemented".into())
            }

            Self::CMD_LIBSTO => {
                // LIBSTO: ( obj 'LIBID' 'name' → )
                // Store object in library's private namespace
                let var_name = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBSTO: expected variable name string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id_str = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBSTO: expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id = match encode_lib_id(&lib_id_str) {
                    Ok(id) => id,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                let obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Store in .SETTINGS.LIBDATA.<LIBID>.<name>
                let path = lib_data_path(lib_id, &var_name);
                ctx.store(path, obj);

                ExecuteResult::Ok
            }

            Self::CMD_LIBRCL => {
                // LIBRCL: ( 'LIBID' 'name' → obj )
                // Recall object from library's private namespace
                let var_name = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBRCL: expected variable name string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id_str = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBRCL: expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id = match encode_lib_id(&lib_id_str) {
                    Ok(id) => id,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Recall from .SETTINGS.LIBDATA.<LIBID>.<name>
                let path = lib_data_path(lib_id, &var_name);
                match ctx.recall(&path) {
                    Some(value) => {
                        let value = value.clone();
                        if ctx.push(value).is_err() {
                            return ExecuteResult::Error("Stack overflow".into());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Variable '{}' not found in library '{}'", var_name, lib_id_str)),
                }
            }

            Self::CMD_LIBDEFRCL => {
                // LIBDEFRCL: ( default 'LIBID' 'name' → obj )
                // Recall object from library's private namespace, or use default
                let var_name = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBDEFRCL: expected variable name string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id_str = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBDEFRCL: expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id = match encode_lib_id(&lib_id_str) {
                    Ok(id) => id,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                let default_value = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                // Recall from .SETTINGS.LIBDATA.<LIBID>.<name>, or use default
                let path = lib_data_path(lib_id, &var_name);
                let result = match ctx.recall(&path) {
                    Some(value) => value.clone(),
                    None => default_value,
                };

                if ctx.push(result).is_err() {
                    return ExecuteResult::Error("Stack overflow".into());
                }
                ExecuteResult::Ok
            }

            Self::CMD_LIBCLEAR => {
                // LIBCLEAR: ( 'LIBID' → )
                // Clear all private data for a library
                let lib_id_str = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("LIBCLEAR: expected library ID string".into()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };

                let lib_id = match encode_lib_id(&lib_id_str) {
                    Ok(id) => id,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Purge all .SETTINGS.LIBDATA.<LIBID>.* variables
                let prefix = format!(".SETTINGS.LIBDATA.{}.", decode_lib_id(lib_id));
                ctx.purge_by_prefix(&prefix);

                ExecuteResult::Ok
            }

            _ => ExecuteResult::Error(format!("Unknown LibPtr command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for LIBPTR prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word) {
                        let type_id = rpl_core::extract_type(word);
                        if type_id == TypeId::LIBPTR.as_u16() {
                            ctx.read(); // consume prolog
                            let lib_id = ctx.read().unwrap_or(0);
                            let cmd_index = ctx.read().unwrap_or(0);

                            // Try to show command name if user library registry is available
                            let cmd_name = ctx
                                .user_lib_registry()
                                .and_then(|r| r.get_command_name(lib_id, cmd_index))
                                .map(|s| s.to_string());

                            if let Some(name) = cmd_name {
                                ctx.write(&name);
                            } else {
                                // Fallback: display as LIB.index
                                let lib_name = decode_lib_id(lib_id);
                                ctx.write(&format!("{}.{}", lib_name, cmd_index));
                            }
                            return DecompileResult::Ok;
                        }

                        if type_id == TypeId::LIBRARY.as_u16() {
                            let size = rpl_core::extract_size(word) as usize;
                            ctx.read(); // consume prolog

                            let lib_id = ctx.read().unwrap_or(0);
                            let lib_name = decode_lib_id(lib_id);

                            // Skip the rest of the library data
                            for _ in 1..size {
                                ctx.read();
                            }

                            ctx.write(&format!("<Library {}>", lib_name));
                            return DecompileResult::Ok;
                        }
                    }
                DecompileResult::Unknown
            }
            DecompileMode::Call(cmd) => {
                let name = match cmd {
                    Self::CMD_CRLIB => "CRLIB",
                    Self::CMD_ATTACH => "ATTACH",
                    Self::CMD_DETACH => "DETACH",
                    Self::CMD_LIBMENU => "LIBMENU",
                    Self::CMD_LIBSTO => "LIBSTO",
                    Self::CMD_LIBRCL => "LIBRCL",
                    Self::CMD_LIBDEFRCL => "LIBDEFRCL",
                    Self::CMD_LIBCLEAR => "LIBCLEAR",
                    _ => return DecompileResult::Unknown,
                };
                ctx.write(name);
                DecompileResult::Ok
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            "CRLIB" => StackEffect::fixed(2, 1),    // ( { commands } 'LIBID' → library )
            "ATTACH" => StackEffect::fixed(1, 0),   // ( library → )
            "DETACH" => StackEffect::fixed(1, 0),   // ( 'LIBID' → )
            "LIBMENU" => StackEffect::fixed(2, 0),  // ( 'LIBID' n → )
            "LIBSTO" => StackEffect::fixed(3, 0),   // ( obj 'LIBID' 'name' → )
            "LIBRCL" => StackEffect::fixed(2, 1),   // ( 'LIBID' 'name' → obj )
            "LIBDEFRCL" => StackEffect::fixed(3, 1), // ( default 'LIBID' 'name' → obj )
            "LIBCLEAR" => StackEffect::fixed(1, 0), // ( 'LIBID' → )
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_decode_lib_id() {
        // Single character
        assert_eq!(encode_lib_id("A").unwrap(), 0x41);
        assert_eq!(decode_lib_id(0x41), "A");

        // Four characters - "MATH" in little-endian
        let math_id = encode_lib_id("MATH").unwrap();
        assert_eq!(decode_lib_id(math_id), "MATH");

        // Two characters
        let ab_id = encode_lib_id("AB").unwrap();
        assert_eq!(decode_lib_id(ab_id), "AB");
    }

    #[test]
    fn encode_lib_id_validation() {
        // Empty string
        assert!(encode_lib_id("").is_err());

        // Too long
        assert!(encode_lib_id("TOOLONG").is_err());

        // Non-alphanumeric
        assert!(encode_lib_id("A-B").is_err());
        assert!(encode_lib_id("A.B").is_err());
        assert!(encode_lib_id("A B").is_err());

        // Valid cases
        assert!(encode_lib_id("TEST").is_ok());
        assert!(encode_lib_id("A1B2").is_ok());
        assert!(encode_lib_id("abc").is_ok());
    }

    #[test]
    fn make_parse_libptr() {
        let lib_id = encode_lib_id("TEST").unwrap();
        let cmd_index = 5;

        let libptr = make_libptr(lib_id, cmd_index);

        let (parsed_lib_id, parsed_cmd_index) = parse_libptr(&libptr).unwrap();
        assert_eq!(parsed_lib_id, lib_id);
        assert_eq!(parsed_cmd_index, cmd_index);
    }

    #[test]
    fn parse_libptr_invalid() {
        // Wrong type
        let wrong_type = Value::Object {
            type_id: TypeId::PROGRAM,
            data: vec![1, 2],
        };
        assert!(parse_libptr(&wrong_type).is_none());

        // Too short
        let too_short = Value::Object {
            type_id: TypeId::LIBPTR,
            data: vec![1],
        };
        assert!(parse_libptr(&too_short).is_none());

        // Not an object
        let not_obj = Value::Int(42);
        assert!(parse_libptr(&not_obj).is_none());
    }

    #[test]
    fn storage_paths() {
        let lib_id = encode_lib_id("TEST").unwrap();

        assert_eq!(lib_storage_path(lib_id), ".SETTINGS.LIB.TEST");
        assert_eq!(lib_data_path(lib_id, "myvar"), ".SETTINGS.LIBDATA.TEST.myvar");
    }

    #[test]
    fn libclear_prefix_isolation() {
        // Verify that LIB1 and LIB2 prefixes don't overlap
        let lib1_id = encode_lib_id("LIB1").unwrap();
        let lib2_id = encode_lib_id("LIB2").unwrap();

        let lib1_path = lib_data_path(lib1_id, "value");
        let lib2_path = lib_data_path(lib2_id, "value");

        let lib1_prefix = format!(".SETTINGS.LIBDATA.{}.", decode_lib_id(lib1_id));

        assert_eq!(lib1_path, ".SETTINGS.LIBDATA.LIB1.value");
        assert_eq!(lib2_path, ".SETTINGS.LIBDATA.LIB2.value");
        assert_eq!(lib1_prefix, ".SETTINGS.LIBDATA.LIB1.");

        // LIB1 path should match LIB1 prefix
        assert!(lib1_path.starts_with(&lib1_prefix), "LIB1 should match its own prefix");
        // LIB2 path should NOT match LIB1 prefix
        assert!(!lib2_path.starts_with(&lib1_prefix), "LIB2 should NOT match LIB1 prefix");
    }

    #[test]
    fn get_library_command_with_name_simple() {
        use rpl_core::make_prolog;

        // Build a simple library with one named command
        let lib_id = encode_lib_id("TEST").unwrap();
        let cmd_count = 1u32;

        // The program bytecode: push real 42.0
        let val = 42.0f64;
        let bits = val.to_bits();
        let hi = (bits >> 32) as u32;
        let lo = bits as u32;
        let program_prolog = make_prolog(TypeId::PROGRAM.as_u16(), 3);

        // Encode command name "ANSWER" (6 bytes)
        let name_words = super::encode_command_name("ANSWER");
        // name_words = [6, packed("ANSW"), packed("ER\0\0")]

        // Library structure with named command:
        // Word 0: lib_id
        // Word 1: cmd_count (1)
        // Word 2: offset to command 0 (3, since header is 3 words)
        // Word 3: name length (6)
        // Word 4: "ANSW" packed
        // Word 5: "ER\0\0" packed
        // Word 6: program prolog
        // Word 7-9: program data
        let offset_to_cmd0 = 3u32;
        let mut lib_data = vec![
            lib_id,
            cmd_count,
            offset_to_cmd0,
        ];
        lib_data.extend(&name_words);
        lib_data.push(program_prolog);
        lib_data.push(make_prolog(TypeId::REAL.as_u16(), 2));
        lib_data.push(hi);
        lib_data.push(lo);

        // Get command 0 with name
        let result = get_library_command_with_name(&lib_data, 0);
        assert!(result.is_some());

        let (name, cmd) = result.unwrap();
        assert_eq!(name, "ANSWER");
        match cmd {
            Value::Program { code, .. } => {
                assert_eq!(code.len(), 3);
            }
            _ => panic!("Expected Program"),
        }

        // Command 1 should not exist
        let cmd1 = get_library_command(&lib_data, 1);
        assert!(cmd1.is_none());
    }

    #[test]
    fn build_library_single_program() {
        use rpl_core::make_prolog;

        let lib_id = encode_lib_id("TEST").unwrap();

        // Create a simple program that would push 42.0
        let val = 42.0f64;
        let bits = val.to_bits();
        let hi = (bits >> 32) as u32;
        let lo = bits as u32;
        let real_prolog = make_prolog(TypeId::REAL.as_u16(), 2);

        let program = Value::Program {
            code: vec![real_prolog, hi, lo],
            debug_info: None,
        };

        // Build library with named command
        let commands = vec![("ANSWER".to_string(), program)];
        let library = build_library(lib_id, &commands).unwrap();

        // Verify it's a LIBRARY object
        match &library {
            Value::Object { type_id, data } => {
                assert_eq!(*type_id, TypeId::LIBRARY);
                assert!(data.len() >= 3);
                assert_eq!(data[0], lib_id);
                assert_eq!(data[1], 1); // cmd_count
            }
            _ => panic!("Expected Object"),
        }

        // Extract the command with name and verify
        if let Value::Object { data, .. } = &library {
            let result = get_library_command_with_name(data, 0);
            assert!(result.is_some());

            let (name, cmd) = result.unwrap();
            assert_eq!(name, "ANSWER");
            match cmd {
                Value::Program { code, .. } => {
                    assert_eq!(code.len(), 3); // real_prolog + hi + lo
                }
                _ => panic!("Expected Program"),
            }
        }
    }

    #[test]
    fn build_library_multiple_programs() {
        use rpl_core::make_prolog;

        let lib_id = encode_lib_id("MATH").unwrap();

        // Program 1: pushes 1.0
        let bits1 = 1.0f64.to_bits();
        let prog1 = Value::Program {
            code: vec![
                make_prolog(TypeId::REAL.as_u16(), 2),
                (bits1 >> 32) as u32,
                bits1 as u32,
            ],
            debug_info: None,
        };

        // Program 2: pushes 2.0
        let bits2 = 2.0f64.to_bits();
        let prog2 = Value::Program {
            code: vec![
                make_prolog(TypeId::REAL.as_u16(), 2),
                (bits2 >> 32) as u32,
                bits2 as u32,
            ],
            debug_info: None,
        };

        // Build library with named commands
        let commands = vec![
            ("ONE".to_string(), prog1),
            ("TWO".to_string(), prog2),
        ];
        let library = build_library(lib_id, &commands).unwrap();

        // Verify structure
        if let Value::Object { type_id, data } = &library {
            assert_eq!(*type_id, TypeId::LIBRARY);
            assert_eq!(data[0], lib_id);
            assert_eq!(data[1], 2); // cmd_count

            // Both commands should be extractable with names
            let result0 = get_library_command_with_name(data, 0);
            let result1 = get_library_command_with_name(data, 1);
            let result2 = get_library_command_with_name(data, 2);

            assert!(result0.is_some());
            assert!(result1.is_some());
            assert!(result2.is_none()); // Only 2 commands

            assert_eq!(result0.unwrap().0, "ONE");
            assert_eq!(result1.unwrap().0, "TWO");

            // Also test get_library_command_names
            let names = get_library_command_names(data);
            assert_eq!(names, vec!["ONE", "TWO"]);
        } else {
            panic!("Expected Object");
        }
    }

    #[test]
    fn build_library_rejects_non_programs() {
        let lib_id = encode_lib_id("TEST").unwrap();

        // Try to build with a non-program value
        let not_a_program = Value::Real(42.0);
        let commands = vec![("BAD".to_string(), not_a_program)];

        let result = build_library(lib_id, &commands);
        assert!(result.is_err());
    }

    #[test]
    fn encode_decode_command_name() {
        // Test various name lengths
        let test_cases = vec![
            "A",        // 1 byte
            "AB",       // 2 bytes
            "ABC",      // 3 bytes
            "ABCD",     // 4 bytes (exact word)
            "ABCDE",    // 5 bytes
            "DOUBLE",   // 6 bytes
            "LONGNAME", // 8 bytes (exact 2 words)
        ];

        for name in test_cases {
            let encoded = super::encode_command_name(name);
            assert_eq!(encoded[0] as usize, name.len(), "Length should match for '{}'", name);

            let (decoded, words_consumed) = super::decode_command_name(&encoded, 0).unwrap();
            assert_eq!(decoded, name, "Decoded name should match for '{}'", name);
            assert_eq!(words_consumed, encoded.len(), "Words consumed should match");
        }
    }
}
