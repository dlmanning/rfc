//! User library support for compile-time and runtime resolution.
//!
//! This module provides types and functions for working with user-defined
//! libraries (LIBPTR, LIBRARY objects) without depending on the actual
//! library implementations in rpl-stdlib.

use std::collections::HashMap;

use rpl_core::{TypeId, Word};
use crate::library::LibraryId;
use rpl_vm::Value;

// =============================================================================
// Well-known Library ID
// =============================================================================

/// Library ID for the libptr library.
pub const LIBPTR_LIB_ID: LibraryId = LibraryId::new(102);

// =============================================================================
// User Library Registry
// =============================================================================

/// Metadata about an attached user library (for compile-time resolution).
#[derive(Debug, Clone)]
pub struct AttachedLibraryMeta {
    /// The library ID (encoded 1-4 character name)
    pub lib_id: u32,
    /// Command names indexed by command index
    pub commands: Vec<String>,
}

/// Registry of attached user libraries for compile-time command resolution.
#[derive(Debug, Default)]
pub struct UserLibraryRegistry {
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
    pub fn register(&mut self, lib_id: u32, commands: Vec<String>) {
        self.attached.insert(
            lib_id,
            AttachedLibraryMeta { lib_id, commands },
        );
    }

    /// Unregister a library.
    pub fn unregister(&mut self, lib_id: u32) {
        self.attached.remove(&lib_id);
    }

    /// Look up a command by name across all attached libraries.
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

// =============================================================================
// Library ID Encoding/Decoding
// =============================================================================

/// Encode a library ID from a string (1-4 ASCII alphanumeric characters).
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

/// Get the storage path for an attached library.
pub fn lib_storage_path(lib_id: u32) -> String {
    format!(".SETTINGS.LIB.{}", decode_lib_id(lib_id))
}

// =============================================================================
// Library Object Parsing
// =============================================================================

/// Decode a command name from words starting at the given offset.
///
/// Returns (name, words_consumed) or None if invalid.
fn decode_command_name(data: &[Word], offset: usize) -> Option<(String, usize)> {
    if offset >= data.len() {
        return None;
    }

    let len = data[offset] as usize;  // Length in bytes
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

/// Parse a library object and extract the command at a given index.
pub fn get_library_command(lib_data: &[Word], cmd_index: u32) -> Option<Value> {
    get_library_command_with_name(lib_data, cmd_index).map(|(_, program)| program)
}

/// Parse a library object and extract the command name and program at a given index.
///
/// Library format:
/// - Word 0: Library ID
/// - Word 1: Command count
/// - Words 2..N+1: Offset table
/// - Words N+2...: Command entries (name + program each)
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

    // Check if it's a prolog word using rpl_core functions
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
