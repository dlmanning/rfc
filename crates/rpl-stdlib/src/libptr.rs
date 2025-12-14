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

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::{TypeId, Word};
use rpl_lang::library::ExecuteOk;
use rpl_lang::Value;

// ============================================================================
// User Library Registry (for compile-time command resolution)
// ============================================================================

/// Metadata about an attached user library (for compile-time resolution).
#[derive(Debug, Clone)]
pub struct AttachedLibraryMeta {
    pub lib_id: u32,
    pub commands: Vec<String>,
}

/// Registry of attached user libraries for compile-time command resolution.
#[derive(Debug, Default)]
pub struct UserLibraryRegistry {
    attached: HashMap<u32, AttachedLibraryMeta>,
}

impl UserLibraryRegistry {
    pub fn new() -> Self {
        Self { attached: HashMap::new() }
    }

    pub fn register(&mut self, lib_id: u32, commands: Vec<String>) {
        self.attached.insert(lib_id, AttachedLibraryMeta { lib_id, commands });
    }

    pub fn unregister(&mut self, lib_id: u32) {
        self.attached.remove(&lib_id);
    }

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

    pub fn get_command_name(&self, lib_id: u32, index: u32) -> Option<&str> {
        self.attached
            .get(&lib_id)
            .and_then(|meta| meta.commands.get(index as usize))
            .map(|s| s.as_str())
    }

    pub fn is_attached(&self, lib_id: u32) -> bool {
        self.attached.contains_key(&lib_id)
    }

    pub fn attached_libraries(&self) -> impl Iterator<Item = u32> + '_ {
        self.attached.keys().copied()
    }
}

rpl_macros::define_library! {
    pub library LibPtrLib(102, "LibPtr");

    commands {
        CRLIB (2 -> 1) "Create a user library from command list" {
            // CRLIB: ( { { "name" <<prog>> } ... } 'LIBID' → library )
            let lib_id_str = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("CRLIB: expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id = match encode_lib_id(&lib_id_str) {
                Ok(id) => id,
                Err(e) => return Err(e.into()),
            };

            let entries = match ctx.pop() {
                Ok(Value::List(items)) => items,
                Ok(_) => return Err("CRLIB: expected list of command entries".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let mut commands: Vec<(String, Value)> = Vec::with_capacity(entries.len());
            for (i, entry) in entries.iter().enumerate() {
                match entry {
                    Value::List(pair) if pair.len() == 2 => {
                        let name = match &pair[0] {
                            Value::String(s) => s.clone(),
                            _ => return Err(format!("CRLIB: entry {} must have string name", i)),
                        };
                        let program = pair[1].clone();
                        match &program {
                            Value::Program { .. } => {}
                            Value::Object { type_id, .. } if *type_id == TypeId::PROGRAM => {}
                            _ => return Err(format!("CRLIB: entry {} must have program", i)),
                        }
                        commands.push((name, program));
                    }
                    _ => return Err(format!("CRLIB: entry {} must be a 2-element list", i)),
                }
            }

            match build_library(lib_id, &commands) {
                Ok(library) => {
                    if ctx.push(library).is_err() {
                        return Err("Stack overflow".into());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(e) => Err(e.into()),
            }
        }

        ATTACH (1 -> 0) "Attach a user library" {
            let lib_obj = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".into()),
            };

            let (lib_id, lib_data) = match &lib_obj {
                Value::Object { type_id, data } if *type_id == TypeId::LIBRARY && !data.is_empty() => {
                    (data[0], data.clone())
                }
                _ => return Err("Expected library object".into()),
            };

            let command_names = get_library_command_names(&lib_data);
            ctx.user_libs_mut().register(lib_id, command_names);

            let path = lib_storage_path(lib_id);
            ctx.store(path, lib_obj);

            Ok(ExecuteOk::Ok)
        }

        DETACH (1 -> 0) "Detach a user library" {
            let lib_id = match ctx.pop() {
                Ok(Value::String(s)) => match encode_lib_id(&s) {
                    Ok(id) => id,
                    Err(e) => return Err(e.into()),
                },
                Ok(_) => return Err("Expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            ctx.user_libs_mut().unregister(lib_id);
            let path = lib_storage_path(lib_id);
            ctx.purge(&path);

            Ok(ExecuteOk::Ok)
        }

        LIBMENU (2 -> 0) "Display library menu" {
            Err("LIBMENU not yet implemented".into())
        }

        LIBSTO (3 -> 0) "Store object in library namespace" {
            let var_name = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBSTO: expected variable name string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id_str = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBSTO: expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id = match encode_lib_id(&lib_id_str) {
                Ok(id) => id,
                Err(e) => return Err(e.into()),
            };

            let obj = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".into()),
            };

            let path = lib_data_path(lib_id, &var_name);
            ctx.store(path, obj);

            Ok(ExecuteOk::Ok)
        }

        LIBRCL (2 -> 1) "Recall object from library namespace" {
            let var_name = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBRCL: expected variable name string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id_str = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBRCL: expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id = match encode_lib_id(&lib_id_str) {
                Ok(id) => id,
                Err(e) => return Err(e.into()),
            };

            let path = lib_data_path(lib_id, &var_name);
            match ctx.recall(&path) {
                Some(value) => {
                    let value = value.clone();
                    if ctx.push(value).is_err() {
                        return Err("Stack overflow".into());
                    }
                    Ok(ExecuteOk::Ok)
                }
                None => Err(format!("Variable '{}' not found in library '{}'", var_name, lib_id_str)),
            }
        }

        LIBDEFRCL (3 -> 1) "Recall object from library namespace with default" {
            let var_name = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBDEFRCL: expected variable name string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id_str = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBDEFRCL: expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id = match encode_lib_id(&lib_id_str) {
                Ok(id) => id,
                Err(e) => return Err(e.into()),
            };

            let default_value = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".into()),
            };

            let path = lib_data_path(lib_id, &var_name);
            let result = match ctx.recall(&path) {
                Some(value) => value.clone(),
                None => default_value,
            };

            if ctx.push(result).is_err() {
                return Err("Stack overflow".into());
            }
            Ok(ExecuteOk::Ok)
        }

        LIBCLEAR (1 -> 0) "Clear all library private data" {
            let lib_id_str = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("LIBCLEAR: expected library ID string".into()),
                Err(_) => return Err("Stack underflow".into()),
            };

            let lib_id = match encode_lib_id(&lib_id_str) {
                Ok(id) => id,
                Err(e) => return Err(e.into()),
            };

            let prefix = format!(".SETTINGS.LIBDATA.{}.", decode_lib_id(lib_id));
            ctx.purge_by_prefix(&prefix);

            Ok(ExecuteOk::Ok)
        }
    }

    custom probe(ctx) {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        // Check built-in commands
        if Self::command_id(&upper).is_some() && text.len() == upper.len() {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        // Check user library commands
        if let Some(registry) = ctx.user_lib_registry()
            && registry.lookup_command(text).is_some() {
                return rpl_lang::library::ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Command,
                };
            }

        rpl_lang::library::ProbeResult::NoMatch
    }

    custom compile(ctx) {
        let text = ctx.text();
        let upper = text.to_ascii_uppercase();

        // Check built-in commands
        if let Some(cmd) = Self::command_id(&upper) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            return rpl_lang::library::CompileResult::Ok;
        }

        // Check user library commands
        if let Some(registry) = ctx.user_lib_registry()
            && let Some((lib_id, cmd_index)) = registry.lookup_command(text) {
                ctx.emit_prolog(TypeId::LIBPTR.as_u16(), 2);
                ctx.emit(lib_id);
                ctx.emit(cmd_index);
                return rpl_lang::library::CompileResult::Ok;
            }

        rpl_lang::library::CompileResult::NoMatch
    }

    custom decompile(ctx) {
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

                            let cmd_name = ctx
                                .user_lib_registry()
                                .and_then(|r| r.get_command_name(lib_id, cmd_index))
                                .map(|s| s.to_string());

                            if let Some(name) = cmd_name {
                                ctx.write(&name);
                            } else {
                                let lib_name = decode_lib_id(lib_id);
                                ctx.write(&format!("{}.{}", lib_name, cmd_index));
                            }
                            return rpl_lang::library::DecompileResult::Ok;
                        }

                        if type_id == TypeId::LIBRARY.as_u16() {
                            let size = rpl_core::extract_size(word) as usize;
                            ctx.read(); // consume prolog

                            let lib_id = ctx.read().unwrap_or(0);
                            let lib_name = decode_lib_id(lib_id);

                            for _ in 1..size {
                                ctx.read();
                            }

                            ctx.write(&format!("<Library {}>", lib_name));
                            return rpl_lang::library::DecompileResult::Ok;
                        }
                    }
                rpl_lang::library::DecompileResult::Unknown
            }
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    rpl_lang::library::DecompileResult::Ok
                } else {
                    rpl_lang::library::DecompileResult::Unknown
                }
            }
        }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

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
pub fn build_library(lib_id: u32, commands: &[(String, Value)]) -> Result<Value, &'static str> {
    let cmd_count = commands.len();

    let mut serialized_commands: Vec<Vec<Word>> = Vec::with_capacity(cmd_count);

    for (name, program) in commands {
        let mut cmd_data = encode_command_name(name);

        match program {
            Value::Program { code, .. } => {
                let prolog = rpl_core::make_prolog(TypeId::PROGRAM.as_u16(), code.len() as u16);
                cmd_data.push(prolog);
                cmd_data.extend(code.iter());
            }
            Value::Object { type_id, data } if *type_id == TypeId::PROGRAM => {
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

    let header_size = 2 + cmd_count;

    let mut offsets = Vec::with_capacity(cmd_count);
    let mut current_offset = header_size;
    for cmd_data in &serialized_commands {
        offsets.push(current_offset as u32);
        current_offset += cmd_data.len();
    }

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
pub fn get_library_command_with_name(lib_data: &[Word], cmd_index: u32) -> Option<(String, Value)> {
    if lib_data.len() < 2 {
        return None;
    }

    let _lib_id = lib_data[0];
    let cmd_count = lib_data[1] as usize;

    if cmd_index as usize >= cmd_count {
        return None;
    }

    if lib_data.len() < 2 + cmd_count {
        return None;
    }

    let offset = lib_data[2 + cmd_index as usize] as usize;

    if offset >= lib_data.len() {
        return None;
    }

    let (name, name_words) = decode_command_name(lib_data, offset)?;

    let prog_offset = offset + name_words;
    if prog_offset >= lib_data.len() {
        return None;
    }

    let remaining = &lib_data[prog_offset..];
    if remaining.is_empty() {
        return None;
    }

    if rpl_core::is_prolog(remaining[0]) {
        let type_id_raw = rpl_core::extract_type(remaining[0]);
        let size = rpl_core::extract_size(remaining[0]) as usize;

        if remaining.len() < 1 + size {
            return None;
        }

        let obj_data: Vec<Word> = remaining[1..1 + size].to_vec();
        let type_id = TypeId::new(type_id_raw);

        let value = if type_id == TypeId::PROGRAM {
            Value::program(obj_data)
        } else {
            Value::Object { type_id, data: obj_data }
        };
        return Some((name, value));
    }

    None
}

/// Parse a library object and extract the command at a given index.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_decode_lib_id() {
        assert_eq!(encode_lib_id("A").unwrap(), 0x41);
        assert_eq!(decode_lib_id(0x41), "A");

        let math_id = encode_lib_id("MATH").unwrap();
        assert_eq!(decode_lib_id(math_id), "MATH");

        let ab_id = encode_lib_id("AB").unwrap();
        assert_eq!(decode_lib_id(ab_id), "AB");
    }

    #[test]
    fn encode_lib_id_validation() {
        assert!(encode_lib_id("").is_err());
        assert!(encode_lib_id("TOOLONG").is_err());
        assert!(encode_lib_id("A-B").is_err());
        assert!(encode_lib_id("A.B").is_err());
        assert!(encode_lib_id("A B").is_err());
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
        let wrong_type = Value::Object {
            type_id: TypeId::PROGRAM,
            data: vec![1, 2],
        };
        assert!(parse_libptr(&wrong_type).is_none());

        let too_short = Value::Object {
            type_id: TypeId::LIBPTR,
            data: vec![1],
        };
        assert!(parse_libptr(&too_short).is_none());

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
        let lib1_id = encode_lib_id("LIB1").unwrap();
        let lib2_id = encode_lib_id("LIB2").unwrap();

        let lib1_path = lib_data_path(lib1_id, "value");
        let lib2_path = lib_data_path(lib2_id, "value");

        let lib1_prefix = format!(".SETTINGS.LIBDATA.{}.", decode_lib_id(lib1_id));

        assert_eq!(lib1_path, ".SETTINGS.LIBDATA.LIB1.value");
        assert_eq!(lib2_path, ".SETTINGS.LIBDATA.LIB2.value");
        assert_eq!(lib1_prefix, ".SETTINGS.LIBDATA.LIB1.");

        assert!(lib1_path.starts_with(&lib1_prefix));
        assert!(!lib2_path.starts_with(&lib1_prefix));
    }

    #[test]
    fn get_library_command_with_name_simple() {
        use rpl_core::make_prolog;

        let lib_id = encode_lib_id("TEST").unwrap();
        let cmd_count = 1u32;

        let val = 42.0f64;
        let bits = val.to_bits();
        let hi = (bits >> 32) as u32;
        let lo = bits as u32;
        let program_prolog = make_prolog(TypeId::PROGRAM.as_u16(), 3);

        let name_words = encode_command_name("ANSWER");

        let offset_to_cmd0 = 3u32;
        let mut lib_data = vec![lib_id, cmd_count, offset_to_cmd0];
        lib_data.extend(&name_words);
        lib_data.push(program_prolog);
        lib_data.push(make_prolog(TypeId::REAL.as_u16(), 2));
        lib_data.push(hi);
        lib_data.push(lo);

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

        let cmd1 = get_library_command(&lib_data, 1);
        assert!(cmd1.is_none());
    }

    #[test]
    fn build_library_single_program() {
        use rpl_core::make_prolog;

        let lib_id = encode_lib_id("TEST").unwrap();

        let val = 42.0f64;
        let bits = val.to_bits();
        let hi = (bits >> 32) as u32;
        let lo = bits as u32;
        let real_prolog = make_prolog(TypeId::REAL.as_u16(), 2);

        let program = Value::Program {
            code: vec![real_prolog, hi, lo],
            debug_info: None,
        };

        let commands = vec![("ANSWER".to_string(), program)];
        let library = build_library(lib_id, &commands).unwrap();

        match &library {
            Value::Object { type_id, data } => {
                assert_eq!(*type_id, TypeId::LIBRARY);
                assert!(data.len() >= 3);
                assert_eq!(data[0], lib_id);
                assert_eq!(data[1], 1);
            }
            _ => panic!("Expected Object"),
        }

        if let Value::Object { data, .. } = &library {
            let result = get_library_command_with_name(data, 0);
            assert!(result.is_some());

            let (name, cmd) = result.unwrap();
            assert_eq!(name, "ANSWER");
            match cmd {
                Value::Program { code, .. } => {
                    assert_eq!(code.len(), 3);
                }
                _ => panic!("Expected Program"),
            }
        }
    }

    #[test]
    fn build_library_multiple_programs() {
        use rpl_core::make_prolog;

        let lib_id = encode_lib_id("MATH").unwrap();

        let bits1 = 1.0f64.to_bits();
        let prog1 = Value::Program {
            code: vec![
                make_prolog(TypeId::REAL.as_u16(), 2),
                (bits1 >> 32) as u32,
                bits1 as u32,
            ],
            debug_info: None,
        };

        let bits2 = 2.0f64.to_bits();
        let prog2 = Value::Program {
            code: vec![
                make_prolog(TypeId::REAL.as_u16(), 2),
                (bits2 >> 32) as u32,
                bits2 as u32,
            ],
            debug_info: None,
        };

        let commands = vec![
            ("ONE".to_string(), prog1),
            ("TWO".to_string(), prog2),
        ];
        let library = build_library(lib_id, &commands).unwrap();

        if let Value::Object { type_id, data } = &library {
            assert_eq!(*type_id, TypeId::LIBRARY);
            assert_eq!(data[0], lib_id);
            assert_eq!(data[1], 2);

            let result0 = get_library_command_with_name(data, 0);
            let result1 = get_library_command_with_name(data, 1);
            let result2 = get_library_command_with_name(data, 2);

            assert!(result0.is_some());
            assert!(result1.is_some());
            assert!(result2.is_none());

            assert_eq!(result0.unwrap().0, "ONE");
            assert_eq!(result1.unwrap().0, "TWO");

            let names = get_library_command_names(data);
            assert_eq!(names, vec!["ONE", "TWO"]);
        } else {
            panic!("Expected Object");
        }
    }

    #[test]
    fn build_library_rejects_non_programs() {
        let lib_id = encode_lib_id("TEST").unwrap();

        let not_a_program = Value::Real(42.0);
        let commands = vec![("BAD".to_string(), not_a_program)];

        let result = build_library(lib_id, &commands);
        assert!(result.is_err());
    }

    #[test]
    fn encode_decode_command_name() {
        let test_cases = vec![
            "A",
            "AB",
            "ABC",
            "ABCD",
            "ABCDE",
            "DOUBLE",
            "LONGNAME",
        ];

        for name in test_cases {
            let encoded = encode_command_name(name);
            assert_eq!(encoded[0] as usize, name.len());

            let (decoded, words_consumed) = decode_command_name(&encoded, 0).unwrap();
            assert_eq!(decoded, name);
            assert_eq!(words_consumed, encoded.len());
        }
    }
}
