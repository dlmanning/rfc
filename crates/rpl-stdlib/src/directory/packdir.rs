//! PACKDIR helper functions.
//!
//! These functions implement the core logic for packing and unpacking
//! directories. The commands themselves are in mod.rs as part of DirectoryLib.

use rpl_core::TypeId;
use rpl_lang::{Value, library::ExecuteContext};

use super::serialize::{
    SerializeError, bytes_to_words, deserialize_value, serialize_value, words_to_bytes,
};

/// PACKDIR format version.
pub const FORMAT_VERSION: u32 = 1;

/// Pack the current directory into a PACKDIR Value.
fn pack_current_directory(ctx: &mut ExecuteContext) -> Result<Value, String> {
    let mut bytes = Vec::new();

    // Collect variable and subdirectory names
    let var_names: Vec<String> = ctx.vars().cloned().collect();
    let subdir_names: Vec<String> = ctx.subdir_names().cloned().collect();

    // Write counts to byte stream
    write_u32(&mut bytes, var_names.len() as u32);
    write_u32(&mut bytes, subdir_names.len() as u32);

    // Serialize each variable
    for name in &var_names {
        write_string(&mut bytes, name);
        if let Some(value) = ctx.recall(name) {
            let value_bytes = serialize_value(value);
            bytes.extend(&value_bytes);
        }
    }

    // Recursively pack subdirectories
    for name in &subdir_names {
        write_string(&mut bytes, name);

        // Enter subdirectory
        if !ctx.enter_subdir(name) {
            return Err(format!("Failed to enter subdirectory '{}'", name));
        }

        // Pack it recursively
        let subdir_packdir = pack_current_directory(ctx)?;

        // Extract and append the byte data from the nested packdir
        if let Value::Object { data, .. } = subdir_packdir {
            // data[0..4] is header, data[4..] is packed bytes
            if data.len() > 4 {
                let subdir_bytes = words_to_bytes(&data[4..]);
                // Write length of nested packdir bytes
                write_u32(&mut bytes, subdir_bytes.len() as u32);
                bytes.extend(&subdir_bytes);
            } else {
                // Empty subdirectory
                write_u32(&mut bytes, 0);
            }
        }

        ctx.updir();
    }

    // Build final object
    let mut data = vec![
        FORMAT_VERSION,
        bytes.len() as u32,
        var_names.len() as u32,
        subdir_names.len() as u32,
    ];
    data.extend(bytes_to_words(&bytes));

    Ok(Value::Object {
        type_id: TypeId::PACKDIR,
        data,
    })
}

/// Pack a directory (current or named subdirectory) into a PACKDIR Value.
pub fn pack_directory(
    ctx: &mut ExecuteContext,
    subdir_name: Option<&str>,
) -> Result<Value, String> {
    if let Some(name) = subdir_name
        && !ctx.enter_subdir(name)
    {
        return Err(format!("Subdirectory '{}' not found", name));
    }

    let result = pack_current_directory(ctx);

    if subdir_name.is_some() {
        ctx.updir();
    }

    result
}

/// Parse PACKDIR header and return (var_count, subdir_count, bytes).
fn parse_packdir_header(packdir: &Value) -> Result<(usize, usize, Vec<u8>), String> {
    match packdir {
        Value::Object { type_id, data } if *type_id == TypeId::PACKDIR => {
            if data.len() < 4 {
                return Err("Invalid PACKDIR: header too short".to_string());
            }

            let version = data[0];
            if version != FORMAT_VERSION {
                return Err(format!("Unsupported PACKDIR version: {}", version));
            }

            let _byte_len = data[1] as usize;
            let var_count = data[2] as usize;
            let subdir_count = data[3] as usize;

            let bytes = if data.len() > 4 {
                words_to_bytes(&data[4..])
            } else {
                Vec::new()
            };

            Ok((var_count, subdir_count, bytes))
        }
        _ => Err("Expected PACKDIR object".to_string()),
    }
}

/// Check if any names in the PACKDIR conflict with existing directory contents.
fn check_conflicts(
    ctx: &ExecuteContext,
    var_count: usize,
    subdir_count: usize,
    bytes: &[u8],
) -> Result<(), String> {
    let mut offset = 8; // Skip var_count and subdir_count in byte stream

    // Check variable names
    for _ in 0..var_count {
        let (name, consumed) = read_string(&bytes[offset..])
            .map_err(|e| format!("Failed to read variable name: {}", e))?;
        offset += consumed;

        if ctx.has_var(&name) {
            return Err(format!("Variable '{}' already exists", name));
        }

        // Skip the serialized value
        let (_, consumed) = deserialize_value(&bytes[offset..])
            .map_err(|e| format!("Failed to parse value: {}", e))?;
        offset += consumed;
    }

    // Check subdirectory names
    for _ in 0..subdir_count {
        let (name, consumed) = read_string(&bytes[offset..])
            .map_err(|e| format!("Failed to read subdir name: {}", e))?;
        offset += consumed;

        // Check if name exists as variable or subdirectory
        if ctx.has_var(&name) {
            return Err(format!("Name '{}' already exists as variable", name));
        }

        // Check subdirectory existence by trying to get subdir names
        let subdir_exists = ctx.subdir_names().any(|n| n == &name);
        if subdir_exists {
            return Err(format!("Subdirectory '{}' already exists", name));
        }

        // Skip the nested packdir bytes
        let nested_len = read_u32(&bytes[offset..])
            .map_err(|e| format!("Failed to read nested length: {}", e))?
            as usize;
        offset += 4 + nested_len;
    }

    Ok(())
}

/// Unpack a PACKDIR into the current directory.
fn unpack_into_current(ctx: &mut ExecuteContext, packdir: &Value) -> Result<(), String> {
    let (var_count, subdir_count, bytes) = parse_packdir_header(packdir)?;

    // First pass: check for conflicts
    check_conflicts(ctx, var_count, subdir_count, &bytes)?;

    // Second pass: unpack (offset starts after var_count and subdir_count in byte stream)
    let mut offset = 8;

    // Unpack variables
    for _ in 0..var_count {
        let (name, consumed) = read_string(&bytes[offset..])
            .map_err(|e| format!("Failed to read variable name: {}", e))?;
        offset += consumed;

        let (value, consumed) = deserialize_value(&bytes[offset..])
            .map_err(|e| format!("Failed to deserialize value: {}", e))?;
        offset += consumed;

        ctx.store(name, value);
    }

    // Unpack subdirectories
    for _ in 0..subdir_count {
        let (name, consumed) = read_string(&bytes[offset..])
            .map_err(|e| format!("Failed to read subdir name: {}", e))?;
        offset += consumed;

        // Read nested packdir bytes length
        let nested_len = read_u32(&bytes[offset..])
            .map_err(|e| format!("Failed to read nested length: {}", e))?
            as usize;
        offset += 4;

        // Create subdirectory and enter it
        ctx.create_subdir(name.clone());
        if !ctx.enter_subdir(&name) {
            return Err(format!("Failed to enter created subdirectory '{}'", name));
        }

        // Reconstruct nested PACKDIR from bytes
        if nested_len > 0 {
            let nested_bytes = &bytes[offset..offset + nested_len];

            // Parse counts from nested bytes
            let nested_var_count = read_u32(nested_bytes)
                .map_err(|e| format!("Failed to read nested var count: {}", e))?
                as usize;
            let nested_subdir_count = read_u32(&nested_bytes[4..])
                .map_err(|e| format!("Failed to read nested subdir count: {}", e))?
                as usize;

            // Build nested PACKDIR object
            let mut nested_data = vec![
                FORMAT_VERSION,
                nested_len as u32,
                nested_var_count as u32,
                nested_subdir_count as u32,
            ];
            nested_data.extend(bytes_to_words(nested_bytes));

            let nested_packdir = Value::Object {
                type_id: TypeId::PACKDIR,
                data: nested_data,
            };

            unpack_into_current(ctx, &nested_packdir)?;
        }

        offset += nested_len;
        ctx.updir();
    }

    Ok(())
}

/// Unpack a PACKDIR into a directory.
/// If dest_name is Some, creates that subdirectory and unpacks into it.
pub fn unpack_directory(
    ctx: &mut ExecuteContext,
    packdir: &Value,
    dest_name: Option<&str>,
) -> Result<(), String> {
    if let Some(name) = dest_name {
        if ctx.has_var(name) {
            return Err(format!("Variable '{}' already exists", name));
        }
        if !ctx.create_subdir(name.to_string()) {
            return Err(format!("Subdirectory '{}' already exists", name));
        }
        if !ctx.enter_subdir(name) {
            return Err(format!("Failed to enter subdirectory '{}'", name));
        }
    }

    let result = unpack_into_current(ctx, packdir);

    if dest_name.is_some() {
        ctx.updir();
    }

    result
}

/// Get entry names from a PACKDIR without unpacking values.
pub fn packdir_entries(packdir: &Value) -> Result<Vec<String>, String> {
    let (var_count, subdir_count, bytes) = parse_packdir_header(packdir)?;
    let mut names = Vec::with_capacity(var_count + subdir_count);
    let mut offset = 8; // Skip counts

    // Read variable names
    for _ in 0..var_count {
        let (name, consumed) =
            read_string(&bytes[offset..]).map_err(|e| format!("Failed to read name: {}", e))?;
        offset += consumed;
        names.push(name);

        // Skip value
        let (_, consumed) = deserialize_value(&bytes[offset..])
            .map_err(|e| format!("Failed to skip value: {}", e))?;
        offset += consumed;
    }

    // Read subdirectory names
    for _ in 0..subdir_count {
        let (name, consumed) =
            read_string(&bytes[offset..]).map_err(|e| format!("Failed to read name: {}", e))?;
        offset += consumed;
        names.push(name);

        // Skip nested packdir
        let nested_len = read_u32(&bytes[offset..])
            .map_err(|e| format!("Failed to read nested length: {}", e))?
            as usize;
        offset += 4 + nested_len;
    }

    Ok(names)
}

// Byte utilities

fn write_u32(buf: &mut Vec<u8>, val: u32) {
    buf.extend(&val.to_le_bytes());
}

fn read_u32(bytes: &[u8]) -> Result<u32, SerializeError> {
    if bytes.len() < 4 {
        return Err(SerializeError::UnexpectedEnd);
    }
    Ok(u32::from_le_bytes(bytes[..4].try_into().unwrap()))
}

fn write_string(buf: &mut Vec<u8>, s: &str) {
    write_u32(buf, s.len() as u32);
    buf.extend(s.as_bytes());
}

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
