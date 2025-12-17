//! Directory library for variable storage and navigation.
//!
//! ## Variable Commands
//!
//! - `STO` - Store value in variable (value name --)
//! - `RCL` - Recall value from variable (name -- value)
//! - `PURGE` - Delete variable (name --)
//! - `VARS` - List all variable names (-- {names})
//! - `CLVAR` - Clear all variables (--)
//! - `INCR` - Increment numeric variable (name -- new_value)
//! - `DECR` - Decrement numeric variable (name -- new_value)
//! - `RENAME` - Rename variable (old_name new_name --)
//!
//! ## Directory Navigation
//!
//! - `CRDIR` - Create subdirectory (name --)
//! - `PGDIR` - Delete empty subdirectory (name --)
//! - `UPDIR` - Move up one directory level (--)
//! - `HOME` - Move to root directory (--)
//! - `PATH` - Get current directory path (-- {path})

use crate::core::{Span, TypeId};

use crate::{
    ir::LibId,
    libs::{
        CommandInfo, ExecuteContext, ExecuteResult, Library, StackEffect,
    },
    lower::{LowerContext, LowerError},
    serialize::{pack_directory, packinfo, unpack_directory_checked},
    types::CStack,
    value::Value,
};

/// Directory library ID (matches rpl-stdlib).
pub const DIRECTORY_LIB: LibId = 28;

/// Directory library command IDs.
pub mod cmd {
    // Variable operations
    pub const STO: u16 = 0;
    pub const RCL: u16 = 1;
    pub const PURGE: u16 = 2;
    pub const VARS: u16 = 3;
    pub const CLVAR: u16 = 4;
    pub const INCR: u16 = 5;
    pub const DECR: u16 = 6;
    pub const RENAME: u16 = 7;
    // Directory navigation
    pub const CRDIR: u16 = 8;
    pub const PGDIR: u16 = 9;
    pub const UPDIR: u16 = 10;
    pub const HOME: u16 = 11;
    pub const PATH: u16 = 12;
    // Directory packing
    pub const PACKDIR: u16 = 13;
    pub const UNPACKDIR: u16 = 14;
    pub const PACKINFO: u16 = 15;
}

/// Directory library.
#[derive(Clone, Copy)]
pub struct DirectoryLib;

impl Library for DirectoryLib {
    fn id(&self) -> LibId {
        DIRECTORY_LIB
    }

    fn name(&self) -> &'static str {
        "Directory"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            // Variable operations
            CommandInfo::with_effect("STO", DIRECTORY_LIB, cmd::STO, 2, 0),
            CommandInfo::with_effect("RCL", DIRECTORY_LIB, cmd::RCL, 1, 1),
            CommandInfo::with_effect("PURGE", DIRECTORY_LIB, cmd::PURGE, 1, 0),
            CommandInfo::with_effect("VARS", DIRECTORY_LIB, cmd::VARS, 0, 1),
            CommandInfo::with_effect("CLVAR", DIRECTORY_LIB, cmd::CLVAR, 0, 0),
            CommandInfo::with_effect("INCR", DIRECTORY_LIB, cmd::INCR, 1, 1),
            CommandInfo::with_effect("DECR", DIRECTORY_LIB, cmd::DECR, 1, 1),
            CommandInfo::with_effect("RENAME", DIRECTORY_LIB, cmd::RENAME, 2, 0),
            // Directory navigation
            CommandInfo::with_effect("CRDIR", DIRECTORY_LIB, cmd::CRDIR, 1, 0),
            CommandInfo::with_effect("PGDIR", DIRECTORY_LIB, cmd::PGDIR, 1, 0),
            CommandInfo::with_effect("UPDIR", DIRECTORY_LIB, cmd::UPDIR, 0, 0),
            CommandInfo::with_effect("HOME", DIRECTORY_LIB, cmd::HOME, 0, 0),
            CommandInfo::with_effect("PATH", DIRECTORY_LIB, cmd::PATH, 0, 1),
            // Directory packing - stack effects are dynamic
            CommandInfo::new("PACKDIR", DIRECTORY_LIB, cmd::PACKDIR),
            CommandInfo::new("UNPACKDIR", DIRECTORY_LIB, cmd::UNPACKDIR),
            CommandInfo::with_effect("PACKINFO", DIRECTORY_LIB, cmd::PACKINFO, 1, 1),
        ]
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(DIRECTORY_LIB, cmd);
        Ok(())
    }

    fn command_effect(&self, cmd: u16, _types: &CStack) -> StackEffect {
        match cmd {
            // Variable operations
            cmd::STO => StackEffect::fixed(2, &[]),
            cmd::RCL => StackEffect::fixed(1, &[None]),
            cmd::PURGE => StackEffect::fixed(1, &[]),
            cmd::VARS => StackEffect::fixed(0, &[Some(TypeId::LIST)]),
            cmd::CLVAR => StackEffect::fixed(0, &[]),
            cmd::INCR | cmd::DECR => StackEffect::fixed(1, &[None]),
            cmd::RENAME => StackEffect::fixed(2, &[]),
            // Directory navigation
            cmd::CRDIR => StackEffect::fixed(1, &[]),
            cmd::PGDIR => StackEffect::fixed(1, &[]),
            cmd::UPDIR => StackEffect::fixed(0, &[]),
            cmd::HOME => StackEffect::fixed(0, &[]),
            cmd::PATH => StackEffect::fixed(0, &[Some(TypeId::LIST)]),
            // Directory packing
            cmd::PACKDIR | cmd::UNPACKDIR => StackEffect::Dynamic,
            cmd::PACKINFO => StackEffect::fixed(1, &[Some(TypeId::LIST)]),
            _ => StackEffect::Dynamic,
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::STO => {
                // (value name --)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("STO: expected string name, got {}", name_val.type_name())
                })?;
                let value = ctx.pop()?;
                ctx.store(name, value);
                Ok(())
            }

            cmd::RCL => {
                // (name -- value)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("RCL: expected string name, got {}", name_val.type_name())
                })?;
                let value = ctx
                    .lookup(&name)
                    .ok_or_else(|| format!("Undefined: {}", name))?
                    .clone();
                ctx.push(value)?;
                Ok(())
            }

            cmd::PURGE => {
                // (name --)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("PURGE: expected string name, got {}", name_val.type_name())
                })?;
                ctx.purge(&name)
                    .ok_or_else(|| format!("Undefined: {}", name))?;
                Ok(())
            }

            cmd::VARS => {
                // (-- {names})
                let names: Vec<Value> = ctx.vars().map(|s| Value::string(s.as_str())).collect();
                ctx.push(Value::list(names))?;
                Ok(())
            }

            cmd::CLVAR => {
                // (--)
                ctx.clear_vars();
                Ok(())
            }

            cmd::INCR => {
                // (name -- new_value)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("INCR: expected string name, got {}", name_val.type_name())
                })?;
                let current = ctx
                    .lookup(&name)
                    .ok_or_else(|| format!("Undefined: {}", name))?
                    .clone();

                let new_val = match current {
                    Value::Integer(n) => Value::integer(n + 1),
                    Value::Real(n) => Value::real(n + 1.0),
                    _ => {
                        return Err(format!(
                            "INCR: expected numeric value, got {}",
                            current.type_name()
                        ))
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                ctx.push(result)?;
                Ok(())
            }

            cmd::DECR => {
                // (name -- new_value)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("DECR: expected string name, got {}", name_val.type_name())
                })?;
                let current = ctx
                    .lookup(&name)
                    .ok_or_else(|| format!("Undefined: {}", name))?
                    .clone();

                let new_val = match current {
                    Value::Integer(n) => Value::integer(n - 1),
                    Value::Real(n) => Value::real(n - 1.0),
                    _ => {
                        return Err(format!(
                            "DECR: expected numeric value, got {}",
                            current.type_name()
                        ))
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                ctx.push(result)?;
                Ok(())
            }

            cmd::RENAME => {
                // (old_name new_name --)
                let new_name_val = ctx.pop()?;
                let new_name = extract_name(&new_name_val).ok_or_else(|| {
                    format!("RENAME: expected string name, got {}", new_name_val.type_name())
                })?;
                let old_name_val = ctx.pop()?;
                let old_name = extract_name(&old_name_val).ok_or_else(|| {
                    format!("RENAME: expected string name, got {}", old_name_val.type_name())
                })?;
                if !ctx.rename_var(&old_name, &new_name) {
                    return Err(format!("Undefined: {}", old_name));
                }
                Ok(())
            }

            // === Directory navigation ===

            cmd::CRDIR => {
                // (name --)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("CRDIR: expected string name, got {}", name_val.type_name())
                })?;
                if !ctx.create_subdir(name.clone()) {
                    return Err(format!(
                        "CRDIR: cannot create '{}': name already exists",
                        name
                    ));
                }
                Ok(())
            }

            cmd::PGDIR => {
                // (name --)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("PGDIR: expected string name, got {}", name_val.type_name())
                })?;
                ctx.remove_subdir(&name)
                    .map_err(|e| format!("PGDIR: {}", e))?;
                Ok(())
            }

            cmd::UPDIR => {
                // (--)
                ctx.updir();
                Ok(())
            }

            cmd::HOME => {
                // (--)
                ctx.home();
                Ok(())
            }

            cmd::PATH => {
                // (-- {path})
                let path: Vec<Value> = ctx
                    .dir_path()
                    .iter()
                    .map(|s| Value::string(s.as_str()))
                    .collect();
                ctx.push(Value::list(path))?;
                Ok(())
            }

            // === Directory packing ===

            cmd::PACKDIR => {
                // PACKDIR: (-- packed) or (name -- packed)
                // Check if there's a string argument on the stack
                let pack_subdir = if ctx.depth() > 0 {
                    if let Ok(name_val) = ctx.peek(0) {
                        if let Some(name) = extract_name(name_val) {
                            // It's a name - pack that subdirectory
                            ctx.pop()?;
                            Some(name)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                let packed = if let Some(name) = pack_subdir {
                    // Pack named subdirectory
                    let subdir = ctx.directory.get_subdir(&name)
                        .ok_or_else(|| format!("PACKDIR: directory '{}' not found", name))?;
                    pack_directory(subdir)
                } else {
                    // Pack current directory
                    pack_directory(ctx.directory.current_node())
                };

                ctx.push(Value::bytes(packed))?;
                Ok(())
            }

            cmd::UNPACKDIR => {
                // UNPACKDIR: (packed --) or (packed name --)
                // Check stack: if depth >= 2 and top is string, unpack into named subdir
                let (packed_bytes, target_subdir) = if ctx.depth() >= 2 {
                    if let Ok(name_val) = ctx.peek(0) {
                        if let Some(name) = extract_name(name_val) {
                            // Unpack into named subdirectory
                            ctx.pop()?;
                            let packed_val = ctx.pop()?;
                            let bytes = packed_val.as_bytes()
                                .ok_or_else(|| format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name()))?
                                .clone();
                            (bytes, Some(name))
                        } else {
                            // Top is not a name, treat as packed bytes only
                            let packed_val = ctx.pop()?;
                            let bytes = packed_val.as_bytes()
                                .ok_or_else(|| format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name()))?
                                .clone();
                            (bytes, None)
                        }
                    } else {
                        let packed_val = ctx.pop()?;
                        let bytes = packed_val.as_bytes()
                            .ok_or_else(|| format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name()))?
                            .clone();
                        (bytes, None)
                    }
                } else {
                    let packed_val = ctx.pop()?;
                    let bytes = packed_val.as_bytes()
                        .ok_or_else(|| format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name()))?
                        .clone();
                    (bytes, None)
                };

                if let Some(name) = target_subdir {
                    // Create subdirectory if needed and unpack into it
                    ctx.create_subdir(name.clone());
                    let node = ctx.directory.current_node_mut().ensure_subdir(&name);
                    unpack_directory_checked(&packed_bytes, node)
                        .map_err(|e| match e {
                            Ok(se) => format!("UNPACKDIR: {}", se),
                            Err(conflict) => conflict.to_string(),
                        })?;
                } else {
                    // Unpack into current directory
                    let node = ctx.directory.current_node_mut();
                    unpack_directory_checked(&packed_bytes, node)
                        .map_err(|e| match e {
                            Ok(se) => format!("UNPACKDIR: {}", se),
                            Err(conflict) => conflict.to_string(),
                        })?;
                }

                Ok(())
            }

            cmd::PACKINFO => {
                // PACKINFO: (packed -- {names})
                let packed_val = ctx.pop()?;
                let bytes = packed_val.as_bytes()
                    .ok_or_else(|| format!("PACKINFO: expected bytes, got {}", packed_val.type_name()))?;

                let names = packinfo(bytes)
                    .map_err(|e| format!("PACKINFO: {}", e))?;

                let names_list: Vec<Value> = names.into_iter()
                    .map(|s| Value::string(s.as_str()))
                    .collect();
                ctx.push(Value::list(names_list))?;
                Ok(())
            }

            _ => Err(format!("Unknown directory command: {}", ctx.cmd)),
        }
    }
}

/// Extract a variable name from a value.
///
/// Supports both string names (`"x"`) and quoted symbol names (`'x'`).
fn extract_name(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.to_string()),
        Value::Symbolic(expr) => {
            // If it's just a variable name (like 'x'), extract it
            if let crate::symbolic::SymExpr::Var(name) = expr.as_ref() {
                Some(name.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::Library;

    #[test]
    fn directory_lib_id() {
        assert_eq!(DirectoryLib.id(), 28);
    }

    #[test]
    fn directory_lib_name() {
        assert_eq!(DirectoryLib.name(), "Directory");
    }

    #[test]
    fn commands_registered() {
        let cmds = DirectoryLib.commands();
        let names: Vec<_> = cmds.iter().map(|c| c.name).collect();
        // Variable operations
        assert!(names.contains(&"STO"));
        assert!(names.contains(&"RCL"));
        assert!(names.contains(&"PURGE"));
        assert!(names.contains(&"VARS"));
        assert!(names.contains(&"CLVAR"));
        assert!(names.contains(&"INCR"));
        assert!(names.contains(&"DECR"));
        assert!(names.contains(&"RENAME"));
        // Directory navigation
        assert!(names.contains(&"CRDIR"));
        assert!(names.contains(&"PGDIR"));
        assert!(names.contains(&"UPDIR"));
        assert!(names.contains(&"HOME"));
        assert!(names.contains(&"PATH"));
    }
}
