//! User library support.
//!
//! Provides commands for:
//! - Library private data: LIBSTO, LIBRCL, LIBDEFRCL, LIBCLEAR
//! - Library management: CRLIB, ATTACH, DETACH
//!
//! Library private data is stored at `.SETTINGS.LIBDATA.<LIBID>.<VARNAME>`.

use std::sync::Arc;

use crate::core::Span;

use crate::{
    ir::{Branch, LibId},
    libs::{CommandInfo, ExecuteContext, ExecuteResult, Library, LibraryExecutor, LibraryLowerer, StackEffect},
    lower::{LowerContext, LowerError},
    value::{LibraryCommand, LibraryData, Value},
};

/// User library ID (matches rpl-stdlib LIBPTR_LIB = 102).
pub const USERLIB_LIB: LibId = 102;

/// User library command IDs.
pub mod cmd {
    /// Store in library private data (LIBSTO).
    pub const LIBSTO: u16 = 0;
    /// Recall from library private data (LIBRCL).
    pub const LIBRCL: u16 = 1;
    /// Recall with default (LIBDEFRCL).
    pub const LIBDEFRCL: u16 = 2;
    /// Clear library private data (LIBCLEAR).
    pub const LIBCLEAR: u16 = 3;
    /// Create library (CRLIB).
    pub const CRLIB: u16 = 4;
    /// Attach library (ATTACH).
    pub const ATTACH: u16 = 5;
    /// Detach library (DETACH).
    pub const DETACH: u16 = 6;
}

/// User library support.
#[derive(Clone, Copy)]
pub struct UserLibLib;

impl Library for UserLibLib {
    fn id(&self) -> LibId {
        USERLIB_LIB
    }

    fn name(&self) -> &'static str {
        "UserLib"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            // LIBSTO: (value libid varname --)
            CommandInfo::with_effect("LIBSTO", USERLIB_LIB, cmd::LIBSTO, 3, 0),
            // LIBRCL: (libid varname -- value)
            CommandInfo::with_effect("LIBRCL", USERLIB_LIB, cmd::LIBRCL, 2, 1),
            // LIBDEFRCL: (default libid varname -- value)
            CommandInfo::with_effect("LIBDEFRCL", USERLIB_LIB, cmd::LIBDEFRCL, 3, 1),
            // LIBCLEAR: (libid --)
            CommandInfo::with_effect("LIBCLEAR", USERLIB_LIB, cmd::LIBCLEAR, 1, 0),
            // CRLIB: (list libid -- library) - will be implemented in Phase 4
            CommandInfo::with_effect("CRLIB", USERLIB_LIB, cmd::CRLIB, 2, 1),
            // ATTACH: (library --) - will be implemented in Phase 6
            CommandInfo::with_effect("ATTACH", USERLIB_LIB, cmd::ATTACH, 1, 0),
            // DETACH: (libid --) - will be implemented in Phase 6
            CommandInfo::with_effect("DETACH", USERLIB_LIB, cmd::DETACH, 1, 0),
        ]
    }
}

impl LibraryLowerer for UserLibLib {
    fn lower_composite(
        &self,
        _id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError { span: None,
            message: "UserLib has no composites".into(),
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<StackEffect, LowerError> {
        ctx.output.emit_call_lib(USERLIB_LIB, cmd);
        Ok(match cmd {
            cmd::LIBSTO => StackEffect::fixed(3, &[]),
            cmd::LIBRCL => StackEffect::fixed(2, &[None]),
            cmd::LIBDEFRCL => StackEffect::fixed(3, &[None]),
            cmd::LIBCLEAR => StackEffect::fixed(1, &[]),
            cmd::CRLIB => StackEffect::fixed(2, &[None]), // Returns Library
            cmd::ATTACH => StackEffect::fixed(1, &[]),
            cmd::DETACH => StackEffect::fixed(1, &[]),
            _ => StackEffect::Dynamic,
        })
    }
}

impl LibraryExecutor for UserLibLib {
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::LIBSTO => {
                // LIBSTO: (value libid varname --)
                // Stack: level 3=value, level 2=libid, level 1=varname
                let varname = extract_string(ctx.pop()?)?;
                let libid = extract_string(ctx.pop()?)?;
                let value = ctx.pop()?;

                // Store at .SETTINGS.LIBDATA.<libid>.<varname>
                let libid_str = libid.as_str();
                ctx.store_at_path(&["SETTINGS", "LIBDATA", libid_str], &varname, value);
                Ok(())
            }

            cmd::LIBRCL => {
                // LIBRCL: (libid varname -- value)
                let varname = extract_string(ctx.pop()?)?;
                let libid = extract_string(ctx.pop()?)?;

                // Look up at .SETTINGS.LIBDATA.<libid>.<varname>
                let libid_str = libid.as_str();
                match ctx.lookup_at_path(&["SETTINGS", "LIBDATA", libid_str], &varname) {
                    Some(value) => {
                        ctx.push(value.clone())?;
                        Ok(())
                    }
                    None => Err(format!("LIBRCL: variable '{}' not found in library '{}'", varname, libid)),
                }
            }

            cmd::LIBDEFRCL => {
                // LIBDEFRCL: (default libid varname -- value)
                let varname = extract_string(ctx.pop()?)?;
                let libid = extract_string(ctx.pop()?)?;
                let default = ctx.pop()?;

                // Look up at .SETTINGS.LIBDATA.<libid>.<varname>
                let libid_str = libid.as_str();
                match ctx.lookup_at_path(&["SETTINGS", "LIBDATA", libid_str], &varname) {
                    Some(value) => {
                        ctx.push(value.clone())?;
                        Ok(())
                    }
                    None => {
                        // Return default value
                        ctx.push(default)?;
                        Ok(())
                    }
                }
            }

            cmd::LIBCLEAR => {
                // LIBCLEAR: (libid --)
                let libid = extract_string(ctx.pop()?)?;

                // Clear all vars at .SETTINGS.LIBDATA.<libid>
                let libid_str = libid.as_str();
                ctx.clear_at_path(&["SETTINGS", "LIBDATA", libid_str]);
                Ok(())
            }

            cmd::CRLIB => {
                // CRLIB: (list libid -- library)
                // Stack: level 2=list, level 1=libid
                let libid = extract_string(ctx.pop()?)?;
                validate_lib_id(&libid)?;

                let list_value = ctx.pop()?;
                let list = list_value.as_list().ok_or_else(|| {
                    format!("CRLIB: expected list, got {}", list_value.type_name())
                })?;

                let mut commands = Vec::new();
                for (i, item) in list.iter().enumerate() {
                    let entry = item.as_list().ok_or_else(|| {
                        format!(
                            "CRLIB: entry {} must be a list {{name program}}, got {}",
                            i,
                            item.type_name()
                        )
                    })?;

                    if entry.len() != 2 {
                        return Err(format!(
                            "CRLIB: entry {} must have exactly 2 elements {{name program}}, got {}",
                            i,
                            entry.len()
                        ));
                    }

                    let name = entry[0].as_string().ok_or_else(|| {
                        format!(
                            "CRLIB: entry {} name must be a string, got {}",
                            i,
                            entry[0].type_name()
                        )
                    })?;

                    let prog = entry[1].as_program().ok_or_else(|| {
                        format!(
                            "CRLIB: entry {} program must be a program, got {}",
                            i,
                            entry[1].type_name()
                        )
                    })?;

                    commands.push(LibraryCommand::new(
                        name.to_uppercase(),
                        prog.code.clone(),
                        prog.strings.clone(),
                    ));
                }

                let lib_data = LibraryData::with_commands(libid.to_uppercase(), commands);
                ctx.push(Value::Library(Arc::new(lib_data)))?;
                Ok(())
            }

            cmd::ATTACH => {
                // ATTACH: (library --)
                // Store library at .SETTINGS.LIB.<id>
                let library = ctx.pop()?;
                let lib_data = library.as_library().ok_or_else(|| {
                    format!("ATTACH: expected library, got {}", library.type_name())
                })?;

                // Store the library at .SETTINGS.LIB.<id>
                let lib_id = lib_data.id.as_str();
                ctx.store_at_path(&["SETTINGS", "LIB"], lib_id, library.clone());
                Ok(())
            }

            cmd::DETACH => {
                // DETACH: (libid --)
                // Remove library from .SETTINGS.LIB.<id>
                let libid = extract_string(ctx.pop()?)?;
                let libid_upper = libid.to_uppercase();

                // Remove from .SETTINGS.LIB.<id>
                ctx.purge_at_path(&["SETTINGS", "LIB"], &libid_upper);
                Ok(())
            }

            _ => Err(format!("Unknown userlib command: {}", ctx.cmd)),
        }
    }
}

/// Extract a string from a value.
fn extract_string(value: Value) -> Result<String, String> {
    match value {
        Value::String(s) => Ok(s.to_string()),
        _ => Err(format!("expected string, got {}", value.type_name())),
    }
}

/// Validate a library ID (1-4 alphanumeric characters).
fn validate_lib_id(id: &str) -> Result<(), String> {
    if id.is_empty() || id.len() > 4 {
        return Err(format!(
            "CRLIB: library ID must be 1-4 characters, got '{}'",
            id
        ));
    }
    if !id.chars().all(|c| c.is_ascii_alphanumeric()) {
        return Err(format!(
            "CRLIB: library ID must be alphanumeric, got '{}'",
            id
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::Library;

    #[test]
    fn userlib_lib_id() {
        assert_eq!(UserLibLib.id(), 102);
    }

    #[test]
    fn userlib_lib_name() {
        assert_eq!(UserLibLib.name(), "UserLib");
    }

    #[test]
    fn userlib_commands_registered() {
        let cmds = UserLibLib.commands();
        let names: Vec<_> = cmds.iter().map(|c| c.name).collect();
        assert!(names.contains(&"LIBSTO"));
        assert!(names.contains(&"LIBRCL"));
        assert!(names.contains(&"LIBDEFRCL"));
        assert!(names.contains(&"LIBCLEAR"));
        assert!(names.contains(&"CRLIB"));
        assert!(names.contains(&"ATTACH"));
        assert!(names.contains(&"DETACH"));
    }
}
