//! Directory operations for RPL.
//!
//! This module provides commands for variable storage, directory navigation,
//! and directory serialization (PACKDIR).
//!
//! ## Variable Commands
//!
//! - `STO` - Store value in variable
//! - `RCL` - Recall value from variable
//! - `PURGE` - Delete variable
//! - `VARS` - List all variables
//! - `CLVAR` - Clear all variables
//! - `INCR` / `DECR` - Increment/decrement numeric variable
//! - `RENAME` - Rename variable
//!
//! ## Directory Navigation
//!
//! - `CRDIR` - Create subdirectory
//! - `PGDIR` - Delete empty subdirectory
//! - `UPDIR` - Move up one directory level
//! - `HOME` - Move to root directory
//! - `PATH` - Get current directory path
//!
//! ## Directory Serialization
//!
//! - `PACKDIR` - Pack directory into portable object
//! - `UNPACKDIR` - Unpack into current or new directory
//! - `PACKINFO` - Get entry names from PACKDIR

mod packdir;
pub(crate) mod serialize;

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

use packdir::{pack_directory, packdir_entries, unpack_directory};
#[cfg(test)]
use packdir::FORMAT_VERSION;

/// Library for directory operations.
pub struct DirectoryLib;

impl DirectoryLib {
    /// Library ID for directory operations.
    pub const ID: LibraryId = LibraryId::new(28);

    // Command IDs - Variable operations
    const CMD_STO: u16 = 0;
    const CMD_RCL: u16 = 1;
    const CMD_PURGE: u16 = 2;
    const CMD_VARS: u16 = 3;
    const CMD_CLVAR: u16 = 4;
    const CMD_INCR: u16 = 5;
    const CMD_DECR: u16 = 6;
    const CMD_RENAME: u16 = 7;

    // Command IDs - Directory navigation
    const CMD_CRDIR: u16 = 8;
    const CMD_PGDIR: u16 = 9;
    const CMD_UPDIR: u16 = 10;
    const CMD_HOME: u16 = 11;
    const CMD_PATH: u16 = 12;

    // Command IDs - PACKDIR operations
    const CMD_PACKDIR: u16 = 13;
    const CMD_UNPACKDIR: u16 = 14;
    const CMD_PACKINFO: u16 = 15;

    /// Get command ID from name.
    fn command_id(name: &str) -> Option<u16> {
        match name.to_ascii_uppercase().as_str() {
            "STO" => Some(Self::CMD_STO),
            "RCL" => Some(Self::CMD_RCL),
            "PURGE" => Some(Self::CMD_PURGE),
            "VARS" => Some(Self::CMD_VARS),
            "CLVAR" | "CLVARS" => Some(Self::CMD_CLVAR),
            "INCR" => Some(Self::CMD_INCR),
            "DECR" => Some(Self::CMD_DECR),
            "RENAME" => Some(Self::CMD_RENAME),
            "CRDIR" => Some(Self::CMD_CRDIR),
            "PGDIR" => Some(Self::CMD_PGDIR),
            "UPDIR" => Some(Self::CMD_UPDIR),
            "HOME" => Some(Self::CMD_HOME),
            "PATH" => Some(Self::CMD_PATH),
            "PACKDIR" => Some(Self::CMD_PACKDIR),
            "UNPACKDIR" => Some(Self::CMD_UNPACKDIR),
            "PACKINFO" => Some(Self::CMD_PACKINFO),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_STO => Some("STO"),
            Self::CMD_RCL => Some("RCL"),
            Self::CMD_PURGE => Some("PURGE"),
            Self::CMD_VARS => Some("VARS"),
            Self::CMD_CLVAR => Some("CLVAR"),
            Self::CMD_INCR => Some("INCR"),
            Self::CMD_DECR => Some("DECR"),
            Self::CMD_RENAME => Some("RENAME"),
            Self::CMD_CRDIR => Some("CRDIR"),
            Self::CMD_PGDIR => Some("PGDIR"),
            Self::CMD_UPDIR => Some("UPDIR"),
            Self::CMD_HOME => Some("HOME"),
            Self::CMD_PATH => Some("PATH"),
            Self::CMD_PACKDIR => Some("PACKDIR"),
            Self::CMD_UNPACKDIR => Some("UNPACKDIR"),
            Self::CMD_PACKINFO => Some("PACKINFO"),
            _ => None,
        }
    }

    /// Extract the variable name from a value.
    fn extract_name(value: &Value) -> Option<String> {
        match value {
            Value::String(s) => Some(s.clone()),
            Value::Symbol(sym) => Some(format!("__sym_{}", sym.as_u32())),
            _ => None,
        }
    }
}

impl Library for DirectoryLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Directory"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if Self::command_id(text).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        if let Some(cmd_id) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd_id);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_STO => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "STO requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                let value = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                ctx.store(name, value);
                ExecuteResult::Ok
            }

            Self::CMD_RCL => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "RCL requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                match ctx.lookup(&name) {
                    Some(value) => {
                        let value = value.clone();
                        if ctx.push(value).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Undefined name: {}", name)),
                }
            }

            Self::CMD_PURGE => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "PURGE requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                if ctx.purge(&name).is_none() {
                    return ExecuteResult::Error(format!("Undefined name: {}", name));
                }
                ExecuteResult::Ok
            }

            Self::CMD_VARS => {
                let names: Vec<Value> = ctx
                    .vars()
                    .map(|name| Value::String(name.clone()))
                    .collect();
                if ctx.push(Value::List(names)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }

            Self::CMD_CLVAR => {
                ctx.clear_vars();
                ExecuteResult::Ok
            }

            Self::CMD_INCR => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "INCR requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                let current = match ctx.recall(&name) {
                    Some(v) => v.clone(),
                    None => return ExecuteResult::Error(format!("Undefined name: {}", name)),
                };

                let new_val = match current {
                    Value::Real(r) => Value::Real(r + 1.0),
                    Value::Int(i) => Value::Int(i + 1),
                    _ => {
                        return ExecuteResult::Error(format!(
                            "INCR requires numeric value, got {:?}",
                            current.type_id()
                        ))
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                if ctx.push(result).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }

            Self::CMD_DECR => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "DECR requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                let current = match ctx.recall(&name) {
                    Some(v) => v.clone(),
                    None => return ExecuteResult::Error(format!("Undefined name: {}", name)),
                };

                let new_val = match current {
                    Value::Real(r) => Value::Real(r - 1.0),
                    Value::Int(i) => Value::Int(i - 1),
                    _ => {
                        return ExecuteResult::Error(format!(
                            "DECR requires numeric value, got {:?}",
                            current.type_id()
                        ))
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                if ctx.push(result).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }

            Self::CMD_RENAME => {
                let new_name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let old_name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let old_name = match Self::extract_name(&old_name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "RENAME requires string names, got {:?}",
                            old_name_val.type_id()
                        ));
                    }
                };

                let new_name = match Self::extract_name(&new_name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "RENAME requires string names, got {:?}",
                            new_name_val.type_id()
                        ));
                    }
                };

                if !ctx.rename_var(&old_name, &new_name) {
                    return ExecuteResult::Error(format!("Undefined name: {}", old_name));
                }
                ExecuteResult::Ok
            }

            Self::CMD_CRDIR => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "CRDIR requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                if !ctx.create_subdir(name.clone()) {
                    return ExecuteResult::Error(format!(
                        "Cannot create directory '{}': name already exists",
                        name
                    ));
                }
                ExecuteResult::Ok
            }

            Self::CMD_PGDIR => {
                let name_val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let name = match Self::extract_name(&name_val) {
                    Some(n) => n,
                    None => {
                        return ExecuteResult::Error(format!(
                            "PGDIR requires a string name, got {:?}",
                            name_val.type_id()
                        ));
                    }
                };

                if let Err(e) = ctx.remove_subdir(&name) {
                    return ExecuteResult::Error(format!("PGDIR '{}': {}", name, e));
                }
                ExecuteResult::Ok
            }

            Self::CMD_UPDIR => {
                ctx.updir();
                ExecuteResult::Ok
            }

            Self::CMD_HOME => {
                ctx.home();
                ExecuteResult::Ok
            }

            Self::CMD_PATH => {
                let path = ctx.dir_path();
                let path_list: Vec<Value> = path
                    .iter()
                    .map(|s| Value::String(s.clone()))
                    .collect();
                if ctx.push(Value::List(path_list)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }

            Self::CMD_PACKDIR => {
                // Check if there's a string on the stack (subdirectory name)
                let subdir_name = match ctx.peek(0) {
                    Ok(Value::String(s)) => {
                        let name = s.clone();
                        let _ = ctx.pop();
                        Some(name)
                    }
                    _ => None,
                };

                match pack_directory(ctx, subdir_name.as_deref()) {
                    Ok(packdir) => {
                        if ctx.push(packdir).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(e) => ExecuteResult::Error(format!("PACKDIR: {}", e)),
                }
            }

            Self::CMD_UNPACKDIR => {
                // Check stack: could be (packdir) or (packdir "name")
                let (packdir, dest_name) = match ctx.peek(0) {
                    Ok(Value::String(s)) => {
                        let name = s.clone();
                        let _ = ctx.pop();
                        match ctx.pop() {
                            Ok(p) => (p, Some(name)),
                            Err(_) => {
                                return ExecuteResult::Error(
                                    "UNPACKDIR: stack underflow".to_string(),
                                );
                            }
                        }
                    }
                    Ok(Value::Object { type_id, .. }) if *type_id == TypeId::PACKDIR => {
                        match ctx.pop() {
                            Ok(p) => (p, None),
                            Err(_) => {
                                return ExecuteResult::Error(
                                    "UNPACKDIR: stack underflow".to_string(),
                                );
                            }
                        }
                    }
                    Ok(_) => {
                        return ExecuteResult::Error(
                            "UNPACKDIR: expected PACKDIR or destination name".to_string(),
                        );
                    }
                    Err(_) => {
                        return ExecuteResult::Error("UNPACKDIR: stack underflow".to_string());
                    }
                };

                match unpack_directory(ctx, &packdir, dest_name.as_deref()) {
                    Ok(()) => ExecuteResult::Ok,
                    Err(e) => ExecuteResult::Error(format!("UNPACKDIR: {}", e)),
                }
            }

            Self::CMD_PACKINFO => {
                let packdir = match ctx.pop() {
                    Ok(p) => p,
                    Err(_) => return ExecuteResult::Error("PACKINFO: stack underflow".to_string()),
                };

                match packdir_entries(&packdir) {
                    Ok(names) => {
                        let list: Vec<Value> = names.into_iter().map(Value::String).collect();
                        if ctx.push(Value::List(list)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(e) => ExecuteResult::Error(format!("PACKINFO: {}", e)),
                }
            }

            _ => ExecuteResult::Error(format!("Unknown directory command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => DecompileResult::Unknown,
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token.to_ascii_uppercase().as_str() {
            "STO" | "RENAME" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "RCL" | "INCR" | "DECR" | "PACKINFO" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "PURGE" | "CRDIR" | "PGDIR" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "VARS" | "PATH" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
            "CLVAR" | "CLVARS" | "UPDIR" | "HOME" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            // PACKDIR and UNPACKDIR have variable stack effects
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::VM;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &'_ mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    // Variable operation tests

    #[test]
    fn probe_sto() {
        let interner = Interner::new();
        let lib = DirectoryLib;
        let ctx = make_probe_ctx("STO", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_rcl() {
        let interner = Interner::new();
        let lib = DirectoryLib;
        let ctx = make_probe_ctx("RCL", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = DirectoryLib;

        let ctx = make_probe_ctx("sto", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("Rcl", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn execute_sto_rcl() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.push(Value::Int(42)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 0);

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 1);
        assert_eq!(vm.pop_int().unwrap(), 42);
    }

    #[test]
    fn execute_purge() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.push(Value::Int(42)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        lib.execute(&mut ctx);

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PURGE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    // PACKDIR tests

    #[test]
    fn probe_packdir_commands() {
        let interner = Interner::new();
        let lib = DirectoryLib;

        for cmd in &["PACKDIR", "UNPACKDIR", "PACKINFO"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                cmd
            );
        }
    }

    #[test]
    fn pack_empty_directory() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        let packdir = vm.pop().unwrap();
        if let Value::Object { type_id, data } = packdir {
            assert_eq!(type_id, TypeId::PACKDIR);
            assert_eq!(data[0], FORMAT_VERSION);
            assert_eq!(data[2], 0); // var_count
            assert_eq!(data[3], 0); // subdir_count
        } else {
            panic!("Expected PACKDIR object");
        }
    }

    #[test]
    fn pack_directory_with_variables() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.store("x".to_string(), Value::Int(42));
        vm.store("msg".to_string(), Value::String("hello".to_string()));

        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        let packdir = vm.pop().unwrap();
        if let Value::Object { type_id, data } = &packdir {
            assert_eq!(*type_id, TypeId::PACKDIR);
            assert_eq!(data[2], 2); // var_count
        } else {
            panic!("Expected PACKDIR object");
        }

        // Check PACKINFO
        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKINFO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        if let Value::List(names) = vm.pop().unwrap() {
            assert_eq!(names.len(), 2);
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn unpack_into_empty_directory() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.store("x".to_string(), Value::Int(42));
        vm.store("y".to_string(), Value::Real(3.14));

        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.clear_vars();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        assert!(vm.has_var("x"));
        assert!(vm.has_var("y"));
        assert!(matches!(vm.recall("x"), Some(Value::Int(42))));
    }

    #[test]
    fn unpack_conflict_error() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.store("x".to_string(), Value::Int(1));
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(e) if e.contains("already exists")));
    }

    #[test]
    fn unpack_into_new_subdir() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.store("x".to_string(), Value::Int(42));
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        vm.push(Value::String("backup".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        assert!(vm.enter_subdir("backup"));
        assert!(vm.has_var("x"));
        assert!(matches!(vm.recall("x"), Some(Value::Int(42))));
        vm.updir();
    }

    #[test]
    fn pack_named_subdirectory() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.create_subdir("sub".to_string());
        vm.enter_subdir("sub");
        vm.store("inner".to_string(), Value::Int(99));
        vm.updir();

        vm.push(Value::String("sub".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKINFO);
        lib.execute(&mut ctx);

        if let Value::List(names) = vm.pop().unwrap() {
            assert_eq!(names.len(), 1);
            assert!(matches!(&names[0], Value::String(s) if s == "inner"));
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn stack_effect_commands() {
        let lib = DirectoryLib;

        assert!(matches!(
            lib.stack_effect("STO"),
            StackEffect::Fixed { consumes: 2, produces: 0 }
        ));
        assert!(matches!(
            lib.stack_effect("RCL"),
            StackEffect::Fixed { consumes: 1, produces: 1 }
        ));
        assert!(matches!(
            lib.stack_effect("PACKINFO"),
            StackEffect::Fixed { consumes: 1, produces: 1 }
        ));
        assert!(matches!(lib.stack_effect("PACKDIR"), StackEffect::Dynamic));
    }
}
