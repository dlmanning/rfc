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

use rpl_core::TypeId;
use rpl_lang::library::{ExecuteOk, StackEffect};
use rpl_lang::Value;

use packdir::{pack_directory, packdir_entries, unpack_directory};
#[cfg(test)]
use packdir::FORMAT_VERSION;

/// Extract the variable name from a value.
fn extract_name(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Symbol(sym) => Some(format!("__sym_{}", sym.as_u32())),
        _ => None,
    }
}

rpl_macros::define_library! {
    pub library DirectoryLib(28, "Directory");

    commands {
        STO | "sto" (2 -> 0) "Store value in variable" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "STO requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            let value = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            ctx.store(name, value);
            Ok(ExecuteOk::Ok)
        }

        RCL | "rcl" (1 -> 1) "Recall value from variable" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "RCL requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            match ctx.lookup(&name) {
                Some(value) => {
                    let value = value.clone();
                    if ctx.push(value).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                None => Err(format!("Undefined name: {}", name)),
            }
        }

        PURGE | "purge" (1 -> 0) "Delete variable" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "PURGE requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            if ctx.purge(&name).is_none() {
                return Err(format!("Undefined name: {}", name));
            }
            Ok(ExecuteOk::Ok)
        }

        VARS | "vars" (0 -> 1) "List all variables" {
            let names: Vec<Value> = ctx
                .vars()
                .map(|name| Value::String(name.clone()))
                .collect();
            if ctx.push(Value::List(names)).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        CLVAR | "clvar" | "CLVARS" | "clvars" (0 -> 0) "Clear all variables" {
            ctx.clear_vars();
            Ok(ExecuteOk::Ok)
        }

        INCR | "incr" (1 -> 1) "Increment numeric variable" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "INCR requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            let current = match ctx.recall(&name) {
                Some(v) => v.clone(),
                None => return Err(format!("Undefined name: {}", name)),
            };

            let new_val = match current {
                Value::Real(r) => Value::Real(r + 1.0),
                Value::Int(i) => Value::Int(i + 1),
                _ => {
                    return Err(format!(
                        "INCR requires numeric value, got {:?}",
                        current.type_id()
                    ))
                }
            };

            let result = new_val.clone();
            ctx.store(name, new_val);
            if ctx.push(result).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        DECR | "decr" (1 -> 1) "Decrement numeric variable" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "DECR requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            let current = match ctx.recall(&name) {
                Some(v) => v.clone(),
                None => return Err(format!("Undefined name: {}", name)),
            };

            let new_val = match current {
                Value::Real(r) => Value::Real(r - 1.0),
                Value::Int(i) => Value::Int(i - 1),
                _ => {
                    return Err(format!(
                        "DECR requires numeric value, got {:?}",
                        current.type_id()
                    ))
                }
            };

            let result = new_val.clone();
            ctx.store(name, new_val);
            if ctx.push(result).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        RENAME | "rename" (2 -> 0) "Rename variable" {
            let new_name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let old_name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let old_name = match extract_name(&old_name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "RENAME requires string names, got {:?}",
                        old_name_val.type_id()
                    ));
                }
            };

            let new_name = match extract_name(&new_name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "RENAME requires string names, got {:?}",
                        new_name_val.type_id()
                    ));
                }
            };

            if !ctx.rename_var(&old_name, &new_name) {
                return Err(format!("Undefined name: {}", old_name));
            }
            Ok(ExecuteOk::Ok)
        }

        CRDIR | "crdir" (1 -> 0) "Create subdirectory" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "CRDIR requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            if !ctx.create_subdir(name.clone()) {
                return Err(format!(
                    "Cannot create directory '{}': name already exists",
                    name
                ));
            }
            Ok(ExecuteOk::Ok)
        }

        PGDIR | "pgdir" (1 -> 0) "Delete empty subdirectory" {
            let name_val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let name = match extract_name(&name_val) {
                Some(n) => n,
                None => {
                    return Err(format!(
                        "PGDIR requires a string name, got {:?}",
                        name_val.type_id()
                    ));
                }
            };

            if let Err(e) = ctx.remove_subdir(&name) {
                return Err(format!("PGDIR '{}': {}", name, e));
            }
            Ok(ExecuteOk::Ok)
        }

        UPDIR | "updir" (0 -> 0) "Move up one directory level" {
            ctx.updir();
            Ok(ExecuteOk::Ok)
        }

        HOME | "home" (0 -> 0) "Move to root directory" {
            ctx.home();
            Ok(ExecuteOk::Ok)
        }

        PATH | "path" (0 -> 1) "Get current directory path" {
            let path = ctx.dir_path();
            let path_list: Vec<Value> = path
                .iter()
                .map(|s| Value::String(s.clone()))
                .collect();
            if ctx.push(Value::List(path_list)).is_err() {
                return Err("Stack overflow".to_string());
            }
            Ok(ExecuteOk::Ok)
        }

        PACKDIR | "packdir" (0 -> 1) "Pack directory into portable object" {
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
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(e) => Err(format!("PACKDIR: {}", e)),
            }
        }

        UNPACKDIR | "unpackdir" (1 -> 0) "Unpack into current or new directory" {
            let (packdir, dest_name) = match ctx.peek(0) {
                Ok(Value::String(s)) => {
                    let name = s.clone();
                    let _ = ctx.pop();
                    match ctx.pop() {
                        Ok(p) => (p, Some(name)),
                        Err(_) => {
                            return Err(
                                "UNPACKDIR: stack underflow".to_string(),
                            );
                        }
                    }
                }
                Ok(Value::Object { type_id, .. }) if *type_id == TypeId::PACKDIR => {
                    match ctx.pop() {
                        Ok(p) => (p, None),
                        Err(_) => {
                            return Err(
                                "UNPACKDIR: stack underflow".to_string(),
                            );
                        }
                    }
                }
                Ok(_) => {
                    return Err(
                        "UNPACKDIR: expected PACKDIR or destination name".to_string(),
                    );
                }
                Err(_) => {
                    return Err("UNPACKDIR: stack underflow".to_string());
                }
            };

            match unpack_directory(ctx, &packdir, dest_name.as_deref()) {
                Ok(()) => Ok(ExecuteOk::Ok),
                Err(e) => Err(format!("UNPACKDIR: {}", e)),
            }
        }

        PACKINFO | "packinfo" (1 -> 1) "Get entry names from PACKDIR" {
            let packdir = match ctx.pop() {
                Ok(p) => p,
                Err(_) => return Err("PACKINFO: stack underflow".to_string()),
            };

            match packdir_entries(&packdir) {
                Ok(names) => {
                    let list: Vec<Value> = names.into_iter().map(Value::String).collect();
                    if ctx.push(Value::List(list)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    Ok(ExecuteOk::Ok)
                }
                Err(e) => Err(format!("PACKINFO: {}", e)),
            }
        }
    }

    custom stack_effect {
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
    use rpl_lang::library::{ExecuteContext, Library, ProbeContext, ProbeResult};
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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert_eq!(vm.depth(), 0);

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
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
        let _ = lib.execute(&mut ctx);

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PURGE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(_)));
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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        let _ = lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.clear_vars();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        let _ = lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Err(e) if e.contains("already exists")));
    }

    #[test]
    fn unpack_into_new_subdir() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.store("x".to_string(), Value::Int(42));
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKDIR);
        let _ = lib.execute(&mut ctx);
        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        vm.push(Value::String("backup".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_UNPACKDIR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

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
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        let packdir = vm.pop().unwrap();

        vm.push(packdir).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PACKINFO);
        let _ = lib.execute(&mut ctx);

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
