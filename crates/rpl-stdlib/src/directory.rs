use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

/// Library for variable directory operations (STO, RCL, PURGE).
pub struct DirectoryLib;

impl DirectoryLib {
    /// Library ID for directory operations.
    pub const ID: LibraryId = LibraryId::new(28);

    // Command IDs
    const CMD_STO: u16 = 0;
    const CMD_RCL: u16 = 1;
    const CMD_PURGE: u16 = 2;
    const CMD_VARS: u16 = 3;
    const CMD_CLVAR: u16 = 4;
    const CMD_INCR: u16 = 5;
    const CMD_DECR: u16 = 6;
    const CMD_RENAME: u16 = 7;
    const CMD_CRDIR: u16 = 8;
    const CMD_PGDIR: u16 = 9;
    const CMD_UPDIR: u16 = 10;
    const CMD_HOME: u16 = 11;
    const CMD_PATH: u16 = 12;

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
            _ => None,
        }
    }

    /// Extract the variable name from a value.
    /// Accepts String values directly, or Symbol values (returns raw index as string for now).
    fn extract_name(value: &Value) -> Option<String> {
        match value {
            Value::String(s) => Some(s.clone()),
            Value::Symbol(sym) => {
                // For symbols, we use the raw index as a placeholder
                // In a real implementation, we'd resolve via interner
                Some(format!("__sym_{}", sym.as_u32()))
            }
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
                // STO: ( value "name" -- )
                // Pop name, pop value, store value with name
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
                // RCL: ( "name" -- value )
                // Pop name, push recalled value
                // Uses lookup() which checks locals first, then globals
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
                // PURGE: ( "name" -- )
                // Pop name, delete variable
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
                // VARS: ( -- { names... } )
                // Push a list of all variable names
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
                // CLVAR: ( -- )
                // Clear all variables
                ctx.clear_vars();
                ExecuteResult::Ok
            }

            Self::CMD_INCR => {
                // INCR: ( "name" -- n+1 )
                // Increment variable by 1 and return new value
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
                // DECR: ( "name" -- n-1 )
                // Decrement variable by 1 and return new value
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
                // RENAME: ( "oldname" "newname" -- )
                // Rename a variable
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
                // CRDIR: ( "name" -- )
                // Create a new subdirectory
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
                // PGDIR: ( "name" -- )
                // Purge (delete) an empty directory
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
                // UPDIR: ( -- )
                // Move up one directory level
                if !ctx.updir() {
                    // Already at root - this is not an error, just a no-op
                }
                ExecuteResult::Ok
            }

            Self::CMD_HOME => {
                // HOME: ( -- )
                // Move to the root (HOME) directory
                ctx.home();
                ExecuteResult::Ok
            }

            Self::CMD_PATH => {
                // PATH: ( -- { path } )
                // Return the current directory path as a list
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
            "STO" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "RCL" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "PURGE" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "VARS" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
            "CLVAR" | "CLVARS" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "INCR" | "DECR" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "RENAME" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "CRDIR" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "PGDIR" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "UPDIR" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "HOME" => StackEffect::Fixed {
                consumes: 0,
                produces: 0,
            },
            "PATH" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
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
    fn probe_purge() {
        let interner = Interner::new();
        let lib = DirectoryLib;
        let ctx = make_probe_ctx("PURGE", &interner);
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
    fn probe_unknown() {
        let interner = Interner::new();
        let lib = DirectoryLib;
        let ctx = make_probe_ctx("UNKNOWN", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn execute_sto_rcl() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        // Push value and name: 42 "x" STO
        vm.push(Value::Int(42)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 0);

        // RCL: "x" RCL
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 1);
        assert_eq!(vm.pop_int().unwrap(), 42);
    }

    #[test]
    fn execute_sto_overwrite() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        // Store 10 in "x"
        vm.push(Value::Int(10)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        lib.execute(&mut ctx);

        // Overwrite with 20
        vm.push(Value::Int(20)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        lib.execute(&mut ctx);

        // RCL should get 20
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        lib.execute(&mut ctx);

        assert_eq!(vm.pop_int().unwrap(), 20);
    }

    #[test]
    fn execute_rcl_undefined() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.push(Value::String("nonexistent".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_purge() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        // Store 42 in "x"
        vm.push(Value::Int(42)).unwrap();
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        lib.execute(&mut ctx);

        // PURGE "x"
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PURGE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));

        // RCL should now fail
        vm.push(Value::String("x".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_RCL);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_purge_undefined() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        vm.push(Value::String("nonexistent".to_string())).unwrap();
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_PURGE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn execute_sto_wrong_type() {
        let lib = DirectoryLib;
        let mut vm = VM::new();

        // Try to STO with integer as name (should fail)
        vm.push(Value::Int(42)).unwrap();
        vm.push(Value::Int(1)).unwrap(); // not a string name
        let mut ctx = make_exec_ctx(&mut vm, DirectoryLib::CMD_STO);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn stack_effect_sto() {
        let lib = DirectoryLib;
        let effect = lib.stack_effect("STO");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 2,
                produces: 0
            }
        ));
    }

    #[test]
    fn stack_effect_rcl() {
        let lib = DirectoryLib;
        let effect = lib.stack_effect("RCL");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
    }

    #[test]
    fn compile_command() {
        use rpl_lang::compile::OutputBuffer;

        let lib = DirectoryLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));

        let mut ctx = CompileContext::new(span, "STO", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
        assert_eq!(ctx.position(), 1);
    }
}
