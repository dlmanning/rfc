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

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::LibId,
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    serialize::{pack_directory, packinfo, unpack_directory_checked},
    value::Value,
};

/// Interface declaration for the Directory library.
const INTERFACE: &str = include_str!("interfaces/directory.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid directory interface"))
}

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

/// Directory library (implementation only).
#[derive(Clone, Copy)]
pub struct DirectoryLib;

impl LibraryLowerer for DirectoryLib {
    fn id(&self) -> LibId {
        DIRECTORY_LIB
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
}

impl LibraryExecutor for DirectoryLib {
    fn id(&self) -> LibId {
        DIRECTORY_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::STO => {
                // (value name --)
                // name can be: string, symbol, or list (path)
                let name_val = ctx.pop()?;
                let value = ctx.pop()?;

                // Check if it's a list-based path
                if let Some(list) = name_val.as_list() {
                    let elements = extract_path_elements(list).ok_or_else(|| {
                        "STO: invalid path list (expected strings or symbols)".to_string()
                    })?;
                    if elements.is_empty() {
                        return Err("STO: empty path".into());
                    }

                    // Last element is the variable name
                    let var_name = match elements.last() {
                        Some(PathElement::Name(n)) => n.clone(),
                        _ => return Err("STO: path must end with a variable name".into()),
                    };

                    // Navigate to the target directory
                    let saved_path: Vec<String> = ctx.dir_path().to_vec();
                    for elem in &elements[..elements.len() - 1] {
                        match elem {
                            PathElement::Home => ctx.home(),
                            PathElement::UpDir => ctx.updir(),
                            PathElement::Name(dir) => {
                                // Ensure directory exists and enter it
                                ctx.create_subdir(dir.clone());
                                if !ctx.enter_subdir(dir) {
                                    // Restore path and error
                                    ctx.home();
                                    for p in &saved_path {
                                        ctx.enter_subdir(p);
                                    }
                                    return Err(format!("STO: cannot enter directory '{}'", dir));
                                }
                            }
                        }
                    }

                    // Store the value
                    ctx.store(var_name, value);

                    // Restore original directory
                    ctx.home();
                    for p in &saved_path {
                        ctx.enter_subdir(p);
                    }

                    return Ok(ExecuteAction::ok());
                }

                // Must be symbolic (like 'x' or 'dir.subdir.var')
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!(
                        "STO: expected symbolic name 'x' or list path, got {}",
                        name_val.type_name()
                    )
                })?;

                // Check for path-based access (e.g., 'entities.0.x')
                if name.contains('.') {
                    let parts: Vec<&str> = name.split('.').collect();
                    let (path, var_name) = parts.split_at(parts.len() - 1);
                    ctx.directory.store_at_path(path, var_name[0], value);
                } else {
                    ctx.store(name, value);
                }
                Ok(ExecuteAction::ok())
            }

            cmd::RCL => {
                // (name -- value)
                // name can be: string, symbol, or list (path)
                let name_val = ctx.pop()?;

                // Check if it's a list-based path
                if let Some(list) = name_val.as_list() {
                    let elements = extract_path_elements(list).ok_or_else(|| {
                        "RCL: invalid path list (expected strings or symbols)".to_string()
                    })?;
                    if elements.is_empty() {
                        return Err("RCL: empty path".into());
                    }

                    // Last element is the variable name
                    let var_name = match elements.last() {
                        Some(PathElement::Name(n)) => n.clone(),
                        _ => return Err("RCL: path must end with a variable name".into()),
                    };

                    // Navigate to the target directory
                    let saved_path: Vec<String> = ctx.dir_path().to_vec();
                    for elem in &elements[..elements.len() - 1] {
                        match elem {
                            PathElement::Home => ctx.home(),
                            PathElement::UpDir => ctx.updir(),
                            PathElement::Name(dir) => {
                                if !ctx.enter_subdir(dir) {
                                    // Restore path and error
                                    ctx.home();
                                    for p in &saved_path {
                                        ctx.enter_subdir(p);
                                    }
                                    return Err(format!("Undefined: directory '{}'", dir));
                                }
                            }
                        }
                    }

                    // Look up the value
                    let value = ctx
                        .lookup(&var_name)
                        .ok_or_else(|| format!("Undefined: {}", var_name))?
                        .clone();

                    // Restore original directory
                    ctx.home();
                    for p in &saved_path {
                        ctx.enter_subdir(p);
                    }

                    ctx.push(value)?;
                    return Ok(ExecuteAction::ok());
                }

                // Must be symbolic (like 'x' or 'dir.subdir.var')
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!(
                        "RCL: expected symbolic name 'x' or list path, got {}",
                        name_val.type_name()
                    )
                })?;

                // Check for path-based access (e.g., 'entities.0.x')
                let value = if name.contains('.') {
                    let parts: Vec<&str> = name.split('.').collect();
                    let (path, var_name) = parts.split_at(parts.len() - 1);
                    ctx.directory
                        .lookup_at_path(path, var_name[0])
                        .ok_or_else(|| format!("Undefined: {}", name))?
                        .clone()
                } else {
                    ctx.lookup(&name)
                        .ok_or_else(|| format!("Undefined: {}", name))?
                        .clone()
                };
                ctx.push(value)?;
                Ok(ExecuteAction::ok())
            }

            cmd::PURGE => {
                // (name --)
                // name can be: string, symbol, or list (path)
                let name_val = ctx.pop()?;

                // Check if it's a list-based path
                if let Some(list) = name_val.as_list() {
                    let elements = extract_path_elements(list).ok_or_else(|| {
                        "PURGE: invalid path list (expected strings or symbols)".to_string()
                    })?;
                    if elements.is_empty() {
                        return Err("PURGE: empty path".into());
                    }

                    // Last element is the variable name
                    let var_name = match elements.last() {
                        Some(PathElement::Name(n)) => n.clone(),
                        _ => return Err("PURGE: path must end with a variable name".into()),
                    };

                    // Navigate to the target directory
                    let saved_path: Vec<String> = ctx.dir_path().to_vec();
                    for elem in &elements[..elements.len() - 1] {
                        match elem {
                            PathElement::Home => ctx.home(),
                            PathElement::UpDir => ctx.updir(),
                            PathElement::Name(dir) => {
                                if !ctx.enter_subdir(dir) {
                                    // Restore path and error
                                    ctx.home();
                                    for p in &saved_path {
                                        ctx.enter_subdir(p);
                                    }
                                    return Err(format!("Undefined: directory '{}'", dir));
                                }
                            }
                        }
                    }

                    // Purge the variable
                    ctx.purge(&var_name)
                        .ok_or_else(|| format!("Undefined: {}", var_name))?;

                    // Restore original directory
                    ctx.home();
                    for p in &saved_path {
                        ctx.enter_subdir(p);
                    }

                    return Ok(ExecuteAction::ok());
                }

                // Must be symbolic (like 'x' or 'dir.subdir.var')
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!(
                        "PURGE: expected symbolic name 'x' or list path, got {}",
                        name_val.type_name()
                    )
                })?;

                // Check for path-based access (e.g., 'entities.0.x')
                if name.contains('.') {
                    let parts: Vec<&str> = name.split('.').collect();
                    let (path, var_name) = parts.split_at(parts.len() - 1);
                    ctx.directory
                        .purge_at_path(path, var_name[0])
                        .ok_or_else(|| format!("Undefined: {}", name))?;
                } else {
                    ctx.purge(&name)
                        .ok_or_else(|| format!("Undefined: {}", name))?;
                }
                Ok(ExecuteAction::ok())
            }

            cmd::VARS => {
                // (-- {names})
                let names: Vec<Value> = ctx.vars().map(|s| Value::string(s.as_str())).collect();
                ctx.push(Value::list(names))?;
                Ok(ExecuteAction::ok())
            }

            cmd::CLVAR => {
                // (--)
                ctx.clear_vars();
                Ok(ExecuteAction::ok())
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
                        ));
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
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
                        ));
                    }
                };

                let result = new_val.clone();
                ctx.store(name, new_val);
                ctx.push(result)?;
                Ok(ExecuteAction::ok())
            }

            cmd::RENAME => {
                // (old_name new_name --)
                let new_name_val = ctx.pop()?;
                let new_name = extract_name(&new_name_val).ok_or_else(|| {
                    format!(
                        "RENAME: expected string name, got {}",
                        new_name_val.type_name()
                    )
                })?;
                let old_name_val = ctx.pop()?;
                let old_name = extract_name(&old_name_val).ok_or_else(|| {
                    format!(
                        "RENAME: expected string name, got {}",
                        old_name_val.type_name()
                    )
                })?;
                if !ctx.rename_var(&old_name, &new_name) {
                    return Err(format!("Undefined: {}", old_name));
                }
                Ok(ExecuteAction::ok())
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
                Ok(ExecuteAction::ok())
            }

            cmd::PGDIR => {
                // (name --)
                let name_val = ctx.pop()?;
                let name = extract_name(&name_val).ok_or_else(|| {
                    format!("PGDIR: expected string name, got {}", name_val.type_name())
                })?;
                ctx.remove_subdir(&name)
                    .map_err(|e| format!("PGDIR: {}", e))?;
                Ok(ExecuteAction::ok())
            }

            cmd::UPDIR => {
                // (--)
                ctx.updir();
                Ok(ExecuteAction::ok())
            }

            cmd::HOME => {
                // (--)
                ctx.home();
                Ok(ExecuteAction::ok())
            }

            cmd::PATH => {
                // (-- {path})
                let path: Vec<Value> = ctx
                    .dir_path()
                    .iter()
                    .map(|s| Value::string(s.as_str()))
                    .collect();
                ctx.push(Value::list(path))?;
                Ok(ExecuteAction::ok())
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
                    let subdir = ctx
                        .directory
                        .get_subdir(&name)
                        .ok_or_else(|| format!("PACKDIR: directory '{}' not found", name))?;
                    pack_directory(subdir)
                } else {
                    // Pack current directory
                    pack_directory(ctx.directory.current_node())
                };

                ctx.push(Value::bytes(packed))?;
                Ok(ExecuteAction::ok())
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
                            let bytes = packed_val
                                .as_bytes()
                                .ok_or_else(|| {
                                    format!(
                                        "UNPACKDIR: expected bytes, got {}",
                                        packed_val.type_name()
                                    )
                                })?
                                .clone();
                            (bytes, Some(name))
                        } else {
                            // Top is not a name, treat as packed bytes only
                            let packed_val = ctx.pop()?;
                            let bytes = packed_val
                                .as_bytes()
                                .ok_or_else(|| {
                                    format!(
                                        "UNPACKDIR: expected bytes, got {}",
                                        packed_val.type_name()
                                    )
                                })?
                                .clone();
                            (bytes, None)
                        }
                    } else {
                        let packed_val = ctx.pop()?;
                        let bytes = packed_val
                            .as_bytes()
                            .ok_or_else(|| {
                                format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name())
                            })?
                            .clone();
                        (bytes, None)
                    }
                } else {
                    let packed_val = ctx.pop()?;
                    let bytes = packed_val
                        .as_bytes()
                        .ok_or_else(|| {
                            format!("UNPACKDIR: expected bytes, got {}", packed_val.type_name())
                        })?
                        .clone();
                    (bytes, None)
                };

                if let Some(name) = target_subdir {
                    // Create subdirectory if needed and unpack into it
                    ctx.create_subdir(name.clone());
                    let node = ctx.directory.current_node_mut().ensure_subdir(&name);
                    unpack_directory_checked(&packed_bytes, node).map_err(|e| match e {
                        Ok(se) => format!("UNPACKDIR: {}", se),
                        Err(conflict) => conflict.to_string(),
                    })?;
                } else {
                    // Unpack into current directory
                    let node = ctx.directory.current_node_mut();
                    unpack_directory_checked(&packed_bytes, node).map_err(|e| match e {
                        Ok(se) => format!("UNPACKDIR: {}", se),
                        Err(conflict) => conflict.to_string(),
                    })?;
                }

                Ok(ExecuteAction::ok())
            }

            cmd::PACKINFO => {
                // PACKINFO: (packed -- {names})
                let packed_val = ctx.pop()?;
                let bytes = packed_val.as_bytes().ok_or_else(|| {
                    format!("PACKINFO: expected bytes, got {}", packed_val.type_name())
                })?;

                let names = packinfo(bytes).map_err(|e| format!("PACKINFO: {}", e))?;

                let names_list: Vec<Value> = names
                    .into_iter()
                    .map(|s| Value::string(s.as_str()))
                    .collect();
                ctx.push(Value::list(names_list))?;
                Ok(ExecuteAction::ok())
            }

            _ => Err(format!("Unknown directory command: {}", ctx.cmd)),
        }
    }
}

/// Extract a variable name from a symbolic value.
///
/// Only accepts quoted symbol names (`'x'`), not strings.
/// For path-based access, use `extract_path_string` instead.
fn extract_name(value: &Value) -> Option<String> {
    match value {
        Value::Symbolic(expr) => {
            // If it's just a variable name (like 'x'), extract it
            if let rpl::symbolic::SymExpr::Var(name) = expr.as_ref() {
                Some(name.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Path element from a list-based path.
#[derive(Debug, Clone, PartialEq)]
enum PathElement {
    /// HOME - go to root directory
    Home,
    /// UPDIR - go up one level
    UpDir,
    /// A named directory or variable
    Name(String),
}

/// Extract path elements from a list value.
///
/// Supports newRPL-style paths like `{ HOME dir subdir var }`.
/// Elements can be:
/// - Strings: `"dirname"`
/// - Symbolic names: `'dirname` or just `dirname` (if unquoted symbols work)
/// - Special commands: HOME, UPDIR
fn extract_path_elements(list: &[Value]) -> Option<Vec<PathElement>> {
    let mut elements = Vec::with_capacity(list.len());

    for item in list {
        match item {
            Value::String(s) => {
                let s_upper = s.to_uppercase();
                match s_upper.as_str() {
                    "HOME" => elements.push(PathElement::Home),
                    "UPDIR" => elements.push(PathElement::UpDir),
                    _ => elements.push(PathElement::Name(s.to_string())),
                }
            }
            Value::Symbolic(expr) => {
                if let rpl::symbolic::SymExpr::Var(name) = expr.as_ref() {
                    let name_upper = name.to_uppercase();
                    match name_upper.as_str() {
                        "HOME" => elements.push(PathElement::Home),
                        "UPDIR" => elements.push(PathElement::UpDir),
                        _ => elements.push(PathElement::Name(name.to_string())),
                    }
                } else {
                    return None; // Complex symbolic expression not allowed in path
                }
            }
            _ => return None, // Only strings and symbols allowed
        }
    }

    Some(elements)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directory_lib_id() {
        assert_eq!(interface().id(), 28);
    }

    #[test]
    fn directory_lib_name() {
        assert_eq!(interface().name(), "Directory");
    }

    #[test]
    fn commands_registered() {
        let cmds = interface().to_command_infos();
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

    #[test]
    fn path_based_sto_rcl() {
        // Store and recall using symbolic path syntax
        let result = crate::eval(
            r#"
            100 'entities.0.x' STO
            200 'entities.0.y' STO
            'entities.0.x' RCL
            'entities.0.y' RCL
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::integer(100));
        assert_eq!(result[1], Value::integer(200));
    }

    #[test]
    fn path_based_purge() {
        let result = crate::eval(
            r#"
            42 'a.b.c' STO
            'a.b.c' RCL
            'a.b.c' PURGE
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::integer(42));
    }

    #[test]
    fn path_based_nested() {
        // Store at different depths
        let result = crate::eval(
            r#"
            1 'a.x' STO
            2 'a.b.x' STO
            3 'a.b.c.x' STO
            'a.x' RCL
            'a.b.x' RCL
            'a.b.c.x' RCL
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 3);
        assert_eq!(result[0], Value::integer(1));
        assert_eq!(result[1], Value::integer(2));
        assert_eq!(result[2], Value::integer(3));
    }

    #[test]
    fn list_path_sto_rcl() {
        // Store and recall using list-based paths (newRPL style)
        let result = crate::eval(
            r#"
            100 { "entities" "0" "x" } STO
            200 { "entities" "0" "y" } STO
            { "entities" "0" "x" } RCL
            { "entities" "0" "y" } RCL
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::integer(100));
        assert_eq!(result[1], Value::integer(200));
    }

    #[test]
    fn list_path_purge() {
        let result = crate::eval(
            r#"
            42 { "a" "b" "c" } STO
            { "a" "b" "c" } RCL
            { "a" "b" "c" } PURGE
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::integer(42));
    }

    #[test]
    fn list_path_with_home() {
        // HOME in path should navigate to root first
        // Store at nested path, then use HOME to access root from anywhere
        let result = crate::eval(
            r#"
            100 'root_var' STO
            200 { "subdir" "nested_var" } STO
            { "HOME" "root_var" } RCL
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::integer(100));
    }

    #[test]
    fn list_path_with_updir() {
        // UPDIR in path should navigate up one level
        // Store in parent, then access via UPDIR from child context
        let result = crate::eval(
            r#"
            100 { "parent" "x" } STO
            { "parent" "UPDIR" "parent" "x" } RCL
        "#,
        )
        .unwrap();

        // Note: The path is: parent -> UPDIR (back to root) -> parent -> x
        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::integer(100));
    }

    #[test]
    fn symbolic_and_list_path_interop() {
        // Both styles should access the same data
        let result = crate::eval(
            r#"
            42 'a.b.c' STO
            { "a" "b" "c" } RCL
        "#,
        )
        .unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::integer(42));
    }
}
