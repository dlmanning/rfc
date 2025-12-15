//! External library interface system.
//!
//! Allows native libraries to be declared in `.rpli` interface files,
//! enabling tools (LSP, debugger) to understand commands without
//! requiring the actual Rust implementation.
//!
//! ## Interface File Format
//!
//! ```text
//! LIBRARY SR5Graphics 200
//!   EXTERN CLS     ( color -- )        "Clear screen to color"
//!   EXTERN RGB     ( r g b -- color )  "Create RGB555 color"
//!   EXTERN RENDER  ( plot -- )         "Render Plot object"
//! ENDLIBRARY
//! ```
//!
//! ## Usage
//!
//! 1. Tools load `.rpli` files to learn vocabulary
//! 2. Runtime registers native handlers by library ID + command name
//! 3. ExternLib dispatches execution to registered handlers

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, ExecuteContext, ExecuteResult, Library, LibraryId,
    ProbeContext, ProbeResult, StackEffect, TokenDoc,
};

/// A declared external command.
#[derive(Debug, Clone)]
pub struct ExternCommand {
    /// Command name (e.g., "CLS")
    pub name: String,
    /// Stack effect notation (e.g., "( color -- )")
    pub stack: String,
    /// Brief description
    pub doc: String,
    /// Parsed stack effect
    pub effect: StackEffect,
}

/// A declared external library interface.
#[derive(Debug, Clone)]
pub struct LibraryInterface {
    /// Library name (e.g., "SR5Graphics")
    pub name: String,
    /// Library ID
    pub id: u16,
    /// Commands indexed by command ID
    pub commands: Vec<ExternCommand>,
    /// Name to command index map (uppercase)
    pub name_map: HashMap<String, u16>,
}

impl LibraryInterface {
    /// Create a new library interface.
    pub fn new(name: impl Into<String>, id: u16) -> Self {
        Self {
            name: name.into(),
            id,
            commands: Vec::new(),
            name_map: HashMap::new(),
        }
    }

    /// Add a command to the interface.
    pub fn add_command(&mut self, name: impl Into<String>, stack: impl Into<String>, doc: impl Into<String>) {
        let name = name.into();
        let stack = stack.into();
        let effect = parse_stack_effect(&stack);
        let cmd_id = self.commands.len() as u16;

        self.name_map.insert(name.to_ascii_uppercase(), cmd_id);
        self.commands.push(ExternCommand {
            name,
            stack: stack.into(),
            doc: doc.into(),
            effect,
        });
    }

    /// Look up a command by name.
    pub fn lookup(&self, name: &str) -> Option<(u16, &ExternCommand)> {
        let upper = name.to_ascii_uppercase();
        self.name_map.get(&upper).map(|&id| (id, &self.commands[id as usize]))
    }

    /// Get a command by ID.
    pub fn get(&self, cmd_id: u16) -> Option<&ExternCommand> {
        self.commands.get(cmd_id as usize)
    }
}

/// Parse a stack effect string like "( a b -- c )" into StackEffect.
fn parse_stack_effect(s: &str) -> StackEffect {
    let s = s.trim();
    if !s.starts_with('(') || !s.ends_with(')') {
        return StackEffect::Dynamic;
    }

    let inner = &s[1..s.len()-1];
    let parts: Vec<&str> = inner.split("--").collect();
    if parts.len() != 2 {
        return StackEffect::Dynamic;
    }

    let inputs: Vec<&str> = parts[0].split_whitespace().collect();
    let outputs: Vec<&str> = parts[1].split_whitespace().collect();

    StackEffect::Fixed {
        consumes: inputs.len() as u8,
        produces: outputs.len() as u8,
    }
}

/// Native command handler type.
pub type NativeHandler = Box<dyn Fn(&mut ExecuteContext) -> ExecuteResult + Send + Sync>;

/// Registry for native command handlers.
#[derive(Default)]
pub struct NativeHandlerRegistry {
    /// Map from (lib_id, cmd_id) to handler
    handlers: HashMap<(u16, u16), Arc<NativeHandler>>,
}

impl NativeHandlerRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a native handler for a command.
    pub fn register<F>(&mut self, lib_id: u16, cmd_id: u16, handler: F)
    where
        F: Fn(&mut ExecuteContext) -> ExecuteResult + Send + Sync + 'static,
    {
        self.handlers.insert((lib_id, cmd_id), Arc::new(Box::new(handler)));
    }

    /// Get a handler for a command.
    pub fn get(&self, lib_id: u16, cmd_id: u16) -> Option<Arc<NativeHandler>> {
        self.handlers.get(&(lib_id, cmd_id)).cloned()
    }
}

/// External library that uses interface declarations + native handlers.
pub struct ExternLib {
    interface: LibraryInterface,
    handlers: Arc<RwLock<NativeHandlerRegistry>>,
}

impl ExternLib {
    /// Create a new external library from an interface.
    pub fn new(interface: LibraryInterface, handlers: Arc<RwLock<NativeHandlerRegistry>>) -> Self {
        Self { interface, handlers }
    }

    /// Create a stub library (no handlers, for tooling only).
    pub fn stub(interface: LibraryInterface) -> Self {
        Self {
            interface,
            handlers: Arc::new(RwLock::new(NativeHandlerRegistry::new())),
        }
    }

    /// Get the interface.
    pub fn interface(&self) -> &LibraryInterface {
        &self.interface
    }
}

impl Library for ExternLib {
    fn id(&self) -> LibraryId {
        LibraryId::new(self.interface.id)
    }

    fn name(&self) -> &'static str {
        // Leak the name string to get a 'static lifetime
        // This is acceptable since libraries live for the program duration
        Box::leak(self.interface.name.clone().into_boxed_str())
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();
        if self.interface.lookup(text).is_some() {
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
        if let Some((cmd_id, _)) = self.interface.lookup(text) {
            ctx.emit_opcode(self.interface.id, cmd_id);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let cmd_id = ctx.cmd();

        // Try to get a registered handler
        if let Ok(handlers) = self.handlers.read() {
            if let Some(handler) = handlers.get(self.interface.id, cmd_id) {
                return handler(ctx);
            }
        }

        // No handler registered - return error
        let cmd_name = self.interface.get(cmd_id)
            .map(|c| c.name.as_str())
            .unwrap_or("unknown");
        Err(format!(
            "{}: no native handler registered for {}.{}",
            cmd_name, self.interface.name, cmd_name
        ))
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        if let Some((_, cmd)) = self.interface.lookup(token) {
            cmd.effect.clone()
        } else {
            StackEffect::Dynamic
        }
    }

    fn tokens(&self) -> &'static [TokenDoc] {
        // Convert interface commands to TokenDoc
        // Leak to get 'static lifetime (acceptable for long-lived libraries)
        let docs: Vec<TokenDoc> = self.interface.commands.iter().map(|cmd| {
            TokenDoc {
                name: Box::leak(cmd.name.clone().into_boxed_str()),
                brief: Box::leak(cmd.doc.clone().into_boxed_str()),
                stack: Box::leak(cmd.stack.clone().into_boxed_str()),
                example: "",
                see_also: &[],
            }
        }).collect();

        Box::leak(docs.into_boxed_slice())
    }
}

/// Parse a `.rpli` interface file and return library interfaces.
pub fn parse_interface_file(source: &str) -> Result<Vec<LibraryInterface>, String> {
    let mut libraries = Vec::new();
    let mut current_lib: Option<LibraryInterface> = None;

    for (line_num, line) in source.lines().enumerate() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('@') || line.starts_with('#') {
            continue;
        }

        let tokens: Vec<&str> = line.split_whitespace().collect();
        if tokens.is_empty() {
            continue;
        }

        match tokens[0].to_ascii_uppercase().as_str() {
            "LIBRARY" => {
                if current_lib.is_some() {
                    return Err(format!("Line {}: nested LIBRARY not allowed", line_num + 1));
                }
                if tokens.len() < 3 {
                    return Err(format!("Line {}: LIBRARY requires name and id", line_num + 1));
                }
                let name = tokens[1];
                let id: u16 = tokens[2].parse()
                    .map_err(|_| format!("Line {}: invalid library ID", line_num + 1))?;
                current_lib = Some(LibraryInterface::new(name, id));
            }
            "ENDLIBRARY" => {
                if let Some(lib) = current_lib.take() {
                    libraries.push(lib);
                } else {
                    return Err(format!("Line {}: ENDLIBRARY without LIBRARY", line_num + 1));
                }
            }
            "EXTERN" => {
                let lib = current_lib.as_mut()
                    .ok_or_else(|| format!("Line {}: EXTERN outside of LIBRARY block", line_num + 1))?;

                if tokens.len() < 2 {
                    return Err(format!("Line {}: EXTERN requires command name", line_num + 1));
                }

                let cmd_name = tokens[1];

                // Parse stack effect: everything between ( and )
                let stack = extract_parens(line).unwrap_or_default();

                // Parse doc string: everything between quotes
                let doc = extract_quoted(line).unwrap_or_default();

                lib.add_command(cmd_name, stack, doc);
            }
            _ => {
                // Unknown token - could be an error or just ignored
            }
        }
    }

    if current_lib.is_some() {
        return Err("Unclosed LIBRARY block".to_string());
    }

    Ok(libraries)
}

/// Extract content between parentheses.
fn extract_parens(s: &str) -> Option<String> {
    let start = s.find('(')?;
    let end = s.find(')')?;
    if end > start {
        Some(s[start..=end].to_string())
    } else {
        None
    }
}

/// Extract content between double quotes.
fn extract_quoted(s: &str) -> Option<String> {
    let start = s.find('"')?;
    let end = s[start + 1..].find('"')?;
    Some(s[start + 1..start + 1 + end].to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_stack_effect_simple() {
        let effect = parse_stack_effect("( a b -- c )");
        match effect {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 2);
                assert_eq!(produces, 1);
            }
            _ => panic!("Expected Fixed"),
        }
    }

    #[test]
    fn parse_stack_effect_none() {
        let effect = parse_stack_effect("( -- )");
        match effect {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 0);
                assert_eq!(produces, 0);
            }
            _ => panic!("Expected Fixed"),
        }
    }

    #[test]
    fn parse_interface_file_simple() {
        let source = r#"
            @ SR5 Graphics Library
            LIBRARY SR5Graphics 200
              EXTERN CLS     ( color -- )        "Clear screen to color"
              EXTERN RGB     ( r g b -- color )  "Create RGB555 color"
              EXTERN RENDER  ( plot -- )         "Render Plot object"
            ENDLIBRARY
        "#;

        let libs = parse_interface_file(source).unwrap();
        assert_eq!(libs.len(), 1);

        let lib = &libs[0];
        assert_eq!(lib.name, "SR5Graphics");
        assert_eq!(lib.id, 200);
        assert_eq!(lib.commands.len(), 3);

        assert_eq!(lib.commands[0].name, "CLS");
        assert_eq!(lib.commands[1].name, "RGB");
        assert_eq!(lib.commands[2].name, "RENDER");

        // Test lookup
        let (cmd_id, cmd) = lib.lookup("cls").unwrap();
        assert_eq!(cmd_id, 0);
        assert_eq!(cmd.name, "CLS");
        assert_eq!(cmd.doc, "Clear screen to color");
    }

    #[test]
    fn parse_interface_file_multiple_libs() {
        let source = r#"
            LIBRARY SR5Graphics 200
              EXTERN CLS ( color -- ) "Clear screen"
            ENDLIBRARY

            LIBRARY SR5Input 201
              EXTERN BTNS ( -- state ) "Get button state"
            ENDLIBRARY
        "#;

        let libs = parse_interface_file(source).unwrap();
        assert_eq!(libs.len(), 2);
        assert_eq!(libs[0].name, "SR5Graphics");
        assert_eq!(libs[1].name, "SR5Input");
    }

    #[test]
    fn extern_lib_probe_and_compile() {
        let mut interface = LibraryInterface::new("Test", 100);
        interface.add_command("FOO", "( -- )", "Test command");
        interface.add_command("BAR", "( a -- b )", "Another command");

        let lib = ExternLib::stub(interface);

        // Test that commands are found
        assert!(lib.interface.lookup("FOO").is_some());
        assert!(lib.interface.lookup("foo").is_some()); // case insensitive
        assert!(lib.interface.lookup("BAR").is_some());
        assert!(lib.interface.lookup("BAZ").is_none());
    }
}
