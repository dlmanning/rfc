//! Library registry for managing commands and token claims.
//!
//! The registry maintains:
//! - Token claims for parsing (libraries can claim keywords like IF, FOR)
//! - Command name → (lib, cmd) mapping
//! - Library instances for parsing, lowering, execution, and analysis
//!
//! # Example
//!
//! ```ignore
//! let mut registry = Registry::new();
//! registry.add(StackLib);
//! registry.add(ArithLib);
//! ```

use std::collections::HashMap;

// Re-export for convenience
pub use crate::libs::{ClaimContext, TokenClaim};
use crate::{
    ir::LibId,
    libs::{
        ArithLib, BinaryLib, CommentsLib, DirectoryLib, Library, ListLib, ProgLib, StackEffect,
        StackLib, StringsLib, SymbolicLib, TranscendentalsLib,
        flow::FlowLib,
        locals::LocalsLib,
        userlib::UserLibLib,
    },
    types::CStack,
};

/// Reference to a command in the registry.
pub struct CommandRef {
    /// Command name.
    pub name: String,
    /// Library ID.
    pub lib: LibId,
    /// Command ID.
    pub cmd: u16,
}

/// Registry of libraries, commands, and token claims.
pub struct Registry {
    /// Token claims sorted by priority (highest first).
    claims: Vec<TokenClaim>,
    /// Command name → (lib_id, cmd_id) mapping.
    commands: HashMap<String, (LibId, u16)>,
    /// Command (lib_id, cmd_id) → static StackEffect mapping.
    command_effects: HashMap<(LibId, u16), StackEffect>,
    /// Registered libraries by ID.
    libraries: HashMap<LibId, Box<dyn Library>>,
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}

impl Registry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            claims: Vec::new(),
            commands: HashMap::new(),
            command_effects: HashMap::new(),
            libraries: HashMap::new(),
        }
    }

    /// Register a library.
    ///
    /// Automatically registers all commands and token claims from the library.
    pub fn add<T: Library + 'static>(&mut self, lib: T) {
        let id = lib.id();

        // Register commands
        for cmd in lib.commands() {
            self.commands.insert(cmd.name.to_string(), (cmd.lib_id, cmd.cmd_id));
            self.command_effects.insert((cmd.lib_id, cmd.cmd_id), cmd.effect);
        }

        // Register token claims
        for claim in lib.claims() {
            self.register_claim(claim.clone());
        }

        // Store the library
        self.libraries.insert(id, Box::new(lib));
    }

    /// Get a library by ID.
    pub fn get(&self, lib_id: LibId) -> Option<&dyn Library> {
        self.libraries.get(&lib_id).map(|b| b.as_ref())
    }

    /// Get a library name by ID.
    pub fn get_library_name(&self, lib_id: LibId) -> Option<&'static str> {
        self.libraries.get(&lib_id).map(|lib| lib.name())
    }

    /// Register a token claim.
    pub fn register_claim(&mut self, claim: TokenClaim) {
        self.claims.push(claim);
        // Keep sorted by priority (highest first)
        self.claims.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Register a command with its name and IDs.
    pub fn register_command(&mut self, name: impl Into<String>, lib_id: LibId, cmd_id: u16) {
        self.commands.insert(name.into(), (lib_id, cmd_id));
    }

    /// Get the stack effect for a command given the current type stack.
    pub fn get_command_effect(&self, lib_id: LibId, cmd_id: u16, types: &CStack) -> StackEffect {
        // First try the library's analyzer
        if let Some(lib) = self.libraries.get(&lib_id) {
            let effect = lib.command_effect(cmd_id, types);
            if !matches!(effect, StackEffect::Dynamic) {
                return effect;
            }
        }

        // Fall back to static effect
        self.command_effects
            .get(&(lib_id, cmd_id))
            .cloned()
            .unwrap_or(StackEffect::Dynamic)
    }

    /// Find a claim for a token in the given context.
    pub fn find_claim(&self, token: &str, context: ClaimContext) -> Option<&TokenClaim> {
        let token_upper = token.to_uppercase();

        for claim in &self.claims {
            let claim_matches = claim.token.eq_ignore_ascii_case(token)
                || claim.token.to_uppercase() == token_upper;

            if !claim_matches {
                continue;
            }

            match (claim.context, context) {
                (ClaimContext::Any, _) => return Some(claim),
                (ClaimContext::NotInfix, ClaimContext::NotInfix) => return Some(claim),
                (ClaimContext::NotInfix, ClaimContext::Any) => return Some(claim),
                (ClaimContext::InfixOnly, ClaimContext::InfixOnly) => return Some(claim),
                (ClaimContext::InfixOnly, ClaimContext::Any) => return Some(claim),
                _ => continue,
            }
        }

        None
    }

    /// Find a command by name.
    pub fn find_command(&self, name: &str) -> Option<(LibId, u16)> {
        // Try exact match first
        if let Some(&result) = self.commands.get(name) {
            return Some(result);
        }

        // Try case-insensitive match
        let name_upper = name.to_uppercase();
        for (cmd_name, &result) in &self.commands {
            if cmd_name.to_uppercase() == name_upper {
                return Some(result);
            }
        }

        None
    }

    /// Get all registered commands.
    pub fn all_commands(&self) -> impl Iterator<Item = CommandRef> + '_ {
        self.commands.iter().map(|(name, &(lib, cmd))| CommandRef {
            name: name.clone(),
            lib,
            cmd,
        })
    }

    /// Create a registry with standard libraries.
    pub fn with_core() -> Self {
        let mut reg = Self::new();

        reg.add(StackLib);
        reg.add(ArithLib);
        reg.add(SymbolicLib);
        reg.add(ListLib);
        reg.add(TranscendentalsLib);
        reg.add(StringsLib);
        reg.add(CommentsLib);
        reg.add(DirectoryLib);
        reg.add(UserLibLib);
        reg.add(BinaryLib);
        reg.add(FlowLib);
        reg.add(ProgLib);
        reg.add(LocalsLib);

        reg
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::{arith, stack, ARITH_LIB, STACK_LIB, flow::FLOW_LIB};

    #[test]
    fn find_command() {
        let reg = Registry::with_core();

        assert_eq!(reg.find_command("+"), Some((ARITH_LIB, arith::cmd::ADD)));
        assert_eq!(reg.find_command("DUP"), Some((STACK_LIB, stack::cmd::DUP)));
        assert_eq!(reg.find_command("dup"), Some((STACK_LIB, stack::cmd::DUP)));
        assert_eq!(reg.find_command("UNKNOWN"), None);
    }

    #[test]
    fn register_claim() {
        let mut reg = Registry::new();

        reg.register_claim(TokenClaim {
            token: "IF",
            priority: 100,
            context: ClaimContext::NotInfix,
            lib_id: 1,
        });

        let claim = reg.find_claim("IF", ClaimContext::NotInfix);
        assert!(claim.is_some());
        assert_eq!(claim.unwrap().lib_id, 1);
    }

    #[test]
    fn claim_priority() {
        let mut reg = Registry::new();

        reg.register_claim(TokenClaim {
            token: "TEST",
            priority: 50,
            context: ClaimContext::Any,
            lib_id: 1,
        });

        reg.register_claim(TokenClaim {
            token: "TEST",
            priority: 100,
            context: ClaimContext::Any,
            lib_id: 2,
        });

        let claim = reg.find_claim("TEST", ClaimContext::Any);
        assert_eq!(claim.unwrap().lib_id, 2);
    }

    #[test]
    fn core_commands_complete() {
        let reg = Registry::with_core();

        assert!(reg.find_command("DUP").is_some());
        assert!(reg.find_command("DROP").is_some());
        assert!(reg.find_command("+").is_some());
        assert!(reg.find_command("EVAL").is_some());
    }

    #[test]
    fn add_library() {
        let mut reg = Registry::new();
        reg.add(StackLib);

        assert!(reg.get(STACK_LIB).is_some());
        assert_eq!(reg.get_library_name(STACK_LIB), Some("Stack"));
        assert!(reg.find_command("DUP").is_some());
    }

    #[test]
    fn add_with_parser() {
        let mut reg = Registry::new();
        reg.add(FlowLib);

        assert!(reg.get(FLOW_LIB).is_some());
        assert!(reg.find_claim("IF", ClaimContext::Any).is_some());
    }
}
