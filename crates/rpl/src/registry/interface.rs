//! Interface registry for parsing and analysis.
//!
//! The InterfaceRegistry holds library interfaces which provide:
//! - Token claims for custom syntax
//! - Command name → (lib, cmd) mapping
//! - Stack effects for type inference
//! - Binding information for scope analysis

use std::{collections::HashMap, sync::Arc};

use crate::{
    ir::LibId,
    libs::{ClaimContext, LibraryInterface, StackEffect, TokenClaim},
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

/// Registry of library interfaces for parsing and analysis.
///
/// This registry is used during:
/// - **Parsing**: Token claims determine how to parse custom syntax
/// - **Analysis**: Stack effects and binding info for type/scope analysis
pub struct InterfaceRegistry {
    /// Token claims sorted by library ID (highest first), then priority.
    claims: Vec<TokenClaim>,
    /// Command name → (lib_id, cmd_id) mapping.
    commands: HashMap<String, (LibId, u16)>,
    /// Command (lib_id, cmd_id) → static StackEffect mapping.
    command_effects: HashMap<(LibId, u16), StackEffect>,
    /// Library interfaces by ID.
    interfaces: HashMap<LibId, Arc<dyn LibraryInterface>>,
}

impl Default for InterfaceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl InterfaceRegistry {
    /// Create a new empty interface registry.
    pub fn new() -> Self {
        Self {
            claims: Vec::new(),
            commands: HashMap::new(),
            command_effects: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }

    /// Register a library interface.
    pub fn add<T: LibraryInterface + 'static>(&mut self, lib: T) {
        let id = lib.id();

        // Register commands
        for cmd in lib.commands() {
            self.commands
                .insert(cmd.name.to_string(), (cmd.lib_id, cmd.cmd_id));
            self.command_effects
                .insert((cmd.lib_id, cmd.cmd_id), cmd.effect);
        }

        // Register token claims
        for claim in lib.claims() {
            self.register_claim(claim.clone());
        }

        // Store interface
        self.interfaces.insert(id, Arc::new(lib));
    }

    /// Register a library interface from an Arc (for shared interfaces).
    pub fn add_arc(&mut self, lib: Arc<dyn LibraryInterface>) {
        let id = lib.id();

        // Register commands
        for cmd in lib.commands() {
            self.commands
                .insert(cmd.name.to_string(), (cmd.lib_id, cmd.cmd_id));
            self.command_effects
                .insert((cmd.lib_id, cmd.cmd_id), cmd.effect);
        }

        // Register token claims
        for claim in lib.claims() {
            self.register_claim(claim.clone());
        }

        // Store interface
        self.interfaces.insert(id, lib);
    }

    /// Get a library interface by ID.
    pub fn get(&self, lib_id: LibId) -> Option<&dyn LibraryInterface> {
        self.interfaces.get(&lib_id).map(|a| a.as_ref())
    }

    /// Get a library name by ID.
    pub fn get_library_name(&self, lib_id: LibId) -> Option<&'static str> {
        self.interfaces.get(&lib_id).map(|lib| lib.name())
    }

    /// Register a token claim.
    pub fn register_claim(&mut self, claim: TokenClaim) {
        self.claims.push(claim);
        // Keep sorted by library ID (highest first), then by priority
        // This allows user libraries to override built-in tokens
        self.claims.sort_by(|a, b| {
            b.lib_id
                .cmp(&a.lib_id)
                .then_with(|| b.priority.cmp(&a.priority))
        });
    }

    /// Register a command with its name and IDs.
    pub fn register_command(&mut self, name: impl Into<String>, lib_id: LibId, cmd_id: u16) {
        self.commands.insert(name.into(), (lib_id, cmd_id));
    }

    /// Get the static (declared) stack effect for a command.
    ///
    /// Returns the effect declared in the command definition. For dynamic
    /// effects based on input types, query the LibraryInterface directly.
    pub fn get_static_effect(&self, lib_id: LibId, cmd_id: u16) -> StackEffect {
        self.command_effects
            .get(&(lib_id, cmd_id))
            .cloned()
            .unwrap_or(StackEffect::Dynamic)
    }

    /// Get the name of a command by its library and command ID.
    pub fn get_command_name(&self, lib_id: LibId, cmd_id: u16) -> &str {
        // Search the commands map for a match
        for (name, &(lib, cmd)) in &self.commands {
            if lib == lib_id && cmd == cmd_id {
                return name;
            }
        }
        "unknown"
    }

    /// Find a claim for a token in the given context (case-sensitive).
    pub fn find_claim(&self, token: &str, context: ClaimContext) -> Option<&TokenClaim> {
        for claim in &self.claims {
            if claim.token != token {
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

    /// Find a command by name (case-sensitive).
    pub fn find_command(&self, name: &str) -> Option<(LibId, u16)> {
        self.commands.get(name).copied()
    }

    /// Get all registered commands.
    pub fn all_commands(&self) -> impl Iterator<Item = CommandRef> + '_ {
        self.commands.iter().map(|(name, &(lib, cmd))| CommandRef {
            name: name.clone(),
            lib,
            cmd,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_claim() {
        let mut reg = InterfaceRegistry::new();

        reg.register_claim(TokenClaim {
            token: "IF".to_string(),
            priority: 100,
            context: ClaimContext::NotInfix,
            lib_id: 1,
        });

        let claim = reg.find_claim("IF", ClaimContext::NotInfix);
        assert!(claim.is_some());
        assert_eq!(claim.unwrap().lib_id, 1);
    }

    #[test]
    fn claim_library_order() {
        let mut reg = InterfaceRegistry::new();

        // Lower library ID
        reg.register_claim(TokenClaim {
            token: "TEST".to_string(),
            priority: 100, // Higher priority but lower lib_id
            context: ClaimContext::Any,
            lib_id: 1,
        });

        // Higher library ID wins
        reg.register_claim(TokenClaim {
            token: "TEST".to_string(),
            priority: 50, // Lower priority but higher lib_id
            context: ClaimContext::Any,
            lib_id: 2,
        });

        let claim = reg.find_claim("TEST", ClaimContext::Any);
        // Higher lib_id wins regardless of priority
        assert_eq!(claim.unwrap().lib_id, 2);
    }

    #[test]
    fn claim_priority_within_library() {
        let mut reg = InterfaceRegistry::new();

        // Same library, different priorities
        reg.register_claim(TokenClaim {
            token: "TEST".to_string(),
            priority: 50,
            context: ClaimContext::Any,
            lib_id: 1,
        });

        reg.register_claim(TokenClaim {
            token: "TEST".to_string(),
            priority: 100,
            context: ClaimContext::Any,
            lib_id: 1,
        });

        let claim = reg.find_claim("TEST", ClaimContext::Any);
        // Higher priority wins within same library
        assert_eq!(claim.unwrap().priority, 100);
    }

    #[test]
    fn register_command() {
        let mut reg = InterfaceRegistry::new();
        reg.register_command("TEST", 1, 0);

        assert_eq!(reg.find_command("TEST"), Some((1, 0)));
        // Case-sensitive: lowercase doesn't match
        assert_eq!(reg.find_command("test"), None);
        assert_eq!(reg.find_command("UNKNOWN"), None);
    }
}
