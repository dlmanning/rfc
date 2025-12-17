//! Library registry for managing commands and token claims.
//!
//! The registry maintains:
//! - Token claims for parsing (libraries can claim keywords like IF, FOR)
//! - Command name → (lib, cmd) mapping
//! - Library lowerers for extended composites
//!
//! # Example
//!
//! ```ignore
//! let registry = Registry::with_core();
//! assert_eq!(registry.find_command("DUP"), Some((0, 0)));
//! ```

use std::collections::HashMap;

// Re-export for convenience
pub use crate::libs::{ClaimContext, TokenClaim};
use crate::{
    ir::LibId,
    libs::{
        ArithLib, BinaryLib, CommentsLib, DirectoryLib, Library, LibraryExecutor, LibraryLowerer,
        LibraryParser, ListLib, ProgLib, StackLib, StringsLib, SymbolicLib, TranscendentalsLib,
        flow::FlowLib, locals::LocalsLib, userlib::UserLibLib,
    },
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
    /// Library parsers by ID.
    parsers: HashMap<LibId, Box<dyn LibraryParser>>,
    /// Library lowerers by ID.
    lowerers: HashMap<LibId, Box<dyn LibraryLowerer>>,
    /// Library executors by ID.
    executors: HashMap<LibId, Box<dyn LibraryExecutor>>,
    /// Library names by ID.
    library_names: HashMap<LibId, &'static str>,
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
            parsers: HashMap::new(),
            lowerers: HashMap::new(),
            executors: HashMap::new(),
            library_names: HashMap::new(),
        }
    }

    /// Register a library parser.
    ///
    /// Also registers all token claims from the parser.
    pub fn register_parser(&mut self, parser: Box<dyn LibraryParser>) {
        let id = parser.id();
        // Register claims
        for claim in parser.claims() {
            self.register_claim(claim.clone());
        }
        self.parsers.insert(id, parser);
    }

    /// Get a library parser by ID.
    pub fn get_parser(&self, lib_id: LibId) -> Option<&dyn LibraryParser> {
        self.parsers.get(&lib_id).map(|b| b.as_ref())
    }

    /// Register a library lowerer.
    pub fn register_lowerer(&mut self, lowerer: Box<dyn LibraryLowerer>) {
        let id = lowerer.id();
        let name = lowerer.name();
        self.library_names.insert(id, name);
        self.lowerers.insert(id, lowerer);
    }

    /// Get a library lowerer by ID.
    pub fn get_lowerer(&self, lib_id: LibId) -> Option<&dyn LibraryLowerer> {
        self.lowerers.get(&lib_id).map(|b| b.as_ref())
    }

    /// Register a library executor.
    pub fn register_executor(&mut self, executor: Box<dyn LibraryExecutor>) {
        let id = executor.id();
        self.executors.insert(id, executor);
    }

    /// Get a library executor by ID.
    pub fn get_executor(&self, lib_id: LibId) -> Option<&dyn LibraryExecutor> {
        self.executors.get(&lib_id).map(|b| b.as_ref())
    }

    /// Register a token claim.
    pub fn register_claim(&mut self, claim: TokenClaim) {
        self.claims.push(claim);
        // Keep sorted by priority (highest first)
        self.claims.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Register a command.
    pub fn register_command(&mut self, name: impl Into<String>, lib_id: LibId, cmd_id: u16) {
        self.commands.insert(name.into(), (lib_id, cmd_id));
    }

    /// Find a claim for a token in the given context.
    pub fn find_claim(&self, token: &str, context: ClaimContext) -> Option<&TokenClaim> {
        // Case-insensitive matching for commands
        let token_upper = token.to_uppercase();

        for claim in &self.claims {
            let claim_matches = claim.token.eq_ignore_ascii_case(token)
                || claim.token.to_uppercase() == token_upper;

            if !claim_matches {
                continue;
            }

            // Check context compatibility
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

    /// Get a library name by ID.
    pub fn get_library_name(&self, lib_id: LibId) -> Option<&'static str> {
        self.library_names.get(&lib_id).copied()
    }

    /// Register a library that provides commands, lowering, and execution.
    pub fn register<T>(&mut self, lib: T)
    where
        T: Library + LibraryLowerer + LibraryExecutor + Copy + 'static,
    {
        for cmd in lib.commands() {
            self.register_command(cmd.name, cmd.lib_id, cmd.cmd_id);
        }
        self.register_lowerer(Box::new(lib));
        self.register_executor(Box::new(lib));
    }

    /// Register a library that also provides parsing (for extended syntax like IF/FOR).
    pub fn register_with_parser<T>(&mut self, lib: T)
    where
        T: Library + LibraryParser + LibraryLowerer + LibraryExecutor + Copy + 'static,
    {
        for cmd in lib.commands() {
            self.register_command(cmd.name, cmd.lib_id, cmd.cmd_id);
        }
        self.register_parser(Box::new(lib));
        self.register_lowerer(Box::new(lib));
        self.register_executor(Box::new(lib));
    }

    /// Register a syntax-only library (parser + lowerer, no runtime executor).
    /// Used for compile-time constructs like local variable bindings (→).
    pub fn register_syntax<T>(&mut self, lib: T)
    where
        T: LibraryParser + LibraryLowerer + Copy + 'static,
    {
        self.register_parser(Box::new(lib));
        self.register_lowerer(Box::new(lib));
    }

    /// Create a registry with standard libraries.
    pub fn with_core() -> Self {
        let mut reg = Self::new();

        reg.register(StackLib);
        reg.register(ArithLib);
        reg.register(ProgLib);
        reg.register_with_parser(FlowLib);
        reg.register(SymbolicLib);
        reg.register(ListLib);
        reg.register(TranscendentalsLib);
        reg.register(StringsLib);
        reg.register(CommentsLib);
        reg.register(DirectoryLib);
        reg.register(UserLibLib);
        reg.register(BinaryLib);

        reg.register_syntax(LocalsLib);

        reg
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::libs::{ARITH_LIB, STACK_LIB, arith, stack};

    #[test]
    fn find_command() {
        let reg = Registry::with_core();

        assert_eq!(reg.find_command("+"), Some((ARITH_LIB, arith::cmd::ADD)));
        assert_eq!(reg.find_command("DUP"), Some((STACK_LIB, stack::cmd::DUP)));
        assert_eq!(reg.find_command("dup"), Some((STACK_LIB, stack::cmd::DUP))); // Case insensitive
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

        // Case insensitive
        let claim = reg.find_claim("if", ClaimContext::NotInfix);
        assert!(claim.is_some());
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

        // Higher priority wins
        let claim = reg.find_claim("TEST", ClaimContext::Any);
        assert_eq!(claim.unwrap().lib_id, 2);
    }

    #[test]
    fn core_commands_complete() {
        let reg = Registry::with_core();

        // Stack ops
        assert!(reg.find_command("DUP").is_some());
        assert!(reg.find_command("DROP").is_some());
        assert!(reg.find_command("SWAP").is_some());
        assert!(reg.find_command("ROT").is_some());

        // Arithmetic
        assert!(reg.find_command("+").is_some());
        assert!(reg.find_command("-").is_some());
        assert!(reg.find_command("*").is_some());
        assert!(reg.find_command("/").is_some());

        // Comparison
        assert!(reg.find_command("==").is_some());
        assert!(reg.find_command("<").is_some());
        assert!(reg.find_command(">").is_some());

        // Program
        assert!(reg.find_command("EVAL").is_some());
    }
}
