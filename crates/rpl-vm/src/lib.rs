//! RPL Virtual Machine and bytecode execution.
//!
//! This crate provides the runtime components for executing RPL bytecode:
//!
//! - [`Vm`] - The virtual machine that executes bytecode
//! - [`Value`] - Runtime value representation
//! - [`Opcode`] - Bytecode instruction definitions
//! - [`ExecutorRegistry`] - Library executor registration
//!
//! # Architecture
//!
//! The VM is the authority on the bytecode format. The compiler (in the `rpl` crate)
//! depends on this crate to emit correct bytecode.
//!
//! ```text
//! rpl-vm (this crate)         rpl (compiler)
//! ├── Opcode definitions  <── uses to emit bytecode
//! ├── Value types
//! └── Vm execution
//! ```

mod bytecode;
mod debug;
mod directory;
mod executor;
mod locals;
mod source_map;
mod span;
mod stack;
mod symbolic;
mod value;
mod vm;

// Re-export public types
pub use bytecode::{
    BlockType, CatchKind, Opcode,
    read_f64, read_leb128_i64, read_leb128_u32, read_u16, read_u32,
    write_f64, write_leb128_i64, write_leb128_u32, write_u16, write_u32,
};
pub use debug::{DebugEvent, DebugMode, DebugState};
pub use directory::Directory;
pub use executor::{ExecuteContext, ExecuteResult, ExecutorRegistry, LibraryExecutor, RplException};
pub use locals::{Locals, LocalsError};
pub use source_map::SourceMap;
pub use span::{Pos, Span};
pub use stack::{Stack, StackError};
pub use symbolic::{BinOp, SymExpr, UnaryOp};
pub use value::{LibraryCommand, LibraryData, ProgramData, Value};
pub use vm::{ExecuteOutcome, ReturnEntry, Vm, VmError};

/// Library ID type.
pub type LibId = u16;

/// Program library ID (EVAL command).
pub const PROG_LIB: LibId = 8;

/// Well-known command IDs for PROG_LIB.
pub mod prog_cmd {
    pub const EVAL: u16 = 0;
}
