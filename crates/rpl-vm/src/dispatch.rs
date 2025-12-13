use std::sync::Arc;

use rpl_core::Word;

use crate::error::RuntimeError;
use crate::machine::VM;
use crate::value::ProgramDebugInfo;

/// Result of dispatching a command.
#[derive(Clone, Debug)]
pub enum DispatchResult {
    /// Command completed normally, advance PC by 1 + operands consumed.
    Ok,
    /// Jump to a specific PC.
    Jump(usize),
    /// Return from current call frame.
    Return,
    /// Halt execution completely.
    Halt,
    /// Evaluate a program (push call frame and enter).
    EvalProgram {
        code: Vec<Word>,
        name: Option<String>,
        debug_info: Option<Arc<ProgramDebugInfo>>,
    },
    /// Error occurred during execution.
    Error(String),
}

/// Trait for dispatching commands to libraries.
///
/// This trait is the key abstraction that separates the VM from the library
/// system. The VM calls `dispatch()` for each command word, and the implementor
/// routes it to the appropriate library implementation.
pub trait CommandDispatch {
    /// Dispatch a command to its library implementation.
    ///
    /// # Arguments
    /// * `vm` - The VM state
    /// * `word` - The bytecode word being executed
    /// * `code` - The complete bytecode being executed (for reading operands)
    /// * `pc` - Current program counter
    ///
    /// # Returns
    /// * `Ok((operands_consumed, result))` - Number of operand words consumed and the dispatch result
    /// * `Err(RuntimeError)` - If a fatal error occurred
    fn dispatch(
        &self,
        vm: &mut VM,
        word: Word,
        code: &[Word],
        pc: usize,
    ) -> Result<(usize, DispatchResult), RuntimeError>;

    /// Handle a prolog (object literal) that needs special processing.
    ///
    /// Some prolog types (like LIST or LIBPTR) require access to the dispatcher
    /// to execute nested code or resolve references. This method allows the
    /// dispatcher to handle these cases.
    ///
    /// # Arguments
    /// * `vm` - The VM state
    /// * `type_id` - The type ID from the prolog word
    /// * `size` - The size field from the prolog word
    /// * `code` - The complete bytecode being executed
    /// * `pc` - Current program counter (pointing at the prolog word)
    ///
    /// # Returns
    /// * `Some(Ok(new_pc))` - If the dispatcher handled the prolog, returns the new PC
    /// * `Some(Err(e))` - If there was an error handling the prolog
    /// * `None` - If this prolog type should be handled by the default VM logic
    fn handle_prolog(
        &self,
        _vm: &mut VM,
        _type_id: u16,
        _size: usize,
        _code: &[Word],
        _pc: usize,
    ) -> Option<Result<usize, RuntimeError>> {
        None // Default: let VM handle it
    }
}

/// A null dispatcher that always returns an error.
/// Useful for testing or when no libraries are registered.
pub struct NullDispatcher;

impl CommandDispatch for NullDispatcher {
    fn dispatch(
        &self,
        _vm: &mut VM,
        word: Word,
        _code: &[Word],
        _pc: usize,
    ) -> Result<(usize, DispatchResult), RuntimeError> {
        let lib_id = rpl_core::extract_lib(word);
        Err(RuntimeError::UnknownLibrary(lib_id))
    }
}
