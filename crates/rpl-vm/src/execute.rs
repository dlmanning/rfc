//! VM execution loop.
//!
//! This module provides the core execution loop that is generic over
//! the CommandDispatch trait, allowing the VM to be used with different
//! library implementations.

use std::sync::Arc;

use rpl_core::{Interner, Span, TypeId, Word, extract_size, extract_type, is_prolog};

use crate::debug::{DebugEvent, DebugState};
use crate::dispatch::{CommandDispatch, DispatchResult};
use crate::error::RuntimeError;
use crate::machine::{ReturnEntry, VM};
use crate::value::{ProgramDebugInfo, Value};

/// Outcome of program execution.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExecuteOutcome {
    /// Execution completed normally.
    Completed,
    /// Execution paused due to debug event.
    Debug(DebugEvent),
}

/// Execute bytecode using the provided dispatcher.
///
/// This is the main entry point for executing bytecode. It takes a dispatcher
/// that routes commands to library implementations.
///
/// # Arguments
/// * `vm` - The VM state
/// * `code` - The bytecode to execute
/// * `dispatcher` - Implementation of CommandDispatch for routing commands
/// * `debug` - Optional debug state for breakpoints and stepping
/// * `interner` - The interner containing symbol definitions
///
/// # Returns
/// * `Ok(ExecuteOutcome::Completed)` - Execution finished normally
/// * `Ok(ExecuteOutcome::Debug(event))` - Execution paused for debugging
/// * `Err(RuntimeError)` - A runtime error occurred
pub fn execute<D: CommandDispatch>(
    vm: &mut VM,
    code: &[Word],
    dispatcher: &D,
    debug: Option<&mut DebugState>,
    interner: Interner,
) -> Result<ExecuteOutcome, RuntimeError> {
    execute_with_source(vm, code, dispatcher, debug, interner, None, None, None)
}

/// Execute bytecode with source information for debugging.
///
/// Like `execute`, but accepts source spans and code for extracting debug info
/// when program prologs are executed.
#[allow(clippy::too_many_arguments)]
pub fn execute_with_source<D: CommandDispatch>(
    vm: &mut VM,
    code: &[Word],
    dispatcher: &D,
    debug: Option<&mut DebugState>,
    interner: Interner,
    spans: Option<&[Span]>,
    source: Option<&str>,
    source_name: Option<&str>,
) -> Result<ExecuteOutcome, RuntimeError> {
    // Transfer the interner to the VM
    vm.set_interner(interner);

    // Only set current code if this is a fresh start (no existing code or past end).
    // If we're resuming after a debug pause inside a called function, we DON'T want
    // to overwrite the current code - we want to continue where we left off.
    let is_fresh_start = vm.current_code_len() == 0 || vm.pc >= vm.current_code_len();
    if is_fresh_start {
        let arc_code: Arc<[Word]> = Arc::from(code);
        vm.set_current_code(arc_code);
        vm.pc = 0;

        // Set debug info if source is provided (only on fresh start)
        if let (Some(spans), Some(src), Some(name)) = (spans, source, source_name) {
            vm.set_debug_info(spans.to_vec(), src.to_string(), name.to_string());
        }
    }

    execute_loop(vm, dispatcher, debug)
}

/// Internal execution loop using iterative call frames instead of recursion.
///
/// This allows debug state to work correctly across function calls, since
/// the debug check happens at every instruction regardless of call depth.
fn execute_loop<D: CommandDispatch>(
    vm: &mut VM,
    dispatcher: &D,
    mut debug: Option<&mut DebugState>,
) -> Result<ExecuteOutcome, RuntimeError> {
    loop {
        // Check if we've reached the end of current code
        if vm.pc >= vm.current_code_len() {
            // Try to pop a call frame and return to caller
            match vm.try_pop_call() {
                Ok(Some((caller_code, return_pc, _name, _debug_info))) => {
                    // Return to caller
                    vm.set_current_code(caller_code);
                    vm.pc = return_pc;
                    continue;
                }
                Ok(None) => {
                    // No more call frames - execution complete
                    return Ok(ExecuteOutcome::Completed);
                }
                Err(e) => {
                    return Err(RuntimeError::Stack(e));
                }
            }
        }

        // Check debug state before each instruction.
        // Get current source offset for source-based breakpoints (works at all call depths).
        let source_offset = get_current_source_offset(vm);

        if let Some(ref mut dbg) = debug
            && let Some(event) = dbg.check_with_source(vm.pc, vm.call_depth(), source_offset)
        {
            return Ok(ExecuteOutcome::Debug(event));
        }

        // Get the current code (we need to clone the Arc to avoid borrowing issues)
        let code = vm.current_code();
        let word = code[vm.pc];

        if is_prolog(word) {
            let type_id = extract_type(word);
            let size = extract_size(word) as usize;

            // Check if the dispatcher wants to handle this prolog
            if let Some(result) = dispatcher.handle_prolog(vm, type_id, size, &code, vm.pc) {
                vm.pc = result?;
            } else {
                // Default prolog handling
                execute_prolog(vm, word, &code)?;
            }
        } else {
            // Dispatch the command
            let (operands_consumed, result) = dispatcher.dispatch(vm, word, &code, vm.pc)?;

            match result {
                DispatchResult::Ok => {
                    vm.pc += 1 + operands_consumed;
                }
                DispatchResult::Jump(addr) => {
                    vm.pc = addr;
                }
                DispatchResult::Return => {
                    // Force return from current call frame
                    // Set PC past end to trigger call frame pop in main loop
                    vm.pc = vm.current_code_len();
                }
                DispatchResult::Halt => {
                    // Set PC past end to stop execution
                    vm.pc = usize::MAX;
                }
                DispatchResult::EvalProgram { code: program_code, name, debug_info } => {
                    // Mark that we executed the call instruction (for stepping)
                    // This must happen BEFORE we push the call frame so that
                    // the debug check at the start of the called function can trigger
                    if let Some(ref mut dbg) = debug {
                        dbg.mark_executed();
                    }

                    // Advance PC first (so we return to the next instruction)
                    vm.pc += 1 + operands_consumed;

                    // Push call frame to return here after program completes
                    let return_pc = vm.pc;
                    let caller_code = vm.current_code();
                    vm.push_call(caller_code, return_pc, name, debug_info)?;

                    // Switch to the new program
                    let new_code: Arc<[Word]> = Arc::from(program_code.as_slice());
                    vm.set_current_code(new_code);
                    vm.pc = 0;
                    continue;
                }
                DispatchResult::Error(msg) => {
                    return Err(RuntimeError::LibraryError(msg));
                }
            }
        }

        // Mark that we executed an instruction (for stepping)
        if let Some(ref mut dbg) = debug {
            dbg.mark_executed();
        }
    }
}

/// Get the current source byte offset for the current instruction.
///
/// Only returns a source offset when inside a called function (call_depth > 0),
/// because source-based breakpoints are designed for debugging inside called functions.
/// In the main program (call_depth == 0), PC-based breakpoints are used instead
/// since they are more accurate.
fn get_current_source_offset(vm: &VM) -> Option<u32> {
    let pc = vm.pc;
    let call_depth = vm.call_depth();

    if call_depth == 0 {
        // Main program - don't use source breakpoints, use PC breakpoints instead
        None
    } else {
        // Inside called function - get debug info from call frame
        let return_stack = vm.return_stack_snapshot();
        for entry in return_stack.iter().rev() {
            if let ReturnEntry::Call { debug_info, .. } = entry {
                if let Some(dbg) = debug_info
                    && let Some(span) = dbg.spans.get(pc)
                {
                    return Some(span.start().offset());
                }
                // No debug info for this call - can't get source offset
                return None;
            }
        }
        None
    }
}

/// Execute a prolog (object literal) - default handler.
fn execute_prolog(vm: &mut VM, word: Word, code: &[Word]) -> Result<(), RuntimeError> {
    let type_id = TypeId::new(extract_type(word));
    let size = extract_size(word) as usize;

    // Check we have enough words
    if vm.pc + 1 + size > code.len() {
        return Err(RuntimeError::InvalidBytecode(
            "Prolog extends past end of code".to_string(),
        ));
    }

    // Handle well-known types
    let value = match type_id {
        TypeId::COMMENT => {
            // Comments are no-ops at runtime - just skip over them
            vm.pc += 1 + size;
            return Ok(());
        }
        TypeId::REAL => {
            // Real is stored as 2 words (64-bit float)
            if size != 2 {
                return Err(RuntimeError::InvalidBytecode(format!(
                    "Real should have size 2, got {}",
                    size
                )));
            }
            let hi = code[vm.pc + 1];
            let lo = code[vm.pc + 2];
            let bits = ((hi as u64) << 32) | (lo as u64);
            Value::Real(f64::from_bits(bits))
        }
        TypeId::BINT => {
            // Binary integer stored as 2 words (64-bit)
            if size != 2 {
                return Err(RuntimeError::InvalidBytecode(format!(
                    "Integer should have size 2, got {}",
                    size
                )));
            }
            let hi = code[vm.pc + 1];
            let lo = code[vm.pc + 2];
            let bits = ((hi as u64) << 32) | (lo as u64);
            Value::Int(bits as i64)
        }
        TypeId::STRING => {
            // String: bytes packed into words (little-endian)
            let mut bytes = Vec::with_capacity(size * 4);
            for i in 0..size {
                let w = code[vm.pc + 1 + i];
                bytes.push((w & 0xFF) as u8);
                bytes.push(((w >> 8) & 0xFF) as u8);
                bytes.push(((w >> 16) & 0xFF) as u8);
                bytes.push(((w >> 24) & 0xFF) as u8);
            }
            // Find actual string length (stop at null or padding)
            let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
            let s = String::from_utf8_lossy(&bytes[..len]).into_owned();
            Value::String(s)
        }
        TypeId::LIST => {
            // LIST requires dispatcher for recursive execution
            // If we reach here, store as raw object (will be handled by dispatcher)
            let data: Vec<Word> = code[vm.pc + 1..vm.pc + 1 + size].to_vec();
            Value::Object {
                type_id: TypeId::LIST,
                data,
            }
        }
        TypeId::PROGRAM => {
            // Program: store bytecode using the new Value::Program variant
            // This allows programs to carry optional debug info when available
            let program_code: Vec<Word> = code[vm.pc + 1..vm.pc + 1 + size].to_vec();

            // If debug info is available, extract spans for this program's body
            if vm.has_debug_info() {
                let span_start = vm.pc + 1;
                let span_end = vm.pc + 1 + size;
                if let Some(spans) = vm.get_spans_for_range(span_start, span_end) {
                    let debug_info = ProgramDebugInfo {
                        spans,
                        source: vm.get_main_source().map(|s| s.to_string()),
                        source_name: vm.get_main_source_name().map(|s| s.to_string()),
                    };
                    Value::program_with_debug(program_code, debug_info)
                } else {
                    Value::program(program_code)
                }
            } else {
                Value::program(program_code)
            }
        }
        TypeId::LIBPTR => {
            // LIBPTR requires dispatcher for library resolution
            // If we reach here without dispatcher handling, store as raw object
            let data: Vec<Word> = code[vm.pc + 1..vm.pc + 1 + size].to_vec();
            Value::Object {
                type_id: TypeId::LIBPTR,
                data,
            }
        }
        _ => {
            // Generic object - store type and data
            let data: Vec<Word> = code[vm.pc + 1..vm.pc + 1 + size].to_vec();
            Value::Object { type_id, data }
        }
    };

    vm.push(value)?;
    vm.pc += 1 + size;

    Ok(())
}

/// Execute bytecode inline (synchronously, without call frames).
///
/// This is used for LIST construction where we need to execute the body
/// to collect elements, but can't use call frames because LIST construction
/// needs to complete synchronously.
pub fn execute_inline<D: CommandDispatch>(
    vm: &mut VM,
    code: &[Word],
    dispatcher: &D,
) -> Result<(), RuntimeError> {
    let saved_pc = vm.pc;
    vm.pc = 0;

    while vm.pc < code.len() {
        let word = code[vm.pc];

        if is_prolog(word) {
            let type_id = extract_type(word);
            let size = extract_size(word) as usize;

            // Check if the dispatcher wants to handle this prolog
            if let Some(result) = dispatcher.handle_prolog(vm, type_id, size, code, vm.pc) {
                vm.pc = result?;
            } else {
                execute_prolog(vm, word, code)?;
            }
        } else {
            let (operands_consumed, result) = dispatcher.dispatch(vm, word, code, vm.pc)?;

            match result {
                DispatchResult::Ok => {
                    vm.pc += 1 + operands_consumed;
                }
                DispatchResult::Jump(addr) => {
                    vm.pc = addr;
                }
                DispatchResult::Return => {
                    break;
                }
                DispatchResult::Halt => {
                    break;
                }
                DispatchResult::EvalProgram { code: program_code, .. } => {
                    // Execute synchronously (recursive) for list construction
                    execute_inline(vm, &program_code, dispatcher)?;
                    vm.pc += 1 + operands_consumed;
                }
                DispatchResult::Error(msg) => {
                    return Err(RuntimeError::LibraryError(msg));
                }
            }
        }
    }

    vm.pc = saved_pc;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dispatch::NullDispatcher;
    use rpl_core::make_prolog;

    #[test]
    fn execute_empty_program() {
        let mut vm = VM::new();
        let dispatcher = NullDispatcher;

        let outcome = execute(&mut vm, &[], &dispatcher, None, Interner::new()).unwrap();
        assert_eq!(outcome, ExecuteOutcome::Completed);
        assert_eq!(vm.depth(), 0);
    }

    #[test]
    fn execute_real_literal() {
        let mut vm = VM::new();
        let dispatcher = NullDispatcher;

        // Create bytecode for real number 3.16
        let value = 3.16f64;
        let bits = value.to_bits();
        let hi = (bits >> 32) as u32;
        let lo = bits as u32;

        let code = vec![make_prolog(TypeId::REAL.as_u16(), 2), hi, lo];

        execute(&mut vm, &code, &dispatcher, None, Interner::new()).unwrap();

        assert_eq!(vm.depth(), 1);
        let result = vm.pop_real().unwrap();
        assert!((result - 3.16).abs() < 1e-10);
    }

    #[test]
    fn execute_multiple_literals() {
        let mut vm = VM::new();
        let dispatcher = NullDispatcher;

        // Push 3.0 and 4.0
        let val1 = 3.0f64;
        let bits1 = val1.to_bits();
        let val2 = 4.0f64;
        let bits2 = val2.to_bits();

        let code = vec![
            make_prolog(TypeId::REAL.as_u16(), 2),
            (bits1 >> 32) as u32,
            bits1 as u32,
            make_prolog(TypeId::REAL.as_u16(), 2),
            (bits2 >> 32) as u32,
            bits2 as u32,
        ];

        execute(&mut vm, &code, &dispatcher, None, Interner::new()).unwrap();

        assert_eq!(vm.depth(), 2);
        assert_eq!(vm.pop_real().unwrap(), 4.0);
        assert_eq!(vm.pop_real().unwrap(), 3.0);
    }

    #[test]
    fn execute_prolog_past_end_error() {
        let mut vm = VM::new();
        let dispatcher = NullDispatcher;
        // Prolog claims size 10 but we only have 2 words
        let code = vec![make_prolog(TypeId::REAL.as_u16(), 10), 0];

        let result = execute(&mut vm, &code, &dispatcher, None, Interner::new());
        assert!(matches!(result, Err(RuntimeError::InvalidBytecode(_))));
    }
}
