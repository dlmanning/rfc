use std::collections::HashMap;
use std::sync::Arc;

use rpl_core::{Interner, Span, Word};

use crate::directory::DirectoryTree;
use crate::error::StackError;
use crate::frame::LocalFrame;
use crate::value::{ProgramDebugInfo, Value};

/// Default maximum stack depth.
const DEFAULT_MAX_STACK_DEPTH: usize = 10000;

/// Entry on the unified return stack.
///
/// The return stack holds both program call frames (for EVAL/subroutine calls)
/// and loop state (for FOR/NEXT, START/NEXT, etc.). This matches HP RPL behavior
/// where a single return stack is used for all control flow.
#[derive(Clone, Debug)]
pub enum ReturnEntry {
    /// Program call: stores return context for EVAL/program execution.
    Call {
        /// Bytecode of the caller (to return to).
        code: Arc<[Word]>,
        /// PC to return to in the caller.
        return_pc: usize,
        /// Name of the called function (for stack traces), if known.
        name: Option<String>,
        /// Debug info for the CURRENT function (the one we're calling into).
        /// This is used to map PC to source lines when inside the called function.
        debug_info: Option<Arc<ProgramDebugInfo>>,
    },
    /// FOR/NEXT loop state.
    ForLoop { end: i64, counter: i64 },
    /// START/NEXT loop state (no local variable).
    StartLoop { end: i64, counter: i64 },
    /// Error handler frame (for IFERR/THENERR/ENDERR).
    ErrorHandler {
        /// PC to jump to when an error occurs (start of THENERR block).
        handler_pc: usize,
        /// Bytecode that contains the handler.
        code: Arc<[Word]>,
        /// Stack depth when IFERR was entered (for cleanup).
        stack_depth: usize,
        /// Return stack depth when IFERR was entered (for cleanup).
        return_depth: usize,
    },
}

/// Virtual machine state.
pub struct VM {
    /// Data stack.
    data_stack: Vec<Value>,
    /// Unified return stack for calls and loops.
    return_stack: Vec<ReturnEntry>,
    /// Current bytecode being executed.
    current_code: Arc<[Word]>,
    /// Program counter within current_code.
    pub pc: usize,
    /// Maximum stack depth for data stack.
    max_stack_depth: usize,
    /// Maximum stack depth for return stack.
    max_return_depth: usize,
    /// Hierarchical variable directory tree.
    directories: DirectoryTree,
    /// Local variable frames (stack of frames for nested → bindings).
    local_frames: Vec<LocalFrame>,
    /// Word size for binary integer operations (1-64 bits, default 64).
    word_size: u8,
    /// Interner for symbol resolution (required for local variable lookup).
    interner: Interner,
    /// Source spans for the main program's bytecode (for debug info extraction).
    /// When a program prolog is executed, we extract the relevant spans to
    /// create ProgramDebugInfo for the stored Value::Program.
    main_spans: Option<Arc<[Span]>>,
    /// Source code for the main program (for debug info).
    main_source: Option<String>,
    /// Source file name for the main program (for debug info).
    main_source_name: Option<String>,
}

impl VM {
    /// Create a new VM with empty stacks and default limits.
    pub fn new() -> Self {
        Self::with_limits(DEFAULT_MAX_STACK_DEPTH, DEFAULT_MAX_STACK_DEPTH)
    }

    /// Create a new VM with a specific interner.
    pub fn with_interner(interner: Interner) -> Self {
        Self {
            data_stack: Vec::new(),
            return_stack: Vec::new(),
            current_code: Arc::from([]),
            pc: 0,
            max_stack_depth: DEFAULT_MAX_STACK_DEPTH,
            max_return_depth: DEFAULT_MAX_STACK_DEPTH,
            directories: DirectoryTree::new(),
            local_frames: Vec::new(),
            word_size: 64,
            interner,
            main_spans: None,
            main_source: None,
            main_source_name: None,
        }
    }

    /// Create a new VM with custom stack depth limits.
    pub fn with_limits(max_stack_depth: usize, max_return_depth: usize) -> Self {
        Self {
            data_stack: Vec::new(),
            return_stack: Vec::new(),
            current_code: Arc::from([]),
            pc: 0,
            max_stack_depth,
            max_return_depth,
            directories: DirectoryTree::new(),
            local_frames: Vec::new(),
            word_size: 64,
            interner: Interner::new(),
            main_spans: None,
            main_source: None,
            main_source_name: None,
        }
    }

    /// Get a reference to the interner.
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    /// Get a mutable reference to the interner.
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    /// Set the interner (transfers ownership of symbols from compiled program).
    pub fn set_interner(&mut self, interner: Interner) {
        self.interner = interner;
    }

    /// Set debug info for the main program.
    /// This enables debug info to be extracted when program prologs are executed.
    /// Only call this when debugging is enabled to avoid overhead in production.
    pub fn set_debug_info(&mut self, spans: Vec<Span>, source: String, source_name: String) {
        self.main_spans = Some(Arc::from(spans));
        self.main_source = Some(source);
        self.main_source_name = Some(source_name);
    }

    /// Check if debug info is available.
    pub fn has_debug_info(&self) -> bool {
        self.main_spans.is_some()
    }

    /// Get the spans for a range of bytecode positions (for extracting debug info for programs).
    pub fn get_spans_for_range(&self, start: usize, end: usize) -> Option<Vec<Span>> {
        self.main_spans.as_ref().map(|spans| {
            spans[start..end.min(spans.len())].to_vec()
        })
    }

    /// Get the source code for debug info.
    pub fn get_main_source(&self) -> Option<&str> {
        self.main_source.as_deref()
    }

    /// Get the source file name for debug info.
    pub fn get_main_source_name(&self) -> Option<&str> {
        self.main_source_name.as_deref()
    }

    /// Push a value onto the data stack.
    pub fn push(&mut self, value: Value) -> Result<(), StackError> {
        if self.data_stack.len() >= self.max_stack_depth {
            return Err(StackError::Overflow);
        }
        self.data_stack.push(value);
        Ok(())
    }

    /// Pop a value from the data stack.
    pub fn pop(&mut self) -> Result<Value, StackError> {
        self.data_stack.pop().ok_or(StackError::Underflow)
    }

    /// Peek at a value on the data stack without removing it.
    /// Depth 0 is the top of the stack.
    pub fn peek(&self, depth: usize) -> Result<&Value, StackError> {
        if depth >= self.data_stack.len() {
            return Err(StackError::Underflow);
        }
        let idx = self.data_stack.len() - 1 - depth;
        Ok(&self.data_stack[idx])
    }

    /// Get a mutable reference to a value on the stack.
    pub fn peek_mut(&mut self, depth: usize) -> Result<&mut Value, StackError> {
        if depth >= self.data_stack.len() {
            return Err(StackError::Underflow);
        }
        let idx = self.data_stack.len() - 1 - depth;
        Ok(&mut self.data_stack[idx])
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.data_stack.len()
    }

    /// Clear the data stack.
    pub fn clear(&mut self) {
        self.data_stack.clear();
    }

    /// Reset VM state for a new top-level evaluation.
    ///
    /// This clears the data stack and resets the code/PC state so that
    /// the next execute call will start fresh. This should be called
    /// before each top-level eval to ensure proper isolation between
    /// separate evaluations.
    pub fn reset_for_eval(&mut self) {
        self.data_stack.clear();
        self.current_code = Arc::from([] as [Word; 0]);
        self.pc = 0;
        // Clear return stack in case a previous execution left partial state
        self.return_stack.clear();
        // Clear local frames
        self.local_frames.clear();
    }

    // ========================================================================
    // Current code management
    // ========================================================================

    /// Set the current bytecode being executed.
    pub fn set_current_code(&mut self, code: Arc<[Word]>) {
        self.current_code = code;
    }

    /// Get the current bytecode.
    pub fn current_code(&self) -> Arc<[Word]> {
        Arc::clone(&self.current_code)
    }

    /// Get the length of the current bytecode.
    pub fn current_code_len(&self) -> usize {
        self.current_code.len()
    }

    /// Read a word from the current bytecode at the given position.
    pub fn read_word(&self, pos: usize) -> Result<Word, StackError> {
        self.current_code
            .get(pos)
            .copied()
            .ok_or(StackError::Underflow)
    }

    // ========================================================================
    // Return stack operations (unified for calls and loops)
    // ========================================================================

    /// Push a call frame onto the return stack.
    ///
    /// The `debug_info` is for the function being called INTO (not the caller).
    pub fn push_call(
        &mut self,
        code: Arc<[Word]>,
        return_pc: usize,
        name: Option<String>,
        debug_info: Option<Arc<ProgramDebugInfo>>,
    ) -> Result<(), StackError> {
        if self.return_stack.len() >= self.max_return_depth {
            return Err(StackError::Overflow);
        }
        self.return_stack.push(ReturnEntry::Call {
            code,
            return_pc,
            name,
            debug_info,
        });
        Ok(())
    }

    /// Try to pop a call frame from the return stack.
    /// Returns None if the stack is empty or top is not a Call entry.
    /// Returns (caller_code, return_pc, function_name, function_debug_info).
    #[allow(clippy::type_complexity)]
    pub fn try_pop_call(
        &mut self,
    ) -> Result<Option<(Arc<[Word]>, usize, Option<String>, Option<Arc<ProgramDebugInfo>>)>, StackError>
    {
        match self.return_stack.last() {
            Some(ReturnEntry::Call { .. }) => {
                if let Some(ReturnEntry::Call {
                    code,
                    return_pc,
                    name,
                    debug_info,
                }) = self.return_stack.pop()
                {
                    Ok(Some((code, return_pc, name, debug_info)))
                } else {
                    unreachable!()
                }
            }
            Some(_) => Ok(None), // Not a call entry
            None => Ok(None),    // Empty stack
        }
    }

    /// Push a FOR loop state onto the return stack.
    pub fn push_for_loop(&mut self, end: i64, counter: i64) -> Result<(), StackError> {
        if self.return_stack.len() >= self.max_return_depth {
            return Err(StackError::Overflow);
        }
        self.return_stack
            .push(ReturnEntry::ForLoop { end, counter });
        Ok(())
    }

    /// Pop a FOR loop state from the return stack.
    pub fn pop_for_loop(&mut self) -> Result<(i64, i64), StackError> {
        match self.return_stack.pop() {
            Some(ReturnEntry::ForLoop { end, counter }) => Ok((end, counter)),
            Some(_) => Err(StackError::ReturnTypeMismatch),
            None => Err(StackError::ReturnUnderflow),
        }
    }

    /// Push a START loop state onto the return stack.
    pub fn push_start_loop(&mut self, end: i64, counter: i64) -> Result<(), StackError> {
        if self.return_stack.len() >= self.max_return_depth {
            return Err(StackError::Overflow);
        }
        self.return_stack
            .push(ReturnEntry::StartLoop { end, counter });
        Ok(())
    }

    /// Pop a START loop state from the return stack.
    pub fn pop_start_loop(&mut self) -> Result<(i64, i64), StackError> {
        match self.return_stack.pop() {
            Some(ReturnEntry::StartLoop { end, counter }) => Ok((end, counter)),
            Some(_) => Err(StackError::ReturnTypeMismatch),
            None => Err(StackError::ReturnUnderflow),
        }
    }

    /// Push an error handler frame onto the return stack.
    pub fn push_error_handler(
        &mut self,
        handler_pc: usize,
        code: Arc<[Word]>,
    ) -> Result<(), StackError> {
        if self.return_stack.len() >= self.max_return_depth {
            return Err(StackError::Overflow);
        }
        let stack_depth = self.data_stack.len();
        let return_depth = self.return_stack.len();
        self.return_stack.push(ReturnEntry::ErrorHandler {
            handler_pc,
            code,
            stack_depth,
            return_depth,
        });
        Ok(())
    }

    /// Pop an error handler frame from the return stack.
    #[allow(clippy::type_complexity)]
    pub fn pop_error_handler(&mut self) -> Result<(usize, Arc<[Word]>, usize, usize), StackError> {
        match self.return_stack.pop() {
            Some(ReturnEntry::ErrorHandler {
                handler_pc,
                code,
                stack_depth,
                return_depth,
            }) => Ok((handler_pc, code, stack_depth, return_depth)),
            Some(_) => Err(StackError::ReturnTypeMismatch),
            None => Err(StackError::ReturnUnderflow),
        }
    }

    /// Find the nearest error handler on the return stack.
    /// Returns None if no handler is found.
    pub fn find_error_handler(&self) -> Option<usize> {
        for (i, entry) in self.return_stack.iter().enumerate().rev() {
            if matches!(entry, ReturnEntry::ErrorHandler { .. }) {
                return Some(i);
            }
        }
        None
    }

    /// Activate an error handler: unwind the return stack to the handler,
    /// restore stack depths, and return the handler PC and code.
    pub fn activate_error_handler(&mut self) -> Option<(usize, Arc<[Word]>)> {
        // Find the error handler
        let handler_idx = self.find_error_handler()?;

        // Pop everything above the handler
        while self.return_stack.len() > handler_idx + 1 {
            self.return_stack.pop();
        }

        // Pop the handler itself
        if let Some(ReturnEntry::ErrorHandler {
            handler_pc,
            code,
            stack_depth,
            return_depth: _,
        }) = self.return_stack.pop()
        {
            // Restore data stack to the depth when IFERR was entered
            self.data_stack.truncate(stack_depth);
            Some((handler_pc, code))
        } else {
            None
        }
    }

    /// Get the return stack depth.
    pub fn return_depth(&self) -> usize {
        self.return_stack.len()
    }

    /// Get the call depth (number of Call entries on return stack).
    pub fn call_depth(&self) -> usize {
        self.return_stack
            .iter()
            .filter(|e| matches!(e, ReturnEntry::Call { .. }))
            .count()
    }

    /// Get a snapshot of call frames for stack traces.
    /// Returns (name, return_pc, debug_info) tuples from bottom to top.
    pub fn call_frames_snapshot(
        &self,
    ) -> Vec<(Option<String>, usize, Option<Arc<ProgramDebugInfo>>)> {
        self.return_stack
            .iter()
            .filter_map(|e| match e {
                ReturnEntry::Call {
                    name,
                    return_pc,
                    debug_info,
                    ..
                } => Some((name.clone(), *return_pc, debug_info.clone())),
                _ => None,
            })
            .collect()
    }

    /// Get the debug info for the currently executing function (if any).
    /// Returns None if we're in the main program or the function has no debug info.
    pub fn current_debug_info(&self) -> Option<Arc<ProgramDebugInfo>> {
        // The most recent Call entry on the return stack has the debug info
        // for the function we're currently executing inside
        for entry in self.return_stack.iter().rev() {
            if let ReturnEntry::Call { debug_info, .. } = entry {
                return debug_info.clone();
            }
        }
        None
    }

    /// Get all values on the stack (bottom to top).
    pub fn stack(&self) -> &[Value] {
        &self.data_stack
    }

    /// Pop a real number from the stack.
    pub fn pop_real(&mut self) -> Result<f64, StackError> {
        let value = self.pop()?;
        value.as_real().ok_or(StackError::TypeError {
            expected: "real",
            got: value.type_id(),
        })
    }

    /// Pop an integer from the stack.
    pub fn pop_int(&mut self) -> Result<i64, StackError> {
        let value = self.pop()?;
        value.as_int().ok_or(StackError::TypeError {
            expected: "integer",
            got: value.type_id(),
        })
    }

    /// Pop a boolean from the stack.
    pub fn pop_bool(&mut self) -> Result<bool, StackError> {
        let value = self.pop()?;
        value.as_bool().ok_or(StackError::TypeError {
            expected: "boolean",
            got: value.type_id(),
        })
    }

    /// Push a real number onto the stack.
    pub fn push_real(&mut self, v: f64) -> Result<(), StackError> {
        self.push(Value::Real(v))
    }

    /// Push an integer onto the stack.
    pub fn push_int(&mut self, v: i64) -> Result<(), StackError> {
        self.push(Value::Int(v))
    }

    /// Push a boolean onto the stack.
    pub fn push_bool(&mut self, v: bool) -> Result<(), StackError> {
        self.push(Value::Bool(v))
    }

    /// Store a value in the current directory.
    pub fn store(&mut self, name: String, value: Value) {
        self.directories.store(name, value);
    }

    /// Recall a value from the current directory.
    pub fn recall(&self, name: &str) -> Option<&Value> {
        self.directories.recall(name)
    }

    /// Purge (delete) a variable from the current directory.
    /// Returns the removed value if it existed.
    pub fn purge(&mut self, name: &str) -> Option<Value> {
        self.directories.purge(name)
    }

    /// Check if a variable exists in the current directory.
    pub fn has_var(&self, name: &str) -> bool {
        self.directories.has_var(name)
    }

    /// Get all variable names in the current directory.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.directories.var_names()
    }

    /// Clear all variables from the current directory.
    pub fn clear_vars(&mut self) {
        self.directories.clear_vars();
    }

    // ========================================================================
    // Directory navigation
    // ========================================================================

    /// Get the current directory path.
    pub fn dir_path(&self) -> &[String] {
        self.directories.path()
    }

    /// Check if we're at the root (HOME) directory.
    pub fn is_at_home(&self) -> bool {
        self.directories.is_at_home()
    }

    /// Navigate to home (root) directory.
    pub fn home(&mut self) {
        self.directories.home();
    }

    /// Navigate up one directory level.
    /// Returns false if already at root.
    pub fn updir(&mut self) -> bool {
        self.directories.updir()
    }

    /// Create a subdirectory in the current directory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.directories.create_subdir(name)
    }

    /// Navigate into a subdirectory.
    /// Returns false if the subdirectory doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        self.directories.enter_subdir(name)
    }

    /// Remove a subdirectory from the current directory (must be empty).
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), &'static str> {
        self.directories.remove_subdir(name)
    }

    /// Get all subdirectory names in the current directory.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.directories.subdir_names()
    }

    // ========================================================================
    // Local variable frames
    // ========================================================================

    /// Push a new local frame onto the frame stack.
    pub fn push_local_frame(&mut self, frame: LocalFrame) {
        self.local_frames.push(frame);
    }

    /// Pop the top local frame from the frame stack.
    pub fn pop_local_frame(&mut self) -> Option<LocalFrame> {
        self.local_frames.pop()
    }

    /// Get the current (top) local frame, if any.
    pub fn current_local_frame(&self) -> Option<&LocalFrame> {
        self.local_frames.last()
    }

    /// Get the current (top) local frame mutably, if any.
    pub fn current_local_frame_mut(&mut self) -> Option<&mut LocalFrame> {
        self.local_frames.last_mut()
    }

    /// Look up a local variable by searching frames from top to bottom.
    pub fn get_local(&self, name: &str) -> Option<&Value> {
        for frame in self.local_frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Check if a local variable exists in any frame.
    pub fn has_local(&self, name: &str) -> bool {
        self.local_frames.iter().rev().any(|f| f.has(name))
    }

    /// Get the number of local frames.
    pub fn local_frame_depth(&self) -> usize {
        self.local_frames.len()
    }

    /// Look up a variable - checks locals first, then globals.
    pub fn lookup(&self, name: &str) -> Option<&Value> {
        // Check local frames first (innermost to outermost)
        if let Some(value) = self.get_local(name) {
            return Some(value);
        }
        // Fall back to global directory
        self.recall(name)
    }

    // ========================================================================
    // Word size for binary integer operations
    // ========================================================================

    /// Get the current word size for binary integer operations.
    pub fn word_size(&self) -> u8 {
        self.word_size
    }

    /// Set the word size for binary integer operations (1-64 bits).
    pub fn set_word_size(&mut self, ws: u8) {
        self.word_size = ws;
    }

    // ========================================================================
    // Debug inspection methods
    // ========================================================================

    /// Get a snapshot of the data stack (cloned values).
    pub fn stack_snapshot(&self) -> Vec<Value> {
        self.data_stack.clone()
    }

    /// Get a snapshot of all variables in the current directory.
    pub fn globals_snapshot(&self) -> HashMap<String, Value> {
        self.directories.vars_snapshot()
    }

    /// Get a snapshot of the current local frame (if any).
    pub fn locals_snapshot(&self) -> Option<HashMap<String, Value>> {
        self.local_frames.last().map(|f| f.bindings.clone())
    }

    /// Get snapshots of all local frames (innermost first).
    pub fn all_locals_snapshot(&self) -> Vec<HashMap<String, Value>> {
        self.local_frames
            .iter()
            .rev()
            .map(|f| f.bindings.clone())
            .collect()
    }

    /// Get a snapshot of the return stack.
    /// Used for building stack traces in the debugger.
    pub fn return_stack_snapshot(&self) -> Vec<ReturnEntry> {
        self.return_stack.clone()
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_new() {
        let vm = VM::new();
        assert_eq!(vm.depth(), 0);
        assert_eq!(vm.return_depth(), 0);
        assert_eq!(vm.pc, 0);
    }

    #[test]
    fn vm_push_pop() {
        let mut vm = VM::new();

        vm.push(Value::Real(3.15)).unwrap();
        vm.push(Value::Int(42)).unwrap();

        assert_eq!(vm.depth(), 2);

        let v = vm.pop().unwrap();
        assert_eq!(v, Value::Int(42));

        let v = vm.pop().unwrap();
        assert_eq!(v, Value::Real(3.15));

        assert_eq!(vm.depth(), 0);
    }

    #[test]
    fn vm_pop_underflow() {
        let mut vm = VM::new();
        assert_eq!(vm.pop(), Err(StackError::Underflow));
    }

    #[test]
    fn vm_peek() {
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();
        vm.push(Value::Real(2.0)).unwrap();
        vm.push(Value::Real(3.0)).unwrap();

        assert_eq!(vm.peek(0), Ok(&Value::Real(3.0)));
        assert_eq!(vm.peek(1), Ok(&Value::Real(2.0)));
        assert_eq!(vm.peek(2), Ok(&Value::Real(1.0)));
        assert_eq!(vm.peek(3), Err(StackError::Underflow));
    }

    #[test]
    fn vm_clear() {
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();
        vm.push(Value::Real(2.0)).unwrap();
        vm.clear();
        assert_eq!(vm.depth(), 0);
    }

    #[test]
    fn vm_for_loop_stack() {
        let mut vm = VM::new();
        vm.push_for_loop(10, 1).unwrap();
        vm.push_for_loop(5, 3).unwrap();

        assert_eq!(vm.return_depth(), 2);
        assert_eq!(vm.pop_for_loop(), Ok((5, 3)));
        assert_eq!(vm.pop_for_loop(), Ok((10, 1)));
        assert_eq!(vm.pop_for_loop(), Err(StackError::ReturnUnderflow));
    }

    #[test]
    fn vm_call_stack() {
        let mut vm = VM::new();
        let code1: Arc<[Word]> = Arc::from([1u32, 2, 3]);
        let code2: Arc<[Word]> = Arc::from([4u32, 5]);

        vm.push_call(code1.clone(), 10, Some("foo".to_string()), None)
            .unwrap();
        vm.push_call(code2.clone(), 20, None, None).unwrap();

        assert_eq!(vm.return_depth(), 2);
        assert_eq!(vm.call_depth(), 2);

        let (_code, pc, name, _debug) = vm.try_pop_call().unwrap().unwrap();
        assert_eq!(pc, 20);
        assert_eq!(name, None);

        let (_code, pc, name, _debug) = vm.try_pop_call().unwrap().unwrap();
        assert_eq!(pc, 10);
        assert_eq!(name, Some("foo".to_string()));

        assert!(vm.try_pop_call().unwrap().is_none());
    }

    #[test]
    fn vm_pop_real() {
        let mut vm = VM::new();
        vm.push(Value::Real(3.15)).unwrap();
        assert_eq!(vm.pop_real(), Ok(3.15));

        vm.push(Value::Int(42)).unwrap();
        assert_eq!(vm.pop_real(), Ok(42.0));
    }

    #[test]
    fn vm_pop_int() {
        let mut vm = VM::new();
        vm.push(Value::Int(42)).unwrap();
        assert_eq!(vm.pop_int(), Ok(42));

        vm.push(Value::Real(3.7)).unwrap();
        assert_eq!(vm.pop_int(), Ok(3));
    }

    #[test]
    fn vm_push_helpers() {
        let mut vm = VM::new();
        vm.push_real(3.15).unwrap();
        vm.push_int(42).unwrap();
        vm.push_bool(true).unwrap();

        assert_eq!(vm.pop(), Ok(Value::Bool(true)));
        assert_eq!(vm.pop(), Ok(Value::Int(42)));
        assert_eq!(vm.pop(), Ok(Value::Real(3.15)));
    }

    #[test]
    fn vm_stack_accessor() {
        let mut vm = VM::new();
        vm.push(Value::Real(1.0)).unwrap();
        vm.push(Value::Real(2.0)).unwrap();

        let stack = vm.stack();
        assert_eq!(stack.len(), 2);
        assert_eq!(stack[0], Value::Real(1.0)); // bottom
        assert_eq!(stack[1], Value::Real(2.0)); // top
    }

    #[test]
    fn vm_stack_overflow_with_custom_limit() {
        let mut vm = VM::with_limits(3, 3);

        // Should succeed for first 3 pushes
        vm.push(Value::Real(1.0)).unwrap();
        vm.push(Value::Real(2.0)).unwrap();
        vm.push(Value::Real(3.0)).unwrap();

        // Fourth push should fail
        assert_eq!(vm.push(Value::Real(4.0)), Err(StackError::Overflow));
    }

    #[test]
    fn vm_return_stack_overflow_with_custom_limit() {
        let mut vm = VM::with_limits(100, 2);

        // Should succeed for first 2 pushes
        vm.push_for_loop(10, 1).unwrap();
        vm.push_for_loop(5, 1).unwrap();

        // Third push should fail
        assert_eq!(vm.push_for_loop(3, 1), Err(StackError::Overflow));
    }

    #[test]
    fn vm_store_recall() {
        let mut vm = VM::new();
        vm.store("x".to_string(), Value::Real(42.0));

        assert!(vm.has_var("x"));
        assert_eq!(vm.recall("x"), Some(&Value::Real(42.0)));
        assert_eq!(vm.recall("y"), None);
    }

    #[test]
    fn vm_purge() {
        let mut vm = VM::new();
        vm.store("x".to_string(), Value::Real(42.0));

        assert!(vm.has_var("x"));
        let removed = vm.purge("x");
        assert_eq!(removed, Some(Value::Real(42.0)));
        assert!(!vm.has_var("x"));

        // Purge non-existent returns None
        assert_eq!(vm.purge("y"), None);
    }

    #[test]
    fn vm_vars() {
        let mut vm = VM::new();
        vm.store("a".to_string(), Value::Real(1.0));
        vm.store("b".to_string(), Value::Real(2.0));

        let names: Vec<&String> = vm.vars().collect();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&&"a".to_string()));
        assert!(names.contains(&&"b".to_string()));
    }

    #[test]
    fn vm_clear_vars() {
        let mut vm = VM::new();
        vm.store("x".to_string(), Value::Real(42.0));
        vm.store("y".to_string(), Value::Real(43.0));

        vm.clear_vars();
        assert!(!vm.has_var("x"));
        assert!(!vm.has_var("y"));
    }

    #[test]
    fn vm_local_frame_push_pop() {
        let mut vm = VM::new();
        assert_eq!(vm.local_frame_depth(), 0);

        let mut frame = LocalFrame::new();
        frame.bind("x".to_string(), Value::Int(42));
        vm.push_local_frame(frame);

        assert_eq!(vm.local_frame_depth(), 1);
        assert!(vm.has_local("x"));
        assert_eq!(vm.get_local("x"), Some(&Value::Int(42)));

        vm.pop_local_frame();
        assert_eq!(vm.local_frame_depth(), 0);
        assert!(!vm.has_local("x"));
    }

    #[test]
    fn vm_local_shadows_global() {
        let mut vm = VM::new();
        vm.store("x".to_string(), Value::Int(100));

        // Global is visible
        assert_eq!(vm.lookup("x"), Some(&Value::Int(100)));

        // Push local frame with same name
        let mut frame = LocalFrame::new();
        frame.bind("x".to_string(), Value::Int(42));
        vm.push_local_frame(frame);

        // Local shadows global
        assert_eq!(vm.lookup("x"), Some(&Value::Int(42)));

        // Pop frame, global visible again
        vm.pop_local_frame();
        assert_eq!(vm.lookup("x"), Some(&Value::Int(100)));
    }

    #[test]
    fn vm_nested_local_frames() {
        let mut vm = VM::new();

        // Outer frame
        let mut outer = LocalFrame::new();
        outer.bind("a".to_string(), Value::Int(1));
        outer.bind("b".to_string(), Value::Int(2));
        vm.push_local_frame(outer);

        assert_eq!(vm.lookup("a"), Some(&Value::Int(1)));
        assert_eq!(vm.lookup("b"), Some(&Value::Int(2)));

        // Inner frame shadows 'a'
        let mut inner = LocalFrame::new();
        inner.bind("a".to_string(), Value::Int(10));
        inner.bind("c".to_string(), Value::Int(3));
        vm.push_local_frame(inner);

        assert_eq!(vm.lookup("a"), Some(&Value::Int(10))); // inner shadows
        assert_eq!(vm.lookup("b"), Some(&Value::Int(2)));  // from outer
        assert_eq!(vm.lookup("c"), Some(&Value::Int(3)));  // from inner

        // Pop inner
        vm.pop_local_frame();
        assert_eq!(vm.lookup("a"), Some(&Value::Int(1))); // outer visible again
        assert_eq!(vm.lookup("c"), None); // inner's var gone
    }

    #[test]
    fn vm_lookup_falls_through() {
        let mut vm = VM::new();
        vm.store("global".to_string(), Value::Int(999));

        let mut frame = LocalFrame::new();
        frame.bind("local".to_string(), Value::Int(42));
        vm.push_local_frame(frame);

        // Local found in frame
        assert_eq!(vm.lookup("local"), Some(&Value::Int(42)));
        // Global found by fallthrough
        assert_eq!(vm.lookup("global"), Some(&Value::Int(999)));
        // Non-existent returns None
        assert_eq!(vm.lookup("nonexistent"), None);
    }
}
