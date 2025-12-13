use std::cell::RefCell;

use rpl_core::{Interner, Span, Symbol, TokenInfo, Word};

// Re-export ConstructStack for backwards compatibility with tests
pub use self::construct_stack::ConstructStack;
use super::traits::ConstructKind;
use crate::{compile::OutputBuffer, user_libs::UserLibraryRegistry};

mod construct_stack {
    use super::ConstructKind;

    /// Simple construct stack for tracking nesting context.
    /// Used by CompileContext for context-sensitive compilation.
    pub struct ConstructStack {
        stack: Vec<ConstructKind>,
    }

    impl ConstructStack {
        pub fn new() -> Self {
            Self { stack: Vec::new() }
        }

        pub fn push(&mut self, kind: ConstructKind) {
            self.stack.push(kind);
        }

        pub fn pop(&mut self) -> Option<ConstructKind> {
            self.stack.pop()
        }

        pub fn top(&self) -> Option<ConstructKind> {
            self.stack.last().copied()
        }

        pub fn depth(&self) -> usize {
            self.stack.len()
        }

        pub fn is_empty(&self) -> bool {
            self.stack.is_empty()
        }
    }

    impl Default for ConstructStack {
        fn default() -> Self {
            Self::new()
        }
    }
}

/// Context for probing tokens.
pub struct ProbeContext<'a> {
    text: &'a str,
    source: &'a str,
    span: Span,
    in_infix: bool,
    construct: Option<ConstructKind>,
    previous: Option<TokenInfo>,
    interner: &'a Interner,
    user_lib_registry: Option<&'a UserLibraryRegistry>,
}

impl<'a> ProbeContext<'a> {
    pub fn new(
        text: &'a str,
        source: &'a str,
        span: Span,
        in_infix: bool,
        construct: Option<ConstructKind>,
        previous: Option<TokenInfo>,
        interner: &'a Interner,
    ) -> Self {
        Self {
            text,
            source,
            span,
            in_infix,
            construct,
            previous,
            interner,
            user_lib_registry: None,
        }
    }

    /// Create a ProbeContext with access to the user library registry.
    #[allow(clippy::too_many_arguments)]
    pub fn with_user_lib_registry(
        text: &'a str,
        source: &'a str,
        span: Span,
        in_infix: bool,
        construct: Option<ConstructKind>,
        previous: Option<TokenInfo>,
        interner: &'a Interner,
        user_lib_registry: &'a UserLibraryRegistry,
    ) -> Self {
        Self {
            text,
            source,
            span,
            in_infix,
            construct,
            previous,
            interner,
            user_lib_registry: Some(user_lib_registry),
        }
    }

    /// The token text being probed.
    pub fn text(&self) -> &str {
        self.text
    }

    /// The full source code.
    pub fn source(&self) -> &str {
        self.source
    }

    /// The span of the token.
    pub fn span(&self) -> Span {
        self.span
    }

    /// Whether we're in infix mode.
    pub fn in_infix(&self) -> bool {
        self.in_infix
    }

    /// The current construct, if any.
    pub fn construct(&self) -> Option<ConstructKind> {
        self.construct
    }

    /// The previous token info, if any.
    pub fn previous(&self) -> Option<TokenInfo> {
        self.previous
    }

    /// The string interner.
    pub fn interner(&self) -> &Interner {
        self.interner
    }

    /// The user library registry for compile-time command resolution.
    pub fn user_lib_registry(&self) -> Option<&UserLibraryRegistry> {
        self.user_lib_registry
    }
}

/// Context for compiling tokens.
///
/// Libraries use this context to emit bytecode. The context automatically
/// attaches the current token's span to all emitted words.
pub struct CompileContext<'a> {
    token_span: Span,
    token_text: &'a str,
    output: &'a mut OutputBuffer,
    interner: &'a mut Interner,
    current_construct: Option<ConstructKind>,
    in_infix: bool,
    user_lib_registry: Option<&'a UserLibraryRegistry>,
}

impl<'a> CompileContext<'a> {
    pub fn new(
        token_span: Span,
        token_text: &'a str,
        output: &'a mut OutputBuffer,
        interner: &'a mut Interner,
        current_construct: Option<ConstructKind>,
        in_infix: bool,
    ) -> Self {
        Self {
            token_span,
            token_text,
            output,
            interner,
            current_construct,
            in_infix,
            user_lib_registry: None,
        }
    }

    /// Create a CompileContext with access to the user library registry.
    pub fn with_user_lib_registry(
        token_span: Span,
        token_text: &'a str,
        output: &'a mut OutputBuffer,
        interner: &'a mut Interner,
        current_construct: Option<ConstructKind>,
        in_infix: bool,
        user_lib_registry: &'a UserLibraryRegistry,
    ) -> Self {
        Self {
            token_span,
            token_text,
            output,
            interner,
            current_construct,
            in_infix,
            user_lib_registry: Some(user_lib_registry),
        }
    }

    /// Emit a word to the output (span attached automatically).
    pub fn emit(&mut self, word: Word) {
        self.output.emit(word, self.token_span);
    }

    /// Emit an opcode (library call).
    pub fn emit_opcode(&mut self, lib: u16, cmd: u16) {
        self.emit(rpl_core::make_call(lib, cmd));
    }

    /// Emit a prolog (object header).
    pub fn emit_prolog(&mut self, type_id: u16, size: u16) {
        self.emit(rpl_core::make_prolog(type_id, size));
    }

    /// Get the current output position.
    pub fn position(&self) -> usize {
        self.output.len()
    }

    /// Reserve space in the output, returning the start position.
    pub fn reserve(&mut self, count: usize) -> usize {
        self.output.reserve(count, self.token_span)
    }

    /// Patch a word at a given position.
    pub fn patch(&mut self, pos: usize, word: Word) {
        self.output.patch(pos, word);
    }

    /// Get the token span.
    pub fn span(&self) -> Span {
        self.token_span
    }

    /// Get the token text.
    pub fn text(&self) -> &str {
        self.token_text
    }

    /// Intern a string.
    pub fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    /// Check if we're in infix mode.
    pub fn in_infix(&self) -> bool {
        self.in_infix
    }

    /// Get the current construct.
    pub fn current_construct(&self) -> Option<ConstructKind> {
        self.current_construct
    }

    /// The user library registry for compile-time command resolution.
    pub fn user_lib_registry(&self) -> Option<&UserLibraryRegistry> {
        self.user_lib_registry
    }
}

/// Context for executing bytecode.
///
/// Libraries use this context to access the VM and read operand words
/// from the code stream.
pub struct ExecuteContext<'a> {
    vm: &'a mut rpl_vm::VM,
    code: &'a [Word],
    pc: usize,
    cmd: u16,
    operands_read: usize,
    /// Raw pointer to user library registry RefCell for ATTACH/DETACH commands.
    /// Using a raw pointer avoids complex lifetime issues with the RefCell.
    /// Safety: The pointer is valid for the duration of the execute call.
    user_libs_ptr: Option<*const RefCell<*mut UserLibraryRegistry>>,
}

impl<'a> ExecuteContext<'a> {
    /// Create a new execute context.
    pub fn new(vm: &'a mut rpl_vm::VM, code: &'a [Word], pc: usize, cmd: u16) -> Self {
        Self {
            vm,
            code,
            pc,
            cmd,
            operands_read: 0,
            user_libs_ptr: None,
        }
    }

    /// Create a new execute context with user library access.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the RefCell remains valid for the duration
    /// of the ExecuteContext's use.
    pub fn with_user_libs(
        vm: &'a mut rpl_vm::VM,
        code: &'a [Word],
        pc: usize,
        cmd: u16,
        user_libs: *const RefCell<*mut UserLibraryRegistry>,
    ) -> Self {
        Self {
            vm,
            code,
            pc,
            cmd,
            operands_read: 0,
            user_libs_ptr: Some(user_libs),
        }
    }

    /// Get mutable access to the user library registry.
    ///
    /// # Panics
    ///
    /// Panics if user_libs was not provided to this context.
    pub fn user_libs_mut(&self) -> UserLibsGuard<'_> {
        let ptr = self.user_libs_ptr
            .expect("user_libs_mut called without user library registry");
        // SAFETY: The pointer was provided by LibraryDispatcher and is valid
        // for the duration of the dispatch call.
        let cell = unsafe { &*ptr };
        UserLibsGuard { guard: cell.borrow_mut() }
    }
}

/// Guard type for user library registry access.
pub struct UserLibsGuard<'a> {
    guard: std::cell::RefMut<'a, *mut UserLibraryRegistry>,
}

impl std::ops::Deref for UserLibsGuard<'_> {
    type Target = UserLibraryRegistry;

    fn deref(&self) -> &Self::Target {
        // SAFETY: The pointer is valid and we have the RefMut guard
        unsafe { &**self.guard }
    }
}

impl std::ops::DerefMut for UserLibsGuard<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: The pointer is valid and we have the RefMut guard
        unsafe { &mut **self.guard }
    }
}

impl<'a> ExecuteContext<'a> {
    /// Get the interner from the VM.
    pub fn interner(&self) -> &Interner {
        self.vm.interner()
    }

    /// Get the command ID.
    pub fn cmd(&self) -> u16 {
        self.cmd
    }

    /// Read the next operand word from the code stream.
    pub fn read_operand(&mut self) -> Result<Word, &'static str> {
        let idx = self.pc + 1 + self.operands_read;
        let word = self.code.get(idx).ok_or("Unexpected end of code")?;
        self.operands_read += 1;
        Ok(*word)
    }

    /// Get the number of operand words consumed.
    pub fn operands_consumed(&self) -> usize {
        self.operands_read
    }

    /// Push a value onto the data stack.
    pub fn push(&mut self, value: rpl_vm::Value) -> Result<(), rpl_vm::StackError> {
        self.vm.push(value)
    }

    /// Pop a value from the data stack.
    pub fn pop(&mut self) -> Result<rpl_vm::Value, rpl_vm::StackError> {
        self.vm.pop()
    }

    /// Peek at a value on the data stack.
    pub fn peek(&self, depth: usize) -> Result<&rpl_vm::Value, rpl_vm::StackError> {
        self.vm.peek(depth)
    }

    /// Pop a real number from the stack.
    pub fn pop_real(&mut self) -> Result<f64, rpl_vm::StackError> {
        self.vm.pop_real()
    }

    /// Pop an integer from the stack.
    pub fn pop_int(&mut self) -> Result<i64, rpl_vm::StackError> {
        self.vm.pop_int()
    }

    /// Push a real number onto the stack.
    pub fn push_real(&mut self, value: f64) -> Result<(), rpl_vm::StackError> {
        self.vm.push_real(value)
    }

    /// Push an integer onto the stack.
    pub fn push_int(&mut self, value: i64) -> Result<(), rpl_vm::StackError> {
        self.vm.push_int(value)
    }

    /// Get the current stack depth.
    pub fn depth(&self) -> usize {
        self.vm.depth()
    }

    /// Clear the stack.
    pub fn clear(&mut self) {
        self.vm.clear()
    }

    /// Push a FOR loop state onto the return stack.
    pub fn push_for_loop(
        &mut self,
        start: i64,
        end: i64,
        direction: i64,
        counter: i64,
    ) -> Result<(), rpl_vm::StackError> {
        self.vm.push_for_loop(start, end, direction, counter)
    }

    /// Pop a FOR loop state from the return stack.
    pub fn pop_for_loop(&mut self) -> Result<(i64, i64, i64, i64), rpl_vm::StackError> {
        self.vm.pop_for_loop()
    }

    /// Push a START loop state onto the return stack.
    pub fn push_start_loop(
        &mut self,
        start: i64,
        end: i64,
        direction: i64,
        counter: i64,
    ) -> Result<(), rpl_vm::StackError> {
        self.vm.push_start_loop(start, end, direction, counter)
    }

    /// Pop a START loop state from the return stack.
    pub fn pop_start_loop(
        &mut self,
    ) -> Result<(i64, i64, i64, i64), rpl_vm::StackError> {
        self.vm.pop_start_loop()
    }

    /// Push an error handler frame onto the return stack.
    pub fn push_error_handler(
        &mut self,
        handler_pc: usize,
        code: std::sync::Arc<[rpl_core::Word]>,
    ) -> Result<(), rpl_vm::StackError> {
        self.vm.push_error_handler(handler_pc, code)
    }

    /// Pop an error handler frame from the return stack.
    #[allow(clippy::type_complexity)]
    pub fn pop_error_handler(
        &mut self,
    ) -> Result<
        (usize, std::sync::Arc<[rpl_core::Word]>, usize, usize),
        rpl_vm::StackError,
    > {
        self.vm.pop_error_handler()
    }

    /// Get the current bytecode being executed.
    pub fn current_code(&self) -> std::sync::Arc<[rpl_core::Word]> {
        self.vm.current_code()
    }

    /// Store a value in the global directory.
    pub fn store(&mut self, name: String, value: rpl_vm::Value) {
        self.vm.store(name, value)
    }

    /// Recall a value from the global directory.
    pub fn recall(&self, name: &str) -> Option<&rpl_vm::Value> {
        self.vm.recall(name)
    }

    /// Purge a variable from the global directory.
    pub fn purge(&mut self, name: &str) -> Option<rpl_vm::Value> {
        self.vm.purge(name)
    }

    /// Check if a variable exists.
    pub fn has_var(&self, name: &str) -> bool {
        self.vm.has_var(name)
    }

    /// Get an iterator over all variable names.
    pub fn vars(&self) -> impl Iterator<Item = &String> {
        self.vm.vars()
    }

    /// Clear all variables from the current directory.
    pub fn clear_vars(&mut self) {
        self.vm.clear_vars()
    }

    // ========================================================================
    // Directory navigation
    // ========================================================================

    /// Get the current directory path.
    pub fn dir_path(&self) -> &[String] {
        self.vm.dir_path()
    }

    /// Check if we're at the root (HOME) directory.
    pub fn is_at_home(&self) -> bool {
        self.vm.is_at_home()
    }

    /// Navigate to home (root) directory.
    pub fn home(&mut self) {
        self.vm.home()
    }

    /// Navigate up one directory level.
    /// Returns false if already at root.
    pub fn updir(&mut self) -> bool {
        self.vm.updir()
    }

    /// Create a subdirectory in the current directory.
    pub fn create_subdir(&mut self, name: String) -> bool {
        self.vm.create_subdir(name)
    }

    /// Navigate into a subdirectory.
    /// Returns false if the subdirectory doesn't exist.
    pub fn enter_subdir(&mut self, name: &str) -> bool {
        self.vm.enter_subdir(name)
    }

    /// Remove a subdirectory from the current directory (must be empty).
    pub fn remove_subdir(&mut self, name: &str) -> Result<(), &'static str> {
        self.vm.remove_subdir(name)
    }

    /// Get all subdirectory names in the current directory.
    pub fn subdir_names(&self) -> impl Iterator<Item = &String> {
        self.vm.subdir_names()
    }

    /// Rename a variable.
    /// Returns true if the variable existed and was renamed.
    pub fn rename_var(&mut self, old_name: &str, new_name: &str) -> bool {
        if let Some(value) = self.vm.purge(old_name) {
            self.vm.store(new_name.to_string(), value);
            true
        } else {
            false
        }
    }

    /// Purge all variables matching a prefix.
    /// Returns the number of variables purged.
    pub fn purge_by_prefix(&mut self, prefix: &str) -> usize {
        // First, collect all matching names (can't mutate while iterating)
        let to_purge: Vec<String> = self
            .vm
            .vars()
            .filter(|name| name.starts_with(prefix))
            .cloned()
            .collect();

        let count = to_purge.len();
        for name in to_purge {
            self.vm.purge(&name);
        }
        count
    }

    // ========================================================================
    // Local variable frames
    // ========================================================================

    /// Push a new local frame.
    pub fn push_local_frame(&mut self, frame: rpl_vm::LocalFrame) {
        self.vm.push_local_frame(frame)
    }

    /// Pop the top local frame.
    pub fn pop_local_frame(&mut self) -> Option<rpl_vm::LocalFrame> {
        self.vm.pop_local_frame()
    }

    /// Look up a variable (locals first, then globals).
    pub fn lookup(&self, name: &str) -> Option<&rpl_vm::Value> {
        self.vm.lookup(name)
    }

    /// Check if a local variable exists.
    pub fn has_local(&self, name: &str) -> bool {
        self.vm.has_local(name)
    }

    /// Get the local frame depth.
    pub fn local_frame_depth(&self) -> usize {
        self.vm.local_frame_depth()
    }

    /// Look up a variable in global directory only (not locals).
    /// Used by CMD_EVAL_NAME since locals are resolved at compile time.
    pub fn lookup_global(&self, name: &str) -> Option<&rpl_vm::Value> {
        self.vm.recall(name)
    }

    /// Create a local frame for a FOR loop with a single variable.
    pub fn create_local_frame_for(
        &mut self,
        symbol: Symbol,
        start_value: i64,
    ) -> Result<(), rpl_vm::StackError> {
        let name = self.vm.interner().resolve(symbol).to_string();
        let mut frame = rpl_vm::LocalFrame::new();
        // Use Real for compatibility with arithmetic operations
        frame.set(&name, rpl_vm::Value::Real(start_value as f64));
        self.vm.push_local_frame(frame);
        Ok(())
    }

    /// Increment the FOR loop variable in the current local frame.
    pub fn increment_for_variable(&mut self) {
        if let Some(frame) = self.vm.current_local_frame_mut() {
            // The FOR variable is the only variable in the frame
            // Increment its value
            frame.increment_first();
        }
    }

    /// Get the FOR loop variable value from the current local frame.
    pub fn get_for_variable(&self) -> i64 {
        if let Some(frame) = self.vm.current_local_frame() {
            frame.get_first()
        } else {
            0
        }
    }

    /// Set the FOR loop variable value in the current local frame.
    pub fn set_for_variable(&mut self, value: i64) {
        if let Some(frame) = self.vm.current_local_frame_mut() {
            frame.set_first(value);
        }
    }

    // ========================================================================
    // Binary integer operations
    // ========================================================================

    /// Pop a binary integer from the stack.
    pub fn pop_bint(&mut self) -> Result<i64, rpl_vm::StackError> {
        self.vm.pop_int()
    }

    /// Push a binary integer onto the stack.
    pub fn push_bint(&mut self, n: i64) -> Result<(), rpl_vm::StackError> {
        self.vm.push_int(n)
    }

    /// Get the current word size for binary integer operations.
    pub fn word_size(&self) -> u8 {
        self.vm.word_size()
    }

    /// Set the word size for binary integer operations.
    pub fn set_word_size(&mut self, ws: u8) {
        self.vm.set_word_size(ws)
    }
}

/// Mode for decompilation - distinguishes prolog vs call handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecompileMode {
    /// Driver is asking "is this your prolog type?"
    Prolog,
    /// Driver is asking "decompile this call with the given cmd"
    Call(u16),
}

/// Context for decompiling bytecode.
pub struct DecompileContext<'a> {
    code: &'a [Word],
    pos: &'a mut usize,
    output: &'a mut String,
    indent: usize,
    registry: Option<&'a crate::library::LibraryRegistry>,
    mode: DecompileMode,
    /// Interner for resolving symbol names (for local variables).
    pub interner: Option<&'a Interner>,
    /// User library registry for decompiling LIBPTR to command names.
    user_lib_registry: Option<&'a UserLibraryRegistry>,
}

impl<'a> DecompileContext<'a> {
    /// Create a context for prolog decompilation (trying to match a literal type).
    pub fn for_prolog(code: &'a [Word], pos: &'a mut usize, output: &'a mut String) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: None,
            mode: DecompileMode::Prolog,
            interner: None,
            user_lib_registry: None,
        }
    }

    /// Create a context for call decompilation (specific command).
    pub fn for_call(
        code: &'a [Word],
        pos: &'a mut usize,
        output: &'a mut String,
        cmd: u16,
    ) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: None,
            mode: DecompileMode::Call(cmd),
            interner: None,
            user_lib_registry: None,
        }
    }

    /// Create a prolog context with registry for recursive decompilation.
    pub fn for_prolog_with_registry(
        code: &'a [Word],
        pos: &'a mut usize,
        output: &'a mut String,
        registry: &'a crate::library::LibraryRegistry,
    ) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: Some(registry),
            mode: DecompileMode::Prolog,
            interner: None,
            user_lib_registry: None,
        }
    }

    /// Create a prolog context with registry and interner for recursive decompilation.
    pub fn for_prolog_with_interner(
        code: &'a [Word],
        pos: &'a mut usize,
        output: &'a mut String,
        registry: &'a crate::library::LibraryRegistry,
        interner: &'a Interner,
    ) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: Some(registry),
            mode: DecompileMode::Prolog,
            interner: Some(interner),
            user_lib_registry: None,
        }
    }

    /// Create a call context with registry for recursive decompilation.
    pub fn for_call_with_registry(
        code: &'a [Word],
        pos: &'a mut usize,
        output: &'a mut String,
        registry: &'a crate::library::LibraryRegistry,
        cmd: u16,
    ) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: Some(registry),
            mode: DecompileMode::Call(cmd),
            interner: None,
            user_lib_registry: None,
        }
    }

    /// Create a call context with registry and interner for recursive decompilation.
    pub fn for_call_with_interner(
        code: &'a [Word],
        pos: &'a mut usize,
        output: &'a mut String,
        registry: &'a crate::library::LibraryRegistry,
        cmd: u16,
        interner: &'a Interner,
    ) -> Self {
        Self {
            code,
            pos,
            output,
            indent: 0,
            registry: Some(registry),
            mode: DecompileMode::Call(cmd),
            interner: Some(interner),
            user_lib_registry: None,
        }
    }

    /// Get the decompilation mode.
    pub fn mode(&self) -> DecompileMode {
        self.mode
    }

    /// Get the command ID if in Call mode.
    pub fn cmd(&self) -> Option<u16> {
        match self.mode {
            DecompileMode::Call(cmd) => Some(cmd),
            DecompileMode::Prolog => None,
        }
    }

    /// Read the next word.
    pub fn read(&mut self) -> Option<Word> {
        if *self.pos < self.code.len() {
            let word = self.code[*self.pos];
            *self.pos += 1;
            Some(word)
        } else {
            None
        }
    }

    /// Peek at the next word without advancing.
    pub fn peek(&self) -> Option<Word> {
        self.code.get(*self.pos).copied()
    }

    /// Write text to the output.
    pub fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Increase indentation.
    pub fn indent(&mut self) {
        self.indent += 2;
    }

    /// Decrease indentation.
    pub fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(2);
    }

    /// Write a newline with current indentation.
    pub fn newline(&mut self) {
        self.output.push('\n');
        for _ in 0..self.indent {
            self.output.push(' ');
        }
    }

    /// Get the current position in the code.
    pub fn position(&self) -> usize {
        *self.pos
    }

    /// Get the remaining code slice from current position.
    pub fn remaining(&self) -> &[Word] {
        &self.code[*self.pos..]
    }

    /// Get a slice of code from current position with given length.
    pub fn slice(&self, len: usize) -> &[Word] {
        let end = (*self.pos + len).min(self.code.len());
        &self.code[*self.pos..end]
    }

    /// Skip a number of words without reading them.
    pub fn skip(&mut self, count: usize) {
        *self.pos = (*self.pos + count).min(self.code.len());
    }

    /// Write a space if the output doesn't end with whitespace.
    pub fn write_space(&mut self) {
        if !self.output.is_empty() && !self.output.ends_with(' ') && !self.output.ends_with('\n') {
            self.output.push(' ');
        }
    }

    /// Decompile inner bytecode recursively.
    ///
    /// This reads `count` words from the current position and decompiles them,
    /// appending the result to the output. Requires the context to have been
    /// created with a registry.
    pub fn decompile_inner(&mut self, count: usize) {
        if let Some(registry) = self.registry {
            // Extract the inner code
            let end = (*self.pos + count).min(self.code.len());
            let inner_code = &self.code[*self.pos..end];
            *self.pos = end;

            // Decompile the inner code (pass interner if available)
            let inner_output =
                crate::decompile::decompile_inner(inner_code, registry, self.interner);
            self.output.push_str(&inner_output);
        } else {
            // No registry - just skip the words
            self.skip(count);
        }
    }

    /// Check if this context has a registry for recursive decompilation.
    pub fn has_registry(&self) -> bool {
        self.registry.is_some()
    }

    /// Get the user library registry for decompiling LIBPTR to command names.
    pub fn user_lib_registry(&self) -> Option<&UserLibraryRegistry> {
        self.user_lib_registry
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::Pos;

    use super::*;

    #[test]
    fn probe_context_accessors() {
        let interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));
        let ctx = ProbeContext::new("DUP", "DUP DROP", span, false, None, None, &interner);

        assert_eq!(ctx.text(), "DUP");
        assert_eq!(ctx.source(), "DUP DROP");
        assert_eq!(ctx.span(), span);
        assert!(!ctx.in_infix());
        assert!(ctx.construct().is_none());
        assert!(ctx.previous().is_none());
    }

    #[test]
    fn construct_stack_operations() {
        let mut stack = ConstructStack::new();
        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);

        stack.push(ConstructKind::Program);
        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.top(), Some(ConstructKind::Program));

        stack.push(ConstructKind::List);
        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.top(), Some(ConstructKind::List));

        assert_eq!(stack.pop(), Some(ConstructKind::List));
        assert_eq!(stack.depth(), 1);
    }

    #[test]
    fn compile_context_emit() {
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));

        let mut ctx = CompileContext::new(span, "DUP", &mut output, &mut interner, None, false);

        ctx.emit(42);
        assert_eq!(ctx.position(), 1);

        ctx.emit_opcode(10, 5);
        assert_eq!(ctx.position(), 2);
    }

    #[test]
    fn decompile_context_read_write() {
        let code = vec![1, 2, 3];
        let mut pos = 0;
        let mut output = String::new();

        let mut ctx = DecompileContext::for_prolog(&code, &mut pos, &mut output);

        assert_eq!(ctx.peek(), Some(1));
        assert_eq!(ctx.read(), Some(1));
        assert_eq!(ctx.read(), Some(2));

        ctx.write("hello");
        ctx.indent();
        ctx.newline();
        ctx.write("world");

        assert!(output.contains("hello"));
        assert!(output.contains("world"));
    }

    #[test]
    fn decompile_context_mode() {
        let code = vec![1];
        let mut pos = 0;
        let mut output = String::new();

        // Test prolog mode
        let ctx = DecompileContext::for_prolog(&code, &mut pos, &mut output);
        assert_eq!(ctx.mode(), DecompileMode::Prolog);
        assert_eq!(ctx.cmd(), None);

        // Test call mode
        let mut pos2 = 0;
        let mut output2 = String::new();
        let ctx2 = DecompileContext::for_call(&code, &mut pos2, &mut output2, 42);
        assert_eq!(ctx2.mode(), DecompileMode::Call(42));
        assert_eq!(ctx2.cmd(), Some(42));
    }
}
