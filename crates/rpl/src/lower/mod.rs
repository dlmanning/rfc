//! Lowerer for converting IR to bytecode.
//!
//! Takes IR nodes and produces WASM-aligned bytecode.
//! Uses type inference to emit optimal typed opcodes when possible.
//!
//! # Bytecode Format
//!
//! ## MakeProgram
//!
//! ```text
//! MakeProgram <string_count:leb128>
//!             [<str_len:leb128> <str_bytes>]*
//!             <code_len:leb128> <code_bytes>
//!             <span_count:u16>
//!             [<bc_offset:u16> <src_start:leb128> <src_end:leb128>]*
//! ```
//!
//! ## CallLib
//!
//! ```text
//! CallLib <lib_id:u16> <cmd_id:u16>
//! ```
//!
//! ## Control Flow
//!
//! ```text
//! Block 0x40          ; empty block type
//! Loop 0x40           ; empty block type
//! If 0x40             ; empty block type
//! TryTable 0x40 <clause_count:leb128> [<catch_kind:u8> <label:leb128>]*
//! ```

use crate::core::{Interner, Span, TypeId};

use crate::{
    ir::{AtomKind, CompositeKind, Node, NodeKind},
    libs::StackEffect,
    registry::Registry,
    types::{CStack, CType},
    vm::bytecode::{CatchKind, Opcode, write_leb128_i64, write_leb128_u32, write_u16},
};

/// Macro for generating simple opcode emitter methods.
macro_rules! emit_simple {
    ($($name:ident => $opcode:ident),* $(,)?) => {
        $(
            #[doc = concat!("Emit ", stringify!($opcode), ".")]
            #[inline]
            pub fn $name(&mut self) {
                self.emit_opcode(Opcode::$opcode);
            }
        )*
    };
}

/// Compiled program with debug information.
///
/// Contains both the bytecode and source mapping information
/// for debugging support.
#[derive(Clone, Debug, Default)]
pub struct CompiledProgram {
    /// The bytecode.
    pub code: Vec<u8>,
    /// Source spans corresponding to bytecode positions.
    /// `spans[i]` is the source span for bytecode starting at `span_offsets[i]`.
    pub spans: Vec<Span>,
    /// Bytecode offsets where new spans start.
    /// Parallel array with `spans`.
    pub span_offsets: Vec<usize>,
    /// String table (data section) for local variable names, etc.
    /// Referenced by index from bytecode.
    pub string_table: Vec<String>,
    /// Number of parameters (0 for regular programs, N for functions).
    pub param_count: u16,
}

impl CompiledProgram {
    /// Create an empty compiled program.
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            spans: Vec::new(),
            span_offsets: Vec::new(),
            string_table: Vec::new(),
            param_count: 0,
        }
    }

    /// Get the source offset for a bytecode PC.
    ///
    /// Returns the start offset of the span containing this PC,
    /// or None if no span covers this PC.
    pub fn source_offset_for_pc(&self, pc: usize) -> Option<u32> {
        // Binary search to find the span containing this PC
        let idx = self.span_offsets.partition_point(|&o| o <= pc);
        if idx > 0 {
            Some(self.spans[idx - 1].start().offset())
        } else {
            None
        }
    }

    /// Get the span for a bytecode PC.
    #[must_use]
    pub fn span_for_pc(&self, pc: usize) -> Option<Span> {
        let idx = self.span_offsets.partition_point(|&o| o <= pc);
        // Bounds check: idx - 1 must be a valid index into spans
        if idx > 0 && idx <= self.spans.len() {
            Some(self.spans[idx - 1])
        } else {
            None
        }
    }
}

/// Lower error with optional location information.
#[derive(Clone, Debug, Default)]
pub struct LowerError {
    /// Human-readable error message.
    pub message: String,
    /// Optional source span where the error occurred.
    #[doc(hidden)]
    pub span: Option<Span>,
}

impl LowerError {
    /// Create a simple error with just a message.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }

    /// Create an error with location information.
    pub fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
        }
    }
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for LowerError {}

/// Bytecode output buffer with span tracking.
pub struct BytecodeBuffer {
    code: Vec<u8>,
    /// Source spans for debug info.
    spans: Vec<Span>,
    /// Bytecode offsets where spans start.
    span_offsets: Vec<usize>,
    /// Current span being emitted.
    current_span: Option<Span>,
    /// String table (data section) for local variable names.
    string_table: Vec<String>,
    /// Number of parameters (0 for regular programs, N for functions).
    param_count: u16,
}

impl Default for BytecodeBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl BytecodeBuffer {
    /// Create a new empty buffer.
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacity(256)
    }

    /// Create a buffer with pre-allocated capacity.
    #[must_use]
    pub fn with_capacity(code_capacity: usize) -> Self {
        Self {
            code: Vec::with_capacity(code_capacity),
            spans: Vec::with_capacity(code_capacity / 8),
            span_offsets: Vec::with_capacity(code_capacity / 8),
            current_span: None,
            string_table: Vec::new(),
            param_count: 0,
        }
    }

    /// Set the parameter count for this program (making it a function).
    pub fn set_param_count(&mut self, count: u16) {
        self.param_count = count;
    }

    /// Intern a string in the string table, returning its index.
    /// If the string already exists, returns the existing index.
    pub fn intern_string(&mut self, s: &str) -> u32 {
        if let Some(idx) = self.string_table.iter().position(|existing| existing == s) {
            idx as u32
        } else {
            let idx = self.string_table.len() as u32;
            self.string_table.push(s.to_string());
            idx
        }
    }

    /// Set the current span for subsequent emissions.
    pub fn set_span(&mut self, span: Span) {
        // Only record if span changed and is not dummy
        if span != Span::DUMMY && self.current_span != Some(span) {
            self.current_span = Some(span);
            self.spans.push(span);
            self.span_offsets.push(self.code.len());
        }
    }

    /// Get current position.
    #[inline]
    #[must_use]
    pub fn position(&self) -> usize {
        self.code.len()
    }

    /// Emit a raw byte.
    #[inline]
    pub fn emit_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }

    /// Emit an opcode.
    #[inline]
    pub fn emit_opcode(&mut self, op: Opcode) {
        self.code.push(op.as_byte());
    }

    /// Emit an i64 constant.
    pub fn emit_i64_const(&mut self, value: i64) {
        self.emit_opcode(Opcode::I64Const);
        write_leb128_i64(value, &mut self.code);
    }

    /// Emit an f64 constant.
    pub fn emit_f64_const(&mut self, value: f64) {
        self.emit_opcode(Opcode::F64Const);
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    /// Emit a library call.
    pub fn emit_call_lib(&mut self, lib_id: u16, cmd_id: u16) {
        self.emit_opcode(Opcode::CallLib);
        write_u16(lib_id, &mut self.code);
        write_u16(cmd_id, &mut self.code);
    }

    /// Emit local.get.
    pub fn emit_local_get(&mut self, index: u32) {
        self.emit_opcode(Opcode::LocalGet);
        write_leb128_u32(index, &mut self.code);
    }

    /// Emit local.set.
    pub fn emit_local_set(&mut self, index: u32) {
        self.emit_opcode(Opcode::LocalSet);
        write_leb128_u32(index, &mut self.code);
    }

    /// Emit local.tee (set and keep on stack).
    pub fn emit_local_tee(&mut self, index: u32) {
        self.emit_opcode(Opcode::LocalTee);
        write_leb128_u32(index, &mut self.code);
    }

    /// Emit a string constant.
    pub fn emit_string_const(&mut self, s: &str) {
        self.emit_opcode(Opcode::StringConst);
        let bytes = s.as_bytes();
        write_leb128_u32(bytes.len() as u32, &mut self.code);
        self.code.extend_from_slice(bytes);
    }

    /// Emit make_list.
    pub fn emit_make_list(&mut self, count: u32) {
        self.emit_opcode(Opcode::MakeList);
        write_leb128_u32(count, &mut self.code);
    }

    /// Emit make_program with embedded string table and source map.
    /// Format: MakeProgram <param_count:leb128> <string_count> [<str_len> <str_bytes>]* <code_len> <code_bytes>
    ///         <span_count> [<bytecode_offset:u16> <source_start:u32> <source_end:u32>]*
    pub fn emit_make_program(&mut self, program: &CompiledProgram) {
        self.emit_opcode(Opcode::MakeProgram);
        // Write param count (0 for regular programs, N for functions)
        write_leb128_u32(program.param_count as u32, &mut self.code);
        // Write string table
        write_leb128_u32(program.string_table.len() as u32, &mut self.code);
        for s in &program.string_table {
            let bytes = s.as_bytes();
            write_leb128_u32(bytes.len() as u32, &mut self.code);
            self.code.extend_from_slice(bytes);
        }
        // Write code
        write_leb128_u32(program.code.len() as u32, &mut self.code);
        self.code.extend_from_slice(&program.code);

        // Write source map spans for debugging
        write_u16(program.spans.len() as u16, &mut self.code);
        for (i, span) in program.spans.iter().enumerate() {
            // bytecode offset (u16 - nested programs are typically small)
            write_u16(program.span_offsets[i] as u16, &mut self.code);
            // source start/end (u32)
            write_leb128_u32(span.start().offset(), &mut self.code);
            write_leb128_u32(span.end().offset(), &mut self.code);
        }
    }

    /// Emit symbolic_const.
    pub fn emit_symbolic_const(&mut self, expr_str: &str) {
        self.emit_opcode(Opcode::SymbolicConst);
        let bytes = expr_str.as_bytes();
        write_leb128_u32(bytes.len() as u32, &mut self.code);
        self.code.extend_from_slice(bytes);
    }

    /// Emit eval_name (runtime variable lookup).
    pub fn emit_eval_name(&mut self, name: &str) {
        self.emit_opcode(Opcode::EvalName);
        let bytes = name.as_bytes();
        write_leb128_u32(bytes.len() as u32, &mut self.code);
        self.code.extend_from_slice(bytes);
    }

    /// Emit push_local_scope (for symbolic eval with locals).
    /// Names are stored in the string table and referenced by index.
    pub fn emit_push_local_scope(&mut self, names_and_indices: &[(String, u32)]) {
        self.emit_opcode(Opcode::PushLocalScope);
        write_leb128_u32(names_and_indices.len() as u32, &mut self.code);
        for (name, local_idx) in names_and_indices {
            let string_idx = self.intern_string(name);
            write_leb128_u32(string_idx, &mut self.code);
            write_leb128_u32(*local_idx, &mut self.code);
        }
    }

    // === Simple opcodes (generated by macro) ===

    emit_simple! {
        // Parametric
        emit_drop => Drop,
        // i64 arithmetic
        emit_i64_add => I64Add,
        emit_i64_sub => I64Sub,
        emit_i64_mul => I64Mul,
        emit_i64_div_s => I64DivS,
        // f64 arithmetic
        emit_f64_add => F64Add,
        emit_f64_sub => F64Sub,
        emit_f64_mul => F64Mul,
        emit_f64_div => F64Div,
        emit_f64_neg => F64Neg,
        // i64 comparison
        emit_i64_eq => I64Eq,
        emit_i64_ne => I64Ne,
        emit_i64_lt_s => I64LtS,
        emit_i64_le_s => I64LeS,
        emit_i64_gt_s => I64GtS,
        emit_i64_ge_s => I64GeS,
        emit_i64_eqz => I64Eqz,
        // f64 comparison
        emit_f64_eq => F64Eq,
        emit_f64_ne => F64Ne,
        emit_f64_lt => F64Lt,
        emit_f64_le => F64Le,
        emit_f64_gt => F64Gt,
        emit_f64_ge => F64Ge,
        // Conversions
        emit_f64_convert_i64_s => F64ConvertI64S,
        emit_i64_trunc_f64_s => I64TruncF64S,
        // Control flow (simple)
        emit_else => Else,
        emit_end => End,
        emit_pop_local_scope => PopLocalScope,
    }

    // === Control flow (with block type) ===

    /// Emit block with empty type.
    #[inline]
    pub fn emit_block(&mut self) {
        self.emit_opcode(Opcode::Block);
        self.emit_byte(0x40); // Empty block type
    }

    /// Emit loop with empty type.
    #[inline]
    pub fn emit_loop(&mut self) {
        self.emit_opcode(Opcode::Loop);
        self.emit_byte(0x40); // Empty block type
    }

    /// Emit if with empty type.
    #[inline]
    pub fn emit_if(&mut self) {
        self.emit_opcode(Opcode::If);
        self.emit_byte(0x40); // Empty block type
    }

    /// Emit br (unconditional branch).
    pub fn emit_br(&mut self, depth: u32) {
        self.emit_opcode(Opcode::Br);
        write_leb128_u32(depth, &mut self.code);
    }

    /// Emit br_if (conditional branch).
    pub fn emit_br_if(&mut self, depth: u32) {
        self.emit_opcode(Opcode::BrIf);
        write_leb128_u32(depth, &mut self.code);
    }

    /// Emit try_table with a single catch_all clause.
    ///
    /// The catch_all branches to the specified label depth when any exception is thrown.
    /// Format: try_table blocktype clause_count [catch_kind label]* ... end
    pub fn emit_try_table_catch_all(&mut self, label: u32) {
        self.emit_opcode(Opcode::TryTable);
        self.emit_byte(0x40); // Empty block type
        write_leb128_u32(1, &mut self.code); // 1 catch clause
        self.emit_byte(CatchKind::CatchAll.as_byte()); // catch_all
        write_leb128_u32(label, &mut self.code); // branch target
    }

    /// Emit try_table with a single catch_all_ref clause.
    ///
    /// Like catch_all, but pushes the exnref onto the stack when triggered.
    pub fn emit_try_table_catch_all_ref(&mut self, label: u32) {
        self.emit_opcode(Opcode::TryTable);
        self.emit_byte(0x40); // Empty block type
        write_leb128_u32(1, &mut self.code); // 1 catch clause
        self.emit_byte(CatchKind::CatchAllRef.as_byte()); // catch_all_ref
        write_leb128_u32(label, &mut self.code); // branch target
    }

    /// Emit throw instruction with a tag index.
    pub fn emit_throw(&mut self, tag: u32) {
        self.emit_opcode(Opcode::Throw);
        write_leb128_u32(tag, &mut self.code);
    }

    /// Finish and return just the bytecode (for nested programs).
    #[must_use]
    pub fn finish_code(self) -> Vec<u8> {
        self.code
    }

    /// Finish and return the compiled program with debug info.
    #[must_use]
    pub fn finish(self) -> CompiledProgram {
        CompiledProgram {
            code: self.code,
            spans: self.spans,
            span_offsets: self.span_offsets,
            string_table: self.string_table,
            param_count: self.param_count,
        }
    }
}

/// Number of scratch locals pre-allocated for stack operations.
pub const SCRATCH_LOCAL_COUNT: u32 = 3;

/// Lower context.
pub struct LowerContext<'a> {
    pub output: BytecodeBuffer,
    registry: &'a Registry,
    interner: &'a Interner,
    /// Type stack for tracking types during lowering.
    pub types: CStack,
    /// Next local index to allocate (after scratch locals).
    next_local: u32,
    /// Base index for scratch locals (0, 1, 2).
    scratch_base: u32,
    /// Tracked types for local variables (indexed by local index).
    local_types: Vec<CType>,
    /// Mapping from parsed user local indices to actual local indices.
    /// This avoids conflicts between user locals and dynamically allocated locals.
    user_local_map: HashMap<u32, u32>,
}

use std::collections::HashMap;

impl<'a> LowerContext<'a> {
    /// Create a new lower context.
    /// Pre-allocates scratch locals for stack operations.
    pub fn new(registry: &'a Registry, interner: &'a Interner) -> Self {
        // Pre-allocate scratch locals as Unknown type
        let mut local_types = Vec::new();
        for _ in 0..SCRATCH_LOCAL_COUNT {
            local_types.push(CType::Unknown);
        }
        Self {
            output: BytecodeBuffer::new(),
            registry,
            interner,
            types: CStack::new(),
            // Reserve first 3 locals for scratch (stack ops)
            next_local: SCRATCH_LOCAL_COUNT,
            scratch_base: 0,
            local_types,
            user_local_map: HashMap::new(),
        }
    }

    /// Get the interner.
    pub fn interner(&self) -> &Interner {
        self.interner
    }

    /// Get a scratch local index (0, 1, or 2).
    /// These are pre-allocated and reused by stack operations.
    #[must_use]
    pub fn scratch(&self, n: u32) -> u32 {
        debug_assert!(n < SCRATCH_LOCAL_COUNT);
        self.scratch_base + n
    }

    /// Convert a user-defined local index (from parsing) to the actual local index.
    /// Uses a mapping to avoid conflicts between user locals and dynamically allocated locals.
    /// Each unique parsed index gets its own allocated local on first use.
    pub fn user_local(&mut self, parsed_idx: u32) -> u32 {
        // Use entry API to avoid double lookup
        *self.user_local_map.entry(parsed_idx).or_insert_with(|| {
            let idx = self.next_local;
            self.next_local += 1;
            self.local_types.push(CType::Unknown);
            idx
        })
    }

    /// Allocate a new local and return its index.
    /// The local starts with Unknown type until a value is stored.
    #[must_use]
    pub fn alloc_local(&mut self) -> u32 {
        let idx = self.next_local;
        self.next_local += 1;
        self.local_types.push(CType::Unknown);
        idx
    }

    /// Allocate multiple locals and return the starting index.
    #[must_use]
    pub fn alloc_locals(&mut self, count: u32) -> u32 {
        let start = self.next_local;
        self.next_local += count;
        for _ in 0..count {
            self.local_types.push(CType::Unknown);
        }
        start
    }

    /// Get the tracked type of a local variable.
    #[must_use]
    pub fn local_type(&self, index: u32) -> CType {
        self.local_types
            .get(index as usize)
            .cloned()
            .unwrap_or(CType::Unknown)
    }

    /// Set the tracked type of a local variable.
    pub fn set_local_type(&mut self, index: u32, ty: CType) {
        let idx = index as usize;
        if idx >= self.local_types.len() {
            self.local_types.resize(idx + 1, CType::Unknown);
        }
        self.local_types[idx] = ty;
    }

    /// Get the number of locals allocated so far (including scratch).
    #[must_use]
    pub fn local_count(&self) -> u32 {
        self.next_local
    }

    // === Type-directed emit helpers for libraries ===
    //
    // These methods emit code based on current type stack state and return
    // the StackEffect. They do NOT modify the type stack - the caller applies
    // the returned effect.

    /// Coerce mixed int/real operands to real.
    ///
    /// Returns `true` if coercion occurred (either operand was converted).
    fn coerce_to_real_if_mixed(&mut self) -> bool {
        let (tos, nos) = (self.types.top(), self.types.nos());

        if tos.is_integer() && nos.is_real() {
            // TOS is int, NOS is real - convert TOS to real
            self.output.emit_f64_convert_i64_s();
            true
        } else if tos.is_real() && nos.is_integer() {
            // TOS is real, NOS is int - need to convert NOS
            // Save TOS, convert NOS, restore TOS
            let tmp = self.scratch(0);
            self.output.emit_local_set(tmp);
            self.output.emit_f64_convert_i64_s();
            self.output.emit_local_get(tmp);
            true
        } else {
            false
        }
    }

    /// Emit a binary numeric operation, choosing optimal opcode based on operand types.
    /// Returns the stack effect to be applied by the caller.
    ///
    /// - If both operands are integers: emits int_op, returns Integer result
    /// - If both are reals: emits real_op, returns Real result
    /// - If mixed: coerces to real, emits real_op, returns Real result
    /// - Otherwise: emits CallLib, returns Unknown result
    pub fn emit_binary_numeric(
        &mut self,
        int_op: Opcode,
        real_op: Opcode,
        lib: u16,
        cmd: u16,
    ) -> StackEffect {
        let (tos, nos) = (self.types.top(), self.types.nos());
        let both_int = tos.is_integer() && nos.is_integer();
        let both_real = tos.is_real() && nos.is_real();

        let coerced = self.coerce_to_real_if_mixed();

        if both_int {
            self.output.emit_opcode(int_op);
            StackEffect::fixed(2, &[Some(TypeId::BINT)])
        } else if both_real || coerced {
            self.output.emit_opcode(real_op);
            StackEffect::fixed(2, &[Some(TypeId::REAL)])
        } else {
            self.output.emit_call_lib(lib, cmd);
            StackEffect::fixed(2, &[None])
        }
    }

    /// Emit a binary comparison operation, choosing optimal opcode based on operand types.
    /// Always returns Integer result (0 or 1).
    pub fn emit_binary_comparison(
        &mut self,
        int_op: Opcode,
        real_op: Opcode,
        lib: u16,
        cmd: u16,
    ) -> StackEffect {
        let (tos, nos) = (self.types.top(), self.types.nos());
        let both_int = tos.is_integer() && nos.is_integer();
        let both_real = tos.is_real() && nos.is_real();

        let coerced = self.coerce_to_real_if_mixed();

        if both_int {
            self.output.emit_opcode(int_op);
        } else if both_real || coerced {
            self.output.emit_opcode(real_op);
        } else {
            self.output.emit_call_lib(lib, cmd);
        }

        // Comparisons always produce integer
        StackEffect::fixed(2, &[Some(TypeId::BINT)])
    }

    /// Emit a unary numeric operation.
    /// Returns the stack effect to be applied by the caller.
    pub fn emit_unary_numeric(
        &mut self,
        int_op: Option<Opcode>,
        real_op: Opcode,
        lib: u16,
        cmd: u16,
    ) -> StackEffect {
        let tos = self.types.top();

        if tos.is_real() {
            self.output.emit_opcode(real_op);
            StackEffect::fixed(1, &[Some(TypeId::REAL)])
        } else if tos.is_integer() {
            if let Some(op) = int_op {
                self.output.emit_opcode(op);
            } else {
                self.output.emit_call_lib(lib, cmd);
            }
            StackEffect::fixed(1, &[Some(TypeId::BINT)])
        } else {
            self.output.emit_call_lib(lib, cmd);
            StackEffect::fixed(1, &[None])
        }
    }

    /// Push a known integer type onto the type stack.
    pub fn push_type_integer(&mut self) {
        self.types.push(CType::integer());
    }

    /// Push a known real type onto the type stack.
    pub fn push_type_real(&mut self) {
        self.types.push(CType::real());
    }

    /// Push an unknown type onto the type stack.
    pub fn push_type_unknown(&mut self) {
        self.types.push(CType::Unknown);
    }

    /// Pop a type from the type stack.
    pub fn pop_type(&mut self) {
        self.types.pop();
    }

    /// Apply a binary integer operation (2 ints -> 1 int).
    ///
    /// Emits the opcode and updates the type stack.
    pub fn emit_i64_binop(&mut self, op: Opcode) {
        self.output.emit_opcode(op);
        self.types.apply_effect(&StackEffect::fixed(2, &[Some(crate::core::TypeId::BINT)]));
    }

    /// Pop the top value for br_if (condition is consumed).
    ///
    /// Use this after emitting br_if to track that the condition was consumed.
    pub fn consume_br_condition(&mut self) {
        self.pop_type();
    }

    /// Mark that the type stack depth is unknown (dynamic operation).
    pub fn mark_unknown_depth(&mut self) {
        self.types.mark_unknown_depth();
    }

    /// Emit local.get with a specific type.
    pub fn emit_local_get_typed(&mut self, index: u32, ty: CType) {
        self.output.emit_local_get(index);
        self.types.push(ty);
    }

    /// Emit local.get with known integer type.
    pub fn emit_local_get_int(&mut self, index: u32) {
        self.emit_local_get_typed(index, CType::integer());
    }

    /// Emit local.set (pops type from stack and records it for the local).
    pub fn emit_local_set(&mut self, index: u32) {
        let ty = self.types.top();
        self.set_local_type(index, ty);
        self.output.emit_local_set(index);
        self.pop_type();
    }

    /// Emit local.get using the tracked type of the local.
    pub fn emit_local_get_tracked(&mut self, index: u32) {
        let ty = self.local_type(index);
        self.emit_local_get_typed(index, ty);
    }

    /// Emit i64.const with type tracking.
    pub fn emit_i64_const(&mut self, value: i64) {
        self.output.emit_i64_const(value);
        self.push_type_integer();
    }

    /// Emit f64.const with type tracking.
    pub fn emit_f64_const(&mut self, value: f64) {
        self.output.emit_f64_const(value);
        self.push_type_real();
    }

    /// Lower a sequence of nodes.
    pub fn lower_all(&mut self, nodes: &[Node]) -> Result<(), LowerError> {
        for node in nodes {
            self.lower(node)?;
        }
        Ok(())
    }

    /// Lower a single node.
    pub fn lower(&mut self, node: &Node) -> Result<(), LowerError> {
        // Record span for debug info
        self.output.set_span(node.span);

        match &node.kind {
            NodeKind::Atom(atom) => self.lower_atom(atom),
            NodeKind::Composite(kind, branches) => self.lower_composite(*kind, branches, node.span),
        }
    }

    /// Lower an atom.
    fn lower_atom(&mut self, atom: &AtomKind) -> Result<(), LowerError> {
        match atom {
            AtomKind::Integer(n) => {
                self.output.emit_i64_const(*n);
                self.types.push(CType::integer());
            }
            AtomKind::Real(n) => {
                self.output.emit_f64_const(*n);
                self.types.push(CType::real());
            }
            AtomKind::String(s) => {
                self.output.emit_string_const(s);
                self.types.push(CType::string());
            }
            AtomKind::LocalRef(idx) => {
                // User locals are offset by SCRATCH_LOCAL_COUNT to avoid conflict with scratch
                let actual_idx = self.user_local(*idx as u32);
                self.emit_local_get_tracked(actual_idx);
            }
            AtomKind::GlobalRef(sym) => {
                // Resolved global reference - still uses runtime lookup via directory
                let name = self.interner.resolve(*sym);
                self.output.emit_eval_name(name);
                self.types.push(CType::Unknown); // Result type unknown until runtime
            }
            AtomKind::Symbol(sym) => {
                // Resolve symbol to name and emit eval_name
                let name = self.interner.resolve(*sym);
                self.output.emit_eval_name(name);
                self.types.push(CType::Unknown); // Result type unknown until runtime
            }
            AtomKind::Command(lib, cmd) => {
                self.lower_command(*lib, *cmd)?;
            }
            AtomKind::Symbolic(expr) => {
                // Serialize to string and emit as symbolic constant
                let expr_str = format!("{}", expr);
                self.output.emit_symbolic_const(&expr_str);
                self.types.push(CType::symbolic());
            }
        }
        Ok(())
    }

    /// Lower a command by dispatching to the library lowerer.
    ///
    /// The library emits bytecode and returns the stack effect,
    /// which is then applied to the type stack.
    pub fn lower_command(&mut self, lib: u16, cmd: u16) -> Result<(), LowerError> {
        if let Some(lowerer) = self.registry.get_lowerer(lib) {
            let effect = lowerer.lower_command(cmd, crate::core::Span::default(), self)?;
            self.types.apply_effect(&effect);
            Ok(())
        } else {
            // Unknown library - emit call and mark types unknown
            self.output.emit_call_lib(lib, cmd);
            self.types.mark_unknown_depth();
            Ok(())
        }
    }

    /// Lower a composite structure.
    fn lower_composite(
        &mut self,
        kind: CompositeKind,
        branches: &[Vec<Node>],
        span: crate::core::Span,
    ) -> Result<(), LowerError> {
        match kind {
            CompositeKind::Program => {
                // Compile the program body to separate bytecode with its own string table
                if let Some(body) = branches.first() {
                    let nested_program = lower_to_program(body, self.registry, self.interner)?;
                    self.output.emit_make_program(&nested_program);
                } else {
                    self.output.emit_make_program(&CompiledProgram::new());
                }
                // Update type stack: MakeProgram pushes a program value
                self.types.push(CType::program());
            }
            CompositeKind::List => {
                // Lower all items, then emit make_list
                // Track stack depth to handle expressions that produce different counts
                let depth_before = self.types.depth();
                if let Some(items) = branches.first() {
                    for item in items {
                        self.lower(item)?;
                    }
                }
                let depth_after = self.types.depth();
                let count = depth_after.saturating_sub(depth_before) as u32;
                self.output.emit_make_list(count);
                // Update type stack: pop the items, push a list
                for _ in 0..count {
                    self.types.pop();
                }
                self.types.push(CType::list());
            }
            CompositeKind::Extended(lib, id) => {
                // Dispatch to library lowerer
                if let Some(lowerer) = self.registry.get_lowerer(lib) {
                    lowerer.lower_composite(id, branches, span, self)?;
                } else {
                    return Err(LowerError {
                        message: format!("no lowerer registered for library {}", lib),
                        span: None,
                    });
                }
            }
        }
        Ok(())
    }

    /// Finish and return the compiled program with debug info.
    #[must_use]
    pub fn finish(self) -> CompiledProgram {
        self.output.finish()
    }

    /// Finish and return just the bytecode (for nested programs).
    #[must_use]
    pub fn finish_code(self) -> Vec<u8> {
        self.output.finish_code()
    }
}

/// Lower IR nodes to a compiled program with debug info.
pub fn lower(
    nodes: &[Node],
    registry: &Registry,
    interner: &Interner,
) -> Result<CompiledProgram, LowerError> {
    let mut ctx = LowerContext::new(registry, interner);
    ctx.lower_all(nodes)?;
    Ok(ctx.finish())
}

/// Lower IR nodes to bytecode and string table (for nested programs).
pub fn lower_to_program(
    nodes: &[Node],
    registry: &Registry,
    interner: &Interner,
) -> Result<CompiledProgram, LowerError> {
    let mut ctx = LowerContext::new(registry, interner);
    ctx.lower_all(nodes)?;
    Ok(ctx.finish())
}

#[cfg(test)]
mod tests {
    use crate::core::{Pos, Span};

    use super::*;
    use crate::{
        ir::Node,
        libs::arith::{self, ARITH_LIB},
    };

    fn dummy_span() -> Span {
        Span::new(Pos::new(0), Pos::new(1))
    }

    fn test_registry() -> Registry {
        Registry::with_core()
    }

    fn test_interner() -> Interner {
        Interner::new()
    }

    #[test]
    fn lower_integer() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::integer(42, dummy_span())];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should be: I64Const 42
        assert_eq!(program.code[0], Opcode::I64Const.as_byte());
    }

    #[test]
    fn lower_command() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::command(0, 10, dummy_span())]; // ADD
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should be: CallLib 0 10
        assert_eq!(program.code[0], Opcode::CallLib.as_byte());
    }

    #[test]
    fn lower_sequence() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![
            Node::integer(1, dummy_span()),
            Node::integer(2, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::ADD, dummy_span()),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // With type inference, integer + integer emits I64Add directly
        // Should be: I64Const 1, I64Const 2, I64Add
        assert_eq!(program.code[0], Opcode::I64Const.as_byte());
        // Should contain I64Add (typed opcode) instead of CallLib
        assert!(program.code.contains(&Opcode::I64Add.as_byte()));
    }

    #[test]
    fn lower_list() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::list(
            vec![
                Node::integer(1, dummy_span()),
                Node::integer(2, dummy_span()),
                Node::integer(3, dummy_span()),
            ],
            dummy_span(),
        )];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should have: I64Const 1, I64Const 2, I64Const 3, MakeList 3
        assert!(program.code.len() > 6);
        // Last instruction should be MakeList
        // Find MakeList opcode
        assert!(program.code.contains(&Opcode::MakeList.as_byte()));
    }

    #[test]
    fn lower_empty_list() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::list(vec![], dummy_span())];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should be: MakeList 0
        assert_eq!(program.code[0], Opcode::MakeList.as_byte());
        assert_eq!(program.code[1], 0); // count = 0
    }

    #[test]
    fn lower_program() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::program(
            vec![
                Node::integer(1, dummy_span()),
                Node::command(0, 10, dummy_span()), // ADD
            ],
            dummy_span(),
        )];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should be: MakeProgram <len> <bytecode>
        assert_eq!(program.code[0], Opcode::MakeProgram.as_byte());
    }

    #[test]
    fn lower_empty_program() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![Node::program(vec![], dummy_span())];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should be: MakeProgram 0
        assert_eq!(program.code[0], Opcode::MakeProgram.as_byte());
        assert_eq!(program.code[1], 0); // length = 0
    }

    // === Type-directed lowering tests ===

    #[test]
    fn typed_integer_add() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![
            Node::integer(1, dummy_span()),
            Node::integer(2, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::ADD, dummy_span()),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should emit I64Add for integer+integer
        assert!(program.code.contains(&Opcode::I64Add.as_byte()));
        assert!(!program.code.contains(&Opcode::CallLib.as_byte()));
    }

    #[test]
    fn typed_real_add() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![
            Node::real(1.5, dummy_span()),
            Node::real(2.5, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::ADD, dummy_span()),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should emit F64Add for real+real
        assert!(program.code.contains(&Opcode::F64Add.as_byte()));
    }

    #[test]
    fn typed_integer_compare() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![
            Node::integer(5, dummy_span()),
            Node::integer(3, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::GT, dummy_span()),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should emit I64GtS for integer comparison
        assert!(program.code.contains(&Opcode::I64GtS.as_byte()));
    }

    #[test]
    fn typed_integer_mul_chain() {
        let reg = test_registry();
        let interner = test_interner();
        let nodes = vec![
            Node::integer(2, dummy_span()),
            Node::integer(3, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::MUL, dummy_span()),
            Node::integer(4, dummy_span()),
            Node::command(ARITH_LIB, arith::cmd::MUL, dummy_span()),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Both MULs use I64Mul since emit_mul tracks result type based on inputs
        let mul_count = program
            .code
            .iter()
            .filter(|&&b| b == Opcode::I64Mul.as_byte())
            .count();
        assert_eq!(mul_count, 2);
    }

    #[test]
    fn span_tracking() {
        let reg = test_registry();
        let interner = test_interner();
        let span1 = Span::new(Pos::new(0), Pos::new(2));
        let span2 = Span::new(Pos::new(3), Pos::new(5));
        let nodes = vec![
            Node::integer(1, span1),
            Node::integer(2, span2),
        ];
        let program = lower(&nodes, &reg, &interner).unwrap();

        // Should have recorded both spans
        assert!(!program.spans.is_empty());
        assert_eq!(program.spans.len(), program.span_offsets.len());
    }
}
