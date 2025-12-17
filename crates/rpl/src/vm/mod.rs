//! Virtual machine for executing WASM-aligned bytecode (v2 - cleaner implementation).
//!
//! The VM executes bytecode produced by the lowerer. It maintains:
//! - A data stack for calculator values
//! - Indexed locals for WASM-style local variables
//! - A control stack for structured control flow
//! - A global directory for named variables

pub mod bytecode;
pub mod debug;
pub mod directory;
pub mod disasm;
pub mod locals;
pub mod stack;

use std::sync::Arc;

use bytecode::{CatchKind, Opcode, read_f64, read_leb128_i64, read_leb128_u32, read_u16};
pub use debug::{DebugEvent, DebugMode, DebugState};
use directory::Directory;
use locals::Locals;
use stack::{Stack, StackError};

use crate::{
    libs::ExecuteContext,
    lower::CompiledProgram,
    registry::Registry,
    value::{ProgramData, Value},
};

// ============================================================================
// Error types
// ============================================================================

/// VM execution error.
#[derive(Clone, Debug)]
pub enum VmError {
    Stack(StackError),
    Locals(locals::LocalsError),
    InvalidOpcode(u8),
    UnexpectedEnd,
    TypeError(String),
    UnknownLibrary(u16),
    UnknownCommand(u16, u16),
    Unreachable,
    InvalidBranch(u32),
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stack(e) => write!(f, "stack error: {}", e),
            Self::Locals(e) => write!(f, "locals error: {}", e),
            Self::InvalidOpcode(op) => write!(f, "invalid opcode: 0x{:02X}", op),
            Self::UnexpectedEnd => write!(f, "unexpected end of bytecode"),
            Self::TypeError(msg) => write!(f, "type error: {}", msg),
            Self::UnknownLibrary(id) => write!(f, "unknown library: {}", id),
            Self::UnknownCommand(lib, cmd) => write!(f, "unknown command: lib={} cmd={}", lib, cmd),
            Self::Unreachable => write!(f, "unreachable instruction"),
            Self::InvalidBranch(depth) => write!(f, "invalid branch depth: {}", depth),
        }
    }
}

impl std::error::Error for VmError {}
impl From<StackError> for VmError {
    fn from(e: StackError) -> Self {
        Self::Stack(e)
    }
}
impl From<locals::LocalsError> for VmError {
    fn from(e: locals::LocalsError) -> Self {
        Self::Locals(e)
    }
}

/// Outcome of debug-aware execution.
#[derive(Clone, Debug)]
pub enum ExecuteOutcome {
    Completed,
    Debug(DebugEvent),
}

/// An RPL exception (for IFERR/THROW).
#[derive(Clone, Debug)]
pub struct RplException {
    pub code: i64,
    pub message: String,
}

impl RplException {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            code: 0,
            message: message.into(),
        }
    }
    pub fn with_code(code: i64, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
        }
    }
}

/// Return stack entry for tracking calls.
#[derive(Clone, Debug)]
pub enum ReturnEntry {
    Call {
        program: Arc<ProgramData>,
        return_pc: usize,
        name: Option<String>,
        saved_locals: Vec<Option<Value>>,
    },
}

// ============================================================================
// VM struct
// ============================================================================

/// The virtual machine.
pub struct Vm {
    pub stack: Stack,
    pub locals: Locals,
    pub directory: Directory,
    pub pc: usize,
    control_stack: Vec<ControlEntry>,
    return_stack: Vec<ReturnEntry>,
    local_name_scopes: Vec<Vec<(String, u32)>>,
    last_error: Option<RplException>,
}

#[derive(Clone, Debug)]
struct ControlEntry {
    kind: ControlKind,
    branch_target: usize,
    end_pos: Option<usize>,
    catch_clauses: Vec<CatchClause>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ControlKind {
    Block,
    Loop,
    If,
    TryTable,
}

#[derive(Clone, Debug)]
struct CatchClause {
    kind: CatchKind,
    label: u32,
}

struct CatchResult {
    target_pc: usize,
}

enum Flow {
    Continue,
    Return,
    Branch(usize),
    DebugPause(DebugEvent),
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            locals: Locals::new(),
            control_stack: Vec::new(),
            directory: Directory::new(),
            return_stack: Vec::new(),
            pc: 0,
            local_name_scopes: Vec::new(),
            last_error: None,
        }
    }

    // --- Public API ---

    pub fn last_error(&self) -> Option<&RplException> {
        self.last_error.as_ref()
    }
    pub fn set_last_error(&mut self, error: RplException) {
        self.last_error = Some(error);
    }
    pub fn clear_last_error(&mut self) {
        self.last_error = None;
    }

    pub fn push_local_names(&mut self, names: Vec<(String, u32)>) {
        self.local_name_scopes.push(names);
    }
    pub fn pop_local_names(&mut self) {
        self.local_name_scopes.pop();
    }

    pub fn lookup_local_by_name(&self, name: &str) -> Option<&Value> {
        for scope in self.local_name_scopes.iter().rev() {
            for (local_name, idx) in scope {
                if local_name == name {
                    return self.locals.try_get(*idx);
                }
            }
        }
        None
    }

    pub fn local_name_for_index(&self, index: u32) -> Option<String> {
        for scope in self.local_name_scopes.iter().rev() {
            for (name, idx) in scope {
                if *idx == index {
                    return Some(name.clone());
                }
            }
        }
        None
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.locals.clear();
        self.control_stack.clear();
        self.return_stack.clear();
        self.pc = 0;
    }

    pub fn stack_contents(&self) -> &[Value] {
        self.stack.as_slice()
    }
    pub fn call_depth(&self) -> usize {
        self.return_stack.len()
    }
    pub fn return_stack(&self) -> &[ReturnEntry] {
        &self.return_stack
    }

    // --- Execution ---

    pub fn execute(
        &mut self,
        code: &[u8],
        registry: &Registry,
        strings: &[String],
    ) -> Result<(), VmError> {
        let saved_pc = self.pc;
        self.pc = 0;
        self.control_stack.clear();

        while self.pc < code.len() {
            match self.step(code, registry, strings, None)? {
                Flow::Continue => {}
                Flow::Return => break,
                Flow::Branch(target) => self.pc = target,
                Flow::DebugPause(_) => unreachable!(),
            }
        }

        self.pc = saved_pc;
        Ok(())
    }

    pub fn execute_debug(
        &mut self,
        program: &CompiledProgram,
        registry: &Registry,
        debug: &mut DebugState,
    ) -> Result<ExecuteOutcome, VmError> {
        // Resume nested calls first
        while !self.return_stack.is_empty() {
            let ReturnEntry::Call {
                program: prog,
                return_pc,
                saved_locals,
                ..
            } = self.return_stack.last().unwrap().clone();

            if let Some(event) =
                self.execute_inner_debug(&prog.code, registry, &prog.strings, debug, |pc| {
                    prog.source_offset_for_pc(pc)
                })?
            {
                return Ok(ExecuteOutcome::Debug(event));
            }

            self.return_stack.pop();
            self.locals.restore(saved_locals);
            self.pc = return_pc;
        }

        // Execute main program
        self.control_stack.clear();
        if let Some(event) = self.execute_inner_debug(
            &program.code,
            registry,
            &program.string_table,
            debug,
            |pc| program.source_offset_for_pc(pc),
        )? {
            return Ok(ExecuteOutcome::Debug(event));
        }

        Ok(ExecuteOutcome::Completed)
    }

    fn execute_inner_debug(
        &mut self,
        code: &[u8],
        registry: &Registry,
        strings: &[String],
        debug: &mut DebugState,
        source_fn: impl Fn(usize) -> Option<u32>,
    ) -> Result<Option<DebugEvent>, VmError> {
        if self.pc == 0 {
            self.control_stack.clear();
        }

        while self.pc < code.len() {
            if let Some(event) = debug.check(self.pc, self.call_depth(), source_fn(self.pc)) {
                return Ok(Some(event));
            }

            match self.step(code, registry, strings, Some(debug))? {
                Flow::Continue => {}
                Flow::Return => break,
                Flow::Branch(target) => self.pc = target,
                Flow::DebugPause(event) => return Ok(Some(event)),
            }

            debug.mark_executed_with_offset(source_fn(self.pc));
        }

        Ok(None)
    }

    // --- Single instruction step ---

    fn step(
        &mut self,
        code: &[u8],
        registry: &Registry,
        strings: &[String],
        debug: Option<&mut DebugState>,
    ) -> Result<Flow, VmError> {
        let op_byte = code[self.pc];
        self.pc += 1;
        let op = Opcode::from_byte(op_byte).ok_or(VmError::InvalidOpcode(op_byte))?;
        self.dispatch(op, code, registry, strings, debug)
    }

    // --- Stack helpers ---

    fn pop_i64(&mut self) -> Result<i64, VmError> {
        match self.stack.pop()? {
            Value::Integer(n) => Ok(n),
            Value::Real(r) => Ok(r as i64),
            v => Err(VmError::TypeError(format!(
                "expected integer, got {}",
                v.type_name()
            ))),
        }
    }

    fn pop_f64(&mut self) -> Result<f64, VmError> {
        match self.stack.pop()? {
            Value::Real(n) => Ok(n),
            v => Err(VmError::TypeError(format!(
                "expected real, got {}",
                v.type_name()
            ))),
        }
    }

    fn pop_two_i64(&mut self) -> Result<(i64, i64), VmError> {
        let b = self.pop_i64()?;
        let a = self.pop_i64()?;
        Ok((a, b))
    }

    fn pop_two_f64(&mut self) -> Result<(f64, f64), VmError> {
        let b = self.pop_f64()?;
        let a = self.pop_f64()?;
        Ok((a, b))
    }

    // --- Control flow helpers ---

    fn branch(&mut self, depth: u32) -> Result<usize, VmError> {
        let depth = depth as usize;
        if depth >= self.control_stack.len() {
            return Err(VmError::InvalidBranch(depth as u32));
        }
        let idx = self.control_stack.len() - 1 - depth;
        let entry = &self.control_stack[idx];
        let target = entry.branch_target;
        if entry.kind != ControlKind::Loop {
            self.control_stack.truncate(idx);
        }
        Ok(target)
    }

    fn find_exception_handler(&mut self) -> Result<Option<CatchResult>, VmError> {
        for i in (0..self.control_stack.len()).rev() {
            let entry = &self.control_stack[i];
            if entry.kind == ControlKind::TryTable {
                for clause in &entry.catch_clauses {
                    if matches!(clause.kind, CatchKind::CatchAll | CatchKind::CatchAllRef) {
                        let target_idx = i.saturating_sub(clause.label as usize);
                        if target_idx < self.control_stack.len() {
                            let target = self.control_stack[target_idx].branch_target;
                            self.control_stack.truncate(i);
                            return Ok(Some(CatchResult { target_pc: target }));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    // --- Call helpers ---

    #[allow(clippy::too_many_arguments)]
    fn call_nested(
        &mut self,
        code: &[u8],
        strings: &[String],
        name: Option<String>,
        program_data: Arc<ProgramData>,
        source_fn: impl Fn(usize) -> Option<u32>,
        registry: &Registry,
        debug: Option<&mut DebugState>,
    ) -> Result<Flow, VmError> {
        // Pop parameters from stack before saving locals (for functions with param_count > 0)
        let params = if program_data.param_count > 0 {
            let mut args = self.stack.pop_many(program_data.param_count as usize)?;
            args.reverse(); // First param should be local 0
            args
        } else {
            Vec::new()
        };

        let saved = self.locals.save();
        self.locals.clear();

        // Bind parameters to locals 0..N
        for (i, param) in params.into_iter().enumerate() {
            self.locals.set(i as u32, param)?;
        }

        self.return_stack.push(ReturnEntry::Call {
            program: program_data,
            return_pc: self.pc,
            name,
            saved_locals: saved.clone(),
        });

        let result = if let Some(debug) = debug {
            let saved_pc = self.pc;
            self.pc = 0;
            match self.execute_inner_debug(code, registry, strings, debug, source_fn)? {
                Some(event) => return Ok(Flow::DebugPause(event)),
                None => {
                    self.pc = saved_pc;
                    Ok(())
                }
            }
        } else {
            self.execute(code, registry, strings)
        };

        self.return_stack.pop();
        self.locals.restore(saved);
        result?;
        Ok(Flow::Continue)
    }

    fn call_library(
        &mut self,
        lib_id: u16,
        cmd_id: u16,
        registry: &Registry,
        debug: Option<&mut DebugState>,
    ) -> Result<Flow, VmError> {
        // Special case: EVAL
        if lib_id == crate::libs::PROG_LIB && cmd_id == crate::libs::prog::cmd::EVAL {
            return self.eval_program(registry, debug);
        }

        let library = registry
            .get(lib_id)
            .ok_or(VmError::UnknownLibrary(lib_id))?;
        let last_error = self.last_error.clone();
        let mut ctx = ExecuteContext::new(&mut self.stack, &mut self.directory, cmd_id, last_error);
        library.execute(&mut ctx).map_err(VmError::TypeError)?;
        Ok(Flow::Continue)
    }

    fn lookup_library_command(&self, name: &str) -> Option<(Vec<u8>, Vec<String>)> {
        let name_upper = name.to_uppercase();
        for (_lib_name, value) in self.directory.vars_at_path(&["SETTINGS", "LIB"]) {
            if let Some(lib_data) = value.as_library() {
                for cmd in &lib_data.commands {
                    if cmd.name == name_upper {
                        return Some((cmd.code.to_vec(), cmd.strings.to_vec()));
                    }
                }
            }
        }
        None
    }

    fn eval_program(
        &mut self,
        registry: &Registry,
        debug: Option<&mut DebugState>,
    ) -> Result<Flow, VmError> {
        let value = self.stack.pop()?;
        match value {
            Value::Program(prog) => {
                let code = prog.code.clone();
                let strings = prog.strings.clone();
                let prog_clone = prog.clone();
                self.call_nested(
                    &code,
                    &strings,
                    None,
                    prog,
                    move |pc| prog_clone.source_offset_for_pc(pc),
                    registry,
                    debug,
                )
            }
            Value::Symbolic(expr) => {
                let result = expr.eval_with_lookup(&|name| {
                    self.lookup_local_by_name(name)
                        .or_else(|| self.directory.lookup(name))
                        .and_then(value_to_f64)
                });
                match result {
                    Ok(n) => {
                        self.stack.push(Value::Real(n))?;
                        Ok(Flow::Continue)
                    }
                    Err(msg) => Err(VmError::TypeError(msg)),
                }
            }
            _ => Err(VmError::TypeError(format!(
                "EVAL expected program or symbolic, got {}",
                value.type_name()
            ))),
        }
    }

    // --- Main dispatch ---

    fn dispatch(
        &mut self,
        op: Opcode,
        code: &[u8],
        registry: &Registry,
        strings: &[String],
        debug: Option<&mut DebugState>,
    ) -> Result<Flow, VmError> {
        use Opcode::*;
        match op {
            Nop => {}
            ThrowRef => return Err(VmError::TypeError("throw_ref not implemented".into())),
            // === Constants ===
            I64Const => {
                let n = read_leb128_i64(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.stack.push(Value::integer(n))?;
            }
            F64Const => {
                let n = read_f64(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.stack.push(Value::Real(n))?;
            }
            StringConst => {
                let s = self.read_string(code)?;
                self.stack.push(Value::string(s))?;
            }
            SymbolicConst => {
                let s = self.read_string(code)?;
                let expr = crate::parse::infix::InfixParser::parse_str(s)
                    .map_err(|e| VmError::TypeError(format!("invalid symbolic: {}", e)))?;
                self.stack.push(Value::symbolic(expr))?;
            }

            // === Locals ===
            LocalGet => {
                let idx = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let val = self.locals.get(idx)?.clone();
                self.stack.push(val)?;
            }
            LocalSet => {
                let idx = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let val = self.stack.pop()?;
                self.locals.set(idx, val)?;
            }
            LocalTee => {
                let idx = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let val = self.stack.top()?.clone();
                self.locals.set(idx, val)?;
            }

            // === Stack ops ===
            Drop => {
                self.stack.pop()?;
            }
            Select => {
                let cond = self.pop_i64()?;
                let else_val = self.stack.pop()?;
                let then_val = self.stack.pop()?;
                self.stack
                    .push(if cond != 0 { then_val } else { else_val })?;
            }

            // === Control flow ===
            Block => {
                let _bt = code.get(self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.pc += 1;
                let end_pos = find_matching_end(code, self.pc)?;
                self.control_stack.push(ControlEntry {
                    kind: ControlKind::Block,
                    branch_target: end_pos,
                    end_pos: Some(end_pos),
                    catch_clauses: vec![],
                });
            }
            Loop => {
                let _bt = code.get(self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.pc += 1;
                let loop_start = self.pc;
                let end_pos = find_matching_end(code, self.pc)?;
                self.control_stack.push(ControlEntry {
                    kind: ControlKind::Loop,
                    branch_target: loop_start,
                    end_pos: Some(end_pos),
                    catch_clauses: vec![],
                });
            }
            If => {
                let _bt = code.get(self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.pc += 1;
                let cond = self.stack.pop()?;
                let (else_pos, end_pos) = find_if_structure(code, self.pc)?;

                if value_is_true(&cond) {
                    self.control_stack.push(ControlEntry {
                        kind: ControlKind::If,
                        branch_target: end_pos,
                        end_pos: Some(end_pos),
                        catch_clauses: vec![],
                    });
                } else if let Some(else_pc) = else_pos {
                    self.pc = else_pc;
                    self.control_stack.push(ControlEntry {
                        kind: ControlKind::If,
                        branch_target: end_pos,
                        end_pos: Some(end_pos),
                        catch_clauses: vec![],
                    });
                } else {
                    self.pc = end_pos;
                }
            }
            Else => {
                if let Some(entry) = self.control_stack.last()
                    && entry.kind == ControlKind::If
                {
                    let end_pos = entry.end_pos.unwrap_or(self.pc);
                    self.control_stack.pop();
                    self.pc = end_pos;
                }
            }
            End => {
                self.control_stack.pop();
            }
            Br => {
                let depth = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                return Ok(Flow::Branch(self.branch(depth)?));
            }
            BrIf => {
                let depth = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let cond = self.stack.pop()?;
                if value_is_true(&cond) {
                    return Ok(Flow::Branch(self.branch(depth)?));
                }
            }
            Return => return Ok(Flow::Return),
            Unreachable => return Err(VmError::Unreachable),

            // === Exception handling ===
            TryTable => {
                let _bt = code.get(self.pc).ok_or(VmError::UnexpectedEnd)?;
                self.pc += 1;
                let clause_count =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let mut clauses = Vec::with_capacity(clause_count as usize);
                for _ in 0..clause_count {
                    let kind_byte = *code.get(self.pc).ok_or(VmError::UnexpectedEnd)?;
                    self.pc += 1;
                    let kind =
                        CatchKind::from_byte(kind_byte).ok_or(VmError::InvalidOpcode(kind_byte))?;
                    if kind.has_tag() {
                        read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                    }
                    let label =
                        read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                    clauses.push(CatchClause { kind, label });
                }
                let end_pos = find_matching_end(code, self.pc)?;
                self.control_stack.push(ControlEntry {
                    kind: ControlKind::TryTable,
                    branch_target: end_pos,
                    end_pos: Some(end_pos),
                    catch_clauses: clauses,
                });
            }
            Throw => {
                let tag_idx = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let error_code = match self.stack.pop()? {
                    Value::Integer(n) => n,
                    v => {
                        return Err(VmError::TypeError(format!(
                            "DOERR expected integer error code, got {}",
                            v.type_name()
                        )));
                    }
                };
                let exception =
                    RplException::with_code(error_code, format!("Error #{}", error_code));
                if let Some(catch) = self.find_exception_handler()? {
                    self.last_error = Some(exception);
                    return Ok(Flow::Branch(catch.target_pc));
                }
                return Err(VmError::TypeError(format!(
                    "Uncaught error #{} (tag {})",
                    error_code, tag_idx
                )));
            }

            // === Calls ===
            CallLib => {
                let lib_id = read_u16(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                let cmd_id = read_u16(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                return self.call_library(lib_id, cmd_id, registry, debug);
            }
            EvalName => {
                let name = self.read_string(code)?;
                if let Some(value) = self.directory.lookup(name).cloned() {
                    if let Value::Program(prog) = value {
                        let code_bytes = prog.code.clone();
                        let prog_strings = prog.strings.clone();
                        let prog_clone = prog.clone();
                        return self.call_nested(
                            &code_bytes,
                            &prog_strings,
                            Some(name.to_string()),
                            prog,
                            move |pc| prog_clone.source_offset_for_pc(pc),
                            registry,
                            debug,
                        );
                    } else {
                        self.stack.push(value)?;
                    }
                } else if let Some((lib_code, lib_strings)) = self.lookup_library_command(name) {
                    let prog = Arc::new(ProgramData::with_strings(
                        lib_code.clone(),
                        lib_strings.clone(),
                    ));
                    return self.call_nested(
                        &lib_code,
                        &lib_strings,
                        Some(name.to_string()),
                        prog,
                        |_| None,
                        registry,
                        debug,
                    );
                } else {
                    return Err(VmError::TypeError(format!("Undefined: {}", name)));
                }
            }

            // === List/Program construction ===
            MakeList => {
                let count =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
                let mut items = self.stack.pop_many(count)?;
                items.reverse();
                self.stack.push(Value::list(items))?;
            }
            MakeProgram => {
                // Read param count (0 for regular programs, N for functions)
                let param_count =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as u16;
                let string_count =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
                let mut prog_strings = Vec::with_capacity(string_count);
                for _ in 0..string_count {
                    prog_strings.push(self.read_string(code)?.to_string());
                }
                let code_len =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
                if self.pc + code_len > code.len() {
                    return Err(VmError::UnexpectedEnd);
                }
                let prog_code = code[self.pc..self.pc + code_len].to_vec();
                self.pc += code_len;

                // Read source map for debugging
                let span_count =
                    read_u16(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;

                let program_data = if span_count > 0 {
                    // Read source map spans
                    let mut offsets = Vec::with_capacity(span_count);
                    let mut spans = Vec::with_capacity(span_count);
                    for _ in 0..span_count {
                        let offset =
                            read_u16(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as u32;
                        let span_start =
                            read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                        let span_end =
                            read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                        offsets.push(offset);
                        spans.push(crate::core::Span::new(
                            crate::core::Pos::new(span_start),
                            crate::core::Pos::new(span_end),
                        ));
                    }
                    let source_map = crate::serialize::SourceMap {
                        source: String::new(), // Source text not embedded in bytecode
                        offsets,
                        spans,
                    };
                    ProgramData::function_with_source_map(prog_code, prog_strings, source_map, param_count)
                } else if param_count > 0 {
                    ProgramData::function(prog_code, prog_strings, param_count)
                } else {
                    ProgramData::with_strings(prog_code, prog_strings)
                };

                self.stack.push(Value::Program(Arc::new(program_data)))?;
            }

            // === Local name scopes ===
            PushLocalScope => {
                let count =
                    read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
                let mut names = Vec::with_capacity(count);
                for _ in 0..count {
                    let str_idx =
                        read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
                    let local_idx =
                        read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)?;
                    let name = strings.get(str_idx).cloned().unwrap_or_default();
                    names.push((name, local_idx));
                }
                self.push_local_names(names);
            }
            PopLocalScope => {
                self.pop_local_names();
            }

            // === i64 comparisons (macro-generated) ===
            I64Eqz => {
                let a = self.pop_i64()?;
                self.stack
                    .push(Value::integer(if a == 0 { 1 } else { 0 }))?;
            }
            I64Eq => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if a == b { 1 } else { 0 }))?;
            }
            I64Ne => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if a != b { 1 } else { 0 }))?;
            }
            I64LtS => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(if a < b { 1 } else { 0 }))?;
            }
            I64LtU => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if (a as u64) < (b as u64) { 1 } else { 0 }))?;
            }
            I64GtS => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(if a > b { 1 } else { 0 }))?;
            }
            I64GtU => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if (a as u64) > (b as u64) { 1 } else { 0 }))?;
            }
            I64LeS => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if a <= b { 1 } else { 0 }))?;
            }
            I64LeU => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if (a as u64) <= (b as u64) { 1 } else { 0 }))?;
            }
            I64GeS => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if a >= b { 1 } else { 0 }))?;
            }
            I64GeU => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(if (a as u64) >= (b as u64) { 1 } else { 0 }))?;
            }

            // === f64 comparisons ===
            F64Eq => {
                let (a, b) = self.pop_two_f64()?;
                self.stack
                    .push(Value::integer(if a == b { 1 } else { 0 }))?;
            }
            F64Ne => {
                let (a, b) = self.pop_two_f64()?;
                self.stack
                    .push(Value::integer(if a != b { 1 } else { 0 }))?;
            }
            F64Lt => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::integer(if a < b { 1 } else { 0 }))?;
            }
            F64Gt => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::integer(if a > b { 1 } else { 0 }))?;
            }
            F64Le => {
                let (a, b) = self.pop_two_f64()?;
                self.stack
                    .push(Value::integer(if a <= b { 1 } else { 0 }))?;
            }
            F64Ge => {
                let (a, b) = self.pop_two_f64()?;
                self.stack
                    .push(Value::integer(if a >= b { 1 } else { 0 }))?;
            }

            // === i64 arithmetic ===
            I64Add => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a.wrapping_add(b)))?;
            }
            I64Sub => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a.wrapping_sub(b)))?;
            }
            I64Mul => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a.wrapping_mul(b)))?;
            }
            I64DivS => {
                let (a, b) = self.pop_two_i64()?;
                if b == 0 {
                    return Err(VmError::TypeError("division by zero".into()));
                }
                self.stack.push(Value::integer(a.wrapping_div(b)))?;
            }
            I64DivU => {
                let (a, b) = self.pop_two_i64()?;
                if b == 0 {
                    return Err(VmError::TypeError("division by zero".into()));
                }
                self.stack
                    .push(Value::integer(((a as u64) / (b as u64)) as i64))?;
            }
            I64RemS => {
                let (a, b) = self.pop_two_i64()?;
                if b == 0 {
                    return Err(VmError::TypeError("division by zero".into()));
                }
                self.stack.push(Value::integer(a.wrapping_rem(b)))?;
            }
            I64RemU => {
                let (a, b) = self.pop_two_i64()?;
                if b == 0 {
                    return Err(VmError::TypeError("division by zero".into()));
                }
                self.stack
                    .push(Value::integer(((a as u64) % (b as u64)) as i64))?;
            }
            I64And => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a & b))?;
            }
            I64Or => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a | b))?;
            }
            I64Xor => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(a ^ b))?;
            }
            I64Shl => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(a.wrapping_shl((b & 63) as u32)))?;
            }
            I64ShrS => {
                let (a, b) = self.pop_two_i64()?;
                self.stack
                    .push(Value::integer(a.wrapping_shr((b & 63) as u32)))?;
            }
            I64ShrU => {
                let (a, b) = self.pop_two_i64()?;
                self.stack.push(Value::integer(
                    (a as u64).wrapping_shr((b & 63) as u32) as i64
                ))?;
            }

            // === f64 arithmetic ===
            F64Add => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::Real(a + b))?;
            }
            F64Sub => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::Real(a - b))?;
            }
            F64Mul => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::Real(a * b))?;
            }
            F64Div => {
                let (a, b) = self.pop_two_f64()?;
                self.stack.push(Value::Real(a / b))?;
            }
            F64Neg => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(-a))?;
            }
            F64Abs => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(a.abs()))?;
            }
            F64Ceil => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(a.ceil()))?;
            }
            F64Floor => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(a.floor()))?;
            }
            F64Trunc => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(a.trunc()))?;
            }
            F64Sqrt => {
                let a = self.pop_f64()?;
                self.stack.push(Value::Real(a.sqrt()))?;
            }

            // === Conversions ===
            F64ConvertI64S => {
                let a = self.pop_i64()?;
                self.stack.push(Value::Real(a as f64))?;
            }
            I64TruncF64S => {
                let a = self.pop_f64()?;
                self.stack.push(Value::integer(a as i64))?;
            }
        }

        Ok(Flow::Continue)
    }

    fn read_string<'a>(&mut self, code: &'a [u8]) -> Result<&'a str, VmError> {
        let len = read_leb128_u32(code, &mut self.pc).ok_or(VmError::UnexpectedEnd)? as usize;
        if self.pc + len > code.len() {
            return Err(VmError::UnexpectedEnd);
        }
        let s = std::str::from_utf8(&code[self.pc..self.pc + len])
            .map_err(|_| VmError::TypeError("invalid UTF-8".into()))?;
        self.pc += len;
        Ok(s)
    }
}

// ============================================================================
// Helper functions
// ============================================================================

fn value_to_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Integer(n) => Some(*n as f64),
        Value::Real(n) => Some(*n),
        _ => None,
    }
}

fn value_is_true(value: &Value) -> bool {
    match value {
        Value::Integer(0) => false,
        Value::Real(x) if *x == 0.0 => false,
        _ => true,
    }
}

fn find_matching_end(code: &[u8], start: usize) -> Result<usize, VmError> {
    let mut depth = 1;
    let mut pc = start;

    while pc < code.len() && depth > 0 {
        let op_byte = code[pc];
        pc += 1;

        match Opcode::from_byte(op_byte) {
            Some(Opcode::Block | Opcode::Loop | Opcode::If) => {
                depth += 1;
                if pc < code.len() {
                    pc += 1;
                }
            }
            Some(Opcode::TryTable) => {
                depth += 1;
                pc = skip_try_header(code, pc)?;
            }
            Some(Opcode::End) => depth -= 1,
            Some(op) => pc = skip_operands(code, pc, op)?,
            None => return Err(VmError::InvalidOpcode(op_byte)),
        }
    }

    if depth != 0 {
        return Err(VmError::UnexpectedEnd);
    }
    Ok(pc)
}

fn find_if_structure(code: &[u8], start: usize) -> Result<(Option<usize>, usize), VmError> {
    let mut depth = 1;
    let mut pc = start;
    let mut else_pos = None;

    while pc < code.len() && depth > 0 {
        let op_byte = code[pc];
        pc += 1;

        match Opcode::from_byte(op_byte) {
            Some(Opcode::Block | Opcode::Loop | Opcode::If) => {
                depth += 1;
                if pc < code.len() {
                    pc += 1;
                }
            }
            Some(Opcode::TryTable) => {
                depth += 1;
                pc = skip_try_header(code, pc)?;
            }
            Some(Opcode::Else) if depth == 1 => else_pos = Some(pc),
            Some(Opcode::End) => depth -= 1,
            Some(op) => pc = skip_operands(code, pc, op)?,
            None => return Err(VmError::InvalidOpcode(op_byte)),
        }
    }

    if depth != 0 {
        return Err(VmError::UnexpectedEnd);
    }
    Ok((else_pos, pc))
}

fn skip_try_header(code: &[u8], mut pc: usize) -> Result<usize, VmError> {
    if pc >= code.len() {
        return Err(VmError::UnexpectedEnd);
    }
    pc += 1; // block type

    let count = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
    for _ in 0..count {
        let kind_byte = *code.get(pc).ok_or(VmError::UnexpectedEnd)?;
        pc += 1;
        if CatchKind::from_byte(kind_byte)
            .map(|k| k.has_tag())
            .unwrap_or(false)
        {
            read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
        }
        read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
    }
    Ok(pc)
}

fn skip_operands(code: &[u8], mut pc: usize, op: Opcode) -> Result<usize, VmError> {
    use Opcode::*;
    match op {
        Br | BrIf | LocalGet | LocalSet | LocalTee | MakeList | Throw => {
            read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
        }
        I64Const => {
            read_leb128_i64(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
        }
        F64Const => {
            if pc + 8 > code.len() {
                return Err(VmError::UnexpectedEnd);
            }
            pc += 8;
        }
        CallLib => {
            if pc + 4 > code.len() {
                return Err(VmError::UnexpectedEnd);
            }
            pc += 4;
        }
        StringConst | SymbolicConst | EvalName => {
            let len = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
            if pc + len > code.len() {
                return Err(VmError::UnexpectedEnd);
            }
            pc += len;
        }
        MakeProgram => {
            // param_count
            read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
            let str_count = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
            for _ in 0..str_count {
                let len = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
                if pc + len > code.len() {
                    return Err(VmError::UnexpectedEnd);
                }
                pc += len;
            }
            let code_len = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
            if pc + code_len > code.len() {
                return Err(VmError::UnexpectedEnd);
            }
            pc += code_len;
            let span_count = read_u16(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
            for _ in 0..span_count {
                read_u16(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
                read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
                read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
            }
        }
        PushLocalScope => {
            let count = read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)? as usize;
            for _ in 0..count {
                read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
                read_leb128_u32(code, &mut pc).ok_or(VmError::UnexpectedEnd)?;
            }
        }
        _ => {}
    }
    Ok(pc)
}

/// Re-exported library modules for tests.
pub mod libs {
    pub use crate::libs::{ARITH_LIB, PROG_LIB, STACK_LIB, arith, prog, stack};
}

#[cfg(test)]
mod tests {
    use bytecode::{write_leb128_i64, write_u16};

    use super::*;

    fn bytecode(f: impl FnOnce(&mut Vec<u8>)) -> Vec<u8> {
        let mut code = Vec::new();
        f(&mut code);
        code
    }

    #[test]
    fn push_integer() {
        let mut vm = Vm::new();
        let reg = Registry::with_core();
        let code = bytecode(|c| {
            c.push(Opcode::I64Const.as_byte());
            write_leb128_i64(42, c);
        });
        vm.execute(&code, &reg, &[]).unwrap();
        assert_eq!(vm.stack.top().unwrap(), &Value::integer(42));
    }

    #[test]
    fn add_two() {
        let mut vm = Vm::new();
        let reg = Registry::with_core();
        let code = bytecode(|c| {
            c.push(Opcode::I64Const.as_byte());
            write_leb128_i64(1, c);
            c.push(Opcode::I64Const.as_byte());
            write_leb128_i64(2, c);
            c.push(Opcode::CallLib.as_byte());
            write_u16(libs::ARITH_LIB, c);
            write_u16(libs::arith::cmd::ADD, c);
        });
        vm.execute(&code, &reg, &[]).unwrap();
        assert_eq!(vm.stack.top().unwrap(), &Value::integer(3));
    }
}
