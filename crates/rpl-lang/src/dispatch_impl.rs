//! Library dispatcher implementation.
//!
//! This module provides the `LibraryDispatcher` which implements the
//! `CommandDispatch` trait from rpl-vm, bridging the VM to the library system.

use std::cell::RefCell;
use std::sync::Arc;

use rpl_core::{TypeId, Word, extract_cmd, extract_lib};
use rpl_vm::{CommandDispatch, DispatchResult, RuntimeError, VM, Value};

use crate::library::{ExecuteContext, ExecuteResult, LibraryId, LibraryRegistry};
use crate::operator::{OperatorKind, OperatorRegistry};
use crate::user_libs::UserLibraryRegistry;

/// Reserved library ID for dynamic dispatch.
pub const DISPATCH_LIB: u16 = 0x7FFE;

/// Encode an operator for dispatch.
pub fn encode_dispatch(op: OperatorKind, arity: u8) -> u16 {
    (encode_operator_kind(op) as u16) | ((arity as u16) << 8)
}

/// The library dispatcher routes commands from the VM to library implementations.
pub struct LibraryDispatcher<'a> {
    pub registry: &'a LibraryRegistry,
    pub operators: &'a OperatorRegistry,
    /// User library registry wrapped in RefCell for interior mutability.
    /// This allows libraries to register/unregister commands even though
    /// the CommandDispatch trait uses `&self`.
    user_libs: RefCell<*mut UserLibraryRegistry>,
    /// Marker for the lifetime of user_libs
    _marker: std::marker::PhantomData<&'a mut UserLibraryRegistry>,
}

impl<'a> LibraryDispatcher<'a> {
    /// Create a new library dispatcher.
    pub fn new(
        registry: &'a LibraryRegistry,
        operators: &'a OperatorRegistry,
        user_libs: &'a mut UserLibraryRegistry,
    ) -> Self {
        Self {
            registry,
            operators,
            user_libs: RefCell::new(user_libs as *mut UserLibraryRegistry),
            _marker: std::marker::PhantomData,
        }
    }

    /// Get a reference to the user library registry.
    pub fn user_libs(&self) -> &UserLibraryRegistry {
        // SAFETY: The pointer is valid for the lifetime 'a
        unsafe { &*(*self.user_libs.borrow()) }
    }

    /// Get a mutable reference to the user library registry.
    pub fn user_libs_mut(&self) -> impl std::ops::DerefMut<Target = UserLibraryRegistry> + '_ {
        struct Guard<'a>(std::cell::RefMut<'a, *mut UserLibraryRegistry>);
        impl std::ops::Deref for Guard<'_> {
            type Target = UserLibraryRegistry;
            fn deref(&self) -> &Self::Target {
                // SAFETY: The pointer is valid
                unsafe { &**self.0 }
            }
        }
        impl std::ops::DerefMut for Guard<'_> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                // SAFETY: The pointer is valid
                unsafe { &mut **self.0 }
            }
        }
        Guard(self.user_libs.borrow_mut())
    }

    /// Get a raw pointer to the RefCell for passing to ExecuteContext.
    fn user_libs_ptr(&self) -> *const RefCell<*mut UserLibraryRegistry> {
        &self.user_libs as *const _
    }

    /// Execute a dynamically dispatched operator.
    fn execute_operator_dispatch(
        &self,
        vm: &mut VM,
        cmd: u16,
    ) -> Result<(usize, DispatchResult), RuntimeError> {
        let op_kind = decode_operator_kind((cmd & 0xFF) as u8);
        let arity = ((cmd >> 8) & 0xFF) as u8;

        match arity {
            1 => self.execute_unary_dispatch(vm, op_kind),
            2 => self.execute_binary_dispatch(vm, op_kind),
            _ => Err(RuntimeError::InvalidBytecode(format!(
                "Invalid arity {} for dispatch",
                arity
            ))),
        }
    }

    /// Execute unary operator dispatch.
    fn execute_unary_dispatch(
        &self,
        vm: &mut VM,
        op: OperatorKind,
    ) -> Result<(usize, DispatchResult), RuntimeError> {
        let operand = vm.peek(0)?;
        let operand_type = operand.type_id();

        if let Some(resolution) = self.operators.resolve_unary(op, operand_type) {
            let lib = self
                .registry
                .get(resolution.lib)
                .ok_or(RuntimeError::UnknownLibrary(resolution.lib.as_u16()))?;

            let mut ctx = ExecuteContext::new(vm, &[], 0, resolution.command);
            let result = lib.execute(&mut ctx);
            Ok((0, self.convert_result(result)))
        } else {
            Err(RuntimeError::LibraryError(format!(
                "No implementation for {:?} on {:?}",
                op, operand_type
            )))
        }
    }

    /// Execute binary operator dispatch.
    fn execute_binary_dispatch(
        &self,
        vm: &mut VM,
        op: OperatorKind,
    ) -> Result<(usize, DispatchResult), RuntimeError> {
        let right = vm.peek(0)?;
        let left = vm.peek(1)?;
        let right_type = right.type_id();
        let left_type = left.type_id();

        if let Some(resolution) = self.operators.resolve_binary(op, left_type, right_type) {
            let lib = self
                .registry
                .get(resolution.lib)
                .ok_or(RuntimeError::UnknownLibrary(resolution.lib.as_u16()))?;

            let mut ctx = ExecuteContext::new(vm, &[], 0, resolution.command);
            let result = lib.execute(&mut ctx);
            Ok((0, self.convert_result(result)))
        } else {
            Err(RuntimeError::LibraryError(format!(
                "No implementation for {:?} on ({:?}, {:?})",
                op, left_type, right_type
            )))
        }
    }

    /// Convert ExecuteResult to DispatchResult.
    fn convert_result(&self, result: ExecuteResult) -> DispatchResult {
        match result {
            ExecuteResult::Ok => DispatchResult::Ok,
            ExecuteResult::Jump(addr) => DispatchResult::Jump(addr),
            ExecuteResult::Call(_) => {
                DispatchResult::Error("Call not implemented".to_string())
            }
            ExecuteResult::Return => DispatchResult::Return,
            ExecuteResult::Yield => DispatchResult::Ok,
            ExecuteResult::Halt => DispatchResult::Halt,
            ExecuteResult::Error(msg) => DispatchResult::Error(msg),
            ExecuteResult::EvalProgram(code) => DispatchResult::EvalProgram {
                code,
                name: None,
                debug_info: None,
            },
            ExecuteResult::EvalProgramNamed(code, name) => DispatchResult::EvalProgram {
                code,
                name: Some(name),
                debug_info: None,
            },
            ExecuteResult::EvalProgramWithDebug { code, name, debug_info } => {
                DispatchResult::EvalProgram {
                    code,
                    name: Some(name),
                    debug_info: Some(debug_info),
                }
            }
        }
    }

    /// Handle LIBPTR prolog - execute the referenced command from an attached library.
    fn handle_libptr(
        &self,
        vm: &mut VM,
        code: &[Word],
        pc: usize,
        size: usize,
    ) -> Result<usize, RuntimeError> {
        if size != 2 {
            return Err(RuntimeError::InvalidBytecode(format!(
                "LIBPTR should have size 2, got {}",
                size
            )));
        }

        let lib_id = code[pc + 1];
        let cmd_index = code[pc + 2];

        // Look up the attached library
        let lib_path = crate::user_libs::lib_storage_path(lib_id);
        let lib_obj = vm.recall(&lib_path).cloned().ok_or_else(|| {
            let lib_name = crate::user_libs::decode_lib_id(lib_id);
            RuntimeError::LibraryError(format!("Library '{}' not attached", lib_name))
        })?;

        // Extract the command from the library
        let lib_data = match &lib_obj {
            Value::Object { type_id: t, data } if *t == TypeId::LIBRARY => data.clone(),
            _ => {
                return Err(RuntimeError::LibraryError(
                    "Attached library is not a LIBRARY object".to_string(),
                ))
            }
        };

        let cmd_value = crate::user_libs::get_library_command(&lib_data, cmd_index)
            .ok_or_else(|| {
                RuntimeError::LibraryError(format!(
                    "Command {} not found in library",
                    cmd_index
                ))
            })?;

        // LIBPTR is a command reference - auto-execute the resolved object
        match cmd_value {
            Value::Program { code: prog_code, .. } => {
                // Advance PC past the LIBPTR prolog before pushing return frame
                let new_pc = pc + 1 + size;

                // Get function name for debugging
                let lib_name = crate::user_libs::decode_lib_id(lib_id);
                let cmd_name = self
                    .user_libs()
                    .get_command_name(lib_id, cmd_index)
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("{}.{}", lib_name, cmd_index));

                // Push call frame to return here after program completes
                let caller_code = vm.current_code();
                vm.push_call(caller_code, new_pc, Some(cmd_name), None)
                    .map_err(RuntimeError::Stack)?;

                // Enter the program
                let new_code: Arc<[Word]> = Arc::from(prog_code.as_slice());
                vm.set_current_code(new_code);
                vm.pc = 0;

                // Return a dummy PC - the VM will use vm.pc directly
                Ok(0)
            }
            _ => {
                // Non-program objects just get pushed to stack
                vm.push(cmd_value)?;
                Ok(pc + 1 + size)
            }
        }
    }

    /// Handle LIST prolog - execute the body to construct elements.
    fn handle_list(
        &self,
        vm: &mut VM,
        code: &[Word],
        pc: usize,
        size: usize,
    ) -> Result<usize, RuntimeError> {
        let list_code: Vec<Word> = code[pc + 1..pc + 1 + size].to_vec();

        // Save current stack depth
        let start_depth = vm.depth();

        // Execute the list body code inline
        rpl_vm::execute_inline(vm, &list_code, self)?;

        // Collect all values pushed during list body execution
        let end_depth = vm.depth();
        let element_count = end_depth - start_depth;
        let mut elements = Vec::with_capacity(element_count);
        for _ in 0..element_count {
            elements.push(vm.pop()?);
        }
        elements.reverse(); // Elements were popped in reverse order

        vm.push(Value::List(elements))?;
        Ok(pc + 1 + size)
    }
}

impl CommandDispatch for LibraryDispatcher<'_> {
    fn dispatch(
        &self,
        vm: &mut VM,
        word: Word,
        code: &[Word],
        pc: usize,
    ) -> Result<(usize, DispatchResult), RuntimeError> {
        let lib_id_raw = extract_lib(word);
        let cmd = extract_cmd(word);

        // Check for dynamic dispatch first
        if lib_id_raw == DISPATCH_LIB {
            return self.execute_operator_dispatch(vm, cmd);
        }

        let lib_id = LibraryId::new(lib_id_raw);
        let lib = self
            .registry
            .get(lib_id)
            .ok_or(RuntimeError::UnknownLibrary(lib_id_raw))?;

        let mut ctx = ExecuteContext::with_user_libs(vm, code, pc, cmd, self.user_libs_ptr());
        let result = lib.execute(&mut ctx);
        let operands_consumed = ctx.operands_consumed();

        Ok((operands_consumed, self.convert_result(result)))
    }

    fn handle_prolog(
        &self,
        vm: &mut VM,
        type_id: u16,
        size: usize,
        code: &[Word],
        pc: usize,
    ) -> Option<Result<usize, RuntimeError>> {
        let tid = TypeId::new(type_id);

        match tid {
            TypeId::LIBPTR => Some(self.handle_libptr(vm, code, pc, size)),
            TypeId::LIST => Some(self.handle_list(vm, code, pc, size)),
            _ => None, // Let VM handle it
        }
    }
}

// =============================================================================
// Operator encoding/decoding (moved from vm/dispatch.rs)
// =============================================================================

/// Encode operator kind to u8.
fn encode_operator_kind(op: OperatorKind) -> u8 {
    match op {
        OperatorKind::Add => 0,
        OperatorKind::Sub => 1,
        OperatorKind::Mul => 2,
        OperatorKind::Div => 3,
        OperatorKind::Mod => 4,
        OperatorKind::Neg => 5,
        OperatorKind::Inv => 6,
        OperatorKind::Pow => 7,
        OperatorKind::Sqrt => 8,
        OperatorKind::Abs => 9,
        OperatorKind::Eq => 10,
        OperatorKind::Ne => 11,
        OperatorKind::Lt => 12,
        OperatorKind::Le => 13,
        OperatorKind::Gt => 14,
        OperatorKind::Ge => 15,
        OperatorKind::Cmp => 16,
        OperatorKind::And => 17,
        OperatorKind::Or => 18,
        OperatorKind::Xor => 19,
        OperatorKind::Not => 20,
        OperatorKind::Size => 21,
        OperatorKind::Get => 22,
        OperatorKind::Put => 23,
        OperatorKind::Head => 24,
        OperatorKind::Tail => 25,
        OperatorKind::ToReal => 26,
        OperatorKind::ToInt => 27,
        OperatorKind::ToString => 28,
        OperatorKind::Eval => 29,
        OperatorKind::Apply => 30,
        OperatorKind::Same => 31,
    }
}

/// Decode u8 to operator kind.
pub fn decode_operator_kind(code: u8) -> OperatorKind {
    match code {
        0 => OperatorKind::Add,
        1 => OperatorKind::Sub,
        2 => OperatorKind::Mul,
        3 => OperatorKind::Div,
        4 => OperatorKind::Mod,
        5 => OperatorKind::Neg,
        6 => OperatorKind::Inv,
        7 => OperatorKind::Pow,
        8 => OperatorKind::Sqrt,
        9 => OperatorKind::Abs,
        10 => OperatorKind::Eq,
        11 => OperatorKind::Ne,
        12 => OperatorKind::Lt,
        13 => OperatorKind::Le,
        14 => OperatorKind::Gt,
        15 => OperatorKind::Ge,
        16 => OperatorKind::Cmp,
        17 => OperatorKind::And,
        18 => OperatorKind::Or,
        19 => OperatorKind::Xor,
        20 => OperatorKind::Not,
        21 => OperatorKind::Size,
        22 => OperatorKind::Get,
        23 => OperatorKind::Put,
        24 => OperatorKind::Head,
        25 => OperatorKind::Tail,
        26 => OperatorKind::ToReal,
        27 => OperatorKind::ToInt,
        28 => OperatorKind::ToString,
        29 => OperatorKind::Eval,
        30 => OperatorKind::Apply,
        31 => OperatorKind::Same,
        _ => OperatorKind::Add, // Default fallback
    }
}

/// Get the source symbol for an operator.
pub fn operator_symbol(op: OperatorKind) -> &'static str {
    match op {
        OperatorKind::Add => "+",
        OperatorKind::Sub => "-",
        OperatorKind::Mul => "*",
        OperatorKind::Div => "/",
        OperatorKind::Mod => "MOD",
        OperatorKind::Neg => "NEG",
        OperatorKind::Inv => "INV",
        OperatorKind::Pow => "^",
        OperatorKind::Sqrt => "SQRT",
        OperatorKind::Abs => "ABS",
        OperatorKind::Eq => "==",
        OperatorKind::Ne => "!=",
        OperatorKind::Lt => "<",
        OperatorKind::Le => "<=",
        OperatorKind::Gt => ">",
        OperatorKind::Ge => ">=",
        OperatorKind::Cmp => "CMP",
        OperatorKind::And => "AND",
        OperatorKind::Or => "OR",
        OperatorKind::Xor => "XOR",
        OperatorKind::Not => "NOT",
        OperatorKind::Size => "SIZE",
        OperatorKind::Get => "GET",
        OperatorKind::Put => "PUT",
        OperatorKind::Head => "HEAD",
        OperatorKind::Tail => "TAIL",
        OperatorKind::ToReal => "->R",
        OperatorKind::ToInt => "->INT",
        OperatorKind::ToString => "->STR",
        OperatorKind::Eval => "EVAL",
        OperatorKind::Apply => "APPLY",
        OperatorKind::Same => "SAME",
    }
}

/// Decode a dispatch command and return the operator symbol.
pub fn decode_dispatch_symbol(cmd: u16) -> &'static str {
    let op_code = (cmd & 0xFF) as u8;
    let op = decode_operator_kind(op_code);
    operator_symbol(op)
}

/// Decode a dispatch command and return (OperatorKind, arity).
pub fn decode_dispatch(cmd: u16) -> (OperatorKind, u8) {
    let op_code = (cmd & 0xFF) as u8;
    let arity = ((cmd >> 8) & 0xFF) as u8;
    (decode_operator_kind(op_code), arity)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_decode_roundtrip() {
        let ops = [
            OperatorKind::Add,
            OperatorKind::Sub,
            OperatorKind::Mul,
            OperatorKind::Neg,
            OperatorKind::Abs,
        ];

        for op in ops {
            let code = encode_operator_kind(op);
            let decoded = decode_operator_kind(code);
            assert_eq!(op, decoded);
        }
    }

    #[test]
    fn encode_dispatch_format() {
        let cmd = encode_dispatch(OperatorKind::Add, 2);
        assert_eq!(cmd & 0xFF, 0); // Add = 0
        assert_eq!((cmd >> 8) & 0xFF, 2); // arity = 2
    }
}
