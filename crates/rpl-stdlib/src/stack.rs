//! Stack operations library.
//!
//! Provides stack manipulation commands:
//! - DUP, DROP, SWAP, ROT, OVER
//! - PICK, ROLL, DEPTH

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};
use rpl_vm::Opcode;

/// Interface declaration for the Stack library.
const INTERFACE: &str = include_str!("interfaces/stack.rpli");

/// Get the interface specification (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid stack interface"))
}

/// Stack library ID (matches rpl-stdlib).
pub const STACK_LIB: LibId = 72;

/// Stack library command IDs.
pub mod cmd {
    pub const DUP: u16 = 0;
    pub const DROP: u16 = 1;
    pub const SWAP: u16 = 2;
    pub const ROT: u16 = 3;
    pub const PICK: u16 = 4;
    pub const ROLL: u16 = 5;
    pub const DEPTH: u16 = 6;
    pub const OVER: u16 = 7;
    // Additional stack operations
    pub const DUP2: u16 = 8;
    pub const DROP2: u16 = 9;
    pub const DUPN: u16 = 10;
    pub const DROPN: u16 = 11;
    pub const DUPDUP: u16 = 12;
    pub const NDUPN: u16 = 13;
    pub const NIP: u16 = 14;
    pub const UNROT: u16 = 15;
    pub const PICK3: u16 = 16;
    pub const ROLLD: u16 = 17;
    pub const REVN: u16 = 18;
    pub const UNPICK: u16 = 19;
    pub const IFT: u16 = 20;
    pub const IFTE: u16 = 21;
}

/// Stack operations library (implementation only).
#[derive(Clone, Copy)]
pub struct StackLib;

impl LibraryLowerer for StackLib {
    fn id(&self) -> LibId {
        STACK_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // Scratch locals 0, 1, 2 for stack manipulation
        let s0 = ctx.scratch(0);
        let s1 = ctx.scratch(1);
        let s2 = ctx.scratch(2);

        // Emit bytecode for the command
        match cmd {
            cmd::DUP => {
                // DUP (x -- x x): local.tee s0, local.get s0
                ctx.output.emit_local_tee(s0);
                ctx.output.emit_local_get(s0);
            }
            cmd::DROP => {
                // DROP (x --): native drop
                ctx.output.emit_opcode(Opcode::Drop);
            }
            cmd::SWAP => {
                // SWAP (a b -- b a): set s0, set s1, get s0, get s1
                ctx.output.emit_local_set(s0); // b -> s0
                ctx.output.emit_local_set(s1); // a -> s1
                ctx.output.emit_local_get(s0); // push b
                ctx.output.emit_local_get(s1); // push a
            }
            cmd::ROT => {
                // ROT (a b c -- b c a): set s0(c), set s1(b), set s2(a), get s1, get s0, get s2
                ctx.output.emit_local_set(s0); // c -> s0
                ctx.output.emit_local_set(s1); // b -> s1
                ctx.output.emit_local_set(s2); // a -> s2
                ctx.output.emit_local_get(s1); // push b
                ctx.output.emit_local_get(s0); // push c
                ctx.output.emit_local_get(s2); // push a
            }
            cmd::OVER => {
                // OVER (a b -- a b a): set s0(b), tee s1(a), get s0, get s1
                ctx.output.emit_local_set(s0); // b -> s0
                ctx.output.emit_local_tee(s1); // a -> s1 (keeps a on stack)
                ctx.output.emit_local_get(s0); // push b
                ctx.output.emit_local_get(s1); // push a
            }
            _ => {
                // All other commands use library call
                ctx.output.emit_call_lib(STACK_LIB, cmd);
            }
        }

        Ok(())
    }
}

impl LibraryExecutor for StackLib {
    fn id(&self) -> LibId {
        STACK_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::DUP => {
                let val = ctx.peek(0)?.clone();
                ctx.push(val)?;
                Ok(())
            }
            cmd::DROP => {
                ctx.pop()?;
                Ok(())
            }
            cmd::SWAP => {
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                ctx.push(b)?;
                ctx.push(a)?;
                Ok(())
            }
            cmd::ROT => {
                let c = ctx.pop()?;
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                ctx.push(b)?;
                ctx.push(c)?;
                ctx.push(a)?;
                Ok(())
            }
            cmd::OVER => {
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                ctx.push(a.clone())?;
                ctx.push(b)?;
                ctx.push(a)?;
                Ok(())
            }
            cmd::DEPTH => {
                let depth = ctx.depth() as i64;
                ctx.push(Value::Integer(depth))?;
                Ok(())
            }
            cmd::PICK => {
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("PICK: expected number".into()),
                };
                if n == 0 {
                    return Err("PICK: n must be >= 1".into());
                }
                let val = ctx.peek(n - 1)?.clone();
                ctx.push(val)?;
                Ok(())
            }
            cmd::ROLL => {
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("ROLL: expected number".into()),
                };
                if n == 0 {
                    return Err("ROLL: n must be >= 1".into());
                }
                if n == 1 {
                    return Ok(());
                }
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop()?);
                }
                let target = items.pop().unwrap();
                for item in items.into_iter().rev() {
                    ctx.push(item)?;
                }
                ctx.push(target)?;
                Ok(())
            }
            cmd::DUP2 => {
                // DUP2 (a b -- a b a b)
                let b = ctx.peek(0)?.clone();
                let a = ctx.peek(1)?.clone();
                ctx.push(a)?;
                ctx.push(b)?;
                Ok(())
            }
            cmd::DROP2 => {
                // DROP2 (a b --)
                ctx.pop()?;
                ctx.pop()?;
                Ok(())
            }
            cmd::DUPN => {
                // DUPN (x1..xn n -- x1..xn x1..xn)
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("DUPN: expected number".into()),
                };
                if n == 0 {
                    return Ok(());
                }
                let mut items = Vec::with_capacity(n);
                for i in 0..n {
                    items.push(ctx.peek(n - 1 - i)?.clone());
                }
                for item in items {
                    ctx.push(item)?;
                }
                Ok(())
            }
            cmd::DROPN => {
                // DROPN (x1..xn n --)
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("DROPN: expected number".into()),
                };
                for _ in 0..n {
                    ctx.pop()?;
                }
                Ok(())
            }
            cmd::DUPDUP => {
                // DUPDUP (x -- x x x)
                let val = ctx.peek(0)?.clone();
                ctx.push(val.clone())?;
                ctx.push(val)?;
                Ok(())
            }
            cmd::NDUPN => {
                // NDUPN (x n -- x x... n) - duplicate x n times, push n
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("NDUPN: expected number".into()),
                };
                let val = ctx.peek(0)?.clone();
                for _ in 1..n {
                    ctx.push(val.clone())?;
                }
                ctx.push(Value::Integer(n as i64))?;
                Ok(())
            }
            cmd::NIP => {
                // NIP (a b -- b)
                let b = ctx.pop()?;
                ctx.pop()?; // drop a
                ctx.push(b)?;
                Ok(())
            }
            cmd::UNROT => {
                // UNROT (a b c -- c a b) - reverse of ROT
                let c = ctx.pop()?;
                let b = ctx.pop()?;
                let a = ctx.pop()?;
                ctx.push(c)?;
                ctx.push(a)?;
                ctx.push(b)?;
                Ok(())
            }
            cmd::PICK3 => {
                // PICK3 (a b c -- a b c a) - same as 3 PICK
                let val = ctx.peek(2)?.clone();
                ctx.push(val)?;
                Ok(())
            }
            cmd::ROLLD => {
                // ROLLD (x1..xn n -- xn x1..xn-1) - moves TOS to nth position
                // Example: 1 2 3 3 ROLLD -> 3 1 2 (3 goes to bottom)
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("ROLLD: expected number".into()),
                };
                if n <= 1 {
                    return Ok(());
                }
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop()?);
                }
                // items = [xn, xn-1, ..., x1] (TOS first)
                // want: xn at bottom, then x1..xn-1 on top
                // Push TOS first (it goes to bottom)
                ctx.push(items[0].clone())?;
                // Push the rest in reverse order (bottom to top)
                for item in items.into_iter().skip(1).rev() {
                    ctx.push(item)?;
                }
                Ok(())
            }
            cmd::REVN => {
                // REVN (x1..xn n -- xn..x1) - reverse top n elements
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("REVN: expected number".into()),
                };
                if n <= 1 {
                    return Ok(());
                }
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop()?);
                }
                // items = [xn, xn-1, ..., x1] (top first)
                // push them back in same order to reverse
                for item in items {
                    ctx.push(item)?;
                }
                Ok(())
            }
            cmd::UNPICK => {
                // UNPICK (x1..xn obj n -- obj x1..xn-1) - place obj at position n
                let n = match ctx.pop()? {
                    Value::Integer(i) => i as usize,
                    Value::Real(r) => r as usize,
                    _ => return Err("UNPICK: expected number".into()),
                };
                let obj = ctx.pop()?;
                if n == 1 {
                    ctx.push(obj)?;
                    return Ok(());
                }
                // Pop n-1 items, insert obj at bottom, push items back
                let mut items = Vec::with_capacity(n - 1);
                for _ in 0..(n - 1) {
                    items.push(ctx.pop()?);
                }
                ctx.push(obj)?;
                for item in items.into_iter().rev() {
                    ctx.push(item)?;
                }
                Ok(())
            }
            cmd::IFT => {
                // IFT (obj flag -- obj | nothing)
                let flag = ctx.pop()?;
                let obj = ctx.pop()?;
                let is_true = match flag {
                    Value::Integer(i) => i != 0,
                    Value::Real(r) => r != 0.0,
                    _ => return Err("IFT: expected numeric flag".into()),
                };
                if is_true {
                    ctx.push(obj)?;
                }
                Ok(())
            }
            cmd::IFTE => {
                // IFTE (true-obj false-obj flag -- true-obj | false-obj)
                let flag = ctx.pop()?;
                let false_obj = ctx.pop()?;
                let true_obj = ctx.pop()?;
                let is_true = match flag {
                    Value::Integer(i) => i != 0,
                    Value::Real(r) => r != 0.0,
                    _ => return Err("IFTE: expected numeric flag".into()),
                };
                if is_true {
                    ctx.push(true_obj)?;
                } else {
                    ctx.push(false_obj)?;
                }
                Ok(())
            }
            _ => Err(format!("Unknown stack command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_lib_id() {
        assert_eq!(interface().id(), 72);
    }

    #[test]
    fn stack_lib_name() {
        assert_eq!(interface().name(), "Stack");
    }
}
