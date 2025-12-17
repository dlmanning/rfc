//! Stack operations library.
//!
//! Provides stack manipulation commands:
//! - DUP, DROP, SWAP, ROT, OVER
//! - PICK, ROLL, DEPTH

use crate::core::Span;

use crate::{
    ir::{Branch, LibId},
    libs::{ExecuteContext, ExecuteResult, Library, LibraryExecutor, LibraryLowerer, StackEffect},
    lower::{LowerContext, LowerError},
    value::Value,
    vm::bytecode::Opcode,
};

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

/// Stack operations library.
#[derive(Clone, Copy)]
pub struct StackLib;

impl Library for StackLib {
    fn id(&self) -> LibId {
        STACK_LIB
    }

    fn name(&self) -> &'static str {
        "Stack"
    }

    fn commands(&self) -> Vec<super::CommandInfo> {
        use super::CommandInfo;
        vec![
            CommandInfo::with_effect("DUP", STACK_LIB, cmd::DUP, 1, 2),
            CommandInfo::with_effect("DROP", STACK_LIB, cmd::DROP, 1, 0),
            CommandInfo::with_effect("SWAP", STACK_LIB, cmd::SWAP, 2, 2),
            CommandInfo::with_effect("ROT", STACK_LIB, cmd::ROT, 3, 3),
            CommandInfo::new("PICK", STACK_LIB, cmd::PICK), // Dynamic
            CommandInfo::new("ROLL", STACK_LIB, cmd::ROLL), // Dynamic
            CommandInfo::with_effect("DEPTH", STACK_LIB, cmd::DEPTH, 0, 1),
            CommandInfo::with_effect("OVER", STACK_LIB, cmd::OVER, 2, 3),
            // Additional stack operations
            CommandInfo::with_effect("DUP2", STACK_LIB, cmd::DUP2, 2, 4),
            CommandInfo::with_effect("DROP2", STACK_LIB, cmd::DROP2, 2, 0),
            CommandInfo::new("DUPN", STACK_LIB, cmd::DUPN),   // Dynamic
            CommandInfo::new("DROPN", STACK_LIB, cmd::DROPN), // Dynamic
            CommandInfo::with_effect("DUPDUP", STACK_LIB, cmd::DUPDUP, 1, 3),
            CommandInfo::new("NDUPN", STACK_LIB, cmd::NDUPN), // Dynamic
            CommandInfo::with_effect("NIP", STACK_LIB, cmd::NIP, 2, 1),
            CommandInfo::with_effect("UNROT", STACK_LIB, cmd::UNROT, 3, 3),
            CommandInfo::with_effect("PICK3", STACK_LIB, cmd::PICK3, 3, 4),
            CommandInfo::new("ROLLD", STACK_LIB, cmd::ROLLD), // Dynamic
            CommandInfo::new("REVN", STACK_LIB, cmd::REVN),   // Dynamic
            CommandInfo::new("UNPICK", STACK_LIB, cmd::UNPICK), // Dynamic
            CommandInfo::with_effect("IFT", STACK_LIB, cmd::IFT, 2, 0), // 0 or 1 results
            CommandInfo::with_effect("IFTE", STACK_LIB, cmd::IFTE, 3, 1),
        ]
    }
}

impl LibraryLowerer for StackLib {
    fn lower_composite(
        &self,
        _id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        // Stack library has no composites
        Err(LowerError { span: None,
            message: "Stack library has no composites".into(),
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<StackEffect, LowerError> {
        // Scratch locals 0, 1, 2 for stack manipulation
        let s0 = ctx.scratch(0);
        let s1 = ctx.scratch(1);
        let s2 = ctx.scratch(2);

        Ok(match cmd {
            cmd::DUP => {
                // DUP (x -- x x): local.tee s0, local.get s0
                ctx.output.emit_local_tee(s0);
                ctx.output.emit_local_get(s0);
                StackEffect::dup()
            }
            cmd::DROP => {
                // DROP (x --): native drop
                ctx.output.emit_opcode(Opcode::Drop);
                StackEffect::drop()
            }
            cmd::SWAP => {
                // SWAP (a b -- b a): set s0, set s1, get s0, get s1
                ctx.output.emit_local_set(s0); // b -> s0
                ctx.output.emit_local_set(s1); // a -> s1
                ctx.output.emit_local_get(s0); // push b
                ctx.output.emit_local_get(s1); // push a
                StackEffect::swap()
            }
            cmd::ROT => {
                // ROT (a b c -- b c a): set s0(c), set s1(b), set s2(a), get s1, get s0, get s2
                ctx.output.emit_local_set(s0); // c -> s0
                ctx.output.emit_local_set(s1); // b -> s1
                ctx.output.emit_local_set(s2); // a -> s2
                ctx.output.emit_local_get(s1); // push b
                ctx.output.emit_local_get(s0); // push c
                ctx.output.emit_local_get(s2); // push a
                StackEffect::rot()
            }
            cmd::OVER => {
                // OVER (a b -- a b a): set s0(b), tee s1(a), get s0, get s1
                ctx.output.emit_local_set(s0); // b -> s0
                ctx.output.emit_local_tee(s1); // a -> s1 (keeps a on stack)
                ctx.output.emit_local_get(s0); // push b
                ctx.output.emit_local_get(s1); // push a
                StackEffect::over()
            }
            cmd::DEPTH => {
                ctx.output.emit_call_lib(STACK_LIB, cmd);
                StackEffect::depth()
            }
            cmd::PICK => {
                // PICK: consumes index, produces unknown type
                ctx.output.emit_call_lib(STACK_LIB, cmd);
                StackEffect::fixed(1, &[None])
            }
            cmd::ROLL => {
                // ROLL: consumes index, permutes stack unpredictably
                ctx.output.emit_call_lib(STACK_LIB, cmd);
                StackEffect::Dynamic
            }
            cmd::DUP2 | cmd::DROP2 | cmd::DUPN | cmd::DROPN | cmd::DUPDUP | cmd::NDUPN
            | cmd::NIP | cmd::UNROT | cmd::PICK3 | cmd::ROLLD | cmd::REVN | cmd::UNPICK
            | cmd::IFT | cmd::IFTE => {
                ctx.output.emit_call_lib(STACK_LIB, cmd);
                StackEffect::Dynamic
            }
            _ => {
                ctx.output.emit_call_lib(STACK_LIB, cmd);
                StackEffect::Dynamic
            }
        })
    }
}

impl LibraryExecutor for StackLib {
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
        assert_eq!(StackLib.id(), 72);
    }

    #[test]
    fn stack_lib_name() {
        assert_eq!(StackLib.name(), "Stack");
    }
}
