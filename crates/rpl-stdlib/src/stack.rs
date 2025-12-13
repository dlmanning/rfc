use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

/// Library for stack manipulation commands.
pub struct StackLib;

impl StackLib {
    /// Library ID for stack operations.
    pub const ID: LibraryId = LibraryId::new(72);

    // Command IDs
    const CMD_DUP: u16 = 0;
    const CMD_DROP: u16 = 1;
    const CMD_SWAP: u16 = 2;
    const CMD_OVER: u16 = 3;
    const CMD_ROT: u16 = 4;
    const CMD_DEPTH: u16 = 5;
    const CMD_CLEAR: u16 = 6;
    const CMD_PICK: u16 = 7;
    const CMD_ROLL: u16 = 8;
    const CMD_DUP2: u16 = 9;
    const CMD_IFT: u16 = 10;
    const CMD_IFTE: u16 = 11;
    const CMD_UNROT: u16 = 12;
    const CMD_DROP2: u16 = 13;
    const CMD_NIP: u16 = 14;
    const CMD_DUPN: u16 = 15;
    const CMD_DROPN: u16 = 16;
    const CMD_ROLLD: u16 = 17;
    const CMD_DUPDUP: u16 = 18;
    const CMD_PICK3: u16 = 19;
    const CMD_NDUPN: u16 = 20;
    const CMD_REVN: u16 = 21;
    const CMD_UNPICK: u16 = 22;

    /// Get command ID from name (case-insensitive).
    fn command_id(name: &str) -> Option<u16> {
        match name.to_ascii_uppercase().as_str() {
            "DUP" => Some(Self::CMD_DUP),
            "DROP" => Some(Self::CMD_DROP),
            "SWAP" => Some(Self::CMD_SWAP),
            "OVER" => Some(Self::CMD_OVER),
            "ROT" => Some(Self::CMD_ROT),
            "DEPTH" => Some(Self::CMD_DEPTH),
            "CLEAR" => Some(Self::CMD_CLEAR),
            "PICK" => Some(Self::CMD_PICK),
            "ROLL" => Some(Self::CMD_ROLL),
            "DUP2" => Some(Self::CMD_DUP2),
            "IFT" => Some(Self::CMD_IFT),
            "IFTE" => Some(Self::CMD_IFTE),
            "UNROT" => Some(Self::CMD_UNROT),
            "DROP2" => Some(Self::CMD_DROP2),
            "NIP" => Some(Self::CMD_NIP),
            "DUPN" => Some(Self::CMD_DUPN),
            "DROPN" => Some(Self::CMD_DROPN),
            "ROLLD" => Some(Self::CMD_ROLLD),
            "DUPDUP" => Some(Self::CMD_DUPDUP),
            "PICK3" => Some(Self::CMD_PICK3),
            "NDUPN" => Some(Self::CMD_NDUPN),
            "REVN" => Some(Self::CMD_REVN),
            "UNPICK" => Some(Self::CMD_UNPICK),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_DUP => Some("DUP"),
            Self::CMD_DROP => Some("DROP"),
            Self::CMD_SWAP => Some("SWAP"),
            Self::CMD_OVER => Some("OVER"),
            Self::CMD_ROT => Some("ROT"),
            Self::CMD_DEPTH => Some("DEPTH"),
            Self::CMD_CLEAR => Some("CLEAR"),
            Self::CMD_PICK => Some("PICK"),
            Self::CMD_ROLL => Some("ROLL"),
            Self::CMD_DUP2 => Some("DUP2"),
            Self::CMD_IFT => Some("IFT"),
            Self::CMD_IFTE => Some("IFTE"),
            Self::CMD_UNROT => Some("UNROT"),
            Self::CMD_DROP2 => Some("DROP2"),
            Self::CMD_NIP => Some("NIP"),
            Self::CMD_DUPN => Some("DUPN"),
            Self::CMD_DROPN => Some("DROPN"),
            Self::CMD_ROLLD => Some("ROLLD"),
            Self::CMD_DUPDUP => Some("DUPDUP"),
            Self::CMD_PICK3 => Some("PICK3"),
            Self::CMD_NDUPN => Some("NDUPN"),
            Self::CMD_REVN => Some("REVN"),
            Self::CMD_UNPICK => Some("UNPICK"),
            _ => None,
        }
    }
}

impl Library for StackLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Stack"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        if Self::command_id(text).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        if let Some(cmd) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_DUP => {
                let val = match ctx.peek(0) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push(val).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_DROP => {
                if ctx.pop().is_err() {
                    return ExecuteResult::Error("Stack underflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_SWAP => {
                let b = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let _ = ctx.push(b);
                let _ = ctx.push(a);
                ExecuteResult::Ok
            }
            Self::CMD_OVER => {
                let b = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let _ = ctx.push(a.clone());
                let _ = ctx.push(b);
                let _ = ctx.push(a);
                ExecuteResult::Ok
            }
            Self::CMD_ROT => {
                let c = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let b = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                // ROT: ( a b c -- b c a )
                let _ = ctx.push(b);
                let _ = ctx.push(c);
                let _ = ctx.push(a);
                ExecuteResult::Ok
            }
            Self::CMD_DEPTH => {
                let depth = ctx.depth() as i64;
                if ctx.push(Value::Int(depth)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_CLEAR => {
                ctx.clear();
                ExecuteResult::Ok
            }
            Self::CMD_PICK => {
                // PICK: ( ... n -- ... item_n ) copy nth item to top (1-indexed)
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("PICK: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 0 {
                    return ExecuteResult::Error("PICK: n must be >= 1".to_string());
                }
                // n=1 means top of stack (index 0), n=2 means second from top (index 1), etc.
                let idx = n - 1;
                let val = match ctx.peek(idx) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("PICK: stack underflow".to_string()),
                };
                if ctx.push(val).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_ROLL => {
                // ROLL: ( ... n -- ... ) rotate nth item to top (1-indexed)
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("ROLL: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 0 {
                    return ExecuteResult::Error("ROLL: n must be >= 1".to_string());
                }
                if n == 1 {
                    // Rolling 1 is a no-op
                    return ExecuteResult::Ok;
                }
                // n=2 is like SWAP, n=3 is like ROT
                // Need to remove item at position n-1 and push to top
                let depth = ctx.depth();
                if n > depth {
                    return ExecuteResult::Error("ROLL: stack underflow".to_string());
                }
                // Pop all items down to and including the nth
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop().unwrap());
                }
                // Last item popped is the one we want to move to top
                let target = items.pop().unwrap();
                // Push back all other items in reverse order
                for item in items.into_iter().rev() {
                    let _ = ctx.push(item);
                }
                // Push target last (on top)
                let _ = ctx.push(target);
                ExecuteResult::Ok
            }
            Self::CMD_DUP2 => {
                // DUP2: ( a b -- a b a b )
                let b = match ctx.peek(0) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.peek(1) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push(a).is_err() || ctx.push(b).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_IFT => {
                // IFT: ( obj flag -- obj or nothing )
                // If flag is true (non-zero), leave obj on stack; otherwise drop both
                let flag = match ctx.pop() {
                    Ok(Value::Real(r)) => r != 0.0,
                    Ok(Value::Int(i)) => i != 0,
                    Ok(_) => return ExecuteResult::Error("IFT: expected number for flag".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if !flag {
                    // Drop the obj too
                    if ctx.pop().is_err() {
                        return ExecuteResult::Error("Stack underflow".to_string());
                    }
                }
                // If flag is true, obj remains on stack (we only popped the flag)
                ExecuteResult::Ok
            }
            Self::CMD_IFTE => {
                // IFTE: ( true_obj false_obj flag -- obj )
                // If flag is true (non-zero), keep true_obj; otherwise keep false_obj
                let flag = match ctx.pop() {
                    Ok(Value::Real(r)) => r != 0.0,
                    Ok(Value::Int(i)) => i != 0,
                    Ok(_) => return ExecuteResult::Error("IFTE: expected number for flag".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let false_obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let true_obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let result = if flag { true_obj } else { false_obj };
                if ctx.push(result).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_UNROT => {
                // UNROT: ( a b c -- c a b ) - opposite of ROT
                let c = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let b = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let a = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let _ = ctx.push(c);
                let _ = ctx.push(a);
                let _ = ctx.push(b);
                ExecuteResult::Ok
            }
            Self::CMD_DROP2 => {
                // DROP2: ( a b -- ) - drop top two items
                if ctx.pop().is_err() {
                    return ExecuteResult::Error("Stack underflow".to_string());
                }
                if ctx.pop().is_err() {
                    return ExecuteResult::Error("Stack underflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_NIP => {
                // NIP: ( a b -- b ) - remove second item
                let b = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.pop().is_err() {
                    return ExecuteResult::Error("Stack underflow".to_string());
                }
                let _ = ctx.push(b);
                ExecuteResult::Ok
            }
            Self::CMD_DUPN => {
                // DUPN: ( ... n -- ... ... ) - duplicate top n items
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("DUPN: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 0 {
                    return ExecuteResult::Ok;
                }
                let depth = ctx.depth();
                if n > depth {
                    return ExecuteResult::Error("DUPN: stack underflow".to_string());
                }
                // Collect items to duplicate (in reverse order from stack)
                let mut items = Vec::with_capacity(n);
                for i in (0..n).rev() {
                    match ctx.peek(i) {
                        Ok(v) => items.push(v.clone()),
                        Err(_) => return ExecuteResult::Error("DUPN: stack underflow".to_string()),
                    }
                }
                // Push copies
                for item in items {
                    if ctx.push(item).is_err() {
                        return ExecuteResult::Error("Stack overflow".to_string());
                    }
                }
                ExecuteResult::Ok
            }
            Self::CMD_DROPN => {
                // DROPN: ( ... n -- ) - drop top n items
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("DROPN: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                for _ in 0..n {
                    if ctx.pop().is_err() {
                        return ExecuteResult::Error("DROPN: stack underflow".to_string());
                    }
                }
                ExecuteResult::Ok
            }
            Self::CMD_ROLLD => {
                // ROLLD: ( ... n -- ... ) - roll top to nth position (opposite of ROLL)
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("ROLLD: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 0 {
                    return ExecuteResult::Error("ROLLD: n must be >= 1".to_string());
                }
                if n == 1 {
                    return ExecuteResult::Ok;
                }
                let depth = ctx.depth();
                if n > depth {
                    return ExecuteResult::Error("ROLLD: stack underflow".to_string());
                }
                // Pop all n items
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop().unwrap());
                }
                // First item popped (was on top) goes to bottom of the group
                let top = items.remove(0);
                // Push back: first the old top (now at bottom), then the rest in reverse
                let _ = ctx.push(top);
                for item in items.into_iter().rev() {
                    let _ = ctx.push(item);
                }
                ExecuteResult::Ok
            }
            Self::CMD_DUPDUP => {
                // DUPDUP: ( a -- a a a ) - duplicate top twice
                let val = match ctx.peek(0) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push(val.clone()).is_err() || ctx.push(val).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_PICK3 => {
                // PICK3: ( a b c -- a b c a ) - copy third item to top
                let val = match ctx.peek(2) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if ctx.push(val).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_NDUPN => {
                // NDUPN: ( obj n -- obj obj ... obj n ) - duplicate obj n times, push n
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as i64,
                    Ok(Value::Int(i)) => i,
                    Ok(_) => return ExecuteResult::Error("NDUPN: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n < 0 {
                    return ExecuteResult::Error("NDUPN: n must be >= 0".to_string());
                }
                let obj = match ctx.peek(0) {
                    Ok(v) => v.clone(),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                // Push n-1 more copies (one is already there)
                for _ in 1..n {
                    if ctx.push(obj.clone()).is_err() {
                        return ExecuteResult::Error("Stack overflow".to_string());
                    }
                }
                // Push n
                if ctx.push(Value::Int(n)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_REVN => {
                // REVN: ( ... n -- ... ) - reverse top n items
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("REVN: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n <= 1 {
                    return ExecuteResult::Ok;
                }
                let depth = ctx.depth();
                if n > depth {
                    return ExecuteResult::Error("REVN: stack underflow".to_string());
                }
                // Pop n items
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop().unwrap());
                }
                // Push back in same order (which reverses them on stack)
                for item in items {
                    let _ = ctx.push(item);
                }
                ExecuteResult::Ok
            }
            Self::CMD_UNPICK => {
                // UNPICK: ( ... obj n -- ... ) - move obj to nth position
                let n = match ctx.pop() {
                    Ok(Value::Real(r)) => r as usize,
                    Ok(Value::Int(i)) => i as usize,
                    Ok(_) => return ExecuteResult::Error("UNPICK: expected number".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 0 {
                    return ExecuteResult::Error("UNPICK: n must be >= 1".to_string());
                }
                let obj = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                if n == 1 {
                    // Just push it back on top
                    let _ = ctx.push(obj);
                    return ExecuteResult::Ok;
                }
                let depth = ctx.depth();
                if n - 1 > depth {
                    return ExecuteResult::Error("UNPICK: stack underflow".to_string());
                }
                // Pop n-1 items, insert obj, push back
                let mut items = Vec::with_capacity(n - 1);
                for _ in 0..(n - 1) {
                    items.push(ctx.pop().unwrap());
                }
                // Push obj first (will be at position n)
                let _ = ctx.push(obj);
                // Push back the items in reverse order
                for item in items.into_iter().rev() {
                    let _ = ctx.push(item);
                }
                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown stack command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => DecompileResult::Unknown,
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token.to_ascii_uppercase().as_str() {
            "DUP" => StackEffect::Fixed {
                consumes: 1,
                produces: 2,
            },
            "DROP" => StackEffect::Fixed {
                consumes: 1,
                produces: 0,
            },
            "SWAP" => StackEffect::Fixed {
                consumes: 2,
                produces: 2,
            },
            "OVER" => StackEffect::Fixed {
                consumes: 2,
                produces: 3,
            },
            "ROT" | "UNROT" => StackEffect::Fixed {
                consumes: 3,
                produces: 3,
            },
            "DEPTH" => StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            },
            "DUP2" => StackEffect::Fixed {
                consumes: 2,
                produces: 4,
            },
            "IFTE" => StackEffect::Fixed {
                consumes: 3,
                produces: 1,
            },
            "DROP2" => StackEffect::Fixed {
                consumes: 2,
                produces: 0,
            },
            "NIP" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            "DUPDUP" => StackEffect::Fixed {
                consumes: 1,
                produces: 3,
            },
            "PICK3" => StackEffect::Fixed {
                consumes: 3,
                produces: 4,
            },
            // Dynamic: CLEAR, PICK, ROLL, IFT, DUPN, DROPN, ROLLD, NDUPN, REVN, UNPICK
            _ => StackEffect::Dynamic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::compile::OutputBuffer;
    use rpl_lang::VM;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &'_ mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn probe_dup() {
        let interner = Interner::new();
        let lib = StackLib;
        let ctx = make_probe_ctx("DUP", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = StackLib;

        let ctx = make_probe_ctx("dup", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("Swap", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_all_commands() {
        let interner = Interner::new();
        let lib = StackLib;

        for cmd in &["DUP", "DROP", "SWAP", "OVER", "ROT", "DEPTH", "CLEAR"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                cmd
            );
        }
    }

    #[test]
    fn probe_unknown() {
        let interner = Interner::new();
        let lib = StackLib;
        let ctx = make_probe_ctx("UNKNOWN", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_dup() {
        let lib = StackLib;
        let effect = lib.stack_effect("DUP");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 1,
                produces: 2
            }
        ));
    }

    #[test]
    fn stack_effect_drop() {
        let lib = StackLib;
        let effect = lib.stack_effect("DROP");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 1,
                produces: 0
            }
        ));
    }

    #[test]
    fn stack_effect_swap() {
        let lib = StackLib;
        let effect = lib.stack_effect("SWAP");
        assert!(matches!(
            effect,
            StackEffect::Fixed {
                consumes: 2,
                produces: 2
            }
        ));
    }

    #[test]
    fn stack_effect_clear_is_dynamic() {
        let lib = StackLib;
        let effect = lib.stack_effect("CLEAR");
        assert!(matches!(effect, StackEffect::Dynamic));
    }

    #[test]
    fn execute_dup() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(42)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_DUP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 2);
        assert_eq!(vm.pop_int().unwrap(), 42);
        assert_eq!(vm.pop_int().unwrap(), 42);
    }

    #[test]
    fn execute_drop() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_DROP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 1);
        assert_eq!(vm.pop_int().unwrap(), 1);
    }

    #[test]
    fn execute_swap() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_SWAP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 1);
        assert_eq!(vm.pop_int().unwrap(), 2);
    }

    #[test]
    fn execute_over() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_OVER);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 3);
        assert_eq!(vm.pop_int().unwrap(), 1);
        assert_eq!(vm.pop_int().unwrap(), 2);
        assert_eq!(vm.pop_int().unwrap(), 1);
    }

    #[test]
    fn execute_rot() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();
        vm.push(Value::Int(3)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_ROT);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        // ROT: ( a b c -- b c a )
        assert_eq!(vm.pop_int().unwrap(), 1);
        assert_eq!(vm.pop_int().unwrap(), 3);
        assert_eq!(vm.pop_int().unwrap(), 2);
    }

    #[test]
    fn execute_depth() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();
        vm.push(Value::Int(3)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_DEPTH);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.pop_int().unwrap(), 3); // depth before DEPTH was 3
    }

    #[test]
    fn execute_clear() {
        let lib = StackLib;
        let mut vm = VM::new();
        vm.push(Value::Int(1)).unwrap();
        vm.push(Value::Int(2)).unwrap();
        vm.push(Value::Int(3)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_CLEAR);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Ok));
        assert_eq!(vm.depth(), 0);
    }

    #[test]
    fn execute_underflow() {
        let lib = StackLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_DUP);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, ExecuteResult::Error(_)));
    }

    #[test]
    fn compile_command() {
        let lib = StackLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(3));

        let mut ctx = CompileContext::new(span, "DUP", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
        assert_eq!(ctx.position(), 1); // One opcode emitted
    }
}
