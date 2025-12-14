use rpl_lang::{Value, library::EXEC_OK};

rpl_macros::define_library! {
    pub library StackLib(72, "Stack");

    commands {
        DUP (1 -> 2) [
            brief: "Duplicate top of stack",
            stack: "( a -- a a )",
            example: "3 DUP",
            see_also: ["DROP", "SWAP"],
        ] {
            let val = match ctx.peek(0) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if ctx.push(val).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        DROP (1 -> 0) [
            brief: "Remove top of stack",
            stack: "( a -- )",
            example: "1 2 DROP",
            see_also: ["DUP", "CLEAR"],
        ] {
            if ctx.pop().is_err() {
                return Err("Stack underflow".to_string());
            }
            EXEC_OK
        }

        SWAP (2 -> 2) [
            brief: "Exchange top two items",
            stack: "( a b -- b a )",
            example: "1 2 SWAP",
            see_also: ["ROT", "OVER"],
        ] {
            let b = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let _ = ctx.push(b);
            let _ = ctx.push(a);
            EXEC_OK
        }

        OVER (2 -> 3) [
            brief: "Copy second item to top",
            stack: "( a b -- a b a )",
            example: "1 2 OVER",
            see_also: ["DUP", "PICK"],
        ] {
            let b = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let _ = ctx.push(a.clone());
            let _ = ctx.push(b);
            let _ = ctx.push(a);
            EXEC_OK
        }

        ROT (3 -> 3) [
            brief: "Rotate third item to top",
            stack: "( a b c -- b c a )",
            example: "1 2 3 ROT",
            see_also: ["UNROT", "ROLL"],
        ] {
            let c = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let b = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let _ = ctx.push(b);
            let _ = ctx.push(c);
            let _ = ctx.push(a);
            EXEC_OK
        }

        DEPTH (0 -> 1) [
            brief: "Push stack depth",
            stack: "( -- n )",
            example: "1 2 3 DEPTH",
            see_also: ["CLEAR"],
        ] {
            let depth = ctx.depth() as i64;
            if ctx.push(Value::Int(depth)).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        CLEAR (*) [
            brief: "Clear entire stack",
            stack: "( ... -- )",
            example: "1 2 3 CLEAR",
            see_also: ["DROP", "DROPN"],
        ] {
            ctx.clear();
            EXEC_OK
        }

        PICK (*) [
            brief: "Copy nth item to top (1-indexed)",
            stack: "( ... n -- ... item_n )",
            example: "1 2 3 2 PICK",
            see_also: ["ROLL", "DUP"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("PICK: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 0 {
                return Err("PICK: n must be >= 1".to_string());
            }
            let idx = n - 1;
            let val = match ctx.peek(idx) {
                Ok(v) => v.clone(),
                Err(_) => return Err("PICK: stack underflow".to_string()),
            };
            if ctx.push(val).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        ROLL (*) [
            brief: "Rotate nth item to top (1-indexed)",
            stack: "( ... n -- ... )",
            example: "1 2 3 3 ROLL",
            see_also: ["ROLLD", "ROT"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("ROLL: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 0 {
                return Err("ROLL: n must be >= 1".to_string());
            }
            if n == 1 {
                return EXEC_OK;
            }
            let depth = ctx.depth();
            if n > depth {
                return Err("ROLL: stack underflow".to_string());
            }
            let mut items = Vec::with_capacity(n);
            for _ in 0..n {
                items.push(ctx.pop().unwrap());
            }
            let target = items.pop().unwrap();
            for item in items.into_iter().rev() {
                let _ = ctx.push(item);
            }
            let _ = ctx.push(target);
            EXEC_OK
        }

        DUP2 (2 -> 4) [
            brief: "Duplicate top two items",
            stack: "( a b -- a b a b )",
            example: "1 2 DUP2",
            see_also: ["DUP", "DUPN"],
        ] {
            let b = match ctx.peek(0) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.peek(1) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if ctx.push(a).is_err() || ctx.push(b).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        IFT (*) [
            brief: "Conditional keep: if flag true, keep obj",
            stack: "( obj flag -- obj | )",
            example: "5 1 IFT",
            see_also: ["IFTE"],
        ] {
            let flag = match ctx.pop() {
                Ok(Value::Real(r)) => r != 0.0,
                Ok(Value::Int(i)) => i != 0,
                Ok(_) => return Err("IFT: expected number for flag".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if !flag
                && ctx.pop().is_err() {
                    return Err("Stack underflow".to_string());
                }
            EXEC_OK
        }

        IFTE (3 -> 1) [
            brief: "Conditional select: if flag true, keep true_obj else false_obj",
            stack: "( true_obj false_obj flag -- obj )",
            example: "10 20 1 IFTE",
            see_also: ["IFT"],
        ] {
            let flag = match ctx.pop() {
                Ok(Value::Real(r)) => r != 0.0,
                Ok(Value::Int(i)) => i != 0,
                Ok(_) => return Err("IFTE: expected number for flag".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let false_obj = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let true_obj = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let result = if flag { true_obj } else { false_obj };
            if ctx.push(result).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        UNROT (3 -> 3) [
            brief: "Reverse rotate: move top under third",
            stack: "( a b c -- c a b )",
            example: "1 2 3 UNROT",
            see_also: ["ROT", "ROLL"],
        ] {
            let c = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let b = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let a = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let _ = ctx.push(c);
            let _ = ctx.push(a);
            let _ = ctx.push(b);
            EXEC_OK
        }

        DROP2 (2 -> 0) [
            brief: "Drop top two items",
            stack: "( a b -- )",
            example: "1 2 3 DROP2",
            see_also: ["DROP", "DROPN"],
        ] {
            if ctx.pop().is_err() {
                return Err("Stack underflow".to_string());
            }
            if ctx.pop().is_err() {
                return Err("Stack underflow".to_string());
            }
            EXEC_OK
        }

        NIP (2 -> 1) [
            brief: "Remove second item",
            stack: "( a b -- b )",
            example: "1 2 NIP",
            see_also: ["DROP", "SWAP"],
        ] {
            let b = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if ctx.pop().is_err() {
                return Err("Stack underflow".to_string());
            }
            let _ = ctx.push(b);
            EXEC_OK
        }

        DUPN (*) [
            brief: "Duplicate top n items",
            stack: "( ... n -- ... ... )",
            example: "1 2 3 2 DUPN",
            see_also: ["DUP", "DUP2"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("DUPN: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 0 {
                return EXEC_OK;
            }
            let depth = ctx.depth();
            if n > depth {
                return Err("DUPN: stack underflow".to_string());
            }
            let mut items = Vec::with_capacity(n);
            for i in (0..n).rev() {
                match ctx.peek(i) {
                    Ok(v) => items.push(v.clone()),
                    Err(_) => return Err("DUPN: stack underflow".to_string()),
                }
            }
            for item in items {
                if ctx.push(item).is_err() {
                    return Err("Stack overflow".to_string());
                }
            }
            EXEC_OK
        }

        DROPN (*) [
            brief: "Drop top n items",
            stack: "( ... n -- )",
            example: "1 2 3 4 2 DROPN",
            see_also: ["DROP", "DROP2"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("DROPN: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            for _ in 0..n {
                if ctx.pop().is_err() {
                    return Err("DROPN: stack underflow".to_string());
                }
            }
            EXEC_OK
        }

        ROLLD (*) [
            brief: "Roll top to nth position",
            stack: "( ... n -- ... )",
            example: "1 2 3 3 ROLLD",
            see_also: ["ROLL", "UNROT"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("ROLLD: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 0 {
                return Err("ROLLD: n must be >= 1".to_string());
            }
            if n == 1 {
                return EXEC_OK;
            }
            let depth = ctx.depth();
            if n > depth {
                return Err("ROLLD: stack underflow".to_string());
            }
            let mut items = Vec::with_capacity(n);
            for _ in 0..n {
                items.push(ctx.pop().unwrap());
            }
            let top = items.remove(0);
            let _ = ctx.push(top);
            for item in items.into_iter().rev() {
                let _ = ctx.push(item);
            }
            EXEC_OK
        }

        DUPDUP (1 -> 3) [
            brief: "Duplicate top twice",
            stack: "( a -- a a a )",
            example: "5 DUPDUP",
            see_also: ["DUP", "DUP2"],
        ] {
            let val = match ctx.peek(0) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if ctx.push(val.clone()).is_err() || ctx.push(val).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        PICK3 (3 -> 4) [
            brief: "Copy third item to top",
            stack: "( a b c -- a b c a )",
            example: "1 2 3 PICK3",
            see_also: ["PICK", "OVER"],
        ] {
            let val = match ctx.peek(2) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if ctx.push(val).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        NDUPN (*) [
            brief: "Duplicate obj n times, push n",
            stack: "( obj n -- obj obj ... obj n )",
            example: "5 3 NDUPN",
            see_also: ["DUPN", "DUP"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as i64,
                Ok(Value::Int(i)) => i,
                Ok(_) => return Err("NDUPN: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n < 0 {
                return Err("NDUPN: n must be >= 0".to_string());
            }
            let obj = match ctx.peek(0) {
                Ok(v) => v.clone(),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            for _ in 1..n {
                if ctx.push(obj.clone()).is_err() {
                    return Err("Stack overflow".to_string());
                }
            }
            if ctx.push(Value::Int(n)).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        REVN (*) [
            brief: "Reverse top n items",
            stack: "( ... n -- ... )",
            example: "1 2 3 3 REVN",
            see_also: ["ROLL", "ROT"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("REVN: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n <= 1 {
                return EXEC_OK;
            }
            let depth = ctx.depth();
            if n > depth {
                return Err("REVN: stack underflow".to_string());
            }
            let mut items = Vec::with_capacity(n);
            for _ in 0..n {
                items.push(ctx.pop().unwrap());
            }
            for item in items {
                let _ = ctx.push(item);
            }
            EXEC_OK
        }

        UNPICK (*) [
            brief: "Move obj to nth position",
            stack: "( ... obj n -- ... )",
            example: "1 2 3 4 3 UNPICK",
            see_also: ["PICK", "ROLL"],
        ] {
            let n = match ctx.pop() {
                Ok(Value::Real(r)) => r as usize,
                Ok(Value::Int(i)) => i as usize,
                Ok(_) => return Err("UNPICK: expected number".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 0 {
                return Err("UNPICK: n must be >= 1".to_string());
            }
            let obj = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };
            if n == 1 {
                let _ = ctx.push(obj);
                return EXEC_OK;
            }
            let depth = ctx.depth();
            if n - 1 > depth {
                return Err("UNPICK: stack underflow".to_string());
            }
            let mut items = Vec::with_capacity(n - 1);
            for _ in 0..(n - 1) {
                items.push(ctx.pop().unwrap());
            }
            let _ = ctx.push(obj);
            for item in items.into_iter().rev() {
                let _ = ctx.push(item);
            }
            EXEC_OK
        }
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::{
        VM,
        compile::OutputBuffer,
        library::{CompileContext, CompileResult, Library, ProbeContext, ProbeResult, StackEffect},
    };

    use super::*;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &'_ mut VM, cmd: u16) -> rpl_lang::library::ExecuteContext<'_> {
        rpl_lang::library::ExecuteContext::new(vm, &[], 0, cmd)
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
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
        assert!(result.is_ok());
        assert_eq!(vm.depth(), 0);
    }

    #[test]
    fn execute_underflow() {
        let lib = StackLib;
        let mut vm = VM::new();

        let mut ctx = make_exec_ctx(&mut vm, StackLib::CMD_DUP);
        let result = lib.execute(&mut ctx);
        assert!(result.is_err());
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
