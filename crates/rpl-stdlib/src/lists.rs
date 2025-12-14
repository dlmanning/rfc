//! Library for list constructs and operations.
//!
//! This library provides:
//! - `{` - Start a list construct
//! - `}` - End a list construct
//! - `LIST→` - Explode list onto stack
//! - `→LIST` - Build list from stack
//! - `HEAD` - First element
//! - `TAIL` - All but first
//! - `SIZE` - Element count
//! - `GET` - Get element by index (1-based)
//! - `PUT` - Put element at index

use rpl_core::TypeId;
use rpl_lang::library::EXEC_OK;
use rpl_lang::Value;

rpl_macros::define_library! {
    pub library ListsLib(62, "Lists");

    constructs {
        "{": open(List);
        "}": close;
    }

    commands {
        // LIST→: Explode list onto stack, push count
        LISTTO | "LIST→" | "LIST->" | "OBJ→" | "OBJ->" (*) "Explode list onto stack" {
            let list = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match list {
                Value::List(elements) => {
                    let count = elements.len();
                    for elem in elements {
                        if ctx.push(elem).is_err() {
                            return Err("Stack overflow".to_string());
                        }
                    }
                    if ctx.push_real(count as f64).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                Value::Object {
                    type_id: TypeId::LIST,
                    ..
                } => {
                    // Raw list object - need to execute its body first
                    // For now, error
                    Err("Raw list object not yet supported".to_string())
                }
                _ => Err("Expected list for LIST→".to_string()),
            }
        }

        // →LIST: Pop count, pop that many items, make list
        TOLIST | "→LIST" | "->LIST" (*) "Build list from stack" {
            let count = match ctx.pop_real() {
                Ok(n) => n as usize,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let mut elements = Vec::with_capacity(count);
            for _ in 0..count {
                match ctx.pop() {
                    Ok(v) => elements.push(v),
                    Err(_) => return Err("Stack underflow".to_string()),
                }
            }
            elements.reverse(); // Elements were popped in reverse order

            if ctx.push(Value::List(elements)).is_err() {
                return Err("Stack overflow".to_string());
            }
            EXEC_OK
        }

        // HEAD: Get first element (works on lists and strings)
        HEAD (1 -> 1) "Get first element" {
            let val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match val {
                Value::List(elements) => {
                    if elements.is_empty() {
                        return Err("Empty list".to_string());
                    }
                    if ctx.push(elements.into_iter().next().unwrap()).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                Value::String(s) => {
                    if s.is_empty() {
                        return Err("Empty string".to_string());
                    }
                    let first = s.chars().next().unwrap().to_string();
                    if ctx.push(Value::String(first)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list or string for HEAD".to_string()),
            }
        }

        // TAIL: Get all but first element (works on lists and strings)
        TAIL (1 -> 1) "Get all but first element" {
            let val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match val {
                Value::List(mut elements) => {
                    if elements.is_empty() {
                        return Err("Empty list".to_string());
                    }
                    elements.remove(0);
                    if ctx.push(Value::List(elements)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                Value::String(s) => {
                    if s.is_empty() {
                        return Err("Empty string".to_string());
                    }
                    let tail: String = s.chars().skip(1).collect();
                    if ctx.push(Value::String(tail)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list or string for TAIL".to_string()),
            }
        }

        // SIZE: Get element count (works on lists and strings)
        SIZE (1 -> 1) "Get element count" {
            let val = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match val {
                Value::List(elements) => {
                    if ctx.push_real(elements.len() as f64).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                Value::String(s) => {
                    if ctx.push_real(s.chars().count() as f64).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list or string for SIZE".to_string()),
            }
        }

        // GET: Get element by 1-based index
        GET (2 -> 1) "Get element by index" {
            let index = match ctx.pop_real() {
                Ok(n) => n as usize,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let list = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match list {
                Value::List(elements) => {
                    if index == 0 || index > elements.len() {
                        return Err(format!(
                            "Index {} out of bounds for list of size {}",
                            index,
                            elements.len()
                        ));
                    }
                    if ctx.push(elements[index - 1].clone()).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list for GET".to_string()),
            }
        }

        // PUT: Put element at 1-based index
        PUT (3 -> 1) "Put element at index" {
            let value = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let index = match ctx.pop_real() {
                Ok(n) => n as usize,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let list = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match list {
                Value::List(mut elements) => {
                    if index == 0 || index > elements.len() {
                        return Err(format!(
                            "Index {} out of bounds for list of size {}",
                            index,
                            elements.len()
                        ));
                    }
                    elements[index - 1] = value;
                    if ctx.push(Value::List(elements)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list for PUT".to_string()),
            }
        }

        // REVLIST: Reverse list
        REVLIST (1 -> 1) "Reverse list" {
            let list = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match list {
                Value::List(mut elements) => {
                    elements.reverse();
                    if ctx.push(Value::List(elements)).is_err() {
                        return Err("Stack overflow".to_string());
                    }
                    EXEC_OK
                }
                _ => Err("Expected list for REVLIST".to_string()),
            }
        }
    }

    prologs {
        LIST: delimited("{", "}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::{SemanticKind, TokenType};
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{
        CompileContext, CompileResult, ConstructKind, Library, ProbeContext, ProbeResult,
        StackEffect,
    };

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_list_open() {
        let interner = Interner::new();
        let lib = ListsLib;
        let ctx = make_probe_ctx("{", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::OpenBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_list_close() {
        let interner = Interner::new();
        let lib = ListsLib;
        let ctx = make_probe_ctx("}", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::CloseBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_commands() {
        let interner = Interner::new();
        let lib = ListsLib;

        for cmd in &["HEAD", "TAIL", "SIZE", "GET", "PUT", "REVLIST"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                cmd
            );
        }
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = ListsLib;

        for name in &["head", "Head", "HEAD", "tail", "Tail", "TAIL"] {
            let ctx = make_probe_ctx(name, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                name
            );
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = ListsLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_delimiters() {
        let lib = ListsLib;
        assert!(matches!(lib.stack_effect("{"), StackEffect::StartConstruct));
        assert!(matches!(lib.stack_effect("}"), StackEffect::EndConstruct));
    }

    #[test]
    fn stack_effect_commands() {
        let lib = ListsLib;
        assert!(matches!(
            lib.stack_effect("HEAD"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("SIZE"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("GET"),
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        ));
    }

    #[test]
    fn compile_list_start() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ListsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        let mut ctx = CompileContext::new(span, "{", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(
            result,
            CompileResult::StartConstruct {
                kind: ConstructKind::List
            }
        ));
    }

    #[test]
    fn compile_list_end() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ListsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        let mut ctx = CompileContext::new(span, "}", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::EndConstruct));
    }

    #[test]
    fn compile_command() {
        use rpl_lang::compile::OutputBuffer;

        let lib = ListsLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(4));

        let mut ctx = CompileContext::new(span, "HEAD", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, CompileResult::Ok));
        assert_eq!(ctx.position(), 1);
    }
}
