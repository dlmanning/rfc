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

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{
    CompileContext, CompileResult, ConstructKind, DecompileContext, DecompileResult,
    ExecuteContext, ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

/// Library for list constructs and operations.
pub struct ListsLib;

impl ListsLib {
    /// Library ID for lists.
    pub const ID: LibraryId = LibraryId::new(62);

    // Command IDs
    const CMD_LIST_TO: u16 = 0; // LIST→
    const CMD_TO_LIST: u16 = 1; // →LIST
    const CMD_HEAD: u16 = 2;
    const CMD_TAIL: u16 = 3;
    const CMD_SIZE: u16 = 4;
    const CMD_GET: u16 = 5;
    const CMD_PUT: u16 = 6;
    const CMD_REVLIST: u16 = 7;

    /// Get command ID from name.
    fn command_id(text: &str) -> Option<u16> {
        let upper = text.to_ascii_uppercase();
        match upper.as_str() {
            "LIST→" | "LIST->" | "OBJ→" | "OBJ->" => Some(Self::CMD_LIST_TO),
            "→LIST" | "->LIST" => Some(Self::CMD_TO_LIST),
            "HEAD" => Some(Self::CMD_HEAD),
            "TAIL" => Some(Self::CMD_TAIL),
            "SIZE" => Some(Self::CMD_SIZE),
            "GET" => Some(Self::CMD_GET),
            "PUT" => Some(Self::CMD_PUT),
            "REVLIST" => Some(Self::CMD_REVLIST),
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_LIST_TO => Some("LIST→"),
            Self::CMD_TO_LIST => Some("→LIST"),
            Self::CMD_HEAD => Some("HEAD"),
            Self::CMD_TAIL => Some("TAIL"),
            Self::CMD_SIZE => Some("SIZE"),
            Self::CMD_GET => Some("GET"),
            Self::CMD_PUT => Some("PUT"),
            Self::CMD_REVLIST => Some("REVLIST"),
            _ => None,
        }
    }
}

impl Library for ListsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Lists"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        match text {
            "{" => ProbeResult::Match {
                info: TokenInfo::open_bracket(1),
                semantic: SemanticKind::Bracket,
            },
            "}" => ProbeResult::Match {
                info: TokenInfo::close_bracket(1),
                semantic: SemanticKind::Bracket,
            },
            _ => {
                if Self::command_id(text).is_some() {
                    ProbeResult::Match {
                        info: TokenInfo::atom(text.len() as u8),
                        semantic: SemanticKind::Command,
                    }
                } else {
                    ProbeResult::NoMatch
                }
            }
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        match text {
            "{" => {
                // Start list construct - compiler will emit LIST prolog
                CompileResult::StartConstruct {
                    kind: ConstructKind::List,
                }
            }
            "}" => {
                // End list construct - compiler will patch prolog size
                CompileResult::EndConstruct
            }
            _ => {
                if let Some(cmd_id) = Self::command_id(text) {
                    ctx.emit_opcode(Self::ID.as_u16(), cmd_id);
                    CompileResult::Ok
                } else {
                    CompileResult::NoMatch
                }
            }
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_LIST_TO => {
                // LIST→: Explode list onto stack, push count
                let list = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match list {
                    Value::List(elements) => {
                        let count = elements.len();
                        for elem in elements {
                            if ctx.push(elem).is_err() {
                                return ExecuteResult::Error("Stack overflow".to_string());
                            }
                        }
                        if ctx.push_real(count as f64).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Value::Object {
                        type_id: TypeId::LIST,
                        ..
                    } => {
                        // Raw list object - need to execute its body first
                        // For now, error
                        ExecuteResult::Error("Raw list object not yet supported".to_string())
                    }
                    _ => ExecuteResult::Error("Expected list for LIST→".to_string()),
                }
            }
            Self::CMD_TO_LIST => {
                // →LIST: Pop count, pop that many items, make list
                let count = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let mut elements = Vec::with_capacity(count);
                for _ in 0..count {
                    match ctx.pop() {
                        Ok(v) => elements.push(v),
                        Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                    }
                }
                elements.reverse(); // Elements were popped in reverse order

                if ctx.push(Value::List(elements)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_HEAD => {
                // HEAD: Get first element (works on lists and strings)
                let val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match val {
                    Value::List(elements) => {
                        if elements.is_empty() {
                            return ExecuteResult::Error("Empty list".to_string());
                        }
                        if ctx.push(elements.into_iter().next().unwrap()).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Value::String(s) => {
                        if s.is_empty() {
                            return ExecuteResult::Error("Empty string".to_string());
                        }
                        let first = s.chars().next().unwrap().to_string();
                        if ctx.push(Value::String(first)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list or string for HEAD".to_string()),
                }
            }
            Self::CMD_TAIL => {
                // TAIL: Get all but first element (works on lists and strings)
                let val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match val {
                    Value::List(mut elements) => {
                        if elements.is_empty() {
                            return ExecuteResult::Error("Empty list".to_string());
                        }
                        elements.remove(0);
                        if ctx.push(Value::List(elements)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Value::String(s) => {
                        if s.is_empty() {
                            return ExecuteResult::Error("Empty string".to_string());
                        }
                        let tail: String = s.chars().skip(1).collect();
                        if ctx.push(Value::String(tail)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list or string for TAIL".to_string()),
                }
            }
            Self::CMD_SIZE => {
                // SIZE: Get element count (works on lists and strings)
                let val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match val {
                    Value::List(elements) => {
                        if ctx.push_real(elements.len() as f64).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Value::String(s) => {
                        if ctx.push_real(s.chars().count() as f64).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list or string for SIZE".to_string()),
                }
            }
            Self::CMD_GET => {
                // GET: Get element by 1-based index
                // Stack: list index -> element
                let index = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let list = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match list {
                    Value::List(elements) => {
                        if index == 0 || index > elements.len() {
                            return ExecuteResult::Error(format!(
                                "Index {} out of bounds for list of size {}",
                                index,
                                elements.len()
                            ));
                        }
                        if ctx.push(elements[index - 1].clone()).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list for GET".to_string()),
                }
            }
            Self::CMD_PUT => {
                // PUT: Put element at 1-based index
                // Stack: list index value -> list'
                let value = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let index = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let list = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match list {
                    Value::List(mut elements) => {
                        if index == 0 || index > elements.len() {
                            return ExecuteResult::Error(format!(
                                "Index {} out of bounds for list of size {}",
                                index,
                                elements.len()
                            ));
                        }
                        elements[index - 1] = value;
                        if ctx.push(Value::List(elements)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list for PUT".to_string()),
                }
            }
            Self::CMD_REVLIST => {
                // REVLIST: Reverse list
                let list = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match list {
                    Value::List(mut elements) => {
                        elements.reverse();
                        if ctx.push(Value::List(elements)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    _ => ExecuteResult::Error("Expected list for REVLIST".to_string()),
                }
            }
            _ => ExecuteResult::Error(format!("Unknown list command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for LIST prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::LIST.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read(); // consume prolog
                    ctx.write("{");

                    // Recursively decompile the list body
                    if size > 0 {
                        ctx.write(" ");
                        ctx.decompile_inner(size);
                    }

                    ctx.write(" }");
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
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
        match token {
            "{" => StackEffect::StartConstruct,
            "}" => StackEffect::EndConstruct,
            _ => {
                let upper = token.to_ascii_uppercase();
                match upper.as_str() {
                    "LIST→" | "LIST->" | "OBJ→" | "OBJ->" => StackEffect::Dynamic, // produces N+1 values
                    "→LIST" | "->LIST" => StackEffect::Dynamic,                    // consumes N+1 values
                    "HEAD" => StackEffect::Fixed {
                        consumes: 1,
                        produces: 1,
                    },
                    "TAIL" => StackEffect::Fixed {
                        consumes: 1,
                        produces: 1,
                    },
                    "SIZE" => StackEffect::Fixed {
                        consumes: 1,
                        produces: 1,
                    },
                    "GET" => StackEffect::Fixed {
                        consumes: 2,
                        produces: 1,
                    },
                    "PUT" => StackEffect::Fixed {
                        consumes: 3,
                        produces: 1,
                    },
                    "REVLIST" => StackEffect::Fixed {
                        consumes: 1,
                        produces: 1,
                    },
                    _ => StackEffect::Dynamic,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_core::token::TokenType;

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
