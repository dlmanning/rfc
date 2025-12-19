//! List operations library.
//!
//! Provides operations on lists:
//! - →LIST: Create list from N items on stack
//! - LIST→: Explode list onto stack
//! - SIZE: Get list size
//! - GET: Get element at index
//! - PUT: Set element at index
//! - HEAD: Get first element
//! - TAIL: Get all but first element
//! - REVLIST: Reverse list

use std::sync::OnceLock;

use rpl::interface::InterfaceSpec;

use rpl::{
    core::Span,
    ir::LibId,
    libs::{ExecuteContext, ExecuteResult, LibraryImpl},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the List library.
const INTERFACE: &str = include_str!("interfaces/list.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid list interface"))
}

/// List library ID.
pub const LIST_LIB: LibId = 88;

/// List library command IDs.
pub mod cmd {
    /// Create list from N items (→LIST).
    pub const TO_LIST: u16 = 0;
    /// Explode list onto stack (LIST→).
    pub const LIST_TO: u16 = 1;
    /// Get list size (SIZE).
    pub const SIZE: u16 = 2;
    /// Get element at index (GET).
    pub const GET: u16 = 3;
    /// Set element at index (PUT).
    pub const PUT: u16 = 4;
    /// Get first element (HEAD).
    pub const HEAD: u16 = 5;
    /// Get all but first (TAIL).
    pub const TAIL: u16 = 6;
    /// Reverse list (REVLIST).
    pub const REVLIST: u16 = 7;
}

/// List operations library (implementation only).
#[derive(Clone, Copy)]
pub struct ListLib;

impl LibraryImpl for ListLib {
    fn id(&self) -> LibId {
        LIST_LIB
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(LIST_LIB, cmd);
        Ok(())
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::TO_LIST => {
                // →LIST: (items... n -- { items })
                let n = match ctx.pop()? {
                    Value::Integer(i) if i >= 0 => i as usize,
                    Value::Real(r) if r >= 0.0 => r as usize,
                    _ => return Err("→LIST: expected non-negative integer".into()),
                };
                let mut items = Vec::with_capacity(n);
                for _ in 0..n {
                    items.push(ctx.pop()?);
                }
                items.reverse(); // Pop order is reversed
                ctx.push(Value::list(items))?;
                Ok(())
            }

            cmd::LIST_TO => {
                // LIST→: ({ items } -- items... n)
                let list = ctx.pop()?;
                let items = match &list {
                    Value::List(items) => items,
                    _ => return Err(format!("LIST→: expected list, got {}", list.type_name())),
                };
                let n = items.len();
                for item in items.iter() {
                    ctx.push(item.clone())?;
                }
                ctx.push(Value::Integer(n as i64))?;
                Ok(())
            }

            cmd::SIZE => {
                // SIZE: ({ items } -- n)
                let list = ctx.pop()?;
                let n = match &list {
                    Value::List(items) => items.len(),
                    Value::String(s) => s.len(),
                    _ => {
                        return Err(format!(
                            "SIZE: expected list or string, got {}",
                            list.type_name()
                        ));
                    }
                };
                ctx.push(Value::Integer(n as i64))?;
                Ok(())
            }

            cmd::GET => {
                // GET: ({ items } n -- item)
                let index = match ctx.pop()? {
                    Value::Integer(i) => i,
                    Value::Real(r) => r as i64,
                    _ => return Err("GET: expected integer index".into()),
                };
                let list = ctx.pop()?;
                let items = match &list {
                    Value::List(items) => items,
                    _ => return Err(format!("GET: expected list, got {}", list.type_name())),
                };
                // 1-based indexing (HP style)
                if index < 1 || index as usize > items.len() {
                    return Err(format!(
                        "GET: index {} out of range (1..{})",
                        index,
                        items.len()
                    ));
                }
                ctx.push(items[(index - 1) as usize].clone())?;
                Ok(())
            }

            cmd::PUT => {
                // PUT: ({ items } n item -- { new_items })
                // HP stack: level 3=list, level 2=index, level 1=value
                let new_item = ctx.pop()?;
                let index = match ctx.pop()? {
                    Value::Integer(i) => i,
                    Value::Real(r) => r as i64,
                    _ => return Err("PUT: expected integer index".into()),
                };
                let list = ctx.pop()?;
                let items = match &list {
                    Value::List(items) => items,
                    _ => return Err(format!("PUT: expected list, got {}", list.type_name())),
                };
                // 1-based indexing (HP style)
                if index < 1 || index as usize > items.len() {
                    return Err(format!(
                        "PUT: index {} out of range (1..{})",
                        index,
                        items.len()
                    ));
                }
                let mut new_items: Vec<Value> = items.iter().cloned().collect();
                new_items[(index - 1) as usize] = new_item;
                ctx.push(Value::list(new_items))?;
                Ok(())
            }

            cmd::HEAD => {
                // HEAD: ({ items } -- item) or (string -- first_char)
                let val = ctx.pop()?;
                match &val {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err("HEAD: empty list".into());
                        }
                        ctx.push(items[0].clone())?;
                    }
                    Value::String(s) => {
                        if s.is_empty() {
                            return Err("HEAD: empty string".into());
                        }
                        // Return first character as string
                        let first: String = s.chars().next().unwrap().to_string();
                        ctx.push(Value::string(first))?;
                    }
                    _ => {
                        return Err(format!(
                            "HEAD: expected list or string, got {}",
                            val.type_name()
                        ));
                    }
                }
                Ok(())
            }

            cmd::TAIL => {
                // TAIL: ({ items } -- { rest }) or (string -- rest_string)
                let val = ctx.pop()?;
                match &val {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err("TAIL: empty list".into());
                        }
                        let rest: Vec<Value> = items.iter().skip(1).cloned().collect();
                        ctx.push(Value::list(rest))?;
                    }
                    Value::String(s) => {
                        if s.is_empty() {
                            return Err("TAIL: empty string".into());
                        }
                        // Return all but first character
                        let rest: String = s.chars().skip(1).collect();
                        ctx.push(Value::string(rest))?;
                    }
                    _ => {
                        return Err(format!(
                            "TAIL: expected list or string, got {}",
                            val.type_name()
                        ));
                    }
                }
                Ok(())
            }

            cmd::REVLIST => {
                // REVLIST: ({ items } -- { reversed })
                let list = ctx.pop()?;
                let items = match &list {
                    Value::List(items) => items,
                    _ => return Err(format!("REVLIST: expected list, got {}", list.type_name())),
                };
                let reversed: Vec<Value> = items.iter().rev().cloned().collect();
                ctx.push(Value::list(reversed))?;
                Ok(())
            }

            _ => Err(format!("Unknown list command: {}", ctx.cmd)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_lib_id() {
        assert_eq!(interface().id(), 88);
    }

    #[test]
    fn list_lib_name() {
        assert_eq!(interface().name(), "List");
    }
}
