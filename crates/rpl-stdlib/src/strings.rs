//! String operations library.
//!
//! Provides string manipulation commands:
//! - NUM: Convert string to number
//! - STR: Convert number to string
//! - CHR: Character code to string
//! - ASC: String to character code
//! - SUB: Substring extraction
//! - POS: Find position of substring
//! - SIZE: String length (also in list lib)
//! - SREV: Reverse string
//! - TRIM, RTRIM: Whitespace trimming
//! - NTOKENS, NTHTOKEN, NTHTOKENPOS: Token operations
//! - SREPL: Search and replace
//! - STRLENCP: String length in code points
//! - UTF8 conversion commands

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::{Branch, LibId},
    libs::{ExecuteContext, ExecuteResult, LibraryImpl},
    lower::{LowerContext, LowerError},
    value::Value,
};

/// Interface declaration for the Strings library.
const INTERFACE: &str = include_str!("interfaces/strings.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid strings interface"))
}

/// Strings library ID (matches rpl-stdlib).
pub const STRINGS_LIB: LibId = 24;

/// Strings library command IDs.
pub mod cmd {
    pub const NUM: u16 = 0;
    pub const STR: u16 = 1;
    pub const CHR: u16 = 2;
    pub const ASC: u16 = 3;
    pub const SUB: u16 = 4;
    pub const POS: u16 = 5;
    pub const SREV: u16 = 6;
    pub const TRIM: u16 = 7;
    pub const RTRIM: u16 = 8;
    pub const STRLENCP: u16 = 9;
    pub const NTOKENS: u16 = 10;
    pub const NTHTOKEN: u16 = 11;
    pub const NTHTOKENPOS: u16 = 12;
    pub const SREPL: u16 = 13;
    pub const TO_UTF8: u16 = 14;
    pub const FROM_UTF8: u16 = 15;
}

/// Strings library (implementation only).
#[derive(Clone, Copy)]
pub struct StringsLib;

impl LibraryImpl for StringsLib {
    fn id(&self) -> LibId {
        STRINGS_LIB
    }

    fn lower_composite(
        &self,
        _construct_id: u16,
        _branches: &[Branch],
        _span: Span,
        _ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        Err(LowerError { span: None,
            message: "Strings library has no composites".into(),
        })
    }

    fn lower_command(
        &self,
        cmd: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        ctx.output.emit_call_lib(STRINGS_LIB, cmd);
        Ok(())
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::NUM => {
                let s = pop_string(ctx)?;
                match s.trim().parse::<f64>() {
                    Ok(n) => {
                        // Return as integer if it's a whole number
                        if n.fract() == 0.0 && n.abs() < 9007199254740992.0 {
                            ctx.push(Value::Integer(n as i64))?;
                        } else {
                            ctx.push(Value::Real(n))?;
                        }
                        Ok(())
                    }
                    Err(_) => Err(format!("Cannot convert '{}' to number", s)),
                }
            }

            cmd::STR => {
                let val = ctx.pop()?;
                let s = match val {
                    Value::Integer(n) => format!("{}", n),
                    Value::Real(n) => {
                        if n.fract() == 0.0 && n.abs() < 1e15 {
                            format!("{}", n as i64)
                        } else {
                            format!("{}", n)
                        }
                    }
                    _ => return Err(format!("STR: expected number, got {}", val.type_name())),
                };
                ctx.push(Value::String(s.into()))?;
                Ok(())
            }

            cmd::CHR => {
                let code = pop_integer(ctx)? as u32;
                match char::from_u32(code) {
                    Some(c) => {
                        ctx.push(Value::String(c.to_string().into()))?;
                        Ok(())
                    }
                    None => Err(format!("Invalid character code: {}", code)),
                }
            }

            cmd::ASC => {
                let s = pop_string(ctx)?;
                match s.chars().next() {
                    Some(c) => {
                        ctx.push(Value::Integer(c as u32 as i64))?;
                        Ok(())
                    }
                    None => Err("ASC: empty string".into()),
                }
            }

            cmd::SUB => {
                let length = pop_integer(ctx)? as usize;
                let start = pop_integer(ctx)? as usize;
                let s = pop_string(ctx)?;

                if start == 0 {
                    return Err("SUB: index starts at 1".into());
                }
                let start_idx = start - 1;

                let chars: Vec<char> = s.chars().collect();
                let end_idx = (start_idx + length).min(chars.len());
                let substring: String = chars
                    .get(start_idx..end_idx)
                    .unwrap_or(&[])
                    .iter()
                    .collect();

                ctx.push(Value::String(substring.into()))?;
                Ok(())
            }

            cmd::POS => {
                let pattern = pop_string(ctx)?;
                let s = pop_string(ctx)?;

                let pos = match s.find(pattern.as_ref()) {
                    Some(byte_pos) => s[..byte_pos].chars().count() + 1,
                    None => 0,
                };

                ctx.push(Value::Integer(pos as i64))?;
                Ok(())
            }

            cmd::SREV => {
                let s = pop_string(ctx)?;
                let reversed: String = s.chars().rev().collect();
                ctx.push(Value::String(reversed.into()))?;
                Ok(())
            }

            cmd::TRIM => {
                let s = pop_string(ctx)?;
                ctx.push(Value::String(s.trim().to_string().into()))?;
                Ok(())
            }

            cmd::RTRIM => {
                let s = pop_string(ctx)?;
                ctx.push(Value::String(s.trim_end().to_string().into()))?;
                Ok(())
            }

            cmd::STRLENCP => {
                let s = pop_string(ctx)?;
                let len = s.chars().count();
                ctx.push(Value::Integer(len as i64))?;
                Ok(())
            }

            cmd::NTOKENS => {
                let s = pop_string(ctx)?;
                let count = s.split_whitespace().count();
                ctx.push(Value::Integer(count as i64))?;
                Ok(())
            }

            cmd::NTHTOKEN => {
                let n = pop_integer(ctx)? as usize;
                let s = pop_string(ctx)?;

                if n == 0 {
                    return Err("NTHTOKEN: index starts at 1".into());
                }

                match s.split_whitespace().nth(n - 1) {
                    Some(token) => {
                        ctx.push(Value::String(token.to_string().into()))?;
                        Ok(())
                    }
                    None => Err(format!("NTHTOKEN: token {} not found", n)),
                }
            }

            cmd::NTHTOKENPOS => {
                let n = pop_integer(ctx)? as usize;
                let s = pop_string(ctx)?;

                if n == 0 {
                    return Err("NTHTOKENPOS: index starts at 1".into());
                }

                let mut token_count = 0;
                let mut in_token = false;
                let mut token_start = 0;

                for (i, c) in s.char_indices() {
                    if c.is_whitespace() {
                        if in_token {
                            token_count += 1;
                            if token_count == n {
                                let start_char = s[..token_start].chars().count() + 1;
                                let end_char = s[..i].chars().count();
                                ctx.push(Value::Integer(start_char as i64))?;
                                ctx.push(Value::Integer(end_char as i64))?;
                                return Ok(());
                            }
                            in_token = false;
                        }
                    } else if !in_token {
                        in_token = true;
                        token_start = i;
                    }
                }

                if in_token {
                    token_count += 1;
                    if token_count == n {
                        let start_char = s[..token_start].chars().count() + 1;
                        let end_char = s.chars().count();
                        ctx.push(Value::Integer(start_char as i64))?;
                        ctx.push(Value::Integer(end_char as i64))?;
                        return Ok(());
                    }
                }

                Err(format!("NTHTOKENPOS: token {} not found", n))
            }

            cmd::SREPL => {
                let replace = pop_string(ctx)?;
                let find = pop_string(ctx)?;
                let s = pop_string(ctx)?;

                if find.is_empty() {
                    ctx.push(Value::String(s))?;
                    ctx.push(Value::Integer(0))?;
                    return Ok(());
                }

                let count = s.matches(find.as_ref()).count();
                let result = s.replace(find.as_ref(), replace.as_ref());

                ctx.push(Value::String(result.into()))?;
                ctx.push(Value::Integer(count as i64))?;
                Ok(())
            }

            cmd::TO_UTF8 => {
                let s = pop_string(ctx)?;
                let bytes: Vec<Value> = s
                    .as_bytes()
                    .iter()
                    .map(|&b| Value::Integer(b as i64))
                    .collect();
                ctx.push(Value::list(bytes))?;
                Ok(())
            }

            cmd::FROM_UTF8 => {
                let list = ctx.pop()?;
                let items = match &list {
                    Value::List(items) => items,
                    _ => return Err(format!("UTF8→: expected list, got {}", list.type_name())),
                };

                let mut bytes = Vec::with_capacity(items.len());
                for v in items.iter() {
                    match v {
                        Value::Integer(i) => {
                            if !(0..=255).contains(i) {
                                return Err(format!("UTF8→: byte value out of range: {}", i));
                            }
                            bytes.push(*i as u8);
                        }
                        Value::Real(r) => {
                            let i = *r as i64;
                            if !(0..=255).contains(&i) {
                                return Err(format!("UTF8→: byte value out of range: {}", r));
                            }
                            bytes.push(i as u8);
                        }
                        _ => {
                            return Err("UTF8→: list must contain only integers".into());
                        }
                    }
                }

                match String::from_utf8(bytes) {
                    Ok(s) => {
                        ctx.push(Value::String(s.into()))?;
                        Ok(())
                    }
                    Err(_) => Err("UTF8→: invalid UTF-8 byte sequence".into()),
                }
            }

            _ => Err(format!("Unknown strings command: {}", ctx.cmd)),
        }
    }
}

// Helper functions

fn pop_string(ctx: &mut ExecuteContext) -> Result<std::sync::Arc<str>, String> {
    match ctx.pop()? {
        Value::String(s) => Ok(s),
        other => Err(format!("Expected string, got {}", other.type_name())),
    }
}

fn pop_integer(ctx: &mut ExecuteContext) -> Result<i64, String> {
    match ctx.pop()? {
        Value::Integer(i) => Ok(i),
        Value::Real(r) => Ok(r as i64),
        other => Err(format!("Expected number, got {}", other.type_name())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strings_lib_id() {
        assert_eq!(interface().id(), 24);
    }

    #[test]
    fn strings_lib_name() {
        assert_eq!(interface().name(), "Strings");
    }

    #[test]
    fn commands_registered() {
        let commands = interface().to_command_infos();
        let names: Vec<_> = commands.iter().map(|c| c.name).collect();

        assert!(names.contains(&"NUM"));
        assert!(names.contains(&"STR"));
        assert!(names.contains(&"CHR"));
        assert!(names.contains(&"ASC"));
        assert!(names.contains(&"SUB"));
        assert!(names.contains(&"POS"));
        assert!(names.contains(&"SREV"));
        assert!(names.contains(&"TRIM"));
    }
}
