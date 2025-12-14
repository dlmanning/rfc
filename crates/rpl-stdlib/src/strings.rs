//! Library for string literals and operations.
//!
//! This library provides:
//! - `"..."` - String literals
//! - `NUM` - Convert string to number
//! - `STR` - Convert number to string
//! - `CHR` - Character code to string
//! - `ASC` - String to character code
//! - `SUB` - Substring extraction
//! - `POS` - Find position of substring
//! - `→UTF8` - Convert string to list of UTF-8 bytes
//! - `UTF8→` - Convert list of UTF-8 bytes to string
//! - `SREV` - Reverse string
//! - `NTOKENS` - Count whitespace-separated tokens
//! - `NTHTOKEN` - Extract nth token
//! - `NTHTOKENPOS` - Get start/end positions of nth token
//! - `TRIM` - Remove leading and trailing whitespace
//! - `RTRIM` - Remove trailing whitespace
//! - `STRLENCP` - String length in code points
//! - `SREPL` - Search and replace

use crate::codecs::StringCodec;
use rpl_core::TypeId;
use rpl_lang::library::EXEC_OK;
use rpl_lang::Value;

rpl_macros::define_library! {
    pub library StringsLib(24, "Strings");

    literals {
        string: StringCodec;
    }

    commands {
        NUM (1 -> 1) "Convert string to number" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for NUM".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match s.trim().parse::<f64>() {
                Ok(n) => {
                    ctx.push_real(n).map_err(|_| "Stack overflow")?;
                    EXEC_OK
                }
                Err(_) => Err(format!("Cannot convert '{}' to number", s)),
            }
        }

        STR | "→STR" | "->STR" (1 -> 1) "Convert number to string" {
            let n = ctx.pop_real().map_err(|_| "Stack underflow")?;

            let s = if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{}", n as i64)
            } else {
                format!("{}", n)
            };

            ctx.push(Value::String(s)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        CHR (1 -> 1) "Character code to single-character string" {
            let code = ctx.pop_real().map_err(|_| "Stack underflow")? as u32;

            match char::from_u32(code) {
                Some(c) => {
                    ctx.push(Value::String(c.to_string())).map_err(|_| "Stack overflow")?;
                    EXEC_OK
                }
                None => Err(format!("Invalid character code: {}", code)),
            }
        }

        ASC (1 -> 1) "First character of string to character code" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for ASC".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            match s.chars().next() {
                Some(c) => {
                    ctx.push_real(c as u32 as f64).map_err(|_| "Stack overflow")?;
                    EXEC_OK
                }
                None => Err("Empty string for ASC".to_string()),
            }
        }

        SUB (3 -> 1) "Extract substring (string start length -> substring)" {
            let length = ctx.pop_real().map_err(|_| "Stack underflow")? as usize;
            let start = ctx.pop_real().map_err(|_| "Stack underflow")? as usize;
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for SUB".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if start == 0 {
                return Err("SUB index starts at 1".to_string());
            }
            let start_idx = start - 1;

            let chars: Vec<char> = s.chars().collect();
            let end_idx = (start_idx + length).min(chars.len());
            let substring: String = chars
                .get(start_idx..end_idx)
                .unwrap_or(&[])
                .iter()
                .collect();

            ctx.push(Value::String(substring)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        POS (2 -> 1) "Find position of substring (1-based, 0 if not found)" {
            let pattern = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string pattern for POS".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for POS".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let pos = match s.find(&pattern) {
                Some(byte_pos) => s[..byte_pos].chars().count() + 1,
                None => 0,
            };

            ctx.push_real(pos as f64).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        TOUTF8 | "→UTF8" | "->UTF8" (1 -> 1) "Convert string to list of UTF-8 bytes" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for →UTF8".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let bytes: Vec<Value> = s.as_bytes().iter().map(|&b| Value::Int(b as i64)).collect();
            ctx.push(Value::List(bytes)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        FROMUTF8 | "UTF8→" | "UTF8->" (1 -> 1) "Convert list of UTF-8 bytes to string" {
            let list = match ctx.pop() {
                Ok(Value::List(l)) => l,
                Ok(_) => return Err("Expected list for UTF8→".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let mut bytes = Vec::with_capacity(list.len());
            for v in list {
                match v {
                    Value::Int(i) => {
                        if !(0..=255).contains(&i) {
                            return Err(format!("Byte value out of range: {}", i));
                        }
                        bytes.push(i as u8);
                    }
                    Value::Real(r) => {
                        let i = r as i64;
                        if !(0..=255).contains(&i) {
                            return Err(format!("Byte value out of range: {}", r));
                        }
                        bytes.push(i as u8);
                    }
                    _ => return Err("List must contain only integers for UTF8→".to_string()),
                }
            }

            match String::from_utf8(bytes) {
                Ok(s) => {
                    ctx.push(Value::String(s)).map_err(|_| "Stack overflow")?;
                    EXEC_OK
                }
                Err(_) => Err("Invalid UTF-8 byte sequence".to_string()),
            }
        }

        SREV (1 -> 1) "Reverse string characters" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for SREV".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let reversed: String = s.chars().rev().collect();
            ctx.push(Value::String(reversed)).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        NTOKENS (1 -> 1) "Count whitespace-separated tokens" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for NTOKENS".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let count = s.split_whitespace().count();
            ctx.push_real(count as f64).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        NTHTOKEN (2 -> 1) "Extract the nth token (1-based)" {
            let n = ctx.pop_real().map_err(|_| "Stack underflow")? as usize;
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for NTHTOKEN".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if n == 0 {
                return Err("NTHTOKEN index starts at 1".to_string());
            }

            match s.split_whitespace().nth(n - 1) {
                Some(token) => {
                    ctx.push(Value::String(token.to_string())).map_err(|_| "Stack overflow")?;
                    EXEC_OK
                }
                None => Err(format!("Token {} not found", n)),
            }
        }

        NTHTOKENPOS (2 -> 2) "Get start and end positions of nth token (1-based)" {
            let n = ctx.pop_real().map_err(|_| "Stack underflow")? as usize;
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for NTHTOKENPOS".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if n == 0 {
                return Err("NTHTOKENPOS index starts at 1".to_string());
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
                            ctx.push_real(start_char as f64).map_err(|_| "Stack overflow")?;
                            ctx.push_real(end_char as f64).map_err(|_| "Stack overflow")?;
                            return EXEC_OK;
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
                    ctx.push_real(start_char as f64).map_err(|_| "Stack overflow")?;
                    ctx.push_real(end_char as f64).map_err(|_| "Stack overflow")?;
                    return EXEC_OK;
                }
            }

            Err(format!("Token {} not found", n))
        }

        TRIM (1 -> 1) "Remove leading and trailing whitespace" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for TRIM".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            ctx.push(Value::String(s.trim().to_string())).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        RTRIM (1 -> 1) "Remove trailing whitespace only" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for RTRIM".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            ctx.push(Value::String(s.trim_end().to_string())).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        STRLENCP (1 -> 1) "String length in code points" {
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for STRLENCP".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            let len = s.chars().count();
            ctx.push_real(len as f64).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }

        SREPL (3 -> 2) "Search and replace all occurrences" {
            let replace = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected replacement string for SREPL".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let find = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected search string for SREPL".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };
            let s = match ctx.pop() {
                Ok(Value::String(s)) => s,
                Ok(_) => return Err("Expected string for SREPL".to_string()),
                Err(_) => return Err("Stack underflow".to_string()),
            };

            if find.is_empty() {
                ctx.push(Value::String(s)).map_err(|_| "Stack overflow")?;
                ctx.push_real(0.0).map_err(|_| "Stack overflow")?;
                return EXEC_OK;
            }

            let count = s.matches(&find).count();
            let result = s.replace(&find, &replace);

            ctx.push(Value::String(result)).map_err(|_| "Stack overflow")?;
            ctx.push_real(count as f64).map_err(|_| "Stack overflow")?;
            EXEC_OK
        }
    }

    // STRING decompilation is handled by the `literals` block via StringCodec
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::SemanticKind;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_string_literal() {
        let interner = Interner::new();
        let lib = StringsLib;
        let ctx = make_probe_ctx("\"hello\"", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::String);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_empty_string() {
        let interner = Interner::new();
        let lib = StringsLib;
        let ctx = make_probe_ctx("\"\"", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_string_with_escapes() {
        let interner = Interner::new();
        let lib = StringsLib;
        let ctx = make_probe_ctx("\"hello\\nworld\"", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_commands() {
        let interner = Interner::new();
        let lib = StringsLib;

        for cmd in &["NUM", "STR", "CHR", "ASC"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                cmd
            );
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = StringsLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }
}
