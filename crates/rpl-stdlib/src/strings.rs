//! Library for string literals and operations.
//!
//! This library provides:
//! - `"..."` - String literals
//! - `SIZE` - String length (shared with lists, handled by SIZE command)
//! - `+` - String concatenation (when both args are strings)
//! - `HEAD` - First character
//! - `TAIL` - All but first character
//! - `SUB` - Substring extraction
//! - `POS` - Find position of substring
//! - `NUM` - Convert string to number
//! - `STR` - Convert number to string
//! - `CHR` - Character code to string
//! - `ASC` - String to character code
//! - `→UTF8` - Convert string to list of UTF-8 bytes
//! - `UTF8→` - Convert list of UTF-8 bytes to string
//! - `SREV` - Reverse string
//! - `NTOKENS` - Count whitespace-separated tokens
//! - `NTHTOKEN` - Extract nth token
//! - `NTHTOKENPOS` - Get start/end positions of nth token
//! - `TRIM` - Remove leading and trailing whitespace
//! - `RTRIM` - Remove trailing whitespace
//! - `STRLENCP` - String length in code points (same as STRLEN)
//! - `SREPL` - Search and replace

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::TypeId;
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

/// Library for string operations.
pub struct StringsLib;

impl StringsLib {
    /// Library ID for strings.
    pub const ID: LibraryId = LibraryId::new(24); // Same as TypeId::STRING

    // Command IDs
    #[allow(dead_code)]
    const CMD_STRING_LITERAL: u16 = 0; // Push string from bytecode (reserved)
    const CMD_SIZE: u16 = 1;
    const CMD_CONCAT: u16 = 2; // + for strings
    const CMD_HEAD: u16 = 3;
    const CMD_TAIL: u16 = 4;
    const CMD_SUB: u16 = 5;
    const CMD_POS: u16 = 6;
    const CMD_NUM: u16 = 7;
    const CMD_STR: u16 = 8;
    const CMD_CHR: u16 = 9;
    const CMD_ASC: u16 = 10;
    const CMD_TOUTF8: u16 = 11;
    const CMD_FROMUTF8: u16 = 12;
    const CMD_SREV: u16 = 13;
    const CMD_NTOKENS: u16 = 14;
    const CMD_NTHTOKEN: u16 = 15;
    const CMD_NTHTOKENPOS: u16 = 16;
    const CMD_TRIM: u16 = 17;
    const CMD_RTRIM: u16 = 18;
    const CMD_STRLENCP: u16 = 19;
    const CMD_SREPL: u16 = 20;

    /// Get command ID from name.
    fn command_id(text: &str) -> Option<u16> {
        let upper = text.to_ascii_uppercase();
        match upper.as_str() {
            "NUM" => Some(Self::CMD_NUM),
            "STR" | "→STR" | "->STR" => Some(Self::CMD_STR),
            "CHR" => Some(Self::CMD_CHR),
            "ASC" => Some(Self::CMD_ASC),
            "SUB" => Some(Self::CMD_SUB),
            "POS" => Some(Self::CMD_POS),
            "→UTF8" | "->UTF8" | "TOUTF8" => Some(Self::CMD_TOUTF8),
            "UTF8→" | "UTF8->" | "FROMUTF8" => Some(Self::CMD_FROMUTF8),
            "SREV" => Some(Self::CMD_SREV),
            "NTOKENS" => Some(Self::CMD_NTOKENS),
            "NTHTOKEN" => Some(Self::CMD_NTHTOKEN),
            "NTHTOKENPOS" => Some(Self::CMD_NTHTOKENPOS),
            "TRIM" => Some(Self::CMD_TRIM),
            "RTRIM" => Some(Self::CMD_RTRIM),
            "STRLENCP" => Some(Self::CMD_STRLENCP),
            "SREPL" => Some(Self::CMD_SREPL),
            // Note: SIZE, HEAD, TAIL are handled by ListsLib for strings too
            _ => None,
        }
    }

    /// Get command name from ID.
    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_SIZE => Some("SIZE"),
            Self::CMD_CONCAT => Some("+"),
            Self::CMD_HEAD => Some("HEAD"),
            Self::CMD_TAIL => Some("TAIL"),
            Self::CMD_SUB => Some("SUB"),
            Self::CMD_POS => Some("POS"),
            Self::CMD_NUM => Some("NUM"),
            Self::CMD_STR => Some("→STR"),
            Self::CMD_CHR => Some("CHR"),
            Self::CMD_ASC => Some("ASC"),
            Self::CMD_TOUTF8 => Some("→UTF8"),
            Self::CMD_FROMUTF8 => Some("UTF8→"),
            Self::CMD_SREV => Some("SREV"),
            Self::CMD_NTOKENS => Some("NTOKENS"),
            Self::CMD_NTHTOKEN => Some("NTHTOKEN"),
            Self::CMD_NTHTOKENPOS => Some("NTHTOKENPOS"),
            Self::CMD_TRIM => Some("TRIM"),
            Self::CMD_RTRIM => Some("RTRIM"),
            Self::CMD_STRLENCP => Some("STRLENCP"),
            Self::CMD_SREPL => Some("SREPL"),
            _ => None,
        }
    }

    /// Check if text starts with a string delimiter.
    fn is_string_start(text: &str) -> bool {
        text.starts_with('"')
    }

    /// Parse a string literal, returning the content and whether it's complete.
    fn parse_string(text: &str) -> Option<(&str, bool)> {
        if !text.starts_with('"') {
            return None;
        }

        // Find closing quote (not escaped)
        let content = &text[1..];
        let chars = content.char_indices();
        let mut escaped = false;

        for (i, c) in chars {
            if escaped {
                escaped = false;
                continue;
            }
            if c == '\\' {
                escaped = true;
                continue;
            }
            if c == '"' {
                // Found closing quote
                return Some((&content[..i], true));
            }
        }

        // No closing quote found - incomplete string
        Some((content, false))
    }
}

impl Library for StringsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Strings"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        // Check for string literal
        if Self::is_string_start(text)
            && let Some((_, complete)) = Self::parse_string(text)
        {
            if complete {
                return ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::String,
                };
            } else {
                // Incomplete string - mark as invalid
                return ProbeResult::Match {
                    info: TokenInfo::atom(text.len() as u8),
                    semantic: SemanticKind::Invalid,
                };
            }
        }

        // Check for string commands
        if Self::command_id(text).is_some() {
            return ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Command,
            };
        }

        ProbeResult::NoMatch
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text();

        // Check for string literal
        if Self::is_string_start(text)
            && let Some((content, complete)) = Self::parse_string(text)
        {
            if !complete {
                return CompileResult::Error {
                    message: "Unterminated string literal".to_string(),
                };
            }

            // Process escape sequences
            let processed = process_escapes(content);

            // Encode string in bytecode:
            // - Prolog with STRING type and size in words
            // - String bytes packed into words
            let bytes = processed.as_bytes();
            let word_count = bytes.len().div_ceil(4); // Round up to words

            // Emit prolog
            ctx.emit(rpl_core::make_prolog(
                TypeId::STRING.as_u16(),
                word_count as u16,
            ));

            // Pack bytes into words (little-endian)
            for chunk in bytes.chunks(4) {
                let mut word: u32 = 0;
                for (i, &byte) in chunk.iter().enumerate() {
                    word |= (byte as u32) << (i * 8);
                }
                ctx.emit(word);
            }

            return CompileResult::Ok;
        }

        // Check for string commands
        if let Some(cmd_id) = Self::command_id(text) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd_id);
            return CompileResult::Ok;
        }

        CompileResult::NoMatch
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_NUM => {
                // NUM: Convert string to number
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for NUM".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match s.trim().parse::<f64>() {
                    Ok(n) => {
                        if ctx.push_real(n).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error(format!("Cannot convert '{}' to number", s)),
                }
            }
            Self::CMD_STR => {
                // STR: Convert number to string
                let n = match ctx.pop_real() {
                    Ok(n) => n,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let s = if n.fract() == 0.0 && n.abs() < 1e15 {
                    format!("{}", n as i64)
                } else {
                    format!("{}", n)
                };

                if ctx.push(Value::String(s)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_CHR => {
                // CHR: Character code to single-character string
                let code = match ctx.pop_real() {
                    Ok(n) => n as u32,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match char::from_u32(code) {
                    Some(c) => {
                        if ctx.push(Value::String(c.to_string())).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Invalid character code: {}", code)),
                }
            }
            Self::CMD_ASC => {
                // ASC: First character of string to character code
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for ASC".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                match s.chars().next() {
                    Some(c) => {
                        if ctx.push_real(c as u32 as f64).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error("Empty string for ASC".to_string()),
                }
            }
            Self::CMD_SUB => {
                // SUB: Extract substring
                // Stack: string start length -> substring (1-based)
                let length = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let start = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for SUB".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Convert to 0-based index
                if start == 0 {
                    return ExecuteResult::Error("SUB index starts at 1".to_string());
                }
                let start_idx = start - 1;

                // Extract substring using chars for Unicode safety
                let chars: Vec<char> = s.chars().collect();
                let end_idx = (start_idx + length).min(chars.len());
                let substring: String = chars
                    .get(start_idx..end_idx)
                    .unwrap_or(&[])
                    .iter()
                    .collect();

                if ctx.push(Value::String(substring)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_POS => {
                // POS: Find position of substring (1-based, 0 if not found)
                // Stack: string pattern -> position
                let pattern = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected string pattern for POS".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for POS".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                // Find position (1-based, 0 if not found)
                let pos = match s.find(&pattern) {
                    Some(byte_pos) => {
                        // Convert byte position to character position (1-based)
                        s[..byte_pos].chars().count() + 1
                    }
                    None => 0,
                };

                if ctx.push_real(pos as f64).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_TOUTF8 => {
                // →UTF8: Convert string to list of UTF-8 byte values
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for →UTF8".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let bytes: Vec<Value> = s.as_bytes().iter().map(|&b| Value::Int(b as i64)).collect();
                if ctx.push(Value::List(bytes)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_FROMUTF8 => {
                // UTF8→: Convert list of UTF-8 byte values to string
                let list = match ctx.pop() {
                    Ok(Value::List(l)) => l,
                    Ok(_) => return ExecuteResult::Error("Expected list for UTF8→".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let mut bytes = Vec::with_capacity(list.len());
                for v in list {
                    match v {
                        Value::Int(i) => {
                            if !(0..=255).contains(&i) {
                                return ExecuteResult::Error(format!(
                                    "Byte value out of range: {}",
                                    i
                                ));
                            }
                            bytes.push(i as u8);
                        }
                        Value::Real(r) => {
                            let i = r as i64;
                            if !(0..=255).contains(&i) {
                                return ExecuteResult::Error(format!(
                                    "Byte value out of range: {}",
                                    r
                                ));
                            }
                            bytes.push(i as u8);
                        }
                        _ => {
                            return ExecuteResult::Error(
                                "List must contain only integers for UTF8→".to_string(),
                            );
                        }
                    }
                }

                match String::from_utf8(bytes) {
                    Ok(s) => {
                        if ctx.push(Value::String(s)).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    Err(_) => ExecuteResult::Error("Invalid UTF-8 byte sequence".to_string()),
                }
            }
            Self::CMD_SREV => {
                // SREV: Reverse string characters
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for SREV".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let reversed: String = s.chars().rev().collect();
                if ctx.push(Value::String(reversed)).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_NTOKENS => {
                // NTOKENS: Count whitespace-separated tokens
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected string for NTOKENS".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let count = s.split_whitespace().count();
                if ctx.push_real(count as f64).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_NTHTOKEN => {
                // NTHTOKEN: Extract the nth token (1-based)
                let n = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected string for NTHTOKEN".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if n == 0 {
                    return ExecuteResult::Error("NTHTOKEN index starts at 1".to_string());
                }

                match s.split_whitespace().nth(n - 1) {
                    Some(token) => {
                        if ctx.push(Value::String(token.to_string())).is_err() {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Token {} not found", n)),
                }
            }
            Self::CMD_NTHTOKENPOS => {
                // NTHTOKENPOS: Get start and end positions of nth token (1-based)
                let n = match ctx.pop_real() {
                    Ok(n) => n as usize,
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected string for NTHTOKENPOS".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if n == 0 {
                    return ExecuteResult::Error("NTHTOKENPOS index starts at 1".to_string());
                }

                // Find the nth token and its position
                let mut token_count = 0;
                let mut in_token = false;
                let mut token_start = 0;

                for (i, c) in s.char_indices() {
                    if c.is_whitespace() {
                        if in_token {
                            // End of a token
                            token_count += 1;
                            if token_count == n {
                                // Found the nth token
                                let start_char = s[..token_start].chars().count() + 1;
                                let end_char = s[..i].chars().count();
                                if ctx.push_real(start_char as f64).is_err()
                                    || ctx.push_real(end_char as f64).is_err()
                                {
                                    return ExecuteResult::Error("Stack overflow".to_string());
                                }
                                return ExecuteResult::Ok;
                            }
                            in_token = false;
                        }
                    } else if !in_token {
                        // Start of a token
                        in_token = true;
                        token_start = i;
                    }
                }

                // Check if last token was the one we wanted
                if in_token {
                    token_count += 1;
                    if token_count == n {
                        let start_char = s[..token_start].chars().count() + 1;
                        let end_char = s.chars().count();
                        if ctx.push_real(start_char as f64).is_err()
                            || ctx.push_real(end_char as f64).is_err()
                        {
                            return ExecuteResult::Error("Stack overflow".to_string());
                        }
                        return ExecuteResult::Ok;
                    }
                }

                ExecuteResult::Error(format!("Token {} not found", n))
            }
            Self::CMD_TRIM => {
                // TRIM: Remove leading and trailing whitespace
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for TRIM".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if ctx.push(Value::String(s.trim().to_string())).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_RTRIM => {
                // RTRIM: Remove trailing whitespace only
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for RTRIM".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if ctx.push(Value::String(s.trim_end().to_string())).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_STRLENCP => {
                // STRLENCP: String length in code points (same as character count for us)
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected string for STRLENCP".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                let len = s.chars().count();
                if ctx.push_real(len as f64).is_err() {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            Self::CMD_SREPL => {
                // SREPL: Search and replace all occurrences
                // Stack: string find replace -> result count
                let replace = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error(
                            "Expected replacement string for SREPL".to_string(),
                        );
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let find = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => {
                        return ExecuteResult::Error("Expected search string for SREPL".to_string());
                    }
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };
                let s = match ctx.pop() {
                    Ok(Value::String(s)) => s,
                    Ok(_) => return ExecuteResult::Error("Expected string for SREPL".to_string()),
                    Err(_) => return ExecuteResult::Error("Stack underflow".to_string()),
                };

                if find.is_empty() {
                    // Can't search for empty string
                    if ctx.push(Value::String(s)).is_err() || ctx.push_real(0.0).is_err() {
                        return ExecuteResult::Error("Stack overflow".to_string());
                    }
                    return ExecuteResult::Ok;
                }

                // Count occurrences
                let count = s.matches(&find).count();
                // Perform replacement
                let result = s.replace(&find, &replace);

                if ctx.push(Value::String(result)).is_err()
                    || ctx.push_real(count as f64).is_err()
                {
                    return ExecuteResult::Error("Stack overflow".to_string());
                }
                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown string command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        match ctx.mode() {
            DecompileMode::Prolog => {
                // Check for STRING prolog
                if let Some(word) = ctx.peek()
                    && rpl_core::is_prolog(word)
                    && rpl_core::extract_type(word) == TypeId::STRING.as_u16()
                {
                    let size = rpl_core::extract_size(word) as usize;
                    ctx.read(); // consume prolog

                    // Read string data
                    let mut bytes = Vec::with_capacity(size * 4);
                    for _ in 0..size {
                        if let Some(w) = ctx.read() {
                            bytes.push((w & 0xFF) as u8);
                            bytes.push(((w >> 8) & 0xFF) as u8);
                            bytes.push(((w >> 16) & 0xFF) as u8);
                            bytes.push(((w >> 24) & 0xFF) as u8);
                        }
                    }

                    // Find null terminator or end
                    let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
                    let s = String::from_utf8_lossy(&bytes[..len]);

                    ctx.write(&format!("\"{}\"", escape_string(&s)));
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
        // String literals produce one value
        if Self::is_string_start(token) {
            return StackEffect::Fixed {
                consumes: 0,
                produces: 1,
            };
        }

        let upper = token.to_ascii_uppercase();
        match upper.as_str() {
            "NUM" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "STR" | "→STR" | "->STR" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "CHR" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "ASC" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "SUB" => StackEffect::Fixed {
                consumes: 3,
                produces: 1,
            },
            "POS" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            "→UTF8" | "->UTF8" | "TOUTF8" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "UTF8→" | "UTF8->" | "FROMUTF8" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "SREV" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "NTOKENS" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "NTHTOKEN" => StackEffect::Fixed {
                consumes: 2,
                produces: 1,
            },
            "NTHTOKENPOS" => StackEffect::Fixed {
                consumes: 2,
                produces: 2,
            },
            "TRIM" | "RTRIM" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "STRLENCP" => StackEffect::Fixed {
                consumes: 1,
                produces: 1,
            },
            "SREPL" => StackEffect::Fixed {
                consumes: 3,
                produces: 2,
            },
            _ => StackEffect::Dynamic,
        }
    }
}

/// Process escape sequences in a string.
fn process_escapes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Escape special characters for string output.
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\0' => result.push_str("\\0"),
            _ => result.push(c),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};

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

    #[test]
    fn parse_string_simple() {
        assert_eq!(StringsLib::parse_string("\"hello\""), Some(("hello", true)));
    }

    #[test]
    fn parse_string_empty() {
        assert_eq!(StringsLib::parse_string("\"\""), Some(("", true)));
    }

    #[test]
    fn parse_string_with_escape() {
        assert_eq!(
            StringsLib::parse_string("\"hello\\\"world\""),
            Some(("hello\\\"world", true))
        );
    }

    #[test]
    fn parse_string_incomplete() {
        assert_eq!(StringsLib::parse_string("\"hello"), Some(("hello", false)));
    }

    #[test]
    fn process_escapes_basic() {
        assert_eq!(process_escapes("hello"), "hello");
        assert_eq!(process_escapes("hello\\nworld"), "hello\nworld");
        assert_eq!(process_escapes("tab\\there"), "tab\there");
        assert_eq!(process_escapes("quote\\\"here"), "quote\"here");
    }

    #[test]
    fn escape_string_basic() {
        assert_eq!(escape_string("hello"), "hello");
        assert_eq!(escape_string("hello\nworld"), "hello\\nworld");
        assert_eq!(escape_string("quote\"here"), "quote\\\"here");
    }

    #[test]
    fn stack_effect_string_literal() {
        let lib = StringsLib;
        assert!(matches!(
            lib.stack_effect("\"hello\""),
            StackEffect::Fixed {
                consumes: 0,
                produces: 1
            }
        ));
    }

    #[test]
    fn stack_effect_commands() {
        let lib = StringsLib;
        assert!(matches!(
            lib.stack_effect("NUM"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("STR"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
    }
}
