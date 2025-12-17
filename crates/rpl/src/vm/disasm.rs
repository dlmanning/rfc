//! Bytecode disassembler for debugging.
//!
//! Converts VM bytecode to human-readable instruction format for
//! use in debugger disassembly views.

use super::bytecode::{
    BlockType, CatchKind, Opcode, read_f64, read_leb128_i64, read_leb128_u32, read_u16,
};

/// A disassembled instruction.
#[derive(Clone, Debug)]
pub struct DisassembledInstr {
    /// Bytecode offset (PC).
    pub pc: usize,
    /// Size in bytes.
    pub size: usize,
    /// Human-readable instruction text (e.g., "I64Const 42").
    pub text: String,
    /// Raw bytes as hex string (e.g., "42 2A").
    pub bytes: String,
}

/// Disassemble instructions from bytecode.
///
/// Returns instructions starting at `start_pc`, up to `count` instructions.
/// If `start_pc` is beyond the bytecode, returns an empty vec.
pub fn disassemble(code: &[u8], start_pc: usize, count: usize) -> Vec<DisassembledInstr> {
    let mut result = Vec::with_capacity(count);
    let mut pc = start_pc;

    for _ in 0..count {
        if pc >= code.len() {
            break;
        }
        if let Some((instr, next_pc)) = disassemble_one(code, pc) {
            result.push(instr);
            pc = next_pc;
        } else {
            // Invalid instruction - emit raw byte and continue
            let byte = code[pc];
            result.push(DisassembledInstr {
                pc,
                size: 1,
                text: format!("??? 0x{:02X}", byte),
                bytes: format!("{:02X}", byte),
            });
            pc += 1;
        }
    }

    result
}

/// Disassemble a single instruction at the given PC.
///
/// Returns `(instruction, next_pc)` or None if at end of bytecode.
pub fn disassemble_one(code: &[u8], pc: usize) -> Option<(DisassembledInstr, usize)> {
    if pc >= code.len() {
        return None;
    }

    let start_pc = pc;
    let mut offset = pc;

    let opcode_byte = code[offset];
    offset += 1;

    let opcode = Opcode::from_byte(opcode_byte);
    let (text, next_offset) = match opcode {
        Some(op) => disassemble_opcode(op, code, offset),
        None => (format!("??? 0x{:02X}", opcode_byte), offset),
    };

    let size = next_offset - start_pc;
    let bytes = format_bytes(&code[start_pc..next_offset]);

    Some((
        DisassembledInstr {
            pc: start_pc,
            size,
            text,
            bytes,
        },
        next_offset,
    ))
}

/// Format bytes as hex string.
fn format_bytes(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| format!("{:02X}", b))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Disassemble a known opcode with its operands.
fn disassemble_opcode(op: Opcode, code: &[u8], mut offset: usize) -> (String, usize) {
    match op {
        // No operands
        Opcode::Unreachable => ("Unreachable".into(), offset),
        Opcode::Nop => ("Nop".into(), offset),
        Opcode::Else => ("Else".into(), offset),
        Opcode::End => ("End".into(), offset),
        Opcode::Return => ("Return".into(), offset),
        Opcode::Drop => ("Drop".into(), offset),
        Opcode::Select => ("Select".into(), offset),
        Opcode::ThrowRef => ("ThrowRef".into(), offset),
        Opcode::PopLocalScope => ("PopLocalScope".into(), offset),

        // i64 comparison (no operands)
        Opcode::I64Eqz => ("I64Eqz".into(), offset),
        Opcode::I64Eq => ("I64Eq".into(), offset),
        Opcode::I64Ne => ("I64Ne".into(), offset),
        Opcode::I64LtS => ("I64LtS".into(), offset),
        Opcode::I64LtU => ("I64LtU".into(), offset),
        Opcode::I64GtS => ("I64GtS".into(), offset),
        Opcode::I64GtU => ("I64GtU".into(), offset),
        Opcode::I64LeS => ("I64LeS".into(), offset),
        Opcode::I64LeU => ("I64LeU".into(), offset),
        Opcode::I64GeS => ("I64GeS".into(), offset),
        Opcode::I64GeU => ("I64GeU".into(), offset),

        // f64 comparison (no operands)
        Opcode::F64Eq => ("F64Eq".into(), offset),
        Opcode::F64Ne => ("F64Ne".into(), offset),
        Opcode::F64Lt => ("F64Lt".into(), offset),
        Opcode::F64Gt => ("F64Gt".into(), offset),
        Opcode::F64Le => ("F64Le".into(), offset),
        Opcode::F64Ge => ("F64Ge".into(), offset),

        // i64 arithmetic (no operands)
        Opcode::I64Add => ("I64Add".into(), offset),
        Opcode::I64Sub => ("I64Sub".into(), offset),
        Opcode::I64Mul => ("I64Mul".into(), offset),
        Opcode::I64DivS => ("I64DivS".into(), offset),
        Opcode::I64DivU => ("I64DivU".into(), offset),
        Opcode::I64RemS => ("I64RemS".into(), offset),
        Opcode::I64RemU => ("I64RemU".into(), offset),
        Opcode::I64And => ("I64And".into(), offset),
        Opcode::I64Or => ("I64Or".into(), offset),
        Opcode::I64Xor => ("I64Xor".into(), offset),
        Opcode::I64Shl => ("I64Shl".into(), offset),
        Opcode::I64ShrS => ("I64ShrS".into(), offset),
        Opcode::I64ShrU => ("I64ShrU".into(), offset),

        // f64 arithmetic (no operands)
        Opcode::F64Abs => ("F64Abs".into(), offset),
        Opcode::F64Neg => ("F64Neg".into(), offset),
        Opcode::F64Ceil => ("F64Ceil".into(), offset),
        Opcode::F64Floor => ("F64Floor".into(), offset),
        Opcode::F64Trunc => ("F64Trunc".into(), offset),
        Opcode::F64Sqrt => ("F64Sqrt".into(), offset),
        Opcode::F64Add => ("F64Add".into(), offset),
        Opcode::F64Sub => ("F64Sub".into(), offset),
        Opcode::F64Mul => ("F64Mul".into(), offset),
        Opcode::F64Div => ("F64Div".into(), offset),

        // Conversions (no operands)
        Opcode::F64ConvertI64S => ("F64ConvertI64S".into(), offset),
        Opcode::I64TruncF64S => ("I64TruncF64S".into(), offset),

        // Block type operands
        Opcode::Block => {
            let (block_type, new_offset) = read_block_type(code, offset);
            (format!("Block {}", block_type), new_offset)
        }
        Opcode::Loop => {
            let (block_type, new_offset) = read_block_type(code, offset);
            (format!("Loop {}", block_type), new_offset)
        }
        Opcode::If => {
            let (block_type, new_offset) = read_block_type(code, offset);
            (format!("If {}", block_type), new_offset)
        }

        // LEB128 u32 operands
        Opcode::Br => {
            if let Some(label) = read_leb128_u32(code, &mut offset) {
                (format!("Br {}", label), offset)
            } else {
                ("Br ???".into(), code.len())
            }
        }
        Opcode::BrIf => {
            if let Some(label) = read_leb128_u32(code, &mut offset) {
                (format!("BrIf {}", label), offset)
            } else {
                ("BrIf ???".into(), code.len())
            }
        }
        Opcode::LocalGet => {
            if let Some(idx) = read_leb128_u32(code, &mut offset) {
                (format!("LocalGet {}", idx), offset)
            } else {
                ("LocalGet ???".into(), code.len())
            }
        }
        Opcode::LocalSet => {
            if let Some(idx) = read_leb128_u32(code, &mut offset) {
                (format!("LocalSet {}", idx), offset)
            } else {
                ("LocalSet ???".into(), code.len())
            }
        }
        Opcode::LocalTee => {
            if let Some(idx) = read_leb128_u32(code, &mut offset) {
                (format!("LocalTee {}", idx), offset)
            } else {
                ("LocalTee ???".into(), code.len())
            }
        }
        Opcode::Throw => {
            if let Some(tag) = read_leb128_u32(code, &mut offset) {
                (format!("Throw {}", tag), offset)
            } else {
                ("Throw ???".into(), code.len())
            }
        }
        Opcode::MakeList => {
            if let Some(count) = read_leb128_u32(code, &mut offset) {
                (format!("MakeList {}", count), offset)
            } else {
                ("MakeList ???".into(), code.len())
            }
        }

        // I64Const - LEB128 signed
        Opcode::I64Const => {
            if let Some(val) = read_leb128_i64(code, &mut offset) {
                (format!("I64Const {}", val), offset)
            } else {
                ("I64Const ???".into(), code.len())
            }
        }

        // F64Const - 8 bytes IEEE 754
        Opcode::F64Const => {
            if let Some(val) = read_f64(code, &mut offset) {
                (format!("F64Const {}", val), offset)
            } else {
                ("F64Const ???".into(), code.len())
            }
        }

        // CallLib - lib_id (u16) + cmd_id (u16)
        Opcode::CallLib => {
            if let (Some(lib_id), Some(cmd_id)) =
                (read_u16(code, &mut offset), read_u16(code, &mut offset))
            {
                let lib_name = lib_name(lib_id);
                (format!("CallLib {}:{}", lib_name, cmd_id), offset)
            } else {
                ("CallLib ???".into(), code.len())
            }
        }

        // String operands - length (LEB128) + UTF-8 bytes
        Opcode::StringConst => {
            if let Some(len) = read_leb128_u32(code, &mut offset) {
                let len = len as usize;
                if offset + len <= code.len() {
                    let s = String::from_utf8_lossy(&code[offset..offset + len]);
                    offset += len;
                    (format!("StringConst {:?}", s), offset)
                } else {
                    ("StringConst ???".into(), code.len())
                }
            } else {
                ("StringConst ???".into(), code.len())
            }
        }
        Opcode::SymbolicConst => {
            if let Some(len) = read_leb128_u32(code, &mut offset) {
                let len = len as usize;
                if offset + len <= code.len() {
                    let s = String::from_utf8_lossy(&code[offset..offset + len]);
                    offset += len;
                    (format!("SymbolicConst '{}'", s), offset)
                } else {
                    ("SymbolicConst ???".into(), code.len())
                }
            } else {
                ("SymbolicConst ???".into(), code.len())
            }
        }

        // EvalName - length (LEB128) + name bytes
        Opcode::EvalName => {
            if let Some(len) = read_leb128_u32(code, &mut offset) {
                let len = len as usize;
                if offset + len <= code.len() {
                    let name = String::from_utf8_lossy(&code[offset..offset + len]);
                    offset += len;
                    (format!("EvalName '{}'", name), offset)
                } else {
                    ("EvalName ???".into(), code.len())
                }
            } else {
                ("EvalName ???".into(), code.len())
            }
        }

        // MakeProgram - format: <param_count> <string_count> [<str_len> <str_bytes>]* <code_len> <code_bytes>
        //                       <span_count:u16> [<offset:u16> <start:LEB128> <end:LEB128>]*
        Opcode::MakeProgram => {
            // Read param count
            if let Some(param_count) = read_leb128_u32(code, &mut offset) {
                // Read string table
                if let Some(str_count) = read_leb128_u32(code, &mut offset) {
                    let mut valid = true;
                    for _ in 0..str_count {
                        if let Some(slen) = read_leb128_u32(code, &mut offset) {
                            if offset + slen as usize <= code.len() {
                                offset += slen as usize;
                            } else {
                                valid = false;
                                break;
                            }
                        } else {
                            valid = false;
                            break;
                        }
                    }
                    if !valid {
                        return ("MakeProgram ???".into(), code.len().min(offset));
                    }
                    // Read code
                    if let Some(code_len) = read_leb128_u32(code, &mut offset) {
                        let code_len = code_len as usize;
                        if offset + code_len <= code.len() {
                            offset += code_len;
                            // Read span count (u16)
                            if let Some(span_count) = read_u16(code, &mut offset) {
                                // Each span is: u16 (offset) + LEB128 (start) + LEB128 (end)
                                for _ in 0..span_count {
                                    // bytecode offset (u16)
                                    let _ = read_u16(code, &mut offset);
                                    // source start (LEB128)
                                    let _ = read_leb128_u32(code, &mut offset);
                                    // source end (LEB128)
                                    let _ = read_leb128_u32(code, &mut offset);
                                }
                                let kind = if param_count > 0 { "Function" } else { "Program" };
                                (format!("Make{} <{} bytes, {} strings, {} params>", kind, code_len, str_count, param_count), offset.min(code.len()))
                            } else {
                                (format!("MakeProgram <{} bytes>", code_len), offset.min(code.len()))
                            }
                        } else {
                            ("MakeProgram ???".into(), code.len())
                        }
                    } else {
                        ("MakeProgram ???".into(), code.len())
                    }
                } else {
                    ("MakeProgram ???".into(), code.len())
                }
            } else {
                ("MakeProgram ???".into(), code.len())
            }
        }

        // TryTable - complex
        Opcode::TryTable => {
            // Block type
            let (block_type, new_offset) = read_block_type(code, offset);
            offset = new_offset;

            // Catch clause count
            if let Some(catch_count) = read_leb128_u32(code, &mut offset) {
                // Skip catch clauses
                for _ in 0..catch_count {
                    if offset >= code.len() {
                        break;
                    }
                    let kind = CatchKind::from_byte(code[offset]);
                    offset += 1;
                    if let Some(k) = kind && k.has_tag() {
                        // Skip tag index
                        let _ = read_leb128_u32(code, &mut offset);
                    }
                    // Skip label index
                    let _ = read_leb128_u32(code, &mut offset);
                }
                (
                    format!("TryTable {} catches={}", block_type, catch_count),
                    offset,
                )
            } else {
                ("TryTable ???".into(), code.len())
            }
        }

        // PushLocalScope - count (LEB128), then for each: string_idx (LEB128), local_idx (LEB128)
        Opcode::PushLocalScope => {
            if let Some(count) = read_leb128_u32(code, &mut offset) {
                for _ in 0..count {
                    // string_idx (index into string table)
                    let _ = read_leb128_u32(code, &mut offset);
                    // local_idx
                    let _ = read_leb128_u32(code, &mut offset);
                }
                (format!("PushLocalScope {}", count), offset)
            } else {
                ("PushLocalScope ???".into(), code.len())
            }
        }
    }
}

/// Read a block type byte.
fn read_block_type(code: &[u8], offset: usize) -> (String, usize) {
    if offset >= code.len() {
        return ("???".into(), offset);
    }
    let byte = code[offset];
    let type_str = match BlockType::from_byte(byte) {
        Some(BlockType::Empty) => "[]",
        Some(BlockType::Value) => "[v]",
        None => "???",
    };
    (type_str.into(), offset + 1)
}

/// Get library name from ID.
fn lib_name(lib_id: u16) -> &'static str {
    match lib_id {
        8 => "PROG",
        9 => "FLOW",
        20 => "COMMENTS",
        24 => "STRINGS",
        28 => "DIRECTORY",
        32 => "LOCALS",
        64 => "ARITH",
        66 => "TRANS",
        72 => "STACK",
        80 => "SYMBOLIC",
        88 => "LIST",
        96 => "BINARY",
        102 => "USERLIB",
        _ => "???",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disassemble_i64_const() {
        // I64Const 42 encoded as: 0x42, 0x2A (LEB128 for 42)
        let code = vec![0x42, 0x2A];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].pc, 0);
        assert_eq!(result[0].size, 2);
        assert_eq!(result[0].text, "I64Const 42");
        assert_eq!(result[0].bytes, "42 2A");
    }

    #[test]
    fn disassemble_no_operand() {
        // I64Add
        let code = vec![0x7C];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "I64Add");
        assert_eq!(result[0].size, 1);
    }

    #[test]
    fn disassemble_f64_const() {
        // F64Const 3.14159...
        let mut code = vec![0x44];
        code.extend_from_slice(&3.14159f64.to_le_bytes());
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert!(result[0].text.starts_with("F64Const 3.14159"));
        assert_eq!(result[0].size, 9);
    }

    #[test]
    fn disassemble_call_lib() {
        // CallLib 8:1 (PROG lib, command 1)
        let code = vec![0xC0, 0x08, 0x00, 0x01, 0x00];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "CallLib PROG:1");
        assert_eq!(result[0].size, 5);
    }

    #[test]
    fn disassemble_string_const() {
        // StringConst "hi"
        let code = vec![0xC1, 0x02, b'h', b'i'];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "StringConst \"hi\"");
        assert_eq!(result[0].size, 4);
    }

    #[test]
    fn disassemble_multiple() {
        // I64Const 1, I64Const 2, I64Add
        let code = vec![0x42, 0x01, 0x42, 0x02, 0x7C];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 3);
        assert_eq!(result[0].text, "I64Const 1");
        assert_eq!(result[1].text, "I64Const 2");
        assert_eq!(result[2].text, "I64Add");
    }

    #[test]
    fn disassemble_with_offset() {
        // I64Const 1, I64Const 2, I64Add
        let code = vec![0x42, 0x01, 0x42, 0x02, 0x7C];
        // Start at PC 2 (I64Const 2)
        let result = disassemble(&code, 2, 10);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].pc, 2);
        assert_eq!(result[0].text, "I64Const 2");
        assert_eq!(result[1].pc, 4);
        assert_eq!(result[1].text, "I64Add");
    }

    #[test]
    fn disassemble_block() {
        // Block [] (empty block type)
        let code = vec![0x02, 0x40];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "Block []");
        assert_eq!(result[0].size, 2);
    }

    #[test]
    fn disassemble_invalid_opcode() {
        // Invalid opcode 0xFF
        let code = vec![0xFF, 0x7C];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 2);
        assert!(result[0].text.contains("0xFF"));
        assert_eq!(result[1].text, "I64Add");
    }

    #[test]
    fn disassemble_eval_name() {
        // EvalName 'is_prime' = C2 08 69 73 5F 70 72 69 6D 65
        let code = vec![0xC2, 0x08, b'i', b's', b'_', b'p', b'r', b'i', b'm', b'e'];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1, "Should be exactly one instruction");
        assert_eq!(result[0].pc, 0);
        assert_eq!(result[0].size, 10, "EvalName 'is_prime' should be 10 bytes");
        assert_eq!(result[0].text, "EvalName 'is_prime'");
    }

    #[test]
    fn disassemble_eval_name_followed_by_other() {
        // EvalName 'test' followed by I64Add
        // C2 04 74 65 73 74 7C
        let code = vec![0xC2, 0x04, b't', b'e', b's', b't', 0x7C];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 2, "Should be two instructions");
        assert_eq!(result[0].text, "EvalName 'test'");
        assert_eq!(result[0].size, 6);
        assert_eq!(result[0].pc, 0);
        assert_eq!(result[1].text, "I64Add");
        assert_eq!(result[1].pc, 6, "I64Add should be at PC 6");
    }

    // === Integration tests with compiled programs ===

    use crate::{parse::parse, lower::lower, registry::Registry};
    use crate::core::Interner;

    /// Compile source code and return the bytecode.
    fn compile(source: &str) -> Vec<u8> {
        let registry = Registry::with_core();
        let mut interner = Interner::new();
        let nodes = parse(source, &registry, &mut interner).expect("parse failed");
        let program = lower(&nodes, &registry, &interner).expect("lower failed");
        program.code
    }

    /// Verify disassembly covers all bytes without gaps or unknown opcodes.
    fn verify_disassembly(code: &[u8]) -> Result<Vec<DisassembledInstr>, String> {
        let instructions = disassemble(code, 0, 10000);

        if instructions.is_empty() && !code.is_empty() {
            return Err("No instructions disassembled from non-empty code".into());
        }

        // Check for contiguous coverage
        let mut expected_pc = 0;
        for instr in &instructions {
            if instr.pc != expected_pc {
                return Err(format!(
                    "Gap in disassembly: expected PC {}, got {}. Previous instruction may have wrong size.",
                    expected_pc, instr.pc
                ));
            }
            expected_pc = instr.pc + instr.size;

            // Check for unknown opcodes (but allow unknown library IDs)
            if instr.text.starts_with("???") {
                return Err(format!(
                    "Unknown opcode at PC {}: {}",
                    instr.pc, instr.text
                ));
            }
        }

        // Verify we covered all bytes
        if expected_pc != code.len() {
            return Err(format!(
                "Disassembly ended at PC {} but code is {} bytes",
                expected_pc, code.len()
            ));
        }

        Ok(instructions)
    }

    #[test]
    fn disassemble_simple_arithmetic() {
        let code = compile("1 2 +");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have at least: I64Const 1, I64Const 2, CallLib (for +)
        assert!(instructions.len() >= 3, "Expected at least 3 instructions, got {}", instructions.len());
        assert!(instructions[0].text.contains("I64Const"));
        assert!(instructions[1].text.contains("I64Const"));
    }

    #[test]
    fn disassemble_with_string() {
        let code = compile("\"hello\" \"world\" +");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have string constants
        let has_string = instructions.iter().any(|i| i.text.contains("StringConst"));
        assert!(has_string, "Expected StringConst instruction");
    }

    #[test]
    fn disassemble_conditional() {
        let code = compile("1 IF 2 THEN 3 ELSE 4 END");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have If, Else, End
        let has_if = instructions.iter().any(|i| i.text.starts_with("If"));
        let has_else = instructions.iter().any(|i| i.text == "Else");
        let has_end = instructions.iter().any(|i| i.text == "End");
        assert!(has_if, "Expected If instruction");
        assert!(has_else, "Expected Else instruction");
        assert!(has_end, "Expected End instruction");
    }

    #[test]
    fn disassemble_loop() {
        let code = compile("1 10 FOR i i NEXT");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Just verify it parses without errors
        assert!(!instructions.is_empty());
    }

    #[test]
    fn disassemble_program_literal() {
        let code = compile("<< 1 2 + >>");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have MakeProgram
        let has_make_program = instructions.iter().any(|i| i.text.contains("MakeProgram"));
        assert!(has_make_program, "Expected MakeProgram instruction");
    }

    #[test]
    fn disassemble_local_variables() {
        let code = compile("<< -> a b << a b + >> >>");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // The outer program should have MakeProgram
        let has_make_program = instructions.iter().any(|i| i.text.contains("MakeProgram"));
        assert!(has_make_program, "Expected MakeProgram instruction");
    }

    #[test]
    fn disassemble_factorial() {
        let source = r#"
            << -> n <<
                IF n 0 ==
                THEN
                    1
                ELSE
                    n 1 - fact n *
                END
            >> >>
            "fact" STO
            5 fact
        "#;
        let code = compile(source);
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Main program: MakeProgram, StringConst, CallLib STO, I64Const, EvalName
        assert!(instructions.len() >= 5, "Expected at least 5 instructions, got {}", instructions.len());
        // Should have MakeProgram for the function
        assert!(instructions.iter().any(|i| i.text.contains("MakeProgram")));
        // Should have the function name
        assert!(instructions.iter().any(|i| i.text.contains("fact")));
    }

    #[test]
    fn disassemble_gcd() {
        let source = r#"
            << -> a b <<
                IF b 0 ==
                THEN
                    a
                ELSE
                    b
                    a b MOD
                    gcd
                END
            >> >>
            "gcd" STO
            48 18 gcd
        "#;
        let code = compile(source);
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Main program: MakeProgram, StringConst, CallLib STO, I64Const, I64Const, EvalName
        assert!(instructions.len() >= 6, "Expected at least 6 instructions, got {}", instructions.len());
        // Should have MakeProgram for the function
        assert!(instructions.iter().any(|i| i.text.contains("MakeProgram")));
        // Should have the function name
        assert!(instructions.iter().any(|i| i.text.contains("gcd")));
    }

    #[test]
    fn disassemble_fibonacci() {
        let source = r#"
            << -> n <<
                n 1 <=
                IF THEN
                    n
                ELSE
                    n 1 - fib
                    n 2 - fib
                    +
                END
            >> >>
            "fib" STO
            10 fib
        "#;
        let code = compile(source);
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Main program: MakeProgram, StringConst, CallLib STO, I64Const, EvalName
        assert!(instructions.len() >= 5, "Expected at least 5 instructions, got {}", instructions.len());
        // Should have MakeProgram for the function
        assert!(instructions.iter().any(|i| i.text.contains("MakeProgram")));
        // Should have the function name
        assert!(instructions.iter().any(|i| i.text.contains("fib")));
    }

    #[test]
    fn disassemble_list_operations() {
        let code = compile("{ 1 2 3 } DUP SIZE");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have MakeList
        let has_make_list = instructions.iter().any(|i| i.text.contains("MakeList"));
        assert!(has_make_list, "Expected MakeList instruction");
    }

    #[test]
    fn disassemble_symbolic() {
        let code = compile("'X' 'Y' +");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have SymbolicConst
        let has_symbolic = instructions.iter().any(|i| i.text.contains("SymbolicConst"));
        assert!(has_symbolic, "Expected SymbolicConst instruction");
    }
}
