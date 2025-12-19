//! Bytecode disassembler for debugging.
//!
//! Converts VM bytecode to human-readable instruction format for
//! use in debugger disassembly views.

use super::bytecode::{
    BlockType, CatchKind, Opcode, read_f64, read_leb128_i64, read_leb128_u32, read_u16, read_u32,
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

        // Block type operands with u32 offsets
        Opcode::Block => {
            let (block_type, new_offset) = read_block_type(code, offset);
            offset = new_offset;
            if let Some(end_offset) = read_u32(code, &mut offset) {
                (format!("Block {} end=@{}", block_type, end_offset), offset)
            } else {
                ("Block ???".into(), code.len())
            }
        }
        Opcode::Loop => {
            let (block_type, new_offset) = read_block_type(code, offset);
            offset = new_offset;
            if let Some(end_offset) = read_u32(code, &mut offset) {
                (format!("Loop {} end=@{}", block_type, end_offset), offset)
            } else {
                ("Loop ???".into(), code.len())
            }
        }
        Opcode::If => {
            let (block_type, new_offset) = read_block_type(code, offset);
            offset = new_offset;
            if let (Some(else_offset), Some(end_offset)) =
                (read_u32(code, &mut offset), read_u32(code, &mut offset))
            {
                if else_offset == end_offset {
                    (format!("If {} end=@{}", block_type, end_offset), offset)
                } else {
                    (format!("If {} else=@{} end=@{}", block_type, else_offset, end_offset), offset)
                }
            } else {
                ("If ???".into(), code.len())
            }
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

        // Rodata references - offset (LEB128) + length (LEB128)
        // The actual data is in the rodata section, not inline in bytecode
        Opcode::StringConst => {
            if let (Some(str_offset), Some(len)) = (
                read_leb128_u32(code, &mut offset),
                read_leb128_u32(code, &mut offset),
            ) {
                (format!("StringConst @{}:{}", str_offset, len), offset)
            } else {
                ("StringConst ???".into(), code.len())
            }
        }
        Opcode::SymbolicConst => {
            if let (Some(sym_offset), Some(len)) = (
                read_leb128_u32(code, &mut offset),
                read_leb128_u32(code, &mut offset),
            ) {
                (format!("SymbolicConst @{}:{}", sym_offset, len), offset)
            } else {
                ("SymbolicConst ???".into(), code.len())
            }
        }
        Opcode::BlobConst => {
            if let (Some(blob_offset), Some(len)) = (
                read_leb128_u32(code, &mut offset),
                read_leb128_u32(code, &mut offset),
            ) {
                (format!("BlobConst @{}:{}", blob_offset, len), offset)
            } else {
                ("BlobConst ???".into(), code.len())
            }
        }

        // EvalName - offset (LEB128) + length (LEB128) into rodata
        Opcode::EvalName => {
            if let (Some(name_offset), Some(len)) = (
                read_leb128_u32(code, &mut offset),
                read_leb128_u32(code, &mut offset),
            ) {
                (format!("EvalName @{}:{}", name_offset, len), offset)
            } else {
                ("EvalName ???".into(), code.len())
            }
        }

        // MakeProgram - format: <param_count> <rodata_len> <rodata_bytes> <code_len> <code_bytes>
        //                       <span_count:u16> [<offset:u16> <start:LEB128> <end:LEB128>]*
        Opcode::MakeProgram => {
            // Read param count
            if let Some(param_count) = read_leb128_u32(code, &mut offset) {
                // Read rodata section
                if let Some(rodata_len) = read_leb128_u32(code, &mut offset) {
                    let rodata_len = rodata_len as usize;
                    if offset + rodata_len > code.len() {
                        return ("MakeProgram ???".into(), code.len());
                    }
                    offset += rodata_len;
                    // Read code section
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
                                (format!("Make{} <{} bytes, {} rodata, {} params>", kind, code_len, rodata_len, param_count), offset.min(code.len()))
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

        // TryTable - complex with u32 end offset
        Opcode::TryTable => {
            // Block type
            let (block_type, new_offset) = read_block_type(code, offset);
            offset = new_offset;

            // End offset (u32)
            if let Some(end_offset) = read_u32(code, &mut offset) {
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
                        format!("TryTable {} end=@{} catches={}", block_type, end_offset, catch_count),
                        offset,
                    )
                } else {
                    ("TryTable ???".into(), code.len())
                }
            } else {
                ("TryTable ???".into(), code.len())
            }
        }

        // PushLocalScope - count (LEB128), then for each: offset (LEB128), len (LEB128), local_idx (LEB128)
        Opcode::PushLocalScope => {
            if let Some(count) = read_leb128_u32(code, &mut offset) {
                for _ in 0..count {
                    // offset (into rodata)
                    let _ = read_leb128_u32(code, &mut offset);
                    // len
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
        // StringConst with offset=0, len=2 (rodata format)
        // 0xC1, offset (LEB128 0), len (LEB128 2)
        let code = vec![0xC1, 0x00, 0x02];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "StringConst @0:2");
        assert_eq!(result[0].size, 3);
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
        // Block [] with end offset (new format: opcode, type, end_offset:u32)
        // end_offset = 10 (0x0A000000 in little-endian)
        let code = vec![0x02, 0x40, 0x0A, 0x00, 0x00, 0x00];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].text, "Block [] end=@10");
        assert_eq!(result[0].size, 6);
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
        // EvalName with offset=5, len=8 (rodata format)
        // 0xC2, offset (LEB128 5), len (LEB128 8)
        let code = vec![0xC2, 0x05, 0x08];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 1, "Should be exactly one instruction");
        assert_eq!(result[0].pc, 0);
        assert_eq!(result[0].size, 3, "EvalName should be 3 bytes");
        assert_eq!(result[0].text, "EvalName @5:8");
    }

    #[test]
    fn disassemble_eval_name_followed_by_other() {
        // EvalName with offset=0, len=4 followed by I64Add
        // 0xC2, offset (LEB128 0), len (LEB128 4), 0x7C (I64Add)
        let code = vec![0xC2, 0x00, 0x04, 0x7C];
        let result = disassemble(&code, 0, 10);
        assert_eq!(result.len(), 2, "Should be two instructions");
        assert_eq!(result[0].text, "EvalName @0:4");
        assert_eq!(result[0].size, 3);
        assert_eq!(result[0].pc, 0);
        assert_eq!(result[1].text, "I64Add");
        assert_eq!(result[1].pc, 3, "I64Add should be at PC 3");
    }

    // === Integration tests with compiled programs ===

    use crate::{parse::parse, lower::lower, registry::{InterfaceRegistry, LowererRegistry}};
    use crate::core::Interner;

    /// Compile source code and return the bytecode.
    fn compile(source: &str) -> Vec<u8> {
        let interfaces = InterfaceRegistry::new();
        let lowerers = LowererRegistry::new();
        let mut interner = Interner::new();
        let nodes = parse(source, &interfaces, &mut interner).expect("parse failed");
        let program = lower(&nodes, &interfaces, &lowerers, &interner).expect("lower failed");
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
        // Compile literals only: "1 2 3"
        let code = compile("1 2 3");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have: I64Const 1, I64Const 2, I64Const 3
        assert_eq!(instructions.len(), 3, "Expected 3 instructions, got {}", instructions.len());
        assert!(instructions[0].text.contains("I64Const"));
        assert!(instructions[1].text.contains("I64Const"));
        assert!(instructions[2].text.contains("I64Const"));
    }

    #[test]
    fn disassemble_with_string() {
        // Compile string literals only
        let code = compile("\"hello\" \"world\"");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have string constants
        let has_string = instructions.iter().any(|i| i.text.contains("StringConst"));
        assert!(has_string, "Expected StringConst instruction");
    }

    #[test]
    fn disassemble_conditional() {
        // Use raw bytecode for If/Else/End structure (new format):
        // I64Const 1, If (type, else_offset:u32, end_offset:u32), I64Const 2, Else, I64Const 3, End
        // Layout: I64Const 1 at 0-1, If at 2-11, I64Const 2 at 12-13, Else at 14, I64Const 3 at 15-16, End at 17
        // else_offset points AFTER Else (15), end_offset points AFTER End (18)
        let code = vec![
            0x42, 0x01,                         // I64Const 1 (PC 0-1)
            0x04, 0x40,                         // If [] (PC 2-3)
            0x0F, 0x00, 0x00, 0x00,             // else_offset = 15 (PC 4-7)
            0x12, 0x00, 0x00, 0x00,             // end_offset = 18 (PC 8-11)
            0x42, 0x02,                         // I64Const 2 (PC 12-13)
            0x05,                               // Else (PC 14)
            0x42, 0x03,                         // I64Const 3 (PC 15-16)
            0x0B,                               // End (PC 17)
        ];
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
    fn disassemble_loop_bytecode() {
        // Use raw bytecode for Loop/End structure (new format):
        // Loop (type, end_offset:u32), I64Const 1, End
        // Layout: Loop at 0-5, I64Const at 6-7, End at 8
        // end_offset points AFTER End (9)
        let code = vec![
            0x03, 0x40,                 // Loop [] (PC 0-1)
            0x09, 0x00, 0x00, 0x00,     // end_offset = 9 (PC 2-5)
            0x42, 0x01,                 // I64Const 1 (PC 6-7)
            0x0B,                       // End (PC 8)
        ];
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Just verify it parses without errors
        assert!(!instructions.is_empty());
        let has_loop = instructions.iter().any(|i| i.text.starts_with("Loop"));
        assert!(has_loop, "Expected Loop instruction");
    }

    #[test]
    fn disassemble_program_literal() {
        // Compile empty program: "<< >>"
        let code = compile("<< >>");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have MakeProgram
        let has_make_program = instructions.iter().any(|i| i.text.contains("MakeProgram"));
        assert!(has_make_program, "Expected MakeProgram instruction");
    }

    #[test]
    fn disassemble_nested_program() {
        // Compile nested programs: "<< << 1 2 >> >>"
        let code = compile("<< << 1 2 >> >>");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have MakeProgram for both programs
        let make_program_count = instructions.iter()
            .filter(|i| i.text.contains("MakeProgram"))
            .count();
        assert!(make_program_count >= 1, "Expected at least 1 MakeProgram instruction");
    }

    #[test]
    fn disassemble_list() {
        // Compile list literal: "{ 1 2 3 }"
        let code = compile("{ 1 2 3 }");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have MakeList
        let has_make_list = instructions.iter().any(|i| i.text.contains("MakeList"));
        assert!(has_make_list, "Expected MakeList instruction");
    }

    #[test]
    fn disassemble_symbolic() {
        // Compile symbolic literals: "'X' 'Y'"
        let code = compile("'X' 'Y'");
        let instructions = verify_disassembly(&code).expect("disassembly failed");

        // Should have SymbolicConst
        let has_symbolic = instructions.iter().any(|i| i.text.contains("SymbolicConst"));
        assert!(has_symbolic, "Expected SymbolicConst instruction");
    }

    // Note: Complex integration tests (factorial, fibonacci, gcd) that use
    // stdlib commands (IF/THEN/ELSE, STO, arithmetic operators) are in the
    // integration test suite (tests/pipeline/) which uses rpl-stdlib.
}
