//! WASM-aligned bytecode opcodes.
//!
//! Opcodes are designed to be close to WASM for future portability,
//! but with extensions for calculator-specific operations.
//!
//! # Encoding
//!
//! - Single-byte opcodes for common operations
//! - LEB128 encoding for immediate values where possible
//! - Fixed-size encoding for library calls (lib_id: u16, cmd_id: u16)

/// Bytecode opcodes.
///
/// Opcodes use WASM byte values where applicable.
/// See: https://webassembly.github.io/spec/core/binary/instructions.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    // === Control flow (WASM 0x00-0x11) ===
    /// Unreachable instruction (trap).
    Unreachable = 0x00,
    /// No operation.
    Nop = 0x01,
    /// Begin a block. Followed by block type.
    Block = 0x02,
    /// Begin a loop. Followed by block type.
    Loop = 0x03,
    /// Begin conditional. Followed by block type.
    If = 0x04,
    /// Else branch of conditional.
    Else = 0x05,
    /// Throw exception. Followed by tag index (LEB128).
    Throw = 0x08,
    /// Throw exception reference (rethrow). Consumes exnref from stack.
    ThrowRef = 0x0A,
    /// End of block/loop/if/try_table.
    End = 0x0B,
    /// Branch to label. Followed by label index (LEB128).
    Br = 0x0C,
    /// Conditional branch. Followed by label index (LEB128).
    BrIf = 0x0D,
    /// Return from function.
    Return = 0x0F,
    /// Try table with catch clauses. Followed by block type, catch clause count (LEB128),
    /// catch clauses, then body instructions ending with End.
    /// Catch clause: kind (u8), [tag_idx (LEB128)], label_idx (LEB128)
    /// Kinds: 0x00=catch, 0x01=catch_ref, 0x05=catch_all, 0x06=catch_all_ref
    TryTable = 0x1F,

    // === Parametric (WASM 0x1A-0x1B) ===
    /// Drop the top value from the stack.
    Drop = 0x1A,
    /// Select between two values based on condition: [t t i32] -> [t]
    /// If condition is non-zero, keeps first value; otherwise keeps second.
    Select = 0x1B,

    // === Variable access (WASM 0x20-0x24) ===
    /// Get local variable. Followed by index (LEB128).
    LocalGet = 0x20,
    /// Set local variable. Followed by index (LEB128).
    LocalSet = 0x21,
    /// Tee local (set and keep on stack). Followed by index (LEB128).
    LocalTee = 0x22,

    // === Constants (WASM 0x41-0x44) ===
    /// Push i64 constant. Followed by LEB128 signed value.
    I64Const = 0x42,
    /// Push f64 constant. Followed by 8 bytes (IEEE 754).
    F64Const = 0x44,

    // === i64 comparison (WASM 0x50-0x5A) ===
    /// i64.eqz - test if zero
    I64Eqz = 0x50,
    /// i64.eq - equal
    I64Eq = 0x51,
    /// i64.ne - not equal
    I64Ne = 0x52,
    /// i64.lt_s - less than (signed)
    I64LtS = 0x53,
    /// i64.lt_u - less than (unsigned)
    I64LtU = 0x54,
    /// i64.gt_s - greater than (signed)
    I64GtS = 0x55,
    /// i64.gt_u - greater than (unsigned)
    I64GtU = 0x56,
    /// i64.le_s - less than or equal (signed)
    I64LeS = 0x57,
    /// i64.le_u - less than or equal (unsigned)
    I64LeU = 0x58,
    /// i64.ge_s - greater than or equal (signed)
    I64GeS = 0x59,
    /// i64.ge_u - greater than or equal (unsigned)
    I64GeU = 0x5A,

    // === f64 comparison (WASM 0x61-0x66) ===
    /// f64.eq - equal
    F64Eq = 0x61,
    /// f64.ne - not equal
    F64Ne = 0x62,
    /// f64.lt - less than
    F64Lt = 0x63,
    /// f64.gt - greater than
    F64Gt = 0x64,
    /// f64.le - less than or equal
    F64Le = 0x65,
    /// f64.ge - greater than or equal
    F64Ge = 0x66,

    // === i64 arithmetic (WASM 0x7C-0x8A) ===
    /// i64.add
    I64Add = 0x7C,
    /// i64.sub
    I64Sub = 0x7D,
    /// i64.mul
    I64Mul = 0x7E,
    /// i64.div_s - divide (signed)
    I64DivS = 0x7F,
    /// i64.div_u - divide (unsigned)
    I64DivU = 0x80,
    /// i64.rem_s - remainder (signed)
    I64RemS = 0x81,
    /// i64.rem_u - remainder (unsigned)
    I64RemU = 0x82,
    /// i64.and - bitwise and
    I64And = 0x83,
    /// i64.or - bitwise or
    I64Or = 0x84,
    /// i64.xor - bitwise xor
    I64Xor = 0x85,
    /// i64.shl - shift left
    I64Shl = 0x86,
    /// i64.shr_s - shift right (signed)
    I64ShrS = 0x87,
    /// i64.shr_u - shift right (unsigned)
    I64ShrU = 0x88,

    // === f64 arithmetic (WASM 0x99-0xA3) ===
    /// f64.abs - absolute value
    F64Abs = 0x99,
    /// f64.neg - negate
    F64Neg = 0x9A,
    /// f64.ceil - ceiling
    F64Ceil = 0x9B,
    /// f64.floor - floor
    F64Floor = 0x9C,
    /// f64.trunc - truncate toward zero
    F64Trunc = 0x9D,
    /// f64.sqrt - square root
    F64Sqrt = 0x9F,
    /// f64.add
    F64Add = 0xA0,
    /// f64.sub
    F64Sub = 0xA1,
    /// f64.mul
    F64Mul = 0xA2,
    /// f64.div
    F64Div = 0xA3,

    // === Conversions (WASM) ===
    /// f64.convert_i64_s - convert signed i64 to f64
    F64ConvertI64S = 0xB9,
    /// i64.trunc_f64_s - truncate f64 to signed i64
    I64TruncF64S = 0xB0,

    // === RPL extensions (0xC0+) ===
    // These are necessary extensions for RPL semantics that WASM doesn't have.
    // Stack operations (DUP, SWAP, etc.) use CallLib dispatch to libraries.

    /// Call a library command. Followed by lib_id (u16) and cmd_id (u16).
    CallLib = 0xC0,
    /// Push a string constant from rodata. Followed by offset (LEB128) and length (LEB128).
    StringConst = 0xC1,
    /// Evaluate a name at runtime. Followed by offset (LEB128) and length (LEB128) into rodata.
    EvalName = 0xC2,
    /// Create a list from N items on stack. Followed by count (LEB128).
    MakeList = 0xC3,
    /// Create a program object. Followed by param_count (LEB128), rodata_len (LEB128),
    /// rodata bytes, code_len (LEB128), code bytes, span_count (u16), spans.
    MakeProgram = 0xC4,
    /// Push a symbolic expression constant from rodata. Followed by offset (LEB128) and length (LEB128).
    SymbolicConst = 0xC5,
    /// Push a local name scope. Followed by count (LEB128), then for each:
    /// offset (LEB128), length (LEB128), local_idx (LEB128).
    PushLocalScope = 0xC6,
    /// Pop a local name scope.
    PopLocalScope = 0xC7,
    /// Push a blob constant from rodata. Followed by offset (LEB128) and length (LEB128).
    BlobConst = 0xC8,
}

impl Opcode {
    /// Try to decode an opcode from a byte.
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            // Control flow
            0x00 => Some(Opcode::Unreachable),
            0x01 => Some(Opcode::Nop),
            0x02 => Some(Opcode::Block),
            0x03 => Some(Opcode::Loop),
            0x04 => Some(Opcode::If),
            0x05 => Some(Opcode::Else),
            0x08 => Some(Opcode::Throw),
            0x0A => Some(Opcode::ThrowRef),
            0x0B => Some(Opcode::End),
            0x0C => Some(Opcode::Br),
            0x0D => Some(Opcode::BrIf),
            0x0F => Some(Opcode::Return),
            0x1F => Some(Opcode::TryTable),

            // Parametric
            0x1A => Some(Opcode::Drop),
            0x1B => Some(Opcode::Select),

            // Variable access
            0x20 => Some(Opcode::LocalGet),
            0x21 => Some(Opcode::LocalSet),
            0x22 => Some(Opcode::LocalTee),

            // Constants
            0x42 => Some(Opcode::I64Const),
            0x44 => Some(Opcode::F64Const),

            // i64 comparison
            0x50 => Some(Opcode::I64Eqz),
            0x51 => Some(Opcode::I64Eq),
            0x52 => Some(Opcode::I64Ne),
            0x53 => Some(Opcode::I64LtS),
            0x54 => Some(Opcode::I64LtU),
            0x55 => Some(Opcode::I64GtS),
            0x56 => Some(Opcode::I64GtU),
            0x57 => Some(Opcode::I64LeS),
            0x58 => Some(Opcode::I64LeU),
            0x59 => Some(Opcode::I64GeS),
            0x5A => Some(Opcode::I64GeU),

            // f64 comparison
            0x61 => Some(Opcode::F64Eq),
            0x62 => Some(Opcode::F64Ne),
            0x63 => Some(Opcode::F64Lt),
            0x64 => Some(Opcode::F64Gt),
            0x65 => Some(Opcode::F64Le),
            0x66 => Some(Opcode::F64Ge),

            // i64 arithmetic
            0x7C => Some(Opcode::I64Add),
            0x7D => Some(Opcode::I64Sub),
            0x7E => Some(Opcode::I64Mul),
            0x7F => Some(Opcode::I64DivS),
            0x80 => Some(Opcode::I64DivU),
            0x81 => Some(Opcode::I64RemS),
            0x82 => Some(Opcode::I64RemU),
            0x83 => Some(Opcode::I64And),
            0x84 => Some(Opcode::I64Or),
            0x85 => Some(Opcode::I64Xor),
            0x86 => Some(Opcode::I64Shl),
            0x87 => Some(Opcode::I64ShrS),
            0x88 => Some(Opcode::I64ShrU),

            // f64 arithmetic
            0x99 => Some(Opcode::F64Abs),
            0x9A => Some(Opcode::F64Neg),
            0x9B => Some(Opcode::F64Ceil),
            0x9C => Some(Opcode::F64Floor),
            0x9D => Some(Opcode::F64Trunc),
            0x9F => Some(Opcode::F64Sqrt),
            0xA0 => Some(Opcode::F64Add),
            0xA1 => Some(Opcode::F64Sub),
            0xA2 => Some(Opcode::F64Mul),
            0xA3 => Some(Opcode::F64Div),

            // Conversions
            0xB0 => Some(Opcode::I64TruncF64S),
            0xB9 => Some(Opcode::F64ConvertI64S),

            // RPL extensions
            0xC0 => Some(Opcode::CallLib),
            0xC1 => Some(Opcode::StringConst),
            0xC2 => Some(Opcode::EvalName),
            0xC3 => Some(Opcode::MakeList),
            0xC4 => Some(Opcode::MakeProgram),
            0xC5 => Some(Opcode::SymbolicConst),
            0xC6 => Some(Opcode::PushLocalScope),
            0xC7 => Some(Opcode::PopLocalScope),
            0xC8 => Some(Opcode::BlobConst),

            _ => None,
        }
    }

    /// Get the byte representation of this opcode.
    pub fn as_byte(self) -> u8 {
        self as u8
    }
}

/// Block type for structured control flow.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum BlockType {
    /// Empty block (no result).
    Empty = 0x40,
    /// Block produces one value (used for expressions).
    Value = 0x7F,
}

impl BlockType {
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0x40 => Some(BlockType::Empty),
            0x7F => Some(BlockType::Value),
            _ => None,
        }
    }

    pub fn as_byte(self) -> u8 {
        self as u8
    }
}

/// Catch clause kinds for try_table (WASM exception handling).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum CatchKind {
    /// Catch specific tag, push exception args.
    Catch = 0x00,
    /// Catch specific tag, push exception args + exnref.
    CatchRef = 0x01,
    /// Catch all exceptions.
    CatchAll = 0x05,
    /// Catch all exceptions, push exnref.
    CatchAllRef = 0x06,
}

impl CatchKind {
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0x00 => Some(CatchKind::Catch),
            0x01 => Some(CatchKind::CatchRef),
            0x05 => Some(CatchKind::CatchAll),
            0x06 => Some(CatchKind::CatchAllRef),
            _ => None,
        }
    }

    pub fn as_byte(self) -> u8 {
        self as u8
    }

    /// Returns true if this catch kind requires a tag index.
    pub fn has_tag(self) -> bool {
        matches!(self, CatchKind::Catch | CatchKind::CatchRef)
    }
}

/// Helper for reading LEB128 encoded integers.
pub fn read_leb128_u32(bytes: &[u8], offset: &mut usize) -> Option<u32> {
    let mut result: u32 = 0;
    let mut shift = 0;

    loop {
        if *offset >= bytes.len() {
            return None;
        }
        let byte = bytes[*offset];
        *offset += 1;

        result |= ((byte & 0x7F) as u32) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
        if shift >= 32 {
            return None; // Overflow
        }
    }

    Some(result)
}

/// Helper for reading LEB128 encoded signed integers.
pub fn read_leb128_i64(bytes: &[u8], offset: &mut usize) -> Option<i64> {
    let mut result: i64 = 0;
    let mut shift = 0;
    let mut byte;

    loop {
        if *offset >= bytes.len() {
            return None;
        }
        byte = bytes[*offset];
        *offset += 1;

        result |= ((byte & 0x7F) as i64) << shift;
        shift += 7;

        if byte & 0x80 == 0 {
            break;
        }
        if shift >= 64 {
            return None; // Overflow
        }
    }

    // Sign extend if necessary
    if shift < 64 && (byte & 0x40) != 0 {
        result |= !0i64 << shift;
    }

    Some(result)
}

/// Helper for reading f64 (IEEE 754).
pub fn read_f64(bytes: &[u8], offset: &mut usize) -> Option<f64> {
    if *offset + 8 > bytes.len() {
        return None;
    }
    let arr: [u8; 8] = bytes[*offset..*offset + 8].try_into().ok()?;
    *offset += 8;
    Some(f64::from_le_bytes(arr))
}

/// Helper for reading u16 (little-endian).
pub fn read_u16(bytes: &[u8], offset: &mut usize) -> Option<u16> {
    if *offset + 2 > bytes.len() {
        return None;
    }
    let arr: [u8; 2] = bytes[*offset..*offset + 2].try_into().ok()?;
    *offset += 2;
    Some(u16::from_le_bytes(arr))
}

/// Helper for reading u32 (little-endian).
pub fn read_u32(bytes: &[u8], offset: &mut usize) -> Option<u32> {
    if *offset + 4 > bytes.len() {
        return None;
    }
    let arr: [u8; 4] = bytes[*offset..*offset + 4].try_into().ok()?;
    *offset += 4;
    Some(u32::from_le_bytes(arr))
}

/// Helper for writing LEB128 encoded u32.
pub fn write_leb128_u32(value: u32, output: &mut Vec<u8>) {
    let mut val = value;
    loop {
        let mut byte = (val & 0x7F) as u8;
        val >>= 7;
        if val != 0 {
            byte |= 0x80;
        }
        output.push(byte);
        if val == 0 {
            break;
        }
    }
}

/// Helper for writing LEB128 encoded i64.
pub fn write_leb128_i64(value: i64, output: &mut Vec<u8>) {
    let mut val = value;
    loop {
        let byte = (val & 0x7F) as u8;
        val >>= 7;

        // Check if we're done
        let done = (val == 0 && (byte & 0x40) == 0) || (val == -1 && (byte & 0x40) != 0);

        if done {
            output.push(byte);
            break;
        } else {
            output.push(byte | 0x80);
        }
    }
}

/// Helper for writing f64 (IEEE 754).
pub fn write_f64(value: f64, output: &mut Vec<u8>) {
    output.extend_from_slice(&value.to_le_bytes());
}

/// Helper for writing u16 (little-endian).
pub fn write_u16(value: u16, output: &mut Vec<u8>) {
    output.extend_from_slice(&value.to_le_bytes());
}

/// Helper for writing u32 (little-endian).
pub fn write_u32(value: u32, output: &mut Vec<u8>) {
    output.extend_from_slice(&value.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn opcode_roundtrip() {
        let opcodes = [
            // Control flow
            Opcode::Unreachable,
            Opcode::Nop,
            Opcode::Block,
            Opcode::Loop,
            Opcode::If,
            Opcode::Else,
            Opcode::Throw,
            Opcode::ThrowRef,
            Opcode::End,
            Opcode::Br,
            Opcode::BrIf,
            Opcode::Return,
            Opcode::TryTable,
            // Parametric
            Opcode::Drop,
            Opcode::Select,
            // Variable access
            Opcode::LocalGet,
            Opcode::LocalSet,
            Opcode::LocalTee,
            // Constants
            Opcode::I64Const,
            Opcode::F64Const,
            // i64 comparison
            Opcode::I64Eqz,
            Opcode::I64Eq,
            Opcode::I64Ne,
            Opcode::I64LtS,
            Opcode::I64LtU,
            Opcode::I64GtS,
            Opcode::I64GtU,
            Opcode::I64LeS,
            Opcode::I64LeU,
            Opcode::I64GeS,
            Opcode::I64GeU,
            // f64 comparison
            Opcode::F64Eq,
            Opcode::F64Ne,
            Opcode::F64Lt,
            Opcode::F64Gt,
            Opcode::F64Le,
            Opcode::F64Ge,
            // i64 arithmetic
            Opcode::I64Add,
            Opcode::I64Sub,
            Opcode::I64Mul,
            Opcode::I64DivS,
            Opcode::I64DivU,
            Opcode::I64RemS,
            Opcode::I64RemU,
            Opcode::I64And,
            Opcode::I64Or,
            Opcode::I64Xor,
            Opcode::I64Shl,
            Opcode::I64ShrS,
            Opcode::I64ShrU,
            // f64 arithmetic
            Opcode::F64Abs,
            Opcode::F64Neg,
            Opcode::F64Ceil,
            Opcode::F64Floor,
            Opcode::F64Trunc,
            Opcode::F64Sqrt,
            Opcode::F64Add,
            Opcode::F64Sub,
            Opcode::F64Mul,
            Opcode::F64Div,
            // Conversions
            Opcode::I64TruncF64S,
            Opcode::F64ConvertI64S,
            // RPL extensions
            Opcode::CallLib,
            Opcode::StringConst,
            Opcode::EvalName,
            Opcode::MakeList,
            Opcode::MakeProgram,
            Opcode::SymbolicConst,
            Opcode::PushLocalScope,
            Opcode::PopLocalScope,
            Opcode::BlobConst,
        ];

        for op in opcodes {
            let byte = op.as_byte();
            let decoded = Opcode::from_byte(byte);
            assert_eq!(decoded, Some(op), "Failed for {:?}", op);
        }
    }

    #[test]
    fn leb128_u32_roundtrip() {
        let values = [0, 1, 127, 128, 255, 256, 16383, 16384, u32::MAX];

        for &value in &values {
            let mut output = Vec::new();
            write_leb128_u32(value, &mut output);

            let mut offset = 0;
            let decoded = read_leb128_u32(&output, &mut offset).unwrap();
            assert_eq!(decoded, value);
            assert_eq!(offset, output.len());
        }
    }

    #[test]
    fn leb128_i64_roundtrip() {
        let values = [0i64, 1, -1, 63, 64, -64, -65, 127, -128, i64::MAX, i64::MIN];

        for &value in &values {
            let mut output = Vec::new();
            write_leb128_i64(value, &mut output);

            let mut offset = 0;
            let decoded = read_leb128_i64(&output, &mut offset).unwrap();
            assert_eq!(decoded, value, "Failed for {}", value);
            assert_eq!(offset, output.len());
        }
    }

    #[test]
    fn f64_roundtrip() {
        let values = [0.0, 1.0, -1.0, 3.14159, f64::MAX, f64::MIN, f64::INFINITY];

        for &value in &values {
            let mut output = Vec::new();
            write_f64(value, &mut output);

            let mut offset = 0;
            let decoded = read_f64(&output, &mut offset).unwrap();
            assert_eq!(decoded, value);
            assert_eq!(offset, 8);
        }
    }
}
