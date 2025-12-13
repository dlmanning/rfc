/// A 32-bit word in compiled bytecode.
pub type Word = u32;

// Bit layout for prolog words:
// Bit 31: 1 = prolog, 0 = call
// Bits 16-30: type ID (15 bits)
// Bits 0-15: size (16 bits)
const PROLOG_BIT: u32 = 1 << 31;
const TYPE_SHIFT: u32 = 16;
const TYPE_MASK: u32 = 0x7FFF; // 15 bits
const SIZE_MASK: u32 = 0xFFFF; // 16 bits

// Bit layout for call words:
// Bit 31: 0 = call
// Bits 16-30: library ID (15 bits)
// Bits 0-15: command (16 bits)
const LIB_SHIFT: u32 = 16;
const LIB_MASK: u32 = 0x7FFF; // 15 bits
const CMD_MASK: u32 = 0xFFFF; // 16 bits

/// Check if a word is a prolog (object header).
pub fn is_prolog(word: Word) -> bool {
    (word & PROLOG_BIT) != 0
}

/// Extract library ID from a call word.
pub fn extract_lib(word: Word) -> u16 {
    ((word >> LIB_SHIFT) & LIB_MASK) as u16
}

/// Extract command from a call word.
pub fn extract_cmd(word: Word) -> u16 {
    (word & CMD_MASK) as u16
}

/// Extract type ID from a prolog word.
pub fn extract_type(word: Word) -> u16 {
    ((word >> TYPE_SHIFT) & TYPE_MASK) as u16
}

/// Extract size from a prolog word.
pub fn extract_size(word: Word) -> u16 {
    (word & SIZE_MASK) as u16
}

/// Create a call word from library ID and command.
pub fn make_call(lib: u16, cmd: u16) -> Word {
    ((lib as u32 & LIB_MASK) << LIB_SHIFT) | (cmd as u32 & CMD_MASK)
}

/// Create a prolog word from type ID and size.
pub fn make_prolog(type_id: u16, size: u16) -> Word {
    PROLOG_BIT | ((type_id as u32 & TYPE_MASK) << TYPE_SHIFT) | (size as u32 & SIZE_MASK)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prolog_round_trip() {
        let type_id: u16 = 56; // SYMBOLIC
        let size: u16 = 100;
        let word = make_prolog(type_id, size);

        assert!(is_prolog(word));
        assert_eq!(extract_type(word), type_id);
        assert_eq!(extract_size(word), size);
    }

    #[test]
    fn call_round_trip() {
        let lib: u16 = 72; // Stack lib
        let cmd: u16 = 5;
        let word = make_call(lib, cmd);

        assert!(!is_prolog(word));
        assert_eq!(extract_lib(word), lib);
        assert_eq!(extract_cmd(word), cmd);
    }

    #[test]
    fn prolog_max_values() {
        let type_id: u16 = 0x7FFF; // max 15 bits
        let size: u16 = 0xFFFF; // max 16 bits
        let word = make_prolog(type_id, size);

        assert!(is_prolog(word));
        assert_eq!(extract_type(word), type_id);
        assert_eq!(extract_size(word), size);
    }

    #[test]
    fn call_max_values() {
        let lib: u16 = 0x7FFF; // max 15 bits
        let cmd: u16 = 0xFFFF; // max 16 bits
        let word = make_call(lib, cmd);

        assert!(!is_prolog(word));
        assert_eq!(extract_lib(word), lib);
        assert_eq!(extract_cmd(word), cmd);
    }

    #[test]
    fn prolog_zero_values() {
        let word = make_prolog(0, 0);
        assert!(is_prolog(word));
        assert_eq!(extract_type(word), 0);
        assert_eq!(extract_size(word), 0);
    }

    #[test]
    fn call_zero_values() {
        let word = make_call(0, 0);
        assert!(!is_prolog(word));
        assert_eq!(extract_lib(word), 0);
        assert_eq!(extract_cmd(word), 0);
    }

    #[test]
    fn prolog_and_call_distinguished() {
        // Same bit pattern except prolog bit
        let prolog = make_prolog(100, 200);
        let call = make_call(100, 200);

        assert!(is_prolog(prolog));
        assert!(!is_prolog(call));
        assert_ne!(prolog, call);
    }
}
