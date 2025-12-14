//! Generic binary blob support for RPL.
//!
//! Blobs are typed binary data containers. Each blob has:
//! - A subtype ID (u32) for application-specific typing
//! - A byte length
//! - Raw byte data packed into words
//!
//! Storage format (in Value::Object data):
//! - Word 0: subtype_id
//! - Word 1: byte_length
//! - Words 2+: packed bytes (4 per word, little-endian)

use rpl_core::TypeId;
use rpl_lang::{Value, library::EXEC_OK};

/// Create a blob value from subtype and bytes.
pub fn make_blob(subtype: u32, bytes: &[u8]) -> Value {
    let mut data = Vec::with_capacity(2 + bytes.len().div_ceil(4));
    data.push(subtype);
    data.push(bytes.len() as u32);

    // Pack bytes into words (little-endian)
    for chunk in bytes.chunks(4) {
        let mut word = 0u32;
        for (i, &b) in chunk.iter().enumerate() {
            word |= (b as u32) << (i * 8);
        }
        data.push(word);
    }

    Value::Object {
        type_id: TypeId::BLOB,
        data,
    }
}

/// Extract blob data from a value.
/// Returns (subtype, bytes) if the value is a valid blob.
pub fn parse_blob(value: &Value) -> Option<(u32, Vec<u8>)> {
    match value {
        Value::Object { type_id, data } if *type_id == TypeId::BLOB && data.len() >= 2 => {
            let subtype = data[0];
            let byte_len = data[1] as usize;

            // Unpack bytes from words
            let mut bytes = Vec::with_capacity(byte_len);
            for &word in &data[2..] {
                bytes.push((word & 0xFF) as u8);
                if bytes.len() < byte_len {
                    bytes.push(((word >> 8) & 0xFF) as u8);
                }
                if bytes.len() < byte_len {
                    bytes.push(((word >> 16) & 0xFF) as u8);
                }
                if bytes.len() < byte_len {
                    bytes.push(((word >> 24) & 0xFF) as u8);
                }
            }
            bytes.truncate(byte_len);

            Some((subtype, bytes))
        }
        _ => None,
    }
}

rpl_macros::define_library! {
    pub library BlobLib(85, "Blob");

    commands {
        // MKBLOB: subtype n_bytes byte... → blob
        MKBLOB (*) "Create blob from stack values" {
            let n_bytes = match ctx.pop() {
                Ok(Value::Int(n)) if n >= 0 => n as usize,
                Ok(Value::Real(r)) if r >= 0.0 => r as usize,
                Ok(_) => return Err("MKBLOB: expected non-negative count".into()),
                Err(_) => return Err("MKBLOB: stack underflow".into()),
            };

            let mut bytes = Vec::with_capacity(n_bytes);
            for _ in 0..n_bytes {
                let b = match ctx.pop() {
                    Ok(Value::Int(i)) => (i & 0xFF) as u8,
                    Ok(Value::Real(r)) => (r as i64 & 0xFF) as u8,
                    Ok(_) => return Err("MKBLOB: expected byte values".into()),
                    Err(_) => return Err("MKBLOB: stack underflow".into()),
                };
                bytes.push(b);
            }
            bytes.reverse();

            let subtype = match ctx.pop() {
                Ok(Value::Int(i)) => i as u32,
                Ok(Value::Real(r)) => r as u32,
                Ok(_) => return Err("MKBLOB: expected subtype number".into()),
                Err(_) => return Err("MKBLOB: stack underflow".into()),
            };

            let blob = make_blob(subtype, &bytes);
            if ctx.push(blob).is_err() {
                return Err("Stack overflow".into());
            }
            EXEC_OK
        }

        // BLOBTYPE: blob → subtype
        BLOBTYPE (1 -> 1) "Get blob subtype" {
            let blob = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("BLOBTYPE: stack underflow".into()),
            };

            match parse_blob(&blob) {
                Some((subtype, _)) => {
                    if ctx.push(Value::Int(subtype as i64)).is_err() {
                        return Err("Stack overflow".into());
                    }
                    EXEC_OK
                }
                None => Err("BLOBTYPE: expected blob".into()),
            }
        }

        // BLOBSIZE: blob → size
        BLOBSIZE (1 -> 1) "Get blob size in bytes" {
            let blob = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("BLOBSIZE: stack underflow".into()),
            };

            match parse_blob(&blob) {
                Some((_, bytes)) => {
                    if ctx.push(Value::Int(bytes.len() as i64)).is_err() {
                        return Err("Stack overflow".into());
                    }
                    EXEC_OK
                }
                None => Err("BLOBSIZE: expected blob".into()),
            }
        }

        // BLOB→LIST: blob → list (with ASCII alias BLOB->LIST)
        BLOBTOLIST | "BLOB→LIST" | "BLOB->LIST" (1 -> 1) "Convert blob to list of bytes" {
            let blob = match ctx.pop() {
                Ok(v) => v,
                Err(_) => return Err("BLOB→LIST: stack underflow".into()),
            };

            match parse_blob(&blob) {
                Some((_, bytes)) => {
                    let list: Vec<Value> = bytes.into_iter()
                        .map(|b| Value::Int(b as i64))
                        .collect();
                    if ctx.push(Value::List(list)).is_err() {
                        return Err("Stack overflow".into());
                    }
                    EXEC_OK
                }
                None => Err("BLOB→LIST: expected blob".into()),
            }
        }

        // LIST→BLOB: subtype list → blob (with ASCII alias LIST->BLOB)
        LISTTOBLOB | "LIST→BLOB" | "LIST->BLOB" (2 -> 1) "Convert list of bytes to blob" {
            let list = match ctx.pop() {
                Ok(Value::List(l)) => l,
                Ok(_) => return Err("LIST→BLOB: expected list".into()),
                Err(_) => return Err("LIST→BLOB: stack underflow".into()),
            };

            let subtype = match ctx.pop() {
                Ok(Value::Int(i)) => i as u32,
                Ok(Value::Real(r)) => r as u32,
                Ok(_) => return Err("LIST→BLOB: expected subtype number".into()),
                Err(_) => return Err("LIST→BLOB: stack underflow".into()),
            };

            let mut bytes = Vec::with_capacity(list.len());
            for val in list {
                match val {
                    Value::Int(i) => bytes.push((i & 0xFF) as u8),
                    Value::Real(r) => bytes.push((r as i64 & 0xFF) as u8),
                    _ => return Err("LIST→BLOB: list must contain only numbers".into()),
                }
            }

            let blob = make_blob(subtype, &bytes);
            if ctx.push(blob).is_err() {
                return Err("Stack overflow".into());
            }
            EXEC_OK
        }
    }
}

#[cfg(test)]
mod tests {
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::{
        VM,
        library::{ExecuteContext, ExecuteOk, Library, ProbeContext, ProbeResult, StackEffect},
    };

    use super::*;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn make_and_parse_blob() {
        let bytes = vec![1, 2, 3, 4, 5];
        let blob = make_blob(42, &bytes);

        let (subtype, parsed_bytes) = parse_blob(&blob).unwrap();
        assert_eq!(subtype, 42);
        assert_eq!(parsed_bytes, bytes);
    }

    #[test]
    fn make_and_parse_empty_blob() {
        let blob = make_blob(0, &[]);

        let (subtype, bytes) = parse_blob(&blob).unwrap();
        assert_eq!(subtype, 0);
        assert!(bytes.is_empty());
    }

    #[test]
    fn make_and_parse_blob_non_aligned() {
        // Test with byte count not multiple of 4
        let bytes = vec![0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77];
        let blob = make_blob(100, &bytes);

        let (subtype, parsed_bytes) = parse_blob(&blob).unwrap();
        assert_eq!(subtype, 100);
        assert_eq!(parsed_bytes, bytes);
    }

    #[test]
    fn probe_commands() {
        let interner = Interner::new();
        let lib = BlobLib;

        for cmd in &["MKBLOB", "BLOBTYPE", "BLOBSIZE", "BLOB→LIST", "LIST→BLOB"] {
            let ctx = make_probe_ctx(cmd, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Failed for {}",
                cmd
            );
        }
    }

    #[test]
    fn probe_ascii_arrows() {
        let interner = Interner::new();
        let lib = BlobLib;

        let ctx = make_probe_ctx("BLOB->LIST", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("LIST->BLOB", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn execute_blobtype() {
        let lib = BlobLib;
        let mut vm = VM::new();

        let blob = make_blob(123, &[1, 2, 3]);
        vm.push(blob).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, BlobLib::CMD_BLOBTYPE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert_eq!(vm.pop_int().unwrap(), 123);
    }

    #[test]
    fn execute_blobsize() {
        let lib = BlobLib;
        let mut vm = VM::new();

        let blob = make_blob(0, &[1, 2, 3, 4, 5]);
        vm.push(blob).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, BlobLib::CMD_BLOBSIZE);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));
        assert_eq!(vm.pop_int().unwrap(), 5);
    }

    #[test]
    fn execute_blob_to_list() {
        let lib = BlobLib;
        let mut vm = VM::new();

        let blob = make_blob(0, &[10, 20, 30]);
        vm.push(blob).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, BlobLib::CMD_BLOBTOLIST);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        match vm.pop().unwrap() {
            Value::List(list) => {
                assert_eq!(list.len(), 3);
                assert!(matches!(list[0], Value::Int(10)));
                assert!(matches!(list[1], Value::Int(20)));
                assert!(matches!(list[2], Value::Int(30)));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn execute_list_to_blob() {
        let lib = BlobLib;
        let mut vm = VM::new();

        vm.push(Value::Int(42)).unwrap(); // subtype
        vm.push(Value::List(vec![
            Value::Int(0xAA),
            Value::Int(0xBB),
            Value::Int(0xCC),
        ]))
        .unwrap();

        let mut ctx = make_exec_ctx(&mut vm, BlobLib::CMD_LISTTOBLOB);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        let blob = vm.pop().unwrap();
        let (subtype, bytes) = parse_blob(&blob).unwrap();
        assert_eq!(subtype, 42);
        assert_eq!(bytes, vec![0xAA, 0xBB, 0xCC]);
    }

    #[test]
    fn execute_mkblob() {
        let lib = BlobLib;
        let mut vm = VM::new();

        // Stack: subtype byte1 byte2 byte3 n_bytes
        vm.push(Value::Int(99)).unwrap(); // subtype
        vm.push(Value::Int(0x11)).unwrap(); // byte 1
        vm.push(Value::Int(0x22)).unwrap(); // byte 2
        vm.push(Value::Int(0x33)).unwrap(); // byte 3
        vm.push(Value::Int(3)).unwrap(); // n_bytes

        let mut ctx = make_exec_ctx(&mut vm, BlobLib::CMD_MKBLOB);
        let result = lib.execute(&mut ctx);
        assert!(matches!(result, Ok(ExecuteOk::Ok)));

        let blob = vm.pop().unwrap();
        let (subtype, bytes) = parse_blob(&blob).unwrap();
        assert_eq!(subtype, 99);
        assert_eq!(bytes, vec![0x11, 0x22, 0x33]);
    }

    #[test]
    fn stack_effects() {
        let lib = BlobLib;

        assert!(matches!(
            lib.stack_effect("BLOBTYPE"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("BLOBSIZE"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        // Arrow aliases now work correctly with the alias feature
        assert!(matches!(
            lib.stack_effect("BLOB→LIST"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("BLOB->LIST"),
            StackEffect::Fixed {
                consumes: 1,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("LIST→BLOB"),
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        ));
        assert!(matches!(
            lib.stack_effect("LIST->BLOB"),
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        ));
        assert!(matches!(lib.stack_effect("MKBLOB"), StackEffect::Dynamic));
    }
}
