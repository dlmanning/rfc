//! Main decompiler driver.

use rpl_core::{Interner, Word, extract_cmd, extract_lib, is_prolog};

use crate::{
    library::{DecompileContext, DecompileResult, LibraryId, LibraryRegistry},
    dispatch_impl::{DISPATCH_LIB, decode_dispatch_symbol},
};

/// Decompiler that converts bytecode back to source code.
pub struct Decompiler<'a> {
    registry: &'a LibraryRegistry,
}

impl<'a> Decompiler<'a> {
    /// Create a new decompiler.
    pub fn new(registry: &'a LibraryRegistry) -> Self {
        Self { registry }
    }

    /// Decompile bytecode to source code.
    pub fn decompile(&self, code: &[Word]) -> String {
        decompile_inner(code, self.registry, None)
    }

    /// Decompile bytecode to source code with an interner for symbol resolution.
    pub fn decompile_with_interner(&self, code: &[Word], interner: &Interner) -> String {
        decompile_inner(code, self.registry, Some(interner))
    }
}

/// Inner decompilation function that can be called recursively.
pub fn decompile_inner(
    code: &[Word],
    registry: &LibraryRegistry,
    interner: Option<&Interner>,
) -> String {
    let mut output = String::new();
    let mut pos = 0;

    while pos < code.len() {
        // Add space between tokens (but not at the start)
        if !output.is_empty() && !output.ends_with(' ') && !output.ends_with('\n') {
            output.push(' ');
        }

        let word = code[pos];

        // Handle prologs (object literals) - try each library by priority
        if is_prolog(word) {
            let start_pos = pos;
            let mut ctx = if let Some(int) = interner {
                DecompileContext::for_prolog_with_interner(
                    code,
                    &mut pos,
                    &mut output,
                    registry,
                    int,
                )
            } else {
                DecompileContext::for_prolog_with_registry(code, &mut pos, &mut output, registry)
            };

            let mut handled = false;
            for lib in registry.iter_by_priority() {
                let result = lib.decompile(&mut ctx);
                if matches!(result, DecompileResult::Ok) {
                    handled = true;
                    break;
                }
            }

            if !handled {
                output.push_str(&format!("0x{:08X}", word));
                pos = start_pos + 1;
            }
            continue;
        }

        // Handle calls - extract lib/cmd and dispatch to specific library
        let lib_id_raw = extract_lib(word);
        let cmd = extract_cmd(word);

        // Check for dynamic dispatch first
        if lib_id_raw == DISPATCH_LIB {
            let symbol = decode_dispatch_symbol(cmd);
            output.push_str(symbol);
            pos += 1;
            continue;
        }

        // Find the library and call its decompile with cmd
        let lib_id = LibraryId::new(lib_id_raw);
        if let Some(lib) = registry.get(lib_id) {
            // Advance past the opcode before creating context
            pos += 1;
            let mut ctx = if let Some(int) = interner {
                DecompileContext::for_call_with_interner(
                    code,
                    &mut pos,
                    &mut output,
                    registry,
                    cmd,
                    int,
                )
            } else {
                DecompileContext::for_call_with_registry(code, &mut pos, &mut output, registry, cmd)
            };
            let result = lib.decompile(&mut ctx);

            if !matches!(result, DecompileResult::Ok) {
                // Library didn't handle it, output as hex
                output.push_str(&format!("0x{:08X}", word));
            }
        } else {
            // Unknown library
            output.push_str(&format!("0x{:08X}", word));
            pos += 1;
        }
    }

    output.trim().to_string()
}

/// Convenience function to decompile bytecode using the standard library registry.
pub fn decompile(code: &[Word], registry: &LibraryRegistry) -> String {
    decompile_inner(code, registry, None)
}

/// Decompile bytecode with an interner for symbol name resolution.
pub fn decompile_with_interner(
    code: &[Word],
    registry: &LibraryRegistry,
    interner: &Interner,
) -> String {
    decompile_inner(code, registry, Some(interner))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_registry() -> LibraryRegistry {
        // Empty registry for unit tests - decompiler handles unknown libs gracefully
        LibraryRegistry::new()
    }

    #[test]
    fn decompile_empty() {
        let registry = make_registry();
        let result = decompile(&[], &registry);
        assert_eq!(result, "");
    }

    // NOTE: This test requires registered libraries to decompile prologs.
    // Run as integration test in rpl-session instead.
    #[test]
    #[ignore = "requires stdlib"]
    fn decompile_real_literal() {
        let registry = make_registry();

        // Create bytecode for a real number: PROLOG(REAL, 2) followed by f64 bits
        let value: f64 = 42.0;
        let bits = value.to_bits();
        let high = (bits >> 32) as u32;
        let low = bits as u32;

        let prolog = rpl_core::make_prolog(rpl_core::TypeId::REAL.as_u16(), 2);
        let code = vec![prolog, high, low];

        let result = decompile(&code, &registry);
        assert_eq!(result, "42");
    }

    // NOTE: This test requires registered libraries to decompile prologs.
    // Run as integration test in rpl-session instead.
    #[test]
    #[ignore = "requires stdlib"]
    fn decompile_multiple_reals() {
        let registry = make_registry();

        // Two real numbers: 3.14 and 2.0
        let mut code = Vec::new();

        let value1: f64 = 3.15;
        let bits1 = value1.to_bits();
        code.push(rpl_core::make_prolog(rpl_core::TypeId::REAL.as_u16(), 2));
        code.push((bits1 >> 32) as u32);
        code.push(bits1 as u32);

        let value2: f64 = 2.0;
        let bits2 = value2.to_bits();
        code.push(rpl_core::make_prolog(rpl_core::TypeId::REAL.as_u16(), 2));
        code.push((bits2 >> 32) as u32);
        code.push(bits2 as u32);

        let result = decompile(&code, &registry);
        assert_eq!(result, "3.15 2");
    }

    #[test]
    fn decompile_unknown_word() {
        let registry = make_registry();

        // Unknown library call
        let code = vec![rpl_core::make_call(999, 0)];
        let result = decompile(&code, &registry);
        // Should output as hex
        assert!(result.starts_with("0x"));
    }

    // NOTE: Tests requiring full stdlib (decompile_stack_commands, decompile_arithmetic)
    // and round-trip tests (round_trip_*) have been moved to integration tests in
    // rpl-session to avoid circular dependencies.
}
