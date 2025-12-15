//! Library for local variable bindings.
//!
//! Syntax: `value1 value2 → param1 param2 :: body ;`
//!
//! Where parameter names are compile-time identifiers (not runtime strings).
//!
//! Example: `5 → x :: x DUP * ;` computes 5² = 25.
//!
//! ## Bytecode Format
//!
//! - `CMD_LOCAL_FRAME_SETUP`: Sets up local frame with N params.
//!   - Followed by: param_count (1 word), then param_count symbol IDs
//!   - Pops param_count values from stack, binds to param names
//!
//! - `CMD_LOCAL_FRAME_POP`: Pops the local frame at end of body.
//!   - Followed by: param_count (1 word) to know how many to pop
//!
//! - `CMD_LOCAL_LOOKUP`: Looks up a local variable by symbol ID.
//!   - Followed by: symbol ID (1 word)
//!   - Pushes the local's value onto the stack (RCL semantics)

use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::{make_call, Symbol, Word};
use rpl_lang::library::{ConstructKind, ExecuteOk};

rpl_macros::define_library! {
    pub library LocalsLib(32, "Locals");

    // Internal commands - emitted by compiler helper functions, not probed directly
    // Stack effects are (0 -> 0) placeholder since these are internal commands
    commands {
        @LOCAL_FRAME_SETUP (0 -> 0) "Set up local frame with N params" {
            // Read param count
            let count = match ctx.read_operand() {
                Ok(w) => w as usize,
                Err(e) => return Err(e.into()),
            };

            // Read symbol IDs and resolve to names using the interner from VM
            let mut names = Vec::with_capacity(count);
            for _ in 0..count {
                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return Err(e.into()),
                };
                let symbol = Symbol::from_raw(sym_id);
                let name = ctx.interner().resolve(symbol).to_string();
                names.push(name);
            }

            // Pop values from stack (in reverse order - last param gets top of stack)
            let mut values = Vec::with_capacity(count);
            for _ in 0..count {
                match ctx.pop() {
                    Ok(v) => values.push(v),
                    Err(_) => return Err("Stack underflow in local binding".into()),
                }
            }
            values.reverse();

            // Create and push local frame
            let mut frame = rpl_lang::LocalFrame::new();
            for (name, value) in names.into_iter().zip(values.into_iter()) {
                frame.bind(name, value);
            }
            ctx.push_local_frame(frame);
            Ok(ExecuteOk::Ok)
        }

        @LOCAL_FRAME_POP (0 -> 0) "Pop the local frame" {
            // Read param count (for consistency, though we might not need it)
            let _count = ctx.read_operand();
            ctx.pop_local_frame();
            Ok(ExecuteOk::Ok)
        }

        @LOCAL_LOOKUP (0 -> 1) "Look up a local variable" {
            // Read symbol ID
            let sym_id = match ctx.read_operand() {
                Ok(w) => w,
                Err(e) => return Err(e.into()),
            };

            let symbol = Symbol::from_raw(sym_id);
            let name = ctx.interner().resolve(symbol);

            // Look up the local using the string-based lookup
            match ctx.lookup(name) {
                Some(value) => {
                    if let Err(e) = ctx.push(value.clone()) {
                        return Err(format!("{:?}", e));
                    }
                    Ok(ExecuteOk::Ok)
                }
                None => Err(format!("Undefined local variable: {}", name)),
            }
        }
    }

    custom probe {
        match ctx.text() {
            // Arrow starts a local binding
            "→" | "->" => rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(ctx.text().len() as u8),
                semantic: SemanticKind::Keyword,
            },
            _ => rpl_lang::library::ProbeResult::NoMatch,
        }
    }

    custom compile {
        match ctx.text() {
            // Arrow starts a LocalBinding construct
            "→" | "->" => rpl_lang::library::CompileResult::StartConstruct {
                kind: ConstructKind::LocalBinding,
            },
            _ => rpl_lang::library::CompileResult::NoMatch,
        }
    }
}

// Re-export command constants for compiler helper functions
pub const CMD_LOCAL_FRAME_SETUP: u16 = LocalsLib::CMD_LOCAL_FRAME_SETUP;
pub const CMD_LOCAL_FRAME_POP: u16 = LocalsLib::CMD_LOCAL_FRAME_POP;
pub const CMD_LOCAL_LOOKUP: u16 = LocalsLib::CMD_LOCAL_LOOKUP;

/// Emit bytecode for local frame setup.
///
/// Called by the compiler when `::` is encountered after parameter collection.
pub fn emit_frame_setup(output: &mut rpl_lang::compile::OutputBuffer, params: &[Symbol], span: rpl_core::Span) {
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_FRAME_SETUP), span);
    output.emit(params.len() as Word, span);
    for sym in params {
        output.emit(sym.as_u32(), span);
    }
}

/// Emit bytecode for local frame pop.
///
/// Called by the compiler when `;` is encountered in a LocalBinding construct.
pub fn emit_frame_pop(output: &mut rpl_lang::compile::OutputBuffer, param_count: usize, span: rpl_core::Span) {
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_FRAME_POP), span);
    output.emit(param_count as Word, span);
}

/// Emit bytecode for local variable lookup.
///
/// Called by the compiler when a local variable reference is compiled.
pub fn emit_local_lookup(output: &mut rpl_lang::compile::OutputBuffer, symbol: Symbol, span: rpl_core::Span) {
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_LOOKUP), span);
    output.emit(symbol.as_u32(), span);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_lang::compile::OutputBuffer;
    use rpl_lang::library::{CompileContext, CompileResult, Library, ProbeContext, ProbeResult};
    use rpl_core::{Interner, Pos, Span};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_arrow_recognized() {
        let interner = Interner::new();
        let lib = LocalsLib;

        let ctx = make_probe_ctx("→", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("->", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_double_colon_not_recognized_by_locals() {
        let interner = Interner::new();
        let lib = LocalsLib;

        let ctx = make_probe_ctx("::", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn probe_other_not_recognized() {
        let interner = Interner::new();
        let lib = LocalsLib;

        let ctx = make_probe_ctx("DROPLOCAL", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));

        let ctx = make_probe_ctx("x", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn compile_arrow_starts_construct() {
        let mut interner = Interner::new();
        let mut output = OutputBuffer::new();
        let span = Span::new(Pos::new(0), Pos::new(2));

        let mut ctx = CompileContext::new(span, "→", &mut output, &mut interner, None, false);
        let lib = LocalsLib;
        let result = lib.compile(&mut ctx);

        assert!(matches!(
            result,
            CompileResult::StartConstruct {
                kind: ConstructKind::LocalBinding
            }
        ));
    }

    #[test]
    fn compile_double_colon_not_handled_by_locals() {
        let mut interner = Interner::new();
        let mut output = OutputBuffer::new();
        let span = Span::new(Pos::new(0), Pos::new(2));

        let mut ctx = CompileContext::new(span, "::", &mut output, &mut interner, None, false);
        let lib = LocalsLib;
        let result = lib.compile(&mut ctx);

        assert!(matches!(result, CompileResult::NoMatch));
    }
}
