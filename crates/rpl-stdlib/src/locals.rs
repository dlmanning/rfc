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

use rpl_core::{Symbol, Word, make_call};
use rpl_lang::library::{
    CompileContext, CompileResult, ConstructKind, DecompileContext, DecompileResult,
    ExecuteContext, ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_core::token::TokenInfo;

/// Library for local variable bindings.
pub struct LocalsLib;

/// Command: Set up a local frame.
pub const CMD_LOCAL_FRAME_SETUP: u16 = 0;
/// Command: Pop the local frame.
pub const CMD_LOCAL_FRAME_POP: u16 = 1;
/// Command: Look up a local variable.
pub const CMD_LOCAL_LOOKUP: u16 = 2;

impl LocalsLib {
    /// Library ID for locals.
    pub const ID: LibraryId = LibraryId::new(32);
}

impl Library for LocalsLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Locals"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        use rpl_core::token::SemanticKind;

        match ctx.text() {
            // Arrow starts a local binding
            "→" | "->" => ProbeResult::Match {
                info: TokenInfo::atom(ctx.text().len() as u8),
                semantic: SemanticKind::Keyword,
            },
            // Note: `::` is NOT probed here - it's handled by ProgramsLib
            // The compiler handles `::` specially during parameter collection
            _ => ProbeResult::NoMatch,
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        match ctx.text() {
            // Arrow starts a LocalBinding construct
            "→" | "->" => CompileResult::StartConstruct {
                kind: ConstructKind::LocalBinding,
            },
            // Note: `::` is NOT handled here - it's handled by ProgramsLib
            // The compiler handles `::` specially during parameter collection
            _ => CompileResult::NoMatch,
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            CMD_LOCAL_FRAME_SETUP => {
                // Read param count
                let count = match ctx.read_operand() {
                    Ok(w) => w as usize,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Read symbol IDs and resolve to names using the interner from VM
                let mut names = Vec::with_capacity(count);
                for _ in 0..count {
                    let sym_id = match ctx.read_operand() {
                        Ok(w) => w,
                        Err(e) => return ExecuteResult::Error(e.into()),
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
                        Err(_) => return ExecuteResult::Error("Stack underflow in local binding".into()),
                    }
                }
                values.reverse();

                // Create and push local frame
                let mut frame = rpl_lang::LocalFrame::new();
                for (name, value) in names.into_iter().zip(values.into_iter()) {
                    frame.bind(name, value);
                }
                ctx.push_local_frame(frame);
                ExecuteResult::Ok
            }
            CMD_LOCAL_FRAME_POP => {
                // Read param count (for consistency, though we might not need it)
                let _count = ctx.read_operand();
                ctx.pop_local_frame();
                ExecuteResult::Ok
            }
            CMD_LOCAL_LOOKUP => {
                // Read symbol ID
                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                let symbol = Symbol::from_raw(sym_id);
                let name = ctx.interner().resolve(symbol);

                // Look up the local using the string-based lookup
                // Note: lookup() checks local frames first, then globals
                match ctx.lookup(name) {
                    Some(value) => {
                        if let Err(e) = ctx.push(value.clone()) {
                            return ExecuteResult::Error(format!("{:?}", e));
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Undefined local variable: {}", name)),
                }
            }
            cmd => ExecuteResult::Error(format!("Unknown LocalsLib command: {}", cmd)),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        let cmd = match ctx.mode() {
            DecompileMode::Prolog => return DecompileResult::Unknown,
            DecompileMode::Call(cmd) => cmd,
        };

        match cmd {
            CMD_LOCAL_FRAME_SETUP => {
                // Read param count
                let count = ctx.read().unwrap_or(0) as usize;

                // Read symbol IDs and convert to names
                let mut param_names = Vec::with_capacity(count);
                for _ in 0..count {
                    let sym_id = ctx.read().unwrap_or(0);
                    let symbol = Symbol::from_raw(sym_id);
                    let name = if let Some(interner) = ctx.interner {
                        interner.resolve(symbol).to_string()
                    } else {
                        format!("${}", sym_id)
                    };
                    param_names.push(name);
                }

                // Format: → param1 param2 ::
                // The body is compiled inline (no PROGRAM prolog), so we emit "::" here
                ctx.write("→ ");
                for (i, name) in param_names.iter().enumerate() {
                    if i > 0 {
                        ctx.write(" ");
                    }
                    ctx.write(name);
                }
                ctx.write(" :: ");
                DecompileResult::Ok
            }
            CMD_LOCAL_FRAME_POP => {
                // Pop the local frame - emit the closing semicolon
                let _count = ctx.read();
                ctx.write(" ;");
                DecompileResult::Ok
            }
            CMD_LOCAL_LOOKUP => {
                // Read symbol ID and resolve name
                let sym_id = ctx.read().unwrap_or(0);
                let symbol = Symbol::from_raw(sym_id);
                let name = if let Some(interner) = ctx.interner {
                    interner.resolve(symbol).to_string()
                } else {
                    format!("${}", sym_id)
                };
                ctx.write(&name);
                DecompileResult::Ok
            }
            _ => DecompileResult::Unknown,
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token {
            "→" | "->" => StackEffect::StartConstruct,
            // Note: "::" is handled by ProgramsLib, not LocalsLib
            _ => StackEffect::Dynamic,
        }
    }
}

/// Emit bytecode for local frame setup.
///
/// Called by the compiler when `::` is encountered after parameter collection.
pub fn emit_frame_setup(output: &mut rpl_lang::compile::OutputBuffer, params: &[Symbol], span: rpl_core::Span) {
    // Emit CMD_LOCAL_FRAME_SETUP
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_FRAME_SETUP), span);
    // Emit param count
    output.emit(params.len() as Word, span);
    // Emit symbol IDs
    for sym in params {
        output.emit(sym.as_u32(), span);
    }
}

/// Emit bytecode for local frame pop.
///
/// Called by the compiler when `;` is encountered in a LocalBinding construct.
pub fn emit_frame_pop(output: &mut rpl_lang::compile::OutputBuffer, param_count: usize, span: rpl_core::Span) {
    // Emit CMD_LOCAL_FRAME_POP
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_FRAME_POP), span);
    // Emit param count
    output.emit(param_count as Word, span);
}

/// Emit bytecode for local variable lookup.
///
/// Called by the compiler when a local variable reference is compiled.
pub fn emit_local_lookup(output: &mut rpl_lang::compile::OutputBuffer, symbol: Symbol, span: rpl_core::Span) {
    // Emit CMD_LOCAL_LOOKUP
    output.emit(make_call(LocalsLib::ID.as_u16(), CMD_LOCAL_LOOKUP), span);
    // Emit symbol ID
    output.emit(symbol.as_u32(), span);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_lang::compile::OutputBuffer;
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
        // Note: :: is handled by ProgramsLib, not LocalsLib
        // The compiler specially handles :: during parameter collection
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
        // Note: :: is handled by ProgramsLib, not LocalsLib
        // The compiler specially handles :: during parameter collection
        let mut interner = Interner::new();
        let mut output = OutputBuffer::new();
        let span = Span::new(Pos::new(0), Pos::new(2));

        let mut ctx = CompileContext::new(span, "::", &mut output, &mut interner, None, false);
        let lib = LocalsLib;
        let result = lib.compile(&mut ctx);

        assert!(matches!(result, CompileResult::NoMatch));
    }
}
