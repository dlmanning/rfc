//! Library for identifier tokens.
//!
//! This library provides a "catch-all" for identifiers that aren't recognized
//! by other libraries. It has the lowest priority and matches any valid
//! identifier (starting with a letter, followed by alphanumerics/underscores).
//!
//! During compilation, identifiers are handled as:
//! - Local variable references (handled by compiler BEFORE calling this library)
//! - Global variable lookups via CMD_EVAL_NAME
//!
//! At runtime, CMD_EVAL_NAME:
//! 1. Looks up the name in the global directory
//! 2. If not found → runtime error "Undefined: name"
//! 3. If found and value is Program → execute it (like EVAL)
//! 4. If found and value is anything else → push it

use rpl_core::{make_call, Symbol, TypeId};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult, ExecuteContext,
    ExecuteResult, Library, LibraryId, ProbeContext, ProbeResult, StackEffect,
};
use rpl_core::token::{SemanticKind, TokenInfo};

/// Library for identifier tokens.
pub struct IdentifiersLib;

/// Command: Evaluate a name (global lookup with auto-execute for programs).
pub const CMD_EVAL_NAME: u16 = 0;

/// Command: Push a symbolic variable reference (just pushes the name, doesn't evaluate).
/// Used inside symbolic expressions like `'x + y'`.
pub const CMD_SYMBOLIC_VAR: u16 = 1;

impl IdentifiersLib {
    /// Library ID for identifiers (HP RPL Library 2).
    pub const ID: LibraryId = LibraryId::new(2);
}

impl Library for IdentifiersLib {
    fn id(&self) -> LibraryId {
        Self::ID
    }

    fn name(&self) -> &'static str {
        "Identifiers"
    }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        let text = ctx.text();

        // Check if this is a valid identifier:
        // - Starts with a letter or underscore
        // - Followed by letters, digits, or underscores
        if is_valid_identifier(text) {
            ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Variable,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        let text = ctx.text().to_string();

        // Local variable handling is done in the compiler BEFORE calling this.
        // If we get here, emit a global lookup (CMD_EVAL_NAME).
        let symbol = ctx.intern(&text);

        // Emit CMD_EVAL_NAME + symbol ID
        ctx.emit_opcode(Self::ID.as_u16(), CMD_EVAL_NAME);
        ctx.emit(symbol.as_u32());

        CompileResult::Ok
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            CMD_EVAL_NAME => {
                // Read symbol ID operand
                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Resolve symbol to name string using the interner from VM
                let symbol = Symbol::from_raw(sym_id);
                let name = ctx.interner().resolve(symbol);

                // Look up in global directory (NOT locals - those are handled at compile time)
                match ctx.lookup_global(name) {
                    Some(value) => {
                        let value = value.clone();

                        // Check if it's a Program - if so, execute it
                        // First check for the new Value::Program variant with optional debug info
                        if let rpl_lang::Value::Program { code, debug_info } = &value {
                            // Has debug info - return EvalProgramWithDebug
                            if let Some(dbg) = debug_info {
                                return ExecuteResult::EvalProgramWithDebug {
                                    code: code.clone(),
                                    name: name.to_string(),
                                    debug_info: dbg.clone(),
                                };
                            }
                            // No debug info - return EvalProgramNamed
                            return ExecuteResult::EvalProgramNamed(code.clone(), name.to_string());
                        }

                        // Legacy: Check for Object with PROGRAM type_id
                        if let rpl_lang::Value::Object { type_id, data } = &value
                            && *type_id == TypeId::PROGRAM {
                                // Return EvalProgramNamed to execute the program's bytecode
                                // Include the function name for stack traces
                                return ExecuteResult::EvalProgramNamed(
                                    data.clone(),
                                    name.to_string(),
                                );
                            }

                        // Otherwise, push the value
                        if ctx.push(value).is_err() {
                            return ExecuteResult::Error("Stack overflow".into());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Undefined: {}", name)),
                }
            }
            CMD_SYMBOLIC_VAR => {
                // Read symbol ID operand
                let sym_id = match ctx.read_operand() {
                    Ok(w) => w,
                    Err(e) => return ExecuteResult::Error(e.into()),
                };

                // Resolve symbol to name
                let symbol = Symbol::from_raw(sym_id);
                let name = ctx.interner().resolve(symbol);

                // Look up in locals first, then globals (matches HP semantics)
                match ctx.lookup(name) {
                    Some(value) => {
                        if ctx.push(value.clone()).is_err() {
                            return ExecuteResult::Error("Stack overflow".into());
                        }
                        ExecuteResult::Ok
                    }
                    None => ExecuteResult::Error(format!("Undefined: {}", name)),
                }
            }
            cmd => ExecuteResult::Error(format!("Unknown IdentifiersLib command: {}", cmd)),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        use rpl_lang::library::DecompileMode;

        let cmd = match ctx.mode() {
            DecompileMode::Prolog => return DecompileResult::Unknown,
            DecompileMode::Call(cmd) => cmd,
        };

        match cmd {
            CMD_EVAL_NAME => {
                // Read symbol ID and resolve to name
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

    fn stack_effect(&self, _token: &str) -> StackEffect {
        // An identifier may push one value (if not a program) or have variable effect (if program)
        StackEffect::Dynamic
    }
}

/// Check if a string is a valid identifier.
fn is_valid_identifier(text: &str) -> bool {
    let mut chars = text.chars();

    // Must start with a letter or underscore
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }

    // Rest must be letters, digits, or underscores
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Emit bytecode for a global name lookup.
///
/// Called by the compiler for identifiers that are not local variables.
pub fn emit_eval_name(
    output: &mut rpl_lang::compile::OutputBuffer,
    symbol: Symbol,
    span: rpl_core::Span,
) {
    output.emit(make_call(IdentifiersLib::ID.as_u16(), CMD_EVAL_NAME), span);
    output.emit(symbol.as_u32(), span);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::compile::OutputBuffer;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_simple_identifier() {
        let interner = Interner::new();
        let lib = IdentifiersLib;

        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("x", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("myVar123", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_identifier_with_underscore() {
        let interner = Interner::new();
        let lib = IdentifiersLib;

        let ctx = make_probe_ctx("_foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));

        let ctx = make_probe_ctx("foo_bar", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_not_identifier() {
        let interner = Interner::new();
        let lib = IdentifiersLib;

        // Numbers are not identifiers
        let ctx = make_probe_ctx("123", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));

        // Starting with number
        let ctx = make_probe_ctx("1foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));

        // Special characters
        let ctx = make_probe_ctx("+", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn compile_identifier_emits_eval_name() {
        use rpl_lang::library::CompileContext;

        let mut interner = Interner::new();
        let mut output = OutputBuffer::new();
        let span = Span::new(Pos::new(0), Pos::new(3));

        let mut ctx = CompileContext::new(span, "foo", &mut output, &mut interner, None, false);
        let lib = IdentifiersLib;
        let result = lib.compile(&mut ctx);

        assert!(matches!(result, CompileResult::Ok));
        // Should emit 2 words: CMD_EVAL_NAME call + symbol ID
        assert_eq!(output.len(), 2);
    }
}
