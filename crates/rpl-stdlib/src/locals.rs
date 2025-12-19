//! Local variable bindings library.
//!
//! Provides the `→` (or `->`) syntax for local variable bindings,
//! using HP calculator syntax:
//!
//! ```text
//! → x y z « body »
//! ```
//!
//! Or with ASCII:
//!
//! ```text
//! -> x y z << body >>
//! ```
//!
//! This pops values from the stack and binds them to local variables
//! that can be referenced in the body. The rightmost name gets the
//! top-of-stack value.
//!
//! # Example
//!
//! ```text
//! 1 2 → a b « a b + »
//! ```
//!
//! This pops 2 and 1, binds a=1 and b=2, then executes `a b +` which
//! pushes 1, pushes 2, and adds them.

use std::sync::OnceLock;

use rpl::core::Span;
use rpl::interface::InterfaceSpec;

use rpl::{
    ir::{AtomKind, Branch, LibId, NodeKind},
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
};

/// Interface declaration for the Locals library.
///
/// The `( $name:Sym )*` repeat parses variable names until the delimiter.
/// Each binding produces a branch with [index, name].
const INTERFACE: &str = include_str!("interfaces/locals.rpli");

/// Get the runtime library (lazily initialized).
pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid locals interface"))
}

/// Locals library ID.
pub const LOCALS_LIB: LibId = 32;

/// Construct IDs for syntax.
pub mod constructs {
    /// Local variable binding (→ or ->)
    pub const LOCAL_BINDING: u16 = 0;
}

/// The locals library (implementation only).
#[derive(Clone, Copy)]
pub struct LocalsLib;

impl LibraryLowerer for LocalsLib {
    fn id(&self) -> LibId {
        LOCALS_LIB
    }

    fn lower_composite(
        &self,
        construct_id: u16,
        branches: &[Branch],
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        match construct_id {
            constructs::LOCAL_BINDING => {
                // DSL parser produces:
                // - branches[0..N-1] = bindings, each with [index, name]
                // - branches[N-1] = body
                if branches.is_empty() {
                    return Err(LowerError {
                        span: None,
                        message: "local binding requires at least a body".into(),
                    });
                }

                // Last branch is the body
                let body = &branches[branches.len() - 1];

                // All other branches are bindings
                let bindings = &branches[..branches.len() - 1];

                if bindings.is_empty() {
                    return Err(LowerError {
                        span: None,
                        message: "local binding requires at least one variable name".into(),
                    });
                }

                // Extract indices and names from bindings
                let mut indices: Vec<usize> = Vec::new();
                let mut names: Vec<String> = Vec::new();

                for binding in bindings {
                    // Each binding has [index, name]
                    if binding.len() >= 2 {
                        if let NodeKind::Atom(AtomKind::Integer(idx)) = &binding[0].kind {
                            indices.push(*idx as usize);
                        }
                        if let NodeKind::Atom(AtomKind::String(s)) = &binding[1].kind {
                            names.push(s.to_string());
                        }
                    }
                }

                // Build name→actual_index pairs for PushLocalScope
                let mut names_and_indices: Vec<(String, u32)> = Vec::new();
                for (i, &idx) in indices.iter().enumerate() {
                    let actual_idx = ctx.user_local(idx as u32);
                    if i < names.len() {
                        names_and_indices.push((names[i].clone(), actual_idx));
                    }
                }

                // Emit PushLocalScope if we have names
                if !names_and_indices.is_empty() {
                    ctx.output.emit_push_local_scope(&names_and_indices);
                }

                // Pop values from stack into locals (reverse order - TOS goes to last name)
                // For → a b c « ... » the stack has [... a_val b_val c_val]
                // c gets TOS (c_val), b gets NOS (b_val), a gets 3rd (a_val)
                for &idx in indices.iter().rev() {
                    let actual_idx = ctx.user_local(idx as u32);
                    ctx.emit_local_set(actual_idx);
                }

                // Lower the body
                ctx.lower_all(body)?;

                // Emit PopLocalScope if we pushed one
                if !names_and_indices.is_empty() {
                    ctx.output.emit_pop_local_scope();
                }

                Ok(())
            }
            _ => Err(LowerError {
                span: None,
                message: format!("unknown locals construct: {}", construct_id),
            }),
        }
    }
}

impl LibraryExecutor for LocalsLib {
    fn id(&self) -> LibId {
        LOCALS_LIB
    }

    fn execute(&self, _ctx: &mut ExecuteContext) -> ExecuteResult {
        // LocalsLib is compile-time only; all local binding is done during lowering.
        // No commands need runtime execution.
        Err("LocalsLib has no executable commands".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locals_lib_id() {
        assert_eq!(LOCALS_LIB, 32);
    }

    #[test]
    fn locals_lib_implements_traits() {
        assert_eq!(interface().id(), LOCALS_LIB);
        assert_eq!(interface().name(), "Locals");
    }

    #[test]
    fn claims_registered() {
        let claims = interface().token_claims();
        assert!(claims.iter().any(|c| c.token == "→"));
        assert!(claims.iter().any(|c| c.token == "->"));
    }
}
