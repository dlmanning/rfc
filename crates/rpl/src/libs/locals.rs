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

use crate::core::{Pos, Span};

use crate::{
    ir::{Branch, LibId, Node},
    libs::{ClaimContext, Library, TokenClaim},
    lower::{LowerContext, LowerError},
    parse::{ParseContext, ParseError},
};

/// Locals library ID.
pub const LOCALS_LIB: LibId = 32;

/// Construct IDs for locals library.
pub mod constructs {
    /// Local binding: → x y « body »
    /// branches[0] = local indices as Integer nodes
    /// branches[1] = body
    pub const LOCAL_BINDING: u16 = 1;
}

/// Static token claims for locals.
static LOCALS_CLAIMS: &[TokenClaim] = &[
    TokenClaim {
        token: "→",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: LOCALS_LIB,
    },
    TokenClaim {
        token: "->",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: LOCALS_LIB,
    },
];

/// The locals library.
#[derive(Clone, Copy)]
pub struct LocalsLib;

impl Library for LocalsLib {
    fn id(&self) -> LibId {
        LOCALS_LIB
    }

    fn name(&self) -> &'static str {
        "locals"
    }

    fn claims(&self) -> &[TokenClaim] {
        LOCALS_CLAIMS
    }

    fn parse(&self, token: &str, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        match token {
            "→" | "->" => self.parse_local_binding(ctx),
            _ => Err(ParseError {
                message: format!("unexpected locals token: {}", token),
                span: Span::new(Pos::new(0), Pos::new(0)),
                expected: None,
                found: Some(token.to_string()),
            }),
        }
    }

    fn lower_composite(
        &self,
        id: u16,
        branches: &[Branch],
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        match id {
            constructs::LOCAL_BINDING => {
                // branches[0] = local indices, branches[1] = body, branches[2] = local names
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "local binding requires indices and body".into(),
                    });
                }

                // Get local indices from branches[0]
                let indices: Vec<usize> = branches[0]
                    .iter()
                    .filter_map(|node| {
                        if let crate::ir::NodeKind::Atom(crate::ir::AtomKind::Integer(n)) =
                            &node.kind
                        {
                            Some(*n as usize)
                        } else {
                            None
                        }
                    })
                    .collect();

                // Get local names from branches[2] if present
                let names: Vec<String> = if branches.len() > 2 {
                    branches[2]
                        .iter()
                        .filter_map(|node| {
                            if let crate::ir::NodeKind::Atom(crate::ir::AtomKind::String(s)) =
                                &node.kind
                            {
                                Some(s.to_string())
                            } else {
                                None
                            }
                        })
                        .collect()
                } else {
                    Vec::new()
                };

                // Build name→actual_index pairs for PushLocalScope
                // We need to map parsed indices to actual indices
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
                // For → a b c :: ... the stack has [... a_val b_val c_val]
                // c gets TOS (c_val), b gets NOS (b_val), a gets 3rd (a_val)
                // NOTE: We already computed actual indices above, reuse them
                for &idx in indices.iter().rev() {
                    let actual_idx = ctx.user_local(idx as u32);
                    ctx.emit_local_set(actual_idx);
                }

                // Lower the body
                ctx.lower_all(&branches[1])?;

                // Emit PopLocalScope if we pushed one
                if !names_and_indices.is_empty() {
                    ctx.output.emit_pop_local_scope();
                }

                Ok(())
            }
            _ => Err(LowerError { span: None,
                message: format!("unknown locals construct: {}", id),
            }),
        }
    }
}

impl LocalsLib {
    /// Parse local binding: → x y z « body »
    ///
    /// Syntax: → <names...> « <body> »  (HP calculator syntax)
    /// Or:     -> <names...> << <body> >>  (ASCII equivalent)
    ///
    /// The names are bound to local variables. Values are popped from
    /// the stack to initialize them (rightmost name = TOS).
    fn parse_local_binding(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Enter a new scope for the locals
        ctx.enter_scope();

        // Parse variable names until « or <<
        let mut local_indices = Vec::new();
        let mut local_names: Vec<(String, Span)> = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected « or variable name".into(),
                span: start_span,
                expected: Some("« or variable name".into()),
                found: Some("end of input".into()),
            })?;

            // Check for « or << (body opener)
            if token.text == "«" || token.text == "<<" {
                ctx.advance(); // consume opener
                break;
            }

            // Must be a variable name - declare it
            let name_token = ctx.advance().unwrap();
            let name_str = name_token.text.clone();
            let name_span = name_token.span;
            let name = ctx.interner.intern(&name_str);
            let index = ctx.declare_local(name);
            local_indices.push(index);
            local_names.push((name_str, name_span));
        }

        if local_indices.is_empty() {
            return Err(ParseError {
                message: "local binding requires at least one variable name".into(),
                span: start_span,
                expected: Some("at least one variable name".into()),
                found: None,
            });
        }

        // Parse body until » or >>
        let mut body = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected » or >>".into(),
                span: start_span,
                expected: Some("» or >>".into()),
                found: Some("end of input".into()),
            })?;

            // Check for » or >> (body closer)
            if token.text == "»" || token.text == ">>" {
                ctx.advance(); // consume closer
                break;
            }

            body.push(ctx.parse_one()?);
        }

        // Exit the scope
        ctx.exit_scope();

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        // Create branches:
        // branches[0] = local indices as Integer nodes
        // branches[1] = body
        // branches[2] = local names as String nodes (with correct spans)
        let index_nodes: Vec<Node> = local_indices
            .iter()
            .map(|&idx| Node::integer(idx as i64, start_span))
            .collect();
        let name_nodes: Vec<Node> = local_names
            .iter()
            .map(|(name, span)| Node::string(name.clone(), *span))
            .collect();

        Ok(Node::extended(
            LOCALS_LIB,
            constructs::LOCAL_BINDING,
            vec![index_nodes, body, name_nodes],
            full_span,
        ))
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
    fn construct_ids() {
        assert_eq!(constructs::LOCAL_BINDING, 1);
    }

    #[test]
    fn locals_lib_implements_traits() {
        let lib = LocalsLib;
        assert_eq!(lib.id(), LOCALS_LIB);
        assert_eq!(lib.name(), "locals");
    }

    #[test]
    fn claims_registered() {
        let lib = LocalsLib;
        let claims = lib.claims();
        assert!(claims.iter().any(|c| c.token == "→"));
        assert!(claims.iter().any(|c| c.token == "->"));
    }
}
