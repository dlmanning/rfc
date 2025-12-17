//! Flow control library.
//!
//! Provides control flow constructs:
//! - IF/THEN/END and IF/THEN/ELSE/END - conditionals
//! - FOR/NEXT - counted loop with named variable
//! - START/NEXT - counted loop with anonymous counter
//! - DO/UNTIL - post-test loop
//! - WHILE/REPEAT - pre-test loop
//! - CASE - multi-way branch
//!
//! # RPL Syntax
//!
//! ```text
//! IF condition THEN body END
//! IF condition THEN true-body ELSE false-body END
//!
//! start end FOR var body NEXT
//! start end FOR var body step STEP
//!
//! start end START body NEXT
//! start end START body step STEP
//!
//! DO body UNTIL condition END
//!
//! WHILE condition REPEAT body END
//!
//! CASE cond1 THEN body1 END cond2 THEN body2 END [default] END
//! ```
//!
//! # Lowering
//!
//! The flow control constructs lower to WASM-style structured control flow:
//! - IF/THEN → `if ... end`
//! - IF/THEN/ELSE → `if ... else ... end`
//! - Loops → `block { loop { ... br_if ... } }`

use crate::core::{Pos, Span};

use crate::{
    ir::{AtomKind, Branch, LibId, Node, NodeKind},
    libs::{
        ClaimContext, CommandInfo, ExecuteContext, ExecuteResult, Library, LibraryExecutor,
        LibraryLowerer, LibraryParser, StackEffect, TokenClaim,
    },
    lower::{LowerContext, LowerError},
    parse::{ParseContext, ParseError},
    value::Value,
    vm::bytecode::Opcode,
};

/// Flow control library ID.
pub const FLOW_LIB: LibId = 9;

/// Extended construct IDs.
pub mod constructs {
    /// IF/THEN/END (two branches: condition, then-body)
    pub const IF_THEN: u16 = 1;
    /// IF/THEN/ELSE/END (three branches: condition, then-body, else-body)
    pub const IF_THEN_ELSE: u16 = 2;
    /// FOR/NEXT loop
    /// branches[0] = local index (as Integer), branches[1] = body
    pub const FOR_NEXT: u16 = 3;
    /// FOR/STEP loop (with custom step)
    /// branches[0] = local index, branches[1] = body, branches[2] = step expression
    pub const FOR_STEP: u16 = 4;
    /// START/NEXT loop
    /// branches[0] = body
    pub const START_NEXT: u16 = 5;
    /// START/STEP loop
    /// branches[0] = body, branches[1] = step expression
    pub const START_STEP: u16 = 6;
    /// DO/UNTIL loop
    /// branches[0] = body, branches[1] = condition
    pub const DO_UNTIL: u16 = 7;
    /// WHILE/REPEAT loop
    /// branches[0] = condition, branches[1] = body
    pub const WHILE_REPEAT: u16 = 8;
    /// CASE construct
    /// branches[0..n-1] = pairs of (condition, body), branches[n] = optional default
    pub const CASE: u16 = 9;
    /// FORUP/NEXT loop (ascending only, skips if start > end)
    pub const FORUP_NEXT: u16 = 10;
    /// FORUP/STEP loop
    pub const FORUP_STEP: u16 = 11;
    /// FORDN/NEXT loop (descending only, skips if start < end)
    pub const FORDN_NEXT: u16 = 12;
    /// FORDN/STEP loop
    pub const FORDN_STEP: u16 = 13;
    /// IFERR/THEN/END (two branches: body, error-handler)
    pub const IFERR_THEN: u16 = 14;
    /// IFERR/THEN/ELSE/END (three branches: body, error-handler, no-error-body)
    pub const IFERR_THEN_ELSE: u16 = 15;
}

/// Command IDs for error handling.
pub mod cmd {
    /// DOERR - throw an error (pops error number from stack).
    pub const DOERR: u16 = 0;
    /// ERRN - push last error number to stack.
    pub const ERRN: u16 = 1;
    /// ERRM - push last error message to stack.
    pub const ERRM: u16 = 2;
}

/// Static token claims for flow control.
static FLOW_CLAIMS: &[TokenClaim] = &[
    TokenClaim {
        token: "IF",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "FOR",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "START",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "DO",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "WHILE",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "CASE",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "FORUP",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "FORDN",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
    TokenClaim {
        token: "IFERR",
        priority: 100,
        context: ClaimContext::NotInfix,
        lib_id: FLOW_LIB,
    },
];

/// The flow control library.
#[derive(Clone, Copy)]
pub struct FlowLib;

impl Library for FlowLib {
    fn id(&self) -> LibId {
        FLOW_LIB
    }

    fn name(&self) -> &'static str {
        "flow"
    }

    fn commands(&self) -> Vec<CommandInfo> {
        vec![
            CommandInfo::with_effect("DOERR", FLOW_LIB, cmd::DOERR, 1, 0), // pops error code
            CommandInfo::with_effect("ERRN", FLOW_LIB, cmd::ERRN, 0, 1),   // pushes error number
            CommandInfo::with_effect("ERRM", FLOW_LIB, cmd::ERRM, 0, 1),   // pushes error message
        ]
    }
}

impl LibraryParser for FlowLib {
    fn claims(&self) -> &[TokenClaim] {
        FLOW_CLAIMS
    }

    fn parse(&self, token: &str, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        match token.to_uppercase().as_str() {
            "IF" => self.parse_if(ctx),
            "FOR" => self.parse_for_variant(ctx, false, false),
            "FORUP" => self.parse_for_variant(ctx, true, false),
            "FORDN" => self.parse_for_variant(ctx, false, true),
            "START" => self.parse_start(ctx),
            "DO" => self.parse_do(ctx),
            "WHILE" => self.parse_while(ctx),
            "CASE" => self.parse_case(ctx),
            "IFERR" => self.parse_iferr(ctx),
            _ => Err(ParseError {
                message: format!("unexpected flow control token: {}", token),
                span: Span::new(Pos::new(0), Pos::new(0)),
                expected: None,
                found: Some(token.to_string()),
            }),
        }
    }
}

impl FlowLib {
    /// Parse IF/THEN/[ELSE]/END structure.
    fn parse_if(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse condition (objects until THEN)
        let mut condition = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected THEN".into(),
                span: start_span,
                expected: Some("THEN".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("THEN") {
                ctx.advance();
                break;
            }

            condition.push(ctx.parse_one()?);
        }

        // Parse then-body (objects until ELSE or END)
        let mut then_body = Vec::new();
        let mut has_else = false;
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected ELSE or END".into(),
                span: start_span,
                expected: Some("ELSE or END".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("ELSE") {
                ctx.advance();
                has_else = true;
                break;
            }

            if token.text.eq_ignore_ascii_case("END") {
                ctx.advance();
                break;
            }

            then_body.push(ctx.parse_one()?);
        }

        // Parse else-body if present
        let else_body = if has_else {
            let mut body = Vec::new();
            loop {
                let token = ctx.peek().ok_or_else(|| ParseError {
                    message: "unexpected end of input, expected END".into(),
                    span: start_span,
                    expected: Some("END".into()),
                    found: Some("end of input".into()),
                })?;

                if token.text.eq_ignore_ascii_case("END") {
                    ctx.advance();
                    break;
                }

                body.push(ctx.parse_one()?);
            }
            Some(body)
        } else {
            None
        };

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        if let Some(else_body) = else_body {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::IF_THEN_ELSE,
                vec![condition, then_body, else_body],
                full_span,
            ))
        } else {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::IF_THEN,
                vec![condition, then_body],
                full_span,
            ))
        }
    }

    /// Parse FOR/FORUP/FORDN loop variants.
    ///
    /// Syntax: start end FOR var body NEXT
    ///         start end FOR var body step STEP
    ///
    /// is_forup: ascending only, skips if start > end
    /// is_fordn: descending only, skips if start < end
    ///
    /// Note: start and end are already on the stack when FOR is encountered.
    fn parse_for_variant(
        &self,
        ctx: &mut ParseContext,
        is_forup: bool,
        is_fordn: bool,
    ) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse variable name
        let var_token = ctx.advance().ok_or_else(|| ParseError {
            message: "expected variable name after FOR".into(),
            span: start_span,
            expected: Some("variable name".into()),
            found: Some("end of input".into()),
        })?;

        // Enter scope and declare the loop variable
        ctx.enter_scope();
        let var_name = ctx.interner.intern(&var_token.text);
        let var_index = ctx.declare_local(var_name);

        // Parse body until NEXT or STEP
        let mut body = Vec::new();
        let mut step_expr = None;

        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected NEXT or STEP".into(),
                span: start_span,
                expected: Some("NEXT or STEP".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("NEXT") {
                ctx.advance();
                break;
            }

            if token.text.eq_ignore_ascii_case("STEP") {
                ctx.advance();
                // The step value should be on the stack (last expression before STEP)
                // We need to pop it from body
                if body.is_empty() {
                    return Err(ParseError {
                        message: "STEP requires a step value".into(),
                        span: start_span,
                        expected: Some("step value".into()),
                        found: None,
                    });
                }
                step_expr = Some(body.pop().unwrap());
                break;
            }

            body.push(ctx.parse_one()?);
        }

        ctx.exit_scope();

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        // Store var_index as an Integer node
        let index_node = Node::integer(var_index as i64, start_span);

        // Determine construct type based on variant and whether step is present
        let construct = if is_forup {
            if step_expr.is_some() {
                constructs::FORUP_STEP
            } else {
                constructs::FORUP_NEXT
            }
        } else if is_fordn {
            if step_expr.is_some() {
                constructs::FORDN_STEP
            } else {
                constructs::FORDN_NEXT
            }
        } else if step_expr.is_some() {
            constructs::FOR_STEP
        } else {
            constructs::FOR_NEXT
        };

        if let Some(step) = step_expr {
            Ok(Node::extended(
                FLOW_LIB,
                construct,
                vec![vec![index_node], body, vec![step]],
                full_span,
            ))
        } else {
            Ok(Node::extended(
                FLOW_LIB,
                construct,
                vec![vec![index_node], body],
                full_span,
            ))
        }
    }

    /// Parse START/NEXT or START/STEP loop.
    ///
    /// Syntax: start end START body NEXT
    ///         start end START body step STEP
    fn parse_start(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse body until NEXT or STEP
        let mut body = Vec::new();
        let mut step_expr = None;

        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected NEXT or STEP".into(),
                span: start_span,
                expected: Some("NEXT or STEP".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("NEXT") {
                ctx.advance();
                break;
            }

            if token.text.eq_ignore_ascii_case("STEP") {
                ctx.advance();
                if body.is_empty() {
                    return Err(ParseError {
                        message: "STEP requires a step value".into(),
                        span: start_span,
                        expected: Some("step value".into()),
                        found: None,
                    });
                }
                step_expr = Some(body.pop().unwrap());
                break;
            }

            body.push(ctx.parse_one()?);
        }

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        if let Some(step) = step_expr {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::START_STEP,
                vec![body, vec![step]],
                full_span,
            ))
        } else {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::START_NEXT,
                vec![body],
                full_span,
            ))
        }
    }

    /// Parse DO/UNTIL loop.
    ///
    /// Syntax: DO body UNTIL condition END
    fn parse_do(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse body until UNTIL
        let mut body = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected UNTIL".into(),
                span: start_span,
                expected: Some("UNTIL".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("UNTIL") {
                ctx.advance();
                break;
            }

            body.push(ctx.parse_one()?);
        }

        // Parse condition until END
        let mut condition = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected END".into(),
                span: start_span,
                expected: Some("END".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("END") {
                ctx.advance();
                break;
            }

            condition.push(ctx.parse_one()?);
        }

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        Ok(Node::extended(
            FLOW_LIB,
            constructs::DO_UNTIL,
            vec![body, condition],
            full_span,
        ))
    }

    /// Parse WHILE/REPEAT loop.
    ///
    /// Syntax: WHILE condition REPEAT body END
    fn parse_while(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse condition until REPEAT
        let mut condition = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected REPEAT".into(),
                span: start_span,
                expected: Some("REPEAT".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("REPEAT") {
                ctx.advance();
                break;
            }

            condition.push(ctx.parse_one()?);
        }

        // Parse body until END
        let mut body = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected END".into(),
                span: start_span,
                expected: Some("END".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("END") {
                ctx.advance();
                break;
            }

            body.push(ctx.parse_one()?);
        }

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        Ok(Node::extended(
            FLOW_LIB,
            constructs::WHILE_REPEAT,
            vec![condition, body],
            full_span,
        ))
    }

    /// Parse CASE construct.
    ///
    /// Syntax: CASE cond1 THEN body1 END cond2 THEN body2 END [default] END
    fn parse_case(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Collect branches: each branch is (condition, body)
        // The last branch might be a default (no condition)
        let mut branches: Vec<Vec<Node>> = Vec::new();

        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input in CASE".into(),
                span: start_span,
                expected: Some("CASE branch or END".into()),
                found: Some("end of input".into()),
            })?;

            // Check for outer END (end of CASE)
            if token.text.eq_ignore_ascii_case("END") {
                ctx.advance();
                break;
            }

            // Parse condition until THEN or END
            let mut condition = Vec::new();
            let mut found_then = false;

            loop {
                let token = ctx.peek().ok_or_else(|| ParseError {
                    message: "unexpected end of input in CASE branch".into(),
                    span: start_span,
                    expected: Some("THEN or END".into()),
                    found: Some("end of input".into()),
                })?;

                if token.text.eq_ignore_ascii_case("THEN") {
                    ctx.advance();
                    found_then = true;
                    break;
                }

                if token.text.eq_ignore_ascii_case("END") {
                    // This is default case or end of CASE
                    break;
                }

                condition.push(ctx.parse_one()?);
            }

            if found_then {
                // Parse body until END
                let mut body = Vec::new();
                loop {
                    let token = ctx.peek().ok_or_else(|| ParseError {
                        message: "unexpected end of input, expected END".into(),
                        span: start_span,
                        expected: Some("END".into()),
                        found: Some("end of input".into()),
                    })?;

                    if token.text.eq_ignore_ascii_case("END") {
                        ctx.advance();
                        break;
                    }

                    body.push(ctx.parse_one()?);
                }

                // Add condition and body as separate branches
                branches.push(condition);
                branches.push(body);
            } else {
                // Default case - condition is actually the default body
                // Push empty condition marker and the body
                branches.push(vec![]); // Empty condition = default
                branches.push(condition); // What we parsed is actually the default body
            }
        }

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        Ok(Node::extended(
            FLOW_LIB,
            constructs::CASE,
            branches,
            full_span,
        ))
    }

    /// Parse IFERR/THEN/[ELSE]/END structure.
    ///
    /// Syntax: IFERR body THEN error-handler END
    ///         IFERR body THEN error-handler ELSE no-error-body END
    ///
    /// Also accepts HP-style aliases: THENERR, ELSEERR, ENDERR
    fn parse_iferr(&self, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let start_span = ctx.current_span();

        // Parse body (objects until THEN or THENERR)
        let mut body = Vec::new();
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected THEN".into(),
                span: start_span,
                expected: Some("THEN".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("THEN")
                || token.text.eq_ignore_ascii_case("THENERR")
            {
                ctx.advance();
                break;
            }

            body.push(ctx.parse_one()?);
        }

        // Parse error-handler (objects until ELSE/ELSEERR or END/ENDERR)
        let mut error_handler = Vec::new();
        let mut has_else = false;
        loop {
            let token = ctx.peek().ok_or_else(|| ParseError {
                message: "unexpected end of input, expected ELSE or END".into(),
                span: start_span,
                expected: Some("ELSE or END".into()),
                found: Some("end of input".into()),
            })?;

            if token.text.eq_ignore_ascii_case("ELSE")
                || token.text.eq_ignore_ascii_case("ELSEERR")
            {
                ctx.advance();
                has_else = true;
                break;
            }

            if token.text.eq_ignore_ascii_case("END")
                || token.text.eq_ignore_ascii_case("ENDERR")
            {
                ctx.advance();
                break;
            }

            error_handler.push(ctx.parse_one()?);
        }

        // Parse no-error-body if present
        let no_error_body = if has_else {
            let mut no_error = Vec::new();
            loop {
                let token = ctx.peek().ok_or_else(|| ParseError {
                    message: "unexpected end of input, expected END".into(),
                    span: start_span,
                    expected: Some("END".into()),
                    found: Some("end of input".into()),
                })?;

                if token.text.eq_ignore_ascii_case("END")
                    || token.text.eq_ignore_ascii_case("ENDERR")
                {
                    ctx.advance();
                    break;
                }

                no_error.push(ctx.parse_one()?);
            }
            Some(no_error)
        } else {
            None
        };

        let end_span = ctx.current_span();
        let full_span = Span::new(start_span.start(), end_span.end());

        if let Some(no_error) = no_error_body {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::IFERR_THEN_ELSE,
                vec![body, error_handler, no_error],
                full_span,
            ))
        } else {
            Ok(Node::extended(
                FLOW_LIB,
                constructs::IFERR_THEN,
                vec![body, error_handler],
                full_span,
            ))
        }
    }
}

impl LibraryLowerer for FlowLib {
    fn lower_composite(
        &self,
        id: u16,
        branches: &[Branch],
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        match id {
            constructs::IF_THEN => {
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "IF/THEN requires condition and body".into(),
                    });
                }
                // Lower condition and pop it (consumed by `if`)
                ctx.lower_all(&branches[0])?;
                ctx.pop_type(); // condition consumed

                // Save state before branch (in case body doesn't execute)
                let pre_branch = ctx.types.clone();

                ctx.output.emit_if();
                ctx.lower_all(&branches[1])?;
                ctx.output.emit_end();

                // Join with pre-branch state (body may or may not have executed)
                let then_state = ctx.types.clone();
                let joined = pre_branch.join(&then_state);
                ctx.types.replace_with(joined);
                Ok(())
            }

            constructs::IF_THEN_ELSE => {
                if branches.len() < 3 {
                    return Err(LowerError { span: None,
                        message: "IF/THEN/ELSE requires condition and two bodies".into(),
                    });
                }
                // Lower condition and pop it (consumed by `if`)
                ctx.lower_all(&branches[0])?;
                ctx.pop_type(); // condition consumed

                // Save state before branches
                let pre_branch = ctx.types.clone();

                ctx.output.emit_if();
                ctx.lower_all(&branches[1])?;

                // Save then-branch state, restore pre-branch for else
                let then_state = ctx.types.clone();
                ctx.types.replace_with(pre_branch);

                ctx.output.emit_else();
                ctx.lower_all(&branches[2])?;

                // Join then and else states
                let else_state = ctx.types.clone();
                let joined = then_state.join(&else_state);
                ctx.types.replace_with(joined);

                ctx.output.emit_end();
                Ok(())
            }

            constructs::FOR_NEXT => {
                // branches[0] = [index_node], branches[1] = body
                // Stack has: start end
                // Loop from start to end, step 1
                self.lower_for_loop(branches, None, ctx)
            }

            constructs::FOR_STEP => {
                // branches[0] = [index_node], branches[1] = body, branches[2] = [step_node]
                if branches.len() < 3 {
                    return Err(LowerError { span: None,
                        message: "FOR/STEP requires index, body, and step".into(),
                    });
                }
                self.lower_for_loop(&branches[..2], Some(&branches[2]), ctx)
            }

            constructs::START_NEXT => {
                // branches[0] = body
                // Stack has: start end
                self.lower_start_loop(branches, None, ctx)
            }

            constructs::START_STEP => {
                // branches[0] = body, branches[1] = [step_node]
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "START/STEP requires body and step".into(),
                    });
                }
                self.lower_start_loop(&branches[..1], Some(&branches[1]), ctx)
            }

            constructs::DO_UNTIL => {
                // branches[0] = body, branches[1] = condition
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "DO/UNTIL requires body and condition".into(),
                    });
                }

                // Save state before loop (body executes at least once, but may repeat)
                let pre_loop = ctx.types.clone();

                // loop {
                //   body
                //   condition
                //   i64.eqz  (invert: continue if condition is false/0)
                //   br_if 0  (back to loop start if condition was false)
                // }
                ctx.output.emit_loop();
                ctx.lower_all(&branches[0])?; // body
                ctx.lower_all(&branches[1])?; // condition
                ctx.output.emit_i64_eqz(); // invert
                ctx.pop_type(); // condition consumed by br_if
                ctx.output.emit_br_if(0); // continue loop if false
                ctx.output.emit_end();

                // Join with pre-loop state (loop might execute multiple times)
                let post_loop = ctx.types.clone();
                let joined = pre_loop.join(&post_loop);
                ctx.types.replace_with(joined);
                Ok(())
            }

            constructs::WHILE_REPEAT => {
                // branches[0] = condition, branches[1] = body
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "WHILE/REPEAT requires condition and body".into(),
                    });
                }

                // Save state before loop (body might execute 0+ times)
                let pre_loop = ctx.types.clone();

                // block {
                //   loop {
                //     condition
                //     i64.eqz  (invert)
                //     br_if 1  (exit block if false)
                //     body
                //     br 0     (back to loop)
                //   }
                // }
                ctx.output.emit_block();
                ctx.output.emit_loop();
                ctx.lower_all(&branches[0])?; // condition
                ctx.output.emit_i64_eqz(); // invert: exit if zero
                ctx.pop_type(); // condition consumed by br_if
                ctx.output.emit_br_if(1); // exit outer block
                ctx.lower_all(&branches[1])?; // body
                ctx.output.emit_br(0); // continue loop
                ctx.output.emit_end(); // end loop
                ctx.output.emit_end(); // end block

                // Join with pre-loop state (body might execute 0+ times)
                let post_loop = ctx.types.clone();
                let joined = pre_loop.join(&post_loop);
                ctx.types.replace_with(joined);
                Ok(())
            }

            constructs::CASE => {
                // branches are pairs: [cond1, body1, cond2, body2, ..., default_cond, default_body]
                // An empty condition indicates default
                self.lower_case(branches, ctx)
            }

            constructs::FORUP_NEXT => {
                // FORUP: ascending only, skip if start > end
                self.lower_for_loop_directional(branches, None, true, ctx)
            }

            constructs::FORUP_STEP => {
                if branches.len() < 3 {
                    return Err(LowerError { span: None,
                        message: "FORUP/STEP requires index, body, and step".into(),
                    });
                }
                self.lower_for_loop_directional(&branches[..2], Some(&branches[2]), true, ctx)
            }

            constructs::FORDN_NEXT => {
                // FORDN: descending only, skip if start < end
                self.lower_for_loop_directional(branches, None, false, ctx)
            }

            constructs::FORDN_STEP => {
                if branches.len() < 3 {
                    return Err(LowerError { span: None,
                        message: "FORDN/STEP requires index, body, and step".into(),
                    });
                }
                self.lower_for_loop_directional(&branches[..2], Some(&branches[2]), false, ctx)
            }

            constructs::IFERR_THEN => {
                // branches[0] = body, branches[1] = error-handler
                if branches.len() < 2 {
                    return Err(LowerError { span: None,
                        message: "IFERR/THEN requires body and error handler".into(),
                    });
                }

                // Structure (using catch_all for simple exception handling):
                // block $outer
                //   block $handler_target
                //     try_table (catch_all 1)
                //       body
                //     end
                //     br 1   ; success: skip handler, jump to after $outer
                //   end
                //   ; catch_all branches here (nothing on stack)
                //   error-handler
                // end
                //
                // ERRN/ERRM access the error via VM's last_error field (set by catch handler).

                // Save state before try body
                let pre_try = ctx.types.clone();

                ctx.output.emit_block(); // $outer
                ctx.output.emit_block(); // $handler_target
                ctx.output.emit_try_table_catch_all(1); // catch_all branches to depth 1
                ctx.lower_all(&branches[0])?; // body
                ctx.output.emit_end(); // end try_table

                // Save success state
                let success_state = ctx.types.clone();

                ctx.output.emit_br(1); // success: skip to after $outer
                ctx.output.emit_end(); // end $handler_target

                // For error handler: restore pre-try state but mark unknown depth
                // (exception could occur at any point, so stack state is unpredictable)
                ctx.types.replace_with(pre_try);
                ctx.types.mark_unknown_depth();

                ctx.lower_all(&branches[1])?; // error handler

                // Join success state with error handler state
                let error_state = ctx.types.clone();
                let joined = success_state.join(&error_state);
                ctx.types.replace_with(joined);

                ctx.output.emit_end(); // end $outer
                Ok(())
            }

            constructs::IFERR_THEN_ELSE => {
                // branches[0] = body, branches[1] = error-handler, branches[2] = no-error-body
                if branches.len() < 3 {
                    return Err(LowerError { span: None,
                        message: "IFERR/THEN/ELSE requires body, error handler, and no-error body"
                            .into(),
                    });
                }

                // Structure (using catch_all for simple exception handling):
                // block $outer
                //   block $handler_target
                //     try_table (catch_all 1)
                //       body
                //     end
                //     no-error-body
                //     br 1   ; skip handler
                //   end
                //   ; catch_all branches here (nothing on stack)
                //   error-handler
                // end
                //
                // ERRN/ERRM access the error via VM's last_error field (set by catch handler).

                // Save state before try body
                let pre_try = ctx.types.clone();

                ctx.output.emit_block(); // $outer
                ctx.output.emit_block(); // $handler_target
                ctx.output.emit_try_table_catch_all(1);
                ctx.lower_all(&branches[0])?; // body
                ctx.output.emit_end(); // end try_table
                ctx.lower_all(&branches[2])?; // no-error-body (runs on success)

                // Save success state (after try body + no-error-body)
                let success_state = ctx.types.clone();

                ctx.output.emit_br(1); // skip handler
                ctx.output.emit_end(); // end $handler_target

                // For error handler: restore pre-try state but mark unknown depth
                // (exception could occur at any point, so stack state is unpredictable)
                ctx.types.replace_with(pre_try);
                ctx.types.mark_unknown_depth();

                ctx.lower_all(&branches[1])?; // error handler

                // Join success state with error handler state
                let error_state = ctx.types.clone();
                let joined = success_state.join(&error_state);
                ctx.types.replace_with(joined);

                ctx.output.emit_end(); // end $outer
                Ok(())
            }

            _ => Err(LowerError { span: None,
                message: format!("unknown flow control construct: {}", id),
            }),
        }
    }

    fn lower_command(
        &self,
        cmd_id: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<StackEffect, LowerError> {
        match cmd_id {
            cmd::DOERR => {
                // DOERR: throw an error (pops error code from stack)
                // Emit Throw opcode with tag 0 (default RPL error tag)
                ctx.output.emit_throw(0);
                Ok(StackEffect::fixed(1, &[]))
            }
            cmd::ERRN | cmd::ERRM => {
                // ERRN/ERRM: need runtime execution to access VM state
                ctx.output.emit_call_lib(FLOW_LIB, cmd_id);
                Ok(StackEffect::fixed(0, &[None]))
            }
            _ => Err(LowerError { span: None,
                message: format!("unknown flow command: {}", cmd_id),
            }),
        }
    }
}

impl LibraryExecutor for FlowLib {
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::ERRN => {
                // Push last error number (or 0 if no error)
                let error_code = ctx.last_error().map(|e| e.code).unwrap_or(0);
                ctx.push(Value::Integer(error_code))?;
                Ok(())
            }
            cmd::ERRM => {
                // Push last error message (or empty string if no error)
                let error_msg = ctx
                    .last_error()
                    .map(|e| e.message.clone())
                    .unwrap_or_default();
                ctx.push(Value::string(error_msg))?;
                Ok(())
            }
            cmd::DOERR => {
                // DOERR should be handled by bytecode, not runtime execution
                // This shouldn't be called, but handle gracefully
                Err("DOERR should be handled by bytecode".into())
            }
            _ => Err(format!("unknown flow command: {}", ctx.cmd)),
        }
    }
}

impl FlowLib {
    /// Lower a FOR loop.
    fn lower_for_loop(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.len() < 2 {
            return Err(LowerError { span: None,
                message: "FOR loop requires index and body".into(),
            });
        }

        // Get the local index from branches[0]
        // NOTE: User locals are offset by SCRATCH_LOCAL_COUNT to avoid conflict with scratch
        let var_index = match branches[0].first() {
            Some(Node {
                kind: NodeKind::Atom(AtomKind::Integer(idx)),
                ..
            }) => ctx.user_local(*idx as u32),
            _ => {
                return Err(LowerError { span: None,
                    message: "FOR loop missing variable index".into(),
                });
            }
        };

        // Allocate locals for end and step
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end (both integers)
        // Store end
        ctx.emit_local_set(end_local);
        // Store start as loop variable
        ctx.emit_local_set(var_index);
        // Store step (default 1)
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop body (loop may execute 0+ times)
        let pre_loop = ctx.types.clone();

        // block {
        //   loop {
        //     body
        //     // increment: var = var + step
        //     local.get var
        //     local.get step
        //     i64.add
        //     local.set var
        //     // check: if step > 0, continue while var <= end
        //     //        if step < 0, continue while var >= end
        //     local.get var
        //     local.get end
        //     i64.le_s
        //     local.get var
        //     local.get end
        //     i64.ge_s
        //     local.get step
        //     i64.const 0
        //     i64.gt_s
        //     select
        //     br_if 0  (continue if true)
        //   }
        // }
        ctx.output.emit_block();
        ctx.output.emit_loop();

        // Execute body
        ctx.lower_all(&branches[1])?;

        // Increment: var = var + step
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(var_index);

        // Check continuation condition
        // For positive step: continue while var <= end
        // For negative step: continue while var >= end
        // We compute both and select based on step sign:
        //   (step > 0) ? (var <= end) : (var >= end)

        // Compute var <= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute var >= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (var <= end) else (var >= end)
        // Select pops condition, false_val, true_val and pushes result
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed
        // le_result stays as the selected value type (already on type stack)

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // continue loop

        ctx.output.emit_end(); // end loop
        ctx.output.emit_end(); // end block

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

        Ok(())
    }

    /// Lower a directional FOR loop (FORUP or FORDN).
    ///
    /// ascending=true: FORUP - skip if start > end, use step +1 by default
    /// ascending=false: FORDN - skip if start < end, use step -1 by default
    fn lower_for_loop_directional(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ascending: bool,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.len() < 2 {
            return Err(LowerError { span: None,
                message: "FOR loop requires index and body".into(),
            });
        }

        // Get the local index from branches[0]
        // NOTE: User locals are offset by SCRATCH_LOCAL_COUNT to avoid conflict with scratch
        let var_index = match branches[0].first() {
            Some(Node {
                kind: NodeKind::Atom(AtomKind::Integer(idx)),
                ..
            }) => ctx.user_local(*idx as u32),
            _ => {
                return Err(LowerError { span: None,
                    message: "FOR loop missing variable index".into(),
                });
            }
        };

        // Allocate locals for start, end, step
        let start_local = ctx.alloc_local();
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end
        // Store end first (it's on top)
        ctx.emit_local_set(end_local);
        // Store start to both start_local and var_index
        ctx.output.emit_local_tee(start_local);
        ctx.emit_local_set(var_index);

        // Store step
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            // NEXT always uses step +1, regardless of loop direction
            // The direction check (skip if wrong bounds) is separate
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop (loop may execute 0+ times)
        let pre_loop = ctx.types.clone();

        // Check if we should skip the loop entirely
        // FORUP: skip if start > end
        // FORDN: skip if start < end
        // Wrap the loop in a block so we can br_if to skip it
        ctx.output.emit_block(); // outer block for skip

        // Compute skip condition
        ctx.emit_local_get_int(start_local);
        ctx.emit_local_get_int(end_local);
        if ascending {
            // FORUP: skip if start > end
            ctx.emit_i64_binop(Opcode::I64GtS);
        } else {
            // FORDN: skip if start < end
            ctx.emit_i64_binop(Opcode::I64LtS);
        }
        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // skip to end of outer block if true

        // The loop itself
        ctx.output.emit_block();
        ctx.output.emit_loop();

        // Execute body
        ctx.lower_all(&branches[1])?;

        // Increment: var = var + step
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(var_index);

        // Check continuation condition (same as regular FOR with step direction handling)
        // Compute var <= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute var >= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (var <= end) else (var >= end)
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // continue loop

        ctx.output.emit_end(); // end loop
        ctx.output.emit_end(); // end inner block
        ctx.output.emit_end(); // end outer block (skip target)

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

        Ok(())
    }

    /// Lower a START loop.
    fn lower_start_loop(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.is_empty() {
            return Err(LowerError { span: None,
                message: "START loop requires body".into(),
            });
        }

        // Allocate locals for counter, end, step
        let counter_local = ctx.alloc_local();
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end
        ctx.emit_local_set(end_local);
        ctx.emit_local_set(counter_local);

        // Store step
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop (loop may execute 0+ times)
        let pre_loop = ctx.types.clone();

        // Loop structure same as FOR
        ctx.output.emit_block();
        ctx.output.emit_loop();

        // Execute body
        ctx.lower_all(&branches[0])?;

        // Increment counter
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(counter_local);

        // Check continuation condition
        // For positive step: continue while counter <= end
        // For negative step: continue while counter >= end

        // Compute counter <= end
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute counter >= end
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (counter <= end) else (counter >= end)
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0);

        ctx.output.emit_end();
        ctx.output.emit_end();

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

        Ok(())
    }

    /// Lower a CASE construct.
    fn lower_case(&self, branches: &[Branch], ctx: &mut LowerContext) -> Result<(), LowerError> {
        // branches are pairs: [cond1, body1, cond2, body2, ...]
        // Empty condition = default case

        if branches.len() < 2 {
            return Ok(()); // Empty CASE does nothing
        }

        // Save pre-case state for joining
        let pre_case = ctx.types.clone();

        // Convert to nested if/else with proper type state tracking
        let mut i = 0;
        let mut depth = 0;
        let mut branch_states: Vec<crate::types::CStack> = Vec::new();

        while i < branches.len() {
            let condition = &branches[i];
            let body = if i + 1 < branches.len() {
                &branches[i + 1]
            } else {
                break;
            };

            if condition.is_empty() {
                // Default case - just execute the body
                ctx.lower_all(body)?;
                branch_states.push(ctx.types.clone());
                i += 2;
            } else {
                // Save state before this branch
                let pre_branch = ctx.types.clone();

                // Conditional case
                ctx.lower_all(condition)?;
                ctx.pop_type(); // condition consumed by if
                ctx.output.emit_if();
                ctx.lower_all(body)?;

                // Save this branch's state
                branch_states.push(ctx.types.clone());

                depth += 1;
                i += 2;

                // If there are more branches, emit else and restore pre-branch state
                if i < branches.len() {
                    ctx.output.emit_else();
                    ctx.types.replace_with(pre_branch);
                }
            }
        }

        // Close all the if blocks
        for _ in 0..depth {
            ctx.output.emit_end();
        }

        // Join all branch states
        if !branch_states.is_empty() {
            let mut joined = branch_states[0].clone();
            for state in &branch_states[1..] {
                joined = joined.join(state);
            }
            // Also join with pre-case state (if no default case, might not execute any branch)
            let has_default = branches.iter().step_by(2).any(|c| c.is_empty());
            if !has_default {
                joined = joined.join(&pre_case);
            }
            ctx.types.replace_with(joined);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flow_lib_id() {
        assert_eq!(FLOW_LIB, 9);
    }

    #[test]
    fn construct_ids() {
        assert_eq!(constructs::IF_THEN, 1);
        assert_eq!(constructs::IF_THEN_ELSE, 2);
        assert_eq!(constructs::FOR_NEXT, 3);
        assert_eq!(constructs::START_NEXT, 5);
        assert_eq!(constructs::DO_UNTIL, 7);
        assert_eq!(constructs::WHILE_REPEAT, 8);
        assert_eq!(constructs::CASE, 9);
    }

    #[test]
    fn flow_lib_implements_traits() {
        let lib = FlowLib;
        assert_eq!(lib.id(), FLOW_LIB);
        assert_eq!(lib.name(), "flow");
    }

    #[test]
    fn claims_include_all_keywords() {
        let lib = FlowLib;
        let claims = lib.claims();
        let tokens: Vec<_> = claims.iter().map(|c| c.token).collect();
        assert!(tokens.contains(&"IF"));
        assert!(tokens.contains(&"FOR"));
        assert!(tokens.contains(&"START"));
        assert!(tokens.contains(&"DO"));
        assert!(tokens.contains(&"WHILE"));
        assert!(tokens.contains(&"CASE"));
        assert!(tokens.contains(&"IFERR"));
    }

    #[test]
    fn iferr_construct_ids() {
        assert_eq!(constructs::IFERR_THEN, 14);
        assert_eq!(constructs::IFERR_THEN_ELSE, 15);
    }
}
