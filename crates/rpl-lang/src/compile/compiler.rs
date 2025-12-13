// Allow large error types - Diagnostic is intentionally large for rich error info
#![allow(clippy::result_large_err)]

use std::collections::HashMap;

use rpl_core::{Interner, Span, Symbol, TypeId, Word, make_call, Diagnostic, TokenType};
use rpl_source::SourceFile;
use crate::{
    analysis::{AnalysisResult, ResolvedToken},
    compile::{
        constructs::{Construct, ConstructStack},
        infix::{InfixParser, OperatorEntry},
        output::OutputBuffer,
        types::{CStack, CType},
    },
    library::{
        CompileContext, CompileResult, ConstructKind, LibraryId, LibraryRegistry, StackEffect,
    },
    operator::{OperatorKind, OperatorRegistry},
};

/// A compiled program ready for execution.
pub struct CompiledProgram {
    /// The bytecode.
    pub code: Vec<Word>,
    /// Source spans for each word (for error reporting).
    pub spans: Vec<Span>,
    /// Type information for each position (for debugging/analysis).
    pub type_map: HashMap<usize, CType>,
    /// The interner used during compilation.
    pub interner: Interner,
}

impl CompiledProgram {
    /// Get the length of the bytecode.
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if the program is empty.
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }
}

/// Compiler that transforms analyzed tokens into bytecode.
pub struct Compiler<'a> {
    registry: &'a LibraryRegistry,
    operators: &'a mut OperatorRegistry,
    analysis: &'a AnalysisResult,
    source: &'a SourceFile,
    interner: Interner,
    output: OutputBuffer,
    constructs: ConstructStack,
    stack: CStack,
    token_index: usize,
    errors: Vec<Diagnostic>,
    /// Infix parser for symbolic expressions (active when in infix mode).
    infix_parser: Option<InfixParser>,
    /// Position where symbolic prolog was emitted (for patching size).
    infix_start_pos: Option<usize>,
    /// Collected parameter names after `→` (for LocalBinding constructs).
    local_binding_params: Vec<Symbol>,
    /// True when in parameter collection mode (between `→` and `::`).
    collecting_local_params: bool,
    /// True when collecting FOR loop variable (after FOR until variable is collected).
    collecting_for_variable: bool,
    /// User library registry for compile-time command resolution.
    user_lib_registry: Option<&'a crate::user_libs::UserLibraryRegistry>,
}

impl<'a> Compiler<'a> {
    /// Create a new compiler.
    pub fn new(
        registry: &'a LibraryRegistry,
        operators: &'a mut OperatorRegistry,
        analysis: &'a AnalysisResult,
        source: &'a SourceFile,
        interner: Interner,
    ) -> Self {
        Self {
            registry,
            operators,
            analysis,
            source,
            interner,
            output: OutputBuffer::new(),
            constructs: ConstructStack::new(),
            stack: CStack::new(),
            token_index: 0,
            errors: Vec::new(),
            infix_parser: None,
            infix_start_pos: None,
            local_binding_params: Vec::new(),
            collecting_local_params: false,
            collecting_for_variable: false,
            user_lib_registry: None,
        }
    }

    /// Create a compiler with access to the user library registry.
    pub fn with_user_lib_registry(
        registry: &'a LibraryRegistry,
        operators: &'a mut OperatorRegistry,
        analysis: &'a AnalysisResult,
        source: &'a SourceFile,
        interner: Interner,
        user_lib_registry: &'a crate::user_libs::UserLibraryRegistry,
    ) -> Self {
        Self {
            registry,
            operators,
            analysis,
            source,
            interner,
            output: OutputBuffer::new(),
            constructs: ConstructStack::new(),
            stack: CStack::new(),
            token_index: 0,
            errors: Vec::new(),
            infix_parser: None,
            infix_start_pos: None,
            local_binding_params: Vec::new(),
            collecting_local_params: false,
            collecting_for_variable: false,
            user_lib_registry: Some(user_lib_registry),
        }
    }

    /// Check if we're currently in infix mode.
    fn in_infix(&self) -> bool {
        self.infix_parser.is_some()
    }

    /// Find the base offset for jump targets.
    ///
    /// When inside an object construct (Program, List, etc.), jump targets must be
    /// relative to the object's body start, not absolute positions in the bytecode.
    /// This is because when the object is stored and executed later, only the body
    /// (without the prolog) is executed, starting at PC=0.
    ///
    /// Returns the position after the innermost object's prolog (i.e., start_pos + 1),
    /// or 0 if we're at top level.
    fn jump_target_base(&self) -> usize {
        // Find the innermost Program, List, Symbolic, or Complex construct
        for construct in self.constructs.iter().rev() {
            match construct.kind {
                ConstructKind::Program | ConstructKind::List | ConstructKind::Symbolic | ConstructKind::Complex => {
                    // start_pos is the prolog position, body starts at start_pos + 1
                    return construct.start_pos + 1;
                }
                _ => continue,
            }
        }
        // At top level, no adjustment needed
        0
    }

    /// Convert an absolute position to a jump target relative to the innermost object.
    fn make_jump_target(&self, absolute_pos: usize) -> u32 {
        let base = self.jump_target_base();
        (absolute_pos - base) as u32
    }

    /// Compile the analyzed tokens into a program.
    pub fn compile(mut self) -> Result<CompiledProgram, Vec<Diagnostic>> {
        for i in 0..self.analysis.tokens.len() {
            self.token_index = i;
            let token = &self.analysis.tokens[i];

            if let Err(diag) = self.compile_token(token) {
                self.errors.push(diag);
            }
        }

        // Check for unclosed infix expression
        if self.infix_parser.is_some() {
            self.errors.push(
                Diagnostic::error(
                    rpl_core::error::ErrorCode::E101,
                    Span::new(rpl_core::Pos::new(0), rpl_core::Pos::new(0)),
                )
                .message("Unclosed symbolic expression")
                .build(),
            );
        }

        // Check for unclosed constructs
        while let Some(construct) = self.constructs.pop() {
            self.errors.push(
                Diagnostic::error(rpl_core::error::ErrorCode::E101, construct.open_span)
                    .message("Unclosed construct")
                    .build(),
            );
        }

        if self.errors.is_empty() {
            let (code, spans) = self.output.finish();
            Ok(CompiledProgram {
                code,
                spans,
                type_map: HashMap::new(), // TODO: populate during compilation
                interner: self.interner,
            })
        } else {
            Err(self.errors)
        }
    }

    /// Compile a single token.
    fn compile_token(&mut self, token: &ResolvedToken) -> Result<(), Diagnostic> {
        // Skip error tokens
        if token.error.is_some() {
            return Ok(());
        }

        // Get the token text
        let text = self.source.span_text(token.span);

        // Handle parameter collection mode (between → and ::)
        if self.collecting_local_params {
            return self.collect_local_param(token, text);
        }

        // Handle FOR variable collection mode (after FOR until variable is collected)
        if self.collecting_for_variable {
            return self.collect_for_variable(token, text);
        }

        // Get the library that matched this token
        let lib = self.registry.get(token.lib).ok_or_else(|| {
            Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                .message("Unknown library")
                .build()
        })?;

        // Get stack effect to track types
        let effect = lib.stack_effect(text);

        // Handle infix mode
        if self.in_infix() {
            return self.compile_infix_token(token);
        }

        // Handle operators with type-based resolution
        if let StackEffect::Operator { kind, arity } = effect {
            return self.compile_operator(kind, arity, token.span);
        }

        // Check for StartInfix (opening quote)
        if matches!(effect, StackEffect::StartConstruct)
            && token.info.ty() == TokenType::OpenBracket
            && text == "'"
        {
            return self.start_infix(token.span);
        }

        // Check for local variable reference
        // Only check atoms that are identifiers (not numbers)
        if token.info.ty() == TokenType::Atom
            && !text
                .chars()
                .next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(true)
        {
            // Intern the text to get a symbol
            let symbol = self.interner.intern(text);
            if let Some(local_symbol) = self.find_local_variable(symbol) {
                // Emit local variable lookup
                crate::well_known::emit_local_lookup(&mut self.output, local_symbol, token.span);
                // Local lookup produces one value on the stack
                self.stack.push(CType::Unknown);
                return Ok(());
            }
        }

        // Call the library's compile method to emit bytecode
        let current_construct = self.constructs.top().map(|c| c.kind);
        let in_infix = self.in_infix();

        // Create CompileContext with user library registry if available
        let result = if let Some(user_libs) = self.user_lib_registry {
            let mut ctx = CompileContext::with_user_lib_registry(
                token.span,
                text,
                &mut self.output,
                &mut self.interner,
                current_construct,
                in_infix,
                user_libs,
            );
            lib.compile(&mut ctx)
        } else {
            let mut ctx = CompileContext::new(
                token.span,
                text,
                &mut self.output,
                &mut self.interner,
                current_construct,
                in_infix,
            );
            lib.compile(&mut ctx)
        };

        match result {
            CompileResult::Ok => {
                // Apply stack effect with type tracking
                self.apply_stack_effect_tracked(token.lib, text, &effect, token.literal_type);
                Ok(())
            }
            CompileResult::NoMatch => {
                // Library didn't handle this token - this shouldn't happen
                // since the library matched during probing
                Err(Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                    .message(format!(
                        "Library {} did not compile token '{}'",
                        lib.name(),
                        text
                    ))
                    .build())
            }
            CompileResult::StartInfix => {
                // Library wants to start infix mode
                self.start_infix(token.span)
            }
            CompileResult::EndInfix => {
                // Library wants to end infix mode
                self.end_infix(token.span)
            }
            CompileResult::StartConstruct { kind } => {
                match kind {
                    ConstructKind::If | ConstructKind::Case => {
                        // Control flow constructs don't emit a prolog
                        let start_pos = self.output.len();
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                    }
                    ConstructKind::ErrorHandler => {
                        // IFERR: emit IFERR_SETUP + placeholder, track placeholder position
                        use crate::well_known::FLOW_CONTROL_LIB_ID;
                        self.output.emit(
                            rpl_core::make_call(FLOW_CONTROL_LIB_ID.as_u16(), 21), // CMD_IFERR_SETUP
                            token.span,
                        );
                        let placeholder_pos = self.output.len();
                        self.output.emit(0, token.span); // Placeholder for handler PC
                        // start_pos tracks the placeholder position for THENERR to patch
                        self.start_construct_at(kind, token.lib, placeholder_pos, token.span);
                    }
                    ConstructKind::For | ConstructKind::ForUp | ConstructKind::ForDn => {
                        // FOR/FORUP/FORDN loop: start construct and enter variable collection mode
                        // The next token should be the loop variable name
                        let start_pos = self.output.len();
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                        self.collecting_for_variable = true;
                    }
                    ConstructKind::DoUntil | ConstructKind::While => {
                        // Loop constructs: record the loop start position
                        let start_pos = self.output.len();
                        let relative_start = self.make_jump_target(start_pos) as usize;
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                        // Record where to jump back to (as relative offset)
                        if let Some(construct) = self.constructs.top_mut() {
                            construct.control_data.loop_start = Some(relative_start);
                        }
                    }
                    ConstructKind::Start => {
                        // START/NEXT loop: emit loop setup code
                        // Stack has: start finish
                        // LOOP_SETUP pops start/finish, computes direction, pushes 4 values to return stack
                        // No skip check - body always runs at least once (matches HP behavior)
                        use crate::well_known::FLOW_CONTROL_LIB_ID;
                        self.output.emit(
                            rpl_core::make_call(FLOW_CONTROL_LIB_ID.as_u16(), 4), // CMD_LOOP_SETUP
                            token.span,
                        );

                        let start_pos = self.output.len();
                        let relative_start = self.make_jump_target(start_pos) as usize;
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                        if let Some(construct) = self.constructs.top_mut() {
                            // Store loop_start as relative offset for NEXT/STEP to jump back to
                            construct.control_data.loop_start = Some(relative_start);
                        }
                        // START consumes two values from stack
                        self.stack.pop();
                        self.stack.pop();
                    }
                    ConstructKind::LocalBinding => {
                        // LocalBinding: Start parameter collection mode
                        // Don't emit anything yet - we'll emit CMD_LOCAL_FRAME_SETUP when :: is seen
                        let start_pos = self.output.len();
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                        // Enter parameter collection mode
                        self.collecting_local_params = true;
                        self.local_binding_params.clear();
                    }
                    ConstructKind::Complex => {
                        // Complex literals don't emit a prolog - values are pushed to stack
                        // and `)` will emit an opcode to create the complex from stack values
                        let start_pos = self.output.len();
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                    }
                    _ => {
                        // Object constructs (Program, List, Symbolic, etc.) emit a prolog
                        let type_id = match kind {
                            ConstructKind::Program => TypeId::PROGRAM,
                            ConstructKind::List => TypeId::LIST,
                            ConstructKind::Symbolic => TypeId::SYMBOLIC,
                            _ => TypeId::PROGRAM,
                        };
                        // Record position BEFORE emitting prolog
                        let start_pos = self.output.len();
                        self.output
                            .emit(rpl_core::make_prolog(type_id.as_u16(), 0), token.span);
                        // Start construct tracking with the prolog position
                        self.start_construct_at(kind, token.lib, start_pos, token.span);
                    }
                }
                Ok(())
            }
            CompileResult::EndConstruct => {
                // Library wants to end the current construct
                let _ = self.end_construct();
                Ok(())
            }
            CompileResult::NeedMore => {
                // Library needs more tokens
                // Check if this is a control flow keyword that needs position tracking
                let upper_text = text.to_ascii_uppercase();
                if upper_text == "THEN" {
                    // THEN just emitted JUMP_IF_FALSE + placeholder
                    // Record the placeholder position for ELSE or END to patch
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::If)
                    {
                        // Position of the placeholder (word after JUMP_IF_FALSE opcode)
                        construct.control_data.then_jump_target = Some(self.output.len() - 1);
                    }
                    // THEN consumes the test result from the stack
                    self.stack.pop();
                } else if upper_text == "ELSE" {
                    // ELSE just emitted JUMP + placeholder (to skip false-clause)
                    // Patch THEN's conditional jump to point here (start of else-clause)
                    // Record ELSE's jump position for END to patch
                    let target = self.make_jump_target(self.output.len());
                    let else_jump_pos = self.output.len() - 1;
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::If)
                    {
                        // Patch THEN's jump to point to current position (start of else)
                        if let Some(then_target_pos) = construct.control_data.then_jump_target {
                            self.output.patch(then_target_pos, target);
                        }
                        // Record ELSE's jump position for END to patch
                        construct.control_data.else_jump_target = Some(else_jump_pos);
                        // Clear then_jump_target since it's been patched
                        construct.control_data.then_jump_target = None;
                    }
                } else if upper_text == "UNTIL" {
                    // UNTIL just emitted JUMP_IF_FALSE + placeholder
                    // Patch the placeholder to jump back to DO (loop_start)
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::DoUntil)
                        && let Some(loop_start) = construct.control_data.loop_start
                    {
                        // Patch to jump back to start of loop
                        self.output.patch(self.output.len() - 1, loop_start as u32);
                    }
                    // UNTIL consumes the test result from the stack
                    self.stack.pop();
                } else if upper_text == "REPEAT" {
                    // REPEAT just emitted JUMP_IF_FALSE + placeholder + REPEAT_MARKER
                    // Record the placeholder for END to patch (exit jump)
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::While)
                    {
                        // Position of the placeholder (for exit jump)
                        // -2 because: -1 for REPEAT_MARKER, -1 more for the placeholder
                        construct.control_data.then_jump_target = Some(self.output.len() - 2);
                    }
                    // REPEAT consumes the test result from the stack
                    self.stack.pop();
                } else if upper_text == "THENCASE" {
                    // THENCASE just emitted JUMP_IF_FALSE + placeholder
                    // Record the placeholder for ENDTHEN to patch
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::Case)
                    {
                        // Position of the placeholder (word after JUMP_IF_FALSE opcode)
                        construct.control_data.then_jump_target = Some(self.output.len() - 1);
                    }
                    // THENCASE consumes the test result from the stack
                    self.stack.pop();
                } else if upper_text == "ENDTHEN" {
                    // ENDTHEN just emitted JUMP + placeholder + ENDTHEN_MARKER
                    // 1. Patch THENCASE's conditional jump to point here (after ENDTHEN_MARKER)
                    // 2. Record ENDTHEN's exit jump for ENDCASE to patch
                    let target = self.make_jump_target(self.output.len());
                    // Position of ENDTHEN's jump placeholder: -2 for ENDTHEN_MARKER, -1 for placeholder
                    let exit_jump_pos = self.output.len() - 2;
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::Case)
                    {
                        // Patch THENCASE's conditional jump to point here (skip to next test)
                        if let Some(thencase_target_pos) = construct.control_data.then_jump_target {
                            self.output.patch(thencase_target_pos, target);
                        }
                        // Record exit jump position for ENDCASE to patch
                        construct.control_data.case_exit_jumps.push(exit_jump_pos);
                        // Clear then_jump_target since it's been patched
                        construct.control_data.then_jump_target = None;
                    }
                } else if upper_text == "THENERR" {
                    // THENERR just emitted IFERR_SUCCESS + JUMP + placeholder + THENERR_MARKER
                    // 1. Patch IFERR's handler PC to point here (start of error handler)
                    // 2. Record THENERR's skip jump for ELSEERR or ENDERR to patch
                    let handler_target = self.make_jump_target(self.output.len());
                    // Position of THENERR's skip jump placeholder: -2 for THENERR_MARKER, -1 for placeholder
                    let skip_jump_pos = self.output.len() - 2;
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::ErrorHandler)
                    {
                        // Patch IFERR's handler PC placeholder (at start_pos, which IS the placeholder)
                        self.output.patch(construct.start_pos, handler_target);
                        // Record skip jump position for ELSEERR or ENDERR to patch
                        construct.control_data.then_jump_target = Some(skip_jump_pos);
                    }
                } else if upper_text == "ELSEERR" {
                    // ELSEERR just emitted JUMP + placeholder + ELSEERR_MARKER
                    // 1. Patch THENERR's skip jump to point here (start of no-error code)
                    // 2. Record ELSEERR's exit jump for ENDERR to patch
                    let target = self.make_jump_target(self.output.len());
                    // Position of ELSEERR's exit jump placeholder: -2 for ELSEERR_MARKER, -1 for placeholder
                    let exit_jump_pos = self.output.len() - 2;
                    if let Some(construct) = self.constructs.top_mut()
                        && matches!(construct.kind, ConstructKind::ErrorHandler)
                    {
                        // Patch THENERR's skip jump to point here
                        if let Some(thenerr_skip_pos) = construct.control_data.then_jump_target {
                            self.output.patch(thenerr_skip_pos, target);
                        }
                        // Record exit jump position for ENDERR to patch
                        construct.control_data.else_jump_target = Some(exit_jump_pos);
                        // Clear then_jump_target since it's been patched
                        construct.control_data.then_jump_target = None;
                    }
                }
                // Note: NEXT handling is done in end_construct() since NEXT returns EndConstruct
                Ok(())
            }
            CompileResult::Split { at_char: _ } => {
                // Token splitting - not implemented yet
                Err(Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                    .message("Token splitting not implemented")
                    .build())
            }
            CompileResult::Error { message } => {
                Err(Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                    .message(message)
                    .build())
            }
        }
    }

    /// Start infix mode for a symbolic expression.
    fn start_infix(&mut self, span: Span) -> Result<(), Diagnostic> {
        // Emit prolog for symbolic object (size will be patched)
        let start_pos = self.output.len();
        self.output
            .emit(rpl_core::make_prolog(TypeId::SYMBOLIC.as_u16(), 0), span);

        // Create infix parser
        self.infix_parser = Some(InfixParser::new(start_pos));
        self.infix_start_pos = Some(start_pos);

        // Start construct for tracking
        self.start_construct(ConstructKind::Symbolic, LibraryId::new(56), span);

        Ok(())
    }

    /// End infix mode.
    fn end_infix(&mut self, span: Span) -> Result<(), Diagnostic> {
        if let Some(parser) = self.infix_parser.take() {
            // Finish the infix parser, emitting remaining operators
            parser.finish(&mut self.output).map_err(|e| {
                Diagnostic::error(rpl_core::error::ErrorCode::E100, span)
                    .message(e.to_string())
                    .build()
            })?;

            // Patch the prolog size
            if let Some(start_pos) = self.infix_start_pos.take() {
                let size = self.output.len() - start_pos - 1; // -1 for prolog itself
                let prolog = rpl_core::make_prolog(TypeId::SYMBOLIC.as_u16(), size as u16);
                self.output.patch(start_pos, prolog);
            }

            // End construct
            let _ = self.end_construct();

            // Symbolic expression produces one value
            self.stack.push(CType::Known(TypeId::SYMBOLIC));

            Ok(())
        } else {
            Err(Diagnostic::error(rpl_core::error::ErrorCode::E102, span)
                .message("Not in symbolic expression")
                .build())
        }
    }

    /// Compile a token while in infix mode.
    fn compile_infix_token(&mut self, token: &ResolvedToken) -> Result<(), Diagnostic> {
        let text = self.source.span_text(token.span);
        let token_type = token.info.ty();

        // Check for closing quote
        if token_type == TokenType::CloseBracket && text == "'" {
            return self.end_infix(token.span);
        }

        // Get the infix parser
        let parser = self.infix_parser.as_mut().ok_or_else(|| {
            Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                .message("Not in infix mode")
                .build()
        })?;

        match token_type {
            TokenType::Atom => {
                // Emit the atom by calling the library's compile method
                // This handles number literals (which need prolog + data) and variables
                let lib = self.registry.get(token.lib).ok_or_else(|| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                        .message("Unknown library")
                        .build()
                })?;
                let current_construct = self.constructs.top().map(|c| c.kind);
                let result = if let Some(user_libs) = self.user_lib_registry {
                    let mut ctx = CompileContext::with_user_lib_registry(
                        token.span,
                        text,
                        &mut self.output,
                        &mut self.interner,
                        current_construct,
                        true, // in_infix = true
                        user_libs,
                    );
                    lib.compile(&mut ctx)
                } else {
                    let mut ctx = CompileContext::new(
                        token.span,
                        text,
                        &mut self.output,
                        &mut self.interner,
                        current_construct,
                        true, // in_infix = true
                    );
                    lib.compile(&mut ctx)
                };
                if !matches!(result, CompileResult::Ok) {
                    return Err(Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                        .message(format!("Failed to compile atom '{}' in infix mode", text))
                        .build());
                }
                parser.push_atom();
            }
            TokenType::BinaryLeft | TokenType::BinaryRight | TokenType::Prefix => {
                // Get the OperatorKind from the token text
                let op_kind = text_to_operator_kind(text).ok_or_else(|| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                        .message(format!(
                            "Unknown operator '{}' in symbolic expression",
                            text
                        ))
                        .build()
                })?;
                let entry = OperatorEntry {
                    span: token.span,
                    op_kind,
                    precedence: token.info.precedence(),
                    arity: token.info.nargs(),
                    right_assoc: token.info.right_assoc(),
                };
                parser.push_operator(entry, &mut self.output);
            }
            TokenType::OpenBracket => {
                parser.push_open_bracket(token.span);
            }
            TokenType::CloseBracket => {
                parser.push_close_bracket(&mut self.output).map_err(|e| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E102, token.span)
                        .message(e.to_string())
                        .build()
                })?;
            }
            TokenType::Comma => {
                parser.push_comma(&mut self.output).map_err(|e| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                        .message(e.to_string())
                        .build()
                })?;
            }
            TokenType::Function => {
                // Get the OperatorKind from the function name
                let op_kind = text_to_operator_kind(text).ok_or_else(|| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                        .message(format!(
                            "Unknown function '{}' in symbolic expression",
                            text
                        ))
                        .build()
                })?;
                let entry = OperatorEntry {
                    span: token.span,
                    op_kind,
                    precedence: 0,
                    arity: token.info.nargs(),
                    right_assoc: false,
                };
                parser.push_function(entry, token.span);
            }
            TokenType::Postfix => {
                // Postfix operators are emitted immediately after their operand using dynamic dispatch
                let op_kind = text_to_operator_kind(text).ok_or_else(|| {
                    Diagnostic::error(rpl_core::error::ErrorCode::E001, token.span)
                        .message(format!(
                            "Unknown postfix operator '{}' in symbolic expression",
                            text
                        ))
                        .build()
                })?;
                let encoded = crate::dispatch_impl::encode_dispatch(op_kind, token.info.nargs());
                let opcode = make_call(crate::dispatch_impl::DISPATCH_LIB, encoded);
                self.output.emit(opcode, token.span);
            }
        }

        Ok(())
    }

    /// Compile an operator with type-based resolution.
    fn compile_operator(
        &mut self,
        op: OperatorKind,
        arity: u8,
        span: Span,
    ) -> Result<(), Diagnostic> {
        if arity == 1 {
            // Unary operator
            let operand_type = self.stack.at(0);

            if let Some(type_id) = operand_type.as_known() {
                // Try to resolve with known type
                if let Some(resolution) = self.operators.resolve_unary(op, type_id) {
                    let opcode = make_call(resolution.lib.as_u16(), resolution.command);
                    self.output.emit(opcode, span);

                    // Update stack
                    self.stack.pop();
                    self.stack.push(CType::Known(resolution.result_type));

                    return Ok(());
                }
            }

            // Fall back to dynamic dispatch
            self.emit_dynamic_dispatch(op, arity, span);
        } else if arity == 2 {
            // Binary operator
            let right_type = self.stack.at(0);
            let left_type = self.stack.at(1);

            if let (Some(left), Some(right)) = (left_type.as_known(), right_type.as_known()) {
                // Try to resolve with known types
                if let Some(resolution) = self.operators.resolve_binary(op, left, right) {
                    let opcode = make_call(resolution.lib.as_u16(), resolution.command);
                    self.output.emit(opcode, span);

                    // Update stack
                    self.stack.pop();
                    self.stack.pop();
                    self.stack.push(CType::Known(resolution.result_type));

                    return Ok(());
                }

                // Try coercion if types don't match directly
                if let Some(common) = self.operators.common_type(left, right) {
                    // Emit coercions and retry
                    if left != common {
                        self.emit_coercion(left, common, span)?;
                    }
                    if right != common {
                        self.emit_coercion(right, common, span)?;
                    }

                    // Try resolution again with common type
                    if let Some(resolution) = self.operators.resolve_binary(op, common, common) {
                        let opcode = make_call(resolution.lib.as_u16(), resolution.command);
                        self.output.emit(opcode, span);

                        self.stack.pop();
                        self.stack.pop();
                        self.stack.push(CType::Known(resolution.result_type));

                        return Ok(());
                    }
                }
            }

            // Fall back to dynamic dispatch
            self.emit_dynamic_dispatch(op, arity, span);
        }

        Ok(())
    }

    /// Emit a coercion from one type to another.
    #[allow(dead_code)]
    fn emit_coercion(&mut self, from: TypeId, to: TypeId, span: Span) -> Result<(), Diagnostic> {
        if let Some(path) = self.operators.find_coercion(from, to) {
            for &(_, lib, cmd) in &path.steps {
                let opcode = make_call(lib.as_u16(), cmd);
                self.output.emit(opcode, span);
            }
            Ok(())
        } else {
            Err(Diagnostic::error(rpl_core::error::ErrorCode::E100, span)
                .message(format!("No coercion from {:?} to {:?}", from, to))
                .build())
        }
    }

    /// Emit dynamic dispatch for an operator.
    fn emit_dynamic_dispatch(&mut self, op: OperatorKind, arity: u8, span: Span) {
        use crate::dispatch_impl::{DISPATCH_LIB, encode_dispatch};

        // Encode operator kind and arity in command
        let cmd = encode_dispatch(op, arity);
        let opcode = make_call(DISPATCH_LIB, cmd);
        self.output.emit(opcode, span);

        // Pop operands, push unknown result
        for _ in 0..arity {
            self.stack.pop();
        }
        self.stack.push(CType::Unknown);
    }

    /// Start a construct (program, list, etc.).
    #[allow(dead_code)]
    fn start_construct(&mut self, kind: ConstructKind, lib: LibraryId, span: Span) {
        self.start_construct_at(kind, lib, self.output.len(), span);
    }

    /// Start a construct at a specific position (for when prolog is emitted separately).
    fn start_construct_at(
        &mut self,
        kind: ConstructKind,
        lib: LibraryId,
        start_pos: usize,
        span: Span,
    ) {
        let construct = Construct::new(kind, lib, start_pos, span);
        self.constructs.push(construct);
    }

    /// End a construct and patch its size.
    #[allow(dead_code)]
    fn end_construct(&mut self) -> Result<(), Diagnostic> {
        let construct = self.constructs.pop().ok_or_else(|| {
            Diagnostic::error(
                rpl_core::error::ErrorCode::E102,
                Span::new(rpl_core::Pos::new(0), rpl_core::Pos::new(0)),
            )
            .message("No construct to close")
            .build()
        })?;

        // Calculate the size of the construct body (excluding the prolog word)
        // Use saturating_sub to handle edge case where output length equals start position + 1
        let size = self.output.len().saturating_sub(construct.start_pos + 1);

        // Patch the prolog at start_pos with the correct size
        match construct.kind {
            ConstructKind::Program => {
                let prolog = rpl_core::make_prolog(TypeId::PROGRAM.as_u16(), size as u16);
                self.output.patch(construct.start_pos, prolog);
                // Program produces one value on the stack
                self.stack.push(CType::Known(TypeId::PROGRAM));
            }
            ConstructKind::Symbolic => {
                // Symbolic prologs are patched by end_infix(), not here.
                // Just pop the construct without doing anything.
            }
            ConstructKind::List => {
                let prolog = rpl_core::make_prolog(TypeId::LIST.as_u16(), size as u16);
                self.output.patch(construct.start_pos, prolog);
                self.stack.push(CType::Known(TypeId::LIST));
            }
            ConstructKind::If => {
                // Patch the remaining jump (either THEN's or ELSE's)
                let target = self.make_jump_target(self.output.len());

                if let Some(else_jump_pos) = construct.control_data.else_jump_target {
                    // We had an ELSE, patch its jump to here
                    self.output.patch(else_jump_pos, target);
                } else if let Some(then_jump_pos) = construct.control_data.then_jump_target {
                    // No ELSE, patch THEN's jump to here
                    self.output.patch(then_jump_pos, target);
                }
                // IF construct doesn't produce a value
            }
            ConstructKind::Case => {
                // Patch all exit jumps (from ENDTHEN) to point here
                let target = self.make_jump_target(self.output.len());
                for exit_jump_pos in &construct.control_data.case_exit_jumps {
                    self.output.patch(*exit_jump_pos, target);
                }
                // CASE construct doesn't produce a value
            }
            ConstructKind::ErrorHandler => {
                // Patch the remaining jump (either THENERR's or ELSEERR's) to point here
                let target = self.make_jump_target(self.output.len());

                if let Some(elseerr_jump_pos) = construct.control_data.else_jump_target {
                    // We had an ELSEERR, patch its jump to here
                    self.output.patch(elseerr_jump_pos, target);
                } else if let Some(thenerr_jump_pos) = construct.control_data.then_jump_target {
                    // No ELSEERR, patch THENERR's jump to here
                    self.output.patch(thenerr_jump_pos, target);
                }
                // ErrorHandler construct doesn't produce a value
            }
            ConstructKind::DoUntil => {
                // DO/UNTIL/END: UNTIL already patched its backward jump
                // END just closes the construct, nothing more to patch
            }
            ConstructKind::While => {
                // WHILE/REPEAT/END: END needs to:
                // 1. Emit unconditional jump back to WHILE (loop_start)
                // 2. Patch REPEAT's exit jump to point here (after the backward jump)
                use crate::well_known::FLOW_CONTROL_LIB_ID;

                // Emit backward jump to loop start
                if let Some(loop_start) = construct.control_data.loop_start {
                    self.output.emit(
                        rpl_core::make_call(FLOW_CONTROL_LIB_ID.as_u16(), 1), // CMD_JUMP
                        construct.open_span,
                    );
                    self.output.emit(loop_start as u32, construct.open_span);
                }

                // Patch REPEAT's exit jump to point here
                let target = self.make_jump_target(self.output.len());
                if let Some(repeat_jump_pos) = construct.control_data.then_jump_target {
                    self.output.patch(repeat_jump_pos, target);
                }
            }
            ConstructKind::Start | ConstructKind::For => {
                // START/NEXT/STEP or FOR/NEXT/STEP
                // NEXT/STEP just emitted CMD_LOOP_NEXT/CMD_FOR_NEXT/CMD_LOOP_STEP/CMD_FOR_STEP + placeholder
                // Patch the backward jump placeholder (at output.len() - 1) with loop_start
                //
                // Note: FOR frame cleanup is handled at runtime by FOR_NEXT/FOR_STEP
                // when they detect loop termination, not here.
                // No skip_target patching needed - HP behavior is body always runs at least once.

                if let Some(loop_start) = construct.control_data.loop_start {
                    self.output.patch(self.output.len() - 1, loop_start as u32);
                }
            }
            ConstructKind::ForUp | ConstructKind::ForDn => {
                // FORUP/FORDN: like FOR but with skip_target patching
                // NEXT/STEP just emitted CMD_FOR_NEXT/CMD_FOR_STEP + placeholder
                // Patch the backward jump placeholder (at output.len() - 1) with loop_start

                if let Some(loop_start) = construct.control_data.loop_start {
                    self.output.patch(self.output.len() - 1, loop_start as u32);
                }

                // Patch skip_target to point to after the NEXT/STEP instruction
                // This is where execution continues for zero iterations
                let skip_target = self.make_jump_target(self.output.len());
                if let Some(skip_target_pos) = construct.control_data.skip_target {
                    self.output.patch(skip_target_pos, skip_target);
                }
            }
            ConstructKind::LocalBinding => {
                // LocalBinding: emit LOCAL_FRAME_POP at the end
                // The param count is stored in the construct's local_params
                let param_count = construct.local_params.len();
                crate::well_known::emit_frame_pop(
                    &mut self.output,
                    param_count,
                    construct.open_span,
                );
                // Pop values from stack for each param consumed
                for _ in 0..param_count {
                    self.stack.pop();
                }
            }
            _ => {
                // Other constructs may need different handling
            }
        }

        Ok(())
    }

    /// Collect a parameter name during local binding parameter collection mode.
    ///
    /// Called for each token between `→` and `::` (or `<<`).
    fn collect_local_param(&mut self, token: &ResolvedToken, text: &str) -> Result<(), Diagnostic> {
        // Check if this is the :: or << that ends parameter collection
        // (matched by ProgramsLib as an open bracket)
        if text == "::" || text == "<<" {
            return self.end_local_param_collection(token.span);
        }

        // Only allow identifiers/atoms as parameter names
        // Token type must be Atom (identifier)
        if token.info.ty() != TokenType::Atom {
            return Err(Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                .message(format!("Expected parameter name after →, found '{}'", text))
                .build());
        }

        // Don't allow numbers as parameter names
        if text
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        {
            return Err(Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                .message(format!("Parameter name cannot be a number: '{}'", text))
                .build());
        }

        // Intern the parameter name and add to collection
        let symbol = self.interner.intern(text);
        self.local_binding_params.push(symbol);

        Ok(())
    }

    /// End local parameter collection and start the body.
    ///
    /// Called when `::` is encountered after `→`.
    fn end_local_param_collection(&mut self, span: Span) -> Result<(), Diagnostic> {
        // Exit parameter collection mode
        self.collecting_local_params = false;

        // Get the current LocalBinding construct
        let construct = self.constructs.top_mut().ok_or_else(|| {
            Diagnostic::error(rpl_core::error::ErrorCode::E102, span)
                .message("No LocalBinding construct active")
                .build()
        })?;

        // Store the params in the construct for later use when ending
        construct.local_params = self.local_binding_params.clone();
        construct.body_started = true;

        // Emit CMD_LOCAL_FRAME_SETUP with the collected params
        crate::well_known::emit_frame_setup(&mut self.output, &self.local_binding_params, span);

        // Clear the temporary param collection
        self.local_binding_params.clear();

        Ok(())
    }

    /// Collect the FOR loop variable (called for the first token after FOR).
    fn collect_for_variable(&mut self, token: &ResolvedToken, text: &str) -> Result<(), Diagnostic> {
        // Exit variable collection mode
        self.collecting_for_variable = false;

        // Only allow identifiers/atoms as variable names
        if token.info.ty() != TokenType::Atom {
            return Err(Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                .message(format!("Expected variable name after FOR, found '{}'", text))
                .build());
        }

        // Don't allow numbers as variable names
        if text
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
        {
            return Err(Diagnostic::error(rpl_core::error::ErrorCode::E100, token.span)
                .message(format!("FOR variable name cannot be a number: '{}'", text))
                .build());
        }

        // Intern the variable name
        let symbol = self.interner.intern(text);

        // Get the current FOR/FORUP/FORDN construct
        let construct = self.constructs.top_mut().ok_or_else(|| {
            Diagnostic::error(rpl_core::error::ErrorCode::E102, token.span)
                .message("No FOR construct active")
                .build()
        })?;

        // Store the variable in the construct's local_params
        construct.local_params = vec![symbol];
        construct.body_started = true;
        let kind = construct.kind;

        // Emit setup command based on construct kind
        // Stack has: start finish
        use crate::well_known::FLOW_CONTROL_LIB_ID;
        match kind {
            ConstructKind::For => {
                // FOR_SETUP pops start/finish, computes direction, creates local frame, pushes 4 values
                // No skip check - body always runs at least once (matches HP behavior)
                // CMD_FOR_SETUP <symbol>
                self.output.emit(
                    make_call(FLOW_CONTROL_LIB_ID.as_u16(), 11), // CMD_FOR_SETUP
                    token.span,
                );
                self.output.emit(symbol.as_u32(), token.span);
            }
            ConstructKind::ForUp => {
                // FORUP_SETUP: ascending FOR with skip if start > end
                // CMD_FORUP_SETUP <symbol> <skip_target>
                self.output.emit(
                    make_call(FLOW_CONTROL_LIB_ID.as_u16(), 16), // CMD_FORUP_SETUP
                    token.span,
                );
                self.output.emit(symbol.as_u32(), token.span);
                // Emit placeholder for skip_target (patched when NEXT/STEP is compiled)
                self.output.emit(0, token.span);
                let skip_target_pos = self.output.len() - 1;
                if let Some(construct) = self.constructs.top_mut() {
                    construct.control_data.skip_target = Some(skip_target_pos);
                }
            }
            ConstructKind::ForDn => {
                // FORDN_SETUP: descending FOR with skip if start < end
                // CMD_FORDN_SETUP <symbol> <skip_target>
                self.output.emit(
                    make_call(FLOW_CONTROL_LIB_ID.as_u16(), 17), // CMD_FORDN_SETUP
                    token.span,
                );
                self.output.emit(symbol.as_u32(), token.span);
                // Emit placeholder for skip_target (patched when NEXT/STEP is compiled)
                self.output.emit(0, token.span);
                let skip_target_pos = self.output.len() - 1;
                if let Some(construct) = self.constructs.top_mut() {
                    construct.control_data.skip_target = Some(skip_target_pos);
                }
            }
            _ => unreachable!("collect_for_variable called for non-FOR construct"),
        }

        // Record positions for NEXT/STEP to use
        let start_pos = self.output.len();
        let relative_start = self.make_jump_target(start_pos) as usize;

        if let Some(construct) = self.constructs.top_mut() {
            construct.control_data.loop_start = Some(relative_start);
        }

        // FOR consumes two values from stack (start, finish)
        self.stack.pop();
        self.stack.pop();

        Ok(())
    }

    /// Find a local variable by symbol, returning its symbol if found.
    ///
    /// Searches from innermost to outermost scope to handle shadowing.
    fn find_local_variable(&self, symbol: Symbol) -> Option<Symbol> {
        // Check all LocalBinding and For/ForUp/ForDn constructs from innermost to outermost
        for construct in self.constructs.iter().rev() {
            if (construct.kind == ConstructKind::LocalBinding
                || construct.kind == ConstructKind::For
                || construct.kind == ConstructKind::ForUp
                || construct.kind == ConstructKind::ForDn)
                && construct.body_started
                && construct.local_params.contains(&symbol)
            {
                return Some(symbol);
            }
        }
        None
    }

    /// Apply a stack effect with type tracking for known operations.
    ///
    /// This method special-cases stack manipulation operations (DUP, SWAP, etc.)
    /// to preserve type information through the operation.
    fn apply_stack_effect_tracked(
        &mut self,
        lib_id: LibraryId,
        text: &str,
        effect: &StackEffect,
        result_type: Option<TypeId>,
    ) {
        use crate::well_known::STACK_LIB_ID;

        // Special-case stack manipulation operations to preserve types
        if lib_id == STACK_LIB_ID {
            match text.to_ascii_uppercase().as_str() {
                "DUP" => {
                    // DUP: [..., a] → [..., a, a]
                    let t = self.stack.at(0).clone();
                    self.stack.push(t);
                    return;
                }
                "DROP" => {
                    // DROP: [..., a] → [...]
                    self.stack.pop();
                    return;
                }
                "SWAP" => {
                    // SWAP: [..., a, b] → [..., b, a]
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(b);
                    self.stack.push(a);
                    return;
                }
                "OVER" => {
                    // OVER: [..., a, b] → [..., a, b, a]
                    let t = self.stack.at(1).clone();
                    self.stack.push(t);
                    return;
                }
                "ROT" => {
                    // ROT: [..., a, b, c] → [..., b, c, a]
                    let c = self.stack.pop();
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(b);
                    self.stack.push(c);
                    self.stack.push(a);
                    return;
                }
                "DEPTH" => {
                    // DEPTH: [...] → [..., n] where n is stack depth
                    // Result is always an integer (Real in our case)
                    self.stack.push(CType::Known(TypeId::REAL));
                    return;
                }
                "CLEAR" => {
                    // CLEAR: [...] → []
                    self.stack.clear();
                    return;
                }
                _ => {}
            }
        }

        // Fall back to generic stack effect application
        self.apply_stack_effect(effect, result_type);
    }

    /// Apply a stack effect to the compile-time stack.
    fn apply_stack_effect(&mut self, effect: &StackEffect, result_type: Option<TypeId>) {
        match effect {
            StackEffect::Fixed { consumes, produces } => {
                for _ in 0..*consumes {
                    self.stack.pop();
                }
                for _ in 0..*produces {
                    if let Some(t) = result_type {
                        self.stack.push(CType::Known(t));
                    } else {
                        self.stack.push(CType::Unknown);
                    }
                }
            }
            StackEffect::Operator { arity, .. } => {
                for _ in 0..*arity {
                    self.stack.pop();
                }
                // Operators typically produce one result
                if let Some(t) = result_type {
                    self.stack.push(CType::Known(t));
                } else {
                    self.stack.push(CType::Unknown);
                }
            }
            StackEffect::Variadic { produces } => {
                // Clear and push produces items
                self.stack.clear();
                for _ in 0..*produces {
                    self.stack.push(CType::Unknown);
                }
            }
            StackEffect::Dynamic => {
                self.stack.mark_unknown_depth();
            }
            StackEffect::StartConstruct | StackEffect::EndConstruct => {
                // No stack effect for construct markers
            }
        }
    }
}

/// Map operator text to OperatorKind for infix expressions.
fn text_to_operator_kind(text: &str) -> Option<OperatorKind> {
    match text {
        "+" => Some(OperatorKind::Add),
        "-" => Some(OperatorKind::Sub),
        "*" => Some(OperatorKind::Mul),
        "/" => Some(OperatorKind::Div),
        "^" => Some(OperatorKind::Pow),
        "==" | "SAME" => Some(OperatorKind::Eq),
        "!=" | "<>" => Some(OperatorKind::Ne),
        "<" => Some(OperatorKind::Lt),
        ">" => Some(OperatorKind::Gt),
        "<=" | "≤" => Some(OperatorKind::Le),
        ">=" | "≥" => Some(OperatorKind::Ge),
        _ => {
            let upper = text.to_ascii_uppercase();
            match upper.as_str() {
                "NEG" => Some(OperatorKind::Neg),
                "ABS" => Some(OperatorKind::Abs),
                "INV" => Some(OperatorKind::Inv),
                "MOD" => Some(OperatorKind::Mod),
                "AND" => Some(OperatorKind::And),
                "OR" => Some(OperatorKind::Or),
                "XOR" => Some(OperatorKind::Xor),
                "NOT" => Some(OperatorKind::Not),
                "SAME" => Some(OperatorKind::Same),
                _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{AnalysisResult, LineState, ParseState, ResolvedToken, TokenContext},
        library::LibraryRegistry,
        operator::OperatorRegistry,
        well_known::{REAL_NUMBERS_LIB_ID, ARITHMETIC_LIB_ID, SYMBOLIC_LIB_ID},
    };
    use rpl_core::{Pos, Span, SemanticKind, TokenInfo};
    use rpl_source::SourceFile;

    fn make_token(start: u32, end: u32, lib: LibraryId, semantic: SemanticKind) -> ResolvedToken {
        ResolvedToken {
            span: Span::new(Pos::new(start), Pos::new(end)),
            lib,
            info: TokenInfo::atom(1),
            semantic,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        }
    }

    fn setup_compiler<'a>(
        registry: &'a LibraryRegistry,
        operators: &'a mut OperatorRegistry,
        analysis: &'a AnalysisResult,
        source: &'a SourceFile,
    ) -> Compiler<'a> {
        Compiler::new(registry, operators, analysis, source, Interner::new())
    }

    #[test]
    fn compiler_new() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();
        let source = SourceFile::new(
            rpl_source::SourceId::new(0),
            "test".into(),
            "3 4 +".into(),
        );
        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens: vec![],
            lines: vec![],
            diagnostics: vec![],
        };

        let compiler = setup_compiler(&registry, &mut operators, &analysis, &source);
        assert_eq!(compiler.token_index, 0);
        assert!(compiler.errors.is_empty());
    }

    #[test]
    fn compiler_compile_empty() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();
        let source = SourceFile::new(rpl_source::SourceId::new(0), "test".into(), "".into());
        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens: vec![],
            lines: vec![],
            diagnostics: vec![],
        };

        let compiler = setup_compiler(&registry, &mut operators, &analysis, &source);
        let result = compiler.compile();

        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(program.is_empty());
    }

    // NOTE: This test requires registered libraries to compile tokens.
    // Run as integration test in rpl-session instead.
    #[test]
    #[ignore = "requires stdlib"]
    fn compiler_compile_simple() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();
        let source = SourceFile::new(
            rpl_source::SourceId::new(0),
            "test".into(),
            "3 4 +".into(),
        );

        // Create tokens for "3 4 +"
        let tokens = vec![
            make_token(0, 1, REAL_NUMBERS_LIB_ID, SemanticKind::Number),
            make_token(2, 3, REAL_NUMBERS_LIB_ID, SemanticKind::Number),
            make_token(4, 5, ARITHMETIC_LIB_ID, SemanticKind::Operator),
        ];

        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens,
            lines: vec![LineState {
                hash: 0,
                first_token: 0,
                token_count: 3,
                state_before: ParseState::default(),
                state_after: ParseState::default(),
            }],
            diagnostics: vec![],
        };

        let compiler = setup_compiler(&registry, &mut operators, &analysis, &source);
        let result = compiler.compile();

        assert!(result.is_ok());
        let program = result.unwrap();
        // Numbers emit: prolog(1) + data(2) = 3 words each
        // So "3 4 +" = 3 + 3 + 1 = 7 words
        assert_eq!(program.len(), 7);
    }

    #[test]
    fn output_buffer_emit_patch() {
        let mut buf = OutputBuffer::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        buf.emit(0x1234, span);
        assert_eq!(buf.get(0), Some(0x1234));

        buf.patch(0, 0xABCD);
        assert_eq!(buf.get(0), Some(0xABCD));
    }

    #[test]
    fn ctype_join_produces_correct_results() {
        let a = CType::Known(TypeId::REAL);
        let b = CType::Known(TypeId::REAL);
        assert_eq!(a.join(&b), CType::Known(TypeId::REAL));

        let c = CType::Known(TypeId::STRING);
        let joined = a.join(&c);
        assert!(matches!(joined, CType::OneOf(_)));
    }

    #[test]
    fn cstack_tracks_types() {
        let mut stack = CStack::new();
        stack.push_known(TypeId::REAL);
        stack.push_known(TypeId::REAL);

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.at(0), CType::Known(TypeId::REAL));

        let popped = stack.pop();
        assert_eq!(popped, CType::Known(TypeId::REAL));
        assert_eq!(stack.depth(), 1);
    }

    #[test]
    fn compiled_program_accessors() {
        let program = CompiledProgram {
            code: vec![1, 2, 3],
            spans: vec![
                Span::new(Pos::new(0), Pos::new(1)),
                Span::new(Pos::new(1), Pos::new(2)),
                Span::new(Pos::new(2), Pos::new(3)),
            ],
            type_map: HashMap::new(),
            interner: Interner::new(),
        };

        assert_eq!(program.len(), 3);
        assert!(!program.is_empty());
    }

    // NOTE: Tests requiring stdlib (compile_resolves_real_add, compile_tracks_result_type,
    // compile_symbolic_simple, compile_unclosed_symbolic_is_error) have been moved to
    // integration tests in rpl-session to avoid circular dependencies.

    // NOTE: This test requires registered libraries to compile tokens.
    // Run as integration test in rpl-session instead.
    #[test]
    #[ignore = "requires stdlib"]
    fn compile_symbolic_simple() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();

        // Source: '3 + 4'
        let source = SourceFile::new(
            rpl_source::SourceId::new(0),
            "test".into(),
            "'3 + 4'".into(),
        );

        // Manually create tokens for a symbolic expression
        // ' (open quote)
        let open_quote = ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(1)),
            lib: SYMBOLIC_LIB_ID,
            info: TokenInfo::open_bracket(1),
            semantic: SemanticKind::Bracket,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        };

        // 3 (number in infix)
        let num1 = ResolvedToken {
            span: Span::new(Pos::new(1), Pos::new(2)),
            lib: REAL_NUMBERS_LIB_ID,
            info: TokenInfo::atom(1),
            semantic: SemanticKind::Number,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: Some(TypeId::REAL),
            error: None,
        };

        // + (operator in infix)
        let add = ResolvedToken {
            span: Span::new(Pos::new(3), Pos::new(4)),
            lib: SYMBOLIC_LIB_ID,
            info: TokenInfo::binary_left(1, 8),
            semantic: SemanticKind::Operator,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        };

        // 4 (number in infix)
        let num2 = ResolvedToken {
            span: Span::new(Pos::new(5), Pos::new(6)),
            lib: REAL_NUMBERS_LIB_ID,
            info: TokenInfo::atom(1),
            semantic: SemanticKind::Number,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: Some(TypeId::REAL),
            error: None,
        };

        // ' (close quote)
        let close_quote = ResolvedToken {
            span: Span::new(Pos::new(6), Pos::new(7)),
            lib: SYMBOLIC_LIB_ID,
            info: TokenInfo::close_bracket(1),
            semantic: SemanticKind::Bracket,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        };

        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens: vec![open_quote, num1, add, num2, close_quote],
            lines: vec![LineState {
                hash: 0,
                first_token: 0,
                token_count: 5,
                state_before: ParseState::default(),
                state_after: ParseState::default(),
            }],
            diagnostics: vec![],
        };

        let compiler = setup_compiler(&registry, &mut operators, &analysis, &source);
        let result = compiler.compile();

        assert!(result.is_ok());
        let program = result.unwrap();

        // Should have: prolog + num1 + num2 + operator = 4 words
        // (Note: actual output depends on how numbers are compiled in infix mode)
        assert!(program.len() >= 3); // At least prolog + some contents
    }

    #[test]
    fn compile_unclosed_symbolic_is_error() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();

        // Source: just an open quote, never closed
        let source = SourceFile::new(rpl_source::SourceId::new(0), "test".into(), "'3".into());

        // ' (open quote) - but no closing quote
        let open_quote = ResolvedToken {
            span: Span::new(Pos::new(0), Pos::new(1)),
            lib: SYMBOLIC_LIB_ID,
            info: TokenInfo::open_bracket(1),
            semantic: SemanticKind::Bracket,
            context: TokenContext::default(),
            defines: None,
            references: None,
            literal_type: None,
            error: None,
        };

        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens: vec![open_quote],
            lines: vec![LineState {
                hash: 0,
                first_token: 0,
                token_count: 1,
                state_before: ParseState::default(),
                state_after: ParseState::default(),
            }],
            diagnostics: vec![],
        };

        let compiler = setup_compiler(&registry, &mut operators, &analysis, &source);
        let result = compiler.compile();

        // Should fail due to unclosed symbolic expression
        assert!(result.is_err());
    }

    #[test]
    fn compiler_in_infix_tracking() {
        let registry = LibraryRegistry::new();
        let mut operators = OperatorRegistry::new();
        let source = SourceFile::new(rpl_source::SourceId::new(0), "test".into(), "".into());
        let analysis = AnalysisResult {
            source_id: rpl_source::SourceId::new(0),
            version: 0,
            tokens: vec![],
            lines: vec![],
            diagnostics: vec![],
        };

        let mut compiler = setup_compiler(&registry, &mut operators, &analysis, &source);

        // Initially not in infix mode
        assert!(!compiler.in_infix());

        // Start infix
        compiler
            .start_infix(Span::new(Pos::new(0), Pos::new(1)))
            .unwrap();
        assert!(compiler.in_infix());

        // End infix
        compiler
            .end_infix(Span::new(Pos::new(1), Pos::new(2)))
            .unwrap();
        assert!(!compiler.in_infix());
    }
}
