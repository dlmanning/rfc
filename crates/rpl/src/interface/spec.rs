//! Interface specification from parsed declarations.
//!
//! Features:
//! - Parser combinator architecture (no code duplication)
//! - Single execution path for all patterns
//! - Generic capture mechanism via `?!` DSL syntax
//! - Clean separation of data and behavior

use super::{
    parse, ConcreteType, Declaration, Library, ParseError as InterfaceParseError, PatternElement,
    Type,
};
use crate::{
    core::{Pos, Span, TypeId},
    ir::{LibId, Node},
    libs::{ClaimContext, CommandInfo, LibraryInterface, StackEffect, TokenClaim},
    parse::{ParseContext, ParseError},
    types::CStack,
};

// ============================================================================
// Core Types
// ============================================================================

/// A library interface specification parsed from a declaration file.
#[derive(Debug, Clone)]
pub struct InterfaceSpec {
    name: String,
    id: LibId,
    commands: Vec<CommandSpec>,
    syntax: Vec<SyntaxDecl>,
}

/// A command specification from an interface.
#[derive(Debug, Clone)]
pub struct CommandSpec {
    pub names: Vec<String>,
    pub cmd_id: u16,
    pub inputs: Vec<Type>,
    pub outputs: Vec<Type>,
    pub effect_kind: EffectKind,
}

/// A syntax declaration with compiled parser.
#[derive(Debug, Clone)]
pub struct SyntaxDecl {
    /// Explicit ID for this syntax construct.
    pub id: u16,
    /// The token that triggers this syntax (for claims).
    pub token: String,
    /// The compiled parser for this syntax.
    pub parser: Parser,
}

/// How to compute the stack effect for a command.
#[derive(Debug, Clone, PartialEq)]
pub enum EffectKind {
    Static(StackEffect),
    BinaryNumeric,
    UnaryPreserving,
}

// ============================================================================
// Parser Combinator Types
// ============================================================================

/// A composable parser built from pattern declarations.
///
/// Each variant represents a parsing operation. Complex patterns are built
/// by combining these with `Sequence`.
#[derive(Debug, Clone)]
pub enum Parser {
    /// Expect a required keyword, consume it.
    Keyword(String),

    /// Try to consume an optional keyword. Always succeeds.
    OptionalKeyword(String),

    /// Expect a required keyword. If consumed, capture the last
    /// expression from the previous branch into a new branch.
    KeywordWithCapture(String),

    /// Try to consume an optional keyword. If consumed, capture the last
    /// expression from the previous branch into a new branch.
    OptionalKeywordWithCapture(String),

    /// Parse expressions until one of the terminator keywords is seen.
    /// Does not consume the terminator.
    ParseUntil(Vec<String>),

    /// Parse all remaining expressions.
    ParseToEnd,

    /// Declare a local binding. Consumes one token (the variable name).
    Binding { name: String, typ: ConcreteType },

    /// Execute parsers in sequence.
    Sequence(Vec<Parser>),

    /// Repeat inner parser until terminator is seen.
    Repeat {
        body: Box<Parser>,
        terminator: String,
    },

    /// Try alternatives in order until one succeeds.
    Alternation(Vec<Parser>),
}

/// Mutable state during parsing.
struct ParseState<'a, 'b> {
    ctx: &'a mut ParseContext<'b>,
    branches: Vec<Vec<Node>>,
    in_scope: bool,
    construct: String,
    start_span: Span,
}

impl<'a, 'b> ParseState<'a, 'b> {
    fn new(ctx: &'a mut ParseContext<'b>, construct: &str) -> Self {
        let start_span = ctx.current_span();
        Self {
            ctx,
            branches: Vec::new(),
            in_scope: false,
            construct: construct.to_string(),
            start_span,
        }
    }

    fn error_expected(&self, expected: &str, found: &str) -> ParseError {
        ParseError {
            message: format!("{}: expected {}, found {}", self.construct, expected, found),
            span: self.start_span,
            expected: Some(expected.to_string()),
            found: Some(found.to_string()),
        }
    }
}

// ============================================================================
// Parser Execution (Single Implementation)
// ============================================================================

impl Parser {
    /// Execute this parser, modifying state.
    fn execute(&self, state: &mut ParseState) -> Result<(), ParseError> {
        match self {
            Parser::Keyword(kw) => {
                let token = state.ctx.peek().ok_or_else(|| {
                    state.error_expected(kw, "end of input")
                })?;

                if !token.text.eq_ignore_ascii_case(kw) {
                    return Err(ParseError {
                        message: format!("{}: expected {}, found '{}'", state.construct, kw, token.text),
                        span: token.span,
                        expected: Some(kw.clone()),
                        found: Some(token.text.clone()),
                    });
                }

                state.ctx.advance();
                Ok(())
            }

            Parser::OptionalKeyword(kw) => {
                if let Some(token) = state.ctx.peek()
                    && token.text.eq_ignore_ascii_case(kw)
                {
                    state.ctx.advance();
                }
                Ok(())
            }

            Parser::ParseUntil(terminators) => {
                let mut exprs = Vec::new();

                loop {
                    let token = state.ctx.peek().ok_or_else(|| {
                        let expected = if terminators.len() == 1 {
                            terminators[0].clone()
                        } else {
                            format!("one of: {}", terminators.join(", "))
                        };
                        state.error_expected(&expected, "end of input")
                    })?;

                    // Check if we hit any terminator
                    if terminators.iter().any(|t| token.text.eq_ignore_ascii_case(t)) {
                        break;
                    }

                    exprs.push(state.ctx.parse_one()?);
                }

                state.branches.push(exprs);
                Ok(())
            }

            Parser::ParseToEnd => {
                let mut exprs = Vec::new();
                while state.ctx.peek().is_some() {
                    exprs.push(state.ctx.parse_one()?);
                }
                state.branches.push(exprs);
                Ok(())
            }

            Parser::Binding { name, typ: _ } => {
                if !state.in_scope {
                    state.ctx.enter_scope();
                    state.in_scope = true;
                }

                let token = state.ctx.peek().ok_or_else(|| {
                    state.error_expected(&format!("variable name for '{}'", name), "end of input")
                })?;

                let var_name = token.text.clone();
                let var_span = token.span;
                state.ctx.advance();

                let interned = state.ctx.interner.intern(&var_name);
                let var_index = state.ctx.declare_local(interned);

                state.branches.push(vec![
                    Node::integer(var_index as i64, var_span),
                    Node::string(var_name, var_span),
                ]);

                Ok(())
            }

            Parser::Sequence(parsers) => {
                for parser in parsers {
                    parser.execute(state)?;
                }
                Ok(())
            }

            Parser::Repeat { body, terminator } => {
                // Find the first required keyword in body for lookahead
                let first_required_kw = body.first_required_keyword();

                while let Some(tok) = state.ctx.peek() {
                    // Stop if we see the terminator
                    if tok.text.eq_ignore_ascii_case(terminator) {
                        break;
                    }

                    // Lookahead: if inner pattern has a required keyword,
                    // check it appears before terminator
                    if let Some(kw) = &first_required_kw
                        && !keyword_appears_before(state.ctx, kw, terminator)
                    {
                        break;
                    }

                    // Save state for potential rollback
                    let saved_branch_count = state.branches.len();

                    // Execute body (uses recursive call - single implementation!)
                    let result = body.execute(state);

                    // Check if we should rollback (hit terminator during parsing)
                    if let Some(tok) = state.ctx.peek()
                        && tok.text.eq_ignore_ascii_case(terminator)
                        && result.is_err()
                    {
                        state.branches.truncate(saved_branch_count);
                        break;
                    }

                    result?;
                }

                Ok(())
            }

            Parser::OptionalKeywordWithCapture(kw) => {
                // Try to consume the keyword and capture if consumed
                if let Some(token) = state.ctx.peek()
                    && token.text.eq_ignore_ascii_case(kw)
                {
                    state.ctx.advance();
                    // Only capture if we consumed the keyword
                    if let Some(last_branch) = state.branches.last_mut()
                        && let Some(expr) = last_branch.pop()
                    {
                        state.branches.push(vec![expr]);
                    }
                }
                Ok(())
            }

            Parser::KeywordWithCapture(kw) => {
                // Required keyword that captures from previous branch
                let token = state.ctx.peek().ok_or_else(|| {
                    state.error_expected(kw, "end of input")
                })?;

                if !token.text.eq_ignore_ascii_case(kw) {
                    return Err(ParseError {
                        message: format!("{}: expected {}, found '{}'", state.construct, kw, token.text),
                        span: token.span,
                        expected: Some(kw.clone()),
                        found: Some(token.text.clone()),
                    });
                }

                state.ctx.advance();
                // Capture from previous branch
                if let Some(last_branch) = state.branches.last_mut()
                    && let Some(expr) = last_branch.pop()
                {
                    state.branches.push(vec![expr]);
                }
                Ok(())
            }

            Parser::Alternation(alternatives) => {
                // Try each alternative in order
                let token = state.ctx.peek().ok_or_else(|| {
                    state.error_expected("one of the alternatives", "end of input")
                })?;

                for alt in alternatives {
                    // Check if this alternative could match
                    if let Some(first_kw) = alt.first_required_keyword()
                        && token.text.eq_ignore_ascii_case(&first_kw)
                    {
                        return alt.execute(state);
                    }
                }

                // No alternative matched
                let expected: Vec<String> = alternatives
                    .iter()
                    .filter_map(|a| a.first_required_keyword())
                    .collect();
                Err(ParseError {
                    message: format!(
                        "{}: expected one of {}, found '{}'",
                        state.construct,
                        expected.join(" or "),
                        token.text
                    ),
                    span: token.span,
                    expected: Some(expected.join(" or ")),
                    found: Some(token.text.clone()),
                })
            }
        }
    }

    /// Find the first required keyword in this parser (for lookahead).
    fn first_required_keyword(&self) -> Option<String> {
        match self {
            Parser::Keyword(kw) | Parser::KeywordWithCapture(kw) => Some(kw.clone()),
            Parser::Sequence(parsers) => {
                for p in parsers {
                    if let Some(kw) = p.first_required_keyword() {
                        return Some(kw);
                    }
                }
                None
            }
            Parser::Alternation(alts) => {
                // Return the first keyword from the first alternative
                alts.first().and_then(|a| a.first_required_keyword())
            }
            _ => None,
        }
    }
}

/// Check if a keyword appears before a terminator in the token stream.
fn keyword_appears_before(ctx: &ParseContext, keyword: &str, terminator: &str) -> bool {
    let mut offset = 0;
    while let Some(tok) = ctx.peek_at(offset) {
        if tok.text.eq_ignore_ascii_case(keyword) {
            return true;
        }
        if tok.text.eq_ignore_ascii_case(terminator) {
            return false;
        }
        offset += 1;
    }
    false
}

// ============================================================================
// Pattern Compilation
// ============================================================================

/// Compile pattern elements into a Parser.
fn compile_pattern(slots: &[PatternElement]) -> Parser {
    compile_pattern_with_context(slots, &[])
}

/// Compile pattern elements with outer terminator context.
fn compile_pattern_with_context(slots: &[PatternElement], outer_terminators: &[String]) -> Parser {
    let mut parsers = Vec::new();

    for (i, element) in slots.iter().enumerate() {
        match element {
            PatternElement::Keyword(kw) => {
                parsers.push(Parser::Keyword(kw.clone()));
            }

            PatternElement::OptionalKeyword(kw) => {
                parsers.push(Parser::OptionalKeyword(kw.clone()));
            }

            PatternElement::OptionalKeywordWithCapture(kw) => {
                parsers.push(Parser::OptionalKeywordWithCapture(kw.clone()));
            }

            PatternElement::KeywordWithCapture(kw) => {
                parsers.push(Parser::KeywordWithCapture(kw.clone()));
            }

            PatternElement::Alternation(alternatives) => {
                let alt_parsers: Vec<Parser> = alternatives
                    .iter()
                    .map(|alt| compile_pattern_with_context(alt, outer_terminators))
                    .collect();
                parsers.push(Parser::Alternation(alt_parsers));
            }

            PatternElement::Slot { name: _, typ: _ } => {
                let mut terminators = find_terminators(&slots[i + 1..]);

                // Add outer terminators for slots inside repeats
                for outer in outer_terminators {
                    if !terminators.contains(outer) {
                        terminators.push(outer.clone());
                    }
                }

                if terminators.is_empty() {
                    parsers.push(Parser::ParseToEnd);
                } else {
                    parsers.push(Parser::ParseUntil(terminators));
                }
            }

            PatternElement::Binding { name, typ } => {
                parsers.push(Parser::Binding {
                    name: name.clone(),
                    typ: *typ,
                });
            }

            PatternElement::Repeat(inner_elements) => {
                let terminator = find_terminators(&slots[i + 1..])
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| "END".to_string());

                let inner_parser = compile_pattern_with_context(
                    inner_elements,
                    std::slice::from_ref(&terminator),
                );

                parsers.push(Parser::Repeat {
                    body: Box::new(inner_parser),
                    terminator,
                });
            }
        }
    }

    if parsers.len() == 1 {
        parsers.pop().unwrap()
    } else {
        Parser::Sequence(parsers)
    }
}

/// Find keywords that could terminate a slot.
fn find_terminators(remaining: &[PatternElement]) -> Vec<String> {
    let mut terminators = Vec::new();

    for element in remaining {
        match element {
            PatternElement::Keyword(kw) | PatternElement::KeywordWithCapture(kw) => {
                terminators.push(kw.clone());
                break; // Required keyword ends search
            }
            PatternElement::OptionalKeyword(kw)
            | PatternElement::OptionalKeywordWithCapture(kw) => {
                terminators.push(kw.clone());
                // Continue - might be more optional or required
            }
            PatternElement::Slot { .. } | PatternElement::Binding { .. } => {
                // Continue searching for keywords
            }
            PatternElement::Repeat(inner) => {
                terminators.extend(find_terminators(inner));
                // Continue searching after repeat
            }
            PatternElement::Alternation(alts) => {
                // Add first keyword from each alternative
                for alt in alts {
                    terminators.extend(find_terminators(alt));
                }
                break; // Alternation is required, ends search
            }
        }
    }

    terminators
}

// ============================================================================
// Validation
// ============================================================================

/// Validation error for pattern declarations.
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub message: String,
    pub construct: String,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.construct, self.message)
    }
}

fn validate_pattern(construct: &str, slots: &[PatternElement]) -> Result<(), ValidationError> {
    // Check: slot without terminating keyword
    for (i, element) in slots.iter().enumerate() {
        if let PatternElement::Slot { name, .. } = element {
            let has_terminator = slots[i + 1..].iter().any(|e| {
                matches!(
                    e,
                    PatternElement::Keyword(_)
                        | PatternElement::OptionalKeyword(_)
                        | PatternElement::KeywordWithCapture(_)
                        | PatternElement::OptionalKeywordWithCapture(_)
                        | PatternElement::Alternation(_)
                )
            });
            if !has_terminator && i + 1 < slots.len() {
                return Err(ValidationError {
                    message: format!(
                        "slot '{}' has no terminating keyword before next element",
                        name
                    ),
                    construct: construct.to_string(),
                });
            }
        }
    }

    // Check: pattern must end with a keyword or alternation
    if let Some(last) = slots.last()
        && !matches!(
            last,
            PatternElement::Keyword(_)
                | PatternElement::OptionalKeyword(_)
                | PatternElement::KeywordWithCapture(_)
                | PatternElement::OptionalKeywordWithCapture(_)
                | PatternElement::Alternation(_)
        )
    {
        return Err(ValidationError {
            message: "pattern must end with a keyword or alternation".to_string(),
            construct: construct.to_string(),
        });
    }

    // Check: consecutive slots without keywords
    let mut prev_was_expr_slot = false;
    for element in slots {
        let is_expr_slot = matches!(element, PatternElement::Slot { .. });
        if is_expr_slot && prev_was_expr_slot {
            return Err(ValidationError {
                message: "consecutive slots without keyword separator".to_string(),
                construct: construct.to_string(),
            });
        }
        prev_was_expr_slot = is_expr_slot;
    }

    Ok(())
}

// ============================================================================
// Binding Analysis
// ============================================================================

/// Describes what kind of slot produces a branch.
#[derive(Debug, Clone, Copy, PartialEq)]
enum SlotKind {
    /// A binding slot ($name:Type) - produces [index, name] branch
    Binding,
    /// An expression slot (name:Type) - produces [nodes...] branch
    Expression,
}

/// Describes the pattern of slots in a parser.
#[derive(Debug, Clone)]
enum SlotPattern {
    /// A single slot
    Single(SlotKind),
    /// A repeated slot (produces variable number of branches)
    Repeat(SlotKind),
}

/// Analyze a Parser to extract its slot pattern (what branches it produces).
fn analyze_parser_slots(parser: &Parser) -> Vec<SlotPattern> {
    let mut pattern = Vec::new();
    collect_slots(parser, &mut pattern);
    pattern
}

/// Recursively collect slot patterns from a parser.
fn collect_slots(parser: &Parser, pattern: &mut Vec<SlotPattern>) {
    match parser {
        Parser::Binding { .. } => {
            pattern.push(SlotPattern::Single(SlotKind::Binding));
        }
        Parser::ParseUntil(_) | Parser::ParseToEnd => {
            pattern.push(SlotPattern::Single(SlotKind::Expression));
        }
        Parser::Sequence(parsers) => {
            for p in parsers {
                collect_slots(p, pattern);
            }
        }
        Parser::Repeat { body, .. } => {
            // Check if the body contains a binding
            let mut inner = Vec::new();
            collect_slots(body, &mut inner);

            // If the repeat body has bindings, it's a binding repeat
            let has_binding = inner.iter().any(|s| matches!(s,
                SlotPattern::Single(SlotKind::Binding) |
                SlotPattern::Repeat(SlotKind::Binding)
            ));

            if has_binding {
                pattern.push(SlotPattern::Repeat(SlotKind::Binding));
            } else if !inner.is_empty() {
                // Expression repeat
                pattern.push(SlotPattern::Repeat(SlotKind::Expression));
            }
        }
        Parser::Alternation(alts) => {
            // For alternation, check first alternative (they should be similar)
            if let Some(first) = alts.first() {
                collect_slots(first, pattern);
            }
        }
        // Keywords don't produce branches
        Parser::Keyword(_)
        | Parser::OptionalKeyword(_)
        | Parser::KeywordWithCapture(_)
        | Parser::OptionalKeywordWithCapture(_) => {}
    }
}

/// Given a slot pattern and actual branch count, compute which branches are bindings.
fn compute_binding_indices(pattern: &[SlotPattern], num_branches: usize) -> Vec<usize> {
    // Find binding repeat position if any
    let mut has_binding_repeat = false;
    let mut binding_repeat_position = 0;

    for (i, slot) in pattern.iter().enumerate() {
        match slot {
            SlotPattern::Single(_) => {}
            SlotPattern::Repeat(SlotKind::Binding) => {
                has_binding_repeat = true;
                binding_repeat_position = i;
            }
            SlotPattern::Repeat(SlotKind::Expression) => {
                // Expression repeat - count as variable but not bindings
            }
        }
    }

    let mut result = Vec::new();

    if has_binding_repeat {
        // Count slots before and after the repeat
        let slots_before: usize = pattern[..binding_repeat_position]
            .iter()
            .filter(|s| matches!(s, SlotPattern::Single(_)))
            .count();
        let slots_after: usize = pattern[binding_repeat_position + 1..]
            .iter()
            .filter(|s| matches!(s, SlotPattern::Single(_)))
            .count();

        // Binding repeat produces (num_branches - slots_before - slots_after) branches
        let repeat_count = num_branches.saturating_sub(slots_before + slots_after);

        // Add indices for slots before repeat that are bindings
        let mut idx = 0;
        for slot in &pattern[..binding_repeat_position] {
            if let SlotPattern::Single(SlotKind::Binding) = slot {
                result.push(idx);
            }
            if matches!(slot, SlotPattern::Single(_)) {
                idx += 1;
            }
        }

        // Add indices for the binding repeat
        for i in 0..repeat_count {
            result.push(slots_before + i);
        }

        // Add indices for slots after repeat that are bindings
        idx = slots_before + repeat_count;
        for slot in &pattern[binding_repeat_position + 1..] {
            if let SlotPattern::Single(SlotKind::Binding) = slot {
                result.push(idx);
            }
            if matches!(slot, SlotPattern::Single(_)) {
                idx += 1;
            }
        }
    } else {
        // No repeat - just check each single slot
        for (i, slot) in pattern.iter().enumerate() {
            if let SlotPattern::Single(SlotKind::Binding) = slot {
                result.push(i);
            }
        }
    }

    result
}

// ============================================================================
// Effect Inference
// ============================================================================

fn infer_effect_kind(decl: &Declaration) -> EffectKind {
    // Dynamic if any input/output is dynamic
    if has_dynamic(&decl.inputs) || has_dynamic(&decl.outputs) {
        return EffectKind::Static(StackEffect::Dynamic);
    }

    // Binary numeric: `a b -> Numeric a b`
    if decl.inputs.len() == 2
        && decl.outputs.len() == 1
        && matches!(&decl.outputs[0], Type::Numeric(_, _))
    {
        return EffectKind::BinaryNumeric;
    }

    // Unary preserving: `a -> a`
    if decl.inputs.len() == 1
        && decl.outputs.len() == 1
        && let (Type::Var(a), Type::Var(b)) = (&decl.inputs[0], &decl.outputs[0])
        && a == b
    {
        return EffectKind::UnaryPreserving;
    }

    // Permutation: all outputs are input vars
    if let Some(effect) = try_permutation(&decl.inputs, &decl.outputs) {
        return EffectKind::Static(effect);
    }

    // Fixed effect
    let consumes = decl.inputs.len() as u8;
    let results: Vec<Option<TypeId>> = decl.outputs.iter().map(type_to_type_id).collect();
    EffectKind::Static(StackEffect::fixed(consumes, &results))
}

fn has_dynamic(types: &[Type]) -> bool {
    types.iter().any(|t| matches!(t, Type::Dynamic))
}

fn try_permutation(inputs: &[Type], outputs: &[Type]) -> Option<StackEffect> {
    let input_vars: Vec<char> = inputs
        .iter()
        .map(|t| match t {
            Type::Var(c) => Some(*c),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    let pattern: Vec<u8> = outputs
        .iter()
        .map(|t| match t {
            Type::Var(c) => input_vars.iter().position(|v| v == c).map(|i| i as u8),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    Some(StackEffect::permutation(inputs.len() as u8, &pattern))
}

fn type_to_type_id(typ: &Type) -> Option<TypeId> {
    match typ {
        Type::Concrete(ct) => match ct {
            ConcreteType::Int => Some(TypeId::BINT),
            ConcreteType::Real => Some(TypeId::REAL),
            ConcreteType::Str => Some(TypeId::STRING),
            ConcreteType::List => Some(TypeId::LIST),
            ConcreteType::Prog => Some(TypeId::PROGRAM),
            ConcreteType::Sym => Some(TypeId::SYMBOLIC),
            ConcreteType::Blob => Some(TypeId::BLOB),
            ConcreteType::Any => None,
        },
        _ => None,
    }
}

// ============================================================================
// InterfaceSpec Implementation
// ============================================================================

impl InterfaceSpec {
    /// Create a runtime library from a DSL string.
    pub fn from_dsl(dsl: &str) -> Result<Self, InterfaceParseError> {
        let ast = parse(dsl)?;
        Ok(Self::from_ast(&ast))
    }

    /// Create a runtime library from a parsed AST.
    pub fn from_ast(lib: &Library) -> Self {
        Self::from_ast_validated(lib).unwrap_or_else(|errors| {
            for err in &errors {
                eprintln!("Warning: {}", err);
            }
            Self::from_ast_unchecked(lib)
        })
    }

    /// Create a runtime library with validation.
    pub fn from_ast_validated(lib: &Library) -> Result<Self, Vec<ValidationError>> {
        let mut errors = Vec::new();
        let mut commands = Vec::new();
        let mut syntax = Vec::new();

        for decl in &lib.declarations {
            if decl.pattern.slots.is_empty() {
                // Simple command - use explicit ID from declaration
                commands.push(CommandSpec {
                    names: decl.pattern.names.clone(),
                    cmd_id: decl.id,
                    inputs: decl.inputs.clone(),
                    outputs: decl.outputs.clone(),
                    effect_kind: infer_effect_kind(decl),
                });
            } else if let Some(token) = decl.pattern.names.first() {
                // Syntax construct - use explicit ID from declaration
                if let Err(e) = validate_pattern(token, &decl.pattern.slots) {
                    errors.push(e);
                    continue;
                }

                let parser = compile_pattern(&decl.pattern.slots);
                syntax.push(SyntaxDecl {
                    id: decl.id,
                    token: token.clone(),
                    parser,
                });
            }
        }

        if errors.is_empty() {
            Ok(InterfaceSpec {
                name: lib.name.clone(),
                id: lib.id,
                commands,
                syntax,
            })
        } else {
            Err(errors)
        }
    }

    fn from_ast_unchecked(lib: &Library) -> Self {
        let mut commands = Vec::new();
        let mut syntax = Vec::new();

        for decl in &lib.declarations {
            if decl.pattern.slots.is_empty() {
                // Simple command - use explicit ID from declaration
                commands.push(CommandSpec {
                    names: decl.pattern.names.clone(),
                    cmd_id: decl.id,
                    inputs: decl.inputs.clone(),
                    outputs: decl.outputs.clone(),
                    effect_kind: infer_effect_kind(decl),
                });
            } else if let Some(token) = decl.pattern.names.first() {
                // Syntax construct - use explicit ID from declaration
                let parser = compile_pattern(&decl.pattern.slots);
                syntax.push(SyntaxDecl {
                    id: decl.id,
                    token: token.clone(),
                    parser,
                });
            }
        }

        InterfaceSpec {
            name: lib.name.clone(),
            id: lib.id,
            commands,
            syntax,
        }
    }

    // --- Accessors ---

    pub fn id(&self) -> LibId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn static_name(&self) -> &'static str {
        Box::leak(self.name.clone().into_boxed_str())
    }

    pub fn commands(&self) -> &[CommandSpec] {
        &self.commands
    }

    pub fn syntax(&self) -> &[SyntaxDecl] {
        &self.syntax
    }

    /// Get the token claims for this library's syntax constructs.
    pub fn token_claims(&self) -> Vec<TokenClaim> {
        self.syntax
            .iter()
            .map(|s| TokenClaim {
                token: s.token.clone(),
                priority: 100,
                context: ClaimContext::NotInfix,
                lib_id: self.id,
            })
            .collect()
    }

    pub fn find_command(&self, name: &str) -> Option<&CommandSpec> {
        let upper = name.to_uppercase();
        self.commands
            .iter()
            .find(|cmd| cmd.names.iter().any(|n| n.to_uppercase() == upper))
    }

    pub fn to_command_infos(&self) -> Vec<crate::libs::CommandInfo> {
        self.commands
            .iter()
            .flat_map(|cmd| {
                let effect = match &cmd.effect_kind {
                    EffectKind::Static(e) => e.clone(),
                    EffectKind::BinaryNumeric | EffectKind::UnaryPreserving => {
                        StackEffect::fixed(cmd.inputs.len() as u8, &vec![None; cmd.outputs.len()])
                    }
                };
                // Generate a CommandInfo for each alias
                cmd.names.iter().map(move |name| {
                    let name: &'static str = Box::leak(name.clone().into_boxed_str());
                    crate::libs::CommandInfo {
                        name,
                        lib_id: self.id,
                        cmd_id: cmd.cmd_id,
                        effect: effect.clone(),
                    }
                })
            })
            .collect()
    }

}

// ============================================================================
// LibraryInterface Implementation
// ============================================================================

impl LibraryInterface for InterfaceSpec {
    fn id(&self) -> LibId {
        self.id
    }

    fn name(&self) -> &'static str {
        self.static_name()
    }

    fn commands(&self) -> Vec<CommandInfo> {
        self.to_command_infos()
    }

    fn claims(&self) -> Vec<TokenClaim> {
        self.token_claims()
    }

    fn command_effect(&self, cmd: u16, types: &CStack) -> StackEffect {
        let cmd = match self.commands.iter().find(|c| c.cmd_id == cmd) {
            Some(c) => c,
            None => return StackEffect::Dynamic,
        };

        match &cmd.effect_kind {
            EffectKind::Static(effect) => effect.clone(),
            EffectKind::BinaryNumeric => crate::libs::binary_numeric_effect(types),
            EffectKind::UnaryPreserving => crate::libs::unary_preserving_effect(types),
        }
    }

    fn parse(&self, token: &str, ctx: &mut ParseContext) -> Result<Node, ParseError> {
        let upper = token.to_uppercase();
        let decl = self
            .syntax
            .iter()
            .find(|s| s.token.to_uppercase() == upper)
            .ok_or_else(|| ParseError {
                message: format!("{}: no syntax declaration for this token", token),
                span: Span::new(Pos::new(0), Pos::new(0)),
                expected: None,
                found: Some(token.to_string()),
            })?;

        let mut state = ParseState::new(ctx, &decl.token);

        // Execute the compiled parser
        decl.parser.execute(&mut state)?;

        // Clean up scope if needed
        if state.in_scope {
            state.ctx.exit_scope();
        }

        let end_span = state.ctx.current_span();
        let full_span = Span::new(state.start_span.start(), end_span.end());

        Ok(Node::extended(
            self.id,
            decl.id,
            state.branches,
            full_span,
        ))
    }

    fn binding_branches(&self, construct_id: u16, num_branches: usize) -> Vec<usize> {
        let decl = match self.syntax.iter().find(|s| s.id == construct_id) {
            Some(d) => d,
            None => return Vec::new(),
        };

        // Analyze the parser to get the slot pattern
        let pattern = analyze_parser_slots(&decl.parser);

        // Compute binding indices from the pattern
        compute_binding_indices(&pattern, num_branches)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{core::Interner, interface::parse, ir::NodeKind, parse::ParseContext, registry::Registry};

    #[test]
    fn test_from_ast_simple() {
        let input = r#"
library Test 1

0: a -> DUP -> a a
1: a -> DROP ->
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        assert_eq!(rt.name(), "Test");
        assert_eq!(rt.id(), 1);
        assert_eq!(rt.commands().len(), 2);

        let dup = rt.find_command("DUP").unwrap();
        assert_eq!(dup.cmd_id, 0);

        let drop = rt.find_command("DROP").unwrap();
        assert_eq!(drop.cmd_id, 1);
    }

    #[test]
    fn test_permutation_inference() {
        let input = r#"
library Stack 1

0: a -> DUP -> a a
1: a b -> SWAP -> b a
2: a b c -> ROT -> b c a
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let dup = rt.find_command("DUP").unwrap();
        assert_eq!(
            dup.effect_kind,
            EffectKind::Static(StackEffect::permutation(1, &[0, 0]))
        );

        let swap = rt.find_command("SWAP").unwrap();
        assert_eq!(
            swap.effect_kind,
            EffectKind::Static(StackEffect::permutation(2, &[1, 0]))
        );

        let rot = rt.find_command("ROT").unwrap();
        assert_eq!(
            rot.effect_kind,
            EffectKind::Static(StackEffect::permutation(3, &[1, 2, 0]))
        );
    }

    #[test]
    fn test_fixed_inference() {
        let input = r#"
library Arith 2

0: Int Int -> IADD -> Int
1: a b -> CMP -> Int
2: -> PI -> Real
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let iadd = rt.find_command("IADD").unwrap();
        assert_eq!(
            iadd.effect_kind,
            EffectKind::Static(StackEffect::fixed(2, &[Some(TypeId::BINT)]))
        );

        let cmp = rt.find_command("CMP").unwrap();
        assert_eq!(
            cmp.effect_kind,
            EffectKind::Static(StackEffect::fixed(2, &[Some(TypeId::BINT)]))
        );

        let pi = rt.find_command("PI").unwrap();
        assert_eq!(
            pi.effect_kind,
            EffectKind::Static(StackEffect::fixed(0, &[Some(TypeId::REAL)]))
        );
    }

    #[test]
    fn test_binary_numeric_inference() {
        let input = r#"
library Arith 3

0: a b -> (+), ADD -> Numeric a b
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let add = rt.find_command("ADD").unwrap();
        assert_eq!(add.effect_kind, EffectKind::BinaryNumeric);

        let add2 = rt.find_command("+").unwrap();
        assert_eq!(add2.effect_kind, EffectKind::BinaryNumeric);
    }

    #[test]
    fn test_unary_preserving_inference() {
        let input = r#"
library Arith 4

0: a -> NEG -> a
1: a -> ABS -> a
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let neg = rt.find_command("NEG").unwrap();
        assert_eq!(neg.effect_kind, EffectKind::UnaryPreserving);

        let abs = rt.find_command("ABS").unwrap();
        assert_eq!(abs.effect_kind, EffectKind::UnaryPreserving);
    }

    #[test]
    fn test_dynamic_inference() {
        let input = r#"
library Stack 5

0: Int -> ROLL -> ...
1: ... -> DEPTH -> ... Int
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let roll = rt.find_command("ROLL").unwrap();
        assert_eq!(roll.effect_kind, EffectKind::Static(StackEffect::Dynamic));

        let depth = rt.find_command("DEPTH").unwrap();
        assert_eq!(depth.effect_kind, EffectKind::Static(StackEffect::Dynamic));
    }

    #[test]
    fn test_syntax_claims() {
        let input = r#"
library Flow 6

10: -> IF cond:Int THEN body:Prog END ->
20: -> FOR $name:Sym body:Prog NEXT ->
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        assert_eq!(rt.commands().len(), 0);
        assert_eq!(rt.syntax().len(), 2);

        let claims = rt.token_claims();
        let tokens: Vec<_> = claims.iter().map(|c| c.token.as_str()).collect();
        assert!(tokens.contains(&"IF"));
        assert!(tokens.contains(&"FOR"));
        assert!(!tokens.contains(&"WHILE"));
    }

    #[test]
    fn test_command_effect_with_types() {
        let input = r#"
library Arith 7

0: a b -> (+), ADD -> Numeric a b
"#;
        let ast = parse(input).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let add = rt.find_command("ADD").unwrap();

        let mut int_stack = CStack::default();
        int_stack.push_known(TypeId::BINT);
        int_stack.push_known(TypeId::BINT);
        let effect = rt.command_effect(add.cmd_id, &int_stack);
        assert_eq!(effect, StackEffect::fixed(2, &[Some(TypeId::BINT)]));

        let mut mixed_stack = CStack::default();
        mixed_stack.push_known(TypeId::BINT);
        mixed_stack.push_known(TypeId::REAL);
        let effect = rt.command_effect(add.cmd_id, &mixed_stack);
        assert_eq!(effect, StackEffect::fixed(2, &[Some(TypeId::REAL)]));
    }

    #[test]
    fn test_parse_novel_unless_do_end() {
        let interface = r#"
library Novel 100

50: -> UNLESS cond:Int DO body:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        assert_eq!(rt.syntax.len(), 1);
        let unless = &rt.syntax[0];
        assert_eq!(unless.token, "UNLESS");
        assert_eq!(unless.id, 50);

        let code = "1 0 == DO 42 END";
        let registry = Registry::new();
        let mut interner = Interner::new();
        let mut ctx = ParseContext::new(code, &registry, &mut interner);

        let node = rt.parse("UNLESS", &mut ctx).unwrap();

        if let NodeKind::Composite(kind, branches) = &node.kind {
            if let crate::ir::CompositeKind::Extended(lib_id, construct_id) = kind {
                assert_eq!(*lib_id, 100);
                assert_eq!(*construct_id, 50);
            } else {
                panic!("Expected Extended composite");
            }
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].len(), 3); // [1, 0, ==]
            assert_eq!(branches[1].len(), 1); // [42]
        } else {
            panic!("Expected Composite node, got {:?}", node.kind);
        }
    }

    #[test]
    fn test_parse_novel_repeat_times() {
        let interface = r#"
library Novel 101

60: -> REPEAT $counter:Sym limit:Int TIMES body:Prog DONE ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let repeat = &rt.syntax[0];
        assert_eq!(repeat.token, "REPEAT");
        assert_eq!(repeat.id, 60);

        let code = "i 10 TIMES i 2 * DONE";
        let registry = Registry::new();
        let mut interner = Interner::new();
        let mut ctx = ParseContext::new(code, &registry, &mut interner);

        let node = rt.parse("REPEAT", &mut ctx).unwrap();

        if let NodeKind::Composite(kind, branches) = &node.kind {
            if let crate::ir::CompositeKind::Extended(lib_id, construct_id) = kind {
                assert_eq!(*lib_id, 101);
                assert_eq!(*construct_id, 60);
            } else {
                panic!("Expected Extended composite");
            }
            assert_eq!(branches.len(), 3);
            assert_eq!(branches[0].len(), 2); // binding info
            assert_eq!(branches[1].len(), 1); // limit [10]
            assert_eq!(branches[2].len(), 3); // body [i, 2, *]
        } else {
            panic!("Expected Composite node");
        }
    }

    #[test]
    fn test_parse_novel_cond_when() {
        let interface = r#"
library Novel 102

70: -> COND first:Prog WHEN second:Prog OTHERWISE default:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let code = "1 WHEN 2 OTHERWISE 3 END";
        let registry = Registry::new();
        let mut interner = Interner::new();
        let mut ctx = ParseContext::new(code, &registry, &mut interner);

        let node = rt.parse("COND", &mut ctx).unwrap();

        if let NodeKind::Composite(_, branches) = &node.kind {
            assert_eq!(branches.len(), 3);
            assert_eq!(branches[0].len(), 1);
            assert_eq!(branches[1].len(), 1);
            assert_eq!(branches[2].len(), 1);
        } else {
            panic!("Expected Composite node");
        }
    }

    #[test]
    fn test_optional_keyword() {
        let interface = r#"
library Novel 103

80: -> IF cond:Int THEN body:Prog ELSE? alt:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let if_syntax = &rt.syntax[0];
        assert_eq!(if_syntax.token, "IF");
    }

    #[test]
    fn test_validation_catches_errors() {
        let bad_interface = r#"
library Bad 999

0: -> BAD cond:Int body:Prog END ->
"#;
        let ast = parse(bad_interface).unwrap();
        let result = InterfaceSpec::from_ast_validated(&ast);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("consecutive slots"));
    }

    #[test]
    fn test_validation_catches_missing_terminator() {
        let bad_interface = r#"
library Bad 998

0: -> BAD THEN body:Prog other:Int ->
"#;
        let ast = parse(bad_interface).unwrap();
        let result = InterfaceSpec::from_ast_validated(&ast);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("no terminating keyword"));
    }

    #[test]
    fn test_validation_catches_no_ending_keyword() {
        let bad_interface = r#"
library Bad 997

0: -> BAD body:Prog ->
"#;
        let ast = parse(bad_interface).unwrap();
        let result = InterfaceSpec::from_ast_validated(&ast);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("must end with a keyword"));
    }

    #[test]
    fn test_validation_allows_good_patterns() {
        let good_interface = r#"
library Good 998

0: -> GOOD cond:Int THEN body:Prog END ->
"#;
        let ast = parse(good_interface).unwrap();
        let result = InterfaceSpec::from_ast_validated(&ast);
        assert!(result.is_ok());
    }

    // Additional v2 tests

    #[test]
    fn test_parser_combinator_structure() {
        let interface = r#"
library Test 1

0: -> IF cond:Int THEN body:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        // Check that we get a Sequence parser
        let if_syntax = &rt.syntax[0];
        assert!(matches!(&if_syntax.parser, Parser::Sequence(_)));

        if let Parser::Sequence(parsers) = &if_syntax.parser {
            assert_eq!(parsers.len(), 4);
            assert!(matches!(&parsers[0], Parser::ParseUntil(_)));
            assert!(matches!(&parsers[1], Parser::Keyword(k) if k == "THEN"));
            assert!(matches!(&parsers[2], Parser::ParseUntil(_)));
            assert!(matches!(&parsers[3], Parser::Keyword(k) if k == "END"));
        }
    }

    #[test]
    fn test_repeat_parser() {
        let interface = r#"
library Test 1

0: -> CASE ( cond:Prog THEN body:Prog END )* default:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let case_syntax = &rt.syntax[0];
        if let Parser::Sequence(parsers) = &case_syntax.parser {
            // Should have: Repeat, ParseUntil(END), Keyword(END)
            assert!(matches!(&parsers[0], Parser::Repeat { .. }));
        }
    }

    #[test]
    fn test_optional_keyword_with_capture() {
        let interface = r#"
library Test 1

0: -> LOOP body:Prog STEP?! END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let loop_syntax = &rt.syntax[0];
        if let Parser::Sequence(parsers) = &loop_syntax.parser {
            // Should have: ParseUntil, OptionalKeywordWithCapture(STEP), Keyword(END)
            let has_capture = parsers.iter().any(|p| {
                matches!(p, Parser::OptionalKeywordWithCapture(kw) if kw == "STEP")
            });
            assert!(has_capture, "Expected OptionalKeywordWithCapture for STEP");
        }
    }

    // --- Binding Analysis Tests ---

    #[test]
    fn test_binding_branches_single_binding() {
        // FOR has a single binding: $name:Sym
        let interface = r#"
library Test 1

10: Int Int -> FOR $name:Sym body:Prog NEXT ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        // FOR with 2 branches: [binding, body]
        let indices = rt.binding_branches(10, 2);
        assert_eq!(indices, vec![0], "FOR should have binding at index 0");
    }

    #[test]
    fn test_binding_branches_repeat() {
        // Locals has repeat binding: ( $name:Sym )*
        let interface = r#"
library Test 1

20: -> BIND ( $name:Sym )* DO body:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        // With 3 bindings + 1 body = 4 branches
        let indices = rt.binding_branches(20, 4);
        assert_eq!(indices, vec![0, 1, 2], "Should have 3 binding branches");

        // With 1 binding + 1 body = 2 branches
        let indices = rt.binding_branches(20, 2);
        assert_eq!(indices, vec![0], "Should have 1 binding branch");
    }

    #[test]
    fn test_binding_branches_no_bindings() {
        // IF has no bindings, just expression slots
        let interface = r#"
library Test 1

30: -> IF cond:Int THEN body:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let indices = rt.binding_branches(30, 2);
        assert!(indices.is_empty(), "IF should have no binding branches");
    }

    #[test]
    fn test_binding_branches_unknown_construct() {
        let interface = r#"
library Test 1

30: -> IF cond:Int THEN body:Prog END ->
"#;
        let ast = parse(interface).unwrap();
        let rt = InterfaceSpec::from_ast(&ast);

        let indices = rt.binding_branches(999, 5);
        assert!(indices.is_empty(), "Unknown construct should return empty");
    }
}
