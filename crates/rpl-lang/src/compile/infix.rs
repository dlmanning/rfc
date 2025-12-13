use rpl_core::Span;
use crate::operator::OperatorKind;
use crate::dispatch_impl::{DISPATCH_LIB, encode_dispatch};

use super::output::OutputBuffer;

/// Entry for an operator on the operator stack.
#[derive(Clone, Debug)]
pub struct OperatorEntry {
    pub span: Span,
    pub op_kind: OperatorKind,
    pub precedence: u8,
    pub arity: u8,
    pub right_assoc: bool,
}

/// Entry for parentheses/function calls on the paren stack.
#[derive(Clone, Debug)]
struct ParenEntry {
    /// How many operators were on the stack when this paren was opened.
    operator_stack_depth: usize,
    /// Whether this is a function call (has a function to emit after args).
    is_function: bool,
    /// The function entry if this is a function call.
    function: Option<OperatorEntry>,
    /// Number of arguments seen so far.
    arg_count: usize,
}

/// Shunting-yard parser for infix expressions.
pub struct InfixParser {
    /// Stack of pending operators.
    operator_stack: Vec<OperatorEntry>,
    /// Stack of parentheses/function calls.
    paren_stack: Vec<ParenEntry>,
    /// Position where infix mode started.
    start_pos: usize,
}

impl InfixParser {
    /// Create a new infix parser.
    pub fn new(start_pos: usize) -> Self {
        Self {
            operator_stack: Vec::new(),
            paren_stack: Vec::new(),
            start_pos,
        }
    }

    /// Get the start position of the infix expression.
    pub fn start_pos(&self) -> usize {
        self.start_pos
    }

    /// Called when an atom (number, variable) is encountered.
    /// The atom has already been emitted to output.
    pub fn push_atom(&mut self) {
        // No action needed - atoms go directly to output
    }

    /// Called when an operator is encountered.
    /// Handles precedence and associativity.
    pub fn push_operator(&mut self, entry: OperatorEntry, output: &mut OutputBuffer) {
        // Pop operators with higher precedence (or equal for left-assoc)
        while let Some(top) = self.operator_stack.last() {
            // Don't pop past a paren boundary
            if let Some(paren) = self.paren_stack.last()
                && self.operator_stack.len() <= paren.operator_stack_depth
            {
                break;
            }

            let should_pop = if entry.right_assoc {
                // Right associative: pop only higher precedence
                top.precedence > entry.precedence
            } else {
                // Left associative: pop higher or equal precedence
                top.precedence >= entry.precedence
            };

            if should_pop {
                let op = self.operator_stack.pop().unwrap();
                self.emit_operator(&op, output);
            } else {
                break;
            }
        }

        self.operator_stack.push(entry);
    }

    /// Called when an open bracket '(' is encountered.
    pub fn push_open_bracket(&mut self, _span: Span) {
        self.paren_stack.push(ParenEntry {
            operator_stack_depth: self.operator_stack.len(),
            is_function: false,
            function: None,
            arg_count: 1, // Opening a paren implies at least one arg
        });
    }

    /// Called when a function followed by '(' is encountered.
    /// The function itself hasn't been emitted yet.
    pub fn push_function(&mut self, entry: OperatorEntry, _span: Span) {
        self.paren_stack.push(ParenEntry {
            operator_stack_depth: self.operator_stack.len(),
            is_function: true,
            function: Some(entry),
            arg_count: 0, // Will be incremented as we see args
        });
    }

    /// Called when a comma is encountered inside parentheses.
    pub fn push_comma(&mut self, output: &mut OutputBuffer) -> Result<(), InfixError> {
        // Pop all operators down to the paren boundary
        let paren_depth = self
            .paren_stack
            .last()
            .map(|p| p.operator_stack_depth)
            .ok_or(InfixError::UnexpectedComma)?;

        while self.operator_stack.len() > paren_depth {
            let op = self.operator_stack.pop().unwrap();
            self.emit_operator(&op, output);
        }

        // Now increment arg count
        if let Some(paren) = self.paren_stack.last_mut() {
            paren.arg_count += 1;
        }

        Ok(())
    }

    /// Called when a close bracket ')' is encountered.
    pub fn push_close_bracket(&mut self, output: &mut OutputBuffer) -> Result<(), InfixError> {
        let paren = self
            .paren_stack
            .pop()
            .ok_or(InfixError::UnmatchedCloseParen)?;

        // Pop all operators down to the paren boundary
        while self.operator_stack.len() > paren.operator_stack_depth {
            let op = self.operator_stack.pop().unwrap();
            self.emit_operator(&op, output);
        }

        // If this was a function call, emit the function
        if paren.is_function
            && let Some(func) = paren.function
        {
            self.emit_operator(&func, output);
        }

        Ok(())
    }

    /// Finish parsing and emit remaining operators.
    pub fn finish(mut self, output: &mut OutputBuffer) -> Result<(), InfixError> {
        // Check for unclosed parens
        if !self.paren_stack.is_empty() {
            return Err(InfixError::UnclosedParen);
        }

        // Pop all remaining operators
        while let Some(op) = self.operator_stack.pop() {
            self.emit_operator(&op, output);
        }

        Ok(())
    }

    /// Emit an operator to the output using dynamic dispatch.
    fn emit_operator(&self, op: &OperatorEntry, output: &mut OutputBuffer) {
        let encoded = encode_dispatch(op.op_kind, op.arity);
        let opcode = rpl_core::make_call(DISPATCH_LIB, encoded);
        output.emit(opcode, op.span);
    }

    /// Check if currently inside parentheses.
    pub fn in_parens(&self) -> bool {
        !self.paren_stack.is_empty()
    }

    /// Get the current argument count for the innermost function call.
    pub fn current_arg_count(&self) -> Option<usize> {
        self.paren_stack.last().map(|p| p.arg_count)
    }
}

/// Errors during infix parsing.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InfixError {
    /// Unexpected comma outside of parentheses.
    UnexpectedComma,
    /// Close paren without matching open paren.
    UnmatchedCloseParen,
    /// Unclosed parenthesis at end of expression.
    UnclosedParen,
}

impl std::fmt::Display for InfixError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixError::UnexpectedComma => write!(f, "Unexpected comma outside of parentheses"),
            InfixError::UnmatchedCloseParen => {
                write!(f, "Closing parenthesis without matching open")
            }
            InfixError::UnclosedParen => write!(f, "Unclosed parenthesis"),
        }
    }
}

impl std::error::Error for InfixError {}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Pos, Word};

    fn make_op(op_kind: OperatorKind, prec: u8, right_assoc: bool, arity: u8) -> OperatorEntry {
        OperatorEntry {
            span: Span::new(Pos::new(0), Pos::new(1)),
            op_kind,
            precedence: prec,
            arity,
            right_assoc,
        }
    }

    fn extract_opcodes(output: &OutputBuffer) -> Vec<Word> {
        let (code, _) = output.clone().finish();
        code
    }

    fn expected_opcode(op_kind: OperatorKind, arity: u8) -> Word {
        let encoded = encode_dispatch(op_kind, arity);
        rpl_core::make_call(DISPATCH_LIB, encoded)
    }

    #[test]
    fn infix_simple_binary() {
        // 3 + 4 -> push_atom, push_operator(+), push_atom, finish
        // Output should be: opcode for +
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom(); // 3
        let add = make_op(OperatorKind::Add, 10, false, 2);
        parser.push_operator(add, &mut output);
        parser.push_atom(); // 4
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 1); // Just the + operator
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Add, 2));
    }

    #[test]
    fn infix_precedence() {
        // 3 + 4 * 5 -> should output: * then +
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom(); // 3
        let add = make_op(OperatorKind::Add, 10, false, 2);
        parser.push_operator(add, &mut output);
        parser.push_atom(); // 4
        let mul = make_op(OperatorKind::Mul, 20, false, 2); // * higher precedence
        parser.push_operator(mul, &mut output);
        parser.push_atom(); // 5
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 2);
        // * should come first (higher precedence), then +
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Mul, 2));
        assert_eq!(opcodes[1], expected_opcode(OperatorKind::Add, 2));
    }

    #[test]
    fn infix_left_associativity() {
        // 3 - 4 - 5 -> should output: - - (left to right)
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom(); // 3
        let sub1 = make_op(OperatorKind::Sub, 10, false, 2);
        parser.push_operator(sub1, &mut output);
        parser.push_atom(); // 4
        let sub2 = make_op(OperatorKind::Sub, 10, false, 2);
        parser.push_operator(sub2, &mut output);
        parser.push_atom(); // 5
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 2);
        // Both are subtraction
        let sub_opcode = expected_opcode(OperatorKind::Sub, 2);
        assert_eq!(opcodes[0], sub_opcode);
        assert_eq!(opcodes[1], sub_opcode);
    }

    #[test]
    fn infix_right_associativity() {
        // 3 ^ 4 ^ 5 -> should output: ^ ^ (right to left, so 4^5 first)
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom(); // 3
        let pow1 = make_op(OperatorKind::Pow, 30, true, 2); // ^ right-assoc
        parser.push_operator(pow1, &mut output);
        parser.push_atom(); // 4
        let pow2 = make_op(OperatorKind::Pow, 30, true, 2);
        parser.push_operator(pow2, &mut output);
        parser.push_atom(); // 5
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 2);
        // Both are power operations
        let pow_opcode = expected_opcode(OperatorKind::Pow, 2);
        assert_eq!(opcodes[0], pow_opcode);
        assert_eq!(opcodes[1], pow_opcode);
    }

    #[test]
    fn infix_parentheses() {
        // (3 + 4) * 5 -> should output: + then *
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_open_bracket(Span::DUMMY);
        parser.push_atom(); // 3
        let add = make_op(OperatorKind::Add, 10, false, 2);
        parser.push_operator(add, &mut output);
        parser.push_atom(); // 4
        parser.push_close_bracket(&mut output).unwrap();
        let mul = make_op(OperatorKind::Mul, 20, false, 2);
        parser.push_operator(mul, &mut output);
        parser.push_atom(); // 5
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 2);
        // + emitted at close bracket, then * at finish
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Add, 2));
        assert_eq!(opcodes[1], expected_opcode(OperatorKind::Mul, 2));
    }

    #[test]
    fn infix_function_call() {
        // SIN(x) -> should output x then SIN
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        // Note: Functions like SIN use a specific OperatorKind
        // For this test, we use Neg as a stand-in unary operator
        let sin = make_op(OperatorKind::Neg, 0, false, 1);
        parser.push_function(sin, Span::DUMMY);
        parser.push_atom(); // x
        parser.push_close_bracket(&mut output).unwrap();
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 1);
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Neg, 1));
    }

    #[test]
    fn infix_unary_prefix() {
        // -3 -> push_op(NEG), push_atom, finish
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        let neg = make_op(OperatorKind::Neg, 25, true, 1); // NEG prefix, high precedence
        parser.push_operator(neg, &mut output);
        parser.push_atom(); // 3
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 1);
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Neg, 1));
    }

    #[test]
    fn infix_error_unmatched_close() {
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom();
        let result = parser.push_close_bracket(&mut output);
        assert_eq!(result, Err(InfixError::UnmatchedCloseParen));
    }

    #[test]
    fn infix_error_unclosed_paren() {
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_open_bracket(Span::DUMMY);
        parser.push_atom();
        let result = parser.finish(&mut output);
        assert_eq!(result, Err(InfixError::UnclosedParen));
    }

    #[test]
    fn infix_error_unexpected_comma() {
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_atom();
        let result = parser.push_comma(&mut output);
        assert_eq!(result, Err(InfixError::UnexpectedComma));
    }

    #[test]
    fn infix_nested_parens() {
        // ((3 + 4)) -> output: +
        let mut output = OutputBuffer::new();
        let mut parser = InfixParser::new(0);

        parser.push_open_bracket(Span::DUMMY);
        parser.push_open_bracket(Span::DUMMY);
        parser.push_atom(); // 3
        let add = make_op(OperatorKind::Add, 10, false, 2);
        parser.push_operator(add, &mut output);
        parser.push_atom(); // 4
        parser.push_close_bracket(&mut output).unwrap();
        parser.push_close_bracket(&mut output).unwrap();
        parser.finish(&mut output).unwrap();

        let opcodes = extract_opcodes(&output);
        assert_eq!(opcodes.len(), 1);
        assert_eq!(opcodes[0], expected_opcode(OperatorKind::Add, 2));
    }

    #[test]
    fn infix_helper_methods() {
        let parser = InfixParser::new(42);
        assert_eq!(parser.start_pos(), 42);
        assert!(!parser.in_parens());
        assert_eq!(parser.current_arg_count(), None);
    }

    #[test]
    fn infix_in_parens() {
        let mut parser = InfixParser::new(0);
        assert!(!parser.in_parens());

        parser.push_open_bracket(Span::DUMMY);
        assert!(parser.in_parens());
        assert_eq!(parser.current_arg_count(), Some(1));
    }
}
