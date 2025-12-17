//! Infix parser for symbolic expressions.
//!
//! Parses expressions like `X+1`, `X^2+2*X+1`, `SIN(X)` using
//! precedence climbing (Pratt parsing).
//!
//! Operator precedence (highest to lowest):
//! - Function calls: `SIN(X)`
//! - Power: `^` (right associative)
//! - Multiplication/Division: `*`, `/`
//! - Addition/Subtraction: `+`, `-`

use crate::symbolic::{BinOp, SymExpr, UnaryOp};

/// Token types for the infix parser.
#[derive(Clone, Debug, PartialEq)]
pub enum InfixToken {
    /// A number (integer or real).
    Num(f64),
    /// An identifier (variable or function name).
    Ident(String),
    /// Binary operator.
    Op(BinOp),
    /// Opening parenthesis.
    LParen,
    /// Closing parenthesis.
    RParen,
    /// Comma (for function arguments).
    Comma,
    /// Unary minus (distinguished during parsing).
    Minus,
    /// End of expression.
    End,
}

/// Tokenize an infix expression string.
pub fn tokenize_infix(input: &str) -> Result<Vec<InfixToken>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            '0'..='9' | '.' => {
                let mut num_str = String::new();
                let mut has_dot = false;
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        num_str.push(c);
                        chars.next();
                    } else if c == '.' && !has_dot {
                        has_dot = true;
                        num_str.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let n: f64 = num_str
                    .parse()
                    .map_err(|_| format!("invalid number: {}", num_str))?;
                tokens.push(InfixToken::Num(n));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(InfixToken::Ident(ident));
            }
            '+' => {
                chars.next();
                tokens.push(InfixToken::Op(BinOp::Add));
            }
            '-' => {
                chars.next();
                tokens.push(InfixToken::Minus);
            }
            '*' => {
                chars.next();
                tokens.push(InfixToken::Op(BinOp::Mul));
            }
            '/' => {
                chars.next();
                tokens.push(InfixToken::Op(BinOp::Div));
            }
            '^' => {
                chars.next();
                tokens.push(InfixToken::Op(BinOp::Pow));
            }
            '(' => {
                chars.next();
                tokens.push(InfixToken::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(InfixToken::RParen);
            }
            ',' => {
                chars.next();
                tokens.push(InfixToken::Comma);
            }
            _ => {
                return Err(format!(
                    "unexpected character in symbolic expression: '{}'",
                    ch
                ));
            }
        }
    }

    tokens.push(InfixToken::End);
    Ok(tokens)
}

/// Parser state for infix expressions.
pub struct InfixParser {
    tokens: Vec<InfixToken>,
    pos: usize,
}

impl InfixParser {
    /// Create a new parser for the given tokens.
    pub fn new(tokens: Vec<InfixToken>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Parse from a string.
    pub fn parse_str(input: &str) -> Result<SymExpr, String> {
        let tokens = tokenize_infix(input)?;
        let mut parser = Self::new(tokens);
        parser.parse_expr(0)
    }

    /// Peek at the current token.
    fn peek(&self) -> &InfixToken {
        self.tokens.get(self.pos).unwrap_or(&InfixToken::End)
    }

    /// Advance and return the current token.
    fn advance(&mut self) -> InfixToken {
        let tok = self.peek().clone();
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    /// Parse an expression with the given minimum precedence.
    fn parse_expr(&mut self, min_prec: u8) -> Result<SymExpr, String> {
        let mut left = self.parse_atom()?;

        loop {
            let op = match self.peek() {
                InfixToken::Op(op) => *op,
                InfixToken::Minus => BinOp::Sub,
                _ => break,
            };

            let prec = op.precedence();
            if prec < min_prec {
                break;
            }

            self.advance(); // consume operator

            // For right-associative operators, use same precedence
            // For left-associative, use precedence + 1
            let next_min_prec = if op.is_right_assoc() { prec } else { prec + 1 };
            let right = self.parse_expr(next_min_prec)?;
            left = SymExpr::binary(op, left, right);
        }

        Ok(left)
    }

    /// Parse an atomic expression (number, variable, function call, or parenthesized).
    fn parse_atom(&mut self) -> Result<SymExpr, String> {
        match self.peek().clone() {
            InfixToken::Num(n) => {
                self.advance();
                Ok(SymExpr::num(n))
            }
            InfixToken::Ident(name) => {
                self.advance();
                // Check for function call
                if matches!(self.peek(), InfixToken::LParen) {
                    self.advance(); // consume (
                    let args = self.parse_args()?;
                    if !matches!(self.peek(), InfixToken::RParen) {
                        return Err("expected ')' after function arguments".into());
                    }
                    self.advance(); // consume )
                    Ok(SymExpr::call(name, args))
                } else {
                    Ok(SymExpr::var(name))
                }
            }
            InfixToken::LParen => {
                self.advance(); // consume (
                let expr = self.parse_expr(0)?;
                if !matches!(self.peek(), InfixToken::RParen) {
                    return Err("expected ')'".into());
                }
                self.advance(); // consume )
                Ok(expr)
            }
            InfixToken::Minus => {
                self.advance(); // consume -
                // Unary minus has high precedence
                let operand = self.parse_atom()?;
                Ok(SymExpr::unary(UnaryOp::Neg, operand))
            }
            InfixToken::End => Err("unexpected end of expression".into()),
            other => Err(format!("unexpected token: {:?}", other)),
        }
    }

    /// Parse comma-separated arguments.
    fn parse_args(&mut self) -> Result<Vec<SymExpr>, String> {
        let mut args = Vec::new();

        // Empty args
        if matches!(self.peek(), InfixToken::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expr(0)?);
            if matches!(self.peek(), InfixToken::Comma) {
                self.advance(); // consume ,
            } else {
                break;
            }
        }

        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: &str) -> Result<SymExpr, String> {
        InfixParser::parse_str(s)
    }

    #[test]
    fn parse_number() {
        assert_eq!(parse("42").unwrap(), SymExpr::num(42.0));
        assert_eq!(parse("3.14").unwrap(), SymExpr::num(3.14));
    }

    #[test]
    fn parse_variable() {
        assert_eq!(parse("X").unwrap(), SymExpr::var("X"));
        assert_eq!(parse("foo").unwrap(), SymExpr::var("foo"));
    }

    #[test]
    fn parse_binary() {
        let expr = parse("X+1").unwrap();
        assert_eq!(
            expr,
            SymExpr::binary(BinOp::Add, SymExpr::var("X"), SymExpr::num(1.0))
        );
    }

    #[test]
    fn parse_precedence() {
        // X+Y*Z should parse as X+(Y*Z)
        let expr = parse("X+Y*Z").unwrap();
        assert_eq!(
            expr,
            SymExpr::binary(
                BinOp::Add,
                SymExpr::var("X"),
                SymExpr::binary(BinOp::Mul, SymExpr::var("Y"), SymExpr::var("Z"))
            )
        );
    }

    #[test]
    fn parse_left_assoc() {
        // X-Y-Z should parse as (X-Y)-Z
        let expr = parse("X-Y-Z").unwrap();
        assert_eq!(
            expr,
            SymExpr::binary(
                BinOp::Sub,
                SymExpr::binary(BinOp::Sub, SymExpr::var("X"), SymExpr::var("Y")),
                SymExpr::var("Z")
            )
        );
    }

    #[test]
    fn parse_right_assoc_pow() {
        // X^Y^Z should parse as X^(Y^Z)
        let expr = parse("X^Y^Z").unwrap();
        assert_eq!(
            expr,
            SymExpr::binary(
                BinOp::Pow,
                SymExpr::var("X"),
                SymExpr::binary(BinOp::Pow, SymExpr::var("Y"), SymExpr::var("Z"))
            )
        );
    }

    #[test]
    fn parse_parens() {
        // (X+Y)*Z
        let expr = parse("(X+Y)*Z").unwrap();
        assert_eq!(
            expr,
            SymExpr::binary(
                BinOp::Mul,
                SymExpr::binary(BinOp::Add, SymExpr::var("X"), SymExpr::var("Y")),
                SymExpr::var("Z")
            )
        );
    }

    #[test]
    fn parse_unary_minus() {
        let expr = parse("-X").unwrap();
        assert_eq!(expr, SymExpr::unary(UnaryOp::Neg, SymExpr::var("X")));
    }

    #[test]
    fn parse_function_call() {
        let expr = parse("SIN(X)").unwrap();
        assert_eq!(expr, SymExpr::call("SIN", vec![SymExpr::var("X")]));
    }

    #[test]
    fn parse_function_multi_args() {
        let expr = parse("MAX(X,Y)").unwrap();
        assert_eq!(
            expr,
            SymExpr::call("MAX", vec![SymExpr::var("X"), SymExpr::var("Y")])
        );
    }

    #[test]
    fn parse_complex() {
        // X^2+2*X+1
        let expr = parse("X^2+2*X+1").unwrap();
        // Should be ((X^2)+(2*X))+1
        let expected = SymExpr::binary(
            BinOp::Add,
            SymExpr::binary(
                BinOp::Add,
                SymExpr::binary(BinOp::Pow, SymExpr::var("X"), SymExpr::num(2.0)),
                SymExpr::binary(BinOp::Mul, SymExpr::num(2.0), SymExpr::var("X")),
            ),
            SymExpr::num(1.0),
        );
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_nested_function() {
        let expr = parse("SIN(X+1)").unwrap();
        assert_eq!(
            expr,
            SymExpr::call(
                "SIN",
                vec![SymExpr::binary(
                    BinOp::Add,
                    SymExpr::var("X"),
                    SymExpr::num(1.0)
                )]
            )
        );
    }
}
