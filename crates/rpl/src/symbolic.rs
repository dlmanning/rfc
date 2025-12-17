//! Symbolic expression representation.
//!
//! Symbolic expressions are unevaluated algebraic expressions like `'X+1'`
//! or `'SIN(X^2)'`. They form a tree structure that can be manipulated
//! algebraically before being evaluated numerically.
//!
//! # Examples
//!
//! ```text
//! 'X'           → Var("X")
//! '2+3'         → Binary(Add, Num(2), Num(3))
//! 'X^2+1'       → Binary(Add, Binary(Pow, Var("X"), Num(2)), Num(1))
//! 'SIN(X)'      → Call("SIN", [Var("X")])
//! ```

use std::{fmt, sync::Arc};

/// A symbolic expression node.
#[derive(Clone, Debug, PartialEq)]
pub enum SymExpr {
    /// A symbolic variable (e.g., X, Y, Z).
    Var(Arc<str>),
    /// A numeric constant.
    Num(f64),
    /// A binary operation.
    Binary(BinOp, Arc<SymExpr>, Arc<SymExpr>),
    /// A unary operation.
    Unary(UnaryOp, Arc<SymExpr>),
    /// A function call (e.g., SIN(X), COS(X+1)).
    Call(Arc<str>, Arc<[SymExpr]>),
}

/// Binary operators in symbolic expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

/// Unary operators in symbolic expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}

impl SymExpr {
    /// Create a variable reference.
    pub fn var(name: impl Into<Arc<str>>) -> Self {
        SymExpr::Var(name.into())
    }

    /// Create a numeric constant.
    pub fn num(n: f64) -> Self {
        SymExpr::Num(n)
    }

    /// Create a binary operation.
    pub fn binary(op: BinOp, left: SymExpr, right: SymExpr) -> Self {
        SymExpr::Binary(op, Arc::new(left), Arc::new(right))
    }

    /// Create a unary operation.
    pub fn unary(op: UnaryOp, operand: SymExpr) -> Self {
        SymExpr::Unary(op, Arc::new(operand))
    }

    /// Create a function call.
    pub fn call(name: impl Into<Arc<str>>, args: Vec<SymExpr>) -> Self {
        SymExpr::Call(name.into(), args.into())
    }

    /// Check if this is a simple numeric constant.
    pub fn is_num(&self) -> bool {
        matches!(self, SymExpr::Num(_))
    }

    /// Check if this is a variable.
    pub fn is_var(&self) -> bool {
        matches!(self, SymExpr::Var(_))
    }

    /// Try to evaluate to a numeric value (only works if no variables).
    pub fn try_eval(&self) -> Option<f64> {
        match self {
            SymExpr::Num(n) => Some(*n),
            SymExpr::Var(_) => None,
            SymExpr::Binary(op, left, right) => {
                let l = left.try_eval()?;
                let r = right.try_eval()?;
                Some(match op {
                    BinOp::Add => l + r,
                    BinOp::Sub => l - r,
                    BinOp::Mul => l * r,
                    BinOp::Div => l / r,
                    BinOp::Pow => l.powf(r),
                })
            }
            SymExpr::Unary(op, operand) => {
                let v = operand.try_eval()?;
                Some(match op {
                    UnaryOp::Neg => -v,
                })
            }
            SymExpr::Call(_, _) => None, // Would need function lookup
        }
    }

    /// Evaluate with variable lookup.
    /// The lookup function returns Some(value) if the variable is found.
    pub fn eval_with_lookup<F>(&self, lookup: &F) -> Result<f64, String>
    where
        F: Fn(&str) -> Option<f64>,
    {
        match self {
            SymExpr::Num(n) => Ok(*n),
            SymExpr::Var(name) => {
                lookup(name).ok_or_else(|| format!("undefined variable: {}", name))
            }
            SymExpr::Binary(op, left, right) => {
                let l = left.eval_with_lookup(lookup)?;
                let r = right.eval_with_lookup(lookup)?;
                Ok(match op {
                    BinOp::Add => l + r,
                    BinOp::Sub => l - r,
                    BinOp::Mul => l * r,
                    BinOp::Div => l / r,
                    BinOp::Pow => l.powf(r),
                })
            }
            SymExpr::Unary(op, operand) => {
                let v = operand.eval_with_lookup(lookup)?;
                Ok(match op {
                    UnaryOp::Neg => -v,
                })
            }
            SymExpr::Call(name, args) => {
                // Evaluate arguments
                let evaluated_args: Result<Vec<f64>, String> =
                    args.iter().map(|a| a.eval_with_lookup(lookup)).collect();
                let args = evaluated_args?;

                // Built-in functions
                match name.to_uppercase().as_str() {
                    "SIN" if args.len() == 1 => Ok(args[0].sin()),
                    "COS" if args.len() == 1 => Ok(args[0].cos()),
                    "TAN" if args.len() == 1 => Ok(args[0].tan()),
                    "ASIN" if args.len() == 1 => Ok(args[0].asin()),
                    "ACOS" if args.len() == 1 => Ok(args[0].acos()),
                    "ATAN" if args.len() == 1 => Ok(args[0].atan()),
                    "SQRT" if args.len() == 1 => Ok(args[0].sqrt()),
                    "EXP" if args.len() == 1 => Ok(args[0].exp()),
                    "LN" if args.len() == 1 => Ok(args[0].ln()),
                    "LOG" if args.len() == 1 => Ok(args[0].log10()),
                    "ABS" if args.len() == 1 => Ok(args[0].abs()),
                    _ => Err(format!("unknown function: {}", name)),
                }
            }
        }
    }
}

impl BinOp {
    /// Operator precedence (higher binds tighter).
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Add | BinOp::Sub => 1,
            BinOp::Mul | BinOp::Div => 2,
            BinOp::Pow => 3,
        }
    }

    /// Is this operator right-associative?
    pub fn is_right_assoc(self) -> bool {
        matches!(self, BinOp::Pow)
    }

    /// Symbol for display.
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Pow => "^",
        }
    }
}

impl UnaryOp {
    /// Symbol for display.
    pub fn symbol(self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
        }
    }
}

impl fmt::Display for SymExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymExpr::Var(name) => write!(f, "{}", name),
            SymExpr::Num(n) => {
                if n.fract() == 0.0 && n.abs() < 1e15 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            SymExpr::Binary(op, left, right) => {
                // Add parens based on precedence
                let needs_left_parens = matches!(left.as_ref(), SymExpr::Binary(lop, _, _) if lop.precedence() < op.precedence());
                let needs_right_parens = matches!(right.as_ref(), SymExpr::Binary(rop, _, _) if rop.precedence() < op.precedence() || (rop.precedence() == op.precedence() && !op.is_right_assoc()));

                if needs_left_parens {
                    write!(f, "({})", left)?;
                } else {
                    write!(f, "{}", left)?;
                }
                write!(f, "{}", op.symbol())?;
                if needs_right_parens {
                    write!(f, "({})", right)?;
                } else {
                    write!(f, "{}", right)?;
                }
                Ok(())
            }
            SymExpr::Unary(op, operand) => {
                write!(f, "{}", op.symbol())?;
                match operand.as_ref() {
                    SymExpr::Binary(_, _, _) => write!(f, "({})", operand),
                    _ => write!(f, "{}", operand),
                }
            }
            SymExpr::Call(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn num_eval() {
        let expr = SymExpr::num(42.0);
        assert_eq!(expr.try_eval(), Some(42.0));
    }

    #[test]
    fn binary_eval() {
        let expr = SymExpr::binary(BinOp::Add, SymExpr::num(2.0), SymExpr::num(3.0));
        assert_eq!(expr.try_eval(), Some(5.0));
    }

    #[test]
    fn var_no_eval() {
        let expr = SymExpr::var("X");
        assert_eq!(expr.try_eval(), None);
    }

    #[test]
    fn precedence() {
        assert!(BinOp::Mul.precedence() > BinOp::Add.precedence());
        assert!(BinOp::Pow.precedence() > BinOp::Mul.precedence());
    }

    #[test]
    fn pow_right_assoc() {
        assert!(BinOp::Pow.is_right_assoc());
        assert!(!BinOp::Add.is_right_assoc());
    }

    #[test]
    fn display_var() {
        let expr = SymExpr::var("X");
        assert_eq!(format!("{}", expr), "X");
    }

    #[test]
    fn display_num() {
        assert_eq!(format!("{}", SymExpr::num(42.0)), "42");
        assert_eq!(format!("{}", SymExpr::num(3.14)), "3.14");
    }

    #[test]
    fn display_binary() {
        let expr = SymExpr::binary(BinOp::Add, SymExpr::var("X"), SymExpr::num(1.0));
        assert_eq!(format!("{}", expr), "X+1");
    }

    #[test]
    fn display_precedence() {
        // X+Y*Z should display as X+Y*Z (no parens needed)
        let expr = SymExpr::binary(
            BinOp::Add,
            SymExpr::var("X"),
            SymExpr::binary(BinOp::Mul, SymExpr::var("Y"), SymExpr::var("Z")),
        );
        assert_eq!(format!("{}", expr), "X+Y*Z");

        // (X+Y)*Z needs parens
        let expr = SymExpr::binary(
            BinOp::Mul,
            SymExpr::binary(BinOp::Add, SymExpr::var("X"), SymExpr::var("Y")),
            SymExpr::var("Z"),
        );
        assert_eq!(format!("{}", expr), "(X+Y)*Z");
    }

    #[test]
    fn display_call() {
        let expr = SymExpr::call("SIN", vec![SymExpr::var("X")]);
        assert_eq!(format!("{}", expr), "SIN(X)");
    }
}
