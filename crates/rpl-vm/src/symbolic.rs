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

use crate::bytecode::{read_leb128_u32, write_leb128_u32};

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
#[repr(u8)]
pub enum BinOp {
    Add = 0,
    Sub = 1,
    Mul = 2,
    Div = 3,
    Pow = 4,
}

impl BinOp {
    /// Convert from byte representation.
    pub fn from_u8(b: u8) -> Option<Self> {
        match b {
            0 => Some(BinOp::Add),
            1 => Some(BinOp::Sub),
            2 => Some(BinOp::Mul),
            3 => Some(BinOp::Div),
            4 => Some(BinOp::Pow),
            _ => None,
        }
    }
}

/// Unary operators in symbolic expressions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum UnaryOp {
    Neg = 0,
}

impl UnaryOp {
    /// Convert from byte representation.
    pub fn from_u8(b: u8) -> Option<Self> {
        match b {
            0 => Some(UnaryOp::Neg),
            _ => None,
        }
    }
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

    // === Serialization ===
    //
    // Binary format:
    // - Tag byte: Var=0, Num=1, Binary=2, Unary=3, Call=4
    // - Var: len (LEB128) + UTF-8 bytes
    // - Num: 8 bytes little-endian f64
    // - Binary: op byte + left + right (recursive)
    // - Unary: op byte + operand (recursive)
    // - Call: name_len (LEB128) + name bytes + arg_count (LEB128) + args (recursive)

    /// Serialize this expression to bytes.
    pub fn serialize(&self, out: &mut Vec<u8>) {
        match self {
            SymExpr::Var(name) => {
                out.push(0); // tag
                let bytes = name.as_bytes();
                write_leb128_u32(bytes.len() as u32, out);
                out.extend_from_slice(bytes);
            }
            SymExpr::Num(n) => {
                out.push(1); // tag
                out.extend_from_slice(&n.to_le_bytes());
            }
            SymExpr::Binary(op, left, right) => {
                out.push(2); // tag
                out.push(*op as u8);
                left.serialize(out);
                right.serialize(out);
            }
            SymExpr::Unary(op, operand) => {
                out.push(3); // tag
                out.push(*op as u8);
                operand.serialize(out);
            }
            SymExpr::Call(name, args) => {
                out.push(4); // tag
                let name_bytes = name.as_bytes();
                write_leb128_u32(name_bytes.len() as u32, out);
                out.extend_from_slice(name_bytes);
                write_leb128_u32(args.len() as u32, out);
                for arg in args.iter() {
                    arg.serialize(out);
                }
            }
        }
    }

    /// Deserialize an expression from bytes.
    /// Returns the expression and the number of bytes consumed.
    pub fn deserialize(bytes: &[u8]) -> Result<(Self, usize), &'static str> {
        if bytes.is_empty() {
            return Err("unexpected end of input");
        }

        let tag = bytes[0];
        let mut offset = 1;

        match tag {
            0 => {
                // Var
                let len = read_leb128_u32(bytes, &mut offset).ok_or("invalid var length")?;
                let end = offset + len as usize;
                if end > bytes.len() {
                    return Err("var name truncated");
                }
                let name = std::str::from_utf8(&bytes[offset..end])
                    .map_err(|_| "invalid UTF-8 in var name")?;
                Ok((SymExpr::Var(name.into()), end))
            }
            1 => {
                // Num
                if offset + 8 > bytes.len() {
                    return Err("num truncated");
                }
                let n = f64::from_le_bytes(bytes[offset..offset + 8].try_into().unwrap());
                Ok((SymExpr::Num(n), offset + 8))
            }
            2 => {
                // Binary
                if offset >= bytes.len() {
                    return Err("binary op truncated");
                }
                let op = BinOp::from_u8(bytes[offset]).ok_or("invalid binary op")?;
                offset += 1;
                let (left, left_len) = SymExpr::deserialize(&bytes[offset..])?;
                offset += left_len;
                let (right, right_len) = SymExpr::deserialize(&bytes[offset..])?;
                offset += right_len;
                Ok((SymExpr::Binary(op, Arc::new(left), Arc::new(right)), offset))
            }
            3 => {
                // Unary
                if offset >= bytes.len() {
                    return Err("unary op truncated");
                }
                let op = UnaryOp::from_u8(bytes[offset]).ok_or("invalid unary op")?;
                offset += 1;
                let (operand, operand_len) = SymExpr::deserialize(&bytes[offset..])?;
                offset += operand_len;
                Ok((SymExpr::Unary(op, Arc::new(operand)), offset))
            }
            4 => {
                // Call
                let name_len = read_leb128_u32(bytes, &mut offset).ok_or("invalid call name length")?;
                let name_end = offset + name_len as usize;
                if name_end > bytes.len() {
                    return Err("call name truncated");
                }
                let name = std::str::from_utf8(&bytes[offset..name_end])
                    .map_err(|_| "invalid UTF-8 in call name")?;
                offset = name_end;
                let arg_count = read_leb128_u32(bytes, &mut offset).ok_or("invalid arg count")?;
                let mut args = Vec::with_capacity(arg_count as usize);
                for _ in 0..arg_count {
                    let (arg, arg_len) = SymExpr::deserialize(&bytes[offset..])?;
                    offset += arg_len;
                    args.push(arg);
                }
                Ok((SymExpr::Call(name.into(), args.into()), offset))
            }
            _ => Err("invalid symexpr tag"),
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

    #[test]
    fn serialize_roundtrip_var() {
        let expr = SymExpr::var("X");
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }

    #[test]
    fn serialize_roundtrip_num() {
        let expr = SymExpr::num(3.14159);
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }

    #[test]
    fn serialize_roundtrip_binary() {
        let expr = SymExpr::binary(BinOp::Add, SymExpr::var("X"), SymExpr::num(1.0));
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }

    #[test]
    fn serialize_roundtrip_unary() {
        let expr = SymExpr::unary(UnaryOp::Neg, SymExpr::var("X"));
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }

    #[test]
    fn serialize_roundtrip_call() {
        let expr = SymExpr::call("SIN", vec![SymExpr::var("X")]);
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }

    #[test]
    fn serialize_roundtrip_complex() {
        // 'X^2 + SIN(Y)'
        let expr = SymExpr::binary(
            BinOp::Add,
            SymExpr::binary(BinOp::Pow, SymExpr::var("X"), SymExpr::num(2.0)),
            SymExpr::call("SIN", vec![SymExpr::var("Y")]),
        );
        let mut bytes = Vec::new();
        expr.serialize(&mut bytes);
        let (decoded, len) = SymExpr::deserialize(&bytes).unwrap();
        assert_eq!(decoded, expr);
        assert_eq!(len, bytes.len());
    }
}
