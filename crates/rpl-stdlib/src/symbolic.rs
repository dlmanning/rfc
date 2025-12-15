use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_core::{extract_cmd, extract_lib, extract_size, extract_type, is_prolog, TypeId, Word};
use rpl_lang::library::{LibraryId, StackEffect};

// ============================================================================
// Expression tree for RPN-to-infix conversion
// ============================================================================

/// Expression tree node for decompilation.
#[derive(Debug, Clone)]
enum Expr {
    /// Numeric literal.
    Num(f64),
    /// Variable reference.
    Var(String),
    /// Binary operation.
    Binary {
        op: &'static str,
        left: Box<Expr>,
        right: Box<Expr>,
        precedence: u8,
        right_assoc: bool,
    },
    /// Unary operation.
    Unary { op: &'static str, arg: Box<Expr> },
    /// Unknown/placeholder.
    Unknown(String),
}

/// Decode an operator from lib/cmd to (symbol, arity, precedence, right_assoc).
fn decode_operator(lib: u16, cmd: u16) -> Option<(&'static str, u8, u8, bool)> {
    use crate::RealNumbersLib;

    if lib == RealNumbersLib::ID.as_u16() {
        // RealNumbersLib command IDs
        match cmd {
            0 => Some(("+", 2, 8, false)),    // ADD
            1 => Some(("-", 2, 8, false)),    // SUB
            2 => Some(("*", 2, 10, false)),   // MUL
            3 => Some(("/", 2, 10, false)),   // DIV
            4 => Some(("MOD", 2, 10, false)), // MOD
            5 => Some(("-", 1, 14, false)),   // NEG (unary minus)
            6 => Some(("ABS", 1, 14, false)), // ABS
            7 => Some(("INV", 1, 14, false)), // INV
            8 => Some(("^", 2, 12, true)),    // POW (right associative)
            _ => None,
        }
    } else {
        None
    }
}

/// Get precedence and associativity for an OperatorKind.
fn operator_precedence(op: rpl_lang::operator::OperatorKind) -> (u8, bool) {
    use rpl_lang::operator::OperatorKind;
    match op {
        // Lowest precedence: comparison operators
        OperatorKind::Eq | OperatorKind::Ne | OperatorKind::Same => (4, false),
        OperatorKind::Lt
        | OperatorKind::Le
        | OperatorKind::Gt
        | OperatorKind::Ge
        | OperatorKind::Cmp => (6, false),
        // Additive operators
        OperatorKind::Add | OperatorKind::Sub => (8, false),
        // Multiplicative operators
        OperatorKind::Mul | OperatorKind::Div | OperatorKind::Mod => (10, false),
        // Power (right associative)
        OperatorKind::Pow => (12, true),
        // Unary operators (highest precedence)
        OperatorKind::Neg
        | OperatorKind::Abs
        | OperatorKind::Inv
        | OperatorKind::Sqrt
        | OperatorKind::Not => (14, false),
        // Logical operators
        OperatorKind::And => (3, false),
        OperatorKind::Or => (2, false),
        OperatorKind::Xor => (2, false),
        // Other operators (default precedence)
        _ => (10, false),
    }
}

/// Parse RPN bytecode into an expression tree.
fn parse_rpn_to_tree(
    code: &[Word],
    registry: Option<&rpl_lang::library::LibraryRegistry>,
    interner: Option<&rpl_core::Interner>,
) -> Expr {
    let mut stack: Vec<Expr> = Vec::new();
    let mut pos = 0;

    while pos < code.len() {
        let word = code[pos];

        if is_prolog(word) {
            let type_id = extract_type(word);
            let size = extract_size(word) as usize;
            pos += 1;

            if type_id == TypeId::REAL.as_u16() && size == 2 && pos + 2 <= code.len() {
                // Real number literal
                let high = code[pos] as u64;
                let low = code[pos + 1] as u64;
                let bits = (high << 32) | low;
                let value = f64::from_bits(bits);
                stack.push(Expr::Num(value));
                pos += 2;
            } else {
                // Unknown prolog type - skip it
                stack.push(Expr::Unknown(format!("<type:{}>", type_id)));
                pos += size;
            }
        } else {
            // Call - could be operator or symbol reference
            let lib = extract_lib(word);
            let cmd = extract_cmd(word);
            pos += 1;

            // Check if it's a dynamic dispatch (operator or symbol reference)
            if lib == rpl_lang::DISPATCH_LIB {
                let (op_kind, arity) = rpl_lang::decode_dispatch(cmd);
                // If arity > 0, it's an operator; otherwise it might be a variable reference
                if arity > 0 {
                    let op_str = rpl_lang::operator_symbol(op_kind);
                    let (prec, right_assoc) = operator_precedence(op_kind);
                    match arity {
                        1 => {
                            let arg = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                            stack.push(Expr::Unary {
                                op: op_str,
                                arg: Box::new(arg),
                            });
                        }
                        2 => {
                            let right = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                            let left = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                            stack.push(Expr::Binary {
                                op: op_str,
                                left: Box::new(left),
                                right: Box::new(right),
                                precedence: prec,
                                right_assoc,
                            });
                        }
                        _ => {
                            stack.push(Expr::Unknown(format!("<dispatch:arity{}>", arity)));
                        }
                    }
                } else {
                    // Arity 0 means it's a symbol reference (variable)
                    let symbol = rpl_lang::decode_dispatch_symbol(cmd);
                    stack.push(Expr::Var(symbol.to_string()));
                }
                continue;
            }

            // Check if it's a symbolic variable reference (from IdentifiersLib)
            if lib == crate::IdentifiersLib::ID.as_u16()
                && cmd == crate::identifiers::CMD_SYMBOLIC_VAR
            {
                // Read the symbol ID
                if pos < code.len() {
                    let sym_id = code[pos];
                    pos += 1;
                    let name = if let Some(int) = interner {
                        let symbol = rpl_core::Symbol::from_raw(sym_id);
                        int.resolve(symbol).to_string()
                    } else {
                        format!("${}", sym_id)
                    };
                    stack.push(Expr::Var(name));
                } else {
                    stack.push(Expr::Unknown("<missing-sym>".into()));
                }
                continue;
            }

            // Check if it's an operator
            if let Some((op_str, arity, prec, right_assoc)) = decode_operator(lib, cmd) {
                match arity {
                    1 => {
                        let arg = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                        stack.push(Expr::Unary {
                            op: op_str,
                            arg: Box::new(arg),
                        });
                    }
                    2 => {
                        let right = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                        let left = stack.pop().unwrap_or(Expr::Unknown("?".into()));
                        stack.push(Expr::Binary {
                            op: op_str,
                            left: Box::new(left),
                            right: Box::new(right),
                            precedence: prec,
                            right_assoc,
                        });
                    }
                    _ => {
                        stack.push(Expr::Unknown(format!("<arity:{}>", arity)));
                    }
                }
            } else {
                // Unknown operator - try to decompile it
                let name = if let Some(reg) = registry {
                    if let Some(lib_ref) = reg.get(LibraryId::new(lib)) {
                        lib_ref.name().to_string()
                    } else {
                        format!("lib{}", lib)
                    }
                } else {
                    format!("lib{}", lib)
                };
                stack.push(Expr::Unknown(format!("<{}:cmd{}>", name, cmd)));
            }
        }
    }

    stack.pop().unwrap_or(Expr::Unknown("?".into()))
}

/// Format an expression as infix notation.
fn format_infix(expr: &Expr) -> String {
    match expr {
        Expr::Num(n) => {
            if n.fract() == 0.0 && n.abs() < 1e15 {
                format!("{}", *n as i64)
            } else {
                format!("{}", n)
            }
        }
        Expr::Var(name) => name.clone(),
        Expr::Unary { op, arg } => {
            let arg_str = format_infix(arg);
            // Unary minus needs parens around complex args
            match arg.as_ref() {
                Expr::Binary { .. } => format!("{}({})", op, arg_str),
                _ => format!("{}{}", op, arg_str),
            }
        }
        Expr::Binary {
            op,
            left,
            right,
            precedence,
            right_assoc,
        } => {
            let left_str = format_with_parens(left, *precedence, true, *right_assoc);
            let right_str = format_with_parens(right, *precedence, false, *right_assoc);
            format!("{} {} {}", left_str, op, right_str)
        }
        Expr::Unknown(s) => s.clone(),
    }
}

/// Format an expression, adding parentheses if needed based on precedence.
fn format_with_parens(
    expr: &Expr,
    parent_prec: u8,
    is_left: bool,
    parent_right_assoc: bool,
) -> String {
    let inner = format_infix(expr);

    let needs_parens = match expr {
        Expr::Binary {
            precedence,
            right_assoc: _,
            ..
        } => {
            if *precedence < parent_prec {
                // Lower precedence always needs parens
                true
            } else if *precedence == parent_prec {
                // Same precedence: depends on associativity
                if parent_right_assoc {
                    // Right-associative parent: left child needs parens at same prec
                    is_left
                } else {
                    // Left-associative parent: right child needs parens at same prec
                    !is_left
                }
            } else {
                false
            }
        }
        _ => false,
    };

    if needs_parens {
        format!("({})", inner)
    } else {
        inner
    }
}

/// Check if a string is a valid identifier for symbolic expressions.
fn is_identifier(text: &str) -> bool {
    if text.is_empty() {
        return false;
    }

    let mut chars = text.chars();
    let first = chars.next().unwrap();

    // Must start with letter or underscore
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }

    // Rest must be alphanumeric or underscore
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Probe for infix operators and return their token info.
fn probe_infix_operator(text: &str) -> Option<(TokenInfo, SemanticKind)> {
    // Precedence levels (higher = binds tighter):
    // 1-2: Logical OR
    // 3-4: Logical AND
    // 5-6: Comparison (==, !=, <, >, <=, >=)
    // 7-8: Addition, Subtraction
    // 9-10: Multiplication, Division
    // 11-12: Power (right associative)
    // 13-14: Unary operators

    match text {
        // Binary operators (left associative unless noted)
        "+" => Some((TokenInfo::binary_left(1, 8), SemanticKind::Operator)),
        "-" => Some((TokenInfo::binary_left(1, 8), SemanticKind::Operator)),
        "*" => Some((TokenInfo::binary_left(1, 10), SemanticKind::Operator)),
        "/" => Some((TokenInfo::binary_left(1, 10), SemanticKind::Operator)),
        "^" => Some((TokenInfo::binary_right(1, 12), SemanticKind::Operator)), // right assoc

        // Comparison operators
        "==" => Some((TokenInfo::binary_left(2, 6), SemanticKind::Operator)),
        "!=" | "<>" => Some((
            TokenInfo::binary_left(text.len() as u8, 6),
            SemanticKind::Operator,
        )),
        "<" => Some((TokenInfo::binary_left(1, 6), SemanticKind::Operator)),
        ">" => Some((TokenInfo::binary_left(1, 6), SemanticKind::Operator)),
        "<=" | "≤" => Some((
            TokenInfo::binary_left(text.len() as u8, 6),
            SemanticKind::Operator,
        )),
        ">=" | "≥" => Some((
            TokenInfo::binary_left(text.len() as u8, 6),
            SemanticKind::Operator,
        )),

        // Logical operators
        "AND" => Some((TokenInfo::binary_left(3, 4), SemanticKind::Keyword)),
        "OR" => Some((TokenInfo::binary_left(2, 2), SemanticKind::Keyword)),
        "XOR" => Some((TokenInfo::binary_left(3, 3), SemanticKind::Keyword)),
        "NOT" => Some((TokenInfo::prefix(3, 14), SemanticKind::Keyword)),

        _ => None,
    }
}

// ============================================================================
// Library implementation using DSL
// ============================================================================

rpl_macros::define_library! {
    pub library SymbolicLib(56, "Symbolic");

    commands {}

    custom probe {
        let text = ctx.text();

        // Single quote at the start: begin symbolic expression
        if text == "'" && !ctx.in_infix() {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::open_bracket(1),
                semantic: SemanticKind::Bracket,
            };
        }

        // Single quote at the end: end symbolic expression
        if text == "'" && ctx.in_infix() {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::close_bracket(1),
                semantic: SemanticKind::Bracket,
            };
        }

        // Inside infix mode, handle identifiers as variables
        if ctx.in_infix() && is_identifier(text) {
            return rpl_lang::library::ProbeResult::Match {
                info: TokenInfo::atom(text.len() as u8),
                semantic: SemanticKind::Variable,
            };
        }

        // Inside infix mode, handle operators with appropriate info
        if ctx.in_infix()
            && let Some((info, semantic)) = probe_infix_operator(text)
        {
            return rpl_lang::library::ProbeResult::Match { info, semantic };
        }

        // Parentheses in infix mode
        if ctx.in_infix() {
            if text == "(" {
                return rpl_lang::library::ProbeResult::Match {
                    info: TokenInfo::open_bracket(1),
                    semantic: SemanticKind::Bracket,
                };
            }
            if text == ")" {
                return rpl_lang::library::ProbeResult::Match {
                    info: TokenInfo::close_bracket(1),
                    semantic: SemanticKind::Bracket,
                };
            }
            if text == "," {
                return rpl_lang::library::ProbeResult::Match {
                    info: TokenInfo::comma(1),
                    semantic: SemanticKind::Bracket,
                };
            }
        }

        rpl_lang::library::ProbeResult::NoMatch
    }

    custom compile {
        let text = ctx.text();

        // Opening quote: start infix mode
        if text == "'" && !ctx.in_infix() {
            return rpl_lang::library::CompileResult::StartInfix;
        }

        // Closing quote: end infix mode
        if text == "'" && ctx.in_infix() {
            return rpl_lang::library::CompileResult::EndInfix;
        }

        // Variables in infix mode: emit as symbolic reference
        if ctx.in_infix() && is_identifier(text) {
            let text_owned = text.to_string();
            let sym = ctx.intern(&text_owned);
            ctx.emit_opcode(
                crate::IdentifiersLib::ID.as_u16(),
                crate::identifiers::CMD_SYMBOLIC_VAR,
            );
            ctx.emit(sym.as_u32());
            return rpl_lang::library::CompileResult::Ok;
        }

        rpl_lang::library::CompileResult::NoMatch
    }

    custom stack_effect {
        match token {
            "'" => StackEffect::Dynamic,
            _ => StackEffect::Dynamic,
        }
    }
}

impl SymbolicLib {
    /// Type ID for symbolic objects.
    pub const TYPE_ID: TypeId = TypeId::SYMBOLIC;
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::token::TokenType;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::library::{Library, ProbeContext, ProbeResult};

    fn make_probe_ctx<'a>(
        text: &'a str,
        in_infix: bool,
        interner: &'a Interner,
    ) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, in_infix, None, None, interner)
    }

    #[test]
    fn probe_opening_quote() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("'", false, &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::OpenBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_closing_quote() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("'", true, &interner); // in infix mode
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert_eq!(info.ty(), TokenType::CloseBracket);
                assert_eq!(semantic, SemanticKind::Bracket);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_variable_in_infix() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("x", true, &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Variable);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_operator_in_infix() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("+", true, &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, semantic } => {
                assert!(info.ty().is_binary());
                assert_eq!(semantic, SemanticKind::Operator);
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_power_right_assoc() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("^", true, &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { info, .. } => {
                assert!(info.right_assoc());
            }
            _ => panic!("expected match"),
        }
    }

    #[test]
    fn probe_parens_in_infix() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx("(", true, &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { info, .. } => {
                assert_eq!(info.ty(), TokenType::OpenBracket);
            }
            _ => panic!("expected match for ("),
        }

        let ctx = make_probe_ctx(")", true, &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { info, .. } => {
                assert_eq!(info.ty(), TokenType::CloseBracket);
            }
            _ => panic!("expected match for )"),
        }
    }

    #[test]
    fn probe_comma_in_infix() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        let ctx = make_probe_ctx(",", true, &interner);
        match lib.probe(&ctx) {
            ProbeResult::Match { info, .. } => {
                assert_eq!(info.ty(), TokenType::Comma);
            }
            _ => panic!("expected match for ,"),
        }
    }

    #[test]
    fn probe_no_match_outside_infix() {
        let interner = Interner::new();
        let lib = SymbolicLib;

        // Variable outside infix mode shouldn't match
        let ctx = make_probe_ctx("x", false, &interner);
        let result = lib.probe(&ctx);
        assert!(matches!(result, ProbeResult::NoMatch));
    }

    #[test]
    fn is_identifier_valid() {
        assert!(is_identifier("x"));
        assert!(is_identifier("foo"));
        assert!(is_identifier("_bar"));
        assert!(is_identifier("x1"));
        assert!(is_identifier("my_var"));
    }

    #[test]
    fn is_identifier_invalid() {
        assert!(!is_identifier(""));
        assert!(!is_identifier("123"));
        assert!(!is_identifier("1x"));
        assert!(!is_identifier("+"));
    }

    #[test]
    fn infix_operator_precedence() {
        // Higher precedence binds tighter
        let add = probe_infix_operator("+").unwrap().0;
        let mul = probe_infix_operator("*").unwrap().0;
        let pow = probe_infix_operator("^").unwrap().0;

        assert!(mul.precedence() > add.precedence());
        assert!(pow.precedence() > mul.precedence());
    }

    #[test]
    fn compile_returns_start_infix() {
        use rpl_lang::compile::OutputBuffer;
        use rpl_lang::library::CompileContext;

        let lib = SymbolicLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        let mut ctx = CompileContext::new(span, "'", &mut output, &mut interner, None, false);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, rpl_lang::library::CompileResult::StartInfix));
    }

    #[test]
    fn compile_returns_end_infix() {
        use rpl_lang::compile::OutputBuffer;
        use rpl_lang::library::CompileContext;

        let lib = SymbolicLib;
        let mut output = OutputBuffer::new();
        let mut interner = Interner::new();
        let span = Span::new(Pos::new(0), Pos::new(1));

        // In infix mode
        let mut ctx = CompileContext::new(span, "'", &mut output, &mut interner, None, true);

        let result = lib.compile(&mut ctx);
        assert!(matches!(result, rpl_lang::library::CompileResult::EndInfix));
    }

    // ========================================================================
    // Expression tree and formatting tests
    // ========================================================================

    #[test]
    fn format_simple_number() {
        let expr = Expr::Num(42.0);
        assert_eq!(format_infix(&expr), "42");
    }

    #[test]
    fn format_float_number() {
        let expr = Expr::Num(3.15);
        assert_eq!(format_infix(&expr), "3.15");
    }

    #[test]
    fn format_variable() {
        let expr = Expr::Var("x".to_string());
        assert_eq!(format_infix(&expr), "x");
    }

    #[test]
    fn format_binary_add() {
        let expr = Expr::Binary {
            op: "+",
            left: Box::new(Expr::Num(3.0)),
            right: Box::new(Expr::Num(4.0)),
            precedence: 8,
            right_assoc: false,
        };
        assert_eq!(format_infix(&expr), "3 + 4");
    }

    #[test]
    fn format_precedence_no_parens() {
        // 3 + 4 * 5 should not need parens
        let expr = Expr::Binary {
            op: "+",
            left: Box::new(Expr::Num(3.0)),
            right: Box::new(Expr::Binary {
                op: "*",
                left: Box::new(Expr::Num(4.0)),
                right: Box::new(Expr::Num(5.0)),
                precedence: 10,
                right_assoc: false,
            }),
            precedence: 8,
            right_assoc: false,
        };
        assert_eq!(format_infix(&expr), "3 + 4 * 5");
    }

    #[test]
    fn format_precedence_with_parens() {
        // (3 + 4) * 5 needs parens
        let expr = Expr::Binary {
            op: "*",
            left: Box::new(Expr::Binary {
                op: "+",
                left: Box::new(Expr::Num(3.0)),
                right: Box::new(Expr::Num(4.0)),
                precedence: 8,
                right_assoc: false,
            }),
            right: Box::new(Expr::Num(5.0)),
            precedence: 10,
            right_assoc: false,
        };
        assert_eq!(format_infix(&expr), "(3 + 4) * 5");
    }

    #[test]
    fn format_right_associative() {
        // 2 ^ 3 ^ 4 should be 2 ^ (3 ^ 4) - right associative
        // When formatted from tree, the right-most is evaluated first
        let expr = Expr::Binary {
            op: "^",
            left: Box::new(Expr::Num(2.0)),
            right: Box::new(Expr::Binary {
                op: "^",
                left: Box::new(Expr::Num(3.0)),
                right: Box::new(Expr::Num(4.0)),
                precedence: 12,
                right_assoc: true,
            }),
            precedence: 12,
            right_assoc: true,
        };
        // Right child at same precedence doesn't need parens for right-assoc
        assert_eq!(format_infix(&expr), "2 ^ 3 ^ 4");
    }

    #[test]
    fn format_left_associative_needs_parens() {
        // a - (b - c) needs parens because - is left associative
        let expr = Expr::Binary {
            op: "-",
            left: Box::new(Expr::Var("a".to_string())),
            right: Box::new(Expr::Binary {
                op: "-",
                left: Box::new(Expr::Var("b".to_string())),
                right: Box::new(Expr::Var("c".to_string())),
                precedence: 8,
                right_assoc: false,
            }),
            precedence: 8,
            right_assoc: false,
        };
        assert_eq!(format_infix(&expr), "a - (b - c)");
    }

    #[test]
    fn format_unary_minus() {
        let expr = Expr::Unary {
            op: "-",
            arg: Box::new(Expr::Num(5.0)),
        };
        assert_eq!(format_infix(&expr), "-5");
    }
}
