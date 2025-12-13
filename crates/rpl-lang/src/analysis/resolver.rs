use rpl_core::token::TokenType;

use super::token::ResolvedToken;

/// Resolve context-dependent token properties.
///
/// This pass resolves:
/// - Unary/binary ambiguity for operators
/// - Construct depth tracking
pub fn resolve_context(tokens: &mut [ResolvedToken]) {
    let mut previous_type: Option<TokenType> = None;
    let mut construct_depth: u16 = 0;

    for token in tokens.iter_mut() {
        // Update construct depth based on token context
        // Depth reflects state AFTER the token's effect
        if token.context.starts_construct {
            construct_depth += 1;
        }
        if token.context.ends_construct {
            construct_depth = construct_depth.saturating_sub(1);
        }
        token.context.construct_depth = construct_depth;

        // Resolve unary/binary ambiguity
        let token_type = token.info.ty();
        if is_potentially_unary(&token_type) {
            if should_be_unary(previous_type) {
                token.context.resolved_unary = true;
                token.context.resolved_binary = false;
            } else {
                token.context.resolved_unary = false;
                token.context.resolved_binary = true;
            }
        }

        previous_type = Some(token_type);
    }
}

/// Check if a token type could be either unary or binary.
fn is_potentially_unary(ty: &TokenType) -> bool {
    matches!(ty, TokenType::BinaryLeft | TokenType::BinaryRight)
}

/// Determine if an operator should be treated as unary based on the previous token.
///
/// Returns true if the operator should be unary (prefix), false if binary.
fn should_be_unary(previous: Option<TokenType>) -> bool {
    match previous {
        // At start of expression: unary
        None => true,
        Some(ty) => match ty {
            // After another operator or open bracket: unary
            TokenType::BinaryLeft
            | TokenType::BinaryRight
            | TokenType::Prefix
            | TokenType::OpenBracket
            | TokenType::Comma => true,
            // After atom, close bracket, postfix, or function: binary
            TokenType::Atom
            | TokenType::CloseBracket
            | TokenType::Postfix
            | TokenType::Function => false,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Pos, Span};
    use crate::library::LibraryId;
    use rpl_core::token::{SemanticKind, TokenInfo};

    fn make_token(ty: TokenType, semantic: SemanticKind) -> ResolvedToken {
        ResolvedToken::new(
            Span::new(Pos::new(0), Pos::new(1)),
            LibraryId::new(1),
            match ty {
                TokenType::Atom => TokenInfo::atom(1),
                TokenType::BinaryLeft => TokenInfo::binary_left(1, 10),
                TokenType::BinaryRight => TokenInfo::binary_right(1, 10),
                TokenType::Prefix => TokenInfo::prefix(1, 20),
                TokenType::Postfix => TokenInfo::postfix(1, 20),
                TokenType::Function => TokenInfo::function(1, 2),
                TokenType::OpenBracket => TokenInfo::open_bracket(1),
                TokenType::CloseBracket => TokenInfo::close_bracket(1),
                TokenType::Comma => TokenInfo::comma(1),
            },
            semantic,
        )
    }

    fn make_number() -> ResolvedToken {
        make_token(TokenType::Atom, SemanticKind::Number)
    }

    fn make_minus() -> ResolvedToken {
        make_token(TokenType::BinaryLeft, SemanticKind::Operator)
    }

    fn make_open_paren() -> ResolvedToken {
        make_token(TokenType::OpenBracket, SemanticKind::Bracket)
    }

    fn make_close_paren() -> ResolvedToken {
        make_token(TokenType::CloseBracket, SemanticKind::Bracket)
    }

    fn make_multiply() -> ResolvedToken {
        make_token(TokenType::BinaryLeft, SemanticKind::Operator)
    }

    #[test]
    fn resolve_binary_minus() {
        // 3 - 4: minus should be binary
        let mut tokens = vec![make_number(), make_minus(), make_number()];
        resolve_context(&mut tokens);

        assert!(!tokens[1].context.resolved_unary);
        assert!(tokens[1].context.resolved_binary);
    }

    #[test]
    fn resolve_unary_after_multiply() {
        // 3 * -4: minus should be unary
        let mut tokens = vec![make_number(), make_multiply(), make_minus(), make_number()];
        resolve_context(&mut tokens);

        // Multiply is binary (after number)
        assert!(tokens[1].context.resolved_binary);
        // Minus is unary (after multiply)
        assert!(tokens[2].context.resolved_unary);
        assert!(!tokens[2].context.resolved_binary);
    }

    #[test]
    fn resolve_unary_in_parens() {
        // (-4): minus should be unary
        let mut tokens = vec![
            make_open_paren(),
            make_minus(),
            make_number(),
            make_close_paren(),
        ];
        resolve_context(&mut tokens);

        assert!(tokens[1].context.resolved_unary);
        assert!(!tokens[1].context.resolved_binary);
    }

    #[test]
    fn resolve_binary_then_unary() {
        // 3 - -4: first minus binary, second unary
        let mut tokens = vec![make_number(), make_minus(), make_minus(), make_number()];
        resolve_context(&mut tokens);

        // First minus: after number -> binary
        assert!(!tokens[1].context.resolved_unary);
        assert!(tokens[1].context.resolved_binary);

        // Second minus: after binary -> unary
        assert!(tokens[2].context.resolved_unary);
        assert!(!tokens[2].context.resolved_binary);
    }

    #[test]
    fn resolve_unary_at_start() {
        // -4: minus should be unary (start of expression)
        let mut tokens = vec![make_minus(), make_number()];
        resolve_context(&mut tokens);

        assert!(tokens[0].context.resolved_unary);
        assert!(!tokens[0].context.resolved_binary);
    }

    #[test]
    fn construct_depth_tracking() {
        let mut tokens = vec![
            make_number(), // depth 0 (no change)
            {
                let mut t = make_open_paren();
                t.context.starts_construct = true;
                t
            }, // depth 1 (after start)
            make_number(), // depth 1 (inside)
            {
                let mut t = make_close_paren();
                t.context.ends_construct = true;
                t
            }, // depth 0 (after end)
            make_number(), // depth 0 (outside)
        ];

        resolve_context(&mut tokens);

        assert_eq!(tokens[0].context.construct_depth, 0);
        assert_eq!(tokens[1].context.construct_depth, 1);
        assert_eq!(tokens[2].context.construct_depth, 1);
        assert_eq!(tokens[3].context.construct_depth, 0); // depth after decrement
        assert_eq!(tokens[4].context.construct_depth, 0);
    }

    #[test]
    fn nested_construct_depth() {
        let mut tokens = vec![
            {
                let mut t = make_open_paren();
                t.context.starts_construct = true;
                t
            }, // depth 1 (after first start)
            {
                let mut t = make_open_paren();
                t.context.starts_construct = true;
                t
            }, // depth 2 (after second start)
            make_number(), // depth 2 (inside both)
            {
                let mut t = make_close_paren();
                t.context.ends_construct = true;
                t
            }, // depth 1 (after first end)
            {
                let mut t = make_close_paren();
                t.context.ends_construct = true;
                t
            }, // depth 0 (after second end)
        ];

        resolve_context(&mut tokens);

        assert_eq!(tokens[0].context.construct_depth, 1);
        assert_eq!(tokens[1].context.construct_depth, 2);
        assert_eq!(tokens[2].context.construct_depth, 2);
        assert_eq!(tokens[3].context.construct_depth, 1); // depth after decrement
        assert_eq!(tokens[4].context.construct_depth, 0);
    }
}
