/// The syntactic type of a token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenType {
    Atom,
    BinaryLeft,
    BinaryRight,
    Prefix,
    Postfix,
    Function,
    OpenBracket,
    CloseBracket,
    Comma,
}

impl TokenType {
    /// Check if this token type is an operator.
    pub fn is_operator(self) -> bool {
        matches!(
            self,
            TokenType::BinaryLeft | TokenType::BinaryRight | TokenType::Prefix | TokenType::Postfix
        )
    }

    /// Check if this token type is a binary operator.
    pub fn is_binary(self) -> bool {
        matches!(self, TokenType::BinaryLeft | TokenType::BinaryRight)
    }
}

/// Metadata about a token for parsing.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TokenInfo {
    length: u8,
    ty: TokenType,
    precedence: u8,
    nargs: u8,
    right_assoc: bool,
}

impl TokenInfo {
    pub fn length(self) -> u8 {
        self.length
    }

    pub fn ty(self) -> TokenType {
        self.ty
    }

    pub fn precedence(self) -> u8 {
        self.precedence
    }

    pub fn nargs(self) -> u8 {
        self.nargs
    }

    pub fn right_assoc(self) -> bool {
        self.right_assoc
    }

    /// Create info for an atom (literal, variable, etc.).
    pub fn atom(len: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::Atom,
            precedence: 0,
            nargs: 0,
            right_assoc: false,
        }
    }

    /// Create info for a left-associative binary operator.
    pub fn binary_left(len: u8, prec: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::BinaryLeft,
            precedence: prec,
            nargs: 2,
            right_assoc: false,
        }
    }

    /// Create info for a right-associative binary operator.
    pub fn binary_right(len: u8, prec: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::BinaryRight,
            precedence: prec,
            nargs: 2,
            right_assoc: true,
        }
    }

    /// Create info for a prefix operator.
    pub fn prefix(len: u8, prec: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::Prefix,
            precedence: prec,
            nargs: 1,
            right_assoc: false,
        }
    }

    /// Create info for a postfix operator.
    pub fn postfix(len: u8, prec: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::Postfix,
            precedence: prec,
            nargs: 1,
            right_assoc: false,
        }
    }

    /// Create info for a function call.
    pub fn function(len: u8, nargs: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::Function,
            precedence: 0,
            nargs,
            right_assoc: false,
        }
    }

    /// Create info for an opening bracket.
    pub fn open_bracket(len: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::OpenBracket,
            precedence: 0,
            nargs: 0,
            right_assoc: false,
        }
    }

    /// Create info for a closing bracket.
    pub fn close_bracket(len: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::CloseBracket,
            precedence: 0,
            nargs: 0,
            right_assoc: false,
        }
    }

    /// Create info for a comma separator.
    pub fn comma(len: u8) -> Self {
        Self {
            length: len,
            ty: TokenType::Comma,
            precedence: 0,
            nargs: 0,
            right_assoc: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_type_is_operator() {
        assert!(!TokenType::Atom.is_operator());
        assert!(TokenType::BinaryLeft.is_operator());
        assert!(TokenType::BinaryRight.is_operator());
        assert!(TokenType::Prefix.is_operator());
        assert!(TokenType::Postfix.is_operator());
        assert!(!TokenType::Function.is_operator());
        assert!(!TokenType::OpenBracket.is_operator());
        assert!(!TokenType::CloseBracket.is_operator());
        assert!(!TokenType::Comma.is_operator());
    }

    #[test]
    fn token_type_is_binary() {
        assert!(!TokenType::Atom.is_binary());
        assert!(TokenType::BinaryLeft.is_binary());
        assert!(TokenType::BinaryRight.is_binary());
        assert!(!TokenType::Prefix.is_binary());
        assert!(!TokenType::Postfix.is_binary());
        assert!(!TokenType::Function.is_binary());
    }

    #[test]
    fn token_info_atom() {
        let info = TokenInfo::atom(5);
        assert_eq!(info.length(), 5);
        assert_eq!(info.ty(), TokenType::Atom);
        assert_eq!(info.precedence(), 0);
        assert_eq!(info.nargs(), 0);
        assert!(!info.right_assoc());
    }

    #[test]
    fn token_info_binary_left() {
        let info = TokenInfo::binary_left(1, 10);
        assert_eq!(info.length(), 1);
        assert_eq!(info.ty(), TokenType::BinaryLeft);
        assert_eq!(info.precedence(), 10);
        assert_eq!(info.nargs(), 2);
        assert!(!info.right_assoc());
    }

    #[test]
    fn token_info_binary_right() {
        let info = TokenInfo::binary_right(1, 15);
        assert_eq!(info.length(), 1);
        assert_eq!(info.ty(), TokenType::BinaryRight);
        assert_eq!(info.precedence(), 15);
        assert_eq!(info.nargs(), 2);
        assert!(info.right_assoc());
    }

    #[test]
    fn token_info_prefix() {
        let info = TokenInfo::prefix(1, 20);
        assert_eq!(info.length(), 1);
        assert_eq!(info.ty(), TokenType::Prefix);
        assert_eq!(info.precedence(), 20);
        assert_eq!(info.nargs(), 1);
    }

    #[test]
    fn token_info_postfix() {
        let info = TokenInfo::postfix(1, 25);
        assert_eq!(info.length(), 1);
        assert_eq!(info.ty(), TokenType::Postfix);
        assert_eq!(info.precedence(), 25);
        assert_eq!(info.nargs(), 1);
    }

    #[test]
    fn token_info_function() {
        let info = TokenInfo::function(3, 2);
        assert_eq!(info.length(), 3);
        assert_eq!(info.ty(), TokenType::Function);
        assert_eq!(info.nargs(), 2);
    }

    #[test]
    fn token_info_brackets() {
        let open = TokenInfo::open_bracket(1);
        assert_eq!(open.ty(), TokenType::OpenBracket);

        let close = TokenInfo::close_bracket(1);
        assert_eq!(close.ty(), TokenType::CloseBracket);
    }

    #[test]
    fn token_info_comma() {
        let info = TokenInfo::comma(1);
        assert_eq!(info.ty(), TokenType::Comma);
    }
}
