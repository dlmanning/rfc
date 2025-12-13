/// Semantic classification of a token for highlighting and analysis.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SemanticKind {
    Number,
    String,
    Command,
    Keyword,
    Operator,
    Variable,
    Definition,
    Local,
    Parameter,
    Comment,
    Bracket,
    Unit,
    Constant,
    Invalid,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn semantic_kind_copy() {
        let kind = SemanticKind::Number;
        let kind2 = kind;
        assert_eq!(kind, kind2);
    }

    #[test]
    fn semantic_kind_eq() {
        assert_eq!(SemanticKind::Number, SemanticKind::Number);
        assert_ne!(SemanticKind::Number, SemanticKind::String);
    }

    #[test]
    fn semantic_kind_debug() {
        assert_eq!(format!("{:?}", SemanticKind::Command), "Command");
        assert_eq!(format!("{:?}", SemanticKind::Invalid), "Invalid");
    }
}
