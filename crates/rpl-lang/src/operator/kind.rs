/// The kind of operator for type-based dispatch.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OperatorKind {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Inv,
    Pow,
    Sqrt,
    Abs,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Cmp,

    // Logical
    And,
    Or,
    Xor,
    Not,

    // Collection
    Size,
    Get,
    Put,
    Head,
    Tail,

    // Conversion
    ToReal,
    ToInt,
    ToString,

    // Evaluation
    Eval,
    Apply,

    // Identity
    Same,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn operator_kind_copy() {
        let kind = OperatorKind::Add;
        let kind2 = kind;
        assert_eq!(kind, kind2);
    }

    #[test]
    fn operator_kind_hash() {
        let mut set = HashSet::new();
        set.insert(OperatorKind::Add);
        set.insert(OperatorKind::Sub);
        set.insert(OperatorKind::Add); // duplicate

        assert_eq!(set.len(), 2);
        assert!(set.contains(&OperatorKind::Add));
        assert!(set.contains(&OperatorKind::Sub));
    }

    #[test]
    fn operator_kind_debug() {
        assert_eq!(format!("{:?}", OperatorKind::Add), "Add");
        assert_eq!(format!("{:?}", OperatorKind::Neg), "Neg");
    }
}
