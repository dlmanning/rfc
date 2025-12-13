use crate::operator::OperatorKind;

/// Stack effect of a token.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum StackEffect {
    /// Fixed number of inputs and outputs.
    Fixed { consumes: u8, produces: u8 },
    /// Operator with typed dispatch.
    Operator { kind: OperatorKind, arity: u8 },
    /// Variable number of inputs, fixed outputs.
    Variadic { produces: u8 },
    /// Unknown effect (runtime dependent).
    Dynamic,
    /// Starts a construct (affects parsing, not stack).
    StartConstruct,
    /// Ends a construct.
    EndConstruct,
}

impl StackEffect {
    /// Create a fixed effect.
    pub fn fixed(consumes: u8, produces: u8) -> Self {
        Self::Fixed { consumes, produces }
    }

    /// Create an operator effect.
    pub fn operator(kind: OperatorKind, arity: u8) -> Self {
        Self::Operator { kind, arity }
    }

    /// Create a variadic effect.
    pub fn variadic(produces: u8) -> Self {
        Self::Variadic { produces }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_effect_fixed() {
        let effect = StackEffect::fixed(2, 1);
        assert_eq!(
            effect,
            StackEffect::Fixed {
                consumes: 2,
                produces: 1
            }
        );
    }

    #[test]
    fn stack_effect_operator() {
        let effect = StackEffect::operator(OperatorKind::Add, 2);
        assert_eq!(
            effect,
            StackEffect::Operator {
                kind: OperatorKind::Add,
                arity: 2
            }
        );
    }

    #[test]
    fn stack_effect_variadic() {
        let effect = StackEffect::variadic(1);
        assert_eq!(effect, StackEffect::Variadic { produces: 1 });
    }

    #[test]
    fn stack_effect_dynamic() {
        let effect = StackEffect::Dynamic;
        assert!(matches!(effect, StackEffect::Dynamic));
    }

    #[test]
    fn stack_effect_constructs() {
        assert!(matches!(
            StackEffect::StartConstruct,
            StackEffect::StartConstruct
        ));
        assert!(matches!(
            StackEffect::EndConstruct,
            StackEffect::EndConstruct
        ));
    }
}
