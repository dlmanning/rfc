//! Test library for declarative control flow patterns.
//!
//! This is a minimal library to test the code generation for control patterns.

rpl_macros::define_library! {
    pub library ControlTestLib(99, "ControlTest");

    control_patterns {
        // Basic if-then-else: IF cond THEN body END (with optional ELSE)
        if_then_else(IF, THEN, END) { alt: ELSE };

        // Do-until loop: DO body UNTIL cond
        do_until(DO, UNTIL);

        // While-repeat loop: WHILE cond REPEAT body END
        while_repeat(WHILE, REPEAT);

        // Start-next loop: start finish START body NEXT
        start_next(START, NEXT) { step: STEP };

        // Inline conditionals: cond obj IFT  or  cond true_obj false_obj IFTE
        inline_conditional(IFT, IFTE);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_core::token::SemanticKind;
    use rpl_lang::library::{Library, ProbeContext, ProbeResult, StackEffect};

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    #[test]
    fn probe_if_keyword() {
        let interner = Interner::new();
        let lib = ControlTestLib;
        let ctx = make_probe_ctx("IF", &interner);
        let result = lib.probe(&ctx);

        match result {
            ProbeResult::Match { semantic, .. } => {
                assert_eq!(semantic, SemanticKind::Keyword);
            }
            _ => panic!("expected match for IF"),
        }
    }

    #[test]
    fn probe_then_keyword() {
        let interner = Interner::new();
        let lib = ControlTestLib;
        let ctx = make_probe_ctx("THEN", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::Match { .. }));
    }

    #[test]
    fn probe_case_insensitive() {
        let interner = Interner::new();
        let lib = ControlTestLib;

        for kw in &["if", "If", "IF", "then", "Then", "THEN", "while", "WHILE"] {
            let ctx = make_probe_ctx(kw, &interner);
            assert!(
                matches!(lib.probe(&ctx), ProbeResult::Match { .. }),
                "Should match {}",
                kw
            );
        }
    }

    #[test]
    fn probe_no_match() {
        let interner = Interner::new();
        let lib = ControlTestLib;
        let ctx = make_probe_ctx("foo", &interner);
        assert!(matches!(lib.probe(&ctx), ProbeResult::NoMatch));
    }

    #[test]
    fn stack_effect_if() {
        let lib = ControlTestLib;
        assert!(matches!(lib.stack_effect("IF"), StackEffect::StartConstruct));
    }

    #[test]
    fn stack_effect_then() {
        let lib = ControlTestLib;
        match lib.stack_effect("THEN") {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 1);
                assert_eq!(produces, 0);
            }
            _ => panic!("expected Fixed stack effect for THEN"),
        }
    }

    #[test]
    fn stack_effect_end() {
        let lib = ControlTestLib;
        assert!(matches!(lib.stack_effect("END"), StackEffect::EndConstruct));
    }

    #[test]
    fn stack_effect_ift() {
        let lib = ControlTestLib;
        match lib.stack_effect("IFT") {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 2);
                assert_eq!(produces, 1);
            }
            _ => panic!("expected Fixed stack effect for IFT"),
        }
    }

    #[test]
    fn stack_effect_ifte() {
        let lib = ControlTestLib;
        match lib.stack_effect("IFTE") {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 3);
                assert_eq!(produces, 1);
            }
            _ => panic!("expected Fixed stack effect for IFTE"),
        }
    }

    #[test]
    fn stack_effect_start() {
        let lib = ControlTestLib;
        match lib.stack_effect("START") {
            StackEffect::Fixed { consumes, produces } => {
                assert_eq!(consumes, 2);
                assert_eq!(produces, 0);
            }
            _ => panic!("expected Fixed stack effect for START"),
        }
    }
}
