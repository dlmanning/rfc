use crate::library::ConstructKind;

/// Scope identifier (stub for later phases).
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct ScopeId(pub u32);

/// Parser state at a point in the source.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct ParseState {
    pub construct_stack: Vec<ConstructKind>,
    pub infix_depth: u8,
    pub scope_stack: Vec<ScopeId>,
}

impl ParseState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if we're inside any construct.
    pub fn in_construct(&self) -> bool {
        !self.construct_stack.is_empty()
    }

    /// Get the current (innermost) construct kind.
    pub fn current_construct(&self) -> Option<ConstructKind> {
        self.construct_stack.last().copied()
    }

    /// Check if we're in infix mode.
    pub fn in_infix(&self) -> bool {
        self.infix_depth > 0
    }

    /// Push a new construct onto the stack.
    pub fn push_construct(&mut self, kind: ConstructKind) {
        self.construct_stack.push(kind);
    }

    /// Pop the current construct from the stack.
    pub fn pop_construct(&mut self) -> Option<ConstructKind> {
        self.construct_stack.pop()
    }

    /// Enter infix mode.
    pub fn enter_infix(&mut self) {
        self.infix_depth += 1;
    }

    /// Exit infix mode.
    pub fn exit_infix(&mut self) {
        self.infix_depth = self.infix_depth.saturating_sub(1);
    }
}

/// State of a source line for incremental analysis.
#[derive(Clone, Debug)]
pub struct LineState {
    /// Hash of the line content for change detection.
    pub hash: u64,
    /// Index of the first token on this line.
    pub first_token: u32,
    /// Number of tokens on this line.
    pub token_count: u32,
    /// Parser state before this line.
    pub state_before: ParseState,
    /// Parser state after this line.
    pub state_after: ParseState,
}

impl LineState {
    pub fn new(hash: u64, first_token: u32, state_before: ParseState) -> Self {
        Self {
            hash,
            first_token,
            token_count: 0,
            state_before,
            state_after: ParseState::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_state_default() {
        let state = ParseState::new();
        assert!(!state.in_construct());
        assert!(!state.in_infix());
        assert!(state.current_construct().is_none());
    }

    #[test]
    fn parse_state_constructs() {
        let mut state = ParseState::new();

        state.push_construct(ConstructKind::Program);
        assert!(state.in_construct());
        assert_eq!(state.current_construct(), Some(ConstructKind::Program));

        state.push_construct(ConstructKind::List);
        assert_eq!(state.current_construct(), Some(ConstructKind::List));

        assert_eq!(state.pop_construct(), Some(ConstructKind::List));
        assert_eq!(state.current_construct(), Some(ConstructKind::Program));

        assert_eq!(state.pop_construct(), Some(ConstructKind::Program));
        assert!(!state.in_construct());
    }

    #[test]
    fn parse_state_infix() {
        let mut state = ParseState::new();
        assert!(!state.in_infix());

        state.enter_infix();
        assert!(state.in_infix());
        assert_eq!(state.infix_depth, 1);

        state.enter_infix();
        assert_eq!(state.infix_depth, 2);

        state.exit_infix();
        assert!(state.in_infix());

        state.exit_infix();
        assert!(!state.in_infix());

        // Should not underflow
        state.exit_infix();
        assert_eq!(state.infix_depth, 0);
    }

    #[test]
    fn parse_state_clone_eq() {
        let mut state1 = ParseState::new();
        state1.push_construct(ConstructKind::Program);
        state1.enter_infix();

        let state2 = state1.clone();
        assert_eq!(state1, state2);
    }

    #[test]
    fn line_state_new() {
        let state_before = ParseState::new();
        let line = LineState::new(12345, 10, state_before.clone());

        assert_eq!(line.hash, 12345);
        assert_eq!(line.first_token, 10);
        assert_eq!(line.token_count, 0);
        assert_eq!(line.state_before, state_before);
    }
}
