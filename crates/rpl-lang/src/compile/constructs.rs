use rpl_core::{Span, Symbol};
use crate::library::{ConstructKind, LibraryId};

/// A construct being compiled (program, list, etc.).
#[derive(Clone, Debug)]
pub struct Construct {
    /// The kind of construct.
    pub kind: ConstructKind,
    /// The library that opened this construct.
    pub lib: LibraryId,
    /// Position in the output buffer where this construct started.
    pub start_pos: usize,
    /// Span of the opening token.
    pub open_span: Span,
    /// Number of arguments/elements seen so far.
    pub arg_count: usize,
    /// Extra data for control flow constructs.
    pub control_data: ControlData,
    /// Whether the body has started (for LocalBinding constructs).
    pub body_started: bool,
    /// Local parameter symbols (for LocalBinding constructs).
    pub local_params: Vec<Symbol>,
}

/// Extra data for control flow constructs.
#[derive(Clone, Debug, Default)]
pub struct ControlData {
    /// Position of THEN's conditional jump target (for IF).
    pub then_jump_target: Option<usize>,
    /// Position of ELSE's unconditional jump target (for IF).
    pub else_jump_target: Option<usize>,
    /// Position to jump back to (for loops).
    pub loop_start: Option<usize>,
    /// Position of skip_target to patch (for START/FOR zero-iteration handling).
    pub skip_target: Option<usize>,
    /// Positions of exit jumps to patch at ENDCASE (for CASE).
    pub case_exit_jumps: Vec<usize>,
}

impl Construct {
    /// Create a new construct.
    pub fn new(kind: ConstructKind, lib: LibraryId, start_pos: usize, open_span: Span) -> Self {
        Self {
            kind,
            lib,
            start_pos,
            open_span,
            arg_count: 0,
            control_data: ControlData::default(),
            body_started: false,
            local_params: Vec::new(),
        }
    }
}

/// Stack of constructs being compiled.
pub struct ConstructStack {
    stack: Vec<Construct>,
}

impl ConstructStack {
    /// Create a new empty construct stack.
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Push a construct onto the stack.
    pub fn push(&mut self, construct: Construct) {
        self.stack.push(construct);
    }

    /// Pop a construct from the stack.
    pub fn pop(&mut self) -> Option<Construct> {
        self.stack.pop()
    }

    /// Get a reference to the top construct.
    pub fn top(&self) -> Option<&Construct> {
        self.stack.last()
    }

    /// Get a mutable reference to the top construct.
    pub fn top_mut(&mut self) -> Option<&mut Construct> {
        self.stack.last_mut()
    }

    /// Get the current nesting depth.
    pub fn depth(&self) -> usize {
        self.stack.len()
    }

    /// Check if the stack is empty.
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    /// Iterate over constructs from bottom to top.
    pub fn iter(&self) -> std::slice::Iter<'_, Construct> {
        self.stack.iter()
    }
}

impl Default for ConstructStack {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::Pos;

    fn dummy_span() -> Span {
        Span::new(Pos::new(0), Pos::new(1))
    }

    #[test]
    fn construct_stack_new() {
        let stack = ConstructStack::new();
        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);
    }

    #[test]
    fn construct_stack_push_pop() {
        let mut stack = ConstructStack::new();
        let lib = LibraryId::new(10);

        stack.push(Construct::new(ConstructKind::Program, lib, 0, dummy_span()));
        assert_eq!(stack.depth(), 1);
        assert!(!stack.is_empty());

        let popped = stack.pop();
        assert!(popped.is_some());
        assert!(matches!(popped.unwrap().kind, ConstructKind::Program));
        assert!(stack.is_empty());
    }

    #[test]
    fn construct_stack_top() {
        let mut stack = ConstructStack::new();
        let lib = LibraryId::new(10);

        assert!(stack.top().is_none());

        stack.push(Construct::new(ConstructKind::Program, lib, 0, dummy_span()));
        stack.push(Construct::new(ConstructKind::List, lib, 5, dummy_span()));

        let top = stack.top().unwrap();
        assert!(matches!(top.kind, ConstructKind::List));
        assert_eq!(top.start_pos, 5);
    }

    #[test]
    fn construct_stack_top_mut() {
        let mut stack = ConstructStack::new();
        let lib = LibraryId::new(10);

        stack.push(Construct::new(ConstructKind::Program, lib, 0, dummy_span()));

        if let Some(top) = stack.top_mut() {
            top.arg_count = 5;
        }

        assert_eq!(stack.top().unwrap().arg_count, 5);
    }

    #[test]
    fn construct_stack_nested() {
        let mut stack = ConstructStack::new();
        let lib = LibraryId::new(10);

        stack.push(Construct::new(ConstructKind::Program, lib, 0, dummy_span()));
        stack.push(Construct::new(ConstructKind::List, lib, 5, dummy_span()));
        stack.push(Construct::new(
            ConstructKind::Symbolic,
            lib,
            10,
            dummy_span(),
        ));

        assert_eq!(stack.depth(), 3);

        stack.pop();
        assert_eq!(stack.depth(), 2);
        assert!(matches!(stack.top().unwrap().kind, ConstructKind::List));

        stack.pop();
        assert_eq!(stack.depth(), 1);
        assert!(matches!(stack.top().unwrap().kind, ConstructKind::Program));
    }

    #[test]
    fn construct_new() {
        let lib = LibraryId::new(42);
        let span = dummy_span();
        let c = Construct::new(ConstructKind::For, lib, 100, span);

        assert!(matches!(c.kind, ConstructKind::For));
        assert_eq!(c.lib.as_u16(), 42);
        assert_eq!(c.start_pos, 100);
        assert_eq!(c.arg_count, 0);
    }
}
