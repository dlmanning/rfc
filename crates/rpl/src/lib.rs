//! RPL: Two-pass RPL compiler with stack-based bytecode.
//!
//! This crate implements a clean two-pass compilation pipeline:
//! - **Parse**: Tokens → IR (intermediate representation)
//! - **Lower**: IR → Bytecode
//! - **Execute**: Bytecode runs on our VM
//!
//! # Architecture
//!
//! ```text
//! Tokens → IR → Bytecode → VM
//!          ↑        ↑        ↑
//!       Phase 1  Phase 2  Phase 3
//!       (parse)  (lower)  (execute)
//! ```
//!
//! # Key Design Decisions
//!
//! 1. **Three grammar primitives**: Program `« »`, List `{ }`, Symbolic `' '`
//! 2. **No complex literals**: Complex numbers via commands, not syntax
//! 3. **No closures**: Variables in nested programs use runtime lookup
//! 4. **Self-contained**: Own VM, Value type, and compilation infrastructure
//!
//! # Example
//!
//! ```
//! use rpl::{eval, value::Value};
//!
//! let result = eval("1 2 +").unwrap();
//! assert_eq!(result, vec![Value::integer(3)]);
//! ```

// Core types (formerly rpl-core)
pub mod core;
pub mod error;
pub mod token;

// Source file management (formerly rpl-source)
pub mod source;

// Compiler and VM
pub mod analysis;
pub mod ir;
pub mod libs;
pub mod lower;
pub mod parse;
pub mod registry;
pub mod serialize;
pub mod session;
pub mod symbolic;
pub mod types;
pub mod value;
pub mod vm;

// Re-export core types at crate root
pub use core::{Interner, Pos, Span, Spanned, Symbol, TypeId};
pub use error::{Diagnostic, DiagnosticBuilder, ErrorCode, Severity, Suggestion};
pub use token::{SemanticKind, TokenInfo, TokenType};

// Re-export source types at crate root
pub use source::{DiagnosticRenderer, LineCol, SourceCache, SourceFile, SourceId};

// Re-export commonly used types at crate root
pub use lower::CompiledProgram;
pub use session::{Session, SessionConfig, EvalError};
pub use session::lsp;
pub use session::debug as debug_helpers;
pub use vm::{DebugState, DebugMode, DebugEvent, ExecuteOutcome, ReturnEntry};

/// Evaluate RPL source code and return the resulting stack.
///
/// This is a convenience function that:
/// 1. Parses the source to IR
/// 2. Lowers the IR to bytecode
/// 3. Executes the bytecode on a VM
/// 4. Returns the final stack contents
pub fn eval(source: &str) -> Result<Vec<value::Value>, String> {
    let registry = registry::Registry::with_core();
    let mut interner = Interner::new();

    // Parse
    let nodes = parse::parse(source, &registry, &mut interner)
        .map_err(|e| format!("parse error: {}", e))?;

    // Lower
    let program = lower::lower(&nodes, &registry, &interner)
        .map_err(|e| format!("lower error: {}", e))?;

    // Execute
    let mut vm = vm::Vm::new();
    vm.execute(&program.code, &registry, &program.string_table)
        .map_err(|e| format!("runtime error: {}", e))?;

    // Return stack contents
    Ok(vm.stack.as_slice().to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;
    use value::Value;

    #[test]
    fn eval_one_two_plus() {
        let result = eval("1 2 +").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_arithmetic_chain() {
        // 10 5 - 3 * = (10 - 5) * 3 = 15
        let result = eval("10 5 - 3 *").unwrap();
        assert_eq!(result, vec![Value::integer(15)]);
    }

    #[test]
    fn eval_multiple_values() {
        let result = eval("1 2 3").unwrap();
        assert_eq!(result, vec![
            Value::integer(1),
            Value::integer(2),
            Value::integer(3),
        ]);
    }

    #[test]
    fn eval_real_arithmetic() {
        let result = eval("3.0 2.0 /").unwrap();
        assert_eq!(result, vec![Value::real(1.5)]);
    }

    #[test]
    fn eval_mixed_int_real_add() {
        // int + real -> real (TOS is real, NOS is int)
        let result = eval("3 2.5 +").unwrap();
        assert_eq!(result, vec![Value::real(5.5)]);

        // real + int -> real (TOS is int, NOS is real)
        let result = eval("2.5 3 +").unwrap();
        assert_eq!(result, vec![Value::real(5.5)]);
    }

    #[test]
    fn eval_mixed_int_real_sub() {
        let result = eval("5 2.5 -").unwrap();
        assert_eq!(result, vec![Value::real(2.5)]);

        let result = eval("5.5 2 -").unwrap();
        assert_eq!(result, vec![Value::real(3.5)]);
    }

    #[test]
    fn eval_mixed_int_real_mul() {
        let result = eval("3 2.5 *").unwrap();
        assert_eq!(result, vec![Value::real(7.5)]);

        let result = eval("2.5 4 *").unwrap();
        assert_eq!(result, vec![Value::real(10.0)]);
    }

    #[test]
    fn eval_mixed_int_real_div() {
        let result = eval("5 2.0 /").unwrap();
        assert_eq!(result, vec![Value::real(2.5)]);

        let result = eval("5.0 2 /").unwrap();
        assert_eq!(result, vec![Value::real(2.5)]);
    }

    #[test]
    fn eval_mixed_int_real_comparison() {
        // int < real
        let result = eval("3 3.5 <").unwrap();
        assert_eq!(result, vec![Value::integer(1)]);

        // real > int
        let result = eval("3.5 3 >").unwrap();
        assert_eq!(result, vec![Value::integer(1)]);

        // int == real (equal values)
        let result = eval("3 3.0 ==").unwrap();
        assert_eq!(result, vec![Value::integer(1)]);
    }

    #[test]
    fn eval_dup() {
        let result = eval("5 DUP +").unwrap();
        assert_eq!(result, vec![Value::integer(10)]);
    }

    #[test]
    fn eval_swap() {
        let result = eval("1 2 SWAP").unwrap();
        assert_eq!(result, vec![
            Value::integer(2),
            Value::integer(1),
        ]);
    }

    #[test]
    fn eval_drop() {
        let result = eval("1 2 3 DROP").unwrap();
        assert_eq!(result, vec![
            Value::integer(1),
            Value::integer(2),
        ]);
    }

    #[test]
    fn eval_comparison() {
        let result = eval("5 3 >").unwrap();
        assert_eq!(result, vec![Value::integer(1)]); // true

        let result = eval("3 5 >").unwrap();
        assert_eq!(result, vec![Value::integer(0)]); // false
    }

    #[test]
    fn eval_list() {
        let result = eval("{ 1 2 3 }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(1));
                assert_eq!(items[1], Value::integer(2));
                assert_eq!(items[2], Value::integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_empty_list() {
        let result = eval("{ }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 0);
            }
            _ => panic!("expected empty list"),
        }
    }

    #[test]
    fn eval_nested_list() {
        let result = eval("{ { 1 2 } { 3 } }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                // First nested list
                match &items[0] {
                    Value::List(inner) => assert_eq!(inner.len(), 2),
                    _ => panic!("expected inner list"),
                }
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_program() {
        // Program should create a program object on stack
        let result = eval("<< 1 2 + >>").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Program(_) => {} // Success
            _ => panic!("expected program, got {:?}", result[0]),
        }
    }

    #[test]
    fn eval_list_with_program() {
        // Program inside list
        let result = eval(r#"{ << 42 >> }"#).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 1, "list should have 1 program");
                assert!(items[0].as_program().is_some());
            }
            _ => panic!("expected list"),
        }

        // String and program inside list
        let result = eval(r#"{ "A" << 42 >> }"#).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2, "list should have string and program");
                assert_eq!(items[0].as_string(), Some("A"));
                assert!(items[1].as_program().is_some());
            }
            _ => panic!("expected list"),
        }

        // Nested list with string and program (CRLIB format)
        let result = eval(r#"{ { "NAME" << 42 >> } }"#).unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(outer) => {
                assert_eq!(outer.len(), 1, "outer list should have 1 inner list");
                match &outer[0] {
                    Value::List(inner) => {
                        assert_eq!(inner.len(), 2, "inner list should have 2 elements");
                        assert_eq!(inner[0].as_string(), Some("NAME"));
                        assert!(inner[1].as_program().is_some());
                    }
                    _ => panic!("expected inner list"),
                }
            }
            _ => panic!("expected outer list"),
        }
    }

    #[test]
    fn eval_program_eval() {
        // Create program and execute it
        let result = eval("<< 1 2 + >> EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_program_guillemets() {
        let result = eval("« 5 DUP * » EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(25)]);
    }

    #[test]
    fn eval_program_with_stack_args() {
        // Push args, then program, then eval
        let result = eval("10 3 << - >> EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(7)]);
    }

    #[test]
    fn eval_nested_programs() {
        // Outer program creates inner program and evals it
        let result = eval("<< << 42 >> EVAL >> EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    // ======== Flow control tests ========

    #[test]
    fn eval_if_then_true() {
        // IF with true condition (1 is true)
        let result = eval("IF 1 THEN 42 END").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_if_then_false() {
        // IF with false condition (0 is false)
        let result = eval("IF 0 THEN 42 END").unwrap();
        assert_eq!(result, vec![]); // Stack should be empty
    }

    #[test]
    fn eval_if_then_else_true() {
        // IF/ELSE with true condition
        let result = eval("IF 1 THEN 42 ELSE 99 END").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_if_then_else_false() {
        // IF/ELSE with false condition
        let result = eval("IF 0 THEN 42 ELSE 99 END").unwrap();
        assert_eq!(result, vec![Value::integer(99)]);
    }

    #[test]
    fn eval_if_with_condition_expression() {
        // Condition is an expression: 5 > 3 = true
        let result = eval("IF 5 3 > THEN 42 END").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);

        // Condition is false: 3 > 5 = false
        let result = eval("IF 3 5 > THEN 42 END").unwrap();
        assert_eq!(result, vec![]);
    }

    #[test]
    fn eval_if_with_body_operations() {
        // Body has multiple operations: push 10, push 5, add
        let result = eval("IF 1 THEN 10 5 + END").unwrap();
        assert_eq!(result, vec![Value::integer(15)]);
    }

    #[test]
    fn eval_nested_if() {
        // Nested IF: outer true, inner true
        let result = eval("IF 1 THEN IF 1 THEN 42 END END").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);

        // Nested IF: outer true, inner false
        let result = eval("IF 1 THEN IF 0 THEN 42 END END").unwrap();
        assert_eq!(result, vec![]);

        // Nested IF: outer false
        let result = eval("IF 0 THEN IF 1 THEN 42 END END").unwrap();
        assert_eq!(result, vec![]);
    }

    #[test]
    fn eval_nested_if_else() {
        // Nested IF/ELSE: outer true, inner true
        let result = eval("IF 1 THEN IF 1 THEN 1 ELSE 2 END ELSE 3 END").unwrap();
        assert_eq!(result, vec![Value::integer(1)]);

        // Nested IF/ELSE: outer true, inner false
        let result = eval("IF 1 THEN IF 0 THEN 1 ELSE 2 END ELSE 3 END").unwrap();
        assert_eq!(result, vec![Value::integer(2)]);

        // Nested IF/ELSE: outer false
        let result = eval("IF 0 THEN IF 1 THEN 1 ELSE 2 END ELSE 3 END").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_if_computed_branch() {
        // Computed condition using comparison
        let result = eval("IF 2 2 == THEN 42 ELSE 0 END").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);

        let result = eval("IF 2 3 == THEN 42 ELSE 0 END").unwrap();
        assert_eq!(result, vec![Value::integer(0)]);
    }

    // ======== Local variable tests ========

    #[test]
    fn eval_local_binding_single() {
        // Bind one value to a local and use it (HP syntax: -> x << body >>)
        let result = eval("42 -> x << x >>").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_local_binding_multiple() {
        // Bind multiple values: rightmost gets TOS
        // Stack: [1, 2] -> a=1, b=2 (b gets TOS)
        let result = eval("1 2 -> a b << a b + >>").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_local_binding_use_multiple_times() {
        // Use a local multiple times
        let result = eval("5 -> x << x x * >>").unwrap();
        assert_eq!(result, vec![Value::integer(25)]);
    }

    #[test]
    fn eval_local_binding_nested() {
        // Nested local bindings
        let result = eval("1 -> a << 2 -> b << a b + >> >>").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_local_binding_shadowing() {
        // Inner binding shadows outer
        let result = eval("1 -> x << 2 -> x << x >> >>").unwrap();
        assert_eq!(result, vec![Value::integer(2)]);
    }

    #[test]
    fn eval_local_binding_with_operations() {
        // Complex expression in body
        let result = eval("10 3 -> a b << a b - a b + * >>").unwrap();
        // (10 - 3) * (10 + 3) = 7 * 13 = 91
        assert_eq!(result, vec![Value::integer(91)]);
    }

    #[test]
    fn eval_local_binding_unicode_arrow() {
        // Unicode arrow and guillemet syntax
        let result = eval("42 → x « x »").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_local_not_visible_outside_scope() {
        // After the binding ends, x should not be resolved as a local
        // This will push x as an unresolved symbol which will cause a runtime error
        // For now, we just test that the binding scope is limited
        let result = eval("1 -> x << x >> 2");
        // The result should have 1 (from x) and 2
        assert!(result.is_ok());
        let stack = result.unwrap();
        assert_eq!(stack.len(), 2);
        assert_eq!(stack[0], Value::integer(1));
        assert_eq!(stack[1], Value::integer(2));
    }

    #[test]
    fn eval_local_with_if() {
        // Combine locals with control flow
        let result = eval("5 -> x << IF x 3 > THEN x ELSE 0 END >>").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);

        let result = eval("2 -> x << IF x 3 > THEN x ELSE 0 END >>").unwrap();
        assert_eq!(result, vec![Value::integer(0)]);
    }

    // ======== Loop tests ========

    #[test]
    fn eval_do_until_simple() {
        // DO/UNTIL always executes body at least once
        let result = eval("DO 1 UNTIL 1 END").unwrap();
        assert_eq!(result, vec![Value::integer(1)]);
    }

    #[test]
    fn eval_do_until_with_stack_counter() {
        // Use stack for counting: start with 0, increment until 3
        // Each iteration: 1 +; condition checks if >= 3
        let result = eval("0 DO 1 + UNTIL DUP 3 >= END").unwrap();
        // Counter goes 1, 2, 3; stops when 3 >= 3
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_while_repeat_false_condition() {
        // WHILE with false condition - body never executes
        let result = eval("WHILE 0 REPEAT 42 END").unwrap();
        assert_eq!(result, vec![]); // Empty stack
    }

    #[test]
    fn eval_while_repeat_with_stack_counter() {
        // Use stack for counter: start with 0, while < 3, increment
        let result = eval("0 WHILE DUP 3 < REPEAT 1 + END").unwrap();
        // Counter goes 0->1->2->3, stops when 3 < 3 is false
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_start_next_simple() {
        // Sum 1+2+3+4+5 using START/NEXT
        // Start with 0 on stack, add counter each iteration
        let result = eval("0 1 5 START 1 + NEXT").unwrap();
        // 0 + 1 + 1 + 1 + 1 + 1 = 5 iterations adding 1 each time
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_for_next_with_variable() {
        // Sum using FOR with named variable
        let result = eval("0 1 3 FOR i i + NEXT").unwrap();
        // 0 + 1 + 2 + 3 = 6
        assert_eq!(result, vec![Value::integer(6)]);
    }

    #[test]
    fn eval_for_next_push_values() {
        // Push loop variable values
        let result = eval("1 3 FOR i i NEXT").unwrap();
        // Pushes 1, 2, 3
        assert_eq!(result, vec![Value::integer(1), Value::integer(2), Value::integer(3)]);
    }

    #[test]
    fn eval_case_first_match() {
        // CASE with first condition true
        let result = eval("CASE 1 THEN 10 END 0 THEN 20 END END").unwrap();
        assert_eq!(result, vec![Value::integer(10)]);
    }

    #[test]
    fn eval_case_second_match() {
        // CASE with second condition true
        let result = eval("CASE 0 THEN 10 END 1 THEN 20 END END").unwrap();
        assert_eq!(result, vec![Value::integer(20)]);
    }

    #[test]
    fn eval_case_no_match() {
        // CASE with no matching condition (no default)
        let result = eval("CASE 0 THEN 10 END 0 THEN 20 END END").unwrap();
        assert_eq!(result, vec![]); // Nothing pushed
    }

    #[test]
    fn eval_case_with_default() {
        // CASE with default
        let result = eval("CASE 0 THEN 10 END 99 END").unwrap();
        assert_eq!(result, vec![Value::integer(99)]);
    }

    #[test]
    fn eval_case_computed_condition() {
        // CASE with computed conditions
        let result = eval("5 -> x << CASE x 3 < THEN 1 END x 7 < THEN 2 END 3 END >>").unwrap();
        // x=5, 5<3 is false, 5<7 is true, so result is 2
        assert_eq!(result, vec![Value::integer(2)]);
    }

    // === Symbolic expression tests ===

    #[test]
    fn eval_symbolic_variable() {
        // Simple symbolic variable
        let result = eval("'X'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "X");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_expression() {
        // Symbolic expression with binary operation
        let result = eval("'X+1'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "X+1");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_complex() {
        // More complex symbolic expression
        let result = eval("'X^2+2*X+1'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "X^2+2*X+1");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_function() {
        // Symbolic function call
        let result = eval("'SIN(X)'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "SIN(X)");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_with_spaces() {
        // Symbolic expression with spaces
        let result = eval("' X + Y '").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "X+Y");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_unary_minus() {
        // Symbolic with unary minus
        let result = eval("'-X'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "-X");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_parens() {
        // Symbolic with parentheses
        let result = eval("'(X+Y)*Z'").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "(X+Y)*Z");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_multiple_symbolic() {
        // Multiple symbolic values
        let result = eval("'X' 'Y'").unwrap();
        assert_eq!(result.len(), 2);
        match (&result[0], &result[1]) {
            (Value::Symbolic(x), Value::Symbolic(y)) => {
                assert_eq!(format!("{}", x), "X");
                assert_eq!(format!("{}", y), "Y");
            }
            _ => panic!("expected two symbolic values"),
        }
    }

    #[test]
    fn eval_symbolic_in_list() {
        // Symbolic inside a list
        let result = eval("{ 'X' 'Y+1' }").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                match (&items[0], &items[1]) {
                    (Value::Symbolic(x), Value::Symbolic(y)) => {
                        assert_eq!(format!("{}", x), "X");
                        assert_eq!(format!("{}", y), "Y+1");
                    }
                    _ => panic!("expected symbolic values in list"),
                }
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_symbolic_tonum() {
        // →NUM converts symbolic to number when no variables
        let result = eval("'2+3' →NUM").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_symbolic_tonum_real() {
        // →NUM with real result
        let result = eval("'3.14' →NUM").unwrap();
        assert_eq!(result, vec![Value::real(3.14)]);
    }

    #[test]
    fn eval_symbolic_tonum_complex_expr() {
        // →NUM with complex expression
        let result = eval("'2^3+1' →NUM").unwrap();
        assert_eq!(result, vec![Value::integer(9)]); // 8 + 1
    }

    #[test]
    fn eval_symbolic_tonum_ascii_arrow() {
        // ->NUM (ASCII arrow) should also work
        let result = eval("'10/2' ->NUM").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_symbolic_tonum_alias() {
        // TONUM alias should work
        let result = eval("'7*8' TONUM").unwrap();
        assert_eq!(result, vec![Value::integer(56)]);
    }

    #[test]
    fn eval_symbolic_tonum_passthrough() {
        // →NUM on a number should pass through
        let result = eval("42 →NUM").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_symbolic_symeval() {
        // SYMEVAL evaluates if possible
        let result = eval("'5-2' SYMEVAL").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_symbolic_symeval_with_var() {
        // SYMEVAL with variable returns as-is
        let result = eval("'X+1' SYMEVAL").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Symbolic(expr) => {
                assert_eq!(format!("{}", expr), "X+1");
            }
            _ => panic!("expected symbolic value"),
        }
    }

    #[test]
    fn eval_symbolic_tonum_fails_with_var() {
        // →NUM should fail with variables
        let result = eval("'X+1' →NUM");
        assert!(result.is_err());
    }

    // === List operation tests ===

    #[test]
    fn eval_list_size() {
        let result = eval("{ 1 2 3 } SIZE").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_list_size_empty() {
        let result = eval("{ } SIZE").unwrap();
        assert_eq!(result, vec![Value::integer(0)]);
    }

    #[test]
    fn eval_list_get() {
        // 1-based indexing
        let result = eval("{ 10 20 30 } 2 GET").unwrap();
        assert_eq!(result, vec![Value::integer(20)]);
    }

    #[test]
    fn eval_list_get_first() {
        let result = eval("{ 10 20 30 } 1 GET").unwrap();
        assert_eq!(result, vec![Value::integer(10)]);
    }

    #[test]
    fn eval_list_get_last() {
        let result = eval("{ 10 20 30 } 3 GET").unwrap();
        assert_eq!(result, vec![Value::integer(30)]);
    }

    #[test]
    fn eval_list_put() {
        // PUT: list position newobj → newlist (HP spec order)
        let result = eval("{ 1 2 3 } 2 99 PUT").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(1));
                assert_eq!(items[1], Value::integer(99));
                assert_eq!(items[2], Value::integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_head() {
        let result = eval("{ 10 20 30 } HEAD").unwrap();
        assert_eq!(result, vec![Value::integer(10)]);
    }

    #[test]
    fn eval_list_tail() {
        let result = eval("{ 10 20 30 } TAIL").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(items[0], Value::integer(20));
                assert_eq!(items[1], Value::integer(30));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_revlist() {
        let result = eval("{ 1 2 3 } REVLIST").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(3));
                assert_eq!(items[1], Value::integer(2));
                assert_eq!(items[2], Value::integer(1));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_to_list() {
        // →LIST: create list from N items
        let result = eval("10 20 30 3 →LIST").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(10));
                assert_eq!(items[1], Value::integer(20));
                assert_eq!(items[2], Value::integer(30));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_to_list_ascii() {
        // ->LIST ASCII arrow
        let result = eval("1 2 2 ->LIST").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_explode() {
        // LIST→: explode list onto stack
        let result = eval("{ 10 20 30 } LIST→").unwrap();
        assert_eq!(result.len(), 4); // 3 items + count
        assert_eq!(result[0], Value::integer(10));
        assert_eq!(result[1], Value::integer(20));
        assert_eq!(result[2], Value::integer(30));
        assert_eq!(result[3], Value::integer(3));
    }

    #[test]
    fn eval_list_explode_ascii() {
        // LIST-> ASCII arrow
        let result = eval("{ 5 6 } LIST->").unwrap();
        assert_eq!(result.len(), 3);
        assert_eq!(result[2], Value::integer(2)); // count
    }

    #[test]
    fn eval_list_obj_arrow() {
        // OBJ→ alias for LIST→
        let result = eval("{ 1 2 } OBJ→").unwrap();
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn eval_list_roundtrip() {
        // Explode then recreate
        let result = eval("{ 1 2 3 } LIST→ →LIST").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], Value::integer(1));
                assert_eq!(items[1], Value::integer(2));
                assert_eq!(items[2], Value::integer(3));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_nested() {
        let result = eval("{ { 1 2 } { 3 4 } } SIZE").unwrap();
        assert_eq!(result, vec![Value::integer(2)]);
    }

    #[test]
    fn eval_list_nested_get() {
        let result = eval("{ { 1 2 } { 3 4 } } 1 GET").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(items[0], Value::integer(1));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_list_head_error_empty() {
        let result = eval("{ } HEAD");
        assert!(result.is_err());
    }

    #[test]
    fn eval_list_tail_error_empty() {
        let result = eval("{ } TAIL");
        assert!(result.is_err());
    }

    #[test]
    fn eval_list_get_error_out_of_range() {
        let result = eval("{ 1 2 3 } 5 GET");
        assert!(result.is_err());
    }

    #[test]
    fn eval_list_get_error_zero_index() {
        // 1-based indexing, 0 is invalid
        let result = eval("{ 1 2 3 } 0 GET");
        assert!(result.is_err());
    }

    // === Transcendentals tests ===

    #[test]
    fn eval_pi() {
        let result = eval("PI").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - std::f64::consts::PI).abs() < 1e-15);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_pi_unicode() {
        let result = eval("π").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - std::f64::consts::PI).abs() < 1e-15);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_sqrt() {
        let result = eval("16 SQRT").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 4.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_sqrt_real() {
        let result = eval("2.0 SQRT").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - std::f64::consts::SQRT_2).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_sqrt_negative_error() {
        let result = eval("-1 SQRT");
        assert!(result.is_err());
    }

    #[test]
    fn eval_sin_zero() {
        let result = eval("0 SIN").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!(r.abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_cos_zero() {
        let result = eval("0 COS").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 1.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_sin_pi_half() {
        // sin(π/2) = 1
        let result = eval("PI 2 / SIN").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 1.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_exp_one() {
        // e^1 = e
        let result = eval("1 EXP").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - std::f64::consts::E).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_ln_e() {
        // ln(e) = 1
        let result = eval("1 EXP LN").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 1.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_log_100() {
        // log10(100) = 2
        let result = eval("100 LOG").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 2.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_alog() {
        // 10^2 = 100
        let result = eval("2 ALOG").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 100.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_atan2() {
        // atan2(1, 1) = π/4
        let result = eval("1 1 ATAN2").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - std::f64::consts::FRAC_PI_4).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_sinh_zero() {
        let result = eval("0 SINH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!(r.abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_cosh_zero() {
        // cosh(0) = 1
        let result = eval("0 COSH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => {
                assert!((*r - 1.0).abs() < 1e-10);
            }
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_asin_domain_error() {
        // asin(2) is undefined
        let result = eval("2 ASIN");
        assert!(result.is_err());
    }

    #[test]
    fn eval_ln_zero_error() {
        // ln(0) is undefined
        let result = eval("0 LN");
        assert!(result.is_err());
    }

    #[test]
    fn eval_ln_negative_error() {
        let result = eval("-1 LN");
        assert!(result.is_err());
    }

    #[test]
    fn eval_abs_positive() {
        let result = eval("5 ABS").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_abs_negative() {
        let result = eval("-5 ABS").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_abs_real() {
        let result = eval("-3.5 ABS").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 3.5).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_ceil() {
        let result = eval("3.2 CEIL").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 4.0).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_floor() {
        let result = eval("3.8 FLOOR").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 3.0).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_ip() {
        // IP truncates toward zero
        let result = eval("3.7 IP").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 3.0).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_ip_negative() {
        // IP(-3.7) = -3 (truncate toward zero)
        let result = eval("-3.7 IP").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - -3.0).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_fp() {
        let result = eval("3.7 FP").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 0.7).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    // === String operation tests ===

    #[test]
    fn eval_str_from_integer() {
        let result = eval("42 STR").unwrap();
        assert_eq!(result, vec![Value::String("42".into())]);
    }

    #[test]
    fn eval_str_from_real() {
        let result = eval("3.14 STR").unwrap();
        assert_eq!(result, vec![Value::String("3.14".into())]);
    }

    #[test]
    fn eval_num_from_string() {
        let result = eval("\"42\" NUM").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_num_from_string_real() {
        let result = eval("\"3.14\" NUM").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 3.14).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_chr() {
        let result = eval("65 CHR").unwrap();
        assert_eq!(result, vec![Value::String("A".into())]);
    }

    #[test]
    fn eval_asc() {
        let result = eval("\"A\" ASC").unwrap();
        assert_eq!(result, vec![Value::integer(65)]);
    }

    #[test]
    fn eval_sub() {
        let result = eval("\"hello\" 2 3 SUB").unwrap();
        assert_eq!(result, vec![Value::String("ell".into())]);
    }

    #[test]
    fn eval_pos_found() {
        let result = eval("\"hello world\" \"world\" POS").unwrap();
        assert_eq!(result, vec![Value::integer(7)]);
    }

    #[test]
    fn eval_pos_not_found() {
        let result = eval("\"hello\" \"xyz\" POS").unwrap();
        assert_eq!(result, vec![Value::integer(0)]);
    }

    #[test]
    fn eval_srev() {
        let result = eval("\"hello\" SREV").unwrap();
        assert_eq!(result, vec![Value::String("olleh".into())]);
    }

    #[test]
    fn eval_trim() {
        let result = eval("\"  hello  \" TRIM").unwrap();
        assert_eq!(result, vec![Value::String("hello".into())]);
    }

    #[test]
    fn eval_rtrim() {
        let result = eval("\"  hello  \" RTRIM").unwrap();
        assert_eq!(result, vec![Value::String("  hello".into())]);
    }

    #[test]
    fn eval_strlencp() {
        let result = eval("\"hello\" STRLENCP").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    #[test]
    fn eval_ntokens() {
        let result = eval("\"one two three\" NTOKENS").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_nthtoken() {
        let result = eval("\"one two three\" 2 NTHTOKEN").unwrap();
        assert_eq!(result, vec![Value::String("two".into())]);
    }

    #[test]
    fn eval_srepl() {
        let result = eval("\"hello world\" \"world\" \"universe\" SREPL").unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::String("hello universe".into()));
        assert_eq!(result[1], Value::integer(1));
    }

    #[test]
    fn eval_to_utf8() {
        let result = eval("\"AB\" →UTF8").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                assert_eq!(items[0], Value::integer(65));
                assert_eq!(items[1], Value::integer(66));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_from_utf8() {
        let result = eval("{ 65 66 } UTF8→").unwrap();
        assert_eq!(result, vec![Value::String("AB".into())]);
    }

    #[test]
    fn eval_string_size() {
        // SIZE should work on strings too (from list library)
        let result = eval("\"hello\" SIZE").unwrap();
        assert_eq!(result, vec![Value::integer(5)]);
    }

    // === Comment tests ===

    #[test]
    fn eval_single_line_comment() {
        // @ comment should be skipped
        let result = eval("1 @ this is a comment\n2 +").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_permanent_comment() {
        // @@ comment should be skipped
        let result = eval("1 @@ permanent comment\n2 +").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_multiline_comment() {
        // @@@ multi-line @@@ should be skipped
        let result = eval("1 @@@ this is\na multi-line\ncomment @@@ 2 +").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_comment_at_end() {
        let result = eval("1 2 + @ result").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_stripcomments() {
        // STRIPCOMMENTS on a program (no-op since comments are stripped during parsing)
        let result = eval("« 1 2 + » STRIPCOMMENTS EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    // === Directory tests ===

    #[test]
    fn eval_sto_rcl() {
        // Store and recall a value
        let result = eval("42 \"x\" STO \"x\" RCL").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_sto_rcl_quoted_symbol() {
        // Store and recall using quoted symbol syntax 'x'
        let result = eval("42 'x' STO 'x' RCL").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_sto_mixed_syntax() {
        // Store with string, recall with quoted symbol (and vice versa)
        let result = eval("10 \"a\" STO 'a' RCL 20 'b' STO \"b\" RCL +").unwrap();
        assert_eq!(result, vec![Value::integer(30)]);
    }

    #[test]
    fn eval_sto_multiple_vars() {
        // Store multiple variables
        let result = eval("10 \"a\" STO 20 \"b\" STO \"a\" RCL \"b\" RCL +").unwrap();
        assert_eq!(result, vec![Value::integer(30)]);
    }

    #[test]
    fn eval_sto_overwrite() {
        // Overwriting a variable
        let result = eval("1 \"x\" STO 99 \"x\" STO \"x\" RCL").unwrap();
        assert_eq!(result, vec![Value::integer(99)]);
    }

    #[test]
    fn eval_purge() {
        // Purge a variable, then RCL should fail
        let result = eval("42 \"x\" STO \"x\" PURGE \"x\" RCL");
        assert!(result.is_err());
    }

    #[test]
    fn eval_vars() {
        // VARS returns list of variable names
        let result = eval("1 \"a\" STO 2 \"b\" STO VARS").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
                // Items should contain "a" and "b" as strings (order may vary)
                let strs: Vec<_> = items.iter().map(|v| {
                    match v {
                        Value::String(s) => s.to_string(),
                        _ => panic!("expected string"),
                    }
                }).collect();
                assert!(strs.contains(&"a".to_string()));
                assert!(strs.contains(&"b".to_string()));
            }
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_clvar() {
        // CLVAR clears all variables
        let result = eval("42 \"x\" STO CLVAR \"x\" RCL");
        assert!(result.is_err()); // Variable no longer exists
    }

    #[test]
    fn eval_incr() {
        // INCR increments a numeric variable
        let result = eval("10 \"n\" STO \"n\" INCR").unwrap();
        assert_eq!(result, vec![Value::integer(11)]);
    }

    #[test]
    fn eval_incr_updates_variable() {
        // INCR should update the stored value
        let result = eval("10 \"n\" STO \"n\" INCR DROP \"n\" RCL").unwrap();
        assert_eq!(result, vec![Value::integer(11)]);
    }

    #[test]
    fn eval_decr() {
        // DECR decrements a numeric variable
        let result = eval("10 \"n\" STO \"n\" DECR").unwrap();
        assert_eq!(result, vec![Value::integer(9)]);
    }

    #[test]
    fn eval_decr_real() {
        // DECR works on real numbers too
        let result = eval("5.5 \"r\" STO \"r\" DECR").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::Real(r) => assert!((*r - 4.5).abs() < 1e-10),
            _ => panic!("expected real"),
        }
    }

    #[test]
    fn eval_rcl_undefined_error() {
        // RCL on undefined variable should error
        let result = eval("\"nonexistent\" RCL");
        assert!(result.is_err());
    }

    #[test]
    fn eval_sto_list() {
        // Store and recall a list
        let result = eval("{ 1 2 3 } \"mylist\" STO \"mylist\" RCL SIZE").unwrap();
        assert_eq!(result, vec![Value::integer(3)]);
    }

    #[test]
    fn eval_sto_program() {
        // Store and recall a program, then execute it
        let result = eval("« DUP * » \"square\" STO 5 \"square\" RCL EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(25)]);
    }

    // === Identifier-based variable access tests ===

    #[test]
    fn eval_identifier_recall() {
        // Using identifier directly to recall variable (not RCL)
        let result = eval("42 \"x\" STO x").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_identifier_auto_execute() {
        // Using identifier to recall a program should auto-execute it
        let result = eval("« 1 2 + » \"add\" STO 5 add").unwrap();
        // Should push 5, then auto-execute the program (pushes 3)
        assert_eq!(result, vec![Value::integer(5), Value::integer(3)]);
    }

    #[test]
    fn eval_identifier_in_expression() {
        // Using identifiers in an expression
        let result = eval("10 \"a\" STO 3 \"b\" STO a b +").unwrap();
        assert_eq!(result, vec![Value::integer(13)]);
    }

    #[test]
    fn eval_identifier_undefined() {
        // Using an undefined identifier should error
        let result = eval("undefined_var");
        assert!(result.is_err());
    }

    #[test]
    fn eval_identifier_in_program() {
        // Identifier inside a program should do runtime lookup
        let result = eval("5 \"n\" STO « n DUP * » EVAL").unwrap();
        assert_eq!(result, vec![Value::integer(25)]);
    }

    #[test]
    fn eval_identifier_program_sees_updated_var() {
        // Program sees updated variable value (runtime lookup)
        let result = eval("1 \"x\" STO « x » \"getx\" STO 42 \"x\" STO getx").unwrap();
        // getx is a program that pushes x
        // We store 1 in x, then create getx, then update x to 42
        // getx should auto-execute and see x=42
        assert_eq!(result, vec![Value::integer(42)]);
    }

    // === RENAME tests ===

    #[test]
    fn eval_rename() {
        // Rename a variable
        let result = eval("42 \"old\" STO \"old\" \"new\" RENAME \"new\" RCL").unwrap();
        assert_eq!(result, vec![Value::integer(42)]);
    }

    #[test]
    fn eval_rename_old_gone() {
        // After rename, old name should not exist
        let result = eval("42 \"old\" STO \"old\" \"new\" RENAME \"old\" RCL");
        assert!(result.is_err());
    }

    #[test]
    fn eval_rename_undefined() {
        // Renaming undefined variable should error
        let result = eval("\"nonexistent\" \"new\" RENAME");
        assert!(result.is_err());
    }

    // === Directory navigation tests ===

    #[test]
    fn eval_path_at_root() {
        // PATH at root should return empty list
        let result = eval("PATH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_crdir() {
        // Create subdirectory, verify with PATH (should still be at root)
        let result = eval("\"sub\" CRDIR PATH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()), // Still at root
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_crdir_duplicate_error() {
        // Creating duplicate subdirectory should error
        let result = eval("\"sub\" CRDIR \"sub\" CRDIR");
        assert!(result.is_err());
    }

    #[test]
    fn eval_crdir_conflicts_with_var() {
        // Can't create subdir with same name as variable
        let result = eval("42 \"x\" STO \"x\" CRDIR");
        assert!(result.is_err());
    }

    #[test]
    fn eval_pgdir() {
        // Create and then delete empty subdirectory
        let result = eval("\"sub\" CRDIR \"sub\" PGDIR PATH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_pgdir_nonexistent() {
        // Deleting non-existent subdirectory should error
        let result = eval("\"nonexistent\" PGDIR");
        assert!(result.is_err());
    }

    #[test]
    fn eval_updir_at_root() {
        // UPDIR at root is a no-op
        let result = eval("UPDIR PATH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected list"),
        }
    }

    #[test]
    fn eval_home_at_root() {
        // HOME at root is a no-op
        let result = eval("HOME PATH").unwrap();
        assert_eq!(result.len(), 1);
        match &result[0] {
            Value::List(items) => assert!(items.is_empty()),
            _ => panic!("expected list"),
        }
    }
}
