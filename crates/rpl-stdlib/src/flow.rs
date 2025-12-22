//! Flow control library.
//!
//! Provides control flow constructs:
//! - IF/THEN/END and IF/THEN/ELSE/END - conditionals
//! - FOR/NEXT - counted loop with named variable
//! - START/NEXT - counted loop with anonymous counter
//! - DO/UNTIL - post-test loop
//! - WHILE/REPEAT - pre-test loop
//! - CASE - multi-way branch

use std::sync::OnceLock;

use rpl::{
    core::Span,
    interface::InterfaceSpec,
    ir::{AtomKind, Branch, LibId, Node, NodeKind},
    libs::{ExecuteAction, ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
};
use rpl_vm::Opcode;

/// Interface declaration with explicit IDs.
const INTERFACE: &str = include_str!("interfaces/flow.rpli");

pub fn interface() -> &'static InterfaceSpec {
    static SPEC: OnceLock<InterfaceSpec> = OnceLock::new();
    SPEC.get_or_init(|| InterfaceSpec::from_dsl(INTERFACE).expect("invalid flow interface"))
}

/// Flow control library ID.
pub const FLOW_LIB: LibId = 9;

/// Command IDs for error handling.
pub mod cmd {
    /// DOERR - throw an error (pops error number from stack).
    pub const DOERR: u16 = 0;
    /// ERRN - push last error number to stack.
    pub const ERRN: u16 = 1;
    /// ERRM - push last error message to stack.
    pub const ERRM: u16 = 2;
}

/// Construct IDs for syntax.
pub mod constructs {
    /// IF/THEN/END or IF/THEN/ELSE/END
    pub const IF: u16 = 10;
    /// IFERR/THENERR/ENDERR with optional ELSEERR
    pub const IFERR: u16 = 11;
    /// FOR/NEXT counted loop with named variable
    pub const FOR: u16 = 20;
    /// FORUP/NEXT counted loop (ascending)
    pub const FORUP: u16 = 21;
    /// FORDN/NEXT counted loop (descending)
    pub const FORDN: u16 = 22;
    /// START/NEXT counted loop (anonymous)
    pub const START: u16 = 23;
    /// DO/UNTIL post-test loop
    pub const DO: u16 = 30;
    /// WHILE/REPEAT pre-test loop
    pub const WHILE: u16 = 31;
    /// CASE multi-way branch
    pub const CASE: u16 = 40;
}

/// The flow control library (implementation only).
#[derive(Clone, Copy)]
pub struct FlowLib;

impl LibraryLowerer for FlowLib {
    fn id(&self) -> LibId {
        FLOW_LIB
    }

    fn lower_composite(
        &self,
        construct_id: u16,
        branches: &[Branch],
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        match construct_id {
            constructs::IF => {
                // 2 branches = IF/THEN/END, 3 branches = IF/THEN/ELSE/END
                if branches.len() < 2 {
                    return Err(LowerError {
                        span: None,
                        message: "IF requires condition and body".into(),
                    });
                }
                ctx.lower_all(&branches[0])?;
                let pre_branch = ctx.save_depth();
                ctx.emit_if(); // consumes condition
                ctx.lower_all(&branches[1])?;

                if branches.len() >= 3 {
                    // Has ELSE branch
                    let then_depth = ctx.save_depth();
                    ctx.restore_depth(pre_branch);
                    ctx.emit_else();
                    ctx.lower_all(&branches[2])?;
                    let else_depth = ctx.save_depth();
                    ctx.restore_depth(LowerContext::merge_depths(&[then_depth, else_depth]));
                    ctx.emit_end();
                } else {
                    // No ELSE
                    ctx.emit_end();
                    let then_depth = ctx.save_depth();
                    ctx.restore_depth(LowerContext::merge_depths(&[pre_branch, then_depth]));
                }
                Ok(())
            }

            constructs::FOR => {
                // branches[0] = binding, branches[1] = body, branches[2] = step (optional)
                let step = if branches.len() > 2 {
                    Some(&branches[2])
                } else {
                    None
                };
                self.lower_for_loop(branches, step, ctx)
            }
            constructs::FORUP => {
                let step = if branches.len() > 2 {
                    Some(&branches[2])
                } else {
                    None
                };
                self.lower_for_loop_directional(branches, step, true, ctx)
            }
            constructs::FORDN => {
                let step = if branches.len() > 2 {
                    Some(&branches[2])
                } else {
                    None
                };
                self.lower_for_loop_directional(branches, step, false, ctx)
            }
            constructs::START => {
                // branches[0] = body, branches[1] = step (optional)
                let step = if branches.len() > 1 {
                    Some(&branches[1])
                } else {
                    None
                };
                self.lower_start_loop(branches, step, ctx)
            }

            constructs::DO => {
                if branches.len() < 2 {
                    return Err(LowerError {
                        span: None,
                        message: "DO/UNTIL requires body and condition".into(),
                    });
                }
                let pre_loop = ctx.save_depth();
                ctx.emit_loop();
                ctx.lower_all(&branches[0])?;
                ctx.lower_all(&branches[1])?;
                ctx.emit_i64_eqz();
                ctx.emit_br_if(0); // consumes condition
                ctx.emit_end();
                let post_loop = ctx.save_depth();
                ctx.restore_depth(LowerContext::merge_depths(&[pre_loop, post_loop]));
                Ok(())
            }

            constructs::WHILE => {
                if branches.len() < 2 {
                    return Err(LowerError {
                        span: None,
                        message: "WHILE/REPEAT requires condition and body".into(),
                    });
                }
                let pre_loop = ctx.save_depth();
                ctx.emit_block();
                ctx.emit_loop();
                ctx.lower_all(&branches[0])?;
                ctx.emit_i64_eqz();
                ctx.emit_br_if(1); // consumes condition
                ctx.lower_all(&branches[1])?;
                ctx.emit_br(0);
                ctx.emit_end();
                ctx.emit_end();
                let post_loop = ctx.save_depth();
                ctx.restore_depth(LowerContext::merge_depths(&[pre_loop, post_loop]));
                Ok(())
            }

            constructs::CASE => self.lower_case(branches, ctx),

            constructs::IFERR => {
                // 2 branches = IFERR/THEN/END, 3 branches = IFERR/THEN/ELSE/END
                if branches.len() < 2 {
                    return Err(LowerError {
                        span: None,
                        message: "IFERR requires body and error handler".into(),
                    });
                }
                let pre_try = ctx.save_depth();
                ctx.emit_block();
                ctx.emit_block();
                ctx.emit_try_table_catch_all(1);
                ctx.lower_all(&branches[0])?;
                ctx.emit_end();

                if branches.len() >= 3 {
                    // Has ELSE (no-error) branch
                    ctx.lower_all(&branches[2])?;
                }
                let success_depth = ctx.save_depth();
                ctx.emit_br(1);
                ctx.emit_end();
                ctx.restore_depth(pre_try);
                ctx.mark_unknown_depth();
                ctx.lower_all(&branches[1])?;
                let error_depth = ctx.save_depth();
                ctx.restore_depth(LowerContext::merge_depths(&[success_depth, error_depth]));
                ctx.emit_end();
                Ok(())
            }

            _ => Err(LowerError {
                span: None,
                message: format!("unknown flow control construct: {}", construct_id),
            }),
        }
    }

    fn lower_command(
        &self,
        cmd_id: u16,
        _span: Span,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        match cmd_id {
            cmd::DOERR => {
                ctx.emit_throw(0);
            }
            cmd::ERRN | cmd::ERRM => {
                ctx.output.emit_call_lib(FLOW_LIB, cmd_id);
            }
            _ => {
                return Err(LowerError {
                    span: None,
                    message: format!("unknown flow command: {}", cmd_id),
                });
            }
        }
        Ok(())
    }
}

impl LibraryExecutor for FlowLib {
    fn id(&self) -> LibId {
        FLOW_LIB
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd {
            cmd::ERRN => {
                let error_code = ctx.last_error().map(|e| e.code).unwrap_or(0);
                ctx.push(Value::Integer(error_code))?;
                Ok(ExecuteAction::ok())
            }
            cmd::ERRM => {
                let error_msg = ctx
                    .last_error()
                    .map(|e| e.message.clone())
                    .unwrap_or_default();
                ctx.push(Value::string(error_msg))?;
                Ok(ExecuteAction::ok())
            }
            cmd::DOERR => {
                Err("DOERR should be handled by bytecode".into())
            }
            _ => Err(format!("unknown flow command: {}", ctx.cmd)),
        }
    }
}

impl FlowLib {
    /// Lower a FOR loop.
    fn lower_for_loop(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.len() < 2 {
            return Err(LowerError {
                span: None,
                message: "FOR loop requires index and body".into(),
            });
        }

        // Get the local index from branches[0]
        // NOTE: User locals are offset by SCRATCH_LOCAL_COUNT to avoid conflict with scratch
        let var_index = match branches[0].first() {
            Some(Node {
                kind: NodeKind::Atom(AtomKind::Integer(idx)),
                ..
            }) => ctx.user_local(*idx as u32),
            _ => {
                return Err(LowerError {
                    span: None,
                    message: "FOR loop missing variable index".into(),
                });
            }
        };

        // Allocate locals for end and step
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end (both integers)
        // Store end
        ctx.emit_local_set(end_local);
        // Store start as loop variable
        ctx.emit_local_set(var_index);
        // Store step (default 1)
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop body (loop may execute 0+ times)
        let pre_loop = ctx.save_depth();

        ctx.emit_block();
        ctx.emit_loop();

        // Execute body
        ctx.lower_all(&branches[1])?;

        // Increment: var = var + step
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(var_index);

        // Check continuation condition
        // For positive step: continue while var <= end
        // For negative step: continue while var >= end
        // We compute both and select based on step sign:
        //   (step > 0) ? (var <= end) : (var >= end)

        // Compute var <= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute var >= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (var <= end) else (var >= end)
        ctx.emit_select(); // consumes 3, produces 1

        ctx.emit_br_if(0); // continue loop, consumes condition

        ctx.emit_end(); // end loop
        ctx.emit_end(); // end block

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.save_depth();
        ctx.restore_depth(LowerContext::merge_depths(&[pre_loop, post_loop]));

        Ok(())
    }

    /// Lower a directional FOR loop (FORUP or FORDN).
    ///
    /// ascending=true: FORUP - skip if start > end, use step +1 by default
    /// ascending=false: FORDN - skip if start < end, use step -1 by default
    fn lower_for_loop_directional(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ascending: bool,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.len() < 2 {
            return Err(LowerError {
                span: None,
                message: "FOR loop requires index and body".into(),
            });
        }

        // Get the local index from branches[0]
        // NOTE: User locals are offset by SCRATCH_LOCAL_COUNT to avoid conflict with scratch
        let var_index = match branches[0].first() {
            Some(Node {
                kind: NodeKind::Atom(AtomKind::Integer(idx)),
                ..
            }) => ctx.user_local(*idx as u32),
            _ => {
                return Err(LowerError {
                    span: None,
                    message: "FOR loop missing variable index".into(),
                });
            }
        };

        // Allocate locals for start, end, step
        let start_local = ctx.alloc_local();
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end
        // Store end first (it's on top)
        ctx.emit_local_set(end_local);
        // Store start to both start_local and var_index
        ctx.output.emit_local_tee(start_local);
        ctx.emit_local_set(var_index);

        // Store step
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            // NEXT always uses step +1, regardless of loop direction
            // The direction check (skip if wrong bounds) is separate
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop (loop may execute 0+ times)
        let pre_loop = ctx.save_depth();

        // Check if we should skip the loop entirely
        // FORUP: skip if start > end
        // FORDN: skip if start < end
        // Wrap the loop in a block so we can br_if to skip it
        ctx.emit_block(); // outer block for skip

        // Compute skip condition
        ctx.emit_local_get_int(start_local);
        ctx.emit_local_get_int(end_local);
        if ascending {
            // FORUP: skip if start > end
            ctx.emit_i64_binop(Opcode::I64GtS);
        } else {
            // FORDN: skip if start < end
            ctx.emit_i64_binop(Opcode::I64LtS);
        }
        ctx.emit_br_if(0); // skip to end of outer block if true

        // The loop itself
        ctx.emit_block();
        ctx.emit_loop();

        // Execute body
        ctx.lower_all(&branches[1])?;

        // Increment: var = var + step
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(var_index);

        // Check continuation condition (same as regular FOR with step direction handling)
        // Compute var <= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute var >= end
        ctx.emit_local_get_int(var_index);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (var <= end) else (var >= end)
        ctx.emit_select(); // consumes 3, produces 1

        ctx.emit_br_if(0); // continue loop, consumes condition

        ctx.emit_end(); // end loop
        ctx.emit_end(); // end inner block
        ctx.emit_end(); // end outer block (skip target)

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.save_depth();
        ctx.restore_depth(LowerContext::merge_depths(&[pre_loop, post_loop]));

        Ok(())
    }

    /// Lower a START loop.
    fn lower_start_loop(
        &self,
        branches: &[Branch],
        step: Option<&Branch>,
        ctx: &mut LowerContext,
    ) -> Result<(), LowerError> {
        if branches.is_empty() {
            return Err(LowerError {
                span: None,
                message: "START loop requires body".into(),
            });
        }

        // Allocate locals for counter, end, step
        let counter_local = ctx.alloc_local();
        let end_local = ctx.alloc_local();
        let step_local = ctx.alloc_local();

        // Stack has: start end
        ctx.emit_local_set(end_local);
        ctx.emit_local_set(counter_local);

        // Store step
        if let Some(step_nodes) = step {
            ctx.lower_all(step_nodes)?;
        } else {
            ctx.emit_i64_const(1);
        }
        ctx.emit_local_set(step_local);

        // Save state before loop (loop may execute 0+ times)
        let pre_loop = ctx.save_depth();

        // Loop structure same as FOR
        ctx.emit_block();
        ctx.emit_loop();

        // Execute body
        ctx.lower_all(&branches[0])?;

        // Increment counter
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_binop(Opcode::I64Add);
        ctx.emit_local_set(counter_local);

        // Check continuation condition
        // For positive step: continue while counter <= end
        // For negative step: continue while counter >= end

        // Compute counter <= end
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64LeS);

        // Compute counter >= end
        ctx.emit_local_get_int(counter_local);
        ctx.emit_local_get_int(end_local);
        ctx.emit_i64_binop(Opcode::I64GeS);

        // Compute step > 0
        ctx.emit_local_get_int(step_local);
        ctx.emit_i64_const(0);
        ctx.emit_i64_binop(Opcode::I64GtS);

        // Select: if step > 0 then (counter <= end) else (counter >= end)
        ctx.emit_select(); // consumes 3, produces 1

        ctx.emit_br_if(0); // consumes condition

        ctx.emit_end();
        ctx.emit_end();

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.save_depth();
        ctx.restore_depth(LowerContext::merge_depths(&[pre_loop, post_loop]));

        Ok(())
    }

    /// Lower a CASE construct.
    fn lower_case(&self, branches: &[Branch], ctx: &mut LowerContext) -> Result<(), LowerError> {
        // branches are pairs: [cond1, body1, cond2, body2, ...]
        // Empty condition = default case
        // Odd number of branches = last one is default body (empty condition implied)

        if branches.is_empty() {
            return Ok(()); // Empty CASE does nothing
        }

        // Save pre-case depth for joining
        let pre_case = ctx.save_depth();

        // Convert to nested if/else with proper depth tracking
        let mut i = 0;
        let mut if_depth = 0;
        let mut branch_depths: Vec<(usize, bool)> = Vec::new();
        let mut has_default = false;

        while i < branches.len() {
            let condition = &branches[i];
            let body = if i + 1 < branches.len() {
                &branches[i + 1]
            } else {
                // Odd branch count - last branch is default body with empty condition
                has_default = true;
                ctx.lower_all(condition)?; // "condition" is actually the default body
                branch_depths.push(ctx.save_depth());
                break;
            };

            if condition.is_empty() {
                // Default case - just execute the body
                has_default = true;
                ctx.lower_all(body)?;
                branch_depths.push(ctx.save_depth());
                i += 2;
            } else {
                // Save depth before this branch
                let pre_branch = ctx.save_depth();

                // Conditional case
                ctx.lower_all(condition)?;
                ctx.emit_if(); // consumes condition
                ctx.lower_all(body)?;

                // Save this branch's depth
                branch_depths.push(ctx.save_depth());

                if_depth += 1;
                i += 2;

                // If there are more branches, emit else and restore pre-branch depth
                if i < branches.len() {
                    ctx.emit_else();
                    ctx.restore_depth(pre_branch);
                }
            }
        }

        // Close all the if blocks
        for _ in 0..if_depth {
            ctx.emit_end();
        }

        // Compute minimum depth from all branches
        if !branch_depths.is_empty() {
            // Also consider pre-case depth if no default case (might not execute any branch)
            if !has_default {
                branch_depths.push(pre_case);
            }
            ctx.restore_depth(LowerContext::merge_depths(&branch_depths));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flow_lib_id() {
        assert_eq!(FLOW_LIB, 9);
    }

    #[test]
    fn flow_lib_implements_traits() {
        assert_eq!(interface().id(), FLOW_LIB);
        assert_eq!(interface().name(), "flow");
    }

    #[test]
    fn claims_include_all_keywords() {
        let claims = interface().token_claims();
        let tokens: Vec<_> = claims.iter().map(|c| c.token.as_str()).collect();
        assert!(tokens.contains(&"IF"));
        assert!(tokens.contains(&"FOR"));
        assert!(tokens.contains(&"START"));
        assert!(tokens.contains(&"DO"));
        assert!(tokens.contains(&"WHILE"));
        assert!(tokens.contains(&"CASE"));
        assert!(tokens.contains(&"IFERR"));
    }

    /// Regression test: EVAL inside START loop must preserve control stack.
    /// Previously, EVAL would clear the control stack, causing "invalid branch depth"
    /// errors when the loop tried to branch after the EVAL returned.
    #[test]
    fn eval_inside_start_loop_preserves_control_stack() {
        let result = crate::eval(
            r#"
            << 1 + >> "inc" STO
            0 "sum" STO
            1 3 START
                sum inc "sum" STO
            NEXT
            sum
            "#,
        );
        assert_eq!(result, Ok(vec![Value::Integer(3)]));
    }

    /// Regression test: EVAL with IF/THEN/ELSE inside loop.
    #[test]
    fn eval_with_conditional_inside_loop() {
        let result = crate::eval(
            r#"
            << 2 MOD 0 == >> "is_even" STO
            0 "count" STO
            1 5 FOR i
                i is_even
                IF THEN count 1 + "count" STO END
            NEXT
            count
            "#,
        );
        // Loop counter 1-5: 2 and 4 are even
        assert_eq!(result, Ok(vec![Value::Integer(2)]));
    }
}
