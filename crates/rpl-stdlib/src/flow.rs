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
    libs::{ExecuteContext, ExecuteResult, LibraryExecutor, LibraryLowerer},
    lower::{LowerContext, LowerError},
    value::Value,
    vm::bytecode::Opcode,
};

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
                ctx.pop_type();
                let pre_branch = ctx.types.clone();
                ctx.output.emit_if();
                ctx.lower_all(&branches[1])?;

                if branches.len() >= 3 {
                    // Has ELSE branch
                    let then_state = ctx.types.clone();
                    ctx.types.replace_with(pre_branch);
                    ctx.output.emit_else();
                    ctx.lower_all(&branches[2])?;
                    let else_state = ctx.types.clone();
                    let joined = then_state.join(&else_state);
                    ctx.types.replace_with(joined);
                    ctx.output.emit_end();
                } else {
                    // No ELSE
                    ctx.output.emit_end();
                    let then_state = ctx.types.clone();
                    let joined = pre_branch.join(&then_state);
                    ctx.types.replace_with(joined);
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
                let pre_loop = ctx.types.clone();
                ctx.output.emit_loop();
                ctx.lower_all(&branches[0])?;
                ctx.lower_all(&branches[1])?;
                ctx.output.emit_i64_eqz();
                ctx.pop_type();
                ctx.output.emit_br_if(0);
                ctx.output.emit_end();
                let post_loop = ctx.types.clone();
                let joined = pre_loop.join(&post_loop);
                ctx.types.replace_with(joined);
                Ok(())
            }

            constructs::WHILE => {
                if branches.len() < 2 {
                    return Err(LowerError {
                        span: None,
                        message: "WHILE/REPEAT requires condition and body".into(),
                    });
                }
                let pre_loop = ctx.types.clone();
                ctx.output.emit_block();
                ctx.output.emit_loop();
                ctx.lower_all(&branches[0])?;
                ctx.output.emit_i64_eqz();
                ctx.pop_type();
                ctx.output.emit_br_if(1);
                ctx.lower_all(&branches[1])?;
                ctx.output.emit_br(0);
                ctx.output.emit_end();
                ctx.output.emit_end();
                let post_loop = ctx.types.clone();
                let joined = pre_loop.join(&post_loop);
                ctx.types.replace_with(joined);
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
                let pre_try = ctx.types.clone();
                ctx.output.emit_block();
                ctx.output.emit_block();
                ctx.output.emit_try_table_catch_all(1);
                ctx.lower_all(&branches[0])?;
                ctx.output.emit_end();

                if branches.len() >= 3 {
                    // Has ELSE (no-error) branch
                    ctx.lower_all(&branches[2])?;
                }
                let success_state = ctx.types.clone();
                ctx.output.emit_br(1);
                ctx.output.emit_end();
                ctx.types.replace_with(pre_try);
                ctx.types.mark_unknown_depth();
                ctx.lower_all(&branches[1])?;
                let error_state = ctx.types.clone();
                let joined = success_state.join(&error_state);
                ctx.types.replace_with(joined);
                ctx.output.emit_end();
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
                ctx.output.emit_throw(0);
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
                Ok(())
            }
            cmd::ERRM => {
                let error_msg = ctx
                    .last_error()
                    .map(|e| e.message.clone())
                    .unwrap_or_default();
                ctx.push(Value::string(error_msg))?;
                Ok(())
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
        let pre_loop = ctx.types.clone();

        // block {
        //   loop {
        //     body
        //     // increment: var = var + step
        //     local.get var
        //     local.get step
        //     i64.add
        //     local.set var
        //     // check: if step > 0, continue while var <= end
        //     //        if step < 0, continue while var >= end
        //     local.get var
        //     local.get end
        //     i64.le_s
        //     local.get var
        //     local.get end
        //     i64.ge_s
        //     local.get step
        //     i64.const 0
        //     i64.gt_s
        //     select
        //     br_if 0  (continue if true)
        //   }
        // }
        ctx.output.emit_block();
        ctx.output.emit_loop();

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
        // Select pops condition, false_val, true_val and pushes result
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed
        // le_result stays as the selected value type (already on type stack)

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // continue loop

        ctx.output.emit_end(); // end loop
        ctx.output.emit_end(); // end block

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

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
        let pre_loop = ctx.types.clone();

        // Check if we should skip the loop entirely
        // FORUP: skip if start > end
        // FORDN: skip if start < end
        // Wrap the loop in a block so we can br_if to skip it
        ctx.output.emit_block(); // outer block for skip

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
        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // skip to end of outer block if true

        // The loop itself
        ctx.output.emit_block();
        ctx.output.emit_loop();

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
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0); // continue loop

        ctx.output.emit_end(); // end loop
        ctx.output.emit_end(); // end inner block
        ctx.output.emit_end(); // end outer block (skip target)

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

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
        let pre_loop = ctx.types.clone();

        // Loop structure same as FOR
        ctx.output.emit_block();
        ctx.output.emit_loop();

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
        ctx.output.emit_opcode(Opcode::Select);
        ctx.pop_type(); // condition consumed
        ctx.pop_type(); // ge_result consumed

        ctx.pop_type(); // br_if consumes condition
        ctx.output.emit_br_if(0);

        ctx.output.emit_end();
        ctx.output.emit_end();

        // Join with pre-loop state (loop may execute multiple times)
        let post_loop = ctx.types.clone();
        let joined = pre_loop.join(&post_loop);
        ctx.types.replace_with(joined);

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

        // Save pre-case state for joining
        let pre_case = ctx.types.clone();

        // Convert to nested if/else with proper type state tracking
        let mut i = 0;
        let mut depth = 0;
        let mut branch_states: Vec<rpl::types::CStack> = Vec::new();
        let mut has_default = false;

        while i < branches.len() {
            let condition = &branches[i];
            let body = if i + 1 < branches.len() {
                &branches[i + 1]
            } else {
                // Odd branch count - last branch is default body with empty condition
                has_default = true;
                ctx.lower_all(condition)?; // "condition" is actually the default body
                branch_states.push(ctx.types.clone());
                break;
            };

            if condition.is_empty() {
                // Default case - just execute the body
                has_default = true;
                ctx.lower_all(body)?;
                branch_states.push(ctx.types.clone());
                i += 2;
            } else {
                // Save state before this branch
                let pre_branch = ctx.types.clone();

                // Conditional case
                ctx.lower_all(condition)?;
                ctx.pop_type(); // condition consumed by if
                ctx.output.emit_if();
                ctx.lower_all(body)?;

                // Save this branch's state
                branch_states.push(ctx.types.clone());

                depth += 1;
                i += 2;

                // If there are more branches, emit else and restore pre-branch state
                if i < branches.len() {
                    ctx.output.emit_else();
                    ctx.types.replace_with(pre_branch);
                }
            }
        }

        // Close all the if blocks
        for _ in 0..depth {
            ctx.output.emit_end();
        }

        // Join all branch states
        if !branch_states.is_empty() {
            let mut joined = branch_states[0].clone();
            for state in &branch_states[1..] {
                joined = joined.join(state);
            }
            // Also join with pre-case state (if no default case, might not execute any branch)
            if !has_default {
                joined = joined.join(&pre_case);
            }
            ctx.types.replace_with(joined);
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
}
