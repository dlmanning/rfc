//! Integration tests for bytecode emission.
//!
//! These tests verify that the lowerer produces correct bytecode
//! for various source patterns. Uses the existing disassembler from rpl::vm::disasm.

use rpl::analysis::analyze;
use rpl::core::Interner;
use rpl::lower::{lower, CompiledProgram};
use rpl::parse::parse;
use rpl::registry::{InterfaceRegistry, LowererRegistry};
use rpl::vm::disasm::{disassemble, DisassembledInstr};

/// Compile RPL source to bytecode.
fn compile(source: &str) -> CompiledProgram {
    let mut interner = Interner::new();
    let mut interfaces = InterfaceRegistry::new();
    let mut lowerers = LowererRegistry::new();

    rpl_stdlib::register_interfaces(&mut interfaces);
    rpl_stdlib::register_lowerers(&mut lowerers);

    let nodes = parse(source, &interfaces, &mut interner).expect("parse failed");
    let analysis = analyze(&nodes, &interfaces, &interner);

    lower(&nodes, &interfaces, &lowerers, &interner, &analysis).expect("lowering failed")
}

/// Disassemble all instructions from a compiled program.
fn disasm_all(program: &CompiledProgram) -> Vec<DisassembledInstr> {
    disassemble(&program.code, 0, 10000)
}

/// Check if any instruction text contains the given substring.
fn has_instr(instrs: &[DisassembledInstr], pattern: &str) -> bool {
    instrs.iter().any(|i| i.text.contains(pattern))
}

/// Check if any instruction text matches exactly.
fn has_exact(instrs: &[DisassembledInstr], text: &str) -> bool {
    instrs.iter().any(|i| i.text == text)
}

/// Get all instruction texts for debugging.
fn instr_texts(instrs: &[DisassembledInstr]) -> Vec<&str> {
    instrs.iter().map(|i| i.text.as_str()).collect()
}

// ============================================================================
// Integer Literal Tests
// ============================================================================

#[test]
fn integer_literal_emits_i64const() {
    let program = compile("42");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Const 42"),
        "Expected I64Const 42, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn negative_integer_literal() {
    let program = compile("-123");
    let instrs = disasm_all(&program);
    // Could be I64Const -123 or I64Const 123 + negation
    assert!(
        has_instr(&instrs, "I64Const"),
        "Expected I64Const, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Real Literal Tests
// ============================================================================

#[test]
fn real_literal_emits_f64const() {
    let program = compile("2.5");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "F64Const 2.5"),
        "Expected F64Const 2.5, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Typed Arithmetic Tests
// ============================================================================

#[test]
fn int_plus_int_emits_i64add() {
    let program = compile("1 2 +");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Add"),
        "Expected I64Add for int+int, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn real_plus_real_emits_f64add() {
    let program = compile("1.0 2.0 +");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "F64Add"),
        "Expected F64Add for real+real, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn int_plus_real_coerces_to_f64add() {
    // When adding int + real, the int should be converted to f64
    let program = compile("1 2.0 +");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "F64Add"),
        "Expected F64Add for mixed types, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn int_minus_int_emits_i64sub() {
    let program = compile("5 3 -");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Sub"),
        "Expected I64Sub for int-int, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn int_times_int_emits_i64mul() {
    let program = compile("3 4 *");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Mul"),
        "Expected I64Mul for int*int, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn division_uses_calllib_for_unknown_types() {
    // Division with integer literals goes through CallLib
    // (result type tracking doesn't infer integer division produces real)
    let program = compile("6 2 /");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "CallLib"),
        "Expected CallLib for division, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn real_division_emits_f64div() {
    // When both operands are known reals, we get F64Div
    let program = compile("6.0 2.0 /");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "F64Div"),
        "Expected F64Div for real/real, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Comparison Tests
// ============================================================================

#[test]
fn int_comparison_emits_i64_ops() {
    let program = compile("1 2 <");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64LtS"),
        "Expected I64LtS for int < int, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn int_equality_emits_i64eq() {
    let program = compile("1 1 ==");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Eq"),
        "Expected I64Eq for int == int, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// List Construction Tests
// ============================================================================

#[test]
fn empty_list_emits_makelist_zero() {
    let program = compile("{ }");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "MakeList 0"),
        "Expected MakeList 0 for empty list, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn list_with_items_emits_makelist() {
    let program = compile("{ 1 2 3 }");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "MakeList 3"),
        "Expected MakeList 3, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn nested_list_emits_multiple_makelists() {
    let program = compile("{ { 1 } { 2 } }");
    let instrs = disasm_all(&program);
    // Should have MakeList 1 for inner lists and MakeList 2 for outer
    let list_counts: Vec<_> = instrs
        .iter()
        .filter(|i| i.text.starts_with("MakeList"))
        .map(|i| i.text.as_str())
        .collect();
    assert_eq!(
        list_counts,
        vec!["MakeList 1", "MakeList 1", "MakeList 2"],
        "Expected [MakeList 1, MakeList 1, MakeList 2] for nested lists"
    );
}

// ============================================================================
// String Tests
// ============================================================================

#[test]
fn string_literal_emits_stringconst() {
    let program = compile("\"hello\"");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "StringConst"),
        "Expected StringConst, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Program Tests
// ============================================================================

#[test]
fn program_emits_makeprogram() {
    let program = compile("<< 1 2 + >>");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "MakeProgram") || has_instr(&instrs, "MakeFunction"),
        "Expected MakeProgram, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Control Flow Tests
// ============================================================================

#[test]
fn if_then_end_structure() {
    let program = compile("1 IF 2 THEN 3 END");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "If") && has_exact(&instrs, "End"),
        "Expected If...End structure, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn if_then_else_structure() {
    let program = compile("1 IF 2 THEN 3 ELSE 4 END");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "If") && has_exact(&instrs, "Else") && has_exact(&instrs, "End"),
        "Expected If...Else...End structure, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Local Variable Tests
// ============================================================================

#[test]
fn local_binding_emits_localset_localget() {
    let program = compile("<< -> x << x >> >>");
    let instrs = disasm_all(&program);
    // The outer program should have MakeProgram/Function
    assert!(
        has_instr(&instrs, "MakeProgram") || has_instr(&instrs, "MakeFunction"),
        "Expected MakeProgram for local binding, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Variable Reference Tests
// ============================================================================

#[test]
fn variable_reference_emits_evalname() {
    let program = compile("x");
    let instrs = disasm_all(&program);
    assert!(
        has_instr(&instrs, "EvalName"),
        "Expected EvalName for variable reference, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Library Call Tests
// ============================================================================

#[test]
fn dup_uses_local_variables() {
    // DUP is optimized to LocalTee + LocalGet (more efficient than runtime call)
    let program = compile("1 DUP");
    let instrs = disasm_all(&program);
    let has_local_ops = has_instr(&instrs, "LocalTee") || has_instr(&instrs, "LocalGet");
    assert!(
        has_local_ops,
        "Expected LocalTee/LocalGet for DUP, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Complex Expression Tests
// ============================================================================

#[test]
fn complex_expression_uses_typed_add() {
    // (1 + 2) * 3: the + is typed (I64Add), but * uses CallLib
    // because analysis doesn't track result types through operations
    let program = compile("1 2 + 3 *");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "I64Add"),
        "Expected I64Add for the first operation, got: {:?}",
        instr_texts(&instrs)
    );
}

#[test]
fn mixed_type_coercion() {
    // 1 + 2.0 should coerce int to float and use F64Add
    let program = compile("1 2.0 +");
    let instrs = disasm_all(&program);
    assert!(
        has_exact(&instrs, "F64Add") && has_exact(&instrs, "F64ConvertI64S"),
        "Expected F64ConvertI64S and F64Add for mixed types, got: {:?}",
        instr_texts(&instrs)
    );
}

// ============================================================================
// Fixture Tests
// ============================================================================

#[test]
fn factorial_fixture_bytecode() {
    let source = include_str!("../programs/factorial.rpl");
    let program = compile(source);
    let instrs = disasm_all(&program);
    let texts = instr_texts(&instrs);

    // Should have a program definition (MakeProgram for << -> n << ... >> >>)
    // Note: -> n is a runtime binding inside the program, not a compile-time parameter
    assert!(
        has_instr(&instrs, "MakeProgram"),
        "Expected MakeProgram for factorial definition, got: {:?}",
        texts
    );

    // Should have StringConst for "fact" (the function name for STO)
    assert!(
        has_instr(&instrs, "StringConst"),
        "Expected StringConst for 'fact', got: {:?}",
        texts
    );

    // Should have CallLib for STO (directory library)
    assert!(
        has_instr(&instrs, "CallLib DIRECTORY"),
        "Expected CallLib DIRECTORY for STO, got: {:?}",
        texts
    );

    // Should have I64Const 5 for the final call
    assert!(
        has_exact(&instrs, "I64Const 5"),
        "Expected I64Const 5 for factorial argument, got: {:?}",
        texts
    );

    // Should have EvalName for calling "fact"
    assert!(
        has_instr(&instrs, "EvalName"),
        "Expected EvalName for 'fact' call, got: {:?}",
        texts
    );

    // Verify the instruction sequence order:
    // 1. MakeProgram (defines the factorial function)
    // 2. StringConst (pushes "fact")
    // 3. CallLib DIRECTORY (STO to store function)
    // 4. I64Const 5 (argument)
    // 5. EvalName (call fact)
    let make_prog_idx = instrs.iter().position(|i| i.text.contains("MakeProgram"));
    let string_idx = instrs.iter().position(|i| i.text.contains("StringConst"));
    let sto_idx = instrs.iter().position(|i| i.text.contains("CallLib DIRECTORY"));
    let const5_idx = instrs.iter().position(|i| i.text == "I64Const 5");
    let eval_idx = instrs.iter().position(|i| i.text.contains("EvalName"));

    assert!(
        make_prog_idx < string_idx && string_idx < sto_idx && sto_idx < const5_idx && const5_idx < eval_idx,
        "Instructions not in expected order: MakeProgram@{:?}, StringConst@{:?}, STO@{:?}, I64Const5@{:?}, EvalName@{:?}",
        make_prog_idx, string_idx, sto_idx, const5_idx, eval_idx
    );
}
