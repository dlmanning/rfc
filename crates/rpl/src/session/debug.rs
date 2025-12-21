//! Debug helpers for source-to-bytecode mapping.
//!
//! This module provides helper functions for mapping between source lines
//! and bytecode positions, used by debuggers like DAP.

use crate::source::SourceFile;

use crate::lower::CompiledProgram;

/// Find the bytecode PC for a source line.
///
/// Returns the PC of the first instruction that starts on or after
/// the given line, or None if no instruction is on that line.
pub fn find_pc_for_line(program: &CompiledProgram, source: &SourceFile, line: u32) -> Option<usize> {
    // Get the byte offset range for this line
    let line_start = source.line_start(line)? as usize;
    let line_end = source
        .line_start(line + 1)
        .map(|o| o as usize)
        .unwrap_or(source.source().len());

    // Find first span that starts within this line
    for (i, &offset) in program.span_offsets.iter().enumerate() {
        let span = &program.spans[i];
        let span_start = span.start().offset() as usize;

        if span_start >= line_start && span_start < line_end {
            return Some(offset);
        }
    }

    // No span starts on this line - caller should handle this
    // (e.g., by adding a source-based breakpoint for nested programs)
    None
}

/// Find the source line for a bytecode PC.
///
/// Returns the 0-based line number containing the instruction at the given PC,
/// or None if no source mapping exists for that PC.
pub fn line_for_pc(program: &CompiledProgram, source: &SourceFile, pc: usize) -> Option<u32> {
    // Find the span containing this PC using binary search
    let span = program.span_for_pc(pc)?;

    // Convert span start position to line number
    Some(source.line_col(span.start()).line)
}

/// Find the column for a bytecode PC.
///
/// Returns the 0-based (line, column) for the instruction at the given PC,
/// or None if no source mapping exists for that PC.
pub fn line_col_for_pc(
    program: &CompiledProgram,
    source: &SourceFile,
    pc: usize,
) -> Option<(u32, u32)> {
    let span = program.span_for_pc(pc)?;
    let lc = source.line_col(span.start());
    Some((lc.line, lc.col))
}

/// Get all valid breakpoint locations (PCs with source mappings).
///
/// Returns a list of (pc, line) pairs for all instruction positions
/// that have source mappings.
pub fn breakpoint_locations(
    program: &CompiledProgram,
    source: &SourceFile,
) -> Vec<(usize, u32)> {
    program
        .span_offsets
        .iter()
        .zip(program.spans.iter())
        .map(|(&pc, span)| {
            let line = source.line_col(span.start()).line;
            (pc, line)
        })
        .collect()
}

/// Verify a breakpoint location and return the actual PC to use.
///
/// If the requested line has valid code, returns Some(pc) for that line.
/// If the line has no code, returns the PC for the next line with code.
/// Returns None if no valid breakpoint location exists at or after the line.
pub fn verify_breakpoint(
    program: &CompiledProgram,
    source: &SourceFile,
    requested_line: u32,
) -> Option<(usize, u32)> {
    // Try to find a breakpoint on or after the requested line
    for (i, span) in program.spans.iter().enumerate() {
        let line = source.line_col(span.start()).line;
        if line >= requested_line {
            return Some((program.span_offsets[i], line));
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lower::lower, parse::parse, registry::{InterfaceRegistry, LowererRegistry}};
    use crate::core::Interner;
    use crate::source::SourceId;
    use crate::analysis;

    fn compile_source(source: &str) -> (CompiledProgram, SourceFile) {
        let interfaces = InterfaceRegistry::new();
        let lowerers = LowererRegistry::new();
        let mut interner = Interner::new();

        let nodes = parse(source, &interfaces, &mut interner).unwrap();
        let analysis = analysis::analyze(&nodes, &interfaces, &interner);
        let program = lower(&nodes, &interfaces, &lowerers, &interner, &analysis).unwrap();

        let source_file = SourceFile::new(SourceId::new(0), "test.rpl".into(), source.into());

        (program, source_file)
    }

    #[test]
    fn find_pc_for_simple_line() {
        let source = "1 2 +";
        let (program, source_file) = compile_source(source);

        // Line 1 (1-based) should have code
        let pc = find_pc_for_line(&program, &source_file, 1);
        assert!(pc.is_some());
        assert_eq!(pc.unwrap(), 0);
    }

    #[test]
    fn line_for_pc_start() {
        let source = "1 2 +";
        let (program, source_file) = compile_source(source);

        // PC 0 should be line 1 (1-based)
        let line = line_for_pc(&program, &source_file, 0);
        assert_eq!(line, Some(1));
    }

    #[test]
    fn multiline_source() {
        let source = "1\n2\n+";
        let (program, source_file) = compile_source(source);

        // Check that we have multiple spans
        assert!(program.spans.len() >= 2);

        // Line 1 (1-based) should map to PC 0
        let pc1 = find_pc_for_line(&program, &source_file, 1);
        assert!(pc1.is_some());

        // Line 2 should map to a later PC
        let pc2 = find_pc_for_line(&program, &source_file, 2);
        assert!(pc2.is_some());
        assert!(pc2.unwrap() > pc1.unwrap());
    }

    #[test]
    fn breakpoint_locations_list() {
        let source = "1 2 +";
        let (program, source_file) = compile_source(source);

        let locations = breakpoint_locations(&program, &source_file);
        assert!(!locations.is_empty());

        // First location should be at PC 0, line 1
        assert_eq!(locations[0].0, 0);
        assert_eq!(locations[0].1, 1);
    }

    #[test]
    fn verify_breakpoint_exact_line() {
        let source = "1\n2\n+";
        let (program, source_file) = compile_source(source);

        // Requesting line 1 (1-based) should give us a valid breakpoint
        let result = verify_breakpoint(&program, &source_file, 1);
        assert!(result.is_some());
        let (_, line) = result.unwrap();
        assert_eq!(line, 1);
    }
}
