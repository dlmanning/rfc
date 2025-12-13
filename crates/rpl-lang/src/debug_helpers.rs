//! Debug helper functions.
//!
//! These functions require access to CompiledProgram, so they live in rpl-lang
//! rather than rpl-vm.

use crate::compile::CompiledProgram;
use rpl_source::SourceFile;

/// Find the first bytecode position for a given source line (1-indexed).
///
/// Returns `None` if no bytecode corresponds to that line.
pub fn find_pc_for_line(program: &CompiledProgram, source: &SourceFile, line: u32) -> Option<usize> {
    // Convert line number to a byte offset range
    let line_start = source.line_start(line)? as usize;
    let line_end = source
        .line_start(line + 1)
        .map(|o| o as usize)
        .unwrap_or(source.source().len());

    // Find the first span that overlaps with this line
    for (pc, span) in program.spans.iter().enumerate() {
        let span_start = span.start().offset() as usize;
        if span_start >= line_start && span_start < line_end {
            return Some(pc);
        }
    }

    None
}

/// Get the source line number for a bytecode position (1-indexed).
pub fn line_for_pc(program: &CompiledProgram, source: &SourceFile, pc: usize) -> Option<u32> {
    let span = program.spans.get(pc)?;
    Some(source.line_col(span.start()).line)
}
