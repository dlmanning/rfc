//! Standard libraries for the RPL language.
//!
//! This crate provides the 17 built-in libraries that implement RPL's
//! core functionality:
//!
//! - Literals: comments, binary integers, real numbers, strings
//! - Data structures: lists, programs, symbolic expressions
//! - Operations: arithmetic, binary ops, transcendentals
//! - Control flow: if/then/else, loops, local variables
//! - Storage: directory operations, user libraries
//! - Stack manipulation

mod arithmetic;
mod binary_int;
mod binary_ops;
pub mod blob;
mod comments;
mod complex;
mod directory;
mod flow;
pub mod identifiers;
pub mod libptr;
mod lists;
pub mod locals;
mod numbers;
pub mod plot;
mod programs;
mod stack;
mod strings;
mod symbolic;
mod transcendentals;

pub use arithmetic::ArithmeticLib;
pub use binary_int::BinaryIntLib;
pub use binary_ops::BinaryOpsLib;
pub use blob::BlobLib;
pub use comments::CommentsLib;
pub use complex::ComplexLib;
pub use directory::DirectoryLib;
pub use flow::FlowControlLib;
pub use identifiers::IdentifiersLib;
pub use libptr::LibPtrLib;
pub use lists::ListsLib;
pub use locals::LocalsLib;
pub use numbers::RealNumbersLib;
pub use plot::PlotLib;
pub use programs::ProgramsLib;
pub use stack::StackLib;
pub use strings::StringsLib;
pub use symbolic::SymbolicLib;
pub use transcendentals::TranscendentalsLib;

use rpl_lang::library::{Library, LibraryRegistry};
use rpl_lang::operator::OperatorRegistry;

/// Register all standard libraries with a registry.
pub fn register_standard_libs(registry: &mut LibraryRegistry) {
    registry.register(CommentsLib, 115);
    registry.register(BinaryIntLib, 110);
    registry.register(BlobLib, 105);
    registry.register(RealNumbersLib, 100);
    registry.register(ComplexLib, 95); // High priority for (re,im) literal parsing
    registry.register(StringsLib, 80);
    registry.register(SymbolicLib, 75);
    registry.register(ListsLib, 70);
    registry.register(ProgramsLib, 60);
    registry.register(BinaryOpsLib, 55);
    registry.register(ArithmeticLib, 50);
    registry.register(TranscendentalsLib, 45);
    registry.register(FlowControlLib, 40);
    registry.register(DirectoryLib, 30);
    registry.register(LocalsLib, 25);
    registry.register(LibPtrLib, 15);
    registry.register(PlotLib, 12);
    registry.register(StackLib, 10);
    registry.register(IdentifiersLib, 1);
}

/// Register all operators from standard libraries.
pub fn register_standard_operators(registry: &mut OperatorRegistry) {
    RealNumbersLib.register_operators(registry);
    ComplexLib.register_operators(registry);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rpl_lang::library::Library;

    #[test]
    fn register_standard_libs_populates_registry() {
        let mut registry = LibraryRegistry::new();
        register_standard_libs(&mut registry);

        assert!(registry.get(CommentsLib.id()).is_some());
        assert!(registry.get(BinaryIntLib.id()).is_some());
        assert!(registry.get(BlobLib.id()).is_some());
        assert!(registry.get(BinaryOpsLib.id()).is_some());
        assert!(registry.get(RealNumbersLib.id()).is_some());
        assert!(registry.get(StringsLib.id()).is_some());
        assert!(registry.get(SymbolicLib.id()).is_some());
        assert!(registry.get(ListsLib.id()).is_some());
        assert!(registry.get(ProgramsLib.id()).is_some());
        assert!(registry.get(ArithmeticLib.id()).is_some());
        assert!(registry.get(TranscendentalsLib.id()).is_some());
        assert!(registry.get(FlowControlLib.id()).is_some());
        assert!(registry.get(DirectoryLib.id()).is_some());
        assert!(registry.get(LocalsLib.id()).is_some());
        assert!(registry.get(LibPtrLib.id()).is_some());
        assert!(registry.get(PlotLib.id()).is_some());
        assert!(registry.get(StackLib.id()).is_some());
        assert!(registry.get(IdentifiersLib.id()).is_some());
    }
}
