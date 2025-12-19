//! Library interface declaration parser.
//!
//! Parses Haskell-inspired interface declarations using the unified syntax:
//!
//! ```text
//! id: inputs -> PATTERN -> outputs
//! ```
//!
//! # Syntax
//!
//! ```text
//! library Stack 72
//!
//! -- Simple commands
//! 0: a -> DUP -> a a
//! 1: a -> DROP ->
//! 2: a b -> SWAP -> b a
//!
//! -- Operators with aliases
//! 3: a b -> (+), ADD -> Numeric a b
//!
//! -- Dynamic effects
//! 4: Int -> ROLL -> ...
//!
//! -- Conditional selection
//! 5: a b Int -> IFTE -> a | b
//!
//! -- Syntax constructs
//! 10: -> IF cond:Int THEN body:Prog END ->
//! 11: -> IF cond:Int THEN body:Prog ELSE alt:Prog END ->
//! 20: Int Int -> FOR $name:Sym body:Prog NEXT ->
//! ```
//!
//! ## Types
//!
//! - `Int`, `Real`, `Str`, `List`, `Prog`, `Sym` - concrete types
//! - `a`, `b`, `c` - type variables (polymorphic)
//! - `...` - dynamic (unknown at compile time)
//! - `Numeric a b` - computed type (int+int→int, else→real)
//! - `a | b` - union type
//!
//! ## Pattern Elements
//!
//! - `NAME` - command name (uppercase)
//! - `(+)` - operator name
//! - `(+), ADD` - aliases
//! - `KEYWORD` - syntax keyword (THEN, END, NEXT, etc.)
//! - `name:Type` - typed slot (parsed expression)
//! - `$name:Type` - binding slot (creates local variable)
//!
//! # Example
//!
//! ```
//! use rpl::interface::parse;
//!
//! let input = r#"
//! library Stack 72
//!
//! 0: a -> DUP -> a a
//! 1: a -> DROP ->
//! "#;
//!
//! let lib = parse(input).unwrap();
//! assert_eq!(lib.name, "Stack");
//! assert_eq!(lib.declarations.len(), 2);
//! ```
//!
//! # Interface Specification
//!
//! Use `InterfaceSpec::from_ast()` to convert a parsed library to an
//! interface specification for use in the LSP and analyzer:
//!
//! ```
//! use rpl::interface::{parse, InterfaceSpec};
//!
//! let input = "library Test 1\n0: a -> DUP -> a a";
//! let ast = parse(input).unwrap();
//! let spec = InterfaceSpec::from_ast(&ast);
//!
//! assert_eq!(spec.name(), "Test");
//! assert_eq!(spec.commands().len(), 1);
//! ```

mod ast;
mod parse;
mod spec;

pub use ast::*;
pub use parse::{parse, ParseError};
pub use spec::{CommandSpec, EffectKind, InterfaceSpec, Parser, SyntaxDecl};
