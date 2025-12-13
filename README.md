# RPL - Reverse Polish Lisp

A modern, open-source implementation of the RPL programming language in Rust. RPL is a stack-based language originally developed by HP for their scientific calculators (HP 48/49/50 series).

## Features

- **Complete Language Implementation** - Tokenization, analysis, bytecode compilation, and VM execution
- **Interactive REPL** - Full-featured terminal UI with split-pane display, syntax highlighting, and command history
- **Language Server Protocol (LSP)** - Editor integration with completions, hover, go-to-definition, and semantic highlighting
- **Debug Adapter Protocol (DAP)** - Full debugging support with breakpoints, stepping, and variable inspection
- **17 Standard Libraries** - Arithmetic, transcendentals, strings, lists, flow control, and more
- **HP RPL Compatible** - Designed to be compatible with classic HP calculator RPL

## Quick Start

### Building

```bash
cargo build --release
```

This builds three binaries:
- `rpl` - Interactive REPL
- `rpl-lsp` - Language Server
- `rpl-dap` - Debug Adapter

### Running the REPL

```bash
cargo run --release --bin rpl
```

Or after building:

```bash
./target/release/rpl
```

## Language Examples

### Factorial

```rpl
@ Factorial using recursion
<< -> n <<
    IF n 0 ==
    THEN 1
    ELSE n 1 - fact n *
    END
>> >>
"fact" STO

5 fact  @ Result: 120
```

### Fibonacci

```rpl
@ Fibonacci sequence
<< -> n <<
    n 1 <=
    IF THEN n
    ELSE
        n 1 - fib
        n 2 - fib +
    END
>> >>
"fib" STO

10 fib  @ Result: 55
```

### Basic Stack Operations

```rpl
1 2 +       @ Add: 3
3 4 *       @ Multiply: 12
DUP         @ Duplicate top of stack
SWAP        @ Swap top two items
DROP        @ Remove top item
```

## Project Structure

```
crates/
  rpl-core/      Core types: symbols, spans, diagnostics, bytecode
  rpl-source/    Source file management and diagnostic rendering
  rpl-vm/        Virtual machine and value types
  rpl-lang/      Language pipeline: analysis, compilation, decompilation
  rpl-stdlib/    17 built-in standard libraries
  rpl-session/   High-level API for applications
  rpl-repl/      Interactive terminal REPL (ratatui-based)
  rpl-lsp/       Language Server Protocol implementation
  rpl-dap/       Debug Adapter Protocol implementation

docs/            Implementation documentation
editors/vscode/  VS Code extension
tests/           Integration tests and example programs
```

## Standard Libraries

| Library | Description |
|---------|-------------|
| StackLib | Stack manipulation (DUP, SWAP, DROP, OVER, etc.) |
| ArithmeticLib | Basic arithmetic operators (+, -, *, /) |
| TranscendentalsLib | Transcendental functions (SIN, COS, EXP, LN, etc.) |
| RealNumbersLib | Floating-point operations |
| ComplexLib | Complex number support |
| BinaryIntLib | Binary integer operations |
| StringsLib | String manipulation |
| ListsLib | List operations |
| ProgramsLib | Program objects and evaluation |
| FlowControlLib | Control flow (IF/THEN/ELSE, loops, error handling) |
| LocalsLib | Local variable support |
| DirectoryLib | Global variables and directory management |
| SymbolicLib | Symbolic expressions |
| LibPtrLib | Library pointers |
| PlotLib | Plotting functionality |
| CommentsLib | Comment parsing |
| IdentifiersLib | Variable name handling |

## Editor Integration

### VS Code

A VS Code extension is available in `editors/vscode/` providing:
- Syntax highlighting via semantic tokens
- Code completions
- Hover documentation
- Go-to-definition
- Find references
- Integrated debugging

## Documentation

- [RPL Commands Reference](RPL-COMMANDS.md) - Complete command documentation
- [Implementing a Library](docs/implementing-a-library.md) - Guide for adding new libraries
- [Differences from HP RPL](docs/differences-from-hp-rpl.md) - Compatibility notes

## Architecture

```
Source Code (.rpl)
       |
   Tokenizer
       |
   Analysis (symbols, scopes)
       |
   Compiler
       |
   Bytecode (32-bit words)
       |
   Virtual Machine
       |
   Library Dispatcher
       |
   Result Values
```

The VM is library-agnostic, delegating command execution to pluggable libraries via the `CommandDispatch` trait. This allows the standard library to be extended or replaced.

## Testing

```bash
cargo test
```

Example programs are available in `tests/programs/`:
- `factorial.rpl` - Recursive factorial
- `fibonacci.rpl` - Fibonacci sequence
- `gcd.rpl` - Greatest common divisor
- `newton_sqrt.rpl` - Newton's method square root
- `prime_sieve.rpl` - Sieve of Eratosthenes
- `symbolic_derivative.rpl` - Symbolic differentiation

## Requirements

- Rust 1.85+ (Edition 2024)

## Contributing

Contributions are welcome! See the documentation in `docs/` for implementation guides.
