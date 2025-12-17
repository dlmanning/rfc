# RPL - Reverse Polish Lisp

A modern, open-source implementation of the RPL programming language in Rust. RPL is a stack-based language originally developed by HP for their scientific calculators (HP 48/49/50 series).

## Features

- **Complete Language Implementation** - Tokenization, analysis, bytecode compilation, and VM execution
- **Command-Line Tool** - Evaluate files, strings, or stdin with `rpl`
- **Interactive Calculator** - Full-featured terminal UI with `rpl-calc`
- **Language Server Protocol (LSP)** - Editor integration with completions, hover, go-to-definition, and semantic highlighting
- **Debug Adapter Protocol (DAP)** - Full debugging support with breakpoints, stepping, and variable inspection
- **19 Standard Libraries** - Arithmetic, transcendentals, strings, lists, flow control, and more
- **HP RPL Compatible** - Designed to be compatible with classic HP calculator RPL

## Quick Start

### Building

```bash
cargo build --release
```

This builds several binaries:

- `rpl` - Command-line evaluator
- `rpl-calc` - Interactive TUI calculator
- `rpl-lsp` - Language Server
- `rpl-dap` - Debug Adapter

### Command-Line Evaluator (`rpl`)

Evaluate RPL code from files, strings, or stdin:

```bash
# Evaluate a file
rpl program.rpl

# Evaluate a string
rpl -e "1 2 + 3 *"

# Read from stdin
echo "PI 2 / SIN" | rpl
```

### Interactive Calculator (`rpl-calc`)

Full-featured terminal UI with split-pane display:

```bash
rpl-calc
```

Features:

- Stack display with live updates
- Variable browser
- Command history with persistence
- Multi-line editor mode (Ctrl-O)
- Syntax highlighting

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
  rpl/           Core library: types, VM, analysis, compilation, standard libraries
  rpl-tui/       Interactive terminal calculator (ratatui-based)
  rpl-lsp/       Language Server Protocol implementation
  rpl-dap/       Debug Adapter Protocol implementation
  rpl-plot/      Scalable vector graphics library for plot objects

docs/            Implementation documentation
editors/vscode/  VS Code extension
tests/           Integration tests and example programs
```

## Editor Integration

### VS Code

A VS Code extension is available in `editors/vscode/` providing:

- Syntax highlighting via semantic tokens
- Code completions
- Hover documentation
- Go-to-definition
- Find references
- Integrated debugging

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
- `prime_sieve.rpl` - Primality testing
- `quicksort.rpl` - Quicksort with list partitioning
- `statistics.rpl` - Statistical functions (mean, variance, stdev)
- `symbolic_derivative.rpl` - Symbolic differentiation

## Requirements

- Rust 1.85+ (Edition 2024)

## Contributing

Contributions are welcome! See the documentation in `docs/` for implementation guides.
