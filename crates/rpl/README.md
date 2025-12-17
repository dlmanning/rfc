# rpl

Core library for the RPL (Reverse Polish Lisp) language implementation.

## Overview

This crate provides everything needed to parse, compile, and execute RPL programs:

- **Parsing**: Tokenization and IR (intermediate representation) generation
- **Compilation**: Two-pass lowering to stack-based bytecode
- **Execution**: Virtual machine with pluggable command libraries
- **Analysis**: Symbols, scopes, diagnostics, and incremental analysis for IDE support
- **Standard Library**: 19 built-in libraries covering arithmetic, control flow, strings, lists, and more

## Architecture

```
Source Code → Tokens → IR → Bytecode → VM → Result
                ↑           ↑            ↑
             Phase 1     Phase 2      Phase 3
             (parse)     (lower)     (execute)
```

## Usage

### Simple Evaluation

```rust
use rpl::{eval, value::Value};

let result = eval("1 2 +").unwrap();
assert_eq!(result, vec![Value::integer(3)]);
```

### Session API

For stateful evaluation with variables and multiple inputs:

```rust
use rpl::Session;

let mut session = Session::new();

// Store a value
session.eval("42 \"x\" STO").unwrap();

// Recall and use it
let result = session.eval("x 10 +").unwrap();
assert_eq!(result[0].as_integer(), Some(52));
```

### Programs and Control Flow

```rust
use rpl::eval;

// Factorial function
let result = eval(r#"
    << -> n <<
        IF n 1 <=
        THEN 1
        ELSE n 1 - fact n *
        END
    >> >> "fact" STO
    5 fact
"#).unwrap();
// Result: 120
```

## Modules

| Module | Description |
|--------|-------------|
| `core` | Fundamental types: `Span`, `Symbol`, `Interner`, `Word` |
| `source` | Source file management and diagnostic rendering |
| `token` | Token types and semantic information |
| `parse` | Tokenizer and IR parser |
| `ir` | Intermediate representation nodes |
| `lower` | IR to bytecode compiler |
| `vm` | Virtual machine and execution context |
| `value` | Runtime value types (Integer, Real, String, List, Program, etc.) |
| `libs` | Standard library implementations |
| `registry` | Command registration and dispatch |
| `analysis` | Static analysis, symbols, scopes, diagnostics |
| `session` | High-level API for applications |
| `serialize` | Value serialization for PACKDIR and libraries |
| `symbolic` | Symbolic expression representation and evaluation |
| `error` | Error codes and diagnostic builders |

## Standard Libraries

| Library | Commands |
|---------|----------|
| Arithmetic | `+`, `-`, `*`, `/`, `^`, `MOD`, `ABS`, `SIGN`, `MIN`, `MAX` |
| Binary | `#...b`, `#...o`, `#...h`, `BAND`, `BOR`, `BXOR`, `BNOT`, `BLSL`, `BLSR` |
| Stack | `DUP`, `DROP`, `SWAP`, `ROT`, `OVER`, `PICK`, `ROLL`, `DEPTH` |
| Flow Control | `IF`/`THEN`/`ELSE`/`END`, `FOR`/`NEXT`, `WHILE`/`REPEAT`, `CASE` |
| Locals | `->` / `→` local bindings |
| Lists | `HEAD`, `TAIL`, `GET`, `PUT`, `SIZE`, `REVLIST`, `→LIST`, `LIST→` |
| Strings | `STR`, `NUM`, `SUB`, `POS`, `SIZE`, `CHR`, `ASC`, `TRIM` |
| Directory | `STO`, `RCL`, `PURGE`, `VARS`, `CRDIR`, `PATH`, `HOME` |
| Transcendentals | `SIN`, `COS`, `TAN`, `EXP`, `LN`, `LOG`, `SQRT`, `PI` |
| Symbolic | `'expr'` syntax, `→NUM`, `SYMEVAL` |
| Programs | `<< >>` / `« »` syntax, `EVAL` |
| Comments | `@`, `@@`, `@@@...@@@` |

## Key Design Decisions

1. **Three grammar primitives**: Program `« »`, List `{ }`, Symbolic `' '`
2. **No complex number literals**: Complex numbers via commands, not syntax
3. **No closures**: Variables in nested programs use runtime lookup
4. **Library-agnostic VM**: Commands dispatch through pluggable `Registry`
