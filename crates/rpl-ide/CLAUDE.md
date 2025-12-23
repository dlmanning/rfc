# rpl-ide Development Rules

## Architecture

### Separation of Concerns

1. **TypeScript (VS Code extension)** - Stateless view layer only:
   - Type mappings between WIT types and VS Code types
   - WASM loading and initialization
   - Event wiring to VS Code APIs (document events, commands, providers)
   - NO business logic, NO project detection, NO state caching

2. **Rust/WASM** - All business logic:
   - Project detection (walk up directories to find `project.toml`)
   - Project caching (`HashMap<String, LoadedProject>`)
   - File analysis, diagnostics, hover, semantic tokens, symbols
   - REPL state management

3. **WIT interface** (`wit/ide.wit`) - The contract between WASM and TypeScript

## Code Patterns

### Namespace Conflicts

Use `::rpl::` prefix for the rpl crate to avoid conflicts with `wit_bindgen::generate!` which creates a `rpl` module:

```rust
use ::rpl::{Session, AnalysisSession};  // crate
use self::rpl::ide::types::*;            // WIT-generated
```

### Conditional Compilation

WIT bindings only compile for wasm32 target. Native builds only include the core module:

```rust
pub mod core;

#[cfg(target_arch = "wasm32")]
mod wasm;
```

### Testing

- Test through `src/core.rs` which contains the same logic as WIT exports
- Tests run natively (not in WASM) via `cargo test -p rpl-ide`
- Test fixtures live in `tests/fixtures/`

## File Structure

```
crates/rpl-ide/
├── src/
│   ├── lib.rs      # Entry point, exports core module
│   ├── core.rs     # Testable business logic (native target)
│   └── wasm.rs     # WIT bindings and exports (wasm32 only)
├── wit/
│   └── ide.wit     # Interface definition
├── tests/
│   ├── integration.rs
│   └── fixtures/
│       └── test_project/
└── vscode/
    └── src/
        └── extension.ts  # Stateless VS Code integration
```

## Project Loading

- Files are evaluated individually during load to produce values
- Cross-file references only work at runtime, not during loading
- Entry point files should be programs that reference other entries when executed, not during evaluation:
  ```rpl
  @ Good: produces a program value
  << 5 lib/square >>

  @ Bad: tries to execute lib/square during load
  << 5 lib/square >>
  "run" STO
  run
  ```
