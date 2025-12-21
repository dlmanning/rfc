# Lowering Module

Compiles IR (intermediate representation) to stack-based bytecode for the RPL virtual machine.

## Architecture

The lowerer performs a single-pass traversal of IR nodes, emitting bytecode while tracking stack depth for list construction.

```
IR Nodes ─────► LowerContext ─────► CompiledProgram
                    │                    │
                    │                    ├─ code: Vec<u8>
                    │                    ├─ rodata: Vec<u8>
                    │                    ├─ spans: Vec<Span>
                    │                    └─ span_offsets: Vec<usize>
                    │
                    ├─ BytecodeBuffer (output)
                    ├─ AnalysisResult (type info)
                    └─ Registries (lib dispatch)
```

## Type-Directed Code Generation

The lowerer uses type information from analysis to emit optimal opcodes:

```
Source          Analysis          Lowering
──────          ────────          ────────
1 2 +     →     TOS=Int           →  I64Add (native)
                NOS=Int

1.0 2 +   →     TOS=Int           →  F64ConvertI64S (coerce)
                NOS=Real             F64Add (native)

x y +     →     TOS=Unknown       →  CallLib (runtime dispatch)
                NOS=Unknown
```

### Stack Snapshot Lookup

Analysis stores `StackSnapshot` for each command node (indexed by span). The lowerer retrieves this via `stack_snapshot()`:

```rust
fn stack_snapshot(&self) -> StackSnapshot {
    if let Some(span) = self.output.current_span()
        && let Some(snapshot) = self.analysis.node_stacks.get(&span) {
            return snapshot.clone();
        }
    // Fallback for non-command contexts
    StackSnapshot { tos: Unknown, nos: Unknown, ... }
}
```

### Emit Helpers

Type-directed emit methods choose optimal opcodes:

| Method                  | Purpose                              |
| ----------------------- | ------------------------------------ |
| `emit_binary_numeric`   | Binary ops: I64 / F64 / CallLib      |
| `emit_binary_real_only` | Division: coerce to F64 or CallLib   |
| `emit_binary_comparison`| Comparisons: I64 / F64 / CallLib     |
| `emit_unary_numeric`    | Unary ops: I64 / F64 / CallLib       |

## Bytecode Format

### Instructions

| Opcode           | Format                                  |
| ---------------- | --------------------------------------- |
| `I64Const`       | `op <value:leb128>`                     |
| `F64Const`       | `op <value:f64le>`                      |
| `StringConst`    | `op <offset:leb128> <len:leb128>`       |
| `MakeList`       | `op <count:leb128>`                     |
| `MakeProgram`    | `op <params:leb128> <rodata> <code> <spans>` |
| `CallLib`        | `op <lib:u16> <cmd:u16>`                |
| `LocalGet/Set`   | `op <index:leb128>`                     |
| `Block/Loop/If`  | `op <blocktype> <end_offset:u32>`       |
| `Br/BrIf`        | `op <depth:leb128>`                     |

### Rodata Section

String constants are stored in `rodata` and referenced by offset/length. This avoids embedding strings directly in bytecode.

### Source Maps

Debug info maps bytecode offsets to source spans:

```rust
struct CompiledProgram {
    code: Vec<u8>,
    spans: Vec<Span>,        // Parallel arrays for
    span_offsets: Vec<usize>, // bytecode → source mapping
    rodata: Vec<u8>,
    param_count: u16,
}
```

## Control Flow

Control flow uses backpatching for branch targets:

```rust
enum ControlFixup {
    Block { end_offset_pos: usize },
    Loop { end_offset_pos: usize },
    If { else_offset_pos: usize, end_offset_pos: usize },
    TryTable { end_offset_pos: usize },
}
```

When `emit_end()` is called, the corresponding placeholder is patched with the actual offset.

## Local Variables

Locals are managed in three categories:

1. **Scratch locals** (indices 0-2): Reserved for stack manipulation (SWAP, ROT)
2. **User locals**: Mapped from parsed indices via `user_local_map`
3. **Dynamic locals**: Allocated during lowering for temporaries

```rust
const SCRATCH_LOCAL_COUNT: u32 = 3;

// Convert parsed local index to actual index
pub fn user_local(&mut self, parsed_idx: u32) -> u32 {
    *self.user_local_map.entry(parsed_idx)
        .or_insert_with(|| { self.next_local += 1; self.next_local - 1 })
}
```

## Depth Tracking

Stack depth is tracked for list construction:

```rust
CompositeKind::List => {
    let depth_before = self.depth;
    for item in items { self.lower(item)?; }
    let count = self.depth - depth_before;
    self.output.emit_make_list(count as u32);
}
```

This handles nested lists and dynamic item counts.

## Library Dispatch

Commands are lowered by dispatching to `LibraryLowerer` implementations:

```rust
pub fn lower_command(&mut self, lib: u16, cmd: u16) -> Result<(), LowerError> {
    if let Some(library) = self.lowerers.get(lib) {
        library.lower_command(cmd, span, self)?;
        // Apply depth effect from interface
        let effect = interface.command_effect(cmd, tos, nos);
        self.apply_depth_effect(effect);
    }
}
```

Libraries can emit specialized bytecode (typed arithmetic) or fall back to `CallLib` for runtime dispatch.

## Usage

```rust
use rpl::lower::lower;
use rpl::analysis::analyze;

// First, analyze the IR
let analysis = analyze(&nodes, &registry, &interner);

// Then, lower to bytecode
let program = lower(&nodes, &interfaces, &lowerers, &interner, &analysis)?;

// Execute or serialize the bytecode
vm.execute(&program)?;
```

## Key Types

```rust
/// Compiled program with bytecode and debug info.
struct CompiledProgram {
    code: Vec<u8>,
    spans: Vec<Span>,
    span_offsets: Vec<usize>,
    rodata: Vec<u8>,
    param_count: u16,
}

/// Lowering context with output buffer and state.
struct LowerContext<'a> {
    output: BytecodeBuffer,
    analysis: &'a AnalysisResult,
    interfaces: &'a InterfaceRegistry,
    lowerers: &'a LowererRegistry,
    depth: usize,
    depth_known: bool,
    next_local: u32,
    user_local_map: HashMap<u32, u32>,
}

/// Error during lowering.
struct LowerError {
    message: String,
    span: Option<Span>,
}
```

## Design Notes

**Analysis dependency**: The lowerer requires `AnalysisResult` for type-directed code generation. The `node_stacks` map provides pre-computed type snapshots at each command.

**Depth tracking**: While analysis provides `StackSnapshot.depth`, the lowerer maintains its own depth counter for list construction. This is because `node_stacks` only has snapshots at command nodes, not between literals.

**No type stack**: Unlike earlier designs, the lowerer doesn't maintain a type stack. All type information comes from analysis via `stack_snapshot()`.
