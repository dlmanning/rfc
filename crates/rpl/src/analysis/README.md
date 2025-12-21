# Analysis Module

Static analysis for RPL programs, providing symbol tracking, type inference, and diagnostics.

## Architecture

The analyzer uses a **4-phase architecture** that processes IR nodes to produce type information, symbol tables, and diagnostics:

```
                                     ┌─────────────────────────────────────┐
                                     │           AnalysisResult            │
                                     ├─────────────────────────────────────┤
  IR Nodes ──┬──► Phase 1 ──────────-┤  • SymbolTable (definitions, refs)  │
             │    Pattern Recognition│  • ScopeTree (lexical scopes)       │
             │                       │  • Diagnostics (errors, warnings)   │
             ├──► Phase 2 ──────────-┤  • node_stacks (per-node types)     │
             │    Global Collection  │                                     │
             │                       └─────────────────────────────────────┘
             ├──► Phase 3 ────────────────────────────────────────────────►
             │    Main Traversal         (parallel type + origin tracking)
             │
             └──► Phase 4 ────────────────────────────────────────────────►
                  Constraint Resolution       (union-find type solving)
```

## Phases

### Phase 1: Pattern Recognition (`patterns.rs`)

Scans IR for multi-node patterns before detailed analysis begins.

**Key pattern**: Function definitions (`<< body >> "name" STO`)

- Identifies program literals followed by string + STO command
- Extracts parameter info from `-> x y << body >>` bindings
- Builds `PatternMap` indexed by the STO command's span

**Output**: `PatternMap` mapping spans to recognized patterns

### Phase 2: Global Collection (`globals.rs`)

Creates preliminary definitions for global functions identified in Phase 1.

- Pre-registers function names in the symbol table
- Assigns `TypeVar` placeholders for return types
- Enables forward references (functions can call each other)

**Output**: `GlobalMap` with function metadata, updated `SymbolTable`

### Phase 3: Main Traversal (`traverse.rs`)

Walks the IR tree with parallel tracking of types and origins.

**Type tracking** (`state.rs: StackState`):

- Simulates abstract stack with `(Type, Origin)` pairs
- Applies stack effects from commands
- Handles control flow merges with type joins

**Origin tracking**:

- `Literal(Span)` - value from source literal
- `Binding(DefinitionId)` - value from variable
- `Result(Span)` - value from operation
- `Phi(Vec<Origin>)` - merged from control flow

**Constraint collection**:

- `MustBe { def_id, requirement }` - definition must satisfy type requirement
- `Equal { def_a, def_b }` - two definitions must have same type

**Output**: `TraversalResult` with symbols, scopes, diagnostics, constraints

### Phase 4: Constraint Resolution (`resolve.rs`)

Solves collected constraints using union-find algorithm.

- Groups definitions by equality constraints
- Intersects type requirements within each group
- Propagates narrowed types back to definitions
- Reports `TypeMismatch` diagnostics for conflicts

**Output**: Resolved types in `SymbolTable`, additional diagnostics

## Type Inference

The analyzer infers types for variables and function parameters without explicit annotations. This section explains how type information flows through the system.

### Sources of Type Information

Types originate from:

1. **Literals**: `42` → Integer, `3.14` → Real, `"hello"` → String, `{ 1 2 }` → List
2. **Command results**: `+` on integers → Integer, `SIN` → Real
3. **Library declarations**: Commands declare their input requirements and output types

### Forward Flow (Traversal)

During Phase 3 traversal, types flow forward through the abstract stack:

```
1 2 +
│ │ │
│ │ └─► TOS=Int, NOS=Int → Result: Int
│ └───► Push Int (TOS=Int)
└─────► Push Int (TOS=Int)
```

When a value is bound to a variable, the stack type becomes the variable's type:

```
3.14 -> x << ... >>
│       │
│       └─► x gets type Real (from TOS)
└─────────► Push Real
```

### Backward Flow (Constraints)

When a variable is _used_, the operation's requirements create constraints that flow backward:

```
-> x << x 1 + >>
       │ │ │
       │ │ └─► + requires numeric inputs
       │ └───► Constraint: x MustBe Numeric
       └─────► x is used here
```

The constraint `MustBe { def_id: x, requirement: Numeric }` is collected and resolved in Phase 4.

### Type Variables and Function Returns

Functions with unknown return types get `TypeVar` placeholders:

```
<< 3.14 >> "pi" STO    @ pi returns Real
<< pi >> "get_pi" STO  @ get_pi returns TypeVar(0)

get_pi -> x << x 1.0 + >>
          │
          └─► x initially gets TypeVar(0)
              Later resolved to Real when pi's body is analyzed
```

The `Substitution` map tracks `TypeVar → Type` bindings, updated as return types are discovered.

### Control Flow and Type Joins

When control flow merges, types are joined (least upper bound):

```
IF cond THEN
    1        @ Int
ELSE
    2.5      @ Real
END
             @ Result: Int ∪ Real (OneOf)
```

The `Type::join()` operation produces union types when branches differ:

```rust
Int.join(Real) = OneOf([Int, Real])
Int.join(Int)  = Int
Unknown.join(T) = T
```

### Constraint Resolution

Phase 4 uses union-find to group related definitions and intersect their requirements:

```
-> a b << a b + a b * >>
         │     │
         │     └─► a MustBe Numeric (from *)
         └───────► a MustBe Numeric (from +)

Both constraints on 'a' intersect to: Numeric
```

If constraints conflict, a `TypeMismatch` diagnostic is reported:

```
-> x << x 1 + x SIZE >>
        │     │
        │     └─► x MustBe List|String (SIZE requires)
        └───────► x MustBe Numeric (+ requires)

Intersection is empty → TypeMismatch error
```

### Example: Full Inference

```rpl
<< -> lst <<
    0                     @ accumulator = Int
    lst SIZE              @ count = Int (SIZE returns Int)
    1 SWAP                @ loop bounds
    START
        lst I GET +       @ GET returns Unknown, + requires Numeric
    NEXT
>> >> "sum" STO
```

Inference steps:

1. `lst` bound from stack → initially Unknown
2. `lst SIZE` → constraint: lst MustBe List|String
3. `lst I GET` → constraint: lst MustBe List (GET on list)
4. `... +` → constraint: GET result MustBe Numeric
5. Resolution: lst = List (intersection of List|String and List)
6. Function signature: `sum: List → Int`

### Type Representation

```rust
enum Type {
    Known(TypeId),                    // e.g., Known(BINT) = definitely Int
    OneOf(SmallVec<[TypeId; 4]>),     // e.g., OneOf([BINT, REAL]) = Int or Real
    TypeVar(TypeVar),                 // e.g., TypeVar(0) = placeholder α0
    Unknown,                          // No information yet
}
```

Operations on types:

| Operation      | Description                                         |
| -------------- | --------------------------------------------------- |
| `join(a, b)`   | Least upper bound (for merging branches)            |
| `meet(a, b)`   | Greatest lower bound (for intersecting constraints) |
| `is_numeric()` | True if known to be Int or Real                     |
| `as_known()`   | Extract TypeId if Known variant                     |

### Origin Tracking

Parallel to types, origins track _where_ values came from:

```
1 -> x << x x + >>
│    │    │ │
│    │    │ └─► Origin: Binding(x)
│    │    └───► Origin: Binding(x)
│    └────────► x defined here
└─────────────► Origin: Literal(span)
```

Origins enable:

- Propagating constraints back to definitions
- Creating `Phi` nodes at control flow merges
- Accurate error spans for type conflicts

## Key Types

### Type System (`types.rs`)

```rust
enum Type {
    Known(TypeId),           // Concrete type (Int, Real, String, etc.)
    OneOf(SmallVec<[TypeId; 4]>),  // Union of possible types
    TypeVar(TypeVar),        // Placeholder for inference
    Unknown,                 // Completely unknown
}

enum Origin {
    Literal(Span),           // From source literal
    Binding(DefinitionId),   // From variable read
    LoopVar(DefinitionId),   // From FOR loop counter
    Result(Span),            // From operation result
    Phi(Vec<Origin>),        // Merged from branches
    Unknown,
}

enum Constraint {
    MustBe { def_id, requirement, span, operation },
    Equal { def_a, def_b, span },
}
```

### Symbol Table (`symbols.rs`)

```rust
struct Definition {
    id: DefinitionId,
    name: String,
    kind: DefinitionKind,    // Global, Local, LoopVar
    span: Span,
    scope: ScopeId,
    value_type: Option<Type>,
    signature: Option<Signature>,  // For functions
    arity: Option<usize>,
    referenced: bool,
}

struct Reference {
    id: ReferenceId,
    name: String,
    kind: ReferenceKind,     // Read, Write, Call
    span: Span,
    resolved_to: Option<DefinitionId>,
}
```

### Analysis Result (`result.rs`)

```rust
struct AnalysisResult {
    symbols: SymbolTable,
    scopes: ScopeTree,
    diagnostics: Vec<Diagnostic>,
    node_stacks: HashMap<Span, StackSnapshot>,  // For lowerer
}

struct StackSnapshot {
    tos: Type,          // Top of stack type
    nos: Type,          // Next on stack type (for binary ops)
    depth: usize,
    depth_known: bool,
}
```

**Design note**: TOS and NOS are sufficient for current instruction selection (binary ops,
comparisons, coercion). If future optimizations need deeper stack access, this could be
extended to `Vec<Type>` or additional fields.

**Indexing note**: `node_stacks` uses `Span` as the key. This works because each IR node
has a unique span from parsing. In languages with macros or code generation where spans
might collide, a node ID or traversal index would be more robust. RPL currently has no
macros, so span-based indexing is sufficient.

## Information Flow

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│  IR Nodes      │────►│  PatternMap     │────►│  GlobalMap       │
│                │     │  (Phase 1)      │     │  (Phase 2)       │
└────────────────┘     └─────────────────┘     └──────────────────┘
                                                        │
                                                        ▼
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│  Constraints   │◄────│  Traverser      │◄────│  Context         │
│  (Phase 3)     │     │  (Phase 3)      │     │  - StackState    │
└────────────────┘     └─────────────────┘     │  - Substitution  │
        │                      │               └──────────────────┘
        │                      ▼
        │              ┌─────────────────┐
        │              │  SymbolTable    │
        │              │  ScopeTree      │
        │              │  Diagnostics    │
        │              │  node_stacks    │
        │              └─────────────────┘
        │                      ▲
        ▼                      │
┌────────────────┐             │
│  Resolver      │─────────────┘
│  (Phase 4)     │  (narrows types, adds diagnostics)
└────────────────┘
```

## Usage

```rust
use rpl::analysis::analyze;

let result = analyze(&nodes, &registry, &interner);

// Access symbols
for def in result.symbols.definitions() {
    println!("{}: {:?}", def.name, def.value_type);
}

// Check for errors
for diag in &result.diagnostics {
    if diag.severity == Severity::Error {
        println!("Error at {:?}: {}", diag.span, diag.message);
    }
}

// Get stack types for lowering
if let Some(snapshot) = result.node_stacks.get(&command_span) {
    let tos_type = &snapshot.tos;
}
```

## Module Files

| File             | Purpose                                         |
| ---------------- | ----------------------------------------------- |
| `mod.rs`         | Entry point, `analyze()` function, re-exports   |
| `patterns.rs`    | Phase 1: Pattern recognition                    |
| `globals.rs`     | Phase 2: Global collection                      |
| `traverse.rs`    | Phase 3: Main IR traversal                      |
| `resolve.rs`     | Phase 4: Constraint resolution                  |
| `types.rs`       | `Type`, `Origin`, `Constraint`, `StackSnapshot` |
| `state.rs`       | `StackState`, `Substitution`, `Context`         |
| `symbols.rs`     | `SymbolTable`, `Definition`, `Reference`        |
| `scopes.rs`      | `ScopeTree`, `Scope`, `ScopeId`                 |
| `result.rs`      | `AnalysisResult`, `Diagnostic`                  |
| `visitor.rs`     | `Visitor` trait for IR traversal                |
| `incremental.rs` | Incremental analysis support                    |

## Diagnostics

| Kind                  | Severity | Description                      |
| --------------------- | -------- | -------------------------------- |
| `UndefinedVariable`   | Error    | Reference to unknown variable    |
| `TypeMismatch`        | Error    | Conflicting type constraints     |
| `StackUnderflow`      | Error    | Operation needs more stack items |
| `DuplicateDefinition` | Error    | Same name defined twice in scope |
| `UnusedVariable`      | Warning  | Local variable never read        |
| `ShadowedVariable`    | Warning  | Inner binding hides outer        |
| `WriteOnlyVariable`   | Warning  | Variable written but never read  |

**Future diagnostics** (not yet implemented):

| Kind              | Severity | Description                           |
| ----------------- | -------- | ------------------------------------- |
| `UnreachableCode` | Warning  | Code after unconditional RETURN/ABORT |
