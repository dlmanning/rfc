# rpl-stdlib

Standard libraries for the RPL language.

## Libraries

| Library | ID | Priority | Description |
|---------|---:|:--------:|-------------|
| `CommentsLib` | 70 | 115 | Comment literals (`@ ...`) |
| `BinaryIntLib` | 71 | 110 | Binary integers (`#1010b`, `#FFh`, `#777o`) |
| `BlobLib` | 85 | 105 | Binary data containers |
| `RealNumbersLib` | 68 | 100 | Real number literals and parsing |
| `ComplexLib` | 86 | 95 | Complex numbers `(re,im)` |
| `StringsLib` | 69 | 80 | String literals (`"..."`) |
| `SymbolicLib` | 74 | 75 | Symbolic expressions (`'name'`) |
| `ListsLib` | 73 | 70 | List literals and operations |
| `ProgramsLib` | 67 | 60 | Program literals (`« ... »`) |
| `BinaryOpsLib` | 76 | 55 | Bitwise operations (`AND`, `OR`, `XOR`, etc.) |
| `ArithmeticLib` | 65 | 50 | Basic math (`+`, `-`, `*`, `/`, `^`, etc.) |
| `TranscendentalsLib` | 77 | 45 | Trig, log, exp functions |
| `FlowControlLib` | 78 | 40 | `IF/THEN/ELSE`, `FOR/NEXT`, `WHILE`, etc. |
| `DirectoryLib` | 79 | 30 | `STO`, `RCL`, `PURGE`, directory ops |
| `LocalsLib` | 80 | 25 | Local variables (`→ x y « ... »`) |
| `LibPtrLib` | 81 | 15 | User libraries and LIBPTR |
| `PlotLib` | 82 | 12 | Plot objects and rendering |
| `StackLib` | 72 | 10 | `DUP`, `DROP`, `SWAP`, `ROT`, etc. |
| `IdentifiersLib` | 66 | 1 | Variable/command name resolution |

**Priority** determines probe order. Higher priority libraries are checked first during tokenization.

## Commands by Category

### Stack Manipulation
`DUP` `DROP` `SWAP` `OVER` `ROT` `UNROT` `DEPTH` `CLEAR` `PICK` `ROLL` `DUP2` `DROP2` `NIP` `DUPN` `DROPN` `ROLLD` `DUPDUP` `PICK3` `NDUPN` `REVN` `UNPICK`

### Arithmetic
`+` `-` `*` `/` `^` `NEG` `INV` `ABS` `SIGN` `MOD` `MIN` `MAX` `%` `%CH` `%T` `FLOOR` `CEIL` `IP` `FP` `RND` `TRNC` `MANT` `XPON`

### Comparison
`==` `<` `>` `<=` `>=` `!=` `SAME` `AND` `OR` `XOR` `NOT`

### Bitwise
`BAND` `BOR` `BXOR` `BNOT` `SL` `SR` `ASR` `RL` `RR` `RLB` `RRB`

### Transcendentals
`SIN` `COS` `TAN` `ASIN` `ACOS` `ATAN` `SINH` `COSH` `TANH` `ASINH` `ACOSH` `ATANH` `LN` `LOG` `EXP` `ALOG` `SQ` `SQRT` `D→R` `R→D`

### Lists
`LIST→` `→LIST` `HEAD` `TAIL` `GET` `PUT` `SIZE` `POS` `SUB` `REPL` `REVLIST` `SORT` `DOLIST` `DOSUBS` `MAP` `STREAM` `ΔLIST` `ΣLIST` `ΠLIST` `ADD` `SEQ`

### Flow Control
`IF` `THEN` `ELSE` `END` `IFT` `IFTE` `CASE` `FOR` `NEXT` `STEP` `START` `WHILE` `REPEAT` `DO` `UNTIL` `EVAL`

### Storage
`STO` `RCL` `PURGE` `VARS` `TVARS` `ORDER` `PATH` `HOME` `CRDIR` `PGDIR` `UPDIR`

### Blobs
`MKBLOB` `BLOBTYPE` `BLOBSIZE` `BLOB→LIST` `LIST→BLOB`

## Implementation Guide

### Library Trait

Every library implements the `Library` trait from `rpl_lang`:

```rust
pub trait Library: Send + Sync + 'static {
    fn id(&self) -> LibraryId;
    fn name(&self) -> &'static str;
    fn probe(&self, ctx: &ProbeContext) -> ProbeResult;
    fn compile(&self, ctx: &mut CompileContext) -> CompileResult;
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult;
    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult;
    fn stack_effect(&self, token: &str) -> StackEffect { ... }
}
```

### Minimal Library Example

```rust
use rpl_core::token::{SemanticKind, TokenInfo};
use rpl_lang::library::{
    CompileContext, CompileResult, DecompileContext, DecompileResult,
    DecompileMode, ExecuteContext, ExecuteResult, Library, LibraryId,
    ProbeContext, ProbeResult, StackEffect,
};
use rpl_lang::Value;

pub struct MyLib;

impl MyLib {
    pub const ID: LibraryId = LibraryId::new(200); // Use 128+ for extensions

    const CMD_FOO: u16 = 0;
    const CMD_BAR: u16 = 1;

    fn command_id(name: &str) -> Option<u16> {
        match name.to_ascii_uppercase().as_str() {
            "FOO" => Some(Self::CMD_FOO),
            "BAR" => Some(Self::CMD_BAR),
            _ => None,
        }
    }

    fn command_name(id: u16) -> Option<&'static str> {
        match id {
            Self::CMD_FOO => Some("FOO"),
            Self::CMD_BAR => Some("BAR"),
            _ => None,
        }
    }
}

impl Library for MyLib {
    fn id(&self) -> LibraryId { Self::ID }
    fn name(&self) -> &'static str { "MyLib" }

    fn probe(&self, ctx: &ProbeContext) -> ProbeResult {
        if Self::command_id(ctx.text()).is_some() {
            ProbeResult::Match {
                info: TokenInfo::atom(ctx.text().len() as u8),
                semantic: SemanticKind::Command,
            }
        } else {
            ProbeResult::NoMatch
        }
    }

    fn compile(&self, ctx: &mut CompileContext) -> CompileResult {
        if let Some(cmd) = Self::command_id(ctx.text()) {
            ctx.emit_opcode(Self::ID.as_u16(), cmd);
            CompileResult::Ok
        } else {
            CompileResult::NoMatch
        }
    }

    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        match ctx.cmd() {
            Self::CMD_FOO => {
                // Pop value, do something, push result
                let val = match ctx.pop() {
                    Ok(v) => v,
                    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
                };
                if ctx.push(val).is_err() {
                    return ExecuteResult::Error("Stack overflow".into());
                }
                ExecuteResult::Ok
            }
            Self::CMD_BAR => {
                // ...
                ExecuteResult::Ok
            }
            _ => ExecuteResult::Error(format!("Unknown command: {}", ctx.cmd())),
        }
    }

    fn decompile(&self, ctx: &mut DecompileContext) -> DecompileResult {
        match ctx.mode() {
            DecompileMode::Prolog => DecompileResult::Unknown,
            DecompileMode::Call(cmd) => {
                if let Some(name) = Self::command_name(cmd) {
                    ctx.write(name);
                    DecompileResult::Ok
                } else {
                    DecompileResult::Unknown
                }
            }
        }
    }

    fn stack_effect(&self, token: &str) -> StackEffect {
        match token.to_ascii_uppercase().as_str() {
            "FOO" => StackEffect::Fixed { consumes: 1, produces: 1 },
            "BAR" => StackEffect::Fixed { consumes: 2, produces: 1 },
            _ => StackEffect::Dynamic,
        }
    }
}
```

### Registration

Add your library to `lib.rs`:

```rust
mod mylib;
pub use mylib::MyLib;

pub fn register_standard_libs(registry: &mut LibraryRegistry) {
    // ... existing registrations ...
    registry.register(MyLib, 50); // priority determines probe order
}
```

### Key Patterns

**Stack operations** - Use `ctx.pop()`, `ctx.push()`, `ctx.peek(n)`:
```rust
let a = ctx.pop()?;  // Returns Result<Value, StackError>
ctx.push(Value::Int(42))?;
let top = ctx.peek(0)?;  // 0 = top of stack
```

**Number extraction** - Handle both Int and Real:
```rust
let n = match ctx.pop() {
    Ok(Value::Int(i)) => i,
    Ok(Value::Real(r)) => r as i64,
    Ok(_) => return ExecuteResult::Error("Expected number".into()),
    Err(_) => return ExecuteResult::Error("Stack underflow".into()),
};
```

**Object types** - Use `Value::Object` for complex data:
```rust
use rpl_core::TypeId;

// Creating an object
let obj = Value::Object {
    type_id: TypeId::BLOB,  // or custom TypeId::new(n) for n >= 128
    data: vec![word1, word2, ...],  // Vec<u32>
};

// Matching an object
match value {
    Value::Object { type_id, data } if type_id == TypeId::BLOB => {
        // process data
    }
    _ => return ExecuteResult::Error("Expected blob".into()),
}
```

**Shared state** - For libraries needing mutable state, use `Arc<Mutex<T>>`:
```rust
pub struct StatefulLib {
    state: Arc<Mutex<MyState>>,
}

impl Library for StatefulLib {
    fn execute(&self, ctx: &mut ExecuteContext) -> ExecuteResult {
        let state = match self.state.lock() {
            Ok(s) => s,
            Err(_) => return ExecuteResult::Error("Lock poisoned".into()),
        };
        // use state...
        ExecuteResult::Ok
    }
}
```

### Library IDs

- **0-127**: Reserved for built-in types and stdlib
- **128+**: Available for extensions

Standard library IDs are defined in each library's `ID` constant. When creating extension libraries (like for a fantasy console), use IDs >= 128.

### Testing

Use the test helpers from existing libraries:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use rpl_core::{Interner, Pos, Span};
    use rpl_lang::VM;

    fn make_probe_ctx<'a>(text: &'a str, interner: &'a Interner) -> ProbeContext<'a> {
        let span = Span::new(Pos::new(0), Pos::new(text.len() as u32));
        ProbeContext::new(text, text, span, false, None, None, interner)
    }

    fn make_exec_ctx(vm: &mut VM, cmd: u16) -> ExecuteContext<'_> {
        ExecuteContext::new(vm, &[], 0, cmd)
    }

    #[test]
    fn test_foo() {
        let lib = MyLib;
        let mut vm = VM::new();
        vm.push(Value::Int(42)).unwrap();

        let mut ctx = make_exec_ctx(&mut vm, MyLib::CMD_FOO);
        assert!(matches!(lib.execute(&mut ctx), ExecuteResult::Ok));
    }
}
```
