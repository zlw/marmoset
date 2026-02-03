# Marmoset Syntax Decisions

**Last updated**: 2026-02-03  
**Status**: APPROVED - Lock these decisions to prevent re-discussion

This document captures ALL syntax decisions for the Marmoset language to avoid repeated discussions. Changes require explicit decision records.

---

## 1. Type Annotation Syntax

### Phase 2: Type Annotations (Minimal Bidirectional)

#### Let Bindings
```marmoset
let x: int = 5
let y: string = "hello"
let z: list[int] = [1, 2, 3]
let w: map[string, int] = {"a": 1}
```

#### Function Parameters
```marmoset
fn add(x: int, y: int) { x + y }
fn print(x: string) { puts(x) }
fn identity(x) { x }                    // No annotation = stays polymorphic
```

#### Function Return Types
```marmoset
fn add(x: int, y: int) -> int { x + y }
fn process(x: string) -> unit { puts(x) }
fn identity(x) -> a { x }               // Can use type variables in return
```

#### Generic Type Parameters
```marmoset
fn id[a](x: a) -> a { x }
fn max[a: ord](x: a, y: a) -> a { ... }
fn map[a, b](arr: list[a], f: fn(a) -> b) -> list[b] { ... }
```

**Key principle**: Generics use `[Type]` bracket notation (Scala 3 style). Type constructors like `list`, `map`, `option`, `result` are just regular generics taking type arguments in brackets.

#### Type Constraints (Trait Bounds)
```marmoset
fn max[a: ord](x: a, y: a) -> a { ... }                        // Single constraint
fn process[a: show + eq](x: a) -> unit { ... }                 // Multiple constraints with +
fn complex[a: show + ord, b: eq](x: a, y: b) -> result { ... } // Multiple generic params
```

**Key principle**: Use `+` to combine multiple trait constraints (consistent with Phase 3 trait composition: `trait A : B + C { }`).

#### Effect Annotations (Phase 5 - Syntax Only in Phase 2)
```marmoset
fn read_file(path: string) => result[string, io_error] { ... }
fn pure_fn(x: int) -> int { x + 1 }                            // -> for pure
fn impure_fn(x: int) => int { puts(x); x }                     // => for impure
```

**Key principle**: `->` for pure functions, `=>` for functions with effects (IO, async, etc.).

#### Type Ascription (Expressions)
```marmoset
let x = (5 : int)                        // Explicit type for expression
let y = ("hello" : string)
let z = (fn(x) { x } : fn(int) -> int)
```

---

## 2. Type Representation

### Primitive Types
```marmoset
int        // 64-bit signed integer
float      // floating point
bool       // true/false
string     // immutable string
unit       // null/void
```

All lowercase.

### Compound Types
```marmoset
list[int]                // list of int (generic type constructor)
map[string, int]         // map with string keys, int values (generic)
set[user]                // set of user (generic)
option[string]           // option wrapping string (generic)
result[int, error]       // result with value type and error type (generic)
fn(int, string) -> bool  // function type: (int, string) -> bool
fn(int) -> string        // function type: int -> string
```

**Key principle**: No special syntax. `list`, `map`, `set`, `option`, `result` are all type constructors that take type arguments in brackets, just like user-defined generics.

### Union Types (Phase 3)
```marmoset
int | string             // union of int and string
int | string | bool      // three-way union
list[int] | none         // union with generic types
```

Use `|` for union types. Works with any types, including generics.

### Generic Types
```marmoset
list[a]                  // list of any type a
map[a, b]                // map with key type a, value type b
fn(a) -> b               // function from a to b
```

Lowercase letters for type variables. Type constructors apply types using bracket notation.

### Traits (Phase 3)
```marmoset
show                     // trait name, lowercase
eq + ord                 // multiple traits combined with +
```

---

## 3. Annotations in Different Contexts

### Let Bindings (Required for non-inferred types)
```marmoset
// Optional when HM can infer
let x = 5                            // infers to int
let identity = fn(x) { x }          // infers to ∀a. a -> a

// Required for:
// - Constraints (forces instantiation)
let max: fn[a: ord](a, a) -> a = fn(x, y) { ... }

// - Polymorphic in unexpected ways
let make_list: fn[a](a) -> [a] = fn(x) { [x] }
```

### Function Definitions (Recommended for clarity)
```marmoset
// Can omit if HM infers correctly:
fn add(x, y) { x + y }                           // HM infers Int -> Int -> Int

// But better with annotations:
fn add(x: int, y: int) -> int { x + y }

// Required for:
// - Generic functions
fn identity[a](x: a) -> a { x }

// - Trait constraints
fn print[a: show](x: a) -> unit { x.show().puts() }

// - Effect annotations
fn read(path: string) => result[string, io_error] { ... }
```

### Annotations Inside Functions
```marmoset
fn process(data: string) -> int {
    let parsed: int = to_int(data)  // Optional if to_int already annotated
    let result = processed + 1       // No annotation needed
    result
}
```

---

## 4. Case Sensitivity

**DECISION**: Everything lowercase initially.

- Keywords: `let`, `fn`, `if`, `else`, `trait`, `impl`, `where`, etc.
- Type names: `int`, `string`, `bool`, `unit`, `result`, `option`, etc.
- Trait names: `show`, `eq`, `ord`, `clone`, etc.
- Function names: `add`, `map`, `filter`, `identity`, etc.
- Type variables: `a`, `b`, `t`, etc.

**Note**: May revisit capitalization later (e.g., for constructors, special types), but not now.

---

## 5. Syntax for Common Patterns

### Error Handling (Phase 3: Try Expressions)
```marmoset
// Phase 3 syntax (not Phase 2):
let user = try fetch_user(id)
let user = try fetch_user(id) else default_user
let user = try fetch_user(id) else |err| handle_error(err)
```

### Pattern Matching (Phase 3)
```marmoset
// Phase 3 syntax (not Phase 2):
match value {
    case some(x) => x
    case none => 0
}
```

### Enum/Variant Definition (Phase 3)
```marmoset
// Phase 3 syntax (not Phase 2):
// Tagged unions with data (Rust/OCaml-style, not C-style enums)

enum option[a] {
    some(a)
    none
}

enum result[a, e] {
    ok(a)
    err(e)
}

// Custom error types for result handling
enum user_repo_error {
    not_saved
    not_unique
    full_storage(string)  // Can carry data
}

// Using in result type
fn persist_user(u: user) => result[user, user_repo_error] {
    ...
}

// Pattern matching on result - extracts data from variants
match persist_user(user) {
    case ok(saved_user) => ...
    case err(not_saved) => ...
    case err(not_unique) => ...
    case err(full_storage(msg)) => println(msg)
}

// Constructors are functions
let x = some(5)           // x: option[int]
let y = err(full_storage("disk full"))  // y: result[user, user_repo_error]
```

**Key principle**: Enums are tagged unions with data (like Rust, OCaml, not C). Each variant can carry associated data. Pattern matching extracts the data. Compile to Go `interface{}` with runtime type switches or discriminated unions.

### Trait Definition (Phase 3)
```marmoset
// Phase 3 syntax (not Phase 2):
trait show {
    show: (self) -> string
}

trait has_name {
    name: string
}

trait printable : show + has_name {
    id: int
}
```

### Impl Block (Phase 3)
```marmoset
// Phase 3 syntax (not Phase 2):
impl user {
    display(self) -> string { self.name }
}

impl show for user {
    show(self) -> string { self.name }
}

impl show for int {
    show(self) -> string { to_string(self) }
}
```

---

## 6. Things NOT in Phase 2

The following are Phase 3+ and should NOT be implemented in Phase 2:

- ❌ Trait definitions (Phase 3)
- ❌ Impl blocks (Phase 3)
- ❌ Union types as a type construct (Phase 3) — syntax only
- ❌ Pattern matching (Phase 3)
- ❌ Enum/variant definitions (Phase 3)
  - Tagged unions with data: `enum option[a] { some(a), none }`
  - Custom error types: `enum user_repo_error { not_saved, not_unique, full_storage(string) }`
  - In Phase 2, you can write `result[user, user_repo_error]` as a type, but enum *definitions* come in Phase 3
  - Pattern matching on variants also Phase 3
- ❌ Try expressions (Phase 3)
- ❌ Effect system semantics (Phase 5) — only syntax parsing in Phase 2
- ❌ Full bidirectional type checking (Phase 2.5 or Phase 3)

Phase 2 = **Syntax + Minimal Checking Only**

**Important**: Users can already write `result[user, user_repo_error]` as a type annotation in Phase 2 (it's just a generic type). The enum *definition* and *pattern matching* come in Phase 3. Phase 2 just accepts the syntax.

---

## 7. Summary Table

| Feature | Syntax | Phase | Implemented |
|---------|--------|-------|-------------|
| Let annotations | `let x: int = 5` | 2 | ✓ Phase 2 |
| Param annotations | `fn(x: int)` | 2 | ✓ Phase 2 |
| Return annotations | `fn(...) -> int` | 2 | ✓ Phase 2 |
| Generic params | `fn[a](x: a)` | 2 | ✓ Phase 2 |
| Trait constraints | `fn[a: show]` | 2 | ✓ Phase 2 (parsing only) |
| Multiple constraints | `[a: show + eq]` | 2 | ✓ Phase 2 (parsing only) |
| Effect arrow | `fn(...) => result` | 5 | ✓ Phase 2 (parsing only) |
| Type ascription | `(expr : type)` | 2 | ✓ Phase 2 |
| Trait definitions | `trait show { }` | 3 | ✗ Phase 3 |
| Impl blocks | `impl show for int` | 3 | ✗ Phase 3 |
| Enum definitions | `enum option[a] { some(a), none }` | 3 | ✗ Phase 3 |
| Tagged unions with data | `some(5)`, `err(msg)` constructors | 3 | ✗ Phase 3 |
| Custom error types | `enum user_repo_error { not_saved, full_storage(string) }` | 3 | ✗ Phase 3 |
| Union types | `int \| string` | 3 | ✗ Phase 3 |
| Try expressions | `try expr else ...` | 3 | ✗ Phase 3 |
| Pattern matching | `match { case ... }` | 3 | ✗ Phase 3 |

---

## Decision Record

| Decision | Date | Rationale |
|----------|------|-----------|
| Use `[a: trait]` for generics with constraints | 2026-02-03 | Consistent with bracket notation, matches existing language style |
| Use `+` for trait composition | 2026-02-03 | Already used in Phase 3 trait syntax: `trait A : B + C` |
| Lowercase everything | 2026-02-03 | Simpler initially, revisit capitalization later if needed |
| `->` for pure, `=>` for impure | 2026-02-03 | Mirrors effect distinction in language philosophy |
| Keep trait constraint syntax in Phase 2 (parsing only) | 2026-02-03 | Sets up Phase 3, allows error messages to guide users |
| Minimal bidirectional in Phase 2 | 2026-02-03 | Get basics working, full bidirectional checking deferred |

