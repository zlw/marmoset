# Ad-hoc Polymorphism: Final Approach

After exploring typeclasses, static dispatch, and abilities, we settled on a **Rust-like impl model with TypeScript-like structural traits**.

## The Journey

```
Typeclasses (Haskell/Rust)
    ↓
Static dispatch (Roc)
    ↓
Abilities (Koka/Unison) → "too complex, feels like goto"
    ↓
Back to Rust-like impl + traits
    ↓
Add TS-like structural fields to traits
    ↓
✓ Final approach
```

---

## Final Model

### Traits (with fields AND methods)

Unlike Rust (methods only), our traits can specify fields too (like TypeScript interfaces):

```
// Methods only (Rust-like)
trait Show {
    show: (self) -> string
}

// Fields only (structural)
trait HasName {
    name: string
}

// Both (TypeScript-like)
trait Entity {
    id: int                     // required field
    name: string                // required field
    show: (self) -> string      // required method
    save: (self) => unit        // method with effect
}

// Trait composition
trait PrintableEntity : HasName + Show {
    id: int
}
```

### Impl Blocks

All methods live in `impl` blocks. One syntax for everything:

```
// Define type (just data)
type user = { name: string, id: int, email: string }

// Add methods to your type
impl user {
    show(self) -> string { self.name }
    greeting(self) -> string { "Hello, " ++ self.name }
}

// Implement trait for your type
impl Show for user {
    show(self) -> string { self.name }
}

// Extend foreign type (same syntax!)
impl library.foreign_user {
    show(self) -> string { self.name }
}

// Implement trait for foreign type
impl Show for library.foreign_user {
    show(self) -> string { self.name }
}

// Generic impl
impl Show for list[a] where a: Show {
    show(self) -> string { 
        "[" ++ self.map(.show()).join(", ") ++ "]" 
    }
}
```

### Constraints

```
// Single constraint
fn print(x: a) where a: Show {
    x.show().puts()
}

// Multiple constraints
fn process(x: a) where a: Show + Entity {
    (x.name ++ ": " ++ x.show()).puts()
}

// Field constraint via trait
fn greet(x: a) where a: HasName {
    ("Hello, " ++ x.name).puts()
}
```

### Dispatch

```
// Method call
my_user.show()

// Compiler looks for show in:
// 1. impl blocks for user
// 2. trait impls for user
// Found → call it
```

---

## Union Types + Traits

Union types work with traits if ALL variants satisfy the trait:

```
// Both int and string have Show impls
impl Show for int { ... }
impl Show for string { ... }

// So union satisfies Show
fn print(x: int | string) {
    x.show()  // works!
}

// Compiler generates runtime dispatch:
// switch on type, call correct show
```

**When monomorphization vs runtime:**

| Scenario | Dispatch |
|----------|----------|
| `print(42)` - known concrete | Static (monomorphize) |
| `print(x)` where `x: int` | Static (monomorphize) |
| `print(x)` where `x: int \| string` unknown | Runtime (type switch) |

```
// Known at compile time → monomorphize
print(42)           // generates print_int
print("hello")      // generates print_string

// Unknown at compile time → runtime dispatch
let x: int | string = if cond then 42 else "hello"
print(x)            // generates switch
```

---

## Existential Types

For "I have some type that implements Show, but I won't tell you which":

```
// Existential / trait object
let x: dyn Show = 42
let y: dyn Show = "hello"

// Heterogeneous list
let items: list[dyn Show] = [42, "hello", my_user]

// Can use trait methods, but type is erased
items.each(fn(x) { x.show().puts() })
```

**Dispatch:**

| | Syntax | Dispatch | Codegen |
|--|--------|----------|---------|
| Generic | `a where a: Show` | Static | Monomorphized functions |
| Existential | `dyn Show` | Runtime | Go interface / vtable |

```
// Static dispatch (default) - zero cost
fn print(x: a) where a: Show {
    x.show().puts()
}

// Dynamic dispatch (explicit) - small cost
fn print_dyn(x: dyn Show) {
    x.show().puts()
}
```

---

## Comparison: Why This Approach?

### vs Haskell Typeclasses

| | Haskell | Marmoset |
|--|---------|----------|
| Dispatch | Dictionary passing (runtime) | Monomorphization (compile time) |
| Why? | Separate compilation, laziness | Whole-program, strict |
| Traits have fields? | No | Yes |

### vs Roc Static Dispatch

| | Roc | Marmoset |
|--|-----|----------|
| Where methods live | Type's module | `impl` blocks |
| Extend foreign types | `extend` keyword | Same `impl` syntax |
| Why we differ | One syntax simpler than two | |

### vs Go Interfaces

| | Go | Marmoset |
|--|-----|----------|
| Dispatch | Runtime (vtable) | Static (monomorphize) + runtime for dyn |
| Satisfaction | Structural | Structural |
| Why we differ | Zero-cost when possible | |

### vs Swift Protocols

| | Swift | Marmoset |
|--|-------|----------|
| Dispatch | Mix of static/dynamic | Static default, dynamic explicit |
| OO interop | Yes (Objective-C) | No |
| Why we differ | No OO baggage | |

### vs Abilities (Koka/Unison)

| | Abilities | Marmoset |
|--|-----------|----------|
| Ad-hoc poly | Handlers at call site | Impls at definition |
| Effects | Same mechanism | Separate (`->` vs `=>`) |
| Why we differ | Too complex, "structured goto" | |

---

## Codegen to Go

### Trait → Interface (for existentials)

```
trait Show {
    show: (self) -> string
}
```

```go
type Show interface {
    Show() string
}
```

### Impl → Methods or Functions

```
impl Show for user {
    show(self) { self.name }
}
```

```go
// Method on type
func (u User) Show() string {
    return u.Name
}

// Or standalone function (for monomorphization)
func user_show(u User) string {
    return u.Name
}
```

### Generic Function → Monomorphized

```
fn print(x: a) where a: Show {
    x.show().puts()
}

print(42)
print("hello")
```

```go
func print_int(x int64) {
    puts(int_show(x))
}

func print_string(x string) {
    puts(string_show(x))
}
```

### Existential → Interface

```
fn print_dyn(x: dyn Show) {
    x.show().puts()
}
```

```go
func print_dyn(x Show) {  // Go interface
    puts(x.Show())
}
```

### Union → Type Switch

```
fn print(x: int | string) {
    x.show()
}

let x: int | string = if cond then 42 else "hello"
print(x)
```

```go
func print_int_or_string(x any) {
    switch v := x.(type) {
    case int64:
        puts(int_show(v))
    case string:
        puts(string_show(v))
    }
}
```

---

## Summary

| Feature | Approach |
|---------|----------|
| **Traits** | Fields + methods (TS-like) |
| **Methods** | `impl` blocks (Rust-like) |
| **Extend foreign types** | `impl` (same syntax) |
| **Default dispatch** | Static (monomorphization) |
| **Existentials** | `dyn Trait` with runtime dispatch |
| **Unions** | Runtime dispatch when type unknown |
| **Effects** | Separate system (`->` vs `=>`) |

**What we get:**
- Rust's zero-cost abstractions
- TypeScript's flexible structural traits
- One syntax for everything (`impl`)
- Extend any type
- Existentials when needed

**What we avoid:**
- Abilities complexity
- Module-based dispatch limitations
- Pure runtime dispatch (Go)
- OO baggage (Swift)
