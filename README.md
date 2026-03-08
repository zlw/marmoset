<p align="center">
  <h1 align="center">🐒 Marmoset</h1>
  <p align="center">
    <strong>A statically typed language with full type inference that compiles to Go.</strong>
    <br/>
    Write concise, expressive code. Get fast native binaries.
  </p>
</p>

<br/>

```marmoset
let fib = fn(n) {
  if (n < 2) { return n }
  fib(n - 2) + fib(n - 1)
}

puts(fib(35))
```

```sh
$ marmoset run fib.mr
9227465
```

---

## 🐵 What is Marmoset?

Marmoset is a **fully statically typed** language that compiles to **native binaries through Go**. Every expression has a known type at compile time — but you rarely have to write any of them. The compiler uses **Hindley-Milner type inference** to figure out all types from context.

This isn't gradual typing. There's no `any`, no untyped escape hatch. You get the same guarantees whether you annotate everything or nothing. Annotations are documentation, not requirements.

**No annotations — the compiler infers everything:**
```marmoset
let reduce = fn(values, initial, step) {
  let iter = fn(remaining, acc) {
    if (len(remaining) == 0) { return acc }
    iter(rest(remaining), step(acc, first(remaining)))
  }
  iter(values, initial)
}

let sum = fn(values) {
  reduce(values, 0, fn(acc, v) { acc + v })
}

puts(sum([1, 2, 3, 4, 5]))  # 15
```

**Fully annotated — for when you want to be explicit:**
```marmoset
let reduce = fn(values: list[int], initial: int, step: fn(int, int) -> int) -> int {
  let iter = fn(remaining: list[int], acc: int) -> int {
    if (len(remaining) == 0) { return acc }
    iter(rest(remaining), step(acc, first(remaining)))
  }
  iter(values, initial)
}

let sum = fn(values: list[int]) -> int {
  reduce(values, 0, fn(acc: int, v: int) -> int { acc + v })
}

puts(sum([1, 2, 3, 4, 5]))  # 15
```

Both compile to the exact same binary. You choose the style.

---

## 🍌 What does Marmoset look like?

### 🔧 Functions & closures

First-class functions with lexical closures:

```marmoset
let apply_discount = fn(rate) {
  fn(price) { price - price * rate / 100 }
}

let black_friday = apply_discount(25)
puts(black_friday(200))  # 150
puts(black_friday(80))   # 60
```

Generics with trait constraints:

```marmoset
let deduplicate = fn[a: eq + show](items: list[a]) -> list[a] {
  let iter = fn(remaining: list[a], seen: list[a]) -> list[a] {
    if (len(remaining) == 0) { return seen }
    
    let head = first(remaining)
    let tail = rest(remaining)
    iter(tail, push(seen, head))
  }
  iter(items, [])
}
```

### 📦 Records

Structural typing with type aliases and spread updates:

```marmoset
type user = { name: string, email: string, age: int }

let alice: user = { name: "alice", email: "alice@example.com", age: 30 }
let updated = { ...alice, email: "new@example.com" }

puts(updated.name)   # alice
puts(updated.email)  # new@example.com
```

### 🎯 Enums & pattern matching

Algebraic data types with exhaustive pattern matching:

```marmoset
enum shape {
  circle(int)
  rect(int, int)
}

let area = fn(s: shape) -> int {
  match s {
    shape.circle(r): r * r * 3
    shape.rect(w, h): w * h
  }
}

puts(area(shape.circle(5)))    # 75
puts(area(shape.rect(4, 6)))   # 24
```

Generic enums with methods — model your domain:

```marmoset
enum task[a] {
  pending(a)
  done(a)
  cancelled
}

impl task[a] {
  fn is_active(t: task[a]) -> bool {
    match t {
      task.pending(_): true
      task.done(_): false
      task.cancelled: false
    }
  }
}

let job = task.pending("deploy v2.1")
puts(job.is_active())  # true
```

### 🤝 Traits

**Field traits** — satisfied structurally, just by having the right shape:

```marmoset
trait named {
  name: string
}

let greet = fn[t: named](x: t) -> string {
  "hello, " + x.name
}

# Any record with a `name: string` field works
let person = { name: "alice", age: 30 }
let company = { name: "acme", employees: 10 }

puts(greet(person))   # hello, alice
puts(greet(company))  # hello, acme
```

**Method traits** — require explicit impls:

```marmoset
enum severity { info warning error }

trait show[a] {
  fn show(x: a) -> string
}

impl show for severity {
  fn show(s: severity) -> string {
    match s {
      severity.info: "INFO"
      severity.warning: "WARN"
      severity.error: "ERROR"
    }
  }
}

puts(severity.warning.show())  # WARN
```

**Supertraits** compose — `ord` implies `eq`:

```marmoset
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}

trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
```

### 🛠️ Inherent methods

Attach methods directly to any type — no trait ceremony:

```marmoset
type cart = { items: list[int] }

impl cart {
  fn total(c: cart) -> int {
    reduce(c.items, 0, fn(acc, price) { acc + price })
  }

  fn with_item(c: cart, price: int) -> cart {
    { items: push(c.items, price) }
  }
}

let c = cart.with_item({ items: [] }, 25)
let c = c.with_item(50)
puts(c.total())  # 75
```

Method-level generics — type args are inferred from usage, but you can be explicit when you want:

```marmoset
type box = { v: int }

impl box {
  fn transform[b](bx: box, f: fn(int) -> b) -> b {
    f(bx.v)
  }
}

let b: box = { v: 42 }

# Type argument inferred from the callback's return type
let label = b.transform(fn(n: int) -> string { "item-" + n.show() })

# Or specify it explicitly
let label = b.transform[string](fn(n: int) -> string { "item-" + n.show() })

puts(label)  # item-42
```

### 🔀 Union types

Type-safe unions with compile-time narrowing:

```marmoset
let parse_port = fn(input: int | string) -> int {
  if (input is int) {
    input
  } else {
    8080
  }
}

puts(parse_port(3000))      # 3000
puts(parse_port("default")) # 8080
```

### ✨ Effect tracking

Pure functions (`->`) can't call effectful ones (`=>`). The compiler enforces this:

```marmoset
# This function does I/O — must be marked effectful
let log = fn(msg: string) => string {
  puts(msg)
  msg
}

# Pure function — just computes a value
let greet = fn(name: string) -> string {
  "hello, " + name
}

# This would be a compile error:
# let bad = fn(name: string) -> string { puts(name); name }
#                               ^^ pure function can't call puts
```

Omit the arrow entirely and the compiler infers it from the body.

---

## 🚀 Install Marmoset

### Requirements

- OCaml 5.x + opam + dune
- Go 1.21+ (for compiling and running Marmoset programs)

### From source

```sh
git clone https://github.com/marmoset-lang/marmoset.git
cd marmoset
make install    # install OCaml dependencies
make release    # build the compiler → ./marmoset
```

Verify it works:

```sh
./marmoset examples/fibonacci.mr
```

---

## 🖥️ Usage

```sh
# Run directly (compile + execute)
./marmoset examples/fibonacci.mr

# Compile to a standalone binary
./marmoset build examples/fibonacci.mr -o fib
./fib

# Build optimized release binary (stripped, trimmed)
./marmoset release examples/fibonacci.mr -o fib

# Type-check only (no codegen)
./marmoset check examples/fibonacci.mr

# Inspect the generated Go source
./marmoset build examples/fibonacci.mr -go out/

# Start the LSP server
./marmoset lsp
```

---

## ✏️ Editor Support

Marmoset ships with an LSP server and plugins for:

| Editor | Path |
|--------|------|
| VS Code | `editors/vscode/` |
| Zed | `editors/zed/` |
| Neovim | `editors/nvim/` |
| JetBrains | `editors/jetbrains/` |

Features: diagnostics, hover, signature help, semantic tokens, inlay hints, folding ranges, selection ranges.

---

## 🧪 Testing

```sh
make unit                   # unit tests
make integration            # full integration suite
make integration traits     # run a specific suite
```

---

## 📄 Docs

Detailed feature specs and architecture docs live in [`docs/`](docs/):

- [`docs/INDEX.md`](docs/INDEX.md) — doc map
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) — compiler pipeline
- [`docs/features/`](docs/features/) — per-feature specs

---

<p align="center">
  <sub>🐒 Under active development — expect breaking changes and bananas.</sub>
</p>
