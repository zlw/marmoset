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
fn fib(n) = {
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
fn reduce(values, initial, step) = {
  let iter = (remaining, acc) -> {
    if (len(remaining) == 0) { return acc }
    iter(rest(remaining), step(acc, first(remaining)))
  }
  iter(values, initial)
}

fn sum(values) = reduce(values, 0, (acc, v) -> acc + v)

puts(sum([1, 2, 3, 4, 5]))  # 15
```

**Type-annotated — for when you want to be explicit:**
```marmoset
fn reduce(values: List[Int], initial: Int, step: (Int, Int) -> Int) -> Int = {
  let iter = (remaining: List[Int], acc: Int) -> {
      if (len(remaining) == 0) { return acc }
      iter(rest(remaining), step(acc, first(remaining)))
  }
  iter(values, initial)
}

fn sum(values: List[Int]) -> Int = reduce(values, 0, (acc: Int, v: Int) -> acc + v)

puts(sum([1, 2, 3, 4, 5]))  # 15
```

Both compile to the exact same binary. You choose the style.

---

## 🍌 What does Marmoset look like?

### 🔧 Functions & closures

First-class functions with lexical closures:

```marmoset
fn apply_discount(rate) = (price) -> price - price * rate / 100

let black_friday = apply_discount(25)
puts(black_friday(200))  # 150
puts(black_friday(80))   # 60
```

Generics with trait constraints:

```marmoset
fn deduplicate[a: Eq & Show](items: List[a]) -> List[a] = {
  let iter = (remaining: List[a], seen: List[a]) -> {
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
type User = { name: Str, email: Str, age: Int }

let alice: User = { name: "alice", email: "alice@example.com", age: 30 }
let updated = { ...alice, email: "new@example.com" }

puts(updated.name)   # alice
puts(updated.email)  # new@example.com
```

### 🎯 Enums & pattern matching

Algebraic data types with exhaustive pattern matching:

```marmoset
enum Shape = { Circle(Int) Rect(Int, Int) }

fn area(s: Shape) -> Int = match s {
  case Shape.Circle(r): r * r * 3
  case Shape.Rect(w, h): w * h
}

puts(area(Shape.Circle(5)))    # 75
puts(area(Shape.Rect(4, 6)))   # 24
```

Generic enums with methods — model your domain:

```marmoset
enum Task[a] = { Pending(a) Done(a) Cancelled }

impl Task[a] = {
  fn is_active(self: Task[a]) -> Bool = match self {
    case Task.Pending(_): true
    case Task.Done(_): false
    case Task.Cancelled: false
  }
}

let job = Task.Pending("deploy v2.1")
puts(job.is_active())  # true
```

### 🤝 Traits

**Field traits** — satisfied structurally, just by having the right shape:

```marmoset
trait Named = { name: Str }

fn greet[a: Named](x: a) -> Str = "hello, " + x.name

# Any record with a `name: Str` field works
let person = { name: "alice", age: 30 }
let company = { name: "acme", employees: 10 }

puts(greet(person))   # hello, alice
puts(greet(company))  # hello, acme
```

**Method traits** — require explicit impls:

```marmoset
enum Severity = { Info Warning Error }

trait Show[a] = {
  fn show(x: a) -> Str
}

impl Show[Severity] = {
  fn show(s: Severity) -> Str = match s {
    case Severity.Info: "INFO"
    case Severity.Warning: "WARN"
    case Severity.Error: "ERROR"
  }
}

puts(Severity.Warning.show())  # WARN
```

**Supertraits** compose — `ord` implies `eq`:

```marmoset
trait Eq[a] = { fn eq(x: a, y: a) -> Bool }

trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Int }
```

### 🛠️ Inherent methods

Attach methods directly to any type — no trait ceremony:

```marmoset
type Cart = { items: List[Int] }

impl Cart = {
  fn total(c: Cart) -> Int = reduce(c.items, 0, (acc, price) -> acc + price)

  fn with_item(c: Cart, price: Int) -> Cart = { items: push(c.items, price) }
}

let c = Cart.with_item({ items: [] }, 25)
let c = c.with_item(50)
puts(c.total())  # 75
```

Method-level generics — type args are inferred from usage, but you can be explicit when you want:

```marmoset
type Box = { v: Int }

impl Box = { fn transform[b](bx: Box, f: (Int) -> b) -> b = f(bx.v) }

let b: Box = { v: 42 }

# Type argument inferred from the callback's return type
let label = b.transform((n: Int) -> "item-" + n.show())

# Or specify it explicitly
let label = b.transform[Str]((n: Int) -> "item-" + n.show())

puts(label)  # item-42
```

### 🔀 Union types

Type-safe unions with compile-time narrowing:

```marmoset
fn parse_port(input: Int | Str) -> Int = {
  if (input is Int) {
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
fn log(msg: Str) => Str = {
  puts(msg)
  msg
}

# Pure function — just computes a value
fn greet(name: Str) -> Str = "hello, " + name

# This would be a compile error:
# fn bad(name: Str) -> Str = { puts(name); name }
#                         ^^ pure function can't call puts
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
| VS Code | `tools/vscode-marmoset/` |
| Zed | `tools/zed-marmoset/` |
| Neovim | `tools/nvim-marmoset/` |
| JetBrains | `tools/jetbrains-marmoset/` |

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
