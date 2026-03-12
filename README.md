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
enum Fruit = { Banana, Mango, Coconut }

fn snack_name(fruit: Fruit) -> Str = match fruit {
  case Fruit.Banana: "banana"
  case Fruit.Mango: "mango"
  case Fruit.Coconut: "coconut"
}

puts(snack_name(Fruit.Mango))
```

```sh
$ marmoset run snack.mr
mango
```

---

## 🐵 What is Marmoset?

Marmoset is a **fully statically typed** language that compiles to **native binaries through Go**. Every expression has a known type at compile time — but you rarely have to write any of them. The compiler uses **Hindley-Milner type inference** to figure out all types from context.

This isn't gradual typing. There's no `any`, no untyped escape hatch. You get the same guarantees whether you annotate everything or nothing. Annotations are documentation, not requirements.

**No annotations — the compiler infers everything:**
```marmoset
fn banana_total(monkeys, bananas_each) = monkeys * bananas_each

puts(banana_total(3, 4))  # 12
```

**Type-annotated — for when you want to be explicit:**
```marmoset
fn banana_total(monkeys: Int, bananas_each: Int) -> Int = monkeys * bananas_each

puts(banana_total(3, 4))  # 12
```

Both compile to the exact same binary. You choose the style.

---

## 🍌 What does Marmoset look like?

### 🔧 Functions & closures

First-class functions with lexical closures:

```marmoset
fn add_bananas(extra) = (pile) -> pile + extra

let breakfast = add_bananas(3)
puts(breakfast(5))  # 8
puts(breakfast(1))  # 4
```

Generics with trait constraints:

```marmoset
fn compare_snacks[a: Eq & Show](left: a, right: a) -> Str = {
  if (left == right) {
    "same snack: " + left.show()
  } else {
    "different snacks"
  }
}

puts(compare_snacks("banana", "banana"))  # same snack: banana
puts(compare_snacks(3, 5))                # different snacks
```

### 📦 Records

Structural typing with type aliases and spread updates:

```marmoset
type Monkey = { name: Str, bananas: Int }

let koko: Monkey = { name: "koko", bananas: 4 }
let hungrier_koko = { ...koko, bananas: 6 }

puts(hungrier_koko.name)     # koko
puts(hungrier_koko.bananas)  # 6
```

### 🎯 Enums & pattern matching

Algebraic data types with exhaustive pattern matching:

```marmoset
enum Snack = { Banana(Int), Coconut(Int) }

fn calories(snack: Snack) -> Int = match snack {
  case Snack.Banana(count): count * 10
  case Snack.Coconut(count): count * 25
}

puts(calories(Snack.Banana(3)))   # 30
puts(calories(Snack.Coconut(2)))  # 50
```

Generic enums with methods — model your domain:

```marmoset
enum Vine[a] = { Holding(a), Dropped(a), Empty }

impl Vine[a] = {
  fn occupied(self: Vine[a]) -> Bool = match self {
    case Vine.Holding(_): true
    case Vine.Dropped(_): false
    case Vine.Empty: false
  }
}

let vine = Vine.Holding("banana")
puts(vine.occupied())  # true
```

### 🤝 Traits

**Field traits** — satisfied structurally, just by having the right shape:

```marmoset
trait Named = { name: Str }

fn greet[a: Named](x: a) -> Str = "hello, " + x.name

# Any record with a `name: Str` field works
let monkey = { name: "milo", bananas: 3 }
let parrot = { name: "kiwi", words: 10 }

puts(greet(monkey))  # hello, milo
puts(greet(parrot))  # hello, kiwi
```

**Method traits** — require explicit impls:

```marmoset
enum Mood = { Calm, Hungry, Excited }

trait Show[a] = {
  fn show(x: a) -> Str
}

impl Show[Mood] = {
  fn show(mood: Mood) -> Str = match mood {
    case Mood.Calm: "calm"
    case Mood.Hungry: "hungry"
    case Mood.Excited: "excited"
  }
}

puts(Mood.Hungry.show())  # hungry
```

**Supertraits** compose — `Ord` implies `Eq`:

```marmoset
trait Eq[a] = { fn eq(x: a, y: a) -> Bool }

trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Int }
```

### 🛠️ Inherent methods

Attach methods directly to any type — no trait ceremony:

```marmoset
type BananaPile = { bananas: Int }

impl BananaPile = {
  fn add(pile: BananaPile, amount: Int) -> BananaPile = { bananas: pile.bananas + amount }
  fn total(pile: BananaPile) -> Int = pile.bananas
}

let pile = BananaPile.add({ bananas: 2 }, 3)
puts(pile.total())  # 5
```

Method-level generics — type args are inferred from usage, but you can be explicit when you want:

```marmoset
type BananaCrate = { bananas: Int }

impl BananaCrate = {
  fn label[a](crate: BananaCrate, f: (Int) -> a) -> a = f(crate.bananas)
}

let crate: BananaCrate = { bananas: 42 }

# Type argument inferred from the callback's return type
let summary = crate.label((n: Int) -> "bananas: " + n.show())

# Or specify it explicitly
let summary2 = crate.label[Str]((n: Int) -> "crate has " + n.show())

puts(summary)   # bananas: 42
puts(summary2)  # crate has 42
```

### 🔀 Union types

Type-safe unions with compile-time narrowing:

```marmoset
fn banana_count(input: Int | Str) -> Int = {
  if (input is Int) {
    input
  } else {
    0
  }
}

puts(banana_count(7))         # 7
puts(banana_count("unknown")) # 0
```

### ✨ Effect tracking

Pure functions (`->`) can't call effectful ones (`=>`). The compiler enforces this:

```marmoset
# This function does I/O — must be marked effectful
fn shout(msg: Str) => Str = {
  puts(msg)
  msg
}

# Pure function — just computes a value
fn banner(name: Str) -> Str = "monkey " + name

shout(banner("milo"))  # monkey milo

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
./marmoset run examples/fibonacci.mr
```

---

## 🖥️ Usage

```sh
# Run directly (compile + execute)
./marmoset run examples/fibonacci.mr

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
