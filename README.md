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
type BananaCrate = { troop: Str, bananas: Int } derive Show

trait RangerReport[a]: Show = {
  fn render(x: a) -> Str = "report: #{x}"
}

impl RangerReport[BananaCrate] = {
  override fn render(x) = "#{x.troop} troop needs #{x.bananas} bananas"
}

impl BananaCrate = {
  fn restock(crate: BananaCrate, extra: Int) -> BananaCrate =
    { ...crate, bananas: crate.bananas + extra }
}

let crate: BananaCrate = { troop: "milo", bananas: 3 }
puts(crate |> BananaCrate.restock(2) |> RangerReport.render)
```

```sh
$ marmoset run snack.mr
milo troop needs 5 bananas
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

Strings support interpolation with `#{expr}`. Embedded values use `Show`, so the same syntax works for primitives and user types with a `Show` impl.

---

## 🍌 What does Marmoset look like?

### 🔧 Functions & closures

First-class functions with lexical closures:

```marmoset
fn make_restocker(extra) = (crate_total) -> crate_total + extra

let morning_delivery = make_restocker(3)
puts(morning_delivery(5))  # 8
puts(morning_delivery(1))  # 4
```

One-arg callbacks can use `_` shorthand when the callee provides the parameter type:

```marmoset
fn recount_twice(count: Int, revise: (Int) -> Int) -> Int = revise(revise(count))

puts(recount_twice(3, _ + 1))             # 5
puts(recount_twice(3, (n: Int) -> n * 2)) # 12
```

Generics with trait constraints:

```marmoset
fn compare_inventory[a: Eq & Show](left: a, right: a) -> Str = {
  if (left == right) {
    "same inventory: #{left}"
  } else {
    "different inventories"
  }
}

puts(compare_inventory("banana", "banana"))  # same inventory: banana
puts(compare_inventory(3, 5))                # different inventories
```

### 🧾 Transparent types

Transparent `type` declarations name existing exact type expressions without creating a new nominal type:

```marmoset
type Perch = { bananas: Int, vines: Int }

let start: Perch = { bananas: 1, vines: 2 }
fn gather_more(p: Perch, extra: Int) -> Perch = { ...p, bananas: p.bananas + extra }

puts(gather_more(start, 3).bananas)  # 4
puts(start.vines)                    # 2
```

### 📦 Records

Structural records support field access and spread updates:

```marmoset
let supply = { bananas: 1, coconuts: 2 }
let restocked = { ...supply, bananas: 10 }

puts(restocked.bananas)   # 10
puts(restocked.coconuts)  # 2
```

Named exact records use the same structural field access and spread updates:

```marmoset
type JungleStop = { bananas: Int, vines: Int }

let start: JungleStop = { bananas: 1, vines: 2 }
let moved = { ...start, bananas: 10 }

puts(moved.bananas)  # 10
puts(moved.vines)    # 2
```

### 🎯 Named sums & pattern matching

Algebraic data types with exhaustive pattern matching:

```marmoset
type Snack = { Banana(Int), Coconut(Int) }

fn calories(snack: Snack) -> Int = match snack {
  case Snack.Banana(count): count * 10
  case Snack.Coconut(count): count * 25
}

puts(calories(Snack.Banana(3)))   # 30
puts(calories(Snack.Coconut(2)))  # 50
```

Constructor-bearing `type` is the canonical sum surface. `enum` remains accepted as compatibility sugar.

Generic sums with methods — model your domain:

```marmoset
type Vine[a] = { Holding(a), Dropped(a), Empty }

impl Vine[a] = {
  fn occupied(vine: Vine[a]) -> Bool = match vine {
    case Vine.Holding(_): true
    case Vine.Dropped(_): false
    case Vine.Empty: false
  }
}

let vine = Vine.Holding("banana")
puts(Vine.occupied(vine))  # true
```

### 🧱 Shapes

Shapes capture structural field constraints:

```marmoset
shape Named = { name: Str }

fn greet[a: Named](x: a) -> Str = "hello, #{x.name}"

# Any record with a `name: Str` field works
let monkey = { name: "milo", bananas: 3 }
let parrot = { name: "kiwi", words: 10 }

puts(greet(monkey))  # hello, milo
puts(greet(parrot))  # hello, kiwi
```

### 🤝 Traits

Method traits require explicit impls:

```marmoset
trait Alarm[a] = {
  fn ring!(x: a) -> Str = "soft rustle"
}

impl Alarm[Int] = {
  override fn ring!(x) = "jungle alarm #{x}"
}

puts(Alarm.ring!(3))  # jungle alarm 3
```

Shapes and traits compose through superconstraints:

```marmoset
shape Named = { name: Str }

trait Parade[a]: Show & Named = {
  fn banner(self: a) -> Str = "#{self.name} ready"
}
```

Built-in derives, user derives, and `Dyn[...]` trait objects work together too:

```marmoset
trait Drum[a] = {
  fn drum(self: a) -> Str = "boom"
}

type Relic = { animal: Str, leaves: Int } derive Show, Drum

fn announce(x: Dyn[Show]) -> Str = "found #{x}"

let relic: Relic = { animal: "jaguar", leaves: 3 }
puts(Drum.drum(relic))    # boom
puts(announce(relic))     # found { animal: jaguar, leaves: 3 }
puts(announce("banana"))  # found banana
```

### 🛠️ Inherent methods

Register exact-type extension methods when a data shape has stable helpers:

```marmoset
type Stash = { bananas: Int }

impl Stash = {
  fn add(stash: Stash, amount: Int) -> Stash = { ...stash, bananas: stash.bananas + amount }
  fn total(stash: Stash) -> Int = stash.bananas
}

let stash: Stash = { bananas: 2 }
let grown = stash |> Stash.add(3)
puts(grown |> Stash.total |> Show.show)  # 5
```

This uses transparent `type` because exact types can own behavior by their underlying type. If you want an opaque quantity rather than a record-shaped value, use an explicit wrapper such as `type BananaPile = BananaPile(Int)`.

Methods can be generic, and shorthand trait constraints work inside them too:

```marmoset
type VineTag = { prefix: Str }

impl VineTag = {
  fn render[a](tag: VineTag, item: a, draw: (a) -> Str) -> Str = "#{tag.prefix}#{draw(item)}"
  fn cheer(tag: VineTag, item: Show) -> Str = "#{tag.prefix}#{item}"
}

let tag: VineTag = { prefix: "seen: " }

puts(VineTag.render(tag, 3, Show.show))  # seen: 3
puts(VineTag.cheer(tag, "banana"))       # seen: banana
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

### 🧩 Intersection types

One value can satisfy multiple structural views at once:

```marmoset
fn name_tag(r) = r.name

let scout: ({ name: Str, bananas: Int } & { name: Str }) = {
  name: "milo",
  bananas: 3,
}

puts(name_tag(scout))  # milo
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
fn banner(name: Str) -> Str = "monkey #{name}"

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
