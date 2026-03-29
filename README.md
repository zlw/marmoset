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
type Monkey = { name: Str, bananas: Int } derive Show

trait Alarm[a]: Show = {
  fn ring!(x: a) -> Str = x.show()
}

impl Alarm[Monkey] = {
  override fn ring!(x) = x.name + " spotted " + x.bananas.show() + " bananas"
}

puts(Monkey(name: "milo", bananas: 3).ring!())
```

```sh
$ marmoset run snack.mr
milo spotted 3 bananas
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

One-arg callbacks can use `_` shorthand when the callee provides the parameter type:

```marmoset
fn call_twice(n: Int, f: (Int) -> Int) -> Int = f(f(n))

puts(call_twice(3, _ + 1))             # 5
puts(call_twice(3, (n: Int) -> n * 2)) # 12
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

### 🧾 Transparent aliases

Transparent aliases name existing type expressions without creating a new nominal type:

```marmoset
alias Point = { x: Int, y: Int }

let start: Point = { x: 1, y: 2 }
fn shift_x(p: Point, dx: Int) -> Point = { ...p, x: p.x + dx }

puts(shift_x(start, 3).x)  # 4
puts(start.y)              # 2
```

### 📦 Records

Structural records support field access and spread updates:

```marmoset
let start = { x: 1, y: 2 }
let moved = { ...start, x: 10 }

puts(moved.x)  # 10
puts(moved.y)  # 2
```

Named product records support the same field access and record-pattern behavior, and they can be rebuilt explicitly with constructor spread:

```marmoset
type Waypoint = { x: Int, y: Int }

let start = Waypoint(x: 1, y: 2)
let moved = Waypoint(...start, x: 10)

puts(moved.x)  # 10
puts(moved.y)  # 2
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

### 🧱 Shapes

Shapes capture structural field constraints:

```marmoset
shape Named = { name: Str }

fn greet[a: Named](x: a) -> Str = "hello, " + x.name

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
  override fn ring!(x) = "jungle alarm " + x.show()
}

puts(3.ring!())  # jungle alarm 3
```

Shapes and traits compose through superconstraints:

```marmoset
shape Named = { name: Str }

trait Parade[a]: Show & Named = {
  fn banner(self: a) -> Str = self.name + " ready"
}
```

Built-in derives, user derives, and `Dyn[...]` trait objects work together too:

```marmoset
trait Drum[a] = {
  fn drum(self: a) -> Str = "boom"
}

type Relic = { animal: Str, leaves: Int } derive Show, Drum

fn announce(x: Dyn[Show]) -> Str = "found " + x.show()

let relic = Relic(animal: "jaguar", leaves: 3)
puts(relic.drum())        # boom
puts(announce(relic))     # found { animal: jaguar, leaves: 3 }
puts(announce("banana"))  # found banana
```

### 🛠️ Inherent methods

Attach methods directly to types that own behavior — no trait ceremony:

```marmoset
type BananaPile = { bananas: Int }

impl BananaPile = {
  fn add(pile: BananaPile, amount: Int) -> BananaPile = BananaPile(bananas: pile.bananas + amount)
  fn total(pile: BananaPile) -> Int = pile.bananas
}

let pile = BananaPile.add(BananaPile(bananas: 2), 3)
puts(pile.total())  # 5
```

This uses `type` rather than `alias` because `BananaPile` owns nominal behavior and explicit constructor syntax.

Methods can be generic, and shorthand trait constraints work inside them too:

```marmoset
type VineTag = { prefix: Str }

impl VineTag = {
  fn render[a](tag: VineTag, item: a, draw: (a) -> Str) -> Str = tag.prefix + draw(item)
  fn cheer(tag: VineTag, item: Show) -> Str = tag.prefix + item.show()
}

let tag = VineTag(prefix: "seen: ")

puts(tag.render(3, _.show()))  # seen: 3
puts(tag.cheer("banana"))      # seen: banana
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
