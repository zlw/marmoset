# Stdlib Plan (Post-FFI)

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Planning (not started)
- Update trigger: Any stdlib, FFI, or prelude change
- Prerequisites: Module system, prelude (`docs/plans/prelude.md`), FFI (`docs/plans/ffi.md`)

## Context

Marmoset compiles to Go but isn't Go's sibling — it's an FP language that happens to use Go's runtime. The stdlib wraps Go's standard library with idiomatic Marmoset APIs: methods on types, immutable by default, Ruby-inspired naming, `?` suffix for boolean methods.

**Modules -> Prelude -> FFI -> Stdlib (this plan)**

---

## Locked Decisions

1. **Methods on types.** Collections and strings use method syntax (`xs.map(f)`, `"hello".upcase()`), not free functions.
2. **Immutable wrappers.** Hash `put`/`delete`/`merge` return new values. Go's mutable internals are hidden.
3. **All IO is effectful.** Every IO operation uses `=>`. Failable IO returns `Result[T, Str]`.
4. **Ruby-inspired naming.** `select`/`reject` not `filter`/`filter_not`. `upcase`/`downcase` not `to_upper`/`to_lower`. `include?` not `contains`. `strip` not `trim`.
5. **`?` suffix for boolean methods.** `empty?()`, `include?("x")`, `any?(fn)`, etc.
6. **Separate modules for console vs file IO.** `console` for terminal, `file` for filesystem.
7. **Concurrency model TBD.** Some abstraction over goroutines/channels — not raw channels. Designed later.

---

## Language Prereq: `?` in Method Names

Before stdlib work begins, the language needs `?` as a valid trailing character in identifiers.

**Changes:**
- **Lexer** (`lib/frontend/syntax/lexer.ml`): Allow `?` at end of identifier tokens (e.g., `empty?`, `include?`)
- **Emitter** (`lib/backend/go/emitter.ml`): Mangle `?` to valid Go identifier suffix (e.g., `empty?` -> `empty_q` or `is_empty`)

Small change, big readability win. This is a prereq for the stdlib, not part of it.

---

## Module: `console`

Terminal IO. All effectful.

```marmoset
# std/console.mr

export stdout, stdin, errout

# Print to stdout
# console.stdout("hello")
fn stdout(value: Str) => Unit

# Read line from stdin
# console.stdin()
fn stdin() => Str

# Print to stderr
# console.errout("oh no")
fn errout(value: Str) => Unit
```

Wraps Go's `fmt.Println` (stdout), `bufio.Scanner` (stdin), `fmt.Fprintln(os.Stderr, ...)` (errout).

---

## Module: `file`

Filesystem operations. All effectful, all return `Result`.

```marmoset
# std/file.mr

export read, write, append

# Read entire file contents
# file.read("config.txt")
fn read(path: Str) => Result[Str, Str]

# Write string to file (creates or overwrites)
# file.write("out.txt", content)
fn write(path: Str, content: Str) => Result[Unit, Str]

# Append string to file
# file.append("log.txt", line)
fn append(path: Str, content: Str) => Result[Unit, Str]
```

Wraps Go's `os.ReadFile`, `os.WriteFile`, `os.OpenFile` with append flag.

---

## String Methods

Methods on `Str`. All pure unless noted.

```marmoset
"hello".length()                 # -> Int
"hello".upcase()                 # -> Str
"hello".downcase()               # -> Str
"  hello  ".strip()              # -> Str
"  hello  ".lstrip()             # -> Str
"  hello  ".rstrip()             # -> Str
"hello world".split(" ")         # -> List[Str]
"hello".include?("ell")          # -> Bool
"hello".starts_with?("he")       # -> Bool
"hello".ends_with?("lo")         # -> Bool
"hello".empty?()                 # -> Bool
"hello".replace("l", "r")        # -> Str
"hello".slice(1, 3)              # -> Str
"hello".chars()                  # -> List[Str]
["a", "b", "c"].join(", ")       # -> Str (method on List[Str])
```

Wraps Go's `strings` package (`strings.ToUpper`, `strings.TrimSpace`, `strings.Split`, etc.).

---

## List Methods

Methods on `List[a]`. Pure unless noted.

```marmoset
[1, 2, 3].map((x) -> x + 1)                 # -> List[Int]
[1, 2, 3].select((x) -> x > 1)              # -> List[Int]
[1, 2, 3].reject((x) -> x > 1)              # -> List[Int]
[1, 2, 3].each((x) => console.stdout(x))    # => Unit (effectful)
[1, 2, 3].reduce(0, (acc, x) -> acc + x)    # -> Int
[1, 2, 3].flat_map((x) -> [x, x])           # -> List[Int]
[1, 2, 3].find((x) -> x > 1)                # -> Option[Int]
[1, 2, 3].any?((x) -> x > 2)                # -> Bool
[1, 2, 3].all?((x) -> x > 0)                # -> Bool
[1, 2, 3].none?((x) -> x > 5)               # -> Bool
[1, 2, 3].empty?()                           # -> Bool
[1, 2, 3].count()                            # -> Int
[1, 2, 3].reverse()                          # -> List[Int]
[1, 2, 3].sort()                             # -> List[Int] (requires Ord)
[1, 2, 3].take(2)                            # -> List[Int]
[1, 2, 3].drop(1)                            # -> List[Int]
[1, 2, 3].zip([4, 5, 6])                     # -> List[(Int, Int)]
[1, 2, 3].include?(2)                        # -> Bool
[1, 2, 3].uniq()                             # -> List[Int]
```

Implemented as generic inherent methods on `List[a]`. Go slices underneath; `map`/`select`/etc. allocate new slices.

---

## Hash Methods

Methods on `Map[k, v]`. Pure unless noted. Immutable; mutating ops return new maps.

```marmoset
h.keys()                                     # -> List[k]
h.values()                                   # -> List[v]
h.get(key)                                   # -> Option[v]
h.put(key, val)                              # -> Map[k, v] (new map)
h.delete(key)                                # -> Map[k, v] (new map)
h.has_key?(key)                              # -> Bool
h.empty?()                                   # -> Bool
h.count()                                    # -> Int
h.merge(other)                               # -> Map[k, v]
h.map((k, v) -> ...)                        # -> Map[k2, v2]
h.select((k, v) -> ...)                     # -> Map[k, v]
h.each((k, v) => ...)                       # => Unit (effectful)
```

Go maps underneath. `put`/`delete`/`merge` copy the map before mutating — immutable semantics over mutable Go internals.

---

## Implementation Phases

### Phase L0: `?` Suffix (Language Prereq)

**Goal:** `?` is valid in identifiers.

**Changes:**
- Lexer: allow trailing `?` in identifiers
- Emitter: mangle `?` to valid Go

**Gate:** `make unit && make integration` green.

---

### Phase L1: Console + File IO

**Goal:** Basic IO works via FFI wrapper modules.

**Modules:**
- `std/console.mr` — wraps `fmt`, `bufio`, `os` for stdout/stdin/errout
- `std/file.mr` — wraps `os` for read/write/append

**Tests:**
- `console.stdout("hello")` prints to stdout
- `console.stdin()` reads a line
- `console.errout("err")` prints to stderr
- `file.write("test.txt", "hi")` then `file.read("test.txt")` returns `Result.Success("hi")`
- `file.read("nonexistent")` returns `Result.Failure(...)`

**Gate:** `make unit && make integration` green.

---

### Phase L2: String Methods

**Goal:** Methods on `Str` available.

**Implementation:** Generic inherent impls on `Str`. Each method wraps a Go `strings` package function via FFI or direct emitter support.

**Tests:**
- All methods listed in String Methods section
- Edge cases: empty strings, unicode, no-match cases

**Gate:** `make unit && make integration` green.

---

### Phase L3: List Methods

**Goal:** Methods on `List[a]` available.

**Implementation:** Generic inherent impls on `List[a]`. Each method is implemented as a Go generic function operating on slices.

**Tests:**
- All methods listed in List Methods section
- Edge cases: empty lists, single element, type polymorphism

**Gate:** `make unit && make integration` green.

---

### Phase L4: Hash Methods

**Goal:** Methods on `Map[k, v]` available.

**Implementation:** Generic inherent impls on `Map[k, v]`. Immutable semantics; copy-on-write for mutating ops.

**Tests:**
- All methods listed in Hash Methods section
- Edge cases: empty hash, missing keys, merge conflicts
- Verify immutability: original hash unchanged after `put`/`delete`

**Gate:** `make unit && make integration` green.

---

### Phase L5: Concurrency (TBD)

**Goal:** Some abstraction over Go's goroutines/channels.

Design deferred. Not raw channels — something higher-level that fits Marmoset's FP style. Revisit after L1-L4 are complete and we have real-world usage to inform the design.

---

### Phase L6: Regroup

**Goal:** Assess what's missing for a real application.

Possible additions:
- HTTP server/client (`std/http`)
- JSON encoding/decoding (`std/json`)
- Time/date (`std/time`)
- Math utilities (`std/math`)
- Regular expressions (`std/regex`)
- Environment variables (`std/env`)

Prioritize based on what's needed to build a simple HTTP server as a proof-of-concept.

---

## Risks

1. **Performance of immutable wrappers.** Copy-on-write for hash `put`/`delete` has O(n) cost. Acceptable for v1 — optimize with persistent data structures later if needed.
2. **String method explosion.** Ruby has ~150 string methods. Start with the core set above, add more as needed.
3. **Generic inherent impls prereq.** List/hash methods need generic inherent impls (being implemented by Codex). String methods need inherent impls on primitive `string` type.
4. **`?` identifier change.** Touches lexer and emitter — small blast radius but needs care to not break existing identifiers.
5. **FFI maturity.** All IO modules depend on FFI working reliably. Phase L1 is the real stress test.

---

## Critical Files

| File | Role |
|------|------|
| `lib/frontend/syntax/lexer.ml` | `?` suffix support |
| `lib/backend/go/emitter.ml` | `?` mangling to valid Go |
| **New:** `std/console.mr` | Terminal IO wrapper |
| **New:** `std/file.mr` | Filesystem IO wrapper |
| String/list/hash inherent impls | Methods on types (location TBD — prelude.mr or separate std modules) |
