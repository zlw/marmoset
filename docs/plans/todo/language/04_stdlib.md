# Stdlib Plan (Post-FFI)

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Planning (not started)
- Update trigger: Any stdlib, FFI, or prelude change
- Prerequisites: Module system, prelude (`docs/plans/done/language/07_prelude.md`), FFI (`docs/plans/todo/language/03_ffi.md`)

## Context

Marmoset compiles to Go but isn't Go's sibling — it's an FP language that happens to use Go's runtime. The stdlib should wrap Go's standard library with idiomatic Marmoset APIs: module functions, pipe-friendly data-first argument order, immutable by default, Ruby-inspired naming, and `?` suffix for boolean predicates.

**Modules -> Prelude -> FFI -> Stdlib (this plan)**

---

## Locked Decisions

1. **Module-function-first APIs.** Collections and strings use qualified functions (`list.map(xs, f)`, `str.upcase("hello")`) with pipe-friendly first-argument order. Dot syntax is not the stdlib's primary abstraction surface.
2. **Immutable wrappers.** Hash `put`/`delete`/`merge` return new values. Go's mutable internals are hidden.
3. **All IO is effectful.** Every IO operation uses `=>`. Failable IO returns `Result[T, Str]`.
4. **Ruby-inspired naming.** `select`/`reject` not `filter`/`filter_not`. `upcase`/`downcase` not `to_upper`/`to_lower`. `include?` not `contains`. `strip` not `trim`.
5. **`?` suffix for boolean predicates.** `empty?()`, `include?("x")`, `any?(fn)`, etc. remain good names even as module functions.
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

## String Functions

Functions in `std/str`. All pure unless noted.

```marmoset
str.length("hello")                 # -> Int
str.upcase("hello")                 # -> Str
str.downcase("hello")               # -> Str
str.strip("  hello  ")              # -> Str
str.lstrip("  hello  ")             # -> Str
str.rstrip("  hello  ")             # -> Str
str.split("hello world", " ")       # -> List[Str]
str.include?("hello", "ell")        # -> Bool
str.starts_with?("hello", "he")     # -> Bool
str.ends_with?("hello", "lo")       # -> Bool
str.empty?("hello")                 # -> Bool
str.replace("hello", "l", "r")      # -> Str
str.slice("hello", 1, 3)            # -> Str
str.chars("hello")                  # -> List[Str]
str.join(["a", "b", "c"], ", ")     # -> Str
```

Wraps Go's `strings` package (`strings.ToUpper`, `strings.TrimSpace`, `strings.Split`, etc.).

---

## List Functions

Functions in `std/list.mr`. Pure unless noted. Prefer explicit qualification or pipes.

```marmoset
[1, 2, 3] |> list.map((x) -> x + 1)                 # -> List[Int]
[1, 2, 3] |> list.select((x) -> x > 1)              # -> List[Int]
[1, 2, 3] |> list.reject((x) -> x > 1)              # -> List[Int]
[1, 2, 3] |> list.each((x) => console.stdout(x))    # => Unit (effectful)
[1, 2, 3] |> list.reduce(0, (acc, x) -> acc + x)    # -> Int
[1, 2, 3] |> list.flat_map((x) -> [x, x])           # -> List[Int]
[1, 2, 3] |> list.find((x) -> x > 1)                # -> Option[Int]
[1, 2, 3] |> list.any?((x) -> x > 2)                # -> Bool
[1, 2, 3] |> list.all?((x) -> x > 0)                # -> Bool
[1, 2, 3] |> list.none?((x) -> x > 5)               # -> Bool
[1, 2, 3] |> list.empty?()                          # -> Bool
[1, 2, 3] |> list.count()                           # -> Int
[1, 2, 3] |> list.reverse()                         # -> List[Int]
[1, 2, 3] |> list.sort()                            # -> List[Int] (requires Ord)
[1, 2, 3] |> list.take(2)                           # -> List[Int]
[1, 2, 3] |> list.drop(1)                           # -> List[Int]
[1, 2, 3] |> list.zip([4, 5, 6])                    # -> List[(Int, Int)]
[1, 2, 3] |> list.include?(2)                       # -> Bool
[1, 2, 3] |> list.uniq()                            # -> List[Int]
```

Implemented as generic module functions over `List[a]`. Go slices underneath; `map`/`select`/etc. allocate new slices.

---

## Hash Functions

Functions in `std/map.mr`. Pure unless noted. Immutable; mutating ops return new maps.

```marmoset
map.keys(h)                                  # -> List[k]
map.values(h)                                # -> List[v]
map.get(h, key)                              # -> Option[v]
map.put(h, key, val)                         # -> Map[k, v] (new map)
map.delete(h, key)                           # -> Map[k, v] (new map)
map.has_key?(h, key)                         # -> Bool
map.empty?(h)                                # -> Bool
map.count(h)                                 # -> Int
map.merge(h, other)                          # -> Map[k, v]
map.map(h, (k, v) -> ...)                    # -> Map[k2, v2]
map.select(h, (k, v) -> ...)                 # -> Map[k, v]
map.each(h, (k, v) => ...)                   # => Unit (effectful)
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

### Phase L2: String Module

**Goal:** `std/str.mr` available.

**Implementation:** Module functions in `std/str.mr`. Each function wraps a Go `strings` package function via FFI or direct emitter support.

**Tests:**
- All methods listed in String Methods section
- Edge cases: empty strings, unicode, no-match cases

**Gate:** `make unit && make integration` green.

---

### Phase L3: List Module

**Goal:** `std/list.mr` available.

**Implementation:** Generic module functions over `List[a]`. Each function is implemented as a Go generic function operating on slices.

**Tests:**
- All methods listed in List Methods section
- Edge cases: empty lists, single element, type polymorphism

**Gate:** `make unit && make integration` green.

---

### Phase L4: Hash Module

**Goal:** `std/map.mr` available.

**Implementation:** Generic module functions over `Map[k, v]`. Immutable semantics; copy-on-write for mutating ops.

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
3. **Generic helper surface.** List/hash modules need generic top-level/module functions with good inference and codegen. This is simpler than inherents, but still touches stdlib ergonomics and emitter support.
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
| **New:** `std/str.mr` | String helper functions |
| **New:** `std/list.mr` | List helper functions |
| **New:** `std/map.mr` | Map helper functions |
