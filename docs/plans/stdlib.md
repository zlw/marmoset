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
3. **All IO is effectful.** Every IO operation uses `=>`. Failable IO returns `result[T, string]`.
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
fn stdout(value: string) => unit

# Read line from stdin
# console.stdin()
fn stdin() => string

# Print to stderr
# console.errout("oh no")
fn errout(value: string) => unit
```

Wraps Go's `fmt.Println` (stdout), `bufio.Scanner` (stdin), `fmt.Fprintln(os.Stderr, ...)` (errout).

---

## Module: `file`

Filesystem operations. All effectful, all return `result`.

```marmoset
# std/file.mr

export read, write, append

# Read entire file contents
# file.read("config.txt")
fn read(path: string) => result[string, string]

# Write string to file (creates or overwrites)
# file.write("out.txt", content)
fn write(path: string, content: string) => result[unit, string]

# Append string to file
# file.append("log.txt", line)
fn append(path: string, content: string) => result[unit, string]
```

Wraps Go's `os.ReadFile`, `os.WriteFile`, `os.OpenFile` with append flag.

---

## String Methods

Methods on `string` type. All pure unless noted.

```marmoset
"hello".length()                 # -> int
"hello".upcase()                 # -> string
"hello".downcase()               # -> string
"  hello  ".strip()              # -> string
"  hello  ".lstrip()             # -> string
"  hello  ".rstrip()             # -> string
"hello world".split(" ")         # -> list[string]
"hello".include?("ell")          # -> bool
"hello".starts_with?("he")       # -> bool
"hello".ends_with?("lo")         # -> bool
"hello".empty?()                 # -> bool
"hello".replace("l", "r")       # -> string
"hello".slice(1, 3)             # -> string
"hello".chars()                  # -> list[string]
["a", "b", "c"].join(", ")      # -> string (method on list[string])
```

Wraps Go's `strings` package (`strings.ToUpper`, `strings.TrimSpace`, `strings.Split`, etc.).

---

## List Methods

Methods on `list[a]` type. Pure unless noted.

```marmoset
[1, 2, 3].map(fn(x) { x + 1 })              # -> list[int]
[1, 2, 3].select(fn(x) { x > 1 })           # -> list[int]
[1, 2, 3].reject(fn(x) { x > 1 })           # -> list[int]
[1, 2, 3].each(fn(x) { console.stdout(x) }) # => unit (effectful)
[1, 2, 3].reduce(0, fn(acc, x) { acc + x }) # -> int
[1, 2, 3].flat_map(fn(x) { [x, x] })        # -> list[int]
[1, 2, 3].find(fn(x) { x > 1 })             # -> option[int]
[1, 2, 3].any?(fn(x) { x > 2 })             # -> bool
[1, 2, 3].all?(fn(x) { x > 0 })             # -> bool
[1, 2, 3].none?(fn(x) { x > 5 })            # -> bool
[1, 2, 3].empty?()                           # -> bool
[1, 2, 3].count()                            # -> int
[1, 2, 3].reverse()                          # -> list[int]
[1, 2, 3].sort()                             # -> list[int] (requires ord)
[1, 2, 3].take(2)                            # -> list[int]
[1, 2, 3].drop(1)                            # -> list[int]
[1, 2, 3].zip([4, 5, 6])                     # -> list[(int, int)]
[1, 2, 3].include?(2)                        # -> bool
[1, 2, 3].uniq()                             # -> list[int]
```

Implemented as generic inherent methods on `list[a]`. Go slices underneath — `map`/`select`/etc. allocate new slices.

---

## Hash Methods

Methods on `hash[k, v]` type. Pure unless noted. Immutable — mutating ops return new hashes.

```marmoset
h.keys()                                     # -> list[k]
h.values()                                   # -> list[v]
h.get(key)                                   # -> option[v]
h.put(key, val)                              # -> hash[k, v] (new hash)
h.delete(key)                                # -> hash[k, v] (new hash)
h.has_key?(key)                              # -> bool
h.empty?()                                   # -> bool
h.count()                                    # -> int
h.merge(other)                               # -> hash[k, v]
h.map(fn(k, v) { ... })                     # -> hash[k2, v2]
h.select(fn(k, v) { ... })                  # -> hash[k, v]
h.each(fn(k, v) { ... })                    # => unit (effectful)
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
- `file.write("test.txt", "hi")` then `file.read("test.txt")` returns `result.success("hi")`
- `file.read("nonexistent")` returns `result.failure(...)`

**Gate:** `make unit && make integration` green.

---

### Phase L2: String Methods

**Goal:** Methods on `string` type available.

**Implementation:** Generic inherent impls on `string`. Each method wraps a Go `strings` package function via FFI or direct emitter support.

**Tests:**
- All methods listed in String Methods section
- Edge cases: empty strings, unicode, no-match cases

**Gate:** `make unit && make integration` green.

---

### Phase L3: List Methods

**Goal:** Methods on `list[a]` type available.

**Implementation:** Generic inherent impls on `list[a]`. Each method implemented as Go generic function operating on slices.

**Tests:**
- All methods listed in List Methods section
- Edge cases: empty lists, single element, type polymorphism

**Gate:** `make unit && make integration` green.

---

### Phase L4: Hash Methods

**Goal:** Methods on `hash[k, v]` type available.

**Implementation:** Generic inherent impls on `hash[k, v]`. Immutable semantics — copy-on-write for mutating ops.

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
