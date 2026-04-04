# Basic Stdlib + Prelude Plan

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Planning (not started)
- Update trigger: Any prelude/stdlib, builtin, or module system change
- Prerequisites: Module system (docs/plans/done/language/06_module-system.md) must be implemented first

## Context

Users must redefine `type Option[a] = { Some(a), None }` and `type Result[a, e] = { Success(a), Failure(e) }` in every file. There is no prelude. This plan creates `std/prelude.mr` with core named types and traits, auto-imported into every module. `enum` remains accepted compatibility sugar, but the prelude and the surrounding plan should use the canonical constructor-bearing `type` surface.

**Modules → Basic stdlib + prelude → FFI → Full stdlib via FFI**

---

## Locked Decisions

1. **Real .mr file.** The prelude is `std/prelude.mr`, written in Marmoset. The compiler auto-imports it.
2. **Auto-import.** Prelude exports are available in every module without explicit `import`.
3. **Primitive impls stay in OCaml.** `builtins.ml` continues to register builtin primitive impls (`Eq[Int]`, `Show[Str]`, etc.) with `~builtin:true`. The emitter's hardcoded Go strings are unchanged. No stub bodies, no migration. Post-FFI these could move to `std/prelude.mr` using `extern` blocks.
4. **Builtin functions stay in OCaml.** `puts`, `len`, `first`, `last`, `rest`, `push` keep their registrations in `builtins.ml` and their Go implementations in `runtime.go`. They move to stdlib modules after FFI.
5. **Option/Result helpers are module functions.** `Option` / `Result` utility APIs (`unwrap_or`, `map`, `bind`, `map_fail`, etc.) live in explicit modules such as `std.option` and `std.result`, not as inherent methods. This keeps the prelude small and fits the no-UFCS, pipe-friendly direction.

---

## What Moves to `std/prelude.mr`

| Item | Currently in | Why move |
|------|-------------|----------|
| `Ordering` named sum (canonical `type`; `enum` still accepted as sugar) | `builtins.ml` | Users shouldn't need to define this |
| `Option[a]` named sum (canonical `type`; `enum` still accepted as sugar) | `enum_registry.ml` | Users redefine in every file today |
| `Result[a, e]` named sum (canonical `type`; `enum` still accepted as sugar) | `enum_registry.ml` | Users redefine in every file today |
| `trait Eq[a]` | `builtins.ml` | Completeness — prelude is the source of truth |
| `trait Show[a]` | `builtins.ml` | Same |
| `trait Debug[a]` | `builtins.ml` | Same |
| `trait Ord[a]: Eq` | `builtins.ml` | Same |
| `trait Hash[a]` | `builtins.ml` | Same |
| `trait Num[a]` | `builtins.ml` | Same |
| `trait Neg[a]` | `builtins.ml` | Same |

## What Stays in OCaml (for now)

| Item | Where | What actually happens |
|------|-------|----------------------|
| Primitive types (`int`, `string`, etc.) | Type system (`types.ml`) | Built into `mono_type` variants |
| Builtin functions (`puts`, `len`, etc.) | `builtins.ml` (types) + `emitter.ml` (Go code) | Types registered in typechecker. Go implementations emitted as `runtime.go` — regular Go functions with generics. |
| ~25 primitive trait impls | `builtins.ml` (types) + `emitter.ml` (Go code) | Types registered with `~builtin:true`. Go implementations emitted as hardcoded Go strings in `main.go` (e.g., `func eq_eq_int64(x, y int64) bool { return x == y }`). |

None of this is "magic" — it's all emitted as normal Go source. The Go code just happens to be hardcoded as OCaml strings rather than compiled from Marmoset. Post-FFI, even these could move to prelude.mr using `extern` blocks.

---

## Prelude File

### `std/prelude.mr`

```marmoset
export Ordering, Option, Result
export Eq, Show, Debug, Ord, Hash, Num, Neg

# --- Core named sums ---

type Ordering = { Less, Equal, Greater }
type Option[a] = { Some(a), None }
type Result[a, e] = { Success(a), Failure(e) }

# --- Core traits ---

trait Eq[a] = {
  fn eq(x: a, y: a) -> Bool
}

trait Show[a] = {
  fn show(x: a) -> Str
}

trait Debug[a] = {
  fn debug(x: a) -> Str
}

trait Ord[a]: Eq = {
  fn compare(x: a, y: a) -> Ordering
}

trait Hash[a] = {
  fn hash(x: a) -> Int
}

trait Num[a] = {
  fn add(x: a, y: a) -> a
  fn sub(x: a, y: a) -> a
  fn mul(x: a, y: a) -> a
  fn div(x: a, y: a) -> a
}

trait Neg[a] = {
  fn neg(x: a) -> a
}
```

Core declarations stay small. Option/result helper APIs are added in Phase S2 as ordinary module functions rather than inherent methods.

---

## Implementation Phases

### Phase S0: Prelude Infrastructure + Content

**Goal:** Compiler auto-loads `std/prelude.mr`, making option/result named types and traits available in all modules.

**Stdlib path resolution:**
- Compiler looks for `std/prelude.mr` relative to the source root
- If it exists, it's compiled first (it has no imports)
- If it doesn't exist, compiler falls back to current `Builtins.prelude_env()` behavior

**Auto-import mechanism:**
1. Compiler compiles `std/prelude.mr` → named types and traits are registered in registries
2. Extracts module signature (exported values, named types, transparent types/shapes if any, traits)
3. For every other module: injects prelude signature into initial type_env
4. `builtins.ml` then registers primitive impls (references traits now in registry from prelude)
5. User code compiled with prelude env + builtin impls + builtin functions

**Changes:**
- `lib/frontend/compiler.ml` (module orchestrator): detect and compile prelude first
- Module signature extraction: reuse from module system
- Inject prelude env into each module's compilation context
- `builtins.ml`: remove `init_builtin_enums()` and `init_builtin_traits()` — prelude named types/traits come from `std/prelude.mr`. Keep `init_builtin_impls()` and `builtin_types` (functions).

**Ordering matters:**
```
1. Parse + typecheck `std/prelude.mr` → named types (`Ordering`, `Option`, `Result`) and traits (`Eq`, `Show`, ...) registered
2. builtins.ml registers primitive impls → these reference traits already registered from step 1
3. builtins.ml registers builtin function types (puts, len, etc.) in type_env
4. User module compiled with combined env
```

**Backwards compat / surface policy:** Existing test files that define `enum Option[a] = { Some(a), None }` still work. `enum` remains accepted surface sugar for named sums, but the canonical prelude source should use constructor-bearing `type`.

**Write `std/prelude.mr`** with the content shown above.

**Tests:**
- `Option.Some(42)` works without a user-defined prelude sum
- `Result.Success("ok")` works without a user-defined prelude sum
- Match on option/result works
- All 7 traits available without user trait definition
- Operators still work (`42 == 42`, `1 + 2`, `Show.show(x)`)
- Existing tests with local `enum Option[a] = { Some(a), None }` still pass until the fixture migration prefers canonical `type` examples
- Missing `std/prelude.mr` falls back to builtin behavior

**Gate:** `make unit && make integration` green.

---

### Phase S1: Test Suite and Docs

**Goal:** Dedicated test suite, documentation.

**Changes:**
- Create `test/integration/09_prelude.sh` — tests for prelude types, traits, and that builtins still work
- Update `docs/ARCHITECTURE.md` — document prelude mechanism
- Update `ROADMAP.md` with stdlib status

**Gate:** `make unit && make integration` green.

---

### Phase S2: Option/Result Helper Modules

**Prereqs:**
- Module system from [06_module-system.md](/Users/zlw/src/marmoset/marmoset/docs/plans/done/language/06_module-system.md)
- Prelude Phase S0 so `Option` and `Result` exist everywhere

**Goal:** Add `std/option.mr` and `std/result.mr` as ordinary module APIs.

Add:

```marmoset
# std/option.mr

export unwrap_or, map, bind, is_some, is_none

fn unwrap_or[a](self: Option[a], fallback: a) -> a = match self {
  case Option.Some(v): v
  case Option.None: fallback
}

fn map[a, b](self: Option[a], f: (a) -> b) -> Option[b] = match self {
  case Option.Some(v): Option.Some(f(v))
  case Option.None: Option.None
}

fn bind[a, b](self: Option[a], f: (a) -> Option[b]) -> Option[b] = match self {
  case Option.Some(v): f(v)
  case Option.None: Option.None
}
```

```marmoset
# std/result.mr

export unwrap_or, map, map_fail, bind, is_ok, is_err

fn unwrap_or[a, e](self: Result[a, e], fallback: a) -> a = match self {
  case Result.Success(v): v
  case Result.Failure(_): fallback
}

fn map[a, b, e](self: Result[a, e], f: (a) -> b) -> Result[b, e] = match self {
  case Result.Success(v): Result.Success(f(v))
  case Result.Failure(err): Result.Failure(err)
}

fn map_fail[a, e, f](self: Result[a, e], g: (e) -> f) -> Result[a, f] = match self {
  case Result.Success(v): Result.Success(v)
  case Result.Failure(err): Result.Failure(g(err))
}
```

These stay ordinary functions and work well with explicit qualification and pipes.

**Tests:**
- `option.unwrap_or(Option.Some(42), 0)` → 42
- `option.unwrap_or(Option.None, 7)` → 7
- `option.map(Option.Some(42), Show.show)` → `Option.Some("42")`
- `option.bind(Option.Some(42), (x) -> Option.Some(x + 1))` → `Option.Some(43)`
- `option.map(Option.None, (x: Int) -> x + 1)` → `Option.None`
- `result.map(Result.Success(42), Show.show)` → `Result.Success("42")`
- `result.map_fail(Result.Failure("err"), (e) -> e + "!")` → `Result.Failure("err!")`
- `result.bind(Result.Success(42), (x) -> Result.Success(x + 1))` → `Result.Success(43)`
- `result.unwrap_or(Result.Failure("err"), 0)` → 0

**Gate:** `make unit && make integration` green.

---

## Future Work (separate from this plan)

1. **Migrate primitive impls to .mr** — Could move builtin primitive impls (`Eq[Int]`, `Ord[Int]`, etc.) to `std/prelude.mr` using `extern` blocks post-FFI. Zero user benefit since they already work. Only do if there's a concrete reason.
2. **Stdlib modules** (post-FFI) — `std.list`, `std.string`, `std.math`, `std.io`, `std.fmt`.

---

## Risks

1. **Prelude compilation ordering** — Prelude must be compiled before user code AND before `init_builtin_impls()`. The impls reference traits that must already be registered. Straightforward to enforce in the compilation pipeline.
2. **Name collision** — User defines `trait eq` in their module. Prelude's `eq` is already registered; user's definition would need to shadow or error. Follow same rules as module system for name resolution.
3. **Circular dependency** — Prelude can't import anything. It's the root of the dependency graph. Fine since it only uses primitive types built into the compiler.

---

## Critical Files

| File | Role |
|------|------|
| **New:** `std/prelude.mr` | Core named sums + traits |
| **New:** `std/option.mr` | Option helper functions |
| **New:** `std/result.mr` | Result helper functions |
| `lib/frontend/compiler.ml` | Prelude auto-import orchestration (module system component) |
| `lib/frontend/typecheck/builtins.ml` | Remove enum/trait init; keep impl init + builtin functions |
