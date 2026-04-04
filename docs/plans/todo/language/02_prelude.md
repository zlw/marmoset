# Basic Stdlib + Prelude Plan

## Maintenance

- Last verified: 2026-04-04
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
6. **`Rem` is core prelude surface.** The `%` operator remains driven by the `Rem` trait, so `trait Rem[a]` ships in `std/prelude.mr` alongside `Num` and `Neg`.
7. **One compilation pipeline.** Headerless single-file programs do not keep a separate prelude/builtin shortcut. All entry files go through the same project discovery + compiler orchestration path, with `std/prelude.mr` auto-loading when present and builtin fallback only when the file is absent.
8. **Split builtin bootstrap responsibilities explicitly.** Replace the current monolithic `Builtins.prelude_env()` behavior with separate helpers for builtin value bindings, builtin prelude fallback registration, and builtin primitive impl registration so compiler and tests can control them independently.

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
| `trait Rem[a]` | `builtins.ml` | `%` depends on it today |
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
export Eq, Show, Debug, Ord, Hash, Num, Rem, Neg

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

trait Rem[a] = {
  fn rem(x: a, y: a) -> a
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
- Every entry file, including headerless single-file programs, goes through project discovery rooted at `source_root` or the entry directory
- If `std/prelude.mr` exists, it is loaded through that same project/compiler machinery before user code
- If it doesn't exist, the compiler uses explicit builtin fallback registration for core sums/traits instead of a special standalone path

**Auto-import mechanism:**
1. Compiler discovers the project and checks whether `std/prelude.mr` exists under the source root
2. If it exists, compiler compiles `std/prelude.mr` first via the normal module pipeline, then extracts its module signature
3. If it does not exist, compiler runs builtin fallback registration for `Ordering`, `Option`, `Result`, `Eq`, `Show`, `Debug`, `Ord`, `Hash`, `Num`, `Rem`, and `Neg`
4. Compiler seeds builtin value bindings (`puts`, `len`, etc.) separately from core prelude registration
5. Compiler registers builtin primitive impls after the relevant traits are available
6. Every user module is then compiled with the same combined environment/signature seeding path regardless of whether the entry file has module headers

**Changes:**
- `lib/frontend/compiler.ml` (module orchestrator): detect and compile prelude first, and remove the legacy standalone fast path so all entry files use the same orchestration
- `lib/frontend/discovery.ml`: ensure headerless single-file entrypoints still produce a normal project rooted at the entry directory or explicit `source_root`
- Module signature extraction: reuse from module system
- Inject prelude signature into each module's compilation context
- `lib/frontend/typecheck/builtins.ml`: split `prelude_env()` into explicit responsibilities:
  - builtin value environment for `puts`, `len`, `first`, `last`, `rest`, `push`
  - builtin fallback registration for core sums/traits when `std/prelude.mr` is missing
  - builtin primitive impl registration
- `lib/frontend/typecheck/checker.ml`, `lib/frontend/typecheck/annotation.ml`, and `lib/backend/go/emitter.ml`: update helper/test setup code that currently assumes one builtin bootstrap call registers everything

**Ordering matters:**
```
1. Discover project rooted at the entry file / explicit source_root
2. If present, parse + typecheck `std/prelude.mr` → named types (`Ordering`, `Option`, `Result`) and traits (`Eq`, `Show`, `Debug`, `Ord`, `Hash`, `Num`, `Rem`, `Neg`) registered
3. Otherwise run builtin fallback registration for those same sums/traits
4. Register builtin primitive impls → these reference traits already registered from step 2 or 3
5. Seed builtin function types (`puts`, `len`, etc.) into the value env
6. Compile every user module through the same combined path
```

**Backwards compat / surface policy:** Existing test files that define `enum Option[a] = { Some(a), None }` still work. `enum` remains accepted surface sugar for named sums, but the canonical prelude source should use constructor-bearing `type`.

**Write `std/prelude.mr`** with the content shown above.

**Tests:**
- `Option.Some(42)` works without a user-defined prelude sum
- `Result.Success("ok")` works without a user-defined prelude sum
- Match on option/result works
- All 8 traits available without user trait definition
- Operators still work (`42 == 42`, `1 + 2`, `10 % 3`, `Show.show(x)`)
- Headerless single-file entrypoints use the same prelude-aware compilation path as module-based entrypoints
- Existing tests with local `enum Option[a] = { Some(a), None }` still pass until the fixture migration prefers canonical `type` examples
- Missing `std/prelude.mr` falls back to builtin core-registration behavior through the same compiler pipeline
- Unit/helper setup that previously called `Builtins.prelude_env()` or `Enum_registry.init_builtins()` directly is updated to use the split bootstrap helpers

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
2. **Single-pipeline cutover** — Removing the legacy standalone shortcut touches build/check/analyze helpers and their tests. Keep this change in the same phase so the compiler no longer has divergent prelude semantics.
3. **Name collision** — User defines `trait eq` in their module. Prelude's `eq` is already registered; user's definition would need to shadow or error. Follow same rules as module system for name resolution.
4. **Circular dependency** — Prelude can't import anything. It's the root of the dependency graph. Fine since it only uses primitive types built into the compiler.

---

## Critical Files

| File | Role |
|------|------|
| **New:** `std/prelude.mr` | Core named sums + traits |
| **New:** `std/option.mr` | Option helper functions |
| **New:** `std/result.mr` | Result helper functions |
| `lib/frontend/compiler.ml` | Prelude auto-import orchestration (module system component) |
| `lib/frontend/discovery.ml` | Source-root project discovery for all entry files |
| `lib/frontend/typecheck/builtins.ml` | Remove enum/trait init; keep impl init + builtin functions |
| `lib/frontend/typecheck/checker.ml` | Update default env/bootstrap helpers |
| `lib/frontend/typecheck/annotation.ml` | Remove direct builtin enum bootstrap assumptions in tests/helpers |
| `lib/backend/go/emitter.ml` | Update direct single-file codegen/test helpers to use split bootstrap |
