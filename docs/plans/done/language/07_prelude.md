# Basic Stdlib + Prelude Plan

## Maintenance

- Last verified: 2026-04-04
- Implementation status: Complete
- Update trigger: Any prelude/stdlib, builtin, or module system change
- Prerequisites: Module system (docs/plans/done/language/06_module-system.md) must be implemented first

## Context

Users must redefine `type Option[a] = { Some(a), None }` and `type Result[a, e] = { Success(a), Failure(e) }` in every file. There is no prelude. This plan creates a toolchain-shipped stdlib with `std/prelude.mr`, `std/option.mr`, and `std/result.mr`, auto-injected into every module through the normal module pipeline. `enum` remains accepted compatibility sugar, but the stdlib sources and the surrounding plan use the canonical constructor-bearing `type` surface.

**Modules → Basic stdlib + prelude → FFI → Full stdlib via FFI**

---

## Locked Decisions

1. **Real .mr files shipped with the toolchain.** `std/prelude.mr`, `std/option.mr`, and `std/result.mr` are written in Marmoset and resolved from the toolchain stdlib root rather than copied into each project.
2. **Auto-import.** Prelude exports are available in every module without explicit `import`.
3. **Primitive impls stay in OCaml.** `builtins.ml` continues to register builtin primitive impls (`Eq[Int]`, `Show[Str]`, etc.) with `~builtin:true`. The emitter's hardcoded Go strings are unchanged. No stub bodies, no migration. Post-FFI these could move to `std/prelude.mr` using `extern` blocks.
4. **Builtin functions stay in OCaml.** `puts`, `len`, `first`, `last`, `rest`, `push` keep their registrations in `builtins.ml` and their Go implementations in `runtime.go`. They move to stdlib modules after FFI.
5. **Option/Result live in their own stdlib modules and expose inherent methods.** `Option` is defined in `std.option`, `Result` in `std.result`, and helper APIs (`unwrap_or`, `map`, `bind`, `value_or`, `or`, etc.) are implemented as inherent methods on those nominal types.
6. **`Rem` is core prelude surface.** The `%` operator remains driven by the `Rem` trait, so `trait Rem[a]` ships in `std/prelude.mr` alongside `Num` and `Neg`.
7. **One compilation pipeline, no compiler fallback.** Headerless single-file programs do not keep a separate prelude/builtin shortcut. All file-backed entry files go through the same project discovery + compiler orchestration path, and missing required toolchain stdlib files are a hard error.
8. **Split builtin bootstrap responsibilities explicitly.** Replace the current monolithic `Builtins.prelude_env()` behavior with separate helpers for builtin value bindings, standalone helper/test enum+trait seeding, and builtin primitive impl registration so compiler and tests can control them independently.

---

## What Moves to `std/prelude.mr`

| Item | Currently in | Why move |
|------|-------------|----------|
| `Ordering` named sum (canonical `type`; `enum` still accepted as sugar) | `builtins.ml` | Users shouldn't need to define this |
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
export Ordering
export Eq, Show, Debug, Ord, Hash, Num, Rem, Neg

# --- Core named sums ---

type Ordering = { Less, Equal, Greater }

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

Core declarations stay small. `Option` and `Result` live in `std.option` / `std.result`, and those modules are injected alongside `std.prelude` for user code.

---

## Implementation Phases

### Phase S0: Prelude Infrastructure + Content

**Goal:** Compiler auto-loads toolchain stdlib core modules, making `Ordering`, traits, `Option`, and `Result` available in all modules.

**Stdlib path resolution:**
- Compiler resolves a toolchain stdlib root from explicit `stdlib_root`, `MARMOSET_STDLIB_ROOT`, or installed-toolchain probing
- Every entry file, including headerless single-file programs, goes through project discovery rooted at `source_root` or the entry directory
- `std/prelude.mr`, `std/option.mr`, and `std/result.mr` are loaded through that same project/compiler machinery before user code
- Missing required toolchain stdlib modules fail loudly instead of falling back to compiler-owned prelude declarations

**Auto-import mechanism:**
1. Compiler discovers the project and resolves the toolchain stdlib root
2. Compiler compiles `std/prelude.mr`, `std/option.mr`, and `std/result.mr` first via the normal module pipeline, then extracts their module signatures
3. Compiler seeds builtin value bindings (`puts`, `len`, etc.) separately from stdlib signature seeding
4. Compiler injects `std.prelude`, `std.option`, and `std.result` into every user module's direct import set, along with direct bindings for core prelude names plus `Option` / `Result`
5. Compiler registers builtin primitive impls after the relevant traits are available
6. Every user module is then compiled with the same combined environment/signature seeding path regardless of whether the entry file has module headers

**Changes:**
- `lib/frontend/compiler.ml` (module orchestrator): resolve/compile the core toolchain stdlib first, carry the derive-expanded module programs forward, and remove the legacy standalone fast path so all file-backed entry files use the same orchestration
- `lib/frontend/discovery.ml`: ensure headerless single-file entrypoints still produce a normal project rooted at the entry directory or explicit `source_root`
- `lib/frontend/import_resolver.ml`: inject `std.prelude`, `std.option`, and `std.result` into user modules without requiring module-system re-export support
- Module signature extraction: reuse from module system
- Inject prelude signature into each module's compilation context
- `lib/frontend/typecheck/builtins.ml`: split `prelude_env()` into explicit responsibilities:
  - builtin value environment for `puts`, `len`, `first`, `last`, `rest`, `push`
  - standalone helper/test enum+trait seeding
  - builtin primitive impl registration
- `lib/frontend/typecheck/checker.ml`, `lib/frontend/typecheck/annotation.ml`, and `lib/backend/go/emitter.ml`: update helper/test setup code that currently assumes one builtin bootstrap call registers everything

**Ordering matters:**
```
1. Discover project rooted at the entry file / explicit source_root
2. Parse + typecheck `std/prelude.mr`, `std/option.mr`, and `std/result.mr`
3. Register builtin primitive impls → these reference traits already registered from step 2
4. Seed builtin function types (`puts`, `len`, etc.) into the value env
5. Compile every user module through the same combined path with injected direct stdlib visibility
```

**Backwards compat / surface policy:** Existing test files that define `enum Option[a] = { Some(a), None }` still work. `enum` remains accepted surface sugar for named sums, but the canonical prelude source should use constructor-bearing `type`.

**Write `std/prelude.mr`** with the content shown above.

**Tests:**
- `Option.Some(42)` works without a user-defined prelude sum
- `Result.Success("ok")` works without a user-defined prelude sum
- Match on option/result works
- All 8 traits available without user trait definition
- Operators still work (`42 == 42`, `1 + 2`, `10 % 3`, `Show.show(x)`)
- `Option.map(...)`, `Option.unwrap_or(...)`, `Result.map(...)`, and `Result.bind(...)` resolve without explicit imports
- Headerless single-file entrypoints use the same prelude-aware compilation path as module-based entrypoints
- Existing tests with local `enum Option[a] = { Some(a), None }` still pass until the fixture migration prefers canonical `type` examples
- Missing required toolchain stdlib files fail with a clear `stdlib-not-found` diagnostic
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

**Goal:** Add `std/option.mr` and `std/result.mr` as ordinary stdlib modules defining the nominal types and their inherent helper APIs.

Add:

```marmoset
# std/option.mr

export Option

type Option[a] = { Some(a), None }

impl Option[a] = {
  fn unwrap_or(self: Option[a], fallback: a) -> a = match self {
    case Option.Some(v): v
    case Option.None: fallback
  }

  fn map[b](self: Option[a], f: (a) -> b) -> Option[b] = match self {
    case Option.Some(v): Option.Some(f(v))
    case Option.None: Option.None
  }
}
```

```marmoset
# std/result.mr

export Result

type Result[a, e] = { Success(a), Failure(e) }

impl Result[a, e] = {
  fn value_or(self: Result[a, e], fallback: a) -> a = match self {
    case Result.Success(v): v
    case Result.Failure(_): fallback
  }

  fn map[b](self: Result[a, e], f: (a) -> b) -> Result[b, e] = match self {
    case Result.Success(v): Result.Success(f(v))
    case Result.Failure(err): Result.Failure(err)
  }
}
```

These are ordinary stdlib modules, but their APIs surface as inherent methods on the nominal types and are callable via type qualification (`Option.map(...)`, `Result.bind(...)`).

**Tests:**
- `Option.unwrap_or(Option.Some(42), 0)` → 42
- `Option.unwrap_or(Option.None, 7)` → 7
- `Option.map(Option.Some(42), Show.show)` → `Option.Some("42")`
- `Option.bind(Option.Some(42), (x) -> Option.Some(x + 1))` → `Option.Some(43)`
- `Option.map(Option.None, (x: Int) -> x + 1)` → `Option.None`
- `Result.map(Result.Success(42), Show.show)` → `Result.Success("42")`
- `Result.or(Result.Failure("err"), (e) -> e + "!")` → `Result.Failure("err!")`
- `Result.bind(Result.Success(42), (x) -> Result.Success(x + 1))` → `Result.Success(43)`
- `Result.value_or(Result.Failure("err"), 0)` → 0

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
| **New:** `std/prelude.mr` | Core ordering sum + traits |
| **New:** `std/option.mr` | `Option` type + inherent helper methods |
| **New:** `std/result.mr` | `Result` type + inherent helper methods |
| `lib/frontend/compiler.ml` | Prelude auto-import orchestration (module system component) |
| `lib/frontend/discovery.ml` | Entry-root discovery plus toolchain stdlib resolution |
| `lib/frontend/import_resolver.ml` | Inject direct stdlib modules/bindings into user modules |
| `lib/frontend/typecheck/builtins.ml` | Keep builtin value env + primitive impl init; trim file-backed fallback responsibility |
| `lib/frontend/typecheck/checker.ml` | Update default env/bootstrap helpers |
| `lib/frontend/typecheck/annotation.ml` | Remove direct builtin enum bootstrap assumptions in tests/helpers |
| `lib/backend/go/emitter.ml` | Update direct single-file codegen/test helpers to use split bootstrap |

## Progress

- 2026-04-04 03:51 CEST: Implementation started. Reconstructed the work into three slices: S0 prelude bootstrap/pipeline, S1 integration+docs, and S2 std.option/std.result helpers. Preparing test-first coverage for S0 before changing compiler behavior.
- 2026-04-04 04:15 CEST: S0 bootstrap/pipeline is green under `make unit compiler`. Headerless file-backed entries now use the unified module pipeline, `std/prelude.mr` auto-discovery is wired into project discovery/rewrite, and missing-prelude fallback uses the split builtin bootstrap helpers.
- 2026-04-04 05:20 CEST: S1 integration+docs slice is green. Added the dedicated prelude integration script, wired it into the integration runner, and updated the architecture/roadmap docs to describe prelude auto-import and the new stdlib surface.
- 2026-04-04 05:35 CEST: S2 stdlib helper slice is green. Added `std/option.mr` and `std/result.mr`, updated compiler/typechecker/codegen plumbing for canonical prelude enums and builtin/user impl coexistence, and refreshed the affected Go snapshots/hardening coverage.
- 2026-04-04 14:38 CEST: Follow-up emitter trimming slice started. Adding red Go-inspection coverage for minimal programs so unused toolchain stdlib enums and primitive builtin trait helpers stop leaking into emitted `main.go`.
- 2026-04-04 05:57 CEST: Final verification is green under `make unit compiler`, targeted fixture checks for the six previously failing regressions, `dune build --root /Users/zlw/src/marmoset/worktrees/prelude _build/default/bin/main.exe`, and `make integration` (1585 fixture tests, 16 exact snapshots, 16 hardening tests). Prelude plan implementation is complete.
- 2026-04-04 06:56 CEST: Direction corrected after implementation review. Prelude/std lookup now targets a toolchain-owned stdlib root instead of project-local `std/`; file-backed compilation no longer uses builtin prelude fallback; `Option`/`Result` moved out of `std.prelude` into `std.option` / `std.result`; helper APIs are inherent methods resolved via direct stdlib injection rather than re-export semantics.
- 2026-04-04 15:27 CEST: Follow-up emitter trimming slice is green. Removed an over-broad enum-registration pass from `emit_inherent_method` so concrete inherent emission no longer pulls ambient `Option`/`Result` into unrelated Go output. Verification is green under `make unit compiler`, `make integration 10_codegen_snapshots.sh 11_pre_modules_hardening.sh`, and full `make integration` (1585 fixture tests, 16 exact snapshots, 16 hardening tests).
- 2026-04-04 17:52 CEST: Codex-only issue hunt across module system + prelude completed three rounds and produced three real regressions. Round 1 added namespace-type-position coverage and fixed dotted type parsing/resolution so `geo.Point` / `wrappers.Users` work in annotations and aliases. Round 2 hardened shadowing diagnostics so local `type Option` / `type Result` misses report source-facing names instead of mangled internals. Round 3 found that qualified impl headers like `impl geometry.Drawable[geometry.Point]` inferred `geometry.Point` as a bogus generic binder, which bypassed duplicate-impl coherence; lowering now treats dotted names as concrete, and qualified duplicate impls are rejected again. Verification is green under `make unit compiler`, `./test/integration.sh modules modules_edge modules_codegen modules_codegen_edge snapshots`, and `make integration 11_pre_modules_hardening.sh 12_prelude.sh`.
