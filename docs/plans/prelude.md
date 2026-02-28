# Basic Stdlib + Prelude Plan

## Maintenance

- Last verified: 2026-03-01
- Implementation status: Planning (not started)
- Update trigger: Any prelude/stdlib, builtin, or module system change
- Prerequisites: Module system (docs/plans/module-system.md) must be implemented first

## Context

Users must redefine `enum option[a] { some(a) none }` and `enum result[a, e] { success(a) failure(e) }` in every file. There is no prelude. This plan creates `std/prelude.mr` with core enums and traits, auto-imported into every module.

**Modules → Basic stdlib + prelude → FFI → Full stdlib via FFI**

---

## Locked Decisions

1. **Real .mr file.** The prelude is `std/prelude.mr`, written in Marmoset. The compiler auto-imports it.
2. **Auto-import.** Prelude exports are available in every module without explicit `import`.
3. **Primitive impls stay in OCaml.** `builtins.ml` continues to register `impl eq for int` etc. with `~builtin:true`. The emitter's hardcoded Go strings are unchanged. No stub bodies, no migration. Post-FFI these could move to prelude.mr using `extern` blocks.
4. **Builtin functions stay in OCaml.** `puts`, `len`, `first`, `last`, `rest`, `push` — types registered in `builtins.ml`, Go implementations emitted as `runtime.go`. They move to stdlib modules after FFI.
5. **Methods on types.** Option/result utility methods (`unwrap_or`, `map`, `bind`, `map_fail`, etc.) as inherent methods. Generic inherent impls (prereq) being implemented separately by Codex.

---

## What Moves to `std/prelude.mr`

| Item | Currently in | Why move |
|------|-------------|----------|
| `enum ordering` | `builtins.ml` | Users shouldn't need to define this |
| `enum option[a]` | `enum_registry.ml` | Users redefine in every file today |
| `enum result[a, e]` | `enum_registry.ml` | Users redefine in every file today |
| `trait eq[a]` | `builtins.ml` | Completeness — prelude is the source of truth |
| `trait show[a]` | `builtins.ml` | Same |
| `trait debug[a]` | `builtins.ml` | Same |
| `trait ord[a]: eq` | `builtins.ml` | Same |
| `trait hash[a]` | `builtins.ml` | Same |
| `trait num[a]` | `builtins.ml` | Same |
| `trait neg[a]` | `builtins.ml` | Same |

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
export ordering, option, result
export eq, show, debug, ord, hash, num, neg

# --- Core enums ---

enum ordering { less equal greater }
enum option[a] { some(a) none }
enum result[a, e] { success(a) failure(e) }

# --- Core traits ---

trait eq[a] {
  fn eq(x: a, y: a) -> bool
}

trait show[a] {
  fn show(x: a) -> string
}

trait debug[a] {
  fn debug(x: a) -> string
}

trait ord[a]: eq {
  fn compare(x: a, y: a) -> ordering
}

trait hash[a] {
  fn hash(x: a) -> int
}

trait num[a] {
  fn add(x: a, y: a) -> a
  fn sub(x: a, y: a) -> a
  fn mul(x: a, y: a) -> a
  fn div(x: a, y: a) -> a
}

trait neg[a] {
  fn neg(x: a) -> a
}
```

Core declarations ~30 lines. Inherent methods for option/result added in Phase S2 (requires method-level generics from `docs/plans/function-model.md`).

---

## Implementation Phases

### Phase S0: Prelude Infrastructure + Content

**Goal:** Compiler auto-loads `std/prelude.mr`, making option/result/traits available in all modules.

**Stdlib path resolution:**
- Compiler looks for `std/prelude.mr` relative to the source root
- If it exists, it's compiled first (it has no imports)
- If it doesn't exist, compiler falls back to current `Builtins.prelude_env()` behavior

**Auto-import mechanism:**
1. Compiler compiles `std/prelude.mr` → enums and traits are registered in registries
2. Extracts module signature (exported types, traits, enums)
3. For every other module: injects prelude signature into initial type_env
4. `builtins.ml` then registers primitive impls (references traits now in registry from prelude)
5. User code compiled with prelude env + builtin impls + builtin functions

**Changes:**
- `lib/frontend/compiler.ml` (module orchestrator): detect and compile prelude first
- Module signature extraction: reuse from module system
- Inject prelude env into each module's compilation context
- `builtins.ml`: remove `init_builtin_enums()` and `init_builtin_traits()` — enums/traits come from prelude.mr. Keep `init_builtin_impls()` and `builtin_types` (functions).

**Ordering matters:**
```
1. Parse + typecheck std/prelude.mr → enums (option, result, ordering) and traits (eq, show, ...) registered
2. builtins.ml registers primitive impls → these reference traits already registered from step 1
3. builtins.ml registers builtin function types (puts, len, etc.) in type_env
4. User module compiled with combined env
```

**Backwards compat:** Existing test files that define `enum option[a] { some(a) none }` still work — `Enum_registry.register` does `Hashtbl.replace`, silently overwrites with identical definition.

**Write `std/prelude.mr`** with the content shown above.

**Tests:**
- `option.some(42)` works without user enum definition
- `result.success("ok")` works without user enum definition
- Match on option/result works
- All 7 traits available without user trait definition
- Operators still work (`42 == 42`, `1 + 2`, `x.show()`)
- Existing tests with local `enum option[a] { some(a) none }` still pass
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

### Phase S2: Methods on Option/Result in Prelude

**Prereqs:**
- Generic inherent impls (being implemented separately by Codex).
- Method-level generics `fn name[b](...)` syntax (`docs/plans/function-model.md` Phase F1).

**Goal:** Add utility methods to prelude.mr.

Methods that introduce new type variables (like `map`, `bind`, `map_fail`) use explicit method-level generics with `fn name[b](...)` syntax — generics attach to the method name, consistent with `trait show[a]`, `enum option[a]`, etc. Methods that only use the impl-level type variables (like `unwrap_or`, `is_some`) don't need method-level generics.

Add to `std/prelude.mr`:
```marmoset
impl option[a] {
  fn unwrap_or(self: option[a], fallback: a) -> a {
    match self {
      option.some(v): v
      option.none: fallback
    }
  }

  fn map[b](self: option[a], f: fn(a) -> b) -> option[b] {
    match self {
      option.some(v): option.some(f(v))
      option.none: option.none
    }
  }

  fn bind[b](self: option[a], f: fn(a) -> option[b]) -> option[b] {
    match self {
      option.some(v): f(v)
      option.none: option.none
    }
  }

  fn is_some(self: option[a]) -> bool {
    match self {
      option.some(_): true
      option.none: false
    }
  }

  fn is_none(self: option[a]) -> bool {
    match self {
      option.some(_): false
      option.none: true
    }
  }
}

impl result[a, e] {
  fn unwrap_or(self: result[a, e], fallback: a) -> a {
    match self {
      result.success(v): v
      result.failure(_): fallback
    }
  }

  fn map[b](self: result[a, e], f: fn(a) -> b) -> result[b, e] {
    match self {
      result.success(v): result.success(f(v))
      result.failure(err): result.failure(err)
    }
  }

  fn map_fail[f](self: result[a, e], g: fn(e) -> f) -> result[a, f] {
    match self {
      result.success(v): result.success(v)
      result.failure(err): result.failure(g(err))
    }
  }

  fn bind[b](self: result[a, e], f: fn(a) -> result[b, e]) -> result[b, e] {
    match self {
      result.success(v): f(v)
      result.failure(err): result.failure(err)
    }
  }

  fn is_ok(self: result[a, e]) -> bool {
    match self {
      result.success(_): true
      result.failure(_): false
    }
  }

  fn is_err(self: result[a, e]) -> bool {
    match self {
      result.success(_): false
      result.failure(_): true
    }
  }
}
```

All real Marmoset — just `match`. Method-level generics (`map[b]`, `map_fail[f]`) are inferred at each call site, same as any polymorphic function. The explicit declaration attaches to the method name, consistent with `trait show[a]`, `enum option[a]`, etc.

**Tests:**
- `option.some(42).unwrap_or(0)` → 42
- `option.none().unwrap_or(7)` → 7
- `option.some(42).map(fn(x) -> string { x.show() })` → `option.some("42")`
- `option.some(42).bind(fn(x) -> option[int] { option.some(x + 1) })` → `option.some(43)`
- `option.none().map(fn(x: int) -> int { x + 1 })` → `option.none`
- `result.success(42).map(fn(x) { x.show() })` → `result.success("42")`
- `result.failure("err").map_fail(fn(e) { e + "!" })` → `result.failure("err!")`
- `result.success(42).bind(fn(x) { result.success(x + 1) })` → `result.success(43)`
- `result.failure("err").unwrap_or(0)` → 0

**Gate:** `make unit && make integration` green.

---

## Future Work (separate from this plan)

1. **Migrate primitive impls to .mr** — Could move `impl eq for int` etc. to prelude.mr using `extern` blocks post-FFI. Zero user benefit since they already work. Only do if there's a concrete reason.
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
| **New:** `std/prelude.mr` | Enums + traits + option/result methods |
| `lib/frontend/compiler.ml` | Prelude auto-import orchestration (module system component) |
| `lib/frontend/typecheck/builtins.ml` | Remove enum/trait init; keep impl init + builtin functions |
