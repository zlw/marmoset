# Function Model Unification Plan

## Maintenance

- Last verified: 2026-03-01
- Implementation status: Planning (not started)
- Update trigger: Any parser/typechecker/codegen change affecting function declarations, method calls, or trait/inherent method resolution
- Related docs:
  - `docs/features/functions-and-polymorphism.md`
  - `docs/features/traits.md`
  - `docs/features/inherent-methods.md`
  - `docs/ARCHITECTURE.md`
  - `docs/plans/module-system.md`

## Summary

This plan unifies function behavior around one core model:

1. Dot-call syntax is sugar over regular function calls.
2. Trait and inherent methods are callable as qualified functions with `.` qualifier syntax.
3. All function declaration shapes normalize to one internal callable form (`let <name> = fn ...` core).

The target is simpler language semantics and simpler compiler internals: fewer special cases in method dispatch, more explicit qualification for ambiguous method sources, and one canonical representation for function definitions.

## Goals

- Make call semantics coherent:
  - `x.m(y)` behaves as sugar for a function call.
  - Qualified forms `Type.m(x, y)` and `Trait.m(x, y)` are first-class.
- Make ambiguity behavior predictable:
  - Inherent/member wins for `x.m(...)`.
  - Multiple trait candidates for `x.m(...)` is a compile error with explicit qualification hint.
- Add method-level generics in impl blocks (`fn name[b](...)`).
- Normalize function declaration forms to a single core representation.
- Keep static dispatch in codegen (no runtime trait object work in this plan).

## Non-Goals

- No dynamic dispatch or trait object redesign.
- No UFCS strong disambiguation syntax (`<Type as Trait>::m`) in this plan.
- No new effect-system semantics.
- No module-system implementation beyond compatibility hooks already defined in `docs/plans/module-system.md`.

## Locked Decisions

1. **Qualifier syntax uses `.` (not `::`).**
   - Inherent-qualified call: `result.map(r, f)`
   - Trait-qualified call: `show.show(x)`

2. **Dot resolution follows unified order** defined in `docs/plans/module-system.md`.
   - For qualified forms (`a.b(args)` where `a` is not a value binding): value > module > enum > trait > type alias > extern.
   - For enum names: variant check first (constructor), then inherent method check. `result.success(42)` is a constructor because `success` is a variant; `result.map(r, f)` is an inherent-qualified call because `map` is not a variant.

3. **Dot-call precedence is member/inherent first.**
   - For `x.m(...)` where `x` IS a value, resolve inherent method first.
   - If no inherent candidate, resolve trait method candidates.

4. **Trait ambiguity on dot-call is an error.**
   - If multiple traits provide `m` for the receiver type, `x.m(...)` errors.
   - User must qualify: `TraitA.m(x, ...)` or `TraitB.m(x, ...)`.

5. **No strong disambiguation form.**
   - Do not add `<Type as Trait>::m`.

6. **Method-level generics use explicit `fn name[b](...)` syntax.**
   - Generics attach to the method name: `fn map[b](...)`, not `fn[b] map(...)`.
   - Consistent with all other named generic declarations: `trait show[a]`, `enum option[a]`, `type pair[a, b]`.
   - For anonymous functions (let-bound), generics stay on `fn`: `let f = fn[a](x: a) -> a { x }` (no name to attach to).
   - No implicit quantification of free type variables.

7. **Function declaration unification.**
   - Internal core form is `let name = fn[...] (...) -> ... { ... }`.
   - Other declaration surfaces are syntax sugar lowered to this model.

## Public Surface Changes

## New/extended valid syntax

```marmoset
trait map_like[a] {
  fn map[b](x: a, f: fn(a) -> b) -> b
}

impl result[a, e] {
  fn map[b](r: result[a, e], f: fn(a) -> b) -> result[b, e] {
    match r {
      result.success(v): result.success(f(v))
      result.failure(err): result.failure(err)
    }
  }
}

let r = result.success(41)
let r2 = r.map(fn(x: int) -> int { x + 1 })             # dot sugar
let r3 = result.map(r, fn(x: int) -> int { x + 1 })     # inherent-qualified
# Note: result.success(41) is an enum constructor (success is a variant).
# result.map(r, f) is an inherent-qualified call (map is NOT a variant).
```

```marmoset
trait printable[a] { fn format(x: a) -> string }
trait debugable[a] { fn format(x: a) -> string }

type user = { id: int }

impl printable for user { fn format(u: user) -> string { "print" } }
impl debugable for user { fn format(u: user) -> string { "debug" } }

let u: user = { id: 1 }

# u.format()  # compile error: ambiguous trait method
let p = printable.format(u)
let d = debugable.format(u)
```

## Error behavior changes

- Inherent/trait same-name collisions at impl-definition time are no longer globally rejected.
- Ambiguity is handled at call sites:
  - `x.m(...)` resolves by precedence rules and can error for ambiguity.
  - `Trait.m(x, ...)` is explicit and deterministic.

## Compiler Architecture Changes

## Parser and AST

### Current state (relevant)

- Parser represents dot calls as `AST.MethodCall(receiver, name, args)`.
- Method impl AST currently has no method generic parameter field.
- Impl method parser does not support `fn name[...]` in method definitions.

### Planned AST updates (`lib/frontend/syntax/ast.ml`)

1. Extend method signature and method impl nodes:
   - `method_sig` gets `method_generics : generic_param list option`
   - `method_impl` gets `impl_method_generics : generic_param list option`

2. Add a normalized function declaration representation:
   - Keep surface statements as parsed today.
   - Add a desugaring pass that lowers to core `let = fn` model.
   - For compiler internals, all executable function bodies are handled through one normalized function payload shape.

### Parser updates (`lib/frontend/syntax/parser.ml`)

1. Parse optional generics after method name:
   - Trait method signatures: `fn name[a, b: show](...) -> ...`
   - Impl/inherent method definitions: `fn name[a](...) -> ... { ... }`
2. Keep `.` parsing shape unchanged (still `FieldAccess`/`MethodCall`); resolution happens in typechecker.
3. Add parser tests for all new generic method forms and rejection paths.

## Name resolution and typechecking

### Core resolution algorithm for `receiver.method(args...)`

Implemented in `lib/frontend/typecheck/infer.ml` with helper extraction to keep logic testable.

The unified dot resolution order is defined in `docs/plans/module-system.md` and governs all `a.b` / `a.b(args)` forms across modules, enums, traits, types, and extern qualifiers. This section details the function-model-specific paths within that order.

Given `receiver.method(args...)`:

1. **Qualified form detection (no AST change required).**
   - If `receiver` is a bare identifier and it is not a value binding in scope, follow the unified dot resolution order:
     - Module name → module access (handled by module system)
     - Enum name → check if `method` is a variant (constructor) or inherent method (inherent-qualified call)
     - Trait name → trait-qualified call (`Trait.m(args...)`)
     - Type alias name → inherent-qualified call (`Type.m(args...)`)
     - Extern qualifier → FFI call (handled by FFI system)

2. **Normal dot call (`x.m(...)`) path** (receiver IS a value binding).
   - Infer `x` type.
   - Resolve inherent method candidates first.
   - If inherent found: choose it.
   - Else resolve trait candidates:
     - 0 candidates: preserve existing field-function fallback behavior.
     - 1 candidate: choose it.
     - >1 candidates: emit ambiguity error with trait list and qualification hint.

3. **Constrained type variable path.**
   - Use expanded trait constraints.
   - If exactly one trait contributes method name, resolve.
   - If multiple, ambiguity error (same hint style).

4. **Trait-qualified call (`Trait.m(x, ...)`) path.**
   - Validate trait exists and method exists.
   - Infer first argument as receiver type.
   - Validate trait satisfaction for receiver type.
   - Instantiate trait type parameter and method-level generics.

5. **Inherent-qualified call (`Type.m(x, ...)` or `Enum.m(x, ...)`) path.**
   - For enum names: only reached if `m` is NOT a variant (variants are constructors, handled separately).
   - Resolve method from inherent registry keyed by target type and method name.
   - Unify first argument with target type.
   - Instantiate impl target generics and method-level generics.

### Registry and coherence changes

#### `lib/frontend/typecheck/inherent_registry.ml`

- Keep duplicate checks per same target + method.
- Keep concrete-over-generic precedence.
- Keep ambiguous generic inherent matches as errors.
- No global trait-collision prohibition here.

#### `lib/frontend/typecheck/trait_registry.ml`

- Keep trait impl uniqueness/coherence.
- Keep method ambiguity reporting across traits.
- Add helper queries for explicit trait-qualified method lookup and method-level generic instantiation.

#### `lib/frontend/typecheck/infer.ml`

- Replace current trait-first-then-inherent logic with locked precedence:
  - inherent-first for dot call.
- Remove registration-time inherent-vs-trait collision rejection logic from inherent impl validation.
- Extend method resolution metadata to capture qualified-call lowering needs.
- Add method generic instantiation and obligation checks.

## Lowering and codegen

### Principle

Codegen stays static and direct. No closure-wrapping overhead is introduced by declaration desugaring.

### Current state

The emitter already has call-target metadata: `method_resolution = TraitMethod of string | InherentMethod` stored in `global_method_resolution_store`, keyed by `expr.id`. The emitter uses `lookup_method_resolution` and does NOT re-resolve from AST shape. This is good — we extend it, not replace it.

### Planned changes

1. Extend existing `method_resolution` type to cover qualified-call forms (trait-qualified and inherent-qualified calls produce the same resolution metadata as their dot-call equivalents).
2. Lower all method/qualified calls to direct helper function calls with explicit argument lists.
3. Keep existing helper naming families:
   - Trait: `<trait>_<method>_<type_suffix>(receiver, args...)`
   - Inherent: `inherent_<method>_<type_suffix>(receiver, args...)`
4. Ensure trait-qualified call and dot-call that resolve to trait use the same emitted helper.
5. Ensure inherent-qualified call and dot-call that resolve to inherent use the same emitted helper.

### Files

- `lib/backend/go/emitter.ml`
  - update method-call emission path to consume new resolution metadata
  - no runtime dispatch changes

## Function Declaration Unification

## Internal canonical form

All function definitions that produce runtime callable code are normalized to:

```marmoset
let name = fn[generics](params...) -> ret { body }
```

This is an internal canonical representation, not a forced source-style migration.

## Scope of normalization

1. Top-level function declarations (if parser supports named `fn` declarations) lower to `let = fn`.
2. Inherent/trait impl method definitions lower to the same function payload model internally, plus registry metadata (`owner`, `method_name`, constraints).
3. Trait method signatures are type-level declarations and remain signature nodes, but their function-type shape uses the same callable type representation.

## Why this helps

- One inference code path for function-body typing.
- One codegen pipeline for callable bodies.
- Fewer bespoke conversions between method nodes and function nodes.

## Phased Implementation

### Phase F1: Characterization tests + method-level generics in parser + AST

Goal: lock current behavior with characterization tests, then parse and carry `fn name[...]` generics in trait/impl/inherent methods.

Changes:
- Add focused integration tests under existing suites (no new edge-case-only files):
  - dot-call currently working cases
  - ambiguous trait method error
  - field-function fallback (`x.f(...)` where `f` is function-valued field)
  - generic inherent method baseline

Files:
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- parser tests in same file

Acceptance:
- characterization tests lock current dot-call, ambiguity, and field-function behavior.
- trait method signatures preserve generic params in AST.
- impl/inherent method definitions preserve generic params in AST.
- invalid generic syntax produces deterministic parser diagnostics.

### Phase F2: Typechecker resolution model switch

Goal: implement locked resolution rules and qualified calls.

Files:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/inherent_registry.ml`

Acceptance:
- inherent-first precedence for `x.m(...)`.
- trait-qualified `Trait.m(x, ...)` works.
- inherent-qualified `Type.m(x, ...)` works.
- ambiguous trait methods on dot-call error with clear qualification hints.
- constrained TVar ambiguity surfaces at call site.
- registration-time inherent-vs-trait collision rejection removed.

### Phase F3: Call lowering + emitter alignment

Goal: make codegen consume resolved call targets only.

Files:
- `lib/frontend/typecheck/infer.ml` (resolution metadata)
- `lib/backend/go/emitter.ml`

Acceptance:
- generated Go for dot-call and equivalent qualified call is identical modulo temporary names.
- no fallback re-resolution in emitter.
- existing enum constructor dot syntax behavior remains unchanged.

### Phase F4: Function declaration normalization pass

Goal: canonical internal callable form (`let = fn`) across declarations.

Files:
- new: `lib/frontend/syntax/normalize_functions.ml` (or similarly named module)
- wiring in compiler pipeline (parser -> normalize -> typecheck)
- `lib/frontend/typecheck/infer.ml` call sites updated for normalized shape

Acceptance:
- normalized AST preserves source spans and file IDs for diagnostics.
- no semantic regressions in function inference.
- method definitions and top-level named function forms share body-inference path.

### Phase F5: Docs and cleanup

Goal: align specs with implementation.

Files:
- `docs/features/functions-and-polymorphism.md`
- `docs/features/traits.md`
- `docs/features/inherent-methods.md`
- `docs/ARCHITECTURE.md`

Acceptance:
- docs reflect precedence, ambiguity, and qualified-call syntax exactly.

## Test Plan

## Unit tests

1. Parser:
   - method-level generics parse in trait and impl/inherent methods.
   - malformed method generic syntax fails cleanly.
2. Typechecker:
   - inherent-first selection for dot-call.
   - trait-qualified call success and failure modes.
   - inherent-qualified call success and failure modes.
   - multi-trait ambiguity diagnostics.
   - method generic instantiation with constraints.
3. Registry:
   - no definition-time rejection for inherent/trait same-name overlap.
   - duplicate inherent for same target still rejected.
4. Emitter:
   - call target metadata drives helper selection.
   - equivalent dot/qualified cases emit equivalent helper calls.

## Integration scenarios (must be added or updated)

1. `result[a, e]` inherent generic method returning `result[b, e]`.
2. Trait method called both via `x.m(...)` and `Trait.m(x, ...)`.
3. Two traits same method name on same type:
   - `x.m(...)` ambiguous error
   - each `TraitX.m(x, ...)` path succeeds.
4. Inherent + trait same method name:
   - `x.m(...)` selects inherent.
   - trait method still reachable via `Trait.m(x, ...)`.
5. Field function fallback still works and is not captured by trait/type qualifier logic.
6. Method-level generic constraints enforced (`fn name[b: show]` cases).

## Regression policy

- Any bug found while implementing this plan gets an integration test in the existing suite files (not separate temporary edge files).

## Risks and Mitigations

1. **Dot overloading with modules/traits/types/values can become brittle.**
   - Mitigation: centralize dot-resolution order in one function with exhaustive tests.

2. **Generic method instantiation can regress inference quality.**
   - Mitigation: add unit tests for expected type-driven inference and explicit ambiguity diagnostics.

3. **Emitter mismatch with typechecker resolution metadata.**
   - Mitigation: make unresolved method resolution in emitter a hard internal error in debug/test mode.

4. **Normalization pass can lose source spans.**
   - Mitigation: preserve `pos`, `end_pos`, `file_id` on rewritten nodes and test diagnostics location stability.

## Parser/Typechecker/Codegen Complexity Impact

- **Parser:** mildly simpler in the long run (fewer semantically meaningful distinctions in declaration forms), slightly larger short-term due method generic parsing.
- **Typechecker:** becomes simpler conceptually (single call model), though initial migration is substantial.
- **Codegen:** simpler and safer after migration (emit from resolved call targets, no semantic guessing from surface AST).

## Assumptions and Defaults

1. `.` remains the only qualifier token for this feature set.
2. Existing source using dot-call remains source-compatible.
3. Dot qualification follows the unified resolution order in `docs/plans/module-system.md`. Value bindings win first; module/enum/trait/type/extern checks follow in defined order.
4. For enum names, variant lookup takes priority over inherent method lookup — constructors are never shadowed by methods.
5. Strong disambiguation syntax is intentionally omitted.
6. Dispatch remains static in Go emitter.

## Acceptance Criteria (Project-level)

1. `make unit` passes.
2. `make integration` passes.
3. New qualified-call and ambiguity tests are present in maintained integration suites.
4. Docs updated to match implemented behavior exactly.
