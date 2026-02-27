# Trait Satisfaction Review (Claude, section-by-section)

## Maintenance

- Last verified: 2026-02-27
- Reviewer: Claude (Opus 4.6)
- Source spec: `docs/features/trait-satisfaction.md`
- Codex review: `docs/review/trait-satisfaction-codex.md`
- Scope: parser/AST, typechecker (infer.ml, trait_registry.ml, trait_solver.ml), Go emitter, integration tests
- Baseline tests: `103 passed, 0 failed` (integration) + all unit tests green
- Method: read every relevant source file line-by-line, then compared against spec

---

## How to read this document

Each spec section gets a verdict:

- **CONFORMANT** - implementation matches spec
- **PARTIALLY CONFORMANT** - core idea works but details diverge or are incomplete
- **NOT IMPLEMENTED** - feature absent from codebase
- **ACTION NEEDED** - concrete fix required (numbered for tracking)

Items marked **ACTION** have a priority tag: `[P0]` (blocks correctness of existing features), `[P1]` (blocks spec conformance for planned features), `[P2]` (nice-to-have / future).

---

## Section 0: Terminology

No code implications. Just noting: the spec defines "trait object" and "constraint context" which do not exist in our codebase yet. Terminology is otherwise consistent.

---

## Section 1: Trait Kinds

### 1.1 Field-only traits (STRUCTURAL)

**NOT IMPLEMENTED.**

The AST `trait_def` in `ast.ml:36-41` stores only `methods : method_sig list`. There is no field storage at all. The parser (`parser.ml:553+`) only accepts `fn ...` members inside trait bodies.

Additionally, the AST requires `type_param : string` (not `string option`), meaning every trait must have a type parameter. The registry stores it as `string option` (allowing `None`), but the parser always demands `[a]` syntax.

> **ACTION-01 [P1]:** Add `fields : (string * type_expr) list` to `trait_def` in AST and registry. Update parser to accept `name: type` members (not just `fn ...`).
>
> **ACTION-02 [P1]:** Make type parameter optional in parser for traits that don't need one (field-only traits like `trait named { name: string }` have no type parameter).

### 1.2 Method-only traits (NOMINAL)

**CONFORMANT.** This is what we have today. All traits are method-only.

### 1.3 Mixed traits (HYBRID)

**NOT IMPLEMENTED.** Blocked by ACTION-01. Once field members are parseable, mixed traits fall out naturally.

### 1.4 Supertraits

**PARTIALLY CONFORMANT.**

Supertraits are declared and stored:
- AST: `supertraits : string list` (`ast.ml:39`)
- Registry: `trait_supertraits : string list` (`trait_registry.ml:17`)
- Parser: `: eq + hash` syntax works (`parser.ml:509-514`)
- Validation: `validate_trait_def` checks supertraits exist at definition time (`trait_registry.ml:211-219`)

**But supertrait obligations are never enforced at satisfaction time.** See Section 3.4.

### Trait kind classification

**NOT IMPLEMENTED.** No `trait_kind` function or enum exists anywhere. The spec requires classifying traits as field-only/method-only/mixed to determine satisfaction rules.

> **ACTION-03 [P1]:** Add `trait_kind` classification function to trait_registry that inspects fields/methods presence.

---

## Section 2: Satisfaction Model Overview

### 2.1 Fields are structural

**NOT IMPLEMENTED.** No structural field satisfaction exists (blocked by missing field members in traits).

### 2.2 Methods are nominal

**CONFORMANT.**

Verified by reading every method resolution path:

1. **Concrete types:** `infer.ml:673-677` calls `Trait_registry.resolve_method` which searches the impl registry by canonical `(trait_name, mono_type)` key. No structural probing.

2. **Constrained type variables:** `infer.ml:641-672` checks the constraint store, finds traits that define the method, and resolves from trait *definitions* (not structural inspection of the receiver).

3. **Trait solver:** `trait_solver.ml:7-17` - `implements_trait` checks `Trait_registry.implements_trait` which does exact key lookup. No "does this type happen to have a method named X" logic.

No fix needed for this sub-point.

### 2.3 Mixed traits

**NOT IMPLEMENTED** (blocked by field traits).

---

## Section 3: Satisfaction Rules in Detail

### 3.1 Field satisfaction: T |= fields(Tr)

**NOT IMPLEMENTED.**

No `satisfies_fields` function exists. No code path checks whether a record type has fields matching a trait's field requirements.

> **ACTION-04 [P1]:** Implement `satisfies_fields(T, Tr)` in trait_registry or a new `trait_satisfaction.ml` module. Must: (a) require T is a record type, (b) check each required field exists, (c) unify field types.

### 3.2 Method satisfaction: T |= methods(Tr)

**PARTIALLY CONFORMANT.**

What works:
- Concrete impl lookup via `(trait_name, mono_type)` canonical key: `trait_registry.ml:77-78`
- Duplicate impl rejection: `trait_registry.ml:63-67`
- Ambiguity detection when multiple traits provide same method: `trait_registry.ml:104-116`
- Builtin impls: registered with `~builtin:true`, user can override once

What doesn't work:

**Generic/conditional impls are broken.** The impl registry key is `(string * mono_type)`, which means only fully-concrete types can be looked up. A generic impl like `impl show[b] for list[b]` would need to be stored differently and matched via unification, not exact key equality.

I verified this by reading the impl registration path. The `impl_for_type` is converted to `mono_type` via `canonical_type`, but if it contains type variables (like `TVar "b"`), those would be stored literally. Lookup for `list[int]` would not match `list[b]` because the key comparison is structural equality, not unification.

The Codex review says this path crashes with "Unknown type constructor: b" during type conversion. I believe this is accurate based on reading the `type_expr_to_mono_type` conversion path in `annotation.ml`, which would fail on unbound type parameter names.

> **ACTION-05 [P0]:** Fix generic impl handling. Options: (a) implement unification-based impl lookup, or (b) monomorphize generic impls at registration time for each concrete usage. At minimum, catch the exception and produce a clear error instead of crashing.

### 3.3 Full trait satisfaction T |= Tr (by kind)

**NOT IMPLEMENTED.**

No unified `satisfies_trait` function exists. Satisfaction checks are scattered:
- Method call resolution: `infer.ml:640-677` (ad-hoc per-call lookup)
- Constraint checking: `infer.ml:190-192` via `obligations_from_substitution` -> `verify_obligations` -> `Trait_solver.check_constraints`
- None of these check field satisfaction or apply kind-based rules

> **ACTION-06 [P1]:** Create unified `satisfies_trait(T, Tr)` API that: (a) determines trait kind, (b) checks fields (structural) and/or methods (nominal) based on kind, (c) checks supertraits recursively. Use this from constraint checker and method resolver.

### 3.4 Supertrait satisfaction

**NOT ENFORCED.**

I verified this by reading every call path:

1. **`trait_solver.ml:7-17`** - `implements_trait` only checks direct impl registry lookup. Comments say "we don't need [supertraits] for this check" and returns `false` if no direct impl. There is NO recursive supertrait check.

2. **`validate_impl` in `trait_registry.ml:299-308`** - Validates signature correctness but does NOT check that supertrait impls exist. You can do `trait ord[a]: eq { fn compare(x: a, y: a) -> int }` then `impl ord for int { ... }` without having `impl eq for int`, and it will succeed.

3. **`verify_constraints_in_substitution` in `infer.ml:190-192`** - Checks each constraint independently. If a function has `fn[a: ord](x: a)` and `ord` has supertrait `eq`, only the `ord` constraint is checked, not `eq`.

This means:
- `impl ord for int` succeeds even without `impl eq for int` (violates spec)
- `fn[a: ord](x: a)` only checks `ord` impl exists, never checks `eq` (violates spec)
- Methods from supertraits are not available through the supertrait relationship

> **ACTION-07 [P0]:** Enforce supertrait satisfaction in THREE places:
> 1. `validate_impl`: when registering `impl Tr for T`, verify that T implements all of Tr's supertraits
> 2. `check_constraints` / `verify_obligations`: when checking `T: Tr`, also check `T: S` for all supertraits S of Tr (recursively)
> 3. Method resolution for constrained type vars: expand available methods through supertrait closure (if `a: ord` and `ord: eq`, then `a.eq(b)` should work)

---

## Section 4: Orphan Policy (Coherence & Ambiguity)

### 4.1 Impl uniqueness rule

**PARTIALLY CONFORMANT.**

For exact concrete type keys: duplicate detection works via `trait_registry.ml:63-67`.

For generic/overlapping impls: no overlap checking exists (no generic impls are supported, so this is currently moot but will matter when ACTION-05 is addressed).

### 4.2 Ambiguity errors include both impl sites

**NOT IMPLEMENTED.**

Ambiguity errors include trait names but no source locations. `trait_registry.ml:112-116`:
```
"Ambiguous method '%s' for type %s (provided by traits: %s)"
```

The `impl_def` type stores no source span information. Neither does `trait_def`.

> **ACTION-08 [P2]:** Add source span fields to registry `trait_def` and `impl_def`. Include both candidate impl locations in ambiguity error messages.

### 4.3 Field-only traits do not have impls

**NOT IMPLEMENTED** (no field traits exist). But this is a good design note: when ACTION-01 lands, ensure field-only traits are excluded from impl registry.

### 4.4 Mixed traits still need method impls

**NOT IMPLEMENTED** (no mixed traits exist). Design note for when mixed traits land.

---

## Section 5: Constraint Checking

### 5.1 Call-site checking

**CONFORMANT.**

Constraint checking happens at function call sites in `infer.ml:1214-1221`:
```ocaml
match verify_constraints_in_substitution final_subst with
| Error msg -> Error (error_at (ConstructorError msg) func)
| Ok () -> ...
```

The flow is:
1. Generic params get fresh type vars with constraints stored in `constraint_store` (`infer.ml:1053-1069`)
2. At call site, after unification resolves type vars to concrete types, `verify_constraints_in_substitution` walks the substitution and checks each resolved var's constraints via `Trait_solver.check_constraints`
3. Errors are reported at the call site

Integration tests confirm this works:
- TEST 61: `fn[a: show](x: a)` with int (succeeds)
- TEST 62: `fn[a: show](x: a)` with array (fails with "does not implement trait")
- TEST 63-64: Multiple constraints `fn[a: show + eq](x: a, y: a)` (succeeds)

### 5.2/5.3 Error detail

**PARTIALLY CONFORMANT.**

Current error format (`infer.ml:138-143`):
```
Trait obligation failed (constraint on type variable 'tN'): Type <T> does not implement trait <Tr>
```

This is reasonable but lacks structured categories. The spec wants:
- missing field(s) for field traits
- missing impl for method traits
- missing supertrait satisfaction
- ambiguous impl

Currently all failures produce the same generic message.

> **ACTION-09 [P2]:** Upgrade constraint failure diagnostics to include reason categories (missing impl, missing field, missing supertrait, ambiguous).

---

## Section 6: Method Call Resolution

### 6.1 Candidate sources

**PARTIALLY CONFORMANT.**

Currently supported:
1. Enum constructors: `infer.ml:574-628` (checked first)
2. Trait methods: `infer.ml:630-767` (checked second)

NOT supported:
3. Inherent methods: planned but not implemented (see `docs/features/inherent-methods.md`)

Field access (`receiver.field`) is correctly separated from method resolution. The parser distinguishes `receiver.name` (FieldAccess) from `receiver.method(args)` (MethodCall) based on whether parentheses follow.

### 6.2 No structural method probing

**CONFORMANT.**

I verified: there is no code path that checks "does this record type have a field or method matching this name" for trait satisfaction. Method resolution goes exclusively through the trait registry.

### 6.3 Collisions

**PARTIALLY CONFORMANT.**

Trait-vs-trait collisions are hard errors: `trait_registry.ml:111-116` (multiple trait candidates for same method on same type).

Inherent-vs-trait collisions: not relevant yet (no inherent methods). The inherent methods spec (`docs/features/inherent-methods.md`) correctly plans for this as a hard error.

### 6.4 Dispatch mode

**PARTIALLY CONFORMANT (static only).**

Static dispatch works correctly:
- Typechecker resolves method to `(trait_name, method_sig)` pair
- Emitter re-resolves at codegen time (see note below) and emits `trait_method_type(receiver, args...)`

**Concern: emitter re-resolves traits instead of using typechecker output.**

The emitter at `emitter.ml:856` calls `Trait_registry.resolve_method` again during codegen:
```ocaml
match Typecheck.Trait_registry.resolve_method receiver_type variant_name with
```

This is redundant work and a potential source of inconsistency if the registry state differs between typecheck and codegen phases. The typechecker already resolved the method and could store the result in the type_map.

> **ACTION-10 [P2]:** Carry resolved method identity from typechecker (e.g., store `(trait_name, method_name, receiver_type)` in the type_map or a parallel resolution map) instead of re-resolving in the emitter.

Additionally: for constrained type variables (generics), the emitter currently receives the **monomorphized** concrete type from the type_map (since generic functions are instantiated at call sites during type inference). This works correctly for the current static dispatch model. But if receiver type in type_map is still a `TVar` (unresolved), `mangle_type` would produce `"any"`, which would generate incorrect Go code. I did not find a test that exercises this edge case.

> **ACTION-11 [P1]:** Add a test that compiles a constrained generic function call and verify the emitted Go code uses the concrete monomorphized type, not `"any"`. Verify what happens if the type map contains an unresolved TVar for a method call receiver.

---

## Section 7: Trait Objects (Trait as Type)

### 7.1 Field-only trait objects

**NOT IMPLEMENTED.**

Trait names in type positions are rejected as unknown type constructors. No Go interface generation for field-only traits exists.

### 7.2 Method-only trait objects

**NOT IMPLEMENTED** (correctly absent per spec - should be disallowed in v1).

### 7.3 Mixed trait objects

**NOT IMPLEMENTED** (correctly absent per spec - should be disallowed in v1).

### 7.4 Trait in list[Tr]

**NOT IMPLEMENTED.**

> **ACTION-12 [P1]:** When trait-as-type is encountered in the typechecker, produce an explicit error message distinguishing between: (a) field-only traits (should be allowed in future), (b) method/mixed traits (explicitly disallowed in v1). Currently all trait names in type position silently fail as unknown type constructors, which is a confusing error.

---

## Section 8: Operators via Traits

**NOT IMPLEMENTED.**

I read the entire operator handling in `infer.ml:813-849`. Operators use direct type unification:

```ocaml
(* Arithmetic: +, -, *, / *)
match unify left_type' right_type with ...

(* Comparison: <, >, <=, >= *)
match unify left_type' right_type with ...

(* Equality: ==, != *)
match unify left_type' right_type with ...
```

There are even TODO comments: `(* TODO: proper numeric type class *)` at lines 837 and 801.

This means `==` works on any type that unifies (including records without `eq` impl), and `<` works on any type that unifies (without requiring `ord` impl). The spec says these should require trait impls.

> **ACTION-13 [P1]:** Route operator typing through trait requirements. `==`/`!=` should require `eq[T]`, `<`/`>`/`<=`/`>=` should require `ord[T]`, `+`/`-`/`*`/`/` should require `num[T]` (or at minimum restrict to known numeric types). This is a significant behavioral change - needs careful rollout.

---

## Section 9: Records + Row Polymorphism Integration

### 9.1 Field traits as named row constraints

**NOT IMPLEMENTED.** Blocked by missing field traits. When field traits land, `t: named` should desugar to a row constraint requiring `name: string`.

### 9.2 Users don't write row vars in signatures

**CONFORMANT.** Row variables exist internally but are not exposed in function signature syntax.

### 9.3 Pattern matching with open record patterns

**CONFORMANT.** Record patterns support `{ x:, ...rest }` syntax.

---

## Section 10: Implementation Plan Conformance

### 10.1 Data structures

**PARTIALLY CONFORMANT.**

What matches:
- Trait registry stores name, type params, supertraits, methods
- Impl registry stores trait name, for_type, methods
- Builtin impl tracking works

What's missing:
- No required fields in trait definition
- No trait kind classification
- No `satisfies_fields` / `satisfies_trait` APIs

### 10.2 Core functions

| Function | Status | Notes |
|----------|--------|-------|
| `trait_kind(trait_def)` | Missing | No classification exists |
| `satisfies_fields(T, trait)` | Missing | No field satisfaction |
| `solve_impl(T, trait)` | Partial | Works for concrete types only, no generic impl matching |
| `satisfies_trait(T, trait)` | Missing | No unified entry point |

### 10.3 Where invoked

| Invocation point | Status | Notes |
|-----------------|--------|-------|
| Constraint checking at instantiation | Working | `verify_constraints_in_substitution` at call sites |
| Method call resolution | Working | `resolve_method` for concrete types, constraint-based for type vars |
| Trait object coercion | Missing | No trait-as-type support |

### 10.4 Codegen requirements

| Requirement | Status | Notes |
|-------------|--------|-------|
| Field-only trait Go interfaces | Missing | No field traits exist |
| Auto-generated getters | Missing | No field traits exist |
| Static dispatch for method traits | Working | Mangled function names, no Go interfaces |

---

## Section 11: Compliance Checklist

### Currently passing

- [x] Method trait calls are nominal (no TS-style structural method satisfaction)
- [x] `x.name` field access and `x.show()` method resolution are separated
- [x] Ambiguous trait-method dispatch is rejected
- [x] Constraints are checked at call sites
- [x] Builtin impls can be overridden by user impls (exactly once)
- [x] Impl signatures are validated against trait definitions with type parameter substitution

### Currently failing

- [ ] Parser/AST cannot represent field or mixed traits (ACTION-01, ACTION-02)
- [ ] Trait kind not classified (ACTION-03)
- [ ] No structural field satisfaction (ACTION-04)
- [ ] Generic impls broken (ACTION-05)
- [ ] No unified `satisfies_trait` API (ACTION-06)
- [ ] Supertraits not semantically enforced (ACTION-07)
- [ ] Ambiguity errors lack source spans (ACTION-08)
- [ ] Trait objects absent (ACTION-12)
- [ ] Operator overloading not trait-based (ACTION-13)

---

## Section 12: Open Decisions Locking

The spec requires explicit policy enforcement for v1 decisions. Current state:

| Decision | Spec answer | Code state | Verdict |
|----------|------------|------------|---------|
| Field-only traits satisfied by non-records? | No (v1) | N/A (field traits missing) | Needs explicit rejection when field traits land |
| Method/mixed trait objects allowed? | No (v1) | Implicitly rejected (unknown type constructor) | Needs explicit error message (ACTION-12) |
| Mixed traits require method impl even if fields match? | Yes | N/A (mixed traits missing) | Needs enforcement when mixed traits land |
| Inherent methods imply trait satisfaction? | No (ever) | N/A (no inherent methods) | Needs explicit rejection when inherent methods land |

> **ACTION-14 [P1]:** When implementing ACTION-12, ensure v1 policy is explicitly enforced with dedicated error messages rather than falling through to generic "unknown type" errors.

---

## Additional Findings (not in spec but discovered during review)

### A. Emitter uses `failwith` on method resolution failure

At `emitter.ml:857`:
```ocaml
| Error msg -> failwith msg
```

If the emitter encounters a method call for which trait resolution fails, it crashes with an unhandled exception rather than producing a compiler error. The typechecker should have caught this, so this is a defense-in-depth issue, but a `failwith` in the emitter is fragile.

> **ACTION-15 [P2]:** Replace `failwith` in emitter method resolution with proper compiler error reporting.

### B. Derive `generate_derived_impl` is a stub

At `trait_registry.ml:162`:
```ocaml
(* TODO: Generate actual implementations based on type structure *)
```

The registry's `generate_derived_impl` creates method signatures with correct type substitution but no actual implementation bodies. The *real* derived impl code generation happens in the emitter (`emitter.ml:1982-2075`) which hardcodes the Go code for each derivable trait.

This split means the registry has "ghost" impls: they pass signature validation but have no bodies. The emitter then independently generates bodies based on type structure. This works but is architecturally fragile - the two halves could drift.

> **ACTION-16 [P2]:** Consolidate derive impl generation so the registry and emitter agree on what's derived. Low priority since current behavior is correct.

### C. Constraint store is global mutable state

The constraint store (`infer.ml:86-100`) uses a global mutable hashtable wrapped in `inference_state`. While the `with_inference_state` pattern provides some isolation, the global mutable nature means:
- Tests must carefully `clear()` before each test
- Concurrent or re-entrant inference would be unsafe

This is fine for the current single-threaded compiler but worth noting for future refactoring.

### D. No test for supertrait violation

There is no integration or unit test that verifies:
1. `impl ord for int` (without `impl eq for int` where `trait ord[a]: eq`) is rejected
2. Calling `fn[a: ord](x: a)` propagates the `eq` supertrait obligation
3. Methods from supertraits are available on subtrait-constrained type variables

> **ACTION-17 [P0]:** Add tests that demonstrate supertrait enforcement (these should currently FAIL, proving ACTION-07 is needed). Specifically:
> - Test that `impl ord for T` without `impl eq for T` is rejected
> - Test that `fn[a: ord](x: a) { x.eq(x) }` works (supertrait method availability)
> - Test that calling `fn[a: ord](x: a)` with a type that has `ord` but not `eq` is rejected

---

## Prioritized Action Summary

### P0 - Blocks correctness of existing features

| ID | Description | Files affected |
|----|-------------|----------------|
| ACTION-05 | Fix generic impl handling (crash on `impl show[b] for list[b]`) | `infer.ml`, `trait_registry.ml`, `annotation.ml` |
| ACTION-07 | Enforce supertrait satisfaction (impl validation, constraints, method availability) | `trait_solver.ml`, `trait_registry.ml`, `infer.ml` |
| ACTION-17 | Add supertrait enforcement tests (expect current failures) | `test/test_typecheck_and_codegen.sh`, `trait_registry.ml` |

### P1 - Blocks spec conformance for planned features

| ID | Description | Files affected |
|----|-------------|----------------|
| ACTION-01 | Add field members to trait AST/parser/registry | `ast.ml`, `parser.ml`, `trait_registry.ml` |
| ACTION-02 | Make trait type parameter optional | `ast.ml`, `parser.ml` |
| ACTION-03 | Add trait kind classification | `trait_registry.ml` |
| ACTION-04 | Implement structural field satisfaction | `trait_registry.ml` or new module |
| ACTION-06 | Create unified `satisfies_trait` API | `trait_registry.ml` or new module |
| ACTION-11 | Test generic function codegen for unresolved TVar edge case | `test/`, `emitter.ml` |
| ACTION-12 | Explicit error for trait-as-type with kind-based messaging | `infer.ml` or `annotation.ml` |
| ACTION-13 | Route operators through trait requirements | `infer.ml` |
| ACTION-14 | Enforce v1 policy decisions with explicit diagnostics | Various |

### P2 - Nice-to-have / future

| ID | Description | Files affected |
|----|-------------|----------------|
| ACTION-08 | Add source spans to ambiguity errors | `trait_registry.ml`, `ast.ml` |
| ACTION-09 | Structured constraint failure diagnostics | `trait_solver.ml`, `infer.ml` |
| ACTION-10 | Carry resolved method from typechecker to emitter | `infer.ml`, `emitter.ml` |
| ACTION-15 | Replace emitter `failwith` with proper error | `emitter.ml` |
| ACTION-16 | Consolidate derive impl generation | `trait_registry.ml`, `emitter.ml` |

---

## Agreement with Codex Review

I largely agree with the Codex review (`docs/review/trait-satisfaction-codex.md`). Key points of agreement:

1. Field traits / mixed traits not representable - confirmed
2. Supertrait satisfaction not enforced - confirmed (I verified every call path)
3. Generic impls broken - confirmed (exact key lookup can't match parameterized types)
4. Operators not trait-driven - confirmed (TODO comments in code acknowledge this)
5. Emitter re-resolves traits - confirmed (emitter.ml:856)

Additional findings beyond Codex review:
- ACTION-11: unresolved TVar edge case in codegen
- ACTION-15: `failwith` in emitter on resolution failure
- ACTION-16: derive impl split between registry and emitter
- ACTION-17: explicit test gap for supertrait enforcement

---

## Conclusion

The trait system is **solid for its current scope**: method-only nominal traits with static dispatch on concrete types. The core machinery (registry, impl validation, signature checking, type parameter substitution, constraint verification at call sites, ambiguity detection) is well-implemented and well-tested.

The gaps fall into two categories:

1. **Correctness issues in existing features** (P0): supertrait enforcement and generic impl handling. These should be fixed before adding new features.

2. **Missing spec features** (P1): field traits, mixed traits, trait objects, operator traits. These are forward-looking and can be implemented incrementally.

Recommended sequence: ACTION-17 (tests) -> ACTION-07 (supertraits) -> ACTION-05 (generic impls) -> ACTION-01/02/03 (field trait foundation) -> rest.
