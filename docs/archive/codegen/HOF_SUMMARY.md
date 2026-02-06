# Polymorphic HOF Limitation: Executive Summary

## Problem
Cannot compile `let map = fn(arr, f) { ... f(...) ... }` where `f` is a function parameter. Generated Go code tries to reference undefined type variables (`T107`, `UNKNOWN`).

## Root Cause
**Current monomorphization** (emitter.ml:203-219) collects instantiations **only at call sites with concrete types**:
```ocaml
let arg_types = List.map (infer_type env) args in  (* Must be concrete! *)
let inst = { func_name = name; concrete_types = arg_types; return_type } in
```

When `f` has polymorphic type `a -> b`, there's no concrete type to instantiate. The system assumes all function definitions exist in `mono_state.func_defs`, but `f` is a **parameter**, not a definition.

**The real issue**: Two distinct problems conflated
- ✓ Works: Polymorphic data types (`[a]` with concrete uses)
- ✗ Fails: Polymorphic function parameters (need **constraint-based** specialization)

## Analysis Results

| Aspect | Finding |
|--------|---------|
| **Available info at codegen** | ✓ Full typed_env, ✓ Substitutions, ✓ Use-site types; ✗ Explicit specialization points |
| **Why HM sufficient** | HM type system is fine—gap is in codegen, not type theory |
| **Solution complexity** | Not impossible, but requires design choice |

## Four Implementation Approaches

| Approach | Difficulty | Performance | Visibility | Phase | Recommendation |
|----------|-----------|-------------|------------|-------|-----------------|
| **1. Constraint-based mono** | Medium | Excellent ⭐⭐⭐ | Full ⭐⭐⭐ | Phase 2 | **RECOMMENDED** |
| **2. Go 1.18+ generics** | Easy | Good ⭐⭐ | Black box | Now | Fast but defeats design goal |
| **3. Closure wrapping** | Medium-Hard | Poor | None | Now | Avoid—loses type safety |
| **4. Type classes** | Hard | Excellent | Full | Phase 3 | Long-term correct solution |

## Detailed Breakdown

### 1. Constraint-Based Monomorphization (RECOMMENDED)
**What it does**: Instead of collecting instantiations, collect **constraints** on type variables, then compute all valid specializations.

**Effort**: ~5 days
- Phase 2 (minimal): Type annotations on functions (~2 days)
  ```marmoset
  let map = fn(arr: [a], f: a -> b) -> [b] { ... }
  ```
- Monomorphization enhancement (~4-5 days)
  - Collect specialization constraints
  - Solve constraints → set of specializations
  - Emit specialized functions recursively

**Why it wins**:
- ✓ Keeps "transparent compilation" (can inspect generated code)
- ✓ Zero runtime overhead
- ✓ Compatible with Phase 3 traits
- ✓ Sets foundation for proper polymorphism ecosystem

### 2. Go 1.18+ Generics
**What it does**: Use Go's generic syntax instead of monomorphization.

```go
// Instead of: func map_int64_int64(...) and func map_string_int64(...)
func map[T any, U any](arr []T, f func(T) U) []U { ... }
```

**Effort**: ~3 days (minimal code changes)

**Why it loses**:
- ✗ Black box—can't inspect generated code
- ✗ Violates design goal: "Transparent, readable Go"
- ✗ Depends on Go 1.18+ (not portable)
- ✗ Duplicates Go compiler's work

**When to use**: If speed to MVP matters more than design philosophy

### 3. Closure Wrapping / Runtime Dispatch
**What it does**: Box all functions in `interface{}`, use runtime type switching.

**Verdict**: ✗ Avoid
- Loses type safety
- Runtime overhead
- Contradicts HM philosophy

### 4. Type Classes / Trait System (Phase 3)
**What it does**: Add trait system with where-clauses for explicit polymorphic constraints.

```marmoset
fn map(arr: [a], f: a -> b) -> [b] where a: Mappable
```

**Effort**: ~4-6 weeks (full system with resolution, impl blocks, dispatch)

**Verdict**: Long-term correct but defers HOF support

## Prerequisite for Solution 1: Phase 2 (Type Annotations)

**Minimal Phase 2** needed:
```marmoset
let id = fn(x: a) -> a { x }
let map = fn(arr: [a], f: a -> b) -> [b] { ... }
```

This is **much simpler** than full Phase 2 with traits. Just:
- Parser: allow type annotations in function parameters
- Typechecker: use annotations to seed quantified type variables
- ~2 days

You DON'T need:
- ✗ Trait definitions
- ✗ Impl blocks
- ✗ Where-clauses
- ✗ Trait dispatch

## Which Approach Fits Best?

**HM Type System Compatibility**: ✓ All approaches work
- HM is not the bottleneck
- The gap is in codegen specialization, not type theory
- Annotations (Phase 2) address the real issue: "which type vars are intentionally polymorphic?"

**Implementation Roadmap**:
```
Option A (Recommended):
  Week 1: Minimal Phase 2 (annotations only)
  Week 2: Constraint-based monomorphization
  Result: Full HOF support, transparent compilation

Option B (Quick win):
  3 days: Switch to Go generics
  Result: HOF works, but loss of visibility

Option C (Long-term):
  Phase 3: Full trait system (4-6 weeks later)
  Result: HOF + ecosystem (Serialize, Display, Iteration traits, etc.)
```

## Feasibility Assessment

| Aspect | Assessment |
|--------|-----------|
| **Root cause identified** | ✓ Yes (monomorphization strategy gap) |
| **Solvable** | ✓ Yes (multiple approaches exist) |
| **Complexity** | Medium (constraint solving + recursion) |
| **Breaking change** | No (Phase 2 backwards compatible) |
| **Phase requirement** | Phase 2 minimum (or current if using Go generics) |
| **Implementation time** | 5 days (constraint-based) or 3 days (Go generics) |

## Conclusion

This is **not a blocker**—it's a **design choice**:

1. **If you want to solve it now**: Use Go generics (~3 days, trade-off: loss of visibility)
2. **If you want to do it right**: Implement Phase 2 + constraint-based mono (~5 days, sets up ecosystem)
3. **If you want long-term**: Plan Phase 3 traits (~4-6 weeks, enables whole polymorphism paradigm)

The HM type system is sufficient. The limitation is entirely in the **codegen strategy**, not the **type system**.

---

**Full analysis**: See `ANALYSIS_HOF_MONOMORPHIZATION.md` (710 lines) for:
- Detailed root cause analysis with code examples
- Algorithm pseudocode for each approach
- Technical implementation roadmap
- Example walkthrough with `map` function
- Constraint solving approach details
