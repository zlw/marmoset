# Analysis: Polymorphic Higher-Order Functions in Go Backend

## Executive Summary

The Marmoset Go backend **cannot currently compile polymorphic higher-order functions** like `map(arr, f)` where `f` is a function parameter. The root cause is that current monomorphization works by generating specialized functions for each **concrete type instantiation**, but function parameters are inherently **abstract** at the call site.

**Key finding**: This is NOT a simple bug—it's a fundamental limitation of the current architecture that requires choosing between different design philosophies. The current system conflates two distinct problems: (1) specializing polymorphic data types and (2) specializing polymorphic function types.

---

## 1. Root Cause Analysis

### 1.1 The Current Monomorphization Strategy

The codegen pipeline works in three passes (see `emitter.ml:590-620`):

```ocaml
(* Pass 1: Collect function definitions *)
List.iter (collect_funcs_stmt mono_state) program;

(* Pass 2: Collect instantiations - find call sites *)
ignore (List.fold_left (collect_insts_stmt mono_state) typed_env program);

(* Pass 3: Generate specialized functions *)
InstSet.elements mono_state.instantiations
|> List.map (emit_specialized_func emit_state)
```

**Pass 2 is the critical point.** When it encounters a call to a user-defined function:

```ocaml
(* emitter.ml:203 *)
| AST.Identifier name when is_user_func state name ->
    (* Get concrete types from the arguments *)
    let arg_types = List.map (infer_type env) args in
    let call_type = infer_type env expr in
    let inst = { func_name = name; concrete_types = arg_types; return_type } in
    state.instantiations <- InstSet.add inst state.instantiations
```

This assumes **all arguments have concrete monomorphic types**. The instantiation record stores:

```ocaml
type instantiation = {
  func_name : string;
  concrete_types : Types.mono_type list;  (* must be concrete! *)
  return_type : Types.mono_type;
}
```

### 1.2 Why HOFs Fail

Consider this example:

```marmoset
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) { accumulated }
        else { iter(rest(arr), push(accumulated, f(first(arr)))) }
    };
    iter(arr, [])
};

map([1, 2, 3], fn(x) { x * 2 })
```

When typechecking, `f` is inferred as `∀a. a -> a` (or more generally, `a -> b`). At the call site `f(first(arr))`, the type checker correctly resolves this through unification.

**But at codegen time (Pass 2):**

1. We encounter the call `f(first(arr))`
2. We infer `f`'s type from the environment → `∀a. a -> a` (polymorphic!)
3. We instantiate this to get a concrete type → `t107 -> t107` (fresh type vars)
4. Now we try to create an instantiation: `{ func_name = "f"; concrete_types = [t107]; return_type = t107 }`
5. **Problem**: `f` is not a *defined function* in `mono_state.func_defs`—it's a **parameter**!

When Pass 3 tries to emit this, it looks for `f` in `func_defs` (line 545):

```ocaml
let func_def = List.find (fun fd -> fd.name = inst.func_name) state.mono.func_defs in
```

This **throws an exception** or silently fails because `f` was never collected as a function definition.

### 1.3 The Deeper Issue: Two Different Problem Spaces

The current design treats **all polymorphism as type instantiation**. But there's a critical distinction:

| Scenario | Type | Example | Solution |
|----------|------|---------|----------|
| **Polymorphic data** | `∀a. [a]` | `let id = fn(x) { x }; id(5); id("hi")` | Specialize per concrete type used |
| **Polymorphic function parameter** | `∀a. a -> b` | `let map = fn(arr, f) { ... f(...) ... }` | ? (depends on approach) |

The **first case works**: when we call `id(5)`, we know the concrete type `Int`, so we generate `id_int64`.

The **second case fails**: when we call `map(arr, f)`, we have `f: a -> b` but `a` and `b` are **constrained by the array element type**, not by the call itself. We need to specialize `f`'s **uses**, not just its **definition**.

---

## 2. What Information IS Available

Despite the limitation, the codegen does have considerable type information:

### 2.1 At Type Checking Time

The Hindley-Milner type system (via `infer.ml`) provides:

1. **Polymorphic types for all bindings**: `typed_env : Infer.type_env` contains `∀a. a -> a` for `id`
2. **Substitutions for constraints**: When `id(5)` is called, the inference generates `a := Int`
3. **Use-site concreteness**: At every call site, we know which concrete types are being used
4. **Function parameter types**: The environment tracks that `f` has type `a -> b`

### 2.2 At Codegen Time (Currently Unused)

1. **The `typed_env` itself**: This is threaded through but only used to re-infer expression types. It contains all the polymorphic types.
2. **The full AST with position info**: We know where each expression occurs and can correlate it with type information.
3. **Constraint information from unification**: The type checker discovered constraints that bound type variables together. This is discarded but could be recovered.

### 2.3 What's Missing

1. **Explicit use-site specialization points**: The codegen doesn't have a data structure representing "here, at this call site, these type variables are bound to these types"
2. **Call graph with type information**: No mapping of "function X is called with concrete types Y at locations Z"
3. **Polymorphic specialization requirements**: No explicit representation of "to code this HOF, you need to specialize its parameter functions too"

---

## 3. What Would Be Needed: Design Space

### 3.1 Prerequisite: Phase 2 Consideration

Before implementing HOF monomorphization, we need **type annotations** (Phase 2) for:

```marmoset
let map = fn(arr: [a], f: a -> b) -> [b] { ... }
```

Without explicit signatures:
- Type inference must happen at **definition site** → but parameters are fresh type vars
- No way to know which type variables should be polymorphic vs. which should be concrete
- The current system assumes all type vars are "fresh" and get resolved during inference

**With annotations**, HOF becomes tractable because:
- We know `f: a -> b` is intentionally polymorphic
- We can track which type variables are universally quantified
- We can specialize recursively

### 3.2 Algorithm: Constraint-Based Monomorphization (Medium difficulty)

**Core idea**: Instead of collecting instantiations at call sites, collect **constraints** on type variables, then compute all valid specializations.

```
Phase 1: Collect polymorphic functions with their signatures
  - Extract ∀-quantified type variables
  - Track which type vars appear in function parameters vs. return types

Phase 2: Build constraint graph
  - When we call map([1,2,3], f), add constraints:
    - arr_elem_type = Int
    - f: Int -> b (constrain input, leave output free)
  - When we call result[0], constrain output types

Phase 3: Solve constraints
  - Find all complete solutions (all variables assigned)
  - Each solution = one specialization

Phase 4: Generate code
  - For each specialization, emit a Go function
  - When calling HOF, look up which specialization applies
```

**Pseudocode**:

```ocaml
(* Collect all constraints from a program *)
let rec collect_constraints (expr : AST.expression) (typ : mono_type) : constraint list =
  match (expr.expr, typ) with
  | Call(f, args), TFun(...) ->
      let arg_constraints = List.map2 collect_constraints args arg_types in
      List.concat arg_constraints
  | Identifier name, Forall(quantified_vars, body) when is_polymorphic ->
      (* This identifier is used with type `typ` - constrain its quantified vars *)
      [ Constraint(name, unify body typ) ]
  | _ -> []

(* Solve to find complete specializations *)
let specializations = 
  let all_constraints = collect_constraints program in
  solve_constraints all_constraints
    |> List.filter (fun sol -> all_type_vars_assigned sol)
```

**Feasibility**: **MEDIUM**
- Requires recovering constraint information from type checker output
- Need to enhance the pipeline to thread more metadata
- Solves are NP-complete in general, but for practical programs should be fast
- Existing type systems (Haskell, Rust) do this, so techniques are known

### 3.3 Algorithm: Go 1.18+ Generics (Easy implementation, but requires dep)

**Core idea**: Use Go's native generic functions instead of generating specializations.

Instead of:
```go
func map_int64_int64(arr []int64, f func(int64) int64) []int64 {
    // ...
}
func map_int64_string(arr []int64, f func(int64) string) []string {
    // ...
}
```

Generate:
```go
func map[T any, U any](arr []T, f func(T) U) []U {
    // ...
}
```

**Current runtime code** (emitter.ml:632-690) already uses generics!
```go
func puts[T any](v T) struct{} { ... }
func first[T any](arr []T) T { ... }
```

**But why haven't we used it for user functions?**
- Go generics require runtime monomorphization by the Go compiler
- Marmoset's philosophy is to monomorphize **at compile time** for control and debugging
- Using Go generics undermines the design goal of "transparent, inspectable compilation"

**However**, it's the **fastest implementation path** if we change philosophy.

**Feasibility**: **EASY**
- Minimal codegen changes (remove mangling, add `[T, U any]` syntax)
- Go compiler handles the rest
- Trade-off: lose visibility into generated code, depend on Go 1.18+

### 3.4 Algorithm: Closure Wrapping / Runtime Dispatch (Medium-Hard)

**Core idea**: When a polymorphic function is passed, wrap it in a closure that captures type information or use Go's interface{} for late binding.

```go
// Instead of generating different map_T_U functions:
func map_generic(arr interface{}, f func(interface{}) interface{}) interface{} {
    // Type switch on arr's runtime type
    switch arr.(type) {
    case []int64:
        // Actually call f with int64s
        arr_int := arr.([]int64)
        result := []interface{}{}
        for _, x := range arr_int {
            result = append(result, f(x))
        }
        return result
    // ... more cases
    }
}
```

**Problem**: 
- Loses type safety (all functions become `interface{}`)
- Runtime overhead (type switches, boxing/unboxing)
- Defeats the purpose of Marmoset's HM type system

**Feasibility**: **MEDIUM-HARD**
- Implementation is straightforward (runtime dispatch)
- But philosophically wrong for a statically-typed language
- Would require extensive changes to type -> Go type conversion

### 3.5 Algorithm: Type Classes / Trait-Based (Hard, requires Phase 3)

This is what Marmoset **already planned** (see `docs/typechecker/approach.md`).

Instead of just Hindley-Milner, add trait constraints:

```marmoset
trait Mappable[A, B] {
    map: ([A], fn(A) -> B) -> [B]
}

impl Mappable[Int, Int] {
    map(arr, f) { ... }
}
```

Now monomorphization works because:
- The **trait specifies which polymorphic functions should exist**
- At call time, we look up the trait implementation
- Dispatch is **compile-time** (monomorphization) or **runtime** (interface)

**Feasibility**: **HARD**
- Requires full trait system (Phase 3)
- Need to implement trait resolution, impl blocks, where-clauses
- But this is already in the design roadmap
- Provides other benefits (better code organization, explicit polymorphism)

---

## 4. Comparison of Approaches

| Approach | Feasibility | Performance | Visibility | Phase | Rust-like | Requires Signatures |
|----------|-------------|-------------|------------|-------|-----------|---------------------|
| **Constraint-based mono** | Medium | Excellent | Full | 2 | Yes | Yes (Phase 2) |
| **Go 1.18+ generics** | Easy | Good | Reduced | Current | Partial | No |
| **Closure wrapping** | Medium-Hard | Poor | None | Current | No | No |
| **Type classes/traits** | Hard | Excellent | Full | 3 | Yes | Yes |
| **Do nothing** | N/A | N/A | N/A | N/A | N/A | N/A |

### Detailed Comparison

#### Constraint-Based Monomorphization
**Pros:**
- Complete visibility into what code is generated
- Zero runtime overhead
- Compatible with HM type system
- Enables debugging and inspection
- Sets up foundation for traits (Phase 3)

**Cons:**
- Complex to implement (~1-2 weeks)
- Needs Phase 2 (type annotations) to be clear about intent
- Solvers can be slow on pathological cases
- Requires threaded metadata through codegen

**Best for**: Teams that want **complete control** and don't mind complexity

#### Go 1.18+ Generics
**Pros:**
- Minimum implementation effort (~2-3 days)
- Leverages existing Go infrastructure
- Works with current HM system
- No Phase 2/3 required

**Cons:**
- Black box—can't inspect generated code
- Depends on Go 1.18+
- Loses Marmoset's "transparent compilation" design goal
- Duplicates work the Go compiler already does
- Makes it harder to debug codegen issues

**Best for**: Teams that want **quick MVP** and trust Go compiler

#### Closure Wrapping
**Pros:**
- Works without Phase 2/3
- Minimal codegen changes

**Cons:**
- Type safety lost
- Significant runtime overhead
- Contradicts HM philosophy
- Hard to optimize

**Best for**: Nobody—only use as fallback

#### Type Classes/Traits (Phase 3)
**Pros:**
- Long-term correct solution
- Enables powerful abstractions
- Sets up ecosystem patterns (serialization, iteration, etc.)
- Aligns with Rust/Haskell philosophy

**Cons:**
- Significant implementation effort (~3-4 weeks)
- Requires Phase 3 completion
- Defers HOF support

**Best for**: Long-term product vision

---

## 5. Fit with Current HM Type System

### 5.1 HM's Strengths (Why It Works for Simple Cases)

HM is **excellent** at:
- Inferring concrete instantiations (the `id(5)` case)
- Unifying type variables through constraint solving
- Working without annotations

### 5.2 HM's Gaps (Why It Fails for HOFs)

HM is **inadequate** for:
- **Explicit polymorphism in parameters**: HM invented let-polymorphism *specifically* to avoid requiring annotations. But for HOFs, you need to know *which* type variables should stay polymorphic.
  
  Example: when you write `fn(f) { f(1) }`, is `f` supposed to be:
  - `Int -> a` (monomorphic in first arg, poly in result)?
  - `a -> Int` (poly in first arg, monomorphic in result)?
  - `a -> b` (fully polymorphic)?
  
  **HM has no way to express this without annotations.**

- **Specialization site tracking**: HM tells you "this function has type `∀a. a -> a`", but not "*where* should specializations be generated". That's a codegen problem, not a type problem.

### 5.3 What Phase 2 Adds

Type annotations solve HM's gap:

```marmoset
let map = fn(arr: [a], f: a -> b) -> [b] { ... }
```

Now:
- `a` and `b` are **explicitly quantified** → codegen knows they should stay polymorphic
- The function is **explicit about intent** → easier to debug
- **Constraint collection** becomes straightforward → can follow uses of `a` and `b`

### 5.4 Conclusion: HM is Fine, But Need Annotations

**The HM type system is not the bottleneck.** The bottleneck is that:

1. **Codegen doesn't know which type variables are intentionally polymorphic**
2. **Current monomorphization doesn't track specialization constraints**

Both of these are fixable **within Phase 2** by adding annotations + constraint-based monomorphization. The HM type system will continue to work perfectly—we're just giving it more information.

---

## 6. Which Approach Fits Best?

### 6.1 Recommendation: Constraint-Based Monomorphization (Medium-term)

**For Marmoset's goals**, I recommend:

**Immediate (this week)**: 
- Document the limitation clearly
- Reject HOFs at type check time with a helpful error message
- Example error: "Higher-order function `map` requires type annotations. Add signature: `let map = fn(arr: [a], f: a -> b) -> [b]`"

**Phase 2 (type annotations)**:
- Implement constraint-based monomorphization
- Collect explicit type variables from signatures
- Generate specializations recursively

**Phase 3 (traits)**:
- Add trait system
- Use trait dispatch for complex polymorphism
- Subsumes constraint-based mono for cleaner code

### 6.2 Why Not Go Generics?

Go generics are tempting because they're "easy", but:

1. **Defeats design goal**: Marmoset emphasizes *readable generated code*. Using Go generics hides what actually gets compiled.

2. **Loses optimization opportunities**: Monomorphization lets you specialize eagerly (inline, optimize per type). Go generics monomorphize late.

3. **Dependency problem**: Marmoset claims "compiles to Go". If you rely on Go 1.18+, you're not generating portable code.

4. **Sets bad precedent**: If you punt polymorphism to Go, what about other type system features? This leads to "just use Go directly".

### 6.3 Why Not Type Classes First?

Type classes are the "right" solution architecturally, but they're also 3-4 weeks of work. If you need HOF support sooner:

- Implement constraint-based monomorphization in Phase 2
- It's compatible with traits (Phase 3) and will coexist
- Traits enhance it but don't replace it

---

## 7. Technical Roadmap

### 7.1 Implementation Path: Constraint-Based Mono

**Prerequisites:**
1. Implement Phase 2 (type annotations on functions)
2. Extend `poly_type` representation to track which variables are quantified

**Step 1: Extract specialization constraints (1 day)**

```ocaml
(* New: Constraint representation *)
type specialization_constraint = {
  poly_func : string;
  type_bindings : (string * mono_type) list;  (* "a" -> Int, "b" -> String *)
  call_location : int;
}

(* New: Collect constraints during codegen *)
let rec collect_specialization_constraints 
    (expr : AST.expression) 
    (typ : mono_type) 
    (env : type_env) 
  : specialization_constraint list =
  match expr.expr with
  | Call(func, args) ->
      (* When calling a polymorphic function at a specific type,
         extract the type bindings *)
      let func_poly_type = TypeEnv.find (name_of func) env in
      let Forall(quantified_vars, _) = func_poly_type in
      let actual_type = infer_type env expr in
      let bindings = unify_and_extract_bindings quantified_vars actual_type in
      [{ poly_func = name_of func; type_bindings = bindings; call_location = expr.pos }]
  | _ -> []
```

**Step 2: Compute specializations (1 day)**

```ocaml
(* Compute the set of all concrete specializations needed *)
let compute_specializations (constraints : specialization_constraint list) 
    : (string * (string * mono_type) list) list =
  (* Group by function *)
  constraints
  |> List.fold_left (fun acc c ->
    let specializations = List.assoc c.poly_func acc |> Option.value ~default:[] in
    (c.poly_func, c.type_bindings :: specializations) :: acc
  ) []
  |> List.map (fun (func_name, bindings_list) ->
    (* Deduplicate *)
    (func_name, List.sort_uniq compare (List.concat bindings_list))
  )
```

**Step 3: Emit recursively (2 days)**

```ocaml
(* When emitting a specialized function, recursively specialize
   functions it calls with the bound type variables *)
let emit_specialized_with_bindings 
    (func_name : string) 
    (bindings : (string * mono_type) list)
    (state : emit_state) : string =
  
  let func_def = List.find (fun fd -> fd.name = func_name) state.mono.func_defs in
  
  (* Create a specialized env where type variables are bound *)
  let spec_env = 
    List.fold_left (fun env (var_name, typ) ->
      Infer.TypeEnv.add var_name (Types.mono_to_poly typ) env
    ) (base_env) bindings
  in
  
  (* Emit body with specialized types *)
  let mangled_name = mangle_func_name func_name bindings in
  let body_str = emit_func_body state spec_env func_def.body in
  Printf.sprintf "func %s(...) ... { %s }" mangled_name body_str
```

**Step 4: Update Pass 2 to use constraints (1 day)**

```ocaml
(* Replace the simple instantiation collection with constraint collection *)
let emit_program_with_typed_env typed_env program =
  let mono_state = create_mono_state () in
  
  (* Pass 1: collect function defs *)
  List.iter (collect_funcs_stmt mono_state) program;
  
  (* Pass 2: collect specialization CONSTRAINTS (NEW) *)
  let constraints = collect_specialization_constraints_program typed_env program in
  
  (* New step: compute specializations from constraints *)
  let specializations = compute_specializations constraints in
  
  (* Pass 3: generate code for each specialization *)
  let specialized_funcs = 
    List.map (fun (func_name, bindings) ->
      emit_specialized_with_bindings func_name bindings emit_state
    ) specializations
  in
  
  ...
```

**Estimated effort**: 4-5 days for a working implementation

### 7.2 Minimal Viable Phase 2

To enable the above, Phase 2 needs:

```marmoset
(* Type annotation syntax *)
let map = fn(arr: [a], f: a -> b) -> [b] { ... }

(* Parser change: allow type annotations in function params *)
(* Typechecker change: use annotations to seed type variables *)
```

This is much simpler than full Phase 2 (no trait system), ~1-2 days.

---

## 8. Conclusion: Is Phase 2/3 Required?

### For Constraint-Based Monomorphization: **Phase 2 only (minimal)**
- Need explicit type annotations on polymorphic functions
- Don't need full trait system
- ~6-7 days total (2 for Phase 2 + 4-5 for mono)

### For Type Classes/Traits: **Phase 3**
- Full trait system required
- More powerful but also more complex
- ~4-6 weeks

### For Go Generics: **None**
- Works with current system
- But contradicts design goals
- ~3 days

**My recommendation**: Implement **minimal Phase 2** (signatures only, no traits) + **constraint-based monomorphization** as the next codegen milestone. This gives you HOF support while staying true to the "transparent compilation" philosophy.

---

## Appendix: Example Walkthrough

### The `map` Function

```marmoset
let map = fn(arr: [a], f: a -> b) -> [b] {
    let iter = fn(accumulated: [b], remaining: [a]) -> [b] {
        if (len(remaining) == 0) { 
            accumulated
        } else { 
            iter(
                push(accumulated, f(first(remaining))),
                rest(remaining)
            )
        }
    };
    iter([], arr)
};

map([1, 2, 3], fn(x) { x * 2 })
map(["a", "b"], fn(s) { len(s) })
```

### Type Checking Phase

```
TypeEnv:
  map: ∀a b. ([a], a -> b) -> [b]

Infer map([1, 2, 3], fn(x) { x * 2 }):
  arr: [Int]
  f: Int -> Int
  result: [Int]
  
  Constraint discovered: a := Int, b := Int

Infer map(["a", "b"], fn(s) { len(s) }):
  arr: [String]
  f: String -> Int
  result: [Int]
  
  Constraint discovered: a := String, b := Int
```

### Codegen Phase (Current - Fails)

```
Pass 2 - Collect instantiations:
  - Call to map([1, 2, 3], fn(x) { x * 2 })
    - Concrete types: [[Int], (Int -> Int)]
    - Creates inst: { func_name = "map"; concrete_types = [[Int], (Int -> Int)] }

  - Call to iter(...) inside map's body
    - Problem: parameter `f` has type (Int -> Int) but...
    - No, wait, parameter bindings are lost!
    - The typed_env is at the *program* level, not the *parameter* level

Pass 3 - Emit:
  - Tries to find "map" in func_defs → FOUND
  - Tries to emit map_list_int64_fn_int64_int64(...)
  - ERROR: can't emit function type with polymorphic parameter
```

### Codegen Phase (Proposed - Works)

```
Pass 2 - Collect specialization constraints:
  - Call to map([1, 2, 3], ...)
    - Function type: ∀a b. [a] -> (a -> b) -> [b]
    - Actual types: [Int] -> (Int -> Int) -> [Int]
    - Constraint: { map, [(a, Int), (b, Int)] }

  - Call to map(["a", "b"], ...)
    - Constraint: { map, [(a, String), (b, Int)] }

  - Call to iter(accumulated, remaining) inside map
    - Function type: ∀b a. [b] -> [a] -> [b]  (parameters flipped in my syntax)
    - But with bindings from outer context: a := Int, b := Int
    - Actual types: [Int] -> [Int] -> [Int]
    - Constraint: { iter, [(a, Int), (b, Int)] }

Specialization set:
  - map[(a, Int), (b, Int)]
  - map[(a, String), (b, Int)]
  - iter[(a, Int), (b, Int)]

Pass 3 - Emit:
  - func map_a_Int_b_Int(arr []int64, f func(int64) int64) []int64 { ... }
  - func map_a_String_b_Int(arr []string, f func(string) int64) []int64 { ... }
  - func iter_b_Int_a_Int(accumulated []int64, remaining []int64) []int64 { ... }
```

---

## References

1. **Current code**: `lib/backend/go/emitter.ml` (monomorphization strategy)
2. **Type system**: `lib/frontend/typecheck/infer.ml` and `types.ml` (HM implementation)
3. **Design docs**: `docs/codegen/backend-architecture.md` (current limitations)
4. **Future vision**: `docs/typechecker/approach.md` (Phase 3 traits design)

---

**Analysis by**: Claude Code
**Date**: 2026-02-03
**Status**: Comprehensive design document ready for implementation planning
