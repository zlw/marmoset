# Detailed Approach Comparison Matrix

## Overview Table

```
┌─────────────────────────┬────────────┬──────────────┬──────────┬────────┬────────────┐
│ Approach                │ Difficulty │ Performance  │ Debuggability │ Phase │ Timeline   │
├─────────────────────────┼────────────┼──────────────┼──────────┼────────┼────────────┤
│ Constraint-Based Mono   │ MEDIUM     │ EXCELLENT ⭐⭐⭐│ EXCELLENT ⭐⭐⭐│ Phase 2 │ 5 days    │
│ Go 1.18+ Generics       │ EASY ⭐    │ GOOD ⭐⭐   │ POOR ⭐    │ Now     │ 3 days    │
│ Closure Wrapping        │ MEDIUM-H   │ POOR ⭐     │ POOR ⭐    │ Now     │ 4 days    │
│ Type Classes (Phase 3)  │ HARD       │ EXCELLENT ⭐⭐⭐│ EXCELLENT ⭐⭐⭐│ Phase 3 │ 4-6 weeks │
└─────────────────────────┴────────────┴──────────────┴──────────┴────────┴────────────┘
```

## Detailed Comparison

### 1. CONSTRAINT-BASED MONOMORPHIZATION ⭐ RECOMMENDED

#### How It Works
```
Input:  let map = fn(arr: [a], f: a -> b) -> [b] { ... }
        map([1,2,3], fn(x) { x*2 })
        map(["a","b"], fn(s) { len(s) })

Step 1: Collect constraints at call sites
        map([1,2,3], ...) → {a: Int, b: Int}
        map(["a","b"], ...) → {a: String, b: Int}

Step 2: Compute specializations needed
        - map_Int_Int
        - map_String_Int

Step 3: Emit specialized functions
        func map_Int_Int(arr []int64, f func(int64) int64) []int64 { ... }
        func map_String_Int(arr []string, f func(string) int64) []int64 { ... }

Output: Full Go code with proper specializations
```

#### Phase 2 Requirement: MINIMAL
```marmoset
(* Annotations only - no trait system needed *)
let map = fn(arr: [a], f: a -> b) -> [b] {
    ...
}
```

**Parser changes**: ~100 lines (parse type annotations in params)
**Typechecker changes**: ~200 lines (use annotations to seed type vars)
**Effort**: 2 days

#### Monomorphization Enhancement: ~4-5 days
```ocaml
(* Add to monomorphization pipeline *)
1. collect_specialization_constraints (extract {a: Int, b: Int} from calls)
2. compute_specializations (deduplicate, solve)
3. emit_with_bindings (recursively emit specialized functions)
```

#### Pros ✓
- **Full visibility**: Can inspect generated code
- **Zero overhead**: All monomorphization at compile time
- **Debuggable**: Each specialization is a real Go function you can trace
- **Composable**: Works with Phase 3 traits later
- **Ecosystem ready**: Sets pattern for Serialize, Show, etc. traits
- **Philosophy aligned**: "Transparent, readable Go"
- **Portable**: No Go version dependency

#### Cons ✗
- Requires Phase 2 (but minimal)
- More complex implementation than Go generics
- Constraint solver can be slow on pathological cases (rare in practice)

#### Example Generated Code
```go
// Specialized for Int -> Int
func map_int64_int64(arr []int64, f func(int64) int64) []int64 {
    // Can inspect and understand every line
    result := []int64{}
    for _, x := range arr {
        result = append(result, f(x))
    }
    return result
}

// Specialized for String -> Int
func map_string_int64(arr []string, f func(string) int64) []int64 {
    result := []int64{}
    for _, s := range arr {
        result = append(result, f(s))
    }
    return result
}
```

#### When to Use
- Want to keep "transparent compilation"
- Can wait 5 days
- Value debuggability over speed-to-MVP
- Planning Phase 3 anyway

---

### 2. GO 1.18+ GENERICS (Fastest Implementation)

#### How It Works
```
Input:  let map = fn(arr, f) { ... f(...) ... }
        (no annotations needed)

Codegen: Instead of mangling names, emit Go generic syntax
        func map[T any, U any](arr []T, f func(T) U) []U { ... }

Go compiler: Monomorphizes for each use site
            map([1,2,3], ...) → Go creates map_int64_int64
            map(["a","b"], ...) → Go creates map_string_int64

Output: Compiled binary with specialized code
```

#### Phase 2 Requirement: NONE
- Works with current type system
- No annotations needed

#### Implementation: ~3 days
```ocaml
(* Minimal codegen changes *)
1. Detect polymorphic functions (type vars in signature)
2. Instead of mangling: emit [T any, U any] syntax
3. Replace concrete types with type variables in function body
4. Done—Go compiler handles the rest
```

#### Current Runtime Already Uses This!
Look at `emitter.ml:639-690`:
```go
func puts[T any](v T) struct{} { ... }
func first[T any](arr []T) T { ... }
```

This proves the infrastructure already exists. We could extend it to user functions easily.

#### Pros ✓
- **Fastest implementation**: 3 days
- **No Phase 2 required**: Works now
- **Proven technology**: Go 1.18+ is stable
- **Minimal codegen changes**: Reuse existing generic syntax

#### Cons ✗
- **Black box**: Can't inspect what Go generated
- **Invisible specializations**: Bug in Go generics = hard to debug
- **Go version lock**: Requires Go 1.18+ (breaks "generated Go")
- **Philosophy violation**: "Transparent compilation" lost
- **Duplicates work**: We already do monomorphization, Go does it again
- **Harder to optimize**: Can't apply Marmoset-specific optimizations

#### Example Generated Code
```go
// What you see in generated code
func map[T any, U any](arr []T, f func(T) U) []U {
    result := []U{}
    for _, x := range arr {
        result = append(result, f(x))
    }
    return result
}

// What Go compiler generates (invisible to you)
// map_int64_int64(arr []int64, f func(int64) int64) []int64
// map_string_int64(arr []string, f func(string) int64) []int64
```

#### When to Use
- Need MVP in 3 days
- Team comfortable with Go 1.18+
- OK with black-box compilation
- Not planning complex polymorphic ecosystem

#### Risk Assessment
- **Go version**: Will Go 1.18+ be default in 2-3 years? Probably.
- **Performance**: Go's generic monomorphization is standard, not "worse"
- **Integration**: If Marmoset adds traits later, they'll conflict with Go generics

---

### 3. CLOSURE WRAPPING / RUNTIME DISPATCH (Avoid)

#### How It Works
```
Input:  let map = fn(arr, f) { ... }

Codegen: Emit generic function with runtime dispatch
        func map_generic(arr interface{}, f func(interface{}) interface{}) interface{} {
            switch arr := arr.(type) {
            case []int64:
                // Actually call f with int64s
                ...
            case []string:
                ...
            }
        }

Output: One function that handles all types via runtime switches
```

#### Phase 2 Requirement: NONE
- Works with current system
- No annotations

#### Implementation: ~3-4 days
- All functions become `interface{}`
- Add type switches for concrete operations
- Boxing/unboxing wrappers

#### Pros ✓
- No Phase 2 needed
- Reasonable implementation speed

#### Cons ✗ (Major)
- **Type safety lost**: Everything is `interface{}`
- **Runtime overhead**: Type switches + boxing/unboxing
- **Contradicts HM**: Why use HM if you throw away types?
- **Ugly generated code**: Unreadable Go
- **Performance**: Measurably slower
- **Unmaintainable**: Hard to debug type errors

#### Example Generated Code
```go
func map_generic(arr interface{}, f func(interface{}) interface{}) interface{} {
    switch arr := arr.(type) {
    case []int64:
        arr_int := arr.([]int64)
        result := []interface{}{}
        for _, x := range arr_int {
            // Boxing: wrap int64 in interface{}
            result = append(result, f(interface{}(x)))
        }
        // Unboxing: restore original type
        return result
    
    case []string:
        arr_str := arr.([]string)
        result := []interface{}{}
        for _, s := range arr_str {
            result = append(result, f(interface{}(s)))
        }
        return result
    
    default:
        panic("map: unsupported array type")
    }
}
```

#### When to Use
**Don't.** Only if you literally have no other option. Cons vastly outweigh pros.

---

### 4. TYPE CLASSES / TRAIT SYSTEM (Phase 3)

#### How It Works
```
Define trait with polymorphic parameter:
trait Mappable[A, B] {
    map: ([A], fn(A) -> B) -> [B]
}

Implement for specific types:
impl Mappable[Int, Int] {
    map(arr, f) { ... (monomorphized) ... }
}

Call with dispatch:
let result: [Int] = map([1,2,3], fn(x) { x*2 })

Compiler: Looks up Mappable[Int, Int] impl → calls specialized version
```

#### Phase 3 Requirement: FULL
- Trait definitions
- Impl blocks
- Where-clauses
- Trait resolution algorithm
- Dispatch (static or dynamic)

#### Implementation: ~4-6 weeks
```ocaml
1. Parser: trait/impl syntax (~3 days)
2. Typechecker: trait resolution + where-clauses (~1 week)
3. Codegen: impl lookup + dispatch (~3-5 days)
4. Testing/refinement (~1 week)
```

#### Pros ✓
- **Most powerful**: Enables whole polymorphism ecosystem
- **Explicit intent**: Code documents which types implement which traits
- **Scales well**: Serialization, Show, etc. traits defined in libraries
- **Industry proven**: Rust, Haskell, Scala all use this
- **Zero overhead**: Static dispatch by default
- **Dynamic option**: Can use `dyn Trait` for runtime dispatch

#### Cons ✗
- **Significant effort**: 4-6 weeks
- **Defers HOF support**: Can't use until Phase 3 complete
- **Learning curve**: Team needs to understand trait resolution

#### Example Generated Code
```go
// Trait implementation for Int
func (i Int) Map(f func(Int) Int) []Int {
    // Concrete, monomorphized implementation
    result := []Int{}
    for _, x := range i {
        result = append(result, f(x))
    }
    return result
}

// Trait implementation for String
func (s String) Map(f func(String) Int) []Int {
    result := []Int{}
    for _, x := range s {
        result = append(result, f(x))
    }
    return result
}

// Call site: Compiler resolves Mappable[String, Int] → calls String.Map
```

#### When to Use
- Planning comprehensive polymorphic system
- Want library ecosystem (Serialize, Show, Iterate, etc.)
- Can wait 4-6 weeks for foundational feature
- Value long-term architecture over short-term MVP

---

## Decision Matrix: Which Should We Choose?

### Timeline Axis

**Need HOF now (within 1 week)?**
→ Use **Go generics** (3 days, accept visibility loss)

**Can invest 1 week?**
→ Use **Constraint-based mono** (5 days total, including minimal Phase 2)

**Planning longer roadmap?**
→ Use **Phase 3 traits** (4-6 weeks, comprehensive solution)

### Philosophy Axis

**"Transparent, readable Go" is core design goal?**
→ **Constraint-based mono** (keeps inspection capability)

**"Quick to market" is priority?**
→ **Go generics** (easiest implementation)

**"Full polymorphic ecosystem" for library-scale code?**
→ **Phase 3 traits** (enables patterns across codebase)

### Technical Axis

**HM type system sufficient?**
→ Yes ✓ (all approaches work)

**Need Phase 2 annotations?**
→ Only Constraint-based (minimal version)

**Go version dependency acceptable?**
→ If not: Constraint-based or Traits
→ If yes: Go generics

---

## Recommendation Summary

### SHORT-TERM (Next Sprint)

**Pick ONE:**

1. **If speed matters**: Go 1.18+ generics
   - Timeline: 3 days
   - Trade-off: Lose transparency
   - Risk: Go version lock

2. **If philosophy matters**: Constraint-based mono
   - Timeline: 5 days (2 Phase 2 + 3 mono)
   - Trade-off: More complex
   - Benefit: Sets up Phase 3

### MEDIUM-TERM (Next 2-3 months)

**Recommendation**: Implement Constraint-based mono even if you chose Go generics
- Proves the approach works
- Builds infrastructure for Phase 3
- Provides comparison point

### LONG-TERM (3-6 months)

**Plan Phase 3 traits**
- Enables ecosystem (Serialize, Show, Eq, Ord, etc.)
- Aligns with industry standards
- Makes Marmoset a serious language

---

## Implementation Effort Comparison

```
Constraint-Based Mono:
├─ Phase 2 (minimal, annotations only)
│  ├─ Parser: 100 lines, 1 day
│  ├─ Typechecker: 200 lines, 1 day
│  └─ Tests: 50 lines, 2 hours
└─ Monomorphization
   ├─ Constraint collection: 150 lines, 1 day
   ├─ Specialization solving: 200 lines, 1 day
   ├─ Recursive emission: 150 lines, 1 day
   ├─ Integration: 100 lines, 1 day
   └─ Tests/refinement: 300 lines, 1 day
Total: ~5 days, ~1200 lines

Go Generics:
├─ Codegen changes: 50-100 lines, 1 day
├─ Type representation: 50 lines, 1 day
├─ Integration: 50 lines, 1 day
└─ Testing: 100 lines, minimal
Total: ~3 days, ~250 lines

Closure Wrapping:
├─ Runtime dispatch generation: 300 lines, 2 days
├─ Type switch generation: 200 lines, 1 day
├─ Boxing/unboxing wrappers: 150 lines, 1 day
└─ Testing: 200 lines, 1 day
Total: ~4 days, ~850 lines (but poor quality)

Phase 3 Traits:
├─ Parser (trait/impl syntax): ~800 lines, 3 days
├─ Typechecker (resolution): ~1500 lines, 1 week
├─ Codegen (dispatch): ~600 lines, 3 days
└─ Testing/integration: ~1000 lines, 1 week
Total: ~4-6 weeks, ~4000 lines
```

---

## Conclusion

**Recommended path for Marmoset**:

1. **Immediate** (this sprint): 
   - Understand the problem (✓ done via this analysis)
   - Decide on philosophy vs. speed trade-off
   - Document limitation clearly for users

2. **Phase 2** (if choosing constraint-based):
   - Implement minimal annotations
   - Set up constraint infrastructure
   - Enable HOF support

3. **Phase 3** (always):
   - Plan full trait system
   - Enables whole ecosystem
   - Makes Marmoset first-class polymorphic language

**If forced to choose now**: Constraint-based monomorphization aligns best with Marmoset's stated philosophy of "transparent, readable generated Go" while enabling the full polymorphic type system you're already building.
