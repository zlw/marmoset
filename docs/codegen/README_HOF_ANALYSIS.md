# Higher-Order Function Monomorphization Analysis

Complete investigation of why polymorphic HOFs fail in the Go backend and four implementation approaches to fix it.

## Documents

### 1. **HOF_SUMMARY.md** (Start here - 6 KB)
**Executive summary** - Best for quick understanding
- Problem statement with code examples
- Root cause in one page
- Four approaches at a glance
- Which to choose and why

**Read time**: 5-10 minutes

---

### 2. **ANALYSIS_HOF_MONOMORPHIZATION.md** (Deep dive - 24 KB)
**Comprehensive technical analysis** - For implementation planning
- Complete root cause analysis with code walkthrough
- What information IS available at codegen time
- Prerequisite: Why Phase 2 type annotations needed
- Five approaches with:
  - Algorithm pseudocode
  - Feasibility assessment
  - Performance implications
  - Fit with HM type system
- Technical roadmap for each approach
- Complete example walkthrough with `map` function

**Read time**: 30-45 minutes
**For**: Architects, lead engineers planning implementation

---

### 3. **APPROACH_COMPARISON.md** (Decision framework - 14 KB)
**Detailed comparison matrix** - For choosing which approach
- Overview table comparing all 4 approaches
- Detailed breakdown of each:
  - How it works (pseudocode + Go output)
  - Phase requirements
  - Implementation effort
  - Generated code examples
  - Pros/cons lists
- Decision matrix by timeline/philosophy/tech
- Implementation effort comparison
- Recommended path forward

**Read time**: 20-30 minutes
**For**: Technical leads, decision makers

---

## Quick Reference

### The Problem
```marmoset
let map = fn(arr, f) {
    if (len(arr) == 0) { [] }
    else { [f(first(arr))] + map(rest(arr), f) }
};

map([1, 2, 3], fn(x) { x * 2 })    // Works in HM, fails in codegen
```

**Error**: Generated Go references undefined type variables (`T107`, `UNKNOWN`)

### Root Cause
Current monomorphization only works at **call sites with concrete types**. When `f` has type `a -> b` (polymorphic), there's no concrete type to instantiate. The system assumes all functions are defined, but `f` is a **parameter**.

### Solutions (Best to Fastest)

| Approach | Effort | Timeline | Philosophy | Recommendation |
|----------|--------|----------|-----------|-----------------|
| **Constraint-based mono** | Medium | 5 days | ✓ Keeps transparency | **BEST** |
| **Go 1.18+ generics** | Easy | 3 days | ✗ Black box | Fast MVP |
| **Closure wrapping** | Medium-Hard | 4 days | ✗ Type-unsafe | Avoid |
| **Phase 3 traits** | Hard | 4-6 weeks | ✓ Long-term | Ecosystem |

### Recommendation

**If you can wait 5 days**: Implement **constraint-based monomorphization**
- Minimal Phase 2 (just type annotations, ~2 days)
- Enhanced monomorphization (~4-5 days total)
- Keeps "transparent compilation" design goal
- Sets up Phase 3 correctly
- Full HOF support with zero runtime overhead

**If you need it now (3 days)**: Use **Go 1.18+ generics**
- Minimal codegen changes
- Trade: lose visibility into generated code
- Dependency: Go 1.18+

## Key Findings

1. **HM type system is sufficient** ✓
   - No type theory gaps
   - The limitation is entirely in codegen strategy
   - Not a "this is impossible" problem

2. **Phase 2 type annotations are prerequisite** (if using constraint approach)
   ```marmoset
   let map = fn(arr: [a], f: a -> b) -> [b] { ... }
   ```
   This is MINIMAL Phase 2 (no traits needed yet)

3. **Four implementation approaches exist**
   - All are feasible
   - Trade-offs are: effort vs. complexity vs. philosophy vs. visibility

4. **Constraint-based mono fits best**
   - Aligns with "transparent compilation" goal
   - Enables Phase 3 traits when ready
   - Zero runtime overhead
   - Practical implementation (~5 days)

## Timeline Options

### Option A: Slow and Right (Recommended)
```
Week 1:
  - Day 1-2: Phase 2 (type annotations)
  - Day 3-5: Constraint-based monomorphization
  
Result: HOF works, full transparency, sets up Phase 3
```

### Option B: Fast and Trade-offs
```
Day 1-3: Switch to Go 1.18+ generics

Result: HOF works in 3 days, but lose visibility
```

### Option C: Long-term
```
Later (Phase 3): Full trait system (4-6 weeks)

Result: HOF + whole polymorphic ecosystem
```

## Files Referenced

- **Current code**: `lib/backend/go/emitter.ml` - Monomorphization strategy (lines 590-620 show Pass 1-3)
- **Type system**: `lib/frontend/typecheck/infer.ml` - HM type inference
- **Design docs**: `docs/codegen/backend-architecture.md` - Known limitations section
- **Future vision**: `docs/typechecker/approach.md` - Phase 3 trait design

## What's in Each Analysis

### HOF_SUMMARY.md
- Problem overview
- Root cause (one page)
- Four approaches (table)
- Detailed breakdown of recommended approach
- Phase requirements
- Feasibility assessment
- Conclusion and next steps

### ANALYSIS_HOF_MONOMORPHIZATION.md
- Root cause analysis (detailed walkthrough)
- Available information at codegen time
- Four algorithm approaches with pseudocode:
  1. Constraint-based monomorphization
  2. Go 1.18+ generics
  3. Closure wrapping/runtime dispatch
  4. Type classes/traits
- Comparison table
- HM type system compatibility
- Implementation roadmap
- Example walkthrough with `map` function

### APPROACH_COMPARISON.md
- Overview table (4 approaches)
- Detailed breakdown of each:
  - How it works
  - Phase requirements
  - Implementation details
  - Pros/cons
  - Generated code examples
  - When to use
- Decision matrix by timeline/philosophy
- Implementation effort comparison
- Recommendation summary

## How to Use

**1. Quick decision (10 min)**
→ Read `HOF_SUMMARY.md`

**2. Understanding the problem deeply (30 min)**
→ Read `ANALYSIS_HOF_MONOMORPHIZATION.md` sections 1-2

**3. Choosing implementation approach (20 min)**
→ Read `APPROACH_COMPARISON.md` decision matrix + your choice's section

**4. Planning implementation (1-2 hours)**
→ Read full `ANALYSIS_HOF_MONOMORPHIZATION.md` section 7 (roadmap)

## Contact Points

These analyses identify HOF support as a **design choice**, not a blocker:
- **Not impossible**: Multiple proven approaches
- **Not urgent**: Affects advanced polymorphic code, not MVP
- **Not complex**: Medium difficulty, clear path forward
- **Aligns with vision**: Phase 2 + constraint-based sets up Phase 3

The HM type system is ready. It's the **codegen strategy** that needs enhancement.

---

**Created**: 2026-02-03
**Status**: Ready for implementation planning
**Next step**: Choose approach and create implementation task
