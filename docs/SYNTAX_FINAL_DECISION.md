# Syntax Final Decision: Hybrid Rust-Scala Approach

**Date**: 2026-02-03  
**Status**: LOCKED - No more syntax discussions  
**Decision**: Keep current syntax with strategic tooling approach

---

## Decision Summary

**Chosen**: Current Marmoset syntax (Scala 3 style with `[a]` for generics)

**Not chosen**: Full Rust syntax (`<a>` generics)

**Reasoning**: 
1. Syntax highlighting reusability (critical for IDE adoption)
2. Readability is 8/10 vs 9/10 (marginal difference)
3. Saves 4-6 hours of parser work
4. Easier IDE marketplace integration

---

## Why Not Full Rust Syntax?

**Cost-Benefit Analysis**:

| Factor | Full Rust | Current Marmoset |
|--------|-----------|------------------|
| Parser effort | 4-6 hours | 0 hours |
| Syntax clarity | 9/10 (where clauses) | 8/10 (brackets) |
| IDE highlighting | TreeSitter from scratch | Reuse Rust grammar |
| IDE marketplace entry | 2-4 weeks | 1-2 weeks |
| Developer familiarity | High (Rust devs) | Medium (FP devs) |
| Long-term maintenance | TreeSitter upkeep | Minimal |

**Verdict**: The IDE highlighting cost outweighs the syntax clarity gain.

---

## Final Marmoset Syntax (LOCKED)

### Functions & Generics
```marmoset
fn add(x: int, y: int) -> int { x + y }
fn id[a](x: a) -> a { x }
fn process[a: show + eq](x: a) { ... }
fn map_with_error[a, b](
    arr: list[a],
    f: fn(a) => result[b, error]
) -> result[list[b], error] { ... }
```

### Variables & Types
```marmoset
let x: int = 5
let arr: list[int] = [1, 2, 3]
let map: map[string, int] = {"a": 1}
let opt: option[string] = none
let res: result[user, error] = ok(user)
```

### Control Flow
```marmoset
if (x > 0) { x } else { -x }
match value {
    case ok(v) => v
    case err(e) => handle(e)
}
```

### Traits & Impl (Phase 3)
```marmoset
trait show {
    show: (self) -> string
}

impl show for int {
    show(self) -> string { to_string(self) }
}
```

### Enums (Phase 3)
```marmoset
enum option[a] {
    some(a)
    none
}

enum result[a, e] {
    ok(a)
    err(e)
}

enum user_error {
    not_found
    validation_failed(string)
    permission_denied
}
```

### Effects (Phase 5)
```marmoset
fn pure_fn(x: int) -> int { x + 1 }
fn impure_fn(x: int) => int { puts(x); x }
```

---

## Syntax Highlighting Strategy: Hybrid Approach

### Phase 2: Immediate (Start of Phase 2)
- **Reuse Rust TreeSitter grammar** (MIT licensed)
- Override generics rule: `[a]` instead of `<T>`
- Override constraint rule: `+` instead of `::`
- Generate Marmoset grammar from modified Rust grammar

**Implementation**:
1. Take Rust TreeSitter grammar (`tree-sitter-rust`)
2. Create Marmoset fork
3. Modify ~20 lines of grammar rules
4. Generate syntax highlighting for all major editors

**Effort**: 8-12 hours
**Reusable**: Yes - maintain grammar, updates to Rust grammar auto-flow

### Phase 3+: Polish
- Submit grammar to editor marketplaces (VS Code, Vim, Neovim)
- Add Marmoset-specific themes
- Ensure proper indent/bracket highlighting

---

## IDE Integration Plan

### Phase 2
- [ ] Fork `tree-sitter-rust` to `tree-sitter-marmoset`
- [ ] Modify grammar rules for Marmoset syntax
- [ ] Test with VS Code Tree-Sitter extension
- [ ] Generate highlighting for: VS Code, Vim, Neovim

### Phase 3+
- [ ] Create VS Code extension
- [ ] Create Vim/Neovim plugin
- [ ] Submit to extension marketplaces
- [ ] Document editor setup

---

## Why This Works

**Hybrid approach lets us**:
1. ✅ Keep clean syntax (`[a]` brackets)
2. ✅ Reuse battle-tested Rust grammar (instead of writing from scratch)
3. ✅ Get syntax highlighting in all major editors within weeks
4. ✅ Reduce maintenance burden (automatic Rust updates flow through)
5. ✅ Still have "Rust-like" story ("Rust syntax on Go runtime")

**Marketing pitch remains valid**:
> "Rust's powerful type system, running on Go's fast runtime. Clean syntax inspired by Scala 3 and Haskell."

---

## Syntax Locked - No More Changes

This decision is FINAL. Any future syntax changes require:
1. Explicit approval from project lead
2. Time estimate for parser + tests + highlighting
3. Documented justification (not just "I like this better")
4. Update to this document

---

## Decision Record

| Item | Decision |
|------|----------|
| Generics syntax | `[a]` (square brackets, Scala 3 style) |
| Constraint syntax | `[a: show + eq]` (with `+`) |
| Effect marker | `->` (pure) vs `=>` (impure) |
| Function keyword | `fn` (not `func`) |
| Variable binding | `let x = value` |
| Type annotation | `x: type` (colon after name) |
| IDE strategy | Reuse/modify Rust TreeSitter grammar |
| Why not full Rust | IDE highlighting complexity outweighs marginal readability gain |

