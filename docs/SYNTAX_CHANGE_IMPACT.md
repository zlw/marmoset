# Syntax Change Impact Analysis

**Purpose**: Show effort/scope required to change syntax at different levels before Phase 2 implementation starts.

**Current State**:
- Lexer: 242 lines
- Parser: 750 lines  
- AST: 141 lines
- Tokens: 61 lines
- Tests: 16 codegen tests + 128+ type checker tests

---

## Scenario 1: "Go++" Syntax (Attract Go Crowd)

### Goal
Make Marmoset look more like Go to attract Go developers.

### Changes
```marmoset
// Current Marmoset
fn add(x: int, y: int) -> int { x + y }
let result = add(5, 3)

// Go++ style
func add(x: int, y: int) int { x + y }
var result = add(5, 3)

// Or: keep fn but use Go-style type placement
fn add(x int, y int) int { x + y }  // Go puts type after param name
```

### Impact

| Component | Change | Effort | Time |
|-----------|--------|--------|------|
| **Lexer** | Add `func`, `var` keywords | 10-15 lines | 30 min |
| **Tokens** | No change (same tokens) | 0 lines | 0 |
| **Parser** | 1. Change function parsing rules<br>2. Change param type syntax<br>3. Update type annotation rules | 100-150 lines | 2-3 hours |
| **AST** | Minimal (same structure) | 0 lines | 0 |
| **Tests** | Update all 140+ tests to new syntax | - | 2-3 hours |
| **Codegen** | No change (uses internal AST) | 0 lines | 0 |

**Total Effort**: 4-6 hours + regression testing

**Risk**: Medium (straightforward changes, but many tests to update)

### Rollback: Easy
- Revert lexer/parser, update tests back
- All functionality identical

---

## Scenario 2: Whitespace-Sensitive (Python/Scala 3 Style)

### Goal
Drop braces, use indentation for blocks.

### Changes
```marmoset
// Current Marmoset
fn add(x: int, y: int) -> int {
  x + y
}

if (x > 0) {
  puts(x)
} else {
  puts(-x)
}

// Whitespace style
fn add(x: int, y: int) -> int:
  x + y

if x > 0:
  puts(x)
else:
  puts(-x)
```

### Impact

| Component | Change | Effort | Time |
|-----------|--------|--------|------|
| **Lexer** | 1. Track indentation levels<br>2. Generate INDENT/DEDENT tokens<br>3. Remove brace/semicolon handling<br>4. Handle newlines differently | 150-200 lines rewrite | 3-4 hours |
| **Tokens** | Add INDENT, DEDENT, NEWLINE tokens | 5-10 lines | 30 min |
| **Parser** | 1. Replace brace-based block parsing<br>2. Use INDENT/DEDENT instead<br>3. Update all 20+ parse_*_block functions<br>4. Handle error recovery differently | 200-300 lines rewrite | 4-6 hours |
| **AST** | No structural change (same) | 0 lines | 0 |
| **Tests** | Rewrite all tests (syntax completely different) | - | 3-4 hours |
| **Codegen** | No change | 0 lines | 0 |

**Total Effort**: 10-16 hours + regression testing

**Risk**: High (affects lexer fundamentally, lots of edge cases with indentation)

**Complexity**: 
- Indentation-based parsing is error-prone (Python has decades of experience, still has weird cases)
- Error messages become harder (what indentation level did you expect?)
- IDE integration harder (need smart indentation)

### Rollback: Very hard
- Can't just revert — breaks all tests, examples, documentation
- Need to maintain both syntaxes if supporting legacy code

---

## Scenario 3: Hybrid Approach (Recommended if you want to change)

Keep braces but change other syntax.

### Example: Mix Go-style + current style
```marmoset
// Hybrid: Go param types, keep braces and fn keyword
fn add(x: int, y: int) -> int {
  x + y
}

// Or: Keep most current, just change fn to func
func add(x: int, y: int) -> int {
  x + y
}
```

### Impact
- **Lexer**: Add 1-2 keywords (~30 lines)
- **Parser**: Modify param parsing (~50 lines)
- **Total**: 3-4 hours

---

## Timing & Strategy

### IMPORTANT: When Should This Decision Happen?

**Option A: NOW (before Phase 2)**
- Pros: Clean slate, don't build on "wrong" syntax
- Cons: Adds 1-2 weeks to timeline, delays Phase 2
- Recommendation: ONLY if you're certain about the direction

**Option B: After Phase 2 (before Phase 3)**
- Pros: Phase 2 sets foundation, you see how it feels
- Cons: Might need to redo Phase 2 parser changes
- Recommendation: Better — you'll know if syntax is good

**Option C: After Phase 3**
- Pros: Full language implemented, can evaluate entire syntax
- Cons: Most work wasted if you change everything
- Recommendation: Too late, would require major refactor

---

## My Recommendation

**LOCK THE SYNTAX NOW** for one simple reason:

You're about to invest 5-7 days in Phase 2 parser work. If you change the syntax after that, you're re-doing that work. 

**Decision**: 
- Current syntax (fn, let, ->): Keep? Or change now?
- If changing, pick ONE thing (fn→func, or add Go-style types, not both)
- Implement that change NOW (4-6 hours)
- Then do Phase 2 on top of final syntax

**Questions to answer**:

1. Do you want "Go++" to attract Go developers?
2. Do you actually prefer whitespace-sensitive like Python/Scala?
3. Or is current syntax (fn, let, brackets for generics) fine?

Once you decide, we lock it in `SYNTAX_DECISIONS.md` and move forward without reconsidering.

---

## Syntax Change Checklist (If You Decide to Change)

If you decide to change syntax, follow this order:

1. ✓ Decide on new syntax (document in SYNTAX_DECISIONS.md)
2. ✓ Update lexer for new keywords/tokens
3. ✓ Update parser for new rules  
4. ✓ Verify all tests still pass with new syntax
5. ✓ Update all examples in /docs and /examples
6. ✓ Update documentation with new syntax
7. Then start Phase 2 implementation

This ensures Phase 2 starts with final, locked syntax.

