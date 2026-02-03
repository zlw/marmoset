# Phase 3 Plan: Expression-Based Returns with Early Exit

**Status:** Planning → Implementation  
**Date Started:** Feb 3, 2026  
**Estimated Time:** 6-8 hours  

---

## Core Vision

Migrate Marmoset to **expression-based return semantics** while preserving early `return` syntax for readability during recursion.

### Key Principles

1. **Everything-is-an-expression** - if/match/try all return values
2. **Early returns allowed** - `return` in if/match for clean recursion
3. **Implicit final expression** - last expression in function body is return
4. **Context-aware else** - else mandatory only if if-value is used
5. **Uniform type checking** - no special return-statement validation needed

---

## Current State Analysis

### What Already Works ✅

```marmoset
fn(x: int) -> int { x + 1 }  // Implicit return

fn(x: int) -> int { 
  if (x < 0) { return -1 }
  x * 2                       // Implicit return
}
```

### What's Broken ❌

```marmoset
let y = if (5 > 0) { 1 }     // Type checker allows, Go code fails
// Should error: if-value used but no else branch
```

### Root Cause

Type checker doesn't distinguish:
- **If-as-statement** (value discarded) - else optional
- **If-as-expression** (value used) - else mandatory

---

## Implementation Plan

### Phase 3.0: Core Type System Fix (3-4 hours)

#### Task 3.0.1: Fix If-As-Expression Type Checking (2 hours)

**File:** `lib/frontend/typecheck/infer.ml`

**Current behavior (infer_if, line ~282):**
```ocaml
match alternative with
| None -> Ok (subst', cons_type)  (* Returns consequence type even without else *)
| Some alt -> ...
```

**Problem:** If no else, returns `cons_type` (from consequence). Caller can use this value even though else is missing.

**Solution:** Change semantics to:
- If no else: return type is `TNull` (or special "unit" type)
- Caller must handle null type appropriately

**Changes needed:**
```ocaml
match alternative with
| None -> 
    (* No else: if-value is null/unit *)
    Ok (subst', TNull)  (* Changed from cons_type *)
| Some alt -> ...
```

**Then add validation:** If caller tries to use null type where a real type is expected, error.

**Affected functions:**
- `infer_if` - return TNull when no else
- `infer_expression` - consumers of if-expressions
- Error types - add "IfValueNotUsed" or similar

**Tests to add:**
```marmoset
// Should ERROR: if-value used without else
let x = if (5 > 0) { 1 }

// Should OK: if-value discarded
if (5 > 0) { print(1) }

// Should OK: if-value with else
let x = if (5 > 0) { 1 } else { 2 }
```

#### Task 3.0.2: Remove Mixed-Return Validation (1 hour)

**Files:** `lib/frontend/typecheck/infer.ml`

**Changes:**
1. Delete `validate_return_statements` function (lines ~608-639)
2. Simplify `infer_function_with_annotations` (line ~348)
   - Remove call to `validate_return_statements`
   - Just check `unify body_type' expected_ret_type`

**Why:** With proper if-expression semantics, we don't need special return validation. Type checking is automatic.

#### Task 3.0.3: Update Error Types (0.5 hours)

**File:** `lib/frontend/typecheck/infer.ml`

**Add new error:**
```ocaml
type error_kind =
  | ...
  | IfExpressionWithoutElse  (* New: if-value used but no else *)
```

**Update error_kind_to_string:**
```ocaml
| IfExpressionWithoutElse -> 
    "If-expression must have else branch when value is used"
```

---

### Phase 3.1: Parser Enhancement (1-2 hours)

#### Task 3.1.1: Support Single-Statement Return in If (1 hour)

**File:** `lib/frontend/syntax/parser.ml`

**Current (line 335-336):**
```ocaml
let* p5 = expect_peek p4 Token.LBrace in
let* p6, cons = parse_block_statement p5 in
```

**Change to:**
```ocaml
let* p5, cons = 
  if peek_token_is p4 Token.LBrace then
    (* Normal case: block with braces *)
    let* p = expect_peek p4 Token.LBrace in
    parse_block_statement p
  else if peek_token_is p4 Token.Return then
    (* New: single return without braces *)
    let* p, expr = parse_return_statement p4 in
    Ok (p, mk_stmt (current_pos p) (AST.Block [expr]))
  else
    Error (parser_error "Expected '{' or 'return'")
in
```

**Add helper function:**
```ocaml
and parse_return_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p1 = expect p Token.Return in
  let* p2, expr = parse_expression (next_token p1) prec_lowest in
  Ok (p2, mk_stmt pos (AST.Return expr))
```

**Test cases:**
```marmoset
if (n < 2) return n else { fib(n-1) + fib(n-2) }
if (x > 0) return 1 else { compute() }
```

---

### Phase 3.2: Test Updates (2-3 hours)

#### Task 3.2.1: Update Phase 2 Tests

**File:** `test/test_typecheck_and_codegen.sh`

No actual test changes needed - Phase 2 tests should still pass!

But add NEW tests:
```bash
test_case "If-expression without else (should fail)" \
  'let x = if (5 > 0) { 1 }' \
  "false" \
  "else branch"

test_case "If-statement without else (should pass)" \
  'if (5 > 0) { print(1) }; 42' \
  "true"

test_case "Early return syntax" \
  'fn(n: int) -> int { if (n < 2) return n; fib(n-1) + fib(n-2) }; fib(5)' \
  "true"
```

#### Task 3.2.2: Verify All Tests Pass

Run full test suite:
```bash
./test/test_typecheck_and_codegen.sh
```

Target: All 17/17 Phase 2 tests + new tests pass.

---

### Phase 3.3: Documentation (1 hour)

#### Task 3.3.1: Update CLAUDE.md

Add to Phase 3 section:
```
### Phase 3: Type Constraints & Unions
- ✅ Expression-based returns (final expr is implicit return)
- ✅ Early return syntax (if/match without else for exits)
- Context-aware else (required only if value used)
- Type constraints: `fn[a: show](x: a)`
- Union types: `int | string | bool`
```

#### Task 3.3.2: Create Return Semantics Documentation

Add section:
```
## Return Semantics

Marmoset uses expression-based returns with early-exit support:

1. **Implicit return** - last expression in function is returned
2. **Early return** - `return expr` exits immediately
3. **If-as-statement** - else optional (value discarded)
4. **If-as-expression** - else mandatory (value used)

Examples:
fn(n: int) -> int {
  if (n < 2) return n          // Early exit, OK
  fib(n-1) + fib(n-2)          // Implicit return
}

// ERROR: if-value used without else
let x = if (cond) { 1 }

// OK: if-value with else
let x = if (cond) { 1 } else { 2 }
```

---

## Risk Assessment

### Low Risk Items ✅
- Type checker changes are isolated
- Parser changes are additive (don't break existing syntax)
- Tests exist to validate

### Medium Risk Items ⚠️
- Changing if-expression semantics could affect Go codegen
- Need to verify all generated Go code is valid

### Mitigation Strategies
- Test incrementally after each task
- Keep git commits small and focused
- Run full test suite frequently
- Review generated Go code for edge cases

---

## Checkpoint Verification

After each task, verify:

### After 3.0.1 (If-as-expression fix)
```bash
# Should FAIL (if-value used without else)
./build 'let x = if (5 > 0) { 1 }' 

# Should PASS (if-value discarded)  
./build 'if (5 > 0) { print(1) }; 42'

# Should PASS (if-value with else)
./build 'let x = if (5 > 0) { 1 } else { 2 }; x'
```

### After 3.0.2 (Remove validation)
```bash
# Run full test suite
./test/test_typecheck_and_codegen.sh
# Should pass all 17/17 tests
```

### After 3.1.1 (Parser enhancement)
```bash
# Should PASS with new syntax
./build 'fn(n: int) -> int { if (n < 2) return n; n * 2 }; 5'

# Should still accept old syntax
./build 'fn(n: int) -> int { if (n < 2) { return n } n * 2 }; 5'
```

### After 3.2 (Tests)
```bash
./test/test_typecheck_and_codegen.sh
# All tests pass including new ones
```

---

## Decision Log

### ✅ Decision: Skip Postfix Return Syntax (Option B)

**Rationale:** `return n if n < 2` is syntactic sugar only. Can add in "polish phase" later without affecting core type system.

**Timeline:** Add in Phase 3.5 or later (2-3 hours when needed)

### ✅ Decision: Require Else Only When Value Used

**Rationale:** Matches Rust/Scala/Ruby behavior. Allows clean early returns without forcing unnecessary else blocks.

**Alternative rejected:** Require else everywhere (too restrictive, breaks recursion patterns)

### ✅ Decision: Support Single-Statement Return Syntax

**Rationale:** Improves readability for common recursion pattern: `if (base_case) return val`

**Implementation:** Parser enhancement, non-breaking

---

## Success Criteria

- [ ] All 17 Phase 2 tests pass
- [ ] New if-expression tests pass (if without else → error)
- [ ] New if-statement tests pass (if without else → OK)
- [ ] Early return syntax works without braces
- [ ] `validate_return_statements` function deleted
- [ ] Generated Go code is valid for all test cases
- [ ] CLAUDE.md updated with new semantics
- [ ] No regressions in existing functionality

---

## Files to Modify

Priority order:

1. **lib/frontend/typecheck/infer.ml** (Main work)
   - Modify `infer_if` to return TNull when no else
   - Delete `validate_return_statements`
   - Simplify `infer_function_with_annotations`
   - Add IfExpressionWithoutElse error type
   - Add error handling

2. **lib/frontend/syntax/parser.ml** (Optional, syntax sugar)
   - Add support for `if (cond) return expr` syntax
   - Add `parse_return_statement` helper

3. **test/test_typecheck_and_codegen.sh** (Tests)
   - Add new test cases for if-expression semantics

4. **CLAUDE.md** (Documentation)
   - Update Phase 3 description
   - Document return semantics

---

## Implementation Order

1. **Start:** Task 3.0.1 (If-as-expression fix)
2. **Then:** Task 3.0.2 (Remove validation)
3. **Test:** Checkpoint verification
4. **Next:** Task 3.1.1 (Parser - optional)
5. **Then:** Task 3.2 (Test updates)
6. **Finally:** Task 3.3 (Documentation)

---

## Time Breakdown

- 3.0.1: 2.0 hours (if-expression semantics)
- 3.0.2: 1.0 hours (delete validation)
- 3.0.3: 0.5 hours (error types)
- **Subtotal 3.0: 3.5 hours**

- 3.1.1: 1.5 hours (parser enhancement)
- **Subtotal 3.1: 1.5 hours**

- 3.2: 2.5 hours (test updates)
- **Subtotal 3.2: 2.5 hours**

- 3.3: 1.0 hours (documentation)
- **Subtotal 3.3: 1.0 hours**

**Total: 8.5 hours**

---

**Next Step:** Begin with Task 3.0.1 - Fix If-As-Expression Type Checking
