# Bug Fix Plan for Phase 2 Completion

This document outlines the exact plan for fixing the 2 bugs blocking Phase 2 completion.

## BUG #1: Parser fails on `list[int]` in return type annotations

### Test that exposes it:
```bash
./test/test_typecheck_and_codegen.sh  # TEST 10
```

### Minimal reproduction:
```marmoset
let f = fn() -> list[int] { [1, 2, 3] };
f();
```

### Error:
```
Parse error: expected next token to be Token.RBracket, got Token.LBrace instead
```

### Root cause:
Parser position tracking after `parse_type_expr` in `parse_function_literal` is incorrect.
When parsing `-> list[int]`, after the type expression is parsed, the position is wrong.

### Fix location:
`lib/frontend/syntax/parser.ml` around line 360-375 in `parse_function_literal`

### Fix strategy (TDD):

1. **WRITE TEST FIRST** (should fail):
   ```ocaml
   let%test "parse list type annotation in function" =
     match Parser.parse "let f = fn() -> list[int] { [1, 2, 3] }; f" with
     | Error _ -> false
     | Ok _ -> true
   ```

2. **Debug the parser**:
   - Check that after `parse_type_expr`, the parser is positioned at `{`
   - Verify the loop in `parse_type_expr` doesn't consume the `{`

3. **Implement fix**:
   - Likely needs to adjust how `parse_type_expr` returns its position
   - Or fix the positioning logic in `parse_function_literal`

4. **Verify test passes**

5. **Run full test suite**: `./test/test_typecheck_and_codegen.sh`
   - TEST 10 should now pass
   - All other tests should still pass

---

## BUG #2: Type inference shows type variables with recursive calls in binary op

### Test that exposes it:
```bash
./test/test_typecheck_and_codegen.sh  # TEST 13
```

### Minimal reproduction:
```marmoset
let f = fn(n: int) -> int {
  if (n < 2) { return n }
  return f(n - 1) + f(n - 1);
};
f(5);
```

### Error:
```
Type error: Function return type annotation mismatch: expected int but inferred t2
```

### Root cause:
Type inference for binary operations with two recursive function calls in a conditional
doesn't properly constrain all type variables. The system creates fresh type variables
for each recursive call, but the unification doesn't fully resolve them.

### Fix location:
`lib/frontend/typecheck/infer.ml` - likely in:
- `infer_infix` (line ~236) - handles binary operations
- `infer_call` (line ~347) - handles function calls
- Or the unification logic that connects them

### Fix strategy (TDD):

1. **WRITE TEST FIRST** (should fail):
   ```ocaml
   let%test "recursive calls in binary op with conditional" =
     Infer.reset_fresh_counter ();
     let code = {|let f = fn(n: int) -> int { 
       if (n < 2) { return n }
       return f(n - 1) + f(n - 1);
     };
     f(5)|} in
     (match check_string code with
     | Error { message; _ } ->
         (* Should NOT have annotation mismatch - it should infer correctly *)
         String.lowercase_ascii message |> String.contains_substring ~substring:"annotation" |> not
     | Ok _ -> true)
   ```

2. **Add debug output** to understand type inference flow:
   - Print substitutions at each step
   - Track which type variables are being created
   - See where unification is incomplete

3. **Investigate unification**:
   - Check if `compose_substitution` is working correctly
   - Verify that substitutions are being applied at the right points
   - Look for cases where type variables aren't being fully resolved

4. **Possible fixes**:
   - May need to ensure substitutions are fully applied before building function types
   - May need to adjust how recursive calls are typed in `infer_call`
   - May need stronger constraint propagation in binary operations

5. **Verify test passes**

6. **Run full test suite**: `./test/test_typecheck_and_codegen.sh`
   - TEST 13 should now pass
   - All other tests should still pass
   - CHECK: fibonacci-typed.mr should now compile with proper error for string annotation

---

## Phase 2 Completion Checklist

Before declaring Phase 2 complete:

- [ ] BUG #1 fixed and TEST 10 passes
- [ ] BUG #2 fixed and TEST 13 passes
- [ ] Full test suite runs: `./test/test_typecheck_and_codegen.sh`
- [ ] ALL 17 tests pass (0 failures)
- [ ] No Phase 1 regressions
- [ ] CLAUDE.md updated with bug fixes
- [ ] Commit message clearly states "Phase 2 COMPLETE"
- [ ] Only then proceed to Phase 3

---

## Testing Command

```bash
# Run all tests
./test/test_typecheck_and_codegen.sh

# Expected output:
# RESULTS: 17 passed, 0 failed out of 17 tests
# ✓ ALL TESTS PASSED
```

---

## Order of Fixing

1. **Fix BUG #1 first** (parser issue)
   - It's simpler (just position tracking)
   - Gets one test passing immediately
   - Unblocks BUG #2 testing (BUG #1 was preventing TEST 10 from running)

2. **Fix BUG #2 second** (type inference issue)
   - More complex but well-understood pattern now
   - May need debugging/investigation
   - Most important for correctness

Both must be fixed before Phase 2 is complete.

---

**Last Updated:** Feb 3, 2026
**Status:** These bugs WILL be fixed. No Phase 3 work starts without them being resolved.
