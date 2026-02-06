# CLAUDE.md - Guidelines for Claude Code Working with This Codebase

This document establishes rules and best practices for Claude Code when working on the Marmoset project.

## 1. Test-Driven Development (TDD) - MANDATORY

**RULE: No feature is complete until tests pass 100%.**

### Process (In This Order):

1. **Write tests FIRST** - Before writing any code
   - Tests should fail initially (RED)
   - Tests verify both success AND failure cases
   - Tests validate error messages are helpful

2. **Implement code** - Make tests pass (GREEN)
   - Only implement enough to pass tests
   - Don't over-engineer

3. **Refactor** - Improve code while keeping tests passing (REFACTOR)
   - Clean up, optimize, document
   - Tests keep you safe

### What "Complete" Means:

- ✅ Tests exist for the feature
- ✅ All tests pass (100%, no skipping)
- ✅ Tests cover normal cases, edge cases, AND error cases
- ✅ Integration tests verify end-to-end flow
- ✅ Error messages are clear and helpful
- ✅ No regressions in existing tests

**NEVER claim "complete" based on:**
- ❌ Code compiles
- ❌ Infrastructure exists
- ❌ Components are wired together
- ❌ Spot testing a few examples

### Example: Type Annotation Validation (Previous Mistake)

**WRONG:** Implemented annotation.ml + check_program_with_annotations, claimed "complete"
- Built but never actually validated anything
- Went unused in compilation pipeline
- No tests existed

**RIGHT:** 
1. Write tests for annotation validation (fail)
2. Wire up check_program_with_annotations (fail)
3. Implement validation logic (pass)
4. Run full test suite (pass)
5. Only then claim "complete"

## 2. Testing Strategy

### For Typecheck/Type System:

Create tests that:
- Parse correct annotations ✅
- Parse incorrect annotations ✅
- Infer correct types ✅
- Catch type mismatches ✅
- Generate clear error messages ✅
- Work with complex scenarios (generics, recursion, conditionals) ✅

Example location: `lib/frontend/typecheck/checker.ml` has `let%test` blocks

### For Codegen:

Create tests that:
- Compile Marmoset source successfully ✅
- Generate valid Go code ✅
- Go code compiles to binary ✅
- Binary runs and produces correct output ✅

Example location: `test/test_typecheck_and_codegen.sh` has integration tests

### Test File Organization:

- **Inline tests**: In the module they test (using `let%test`)
  - Fast to run
  - Located near code
  - For unit-level behavior

- **Integration tests**: In `test/` directory (shell scripts or standalone)
  - Test end-to-end flow
  - Verify cross-module interactions
  - Test actual binaries/outputs

## 3. Commits and Documentation

### Before Committing:

1. ✅ All tests pass
2. ✅ No regressions in existing tests
3. ✅ Code is clean and readable
4. ✅ Commit message explains WHAT and WHY, not just code changes

### Commit Message Format:

```
Brief one-line summary of what was done

**Detailed explanation:**
- What problem this solves
- How it works
- Any caveats or limitations

**Testing:**
- What tests verify this works
- Any known issues

**Changes:**
- File: specific changes
```

Example:
```
Phase 2: Complete type annotation validation and parameter type parsing

**Major Changes:**
1. Parser now captures parameter type annotations (x: int)
2. Infer uses parameter annotations during type inference
3. Return type annotations are validated against inferred types

**Bug Fixes:**
- Fixed polymorphic type extraction in checker
- Fixed return type extraction for multi-parameter functions

**Testing:**
✅ 25+ inline tests in checker.ml
✅ 17 integration tests pass (15/17 before fixing parser bugs)
✅ No Phase 1 regressions
```

### Known Issues/Bugs:

If you discover a bug during testing, IMMEDIATELY:
1. Document it in the commit message
2. Add it to the TODO in code or CLAUDE.md
3. Create a test that demonstrates the bug
4. Do NOT "work around" it - fix it

## 4. Code Quality Standards

### OCaml-Specific:

- Use pattern matching extensively (it's OCaml's superpower)
- Prefer immutable data structures
- Type annotations on public functions help self-document
- Use meaningful variable names (avoid `x`, `y`, `z` for complex data)
- Leverage the type system - if it compiles and has right types, it usually works

### Error Messages:

- Must be clear and actionable
- Should include:
  - What went wrong
  - Where it went wrong (line/column)
  - What was expected
  - How to fix it

Example GOOD:
```
Type annotation mismatch at line 5, column 10:
  Expected: string
  But got: int
  Function returns integer from arithmetic, but annotation says string
```

Example BAD:
```
Type error
```

### Documentation:

- Document WHY, not WHAT
- Comments explain design decisions
- Examples in docstrings for complex functions
- Link to issues/todos if relevant

## 5. Building and Testing

### Normal Development:

```bash
# Build
dune build

# Run tests
dune runtest

# Or specific test suite
./test/test_typecheck_and_codegen.sh
```

### Performance:

- Profile before optimizing
- Don't sacrifice clarity for micro-optimizations
- Use `[@inline]` sparingly, only after profiling

### Debugging:

- Use print debugging when needed (then remove)
- OCaml toplevel (`ocaml`) useful for experimentation
- Use `dune utop` for interactive testing

## 6. Phase Structure

### Phase 1: Core Language ✅ DONE
- Lexer, Parser, Evaluator, Compiler
- Basic types: int, string, bool, arrays, hashes
- Functions, conditionals, loops
- **Status:** Complete with tests

### Phase 2: Type Annotations ✅ COMPLETE
- Type syntax: `fn(x: int) -> string { ... }`
- Type inference with annotations
- Generic parameters: `fn[a](x: a) -> a { ... }`
- **Status:** ✅ 17/17 tests passing (ALL FIXED)

### Phase 3: Expression-Based Returns with Early Exit ✅ COMPLETE
- Early return syntax: `if (cond) return value else { ... }`
- If-expression semantics: if without else returns TNull
- Single-statement returns without braces
- **Status:** ✅ 21/21 tests passing (17 Phase 2 + 4 Phase 3)

### Phase 4, Milestone 1: Union Types ✅ COMPLETE
- Union type inference from if-expressions
- Union type annotations: `int | string`
- Runtime type tags and Go codegen
- **Status:** ✅ 31/31 tests passing

### Phase 4, Milestone 2: Enums & Pattern Matching ✅ COMPLETE
- Enum definitions: `enum option[a] { some(a) none }`
- Generic enum parameters with monomorphization
- Pattern matching: `match x { pattern: expr }`
- Constructor patterns, wildcard patterns, literal patterns
- Multi-field enum variants with field extraction
- **Status:** ✅ 47/47 tests passing

### Phase 4, Milestone 3: Traits System ✅ COMPLETE
- Trait definitions: `trait show[a] { fn show(x: a) -> string }`
- Trait implementations: `impl show for int { ... }`
- Implementation validation (full signature checking with type parameter substitution)
- Automatic derivation: `derive eq, show for int;`
- Supertraits: `trait ord[a]: eq { ... }`
- **Trait method calls:** `x.show()` compiles to `show_show_int64(x)`
- **Status:** ✅ 62/62 tests passing (33 unit + 15 integration + 14 parser + 5 method call tests)
- **Features:**
  - ✅ Trait registry with global hashtables
  - ✅ Method signature validation (param count, param types, return types)
  - ✅ Type parameter substitution (TVar "a" → concrete type)
  - ✅ Derivable traits: eq, show, debug
  - ✅ Manual and derived impls can coexist
  - ✅ Detailed error messages for all validation failures
  - ✅ Trait method call syntax with dot notation
  - ✅ Type inference for method calls (receiver type → trait lookup)
  - ✅ Go codegen for trait impl functions (static dispatch)
  - ✅ Method calls work on primitives (int, string, bool)
- **Implementation:**
  - Parser refactored: dot operator is now a general postfix operator
  - `MethodCall` AST node: `expr.method(args)`
  - Type checker resolves method calls via `Trait_registry.lookup_method`
  - Codegen generates: `trait_method_type(receiver, args...)`
  - Function naming: `{trait}_{method}_{mangled_type}` (e.g., `show_show_int64`)
- **Commits:** 12 commits (315efd8 through current)
- **Lines of Code:** ~1,900 lines added (40% tests)
- **Deferred:** 
  - Generic function constraints `fn[a: show](x: a)` (requires parser changes)
  - If-else expressions in impl method bodies (parser limitation)

## 7. Git Workflow

### Branch Naming:

- `feature/description` - New features
- `fix/description` - Bug fixes
- `typecheck` - Type system work (current)
- `codegen` - Code generation work

### Before Push:

```bash
# Run full test suite
make unit

# Check for regressions
git diff main..HEAD  # Review your changes

# Make sure commit message is clear
git log -1
```

### Push to Remote:

```bash
git push origin <branch>
```

Then create PR or notify of changes.

## 8. Common Pitfalls to Avoid

### ❌ DON'T:

1. **Skip testing** - "This is small, it obviously works"
   - It won't. Type systems are subtle. Test it.

2. **Claim "complete" without 100% test pass**
   - This happened with annotations
   - Infrastructure existed but wasn't wired/tested
   - Now it's a rule: no completion without passing tests

3. **Make breaking changes silently**
   - Always check for regressions
   - Run full test suite before committing

4. **Over-complicate the code**
   - KISS principle: Keep It Simple, Stupid
   - OCaml's type system catches bugs for you
   - Trust it

5. **Ignore error cases**
   - Tests should verify error handling
   - Error messages should be helpful
   - Don't just crash

### ✅ DO:

1. **Write tests first** - RED, GREEN, REFACTOR
2. **Keep commits small** - One logical change per commit
3. **Update docs** - Keep CLAUDE.md and README.md current
4. **Run tests often** - After every meaningful change
5. **Ask questions** - If unclear, document assumptions

## 9. Common Tasks

### Adding a New Phase:

1. Create tests first (they should fail)
2. Update this CLAUDE.md with phase info
3. Implement features incrementally
4. Each commit should pass all tests
5. Only update README when feature is 100% done and tested

### Fixing a Bug:

1. Create test that reproduces bug (failing)
2. Locate root cause
3. Implement fix
4. Test passes
5. Run full suite (no regressions)
6. Commit with clear message

### Adding a New Test:

1. Decide: inline (unit) or integration (shell/script)
2. Write test code
3. Verify it fails initially
4. Implement feature
5. Verify test passes
6. Commit with test included

## 10. Current Status & Known Issues

### Tests Coverage:

- ✅ Lexer/Parser: Well tested with inline tests
- ✅ Type Inference: Good test coverage
- ✅ Type Annotations: **ALL 17/17 PASSING** ✅
- ✅ Early Return Syntax: **4/4 NEW TESTS PASSING** ✅
- ✅ Union Types: **10 integration tests PASSING** ✅
- ✅ Enums & Pattern Matching: **16 integration tests PASSING** ✅
- ✅ Codegen: **47 integration tests, ALL 47/47 PASSING** ✅
- ❌ Runtime/Interpreter: Minimal tests (should add)

### Bug Fixes Completed:

- **BUG #1:** ✅ FIXED - Parser now handles `list[int]` in return type annotations
  - Test: `test/test_typecheck_and_codegen.sh` TEST 10
  - Status: FIXED (b257986)
  - Fix: Changed `expect_peek` to `curr_token_is` for bracket checking

- **BUG #2:** ✅ FIXED - Type inference now properly constraints recursive function returns
  - Pattern: `fn(n: int) -> int { if (n < 2) { return n } return f(n-1) + f(n-1) }`
  - Root cause: Recursive functions didn't know their return type, so recursive calls had unresolved return types
  - Solution: Modified `infer_let` to extract return type annotations and create partially constrained function types
  - Test: `test/test_typecheck_and_codegen.sh` TEST 13 - NOW PASSES ✅
  - Status: FIXED (9fac047)

- **TEST 14:** ✅ FIXED - All return statements now validated
  - Pattern: `fn(n: int) -> string { if (n < 2) { return n } return f(n-1) + f(n-2) }`
  - Solution: Added validate_return_statements function that recursively walks the AST and validates each Return statement
  - Test: `test/test_typecheck_and_codegen.sh` TEST 14 - NOW PASSES ✅
  - Status: FIXED (d9afea9)

### Phase 2: COMPLETE ✅

- **Status: 17/17 TESTS PASSING (100%)**
- All type annotation features working correctly
- Parameter type annotations ✅
- Return type annotations ✅
- Type inference with annotations ✅
- Recursive functions with annotations ✅
- All return paths validated ✅
- Clear error messages ✅

### Phase 3: COMPLETE ✅

- **Status: 21/21 TESTS PASSING (100%)**
- Early return syntax fully working
- If-without-else returns TNull (prevents value usage) ✅
- If-with-else works as expression ✅
- Single-statement return support: `if (cond) return expr` ✅
- Return validation working correctly ✅
- Parser enhancements complete ✅
- Test coverage comprehensive ✅

**Completed Tasks:**
1. ✅ Task 3.0.1: If-as-expression type checking fix
2. ✅ Task 3.0.2: Return validation integration
3. ✅ Task 3.1: Parser support for single-statement returns
4. ✅ Task 3.2: Comprehensive test suite (4 new tests)

### Phase 4, Milestone 1: COMPLETE ✅

- **Status: 31/31 TESTS PASSING (100%)**
- Union type inference from if-expressions ✅
- Union type annotations: `int | string` ✅
- Runtime type tags and Go codegen ✅
- Clear error messages ✅

### Phase 4, Milestone 2: COMPLETE ✅

- **Status: 47/47 TESTS PASSING (100%)**
- Enum definitions: `enum option[a] { some(a) none }` ✅
- Generic enum parameters with monomorphization ✅
- Pattern matching: `match x { pattern: expr }` ✅
- Constructor patterns, wildcard patterns, literal patterns ✅
- Multi-field enum variants with field extraction ✅
- Match expression codegen (enum and primitive types) ✅
- Exhaustiveness checking ✅
- Typed AST refactoring (type_map) ✅

**Major Commits:**
1. ✅ f756cc5: Refactor: Replace type re-inference with typed AST (type_map)
2. ✅ d6fa627: Fix: Match arms with different types now create union types
3. ✅ 994fdc9: Fix: Separate emit_match_primitive from emit_match_enum, handle wildcard patterns correctly

### Phase 4, Milestone 3: COMPLETE ✅

- **Status: 62/62 TESTS PASSING (100%)**
- Trait definitions and implementations ✅
- Automatic trait derivation ✅
- Trait method calls with dot notation ✅
- Type inference for method calls ✅
- Go codegen for trait impl functions ✅
- Static dispatch with mangled function names ✅

**Features Implemented:**
1. ✅ Trait definitions: `trait show[a] { fn show(x: a) -> string }`
2. ✅ Trait impls: `impl show for int { fn show(x: int) -> string { ... } }`
3. ✅ Trait derivation: `derive eq, show for int;`
4. ✅ Method call syntax: `x.show()` compiles to `show_show_int64(x)`
5. ✅ Type inference resolves receiver type → trait lookup
6. ✅ Full signature validation with type parameter substitution
7. ✅ Detailed error messages for validation failures

**Implementation Details:**
- Parser refactored dot operator as general postfix operator
- `MethodCall` AST node for `expr.method(args)` syntax
- Type checker uses `Trait_registry.lookup_method` for resolution
- Codegen generates Go functions: `{trait}_{method}_{mangled_type}`
- Supports primitives: int, string, bool

**Major Commits:**
1. ✅ Parser refactoring for dot operator generalization
2. ✅ Type inference for trait method calls
3. ✅ Go codegen for trait impl functions
4. ✅ Comprehensive test suite (5 new method call tests)

**Known Limitations:**
- Generic function constraints `fn[a: show](x: a)` not yet supported
- If-else expressions in impl method bodies have parser limitations

---

**Last Updated:** Feb 5, 2026 (Phase 4, Milestone 3 COMPLETE - 62/62 tests passing!)  
**Written by:** Claude Code (TDD in action)  
**Remember:** 
- ✅ No feature is complete until tests pass 100%. Always.
- ✅ Bugs found in tests MUST be fixed before feature is done.
- ✅ No moving to next phase until current phase passes all tests.
- ✅ Phase 4, Milestone 3: DONE. Trait method calls working!
