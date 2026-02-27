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

## 6. Git Workflow

### NEVER Commit These:

- `docs/review/` — analysis/progress tracking docs (local only)
- Temporary test directories (e.g., `test_alias/`, `test_derive/`, `test_row_check/`)
- Build artifacts or binaries
- `.claude/` directory contents
- Any file in `/tmp/`

Always use `git add <specific files>` — never `git add .` or `git add -A`.

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

## 7. Common Pitfalls to Avoid

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

## 8. Common Tasks

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
