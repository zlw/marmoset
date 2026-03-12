# Diagnostics Rework

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Implemented
- Type: Historical implementation record
- Original branch target: `diagnostics-rework`

## Summary

This milestone made `Diagnostic.t` the canonical diagnostics payload across parser, typechecker, codegen, CLI, and LSP. It also migrated the fixture harness away from legacy text-shape matching and onto strict matching against canonical diagnostics.

## Outcome

- Canonical diagnostics core lives in `lib/diagnostics/`.
- Parser, typechecker, and codegen now emit structured diagnostics with spans and stable codes.
- `marmoset check` and `marmoset build` share one diagnostics rendering path.
- LSP consumes the same canonical diagnostics model instead of fabricating separate parse/typecheck output paths.
- The fixture harness matches diagnostics strictly by severity, file, line, and unexpected-output rules.
- Parser file identity and compiler/LSP boundary behavior no longer fail open in expected paths.

## Final Invariants

1. `Diagnostic.t` is the canonical compiler-grade diagnostics payload at frontend and tool boundaries.
2. CLI rendering is intentionally minimal and deterministic. Rich console presentation is still out of scope.
3. Bail-on-first remains the current compiler policy.
4. LSP consumes canonical diagnostics and must not silently collapse internal failures to `[]`.
5. Integration fixtures remain strict by default:
   - missing expected diagnostics fail,
   - line mismatches fail when diagnostics are source-located,
   - unexpected diagnostics fail.
6. `marmoset build` and `marmoset check` must not diverge in diagnostics formatting or renderer ownership.

## Major Changes

### Compiler and Libraries

- Introduced a shared diagnostics core and renderer in `lib/diagnostics/`.
- Threaded spans and file identity through parser, typechecker, and codegen boundaries.
- Removed or isolated expected-path string/exception transport in frontend layers.
- Standardized descriptive diagnostic codes and shared severity handling.

### CLI and LSP

- Unified CLI diagnostics rendering between `check` and `build`.
- Removed LSP fail-open behavior that could mask internal analyzer failures.
- Kept codegen/build diagnostics CLI-only; LSP remains a parser + typechecker surface.

### Harness and Tests

- Updated `test/integration.sh` to parse canonical diagnostics rather than legacy flat error text.
- Preserved strict reject semantics, annotation placement checks, and deterministic reporting.
- Kept the monkey fixture drift guard intact through the diagnostics migration.

## Deliberate Non-Goals That Remain Non-Goals

- multi-error recovery or accumulation
- rich terminal rendering
- external JSON diagnostics API
- codegen/build diagnostics in LSP

## Follow-Up Boundary

Future language and type-system plans should treat canonical diagnostics as settled infrastructure, not as an active migration track. Later work may still improve renderer polish or add multi-diagnostic surfaces, but that is outside this completed milestone.
