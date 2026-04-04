# Codex Review: 02_prelude

Date: 2026-04-04

Scope:
- Reviewed the implementation against [docs/plans/todo/language/02_prelude.md](/Users/zlw/src/marmoset/worktrees/prelude/docs/plans/todo/language/02_prelude.md)
- Codex-only pass, per request

## Findings

No prelude-specific implementation bug was found in the reviewed prelude surface.

One follow-up issue was identified after the review: the current module-system spec and resolver still treat bare nested module imports as root-bound namespaces (`import collections.list` => `collections.list(...)` / `collections.list.member`). That is stale relative to the intended language semantics, where nested module imports should bind the leaf module name (`import collections.list` => `list.member(...)`).

That issue is not specific to prelude injection. It is a module-system namespace-binding bug that happened to surface during the prelude review.

That follow-up issue has since been fixed in the module resolver and module-system spec examples; it remains recorded here because it was discovered during this review pass.

The valid prelude surface under review remains:

- `Option.Some(...)`
- `Option.unwrap_or(...)`
- non-core `std.*` modules consuming injected core stdlib bindings

The separate unsupported assumption I checked and kept out of scope is concrete-value dot-call fallback to inherent methods (`Option.Some(1).unwrap_or(0)`).

## Assessment

Plan fidelity:
- The prelude implementation matches the plan's chosen surface: toolchain-owned stdlib, direct injection of `std.prelude` / `std.option` / `std.result`, and qualified inherent helper calls such as `Option.unwrap_or(...)`.
- A separate stale assumption in nested namespace import semantics was identified during review and then corrected in the module-system implementation/docs.

Complexity:
- The design is simpler than the earlier project-local/fallback shape and is coherent with the existing module pipeline.

Likely bug/unsound corners:
- No remaining high-confidence prelude correctness bug found in the reviewed path.

Testing:
- Coverage is now better around non-core stdlib consumers.
- Residual risk remains around user expectations for value-dot inherent calls, because the language currently requires qualified calls rather than `value.method(...)`.

## Verification

Ran:
- `make unit compiler`
- end-to-end CLI repro with a temporary toolchain stdlib root and `std/foo.mr`

Result:
- green on the supported surface
