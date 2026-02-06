# Documentation Policy

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

This repository now has a two-layer documentation model:

## 1) Canonical docs (current truth)

Use docs in `docs/` as authoritative:

- `docs/INDEX.md`
- `docs/ARCHITECTURE.md`
- `docs/ROADMAP.md`
- `docs/features/*.md`

These files are the maintained design/spec references for language behavior and implementation choices.

## 2) Historical archive (context, plans, superseded ideas)

All previous docs are preserved in:

- `docs/archive/`

This includes previous phase plans, milestone docs, analysis notes, and alternative proposals.
Nothing was deleted; historical context is retained there.

## Precedence rules

If any inconsistency appears, use this order:

1. Current implementation and tests (`lib/**`, `test/**`)
2. Canonical docs in `docs/`
3. Historical docs in `docs/archive/`

## Notes about milestone docs

The prior Phase 4 milestone docs were retained under:

- `docs/archive/typechecker/phase4/milestone-*.md`

They were used as major input for the new canonical feature docs, but the canonical references are now the docs in `docs/` root and `docs/features/`.
