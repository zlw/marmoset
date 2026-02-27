# Documentation Policy

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

This repository uses a single canonical documentation set.

## 1) Canonical docs (current truth)

Use docs in `docs/` as authoritative:

- `docs/INDEX.md`
- `docs/ARCHITECTURE.md`
- `docs/ROADMAP.md`
- `docs/features/*.md`

These files are the maintained design/spec references for language behavior and implementation choices.

## Precedence rules

If any inconsistency appears, use this order:

1. Current implementation and tests (`lib/**`, `test/**`)
2. Canonical docs in `docs/`
