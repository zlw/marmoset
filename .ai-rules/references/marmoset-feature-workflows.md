# Marmoset Feature Workflow Reference

## Operating Stance

- Act like a compiler engineer and type-system designer, not a generic code assistant.
- Optimize for correctness, soundness, maintainability, and explicit reasoning over speed or token cost.
- Prefer simple, principled designs over clever local patches.
- Draw on mature compiler practice from OCaml, Rust, Haskell, Scala, Clojure, Flix, Koka, and Roc when evaluating tradeoffs.
- If a plan or implementation would be unsound, under-specified, or obviously brittle, stop and surface the issue directly.

## Shared Skill Names

Use these canonical skill names:

- `plan-create`
- `plan-review`
- `plan-check`
- `feature-implement`
- `feature-review`
- `issue-hunt`
- `issue-fix`

Claude invokes these as `/plan-create`, `/plan-review`, and so on.
Codex invokes these as `$plan-create`, `$plan-review`, and so on.

## Mandatory Reads

Before doing substantial work, read:

1. `CLAUDE.md`
2. The relevant plan files under `docs/plans/todo/` and `docs/plans/done/`
3. Relevant feature docs under `docs/features/`
4. Relevant code and tests in touched areas
5. `git status --short` so you do not trample unrelated work

## Repo-Wide Policy Source

For repo-wide engineering rules, follow `CLAUDE.md` rather than restating them in each skill. In particular, treat `CLAUDE.md` as the source of truth for:

- TDD and verification cadence
- commit message format
- current-tree and worktree rules
- push-time quality gates
- non-committable local artifacts

## Parallelism Rules

- Parallelize whenever the subtasks are independent.
- Good parallel work:
  - reading neighboring modules and tests
  - reviewing similar fixtures
  - drafting mechanical fixture additions
  - adversarial review of a draft plan
  - read-only implementation review while coding continues
- Keep urgent blocking work local. Do not hand the critical path to a peer agent and wait idly.
- If invoked from Codex, use multiple subagents for independent read-only analysis when useful.
- If invoked from Claude, use its task/subagent facilities when useful.
- For cross-tool review, use the other CLI when it is available locally.

## Plan Path Conventions

- Default destination: `docs/plans/todo/<area>/<NN>_<slug>.md`
- Infer `<area>` from the closest existing neighbors such as `language`, `tooling`, or `ci`.
- If the user did not specify the ordinal, inspect the chosen area directory and pick the next available two-digit prefix.
- Always create a new file for a new plan unless the user explicitly asked to revise a specific existing plan file.
- The plan must be long-form and implementation-driving, not a vague outline.

## Required Plan Shape

Plans should usually include:

- Title
- `## Maintenance`
- `- Last verified: YYYY-MM-DD`
- `- Implementation status: Planning`
- `## Context`
- `## Goals`
- `## Non-Goals`
- `## Current State` or equivalent codebase findings
- `## Design Decisions` or invariants
- `## Implementation Phases` or `## Implementation Plan`
- `## Testing Strategy`
- `## Commit Plan` or small-commit sequence
- `## Risks` / rollback / open questions
- `## Progress`

The plan must name concrete files, modules, functions, data types, diagnostics, fixtures, and integration suites when they matter. If implementation details could vary, choose one and write it down.

## Progress Section

- Always append progress entries at the end under `## Progress`
- Use timestamped bullet lines with local time zone context when available
- Record at least:
  - drafting started
  - major review pass completed
  - test-first slice started
  - focused tests green
  - commit created
  - final verification completed

## Review Artifact Conventions

- Put read-only review notes under `docs/review/<plan-slug>/`
- Treat `docs/review/` as local scratch unless the user explicitly says to commit it
- Prefer filenames like:
  - `01_initial-review.md`
  - `02_after-abc1234.md`
  - `03_final-review.md`

## AI-Specific Commit Additions

If the current assistant creates a commit, include a co-author trailer:

- `Co-authored-by: Codex <codex@openai.com>`
- `Co-authored-by: Claude <claude@anthropic.com>`

If the peer AI materially shaped the specific change, include both trailers.

## Counterpart Review Expectations

Cross-tool reviews must be adversarial and concrete. Ask the peer AI to focus on:

- missing invariants
- unsound semantics
- parser, resolver, typechecker, and codegen interactions
- ambiguous implementation steps
- over-complication and simpler alternatives
- missing edge-case fixtures
- diagnostic quality
- regression risk

Each review round must make the artifact better or expose why it is already strong. Do not allow review passes that merely restate the current draft.

## Cross-Tool Availability

If both `codex` and `claude` CLIs are available locally, prefer true cross-tool review for `plan-review`, `feature-review`, and `issue-hunt`.

If one CLI is unavailable, fall back to same-tool subagents and state that the cross-tool pass was unavailable.
