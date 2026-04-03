---
name: feature-implement
description: Implement a Marmoset feature from a detailed plan by following CLAUDE.md, updating the plan progress log, writing tests first, running focused verification during development, and making small commits with co-author trailers. Use when a specific plan is ready to execute.
disable-model-invocation: true
---

# Feature Implement

Before implementation, read:

- `.ai-rules/references/marmoset-feature-workflows.md`
- `CLAUDE.md`
- the target plan file

## Mission

Implement the plan faithfully. Do not improvise new semantics unless the plan is clearly broken.

This workflow is for disciplined plan execution, not freeform exploration.

## Required Inputs

You must identify one specific plan file before changing code. If the user did not name the plan and the correct target is not obvious from local context, stop and ask.

## Workflow

### 1. Reconstruct the plan as executable slices

Break the plan into small, reviewable slices:

- each slice should have explicit tests
- each slice should have a natural commit boundary
- each slice should touch a coherent subset of files

Update the plan's `## Progress` section before starting the first slice.

### 1.5 Parallelize by dependency graph, not wishful thinking

Look for disjoint write slices that can proceed in parallel inside the same model or with subagents.

Good candidates:

- AST or typed IR definition changes
- typechecker updates after the shared shape is settled
- codegen follow-through after semantics are fixed
- LSP or editor-facing fallout after diagnostics and spans are stable
- mechanical fixture or snapshot expansion

Do not parallelize slices that are secretly blocked on unresolved shared data shapes.

### 2. Read the exact touched areas

Inspect the specific modules, fixtures, snapshots, and neighboring tests named in the plan. Do this in parallel when possible.

Also inspect current uncommitted work so you do not overwrite unrelated edits.

### 3. Follow strict test-first execution

For each slice, follow the RED/GREEN/REFACTOR discipline from `CLAUDE.md`. In practice, that means: encode the slice in failing tests or fixtures first, implement the minimum code required, and use focused verification while the slice is in flight.

### 4. Keep the plan current

Append progress entries for meaningful checkpoints:

- test-first slice started
- failing tests observed
- implementation green
- commit created
- major deviation considered and rejected

The progress section is part of the deliverable, not optional bookkeeping.

### 5. Commit in small steps

Each meaningful green slice should usually end in a small commit.

Use the commit message rules from `CLAUDE.md` plus the AI co-author trailer rules from `.ai-rules/references/marmoset-feature-workflows.md`.

### 6. Final verification before push

Before any push step, switch from focused slice verification to the broader final verification cadence from `CLAUDE.md`, including the final integration coverage and quality gate.

## Parallelism

Use parallel help aggressively for non-blocking work:

- read-only analysis of touched modules
- locating similar fixtures
- drafting mechanical fixture expansions
- reviewing diagnostics or snapshot updates
- implementing disjoint subsystems once their shared contracts are explicit

Do not delegate the immediate blocking coding step if you need the result to continue.

## Non-Negotiable Rules

- Implement the plan as written unless it is clearly broken.
- If the plan is clearly broken, stop and ask instead of silently deviating.
- Do not over-complicate the code.
- Prefer the simplest design that satisfies the documented semantics.
- Preserve soundness and diagnostic quality.

## Recommended Companion Flow

Once the first implementation commit exists, encourage a parallel `feature-review` run so the implementation is reviewed incrementally rather than only at the end.

## Final Output

Report:

- the plan file you followed
- which slices were completed
- what verification ran
- whether final pre-push verification was completed or still pending
