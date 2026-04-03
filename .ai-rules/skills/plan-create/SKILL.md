---
name: plan-create
description: Create a new long-form Marmoset compiler plan under docs/plans/todo with explicit design decisions, concrete implementation touchpoints, invariants, tests, and commit boundaries. Use when starting a new compiler, type-system, module-system, tooling, or hardening effort.
disable-model-invocation: true
---

# Plan Create

Before drafting, read `.ai-rules/references/marmoset-feature-workflows.md`.

## Mission

Create a new plan that is detailed enough to implement with minimal guessing.

This workflow writes the first strong draft. Cross-model hardening belongs to `plan-review`.

## Workflow

### 1. Build context aggressively

Read in parallel when possible:

- `CLAUDE.md`
- neighboring plans in the likely target area
- relevant `docs/features/*.md`
- touched compiler modules and tests
- similar fixtures, snapshots, and integration suites

Also inspect `git status --short` so you do not plan over unrelated in-flight work.

### 2. Choose the plan path

- Default to `docs/plans/todo/<area>/<NN>_<slug>.md`
- Infer `<area>` from existing plan neighborhoods
- If ordering is not given, choose the next available two-digit prefix in the chosen area
- The slug should describe the actual capability, not the process

If placement is ambiguous after inspecting nearby plans, choose the best fit and justify it in the plan.

### 3. Draft the plan concretely

Avoid shallow bullets like "update parser" or "add tests". Instead specify:

- exact files and modules likely to change
- AST, resolver, typechecker, codegen, runtime, or diagnostic shape changes
- semantic invariants and compatibility constraints
- failure modes and required diagnostics
- unit test additions
- fixture and integration coverage
- phased commit boundaries

When ambiguity would hurt implementation, include pseudo-code, function signatures, data-shape sketches, or fixture examples.

### 4. Write the initial plan to disk

The draft must include the standard maintenance metadata and end with `## Progress`.

Seed the progress section immediately with an entry noting that the draft plan was created and is awaiting review.

### 5. Recommend the next step

Unless the user asked only for a draft, recommend a `plan-review` pass immediately after creation.

Use the plan path conventions and required plan shape from `.ai-rules/references/marmoset-feature-workflows.md`. The draft should resolve material implementation choices rather than deferring them.

## Quality Bar

Reject your own draft and rewrite it if any of these are true:

- phases are only one sentence long
- critical implementation details are left as "figure this out later"
- tests are mentioned only generically
- no concrete modules or files are named
- tradeoffs are not justified
- the plan could support multiple materially different implementations without saying which one to use

## Stop Conditions

Stop and ask the user instead of finalizing if:

- the requested scope is internally contradictory
- there are multiple plausible semantics and the codebase does not justify one
- an existing active plan should obviously be superseded or merged and the user did not say which

## Final Output

Present:

- the chosen plan path
- a short summary of the feature and why the plan is shaped this way
- any remaining open questions that blocked full certainty
- a recommendation to run `plan-review`
