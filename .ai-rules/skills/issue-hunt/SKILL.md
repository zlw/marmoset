---
name: issue-hunt
description: Hunt for edge cases and latent bugs in an implemented Marmoset feature through three increasingly aggressive probing rounds, using both Codex and Claude when available, then fix the red cases with TDD and small commits.
disable-model-invocation: true
---

# Issue Hunt

Before hunting, read:

- `.ai-rules/references/marmoset-feature-workflows.md`
- `CLAUDE.md`
- the relevant plan file when one exists

## Mission

Probe the implemented feature until it earns confidence.

This is not a shallow smoke test. It is a structured hardening loop that creates new fixtures, finds red cases, and fixes them cleanly.

## Workflow

### 1. Inventory existing coverage first

Before inventing new cases, inspect in parallel:

- unit tests in touched compiler modules
- relevant fixture directories
- integration suites and snapshots
- neighboring features that interact with this one

The point is to avoid duplicating existing cases and to identify the current coverage ceiling.

### 2. Partition the search space before probing

When both CLIs are available, do not send both models after the same surface at once.

Split the first pass by subsystem or fixture family, for example:

- one model reviews parser and resolver edges
- the other reviews typechecker, codegen, snapshots, or diagnostics

Before the next round, let each side see the newly added fixtures so the second round explores fresh gaps instead of duplicating work.

### 3. Run three rounds of increasingly aggressive probing

#### Round 1: obvious adjacent cases

Probe the natural neighbors of the implemented feature:

- boundary cases
- nearby syntax shapes
- missing reject cases
- common diagnostics
- combinations already implied by the plan

#### Round 2: composition and interaction

Probe interactions with nearby subsystems:

- parser plus resolver
- resolver plus typechecker
- typechecker plus codegen
- feature composition with traits, modules, generics, records, pattern matching, or effects when relevant
- snapshot stability and name-collision behavior

#### Round 3: adversarial compiler cases

Probe the cases that usually expose unsoundness or brittle code:

- shadowing and collision behavior
- ambiguous or near-ambiguous syntax
- conflicting impls or coherence edges
- inference cliffs
- diagnostics under nested failures
- codegen escaping and namespace collisions
- regressions suggested by prior fixes

Each round should be more aggressive than the last.

### 4. Add new tests before fixing

When you find a proven gap, hand it through the same bug-fix discipline as `issue-fix`: encode the repro as failing coverage first, implement the minimum sound fix, rerun focused coverage, and keep the stronger tests or fixtures.

### 5. Keep progress visible

Update the target plan's `## Progress` section when a hunt produces meaningful new red or green information.

If you are running without a plan file, summarize the hunt rounds explicitly in your final report.

## What To Probe For

For compiler features, always consider:

- parsing acceptance and rejection
- name resolution and shadowing
- type inference and annotation interaction
- generalization and specialization edges
- trait or impl coherence
- exhaustiveness or reachability
- diagnostic clarity and source spans
- code generation correctness
- snapshot stability

## Parallelism

Parallelize aggressively when the work splits cleanly:

- reviewing current coverage
- designing candidate fixture sets
- inspecting likely failure sites
- drafting mechanical fixture files
- splitting probe ownership between Codex and Claude or between subagents

Integrate and fix locally once the real failures are known.

## Stop Conditions

Do not keep generating noise forever. Stop after three serious rounds unless new failures are still appearing at a materially useful rate.

## Final Output

Report:

- which feature was hunted
- which rounds ran
- what new tests were added
- what bugs were fixed
- what residual risk remains
