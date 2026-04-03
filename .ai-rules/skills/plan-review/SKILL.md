---
name: plan-review
description: Cross-review an existing Marmoset plan through multiple Codex and Claude passes, tightening semantics, invariants, implementation detail, and testing expectations until the plan is ready to execute. Use after a draft plan exists.
disable-model-invocation: true
---

# Plan Review

Before reviewing, read `.ai-rules/references/marmoset-feature-workflows.md`.

## Mission

Take an existing plan and make it stronger through adversarial multi-pass review.

This workflow is for hardening a plan, not creating the first draft from nothing.

## Required Input

You need one specific plan file. If the intended file is not obvious from local context, stop and ask.

## Review Loop

Run this standard sequence:

1. The current model reviews the plan and records concrete weaknesses.
2. The counterpart model reviews the plan independently.
3. The current model reconciles both reviews and patches the plan.
4. The counterpart model reviews the revised plan again.
5. The current model does a final tightening pass and updates the plan.

If the counterpart CLI is available, use it. If not, use a same-tool subagent and say so.

## What Each Review Pass Must Check

Use the counterpart review expectations from `.ai-rules/references/marmoset-feature-workflows.md`, with extra attention to stale assumptions, hidden open questions, weak tests, and steps that are still too vague to implement safely.

## How To Patch The Plan

When revising the plan:

- make it more concrete, not longer for its own sake
- resolve ambiguity where the codebase clearly supports one choice
- add explicit file/module touchpoints
- add concrete fixture and integration expectations
- write down implementation details when multiple valid approaches exist

Do not merely append review comments. The actual plan must absorb the improvements.

## Progress Tracking

Append progress entries that record:

- review pass started
- counterpart review received
- plan revised after review
- final review completed

## Final Output

Present:

- the plan file reviewed
- the main improvements made
- any unresolved questions that still block implementation
