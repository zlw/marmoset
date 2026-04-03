---
name: plan-check
description: Check whether an existing Marmoset plan is stale, actionable, blocked, or missing key decisions by comparing it against the current codebase, tests, and documentation. Use before implementation or when returning to an old plan.
disable-model-invocation: true
---

# Plan Check

Before checking, read `.ai-rules/references/marmoset-feature-workflows.md`.

## Mission

QA an existing plan without turning it into a full rewrite.

This is a read-heavy validation pass that determines whether the plan is still implementable as written.

## Required Input

You need one specific plan file. If the intended file is not obvious from local context, stop and ask.

## Workflow

### 1. Compare the plan against reality

Read in parallel:

- the plan
- touched code paths named in the plan
- neighboring plans and relevant feature docs
- current tests and fixture coverage
- recent commits in the affected area when relevant

### 2. Classify the plan

Classify it as exactly one of:

- `actionable`
- `actionable with fixes`
- `stale`
- `blocked`

### 3. Explain the classification concretely

Check for:

- stale assumptions about module layout or current semantics
- missing invariants
- implementation steps that are too vague to execute
- hidden open questions
- tests that are too generic
- missing rollback or migration detail
- plan steps that no longer match the codebase

### 4. Write a plan QA note

Write the result to:

- `docs/review/<plan-slug>/NN_plan-check.md`

Treat `docs/review/` as local scratch unless the user explicitly tells you to commit it.

### 5. Recommend the next action

Based on the classification, recommend one of:

- proceed to `feature-implement`
- run `plan-review`
- patch the plan before implementation
- stop because the plan is blocked

## Read-Only Constraint

Do not edit the plan unless the user explicitly asks for a repair after seeing the check result.

## Final Output

Present:

- the plan file checked
- the classification
- the highest-value missing or stale assumptions
- the review note path
