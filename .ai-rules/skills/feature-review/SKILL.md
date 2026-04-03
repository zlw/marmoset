---
name: feature-review
description: Review a Marmoset implementation against its plan using both Codex and Claude when available, write separate review notes under docs/review/<plan-slug>/, then fix high-confidence findings with the same TDD and commit discipline used for implementation.
disable-model-invocation: true
---

# Feature Review

Before reviewing, read:

- `.ai-rules/references/marmoset-feature-workflows.md`
- `CLAUDE.md`
- the target plan file

## Mission

Review the implementation against the plan, collect adversarial findings, and then fix the proven problems.

This workflow is an orchestrator: review first, then remediate the findings that are strong enough to act on safely.

## Required Inputs

You need a specific plan file. If the correct one is not obvious from the user's message or current context, stop and ask.

## Workflow

### 1. Build the comparison set

Read in parallel:

- the plan
- touched code
- new and changed tests
- relevant fixture directories
- commit history for the current feature when applicable

### 2. Run separate reviews

When both CLIs are available:

- run one review pass from the current tool
- run one review pass from the counterpart tool

Write each review to its own file under:

- `docs/review/<plan-slug>/NN_codex-<label>.md`
- `docs/review/<plan-slug>/NN_claude-<label>.md`

If the counterpart CLI is unavailable, use a same-tool subagent and say so.

### 3. Synthesize the findings

Compare the reviews and identify:

- plan mismatches
- unjustified complexity
- likely bugs or unsound corners
- weak tests or missing fixtures
- conflicting findings that require user input

### 4. Fix what is strong enough to fix

For high-confidence findings, route the fix through the same RED/GREEN/commit loop used by `feature-implement`: reproduce the issue, encode it as a failing test or fixture, implement the smallest sound fix, and verify it before committing.

For disputed or semantics-changing findings, stop and ask instead of forcing a resolution.

## Review Questions

Always answer these explicitly:

- Was the plan implemented faithfully?
- Is the implementation simpler than it looks, or more complicated than necessary?
- Are there likely bugs or unsound corners?
- Are the tests strong enough for a compiler change?

## Final Output

Report:

- the review files written
- the highest-severity findings
- what was fixed
- any findings left open because they were ambiguous or too risky to auto-resolve
