---
name: issue-fix
description: Fix a specific Marmoset bug from an error message, failing code sample, screenshot, issue description, or external report by reproducing it, writing a failing test first, and landing a small verified fix with disciplined commits.
disable-model-invocation: true
---

# Issue Fix

Before fixing, read:

- `.ai-rules/references/marmoset-feature-workflows.md`
- `CLAUDE.md`

## Mission

Take a concrete bug report and turn it into a reproduced failure, a failing test, and a verified fix.

## Accepted Inputs

This workflow can start from:

- an error message
- a code sample
- a screenshot transcribed into text
- a GitHub issue description
- a failing fixture path
- a failing snapshot or integration command

If the bug report is too vague to reproduce safely, stop and ask for the missing detail.

## Workflow

### 1. Build the smallest reliable repro

Extract the minimal failing case and determine where it belongs:

- inline unit test
- fixture
- snapshot
- focused integration case

### 2. Reproduce before fixing

Confirm the failure locally before proposing a code change.

### 3. Add the failing test first

Encode the repro in the strongest local test form available and verify it fails for the expected reason.

### 4. Fix the bug minimally

Implement the smallest sound fix that addresses the root cause rather than papering over the symptom.

### 5. Verify and commit

Run focused verification, update any relevant plan or review notes, and commit using the repo-wide rules from `CLAUDE.md` when the fix is green.

## Parallelism

Parallelize only the non-blocking parts:

- locating likely source modules
- finding similar tests or fixtures
- checking whether the bug matches prior regressions

Keep the actual root-cause fix local unless you can split it safely.

## Stop Conditions

Stop and ask if:

- the report implies a semantic change rather than a bug fix
- the repro is still ambiguous after local investigation
- multiple plausible fixes exist with different language semantics

## Final Output

Report:

- the repro that was encoded
- where the failing test lives
- what was fixed
- what verification ran
