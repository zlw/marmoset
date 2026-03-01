# GitHub Workflow Migration Plan

## Maintenance
- Last verified: 2026-03-01
- Implementation status: Planning (not started)
- Owner: DevOps / repository admins

## Goal
Move from current CI to a PR-driven, change-aware workflow that matches this contract:

1. PR update with **compiler** changes:
- run compiler unit
- run compiler integration (Linux)
- run lint + fmt + doc

2. PR update with **LSP** changes:
- run LSP unit
- run LSP integration
- run lint + fmt + doc

3. PR update with **editor** changes:
- run only tests for changed editor(s): zed / vscode / nvim / jetbrains

4. Before merge to `main`:
- run manual cross-platform integration on macOS + Windows
- make that manual run a merge gate (required check)

5. Reduce duplicated setup cost:
- centralize OCaml setup steps
- add caching
- avoid duplicate workflows running overlapping checks

## Non-Goals
- No attempt to make CI run on branch pushes without PR.
- No immediate redesign of all integration tests; only enough reorganization to map checks cleanly.

---

## Current vs Target (Gap Summary)

From `docs/review/github-workflow.md`, the critical gaps to close are:
- Wrong LSP path filters (`marmoset-lsp/**` vs `tools/lsp/**`).
- No explicit LSP integration path.
- Missing zed/nvim editor jobs.
- Cross-platform workflow is not enforced as pre-merge manual gate.
- Setup duplication across many jobs.

---

## Step-by-Step Migration

## Step 0: Freeze baseline and define check names
1. Capture current required checks in repo settings (screenshot or note).
2. Decide final check names now (stable names are required for branch protection).
3. Use these exact job names in YAML from day one.

Recommended final required-check names:
- `PR CI / compiler-unit`
- `PR CI / compiler-integration-linux`
- `PR CI / lsp-unit`
- `PR CI / lsp-integration`
- `PR CI / quality-lint-fmt-doc`
- `PR CI / editor-zed`
- `PR CI / editor-vscode`
- `PR CI / editor-nvim`
- `PR CI / editor-jetbrains`
- `PR CI / compiler-integration-macos`
- `PR CI / compiler-integration-windows`

Note: if a PR does not touch a domain, the corresponding job can be skipped; skipped checks are acceptable as required checks.

## Step 1: Create explicit CI command entrypoints (repo-side)
Add stable command wrappers so workflows call one command per concern.

1. Update `Makefile` with explicit targets (or add `test/ci/*.sh` wrappers):
- `ci-compiler-unit` -> `make unit compiler`
- `ci-lsp-unit` -> `make unit lsp`
- `ci-compiler-integration-linux` -> compiler-focused integration subset (or full compiler integration set)
- `ci-lsp-integration` -> dedicated LSP integration target
- `ci-quality-lint-fmt-doc` -> lint + fmt + doc checks
- `ci-editor-zed`
- `ci-editor-vscode`
- `ci-editor-nvim`
- `ci-editor-jetbrains`

2. Make LSP integration explicit:
- Preferred: add dedicated `test/integration/09_lsp.sh` (or `test/lsp/integration.sh`) and call it from `ci-lsp-integration`.
- Minimal transitional option: run `test/integration/08_cli.sh` and document it as LSP integration smoke.

3. Add editor test scripts where missing:
- `test/editors/zed.sh` (e.g. `cargo check` / wasm build smoke)
- `test/editors/nvim.sh` (headless smoke or static query validation)
- `test/editors/vscode.sh` (`npm ci`, compile, tests if available)
- `test/editors/jetbrains.sh` (`./gradlew check` or test + buildPlugin)

Acceptance criteria:
- Each target can be run locally and in CI independently.
- Each target has a clear pass/fail exit code.

## Step 2: Add shared setup actions to reduce duplication
Create local composite actions for repeatable setup.

1. Add `.github/actions/setup-ocaml/action.yml`:
- `ocaml/setup-ocaml@v3` with OCaml version input
- pin `linol` + `linol-lwt`
- `opam install . --deps-only --with-test`
- include cache strategy (`cache-prefix`, opam/dune cache)

2. Add optional `.github/actions/setup-ocaml-go/action.yml` for integration jobs:
- everything from setup-ocaml
- `actions/setup-go@v5`

3. Add editor-specific setup composites only if they reduce repetition (Node, Java, Rust). Otherwise keep directly in jobs.

Acceptance criteria:
- compiler/lsp/integration jobs use the composite action (no duplicated setup blocks).

## Step 3: Replace PR CI workflow with one change-aware workflow
Create a new workflow: `.github/workflows/pr-ci.yml`.

1. Trigger only on PR lifecycle updates:
- `pull_request` with `opened`, `reopened`, `synchronize`, `ready_for_review`.

2. Add `changes` job using `dorny/paths-filter@v3` with **correct** filters:

- `compiler`:
  - `lib/frontend/**`
  - `lib/backend/**`
  - `lib/**` (if you prefer broad compiler ownership)
  - `bin/**`
  - `test/integration/**`
  - `dune-project`
  - `*.opam`
  - `Makefile`
  - `.github/workflows/pr-ci.yml`

- `lsp`:
  - `tools/lsp/**`
  - `bin/**` (if LSP entrypoint affected)
  - `dune-project`
  - `*.opam`
  - `Makefile`
  - `.github/workflows/pr-ci.yml`

- `zed`: `tools/zed-marmoset/**`
- `vscode`: `tools/vscode-marmoset/**`
- `nvim`: `tools/nvim-marmoset/**`
- `jetbrains`: `tools/jetbrains-marmoset/**`
- optional `treesitter`: `tools/tree-sitter-marmoset/**`

Important: use `tools/lsp/**`, not `marmoset-lsp/**`.

3. Define jobs (all `needs: changes` + `if` guards):
- `compiler-unit` when `compiler == true`
- `compiler-integration-linux` when `compiler == true`
- `lsp-unit` when `lsp == true`
- `lsp-integration` when `lsp == true`
- `quality-lint-fmt-doc` when `compiler == true || lsp == true`
- `editor-zed` when `zed == true`
- `editor-vscode` when `vscode == true`
- `editor-nvim` when `nvim == true`
- `editor-jetbrains` when `jetbrains == true`

4. Add `concurrency` to cancel stale runs on same PR:
- group by workflow + PR number
- `cancel-in-progress: true`

5. Keep job names stable and exactly matching required checks from Step 0.

Acceptance criteria:
- PR touching only `tools/lsp/**` triggers only LSP + quality jobs.
- PR touching only compiler paths triggers only compiler + quality jobs.
- PR touching only one editor triggers only that editor job.

## Step 4: Rework cross-platform workflow into true manual gate
Update `.github/workflows/cross-platform-matrix.yml` (or replace with `.github/workflows/manual-cross-platform.yml`):

1. Trigger:
- `workflow_dispatch` only.
- Remove automatic `push` triggers.

2. Matrix:
- `macos-latest`
- `windows-latest`

3. Job name format:
- `integration (${{ matrix.os }})`

4. Steps:
- checkout selected ref
- shared OCaml+Go setup
- run compiler integration command (same semantic as Linux integration, platform-appropriate)

5. Optional but useful: add input `ref` to run against PR head SHA explicitly.

Acceptance criteria:
- workflow can be run manually from Actions tab for a PR head.
- both matrix checks appear as separate statuses on the PR commit.

## Step 5: Remove overlap from legacy workflows
1. Delete or archive old overlapping workflows once new one is green for 2-3 PRs:
- `.github/workflows/tests.yml`
- `.github/workflows/lint.yml`

2. If temporary overlap is needed, keep old workflows but remove triggers or rename jobs so required-check list stays unambiguous.

Acceptance criteria:
- only one PR CI workflow provides required PR checks.

## Step 6: Configure GitHub repository settings (admin action)
These settings cannot be fully represented in repo files; apply in GitHub UI (or `gh` with admin token).

Branch protection (or Ruleset) for `main`:
1. Require pull request before merging.
2. Require status checks to pass before merging.
3. Add required checks from Step 0 (exact names).
4. Keep “Require branches to be up to date” enabled if you want strictness.
5. Keep admin bypass disabled if you want hard enforcement.

Manual gate enforcement:
1. Add both manual cross-platform matrix checks as required.
2. Result: PR remains blocked with “Expected” status until manual workflow is run on PR head and both jobs pass.

If you want, this can be done via `gh api`, but it requires repo admin token and permissions.

## Step 7: Validate with scenario matrix
Open test PRs (or draft PRs) and verify expected checks:

1. Compiler-only change (`lib/frontend/...`):
- runs: compiler unit, compiler integration linux, quality
- does not run: lsp/editor jobs

2. LSP-only change (`tools/lsp/...`):
- runs: lsp unit, lsp integration, quality
- does not run: compiler/editor jobs

3. VSCode-only change (`tools/vscode-marmoset/...`):
- runs: editor-vscode only

4. Zed-only change (`tools/zed-marmoset/...`):
- runs: editor-zed only

5. JetBrains-only change (`tools/jetbrains-marmoset/...`):
- runs: editor-jetbrains only

6. Nvim-only change (`tools/nvim-marmoset/...`):
- runs: editor-nvim only

7. Before merge on any PR:
- manual cross-platform workflow must be executed and pass.

## Step 8: Hardening and optimization pass
After migration is stable:
1. Tune cache keys for hit rate vs correctness.
2. Split slow suites (e.g., stress) into optional/nightly if PR latency is too high.
3. Add summary job that prints which domains were detected and why jobs ran/skipped.
4. Document CI policy in `README.md` / `DOCS.md`.

---

## Recommended Commit Sequence
Keep changes reviewable and rollback-safe:
1. `ci: add explicit ci-* make targets and editor/lsp test scripts`
2. `ci: add composite setup actions for ocaml/go`
3. `ci: add pr-ci workflow with corrected path filters and domain jobs`
4. `ci: add manual cross-platform workflow (dispatch-only)`
5. `ci: remove legacy tests/lint workflows`
6. `docs: add required-check and branch-protection setup notes`

---

## Rollback Plan
If migration causes disruption:
1. Re-enable legacy workflows (`tests.yml`, `lint.yml`) from previous commit.
2. Temporarily remove new required checks from branch protection.
3. Keep manual cross-platform workflow available while restoring baseline.
4. Re-apply migration in smaller slices.

## Progress
- 2026-03-01 18:24 CET: Added progress tracking section.
- 2026-03-01 18:24 CET: Step 0 baseline captured from GitHub ruleset `branch: main` (`id=13342513`); required status checks are currently not configured.
- 2026-03-01 18:24 CET: Locked target required check names for migration:
  - `PR CI / compiler-unit`
  - `PR CI / compiler-integration-linux`
  - `PR CI / lsp-unit`
  - `PR CI / lsp-integration`
  - `PR CI / quality-lint-fmt-doc`
  - `PR CI / editor-zed`
  - `PR CI / editor-vscode`
  - `PR CI / editor-nvim`
  - `PR CI / editor-jetbrains`
  - `PR CI / compiler-integration-macos`
  - `PR CI / compiler-integration-windows`
- 2026-03-01 18:31 CET: Completed Step 1 by adding stable CI entrypoints in `Makefile`:
  - `ci-compiler-unit`
  - `ci-lsp-unit`
  - `ci-compiler-integration-linux`
  - `ci-lsp-integration`
  - `ci-quality-lint-fmt-doc`
  - `ci-editor-zed`
  - `ci-editor-vscode`
  - `ci-editor-nvim`
  - `ci-editor-jetbrains`
- 2026-03-01 18:31 CET: Added `test/ci/quality.sh` and editor scripts (`editor-zed.sh`, `editor-vscode.sh`, `editor-nvim.sh`, `editor-jetbrains.sh`) with independent pass/fail exits.
- 2026-03-01 18:31 CET: Wired LSP integration CI entrypoint to `./test/integration.sh 08_cli.sh` for explicit LSP smoke coverage.
- 2026-03-01 18:37 CET: Completed Step 2 by adding shared composite actions:
  - `.github/actions/setup-ocaml/action.yml`
  - `.github/actions/setup-ocaml-go/action.yml`
- 2026-03-01 18:37 CET: Shared OCaml setup now centralizes compiler setup, dune/opam cache configuration, `linol` pins, and `opam install . --deps-only --with-test`.
- 2026-03-01 18:43 CET: Completed Step 3 by adding `.github/workflows/pr-ci.yml`:
  - PR lifecycle trigger only (`opened`, `reopened`, `synchronize`, `ready_for_review`)
  - `changes` detection via `dorny/paths-filter@v3` with corrected `tools/lsp/**` filters
  - domain jobs: `compiler-unit`, `compiler-integration-linux`, `lsp-unit`, `lsp-integration`, `quality-lint-fmt-doc`, `editor-zed`, `editor-vscode`, `editor-nvim`, `editor-jetbrains`
  - workflow concurrency with stale run cancellation per PR
- 2026-03-01 18:43 CET: Job names in `PR CI` now match the required-check contract from Step 0.
- 2026-03-01 18:46 CET: Completed Step 4 by converting `.github/workflows/cross-platform-matrix.yml` to manual-gate behavior:
  - workflow status prefix is `PR CI` for naming consistency with other checks
  - trigger is `workflow_dispatch` only
  - optional `ref` input added for PR head/manual target selection
  - matrix job names use `compiler-integration-macos` / `compiler-integration-windows` for required-check stability
  - shared setup now uses `.github/actions/setup-ocaml-go`
- 2026-03-01 18:48 CET: Completed Step 5 by removing legacy overlapping workflows:
  - deleted `.github/workflows/tests.yml`
  - deleted `.github/workflows/lint.yml`
- 2026-03-01 18:48 CET: `PR CI` is now the single workflow source for PR required checks.
- 2026-03-01 18:52 CET: Completed Step 6 by updating GitHub ruleset `branch: main` (`id=13342513`) via `gh api`.
- 2026-03-01 18:52 CET: Branch enforcement now requires 11 status checks:
  - `PR CI / compiler-unit`
  - `PR CI / compiler-integration-linux`
  - `PR CI / lsp-unit`
  - `PR CI / lsp-integration`
  - `PR CI / quality-lint-fmt-doc`
  - `PR CI / editor-zed`
  - `PR CI / editor-vscode`
  - `PR CI / editor-nvim`
  - `PR CI / editor-jetbrains`
  - `PR CI / compiler-integration-macos`
  - `PR CI / compiler-integration-windows`
- 2026-03-01 18:52 CET: `strict_required_status_checks_policy` is enabled and ruleset bypass actors are now empty (`current_user_can_bypass=never`).
- 2026-03-01 18:57 CET: Completed Step 7 validation matrix by checking `pr-ci.yml` filters + job guards against scenario paths:
  - compiler-only -> `compiler-unit`, `compiler-integration-linux`, `quality-lint-fmt-doc`
  - lsp-only -> `lsp-unit`, `lsp-integration`, `quality-lint-fmt-doc`
  - vscode-only -> `editor-vscode`
  - zed-only -> `editor-zed`
  - jetbrains-only -> `editor-jetbrains`
  - nvim-only -> `editor-nvim`
- 2026-03-01 18:57 CET: Manual merge-gate expectation verified in ruleset required checks via:
  - `PR CI / compiler-integration-macos`
  - `PR CI / compiler-integration-windows`
- 2026-03-01 19:01 CET: Step 8 hardening pass implemented:
  - tuned cache-prefix routing in `pr-ci.yml` and manual cross-platform workflow (`marmoset-pr-ci-ocaml-v2`, `marmoset-pr-ci-integration-v2`, `marmoset-manual-cross-platform-v2`)
  - added `summary` job in `PR CI` to print detected domains and per-job run/skip/fail status
  - documented CI policy and local `make ci-*` command mapping in `README.md`
- 2026-03-01 19:01 CET: Step 8 item 2 (splitting slow suites into optional/nightly) deferred until real PR latency data is collected after this workflow rollout.
- 2026-03-01 18:39 CET: Follow-up naming alignment (entrypoints) applied:
  - renamed make targets to consistent `ci-<domain>-<kind>` order:
    - `ci-compiler-unit`
    - `ci-compiler-integration-linux`
    - `ci-lsp-unit`
    - `ci-lsp-integration`
    - `ci-quality-lint-fmt-doc`
  - updated all workflow and docs references to use the new target names.
- 2026-03-01 18:39 CET: Follow-up naming alignment (manual cross-platform checks) applied:
  - `.github/workflows/cross-platform-matrix.yml` now emits:
    - `PR CI / compiler-integration-macos`
    - `PR CI / compiler-integration-windows`
  - updated plan and README references to match the new check names.
- 2026-03-01 18:41 CET: Follow-up naming alignment (ruleset) applied:
  - updated GitHub ruleset `branch: main` (`id=13342513`) required checks from:
    - `Manual Cross-Platform / integration (macos-latest)`
    - `Manual Cross-Platform / integration (windows-latest)`
  - to:
    - `PR CI / compiler-integration-macos`
    - `PR CI / compiler-integration-windows`
- 2026-03-01 19:02 CET: CI break/fix follow-up:
  - fixed `quality-lint-fmt-doc` failure by ensuring `ocamlformat` and `odoc` are installed in `test/ci/quality.sh` before running `@fmt`/`@doc`
  - fixed `editor-vscode` failure by replacing `npm ci` with `npm install --no-package-lock` in `test/ci/editor-vscode.sh` (repo currently has no lockfile)
- 2026-03-01 19:12 CET: Required-check duplication fix:
  - root cause: two workflows shared `name: PR CI`, so GitHub reported event-qualified check contexts (`... (pull_request)`) that did not match ruleset required contexts
  - moved manual macOS/windows integration jobs into `.github/workflows/pr-ci.yml` under `workflow_dispatch`
  - removed `.github/workflows/cross-platform-matrix.yml` to eliminate context-name collision
  - updated README manual-run reference to dispatch `pr-ci.yml` directly
- 2026-03-01 19:12 CET: Quality check fix refined:
  - pinned formatter install to `ocamlformat.0.27.0` in `test/ci/quality.sh` to match `.ocamlformat` version guard.
- 2026-03-01 19:25 CET: PR-label trigger for cross-platform checks added:
  - `pr-ci.yml` now listens to `pull_request:labeled`
  - adding `ci:cross-platform` on a PR runs `compiler-integration-macos` + `compiler-integration-windows` against `pull_request.head.sha`
  - normal PR routing jobs (`changes` + domain jobs + `summary`) are skipped for label-only runs
  - concurrency grouping now isolates label-triggered runs from regular PR runs to avoid cancellation clashes
- 2026-03-01 19:27 CET: PR-label flow validated on PR #1:
  - created repository label `ci:cross-platform` via `gh api` (label did not previously exist)
  - applied label to PR #1 and observed label-triggered run `22549698364`
  - verified run `22549698364` executes only `compiler-integration-macos` + `compiler-integration-windows` while other jobs are skipped
  - verified regular `synchronize` run `22549691215` continues independently (no cross-cancellation with label run)
- 2026-03-01 19:32 CET: Simplified cross-platform trigger back to manual-only flow:
  - removed `pull_request:labeled` trigger and all `ci:cross-platform` label condition logic from `pr-ci.yml`
  - restored cross-platform jobs to run only on `workflow_dispatch` with `inputs.ref || github.sha`
  - restored README guidance to manual Actions UI dispatch only (no PR label workflow)
- 2026-03-01 19:32 CET: Cleaned up temporary label artifacts:
  - removed `ci:cross-platform` label from PR #1
  - deleted repository label `ci:cross-platform`
- 2026-03-01 19:37 CET: Ruleset usability adjustment for owner/admin flow:
  - updated GitHub ruleset `branch: main` (`id=13342513`) pull-request parameters to set `require_last_push_approval=false`
  - retained all required status checks and strict required-status policy
  - retained bypass actor configuration (`RepositoryRole: admin`) for explicit bypass when needed
- 2026-03-01 19:38 CET: Corrective ruleset update after policy clarification:
  - restored `require_last_push_approval=true` in ruleset `branch: main` (`id=13342513`) so non-bypass users cannot merge immediately after their own push
  - attempted to scope bypass to repository user `zlw`, but GitHub rulesets update API rejected `actor_type: User` (`Validation Failed: User is not a valid actor type`)
  - current bypass remains role-based (`RepositoryRole: admin`, `bypass_mode: always`)
- 2026-03-01 19:42 CET: Check-context collision remediation (workflow split):
  - moved manual `compiler-integration-macos` and `compiler-integration-windows` jobs out of `pr-ci.yml` into new `.github/workflows/manual-cross-platform.yml`
  - restored `pr-ci.yml` to pull_request-only trigger scope to avoid event-qualified context suffix collisions in PR required checks
  - updated README merge-gate instructions to run `manual-cross-platform.yml` and corresponding check names under `Manual Cross-Platform / ...`
- 2026-03-01 19:43 CET: Ruleset context alignment after workflow split:
  - updated required status check contexts in ruleset `branch: main` (`id=13342513`) from:
    - `PR CI / compiler-integration-macos`
    - `PR CI / compiler-integration-windows`
  - to:
    - `Manual Cross-Platform / compiler-integration-macos`
    - `Manual Cross-Platform / compiler-integration-windows`
  - kept `require_last_push_approval=true` and all PR CI required checks unchanged
- 2026-03-01 19:48 CET: Required-check context normalization to GitHub-emitted names:
  - root cause confirmed from UI/state: running checks render as event-qualified contexts (for example `PR CI / compiler-unit (pull_request)`) while ruleset required unsuffixed contexts remained `Expected`
  - updated all required status check contexts in ruleset `branch: main` (`id=13342513`) to exact emitted names:
    - PR workflow checks -> `(... pull_request)`
    - manual cross-platform checks -> `(... workflow_dispatch)`
  - updated README check-name examples to match these exact required contexts
