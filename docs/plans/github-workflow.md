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
- `Manual Cross-Platform / integration (macos-latest)`
- `Manual Cross-Platform / integration (windows-latest)`

Note: if a PR does not touch a domain, the corresponding job can be skipped; skipped checks are acceptable as required checks.

## Step 1: Create explicit CI command entrypoints (repo-side)
Add stable command wrappers so workflows call one command per concern.

1. Update `Makefile` with explicit targets (or add `test/ci/*.sh` wrappers):
- `ci-unit-compiler` -> `make unit compiler`
- `ci-unit-lsp` -> `make unit lsp`
- `ci-integration-compiler-linux` -> compiler-focused integration subset (or full compiler integration set)
- `ci-integration-lsp` -> dedicated LSP integration target
- `ci-quality` -> lint + fmt + doc checks
- `ci-editor-zed`
- `ci-editor-vscode`
- `ci-editor-nvim`
- `ci-editor-jetbrains`

2. Make LSP integration explicit:
- Preferred: add dedicated `test/integration/09_lsp.sh` (or `test/lsp/integration.sh`) and call it from `ci-integration-lsp`.
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
  - `Manual Cross-Platform / integration (macos-latest)`
  - `Manual Cross-Platform / integration (windows-latest)`
- 2026-03-01 18:31 CET: Completed Step 1 by adding stable CI entrypoints in `Makefile`:
  - `ci-unit-compiler`
  - `ci-unit-lsp`
  - `ci-integration-compiler-linux`
  - `ci-integration-lsp`
  - `ci-quality`
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
  - workflow name is now `Manual Cross-Platform`
  - trigger is `workflow_dispatch` only
  - optional `ref` input added for PR head/manual target selection
  - matrix job names use `integration (${{ matrix.os }})` for required-check stability
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
  - `Manual Cross-Platform / integration (macos-latest)`
  - `Manual Cross-Platform / integration (windows-latest)`
- 2026-03-01 18:52 CET: `strict_required_status_checks_policy` is enabled and ruleset bypass actors are now empty (`current_user_can_bypass=never`).
