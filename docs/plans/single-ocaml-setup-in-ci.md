# Single OCaml Setup in CI via Prebuilt Runner Image

## Context

Current PR CI uses shared composite actions for OCaml setup, which keeps workflow YAML DRY, but each job still runs on a fresh GitHub-hosted runner VM. That means OCaml/opam setup is repeated per job.

- Reused config: yes (`.github/actions/setup-ocaml/action.yml`, `.github/actions/setup-ocaml-go/action.yml`)
- Reused runtime environment across jobs: no (job isolation)

Goal: keep parallel jobs while avoiding repeated OCaml toolchain/bootstrap work in each OCaml-related job.

## Decision

Use a prebuilt CI container image published to GHCR and run OCaml jobs inside that image.

This gives:

1. Parallel jobs remain parallel.
2. OCaml compiler + pinned opam dependencies are pre-installed once per image build, not once per CI job.
3. PR jobs start faster and more predictably.

## Non-Goals

- No merge of all OCaml checks into one serial job.
- No migration of editor jobs (Rust/Node/Java) to this container in the first phase.
- No change to test semantics.

---

## Constraints and Tradeoffs

### Why not "setup once in one job and reuse in others"?

GitHub-hosted jobs are isolated VMs/containers. `needs:` can pass artifacts/outputs, but not a live installed environment. Reusing pre-installed tools across parallel jobs requires either:

1. single job execution (loses parallelism),
2. self-hosted persistent runners, or
3. prebuilt container images.

We choose option 3.

### Tradeoffs

Pros:

- Lower per-job startup overhead.
- Stable and reproducible toolchain/deps.
- Keeps job-level parallelism.

Cons:

- Added image build/publish workflow to maintain.
- Must manage image versioning and rebuild triggers.
- Fork PR access to private images is tricky; easiest path is public package visibility.

---

## Scope of Change

### New

1. CI image Dockerfile and supporting files under `.github/images/ci-ocaml-go/`.
2. Workflow that builds and pushes image to GHCR.
3. PR CI jobs switched to `container:` image for OCaml jobs.

### Modified

1. `.github/workflows/pr-ci.yml`:
   - OCaml jobs run in container image.
   - remove per-job OCaml setup composite steps from those jobs.
2. (Optional) simplify `.github/actions/setup-ocaml*.yml` usage if no longer needed in PR CI.

---

## Image Design

### Base

Use a stable OCaml/opam base image compatible with repository toolchain (currently OCaml 5.2.x).

Candidate:

- `ocaml/opam:ubuntu-24.04-ocaml-5.2`

### Installed components

The image should include:

1. OCaml + opam + dune (from base / opam environment)
2. pinned packages required by repository bootstrap:
   - `linol` pinned to `https://github.com/c-cube/linol.git#v0.10`
   - `linol-lwt` pinned to `https://github.com/c-cube/linol.git#v0.10`
3. repository opam deps (`opam install . --deps-only --with-test`)
4. Go toolchain for integration jobs (currently `1.22.x` equivalent)
5. basic tooling commonly needed in CI scripts (`git`, `make`, `curl`, `ca-certificates`)

### Example layout

```text
.github/images/ci-ocaml-go/
  Dockerfile
  README.md
```

### Dockerfile sketch

```dockerfile
FROM ocaml/opam:ubuntu-24.04-ocaml-5.2

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl git make rsync && \
    rm -rf /var/lib/apt/lists/*

# Install Go 1.22.x
RUN curl -fsSL https://go.dev/dl/go1.22.12.linux-amd64.tar.gz | tar -C /usr/local -xz
ENV PATH="/usr/local/go/bin:${PATH}"

USER opam
WORKDIR /home/opam/project
COPY --chown=opam:opam marmoset.opam dune-project ./

RUN opam pin add linol https://github.com/c-cube/linol.git#v0.10 -n && \
    opam pin add linol-lwt https://github.com/c-cube/linol.git#v0.10 -n && \
    opam install . --deps-only --with-test -y
```

Notes:

- Copying only dependency descriptor files keeps dependency layer cache efficient.
- Full repo checkout remains in PR jobs, not in image build stage.

---

## GHCR Publish Workflow

Add workflow, for example:

- `.github/workflows/build-ci-image.yml`

### Triggers

1. Push to `main` when dependency-related files change:
   - `*.opam`
   - `dune-project`
   - `.github/actions/setup-ocaml/action.yml`
   - `.github/images/ci-ocaml-go/**`
2. Manual dispatch for emergency rebuilds.

### Permissions

Workflow needs package write permissions:

- `packages: write`
- `contents: read`

### Build strategy

1. Build with `docker/build-push-action`.
2. Push tags:
   - immutable SHA tag: `ghcr.io/zlw/marmoset/ci-ocaml-go:<git-sha>`
   - rolling major tag: `ghcr.io/zlw/marmoset/ci-ocaml-go:v1`
3. Optionally publish metadata label with source commit and dependency lock context.

### Example outline

```yaml
name: Build CI Image
on:
  workflow_dispatch:
  push:
    branches: [main]
    paths:
      - "*.opam"
      - "dune-project"
      - ".github/images/ci-ocaml-go/**"
      - ".github/actions/setup-ocaml/action.yml"

permissions:
  contents: read
  packages: write

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - uses: docker/build-push-action@v6
        with:
          context: .
          file: .github/images/ci-ocaml-go/Dockerfile
          push: true
          tags: |
            ghcr.io/zlw/marmoset/ci-ocaml-go:${{ github.sha }}
            ghcr.io/zlw/marmoset/ci-ocaml-go:v1
```

---

## PR CI Migration Plan

### Jobs to migrate first

Move only OCaml-dependent jobs to container image:

1. `compiler-unit`
2. `compiler-integration-linux`
3. `lsp-unit`
4. `lsp-integration`
5. `quality-lint-fmt-doc`

Keep editor jobs unchanged.

### Job shape after migration

```yaml
compiler-unit:
  runs-on: ubuntu-latest
  container:
    image: ghcr.io/zlw/marmoset/ci-ocaml-go:v1
  steps:
    - uses: actions/checkout@v4
    - run: opam exec -- make ci-compiler-unit
```

### Changes to remove

For migrated jobs, remove:

- `uses: ./.github/actions/setup-ocaml`
- `uses: ./.github/actions/setup-ocaml-go`

### Cache implications

- Dune/opam cache steps become less critical when deps are baked into image.
- Keep workspace-level build caching decisions separate; initial migration can avoid extra complexity.

---

## Security and Access

### GHCR visibility

For PRs from forks, image pulls are simplest when the package is public.

If package remains private, fork PRs may fail to pull image without additional auth setup, depending on workflow permissions context.

Recommendation:

1. Publish CI base image as public GHCR package.
2. Keep source repo private/public independent of image visibility decisions.

### Provenance

Include OCI labels:

- source repo URL
- source commit SHA
- build timestamp

Optional later:

- SBOM generation
- image signing/attestation

---

## Rollout Strategy

### Phase 1: Prepare image pipeline

1. Add Dockerfile.
2. Add build/publish workflow.
3. Build and publish `v1`.

### Phase 2: Migrate one low-risk OCaml job

1. Switch `quality-lint-fmt-doc` first.
2. Validate stability and runtime improvement.

### Phase 3: Migrate remaining OCaml jobs

1. Switch compiler/lsp unit + integration jobs.
2. Keep summary job contract unchanged.

### Phase 4: Cleanup

1. Remove unused setup steps in PR CI.
2. Keep composite actions only if still used elsewhere; otherwise deprecate in follow-up.

---

## Validation Checklist

### Functional parity

1. OCaml jobs still execute with same commands (`opam exec -- make ...`).
2. All changed-domain routing in `changes` job remains intact.
3. `summary` job still reports status correctly.

### Performance

Compare before/after for at least 5 PR runs:

1. median job start-to-first-test time
2. median total duration for OCaml jobs
3. total workflow duration for compiler/lsp change PRs

### Reliability

1. image pull success rate
2. no opam dependency drift failures in migrated jobs
3. no fork PR regressions (if expected to be supported)

---

## Rollback Plan

If issues occur:

1. revert container declarations in `pr-ci.yml` for affected jobs,
2. restore shared setup composite steps,
3. keep image pipeline in place for future iteration.

Rollback is low-risk because test commands remain unchanged.

---

## Open Questions

1. Should GHCR package be public to simplify fork PR support?
2. Do we want separate images for:
   - OCaml-only jobs (`ci-ocaml`),
   - OCaml+Go jobs (`ci-ocaml-go`),
   or keep one unified image initially?
3. How aggressively should we auto-rebuild images (on every main push vs only dependency-file changes)?

Recommended initial answers:

1. public package,
2. single unified `ci-ocaml-go` image first,
3. rebuild on dependency-file changes + manual dispatch.

---

## Definition of Done

1. PR CI OCaml-related jobs run in GHCR image and no longer run per-job OCaml setup actions.
2. Parallel execution remains unchanged.
3. CI pass/fail behavior remains unchanged.
4. Median OCaml job startup time is reduced.
5. Rollback instructions are documented and tested once.
