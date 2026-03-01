# Marmoset

Marmoset is a statically typed programming language implemented in OCaml.
It typechecks source programs and compiles them to Go.

## Current Scope

- Type inference with optional annotations
- Primitive types, arrays, and maps
- Unions and narrowing
- Enums and pattern matching
- Structural records with row-polymorphic behavior
- Traits with static method dispatch
- Code generation to Go binaries

## Requirements

- OCaml + opam + dune
- Go (required for `marmoset run` and `marmoset build`)

## Setup

Install dependencies:

```sh
make install
```

Build the release executable:

```sh
make release
```

This creates `./marmoset`.

## Usage

Run a program:

```sh
./marmoset examples/fibonacci-typed.mr
```

Compile Marmoset source to a Go-built binary:

```sh
./marmoset build examples/fibonacci-typed.mr -o fibonacci
./fibonacci
```

Emit generated Go source alongside build:

```sh
./marmoset build examples/fibonacci-typed.mr -go out
```

Build an optimized release binary (strips symbols, trims paths):

```sh
./marmoset release examples/fibonacci-typed.mr -o fibonacci
```

## Examples

Core:

- `examples/fibonacci.mr`
- `examples/fibonacci-typed.mr`
- `examples/indexing.mr`
- `examples/indexing-typed.mr`
- `examples/sum.mr`
- `examples/sum-typed.mr`
- `examples/monkey.mr`

Feature-focused typed examples:

- `examples/unions-typed.mr`
- `examples/enums-typed.mr`
- `examples/traits-typed.mr`
- `examples/records-typed.mr`

Run any example with:

```sh
./marmoset examples/<file>.mr
```

## Testing

Run unit tests:

```sh
make unit
```

Run integration suites:

```sh
./test/integration.sh
```

Run a specific integration suite:

```sh
./test/integration.sh traits
```

## CI Policy

PR checks are driven by `.github/workflows/pr-ci.yml` and run on pull request updates (`opened`, `reopened`, `synchronize`, `ready_for_review`).

- Compiler changes run:
  - `PR CI / compiler-unit`
  - `PR CI / compiler-integration-linux`
  - `PR CI / quality-lint-fmt-doc`
- LSP changes run:
  - `PR CI / lsp-unit`
  - `PR CI / lsp-integration`
  - `PR CI / quality-lint-fmt-doc`
- Editor changes run only the changed editor check:
  - `PR CI / editor-zed`
  - `PR CI / editor-vscode`
  - `PR CI / editor-nvim`
  - `PR CI / editor-jetbrains`

Before merging to `main`, manually run `.github/workflows/pr-ci.yml` via `workflow_dispatch` on the PR head so both required checks pass:

- `PR CI / compiler-integration-macos`
- `PR CI / compiler-integration-windows`

Local command equivalents used by CI:

```sh
make ci-compiler-unit
make ci-compiler-integration-linux
make ci-lsp-unit
make ci-lsp-integration
make ci-quality-lint-fmt-doc
make ci-editor-zed
make ci-editor-vscode
make ci-editor-nvim
make ci-editor-jetbrains
```

## Documentation

- `docs/INDEX.md`
- `docs/ARCHITECTURE.md`
- `docs/ROADMAP.md`
- `docs/features/`

## Status

This repository is under active development, expect frequent breaking changes.
