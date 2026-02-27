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
- Go (required for `marmoset build`)

## Setup

Install dependencies:

```sh
make install
```

Build:

```sh
make build
```

## Usage

Run a program:

```sh
dune exec marmoset -- examples/fibonacci-typed.mr
```

Start REPL:

```sh
make repl
```

Compile Marmoset source to a Go-built binary:

```sh
dune exec marmoset -- build examples/fibonacci-typed.mr -o fibonacci
./fibonacci
```

Emit generated Go source alongside build:

```sh
dune exec marmoset -- build examples/fibonacci-typed.mr --emit-go out
```

## Testing

Run unit tests:

```sh
make unit
```

Run integration suites:

```sh
./test/test_typecheck_and_codegen.sh
```

Run a specific integration suite:

```sh
./test/test_typecheck_and_codegen.sh traits
```

## Documentation

- `/Users/zlw/src/marmoset/marmoset-ml/docs/INDEX.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/ARCHITECTURE.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/ROADMAP.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/features/`

## Status

This repository is under active development and cleanup while preparing for migration to a dedicated `marmoset` repository.
