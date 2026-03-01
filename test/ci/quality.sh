#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$REPO_ROOT"

# Format, docs, and opam metadata lint.
dune build --root "$REPO_ROOT" @fmt
dune build --root "$REPO_ROOT" @doc
opam lint marmoset.opam
