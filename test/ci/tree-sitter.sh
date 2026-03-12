#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TREE_SITTER_DIR="$REPO_ROOT/tools/tree-sitter-marmoset"

cd "$TREE_SITTER_DIR"
npm install --no-package-lock
npm run generate
git diff --exit-code -- src/grammar.json src/node-types.json src/parser.c
npm test
