#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZED_DIR="$REPO_ROOT/tools/zed-marmoset"

cd "$ZED_DIR"
cargo check --locked

EXPECTED_GRAMMAR_COMMIT="d7e2341c352f8099b5768310bbe24875c9f90c7e"

rg -Fq "commit = \"$EXPECTED_GRAMMAR_COMMIT\"" extension.toml
rg -Fq 'correct `commit`' README.md
rg -Fq '"case" @keyword.conditional' languages/marmoset/highlights.scm
rg -Fq '"override" @keyword.modifier' languages/marmoset/highlights.scm
rg -Fq '"=>" @operator' languages/marmoset/highlights.scm
rg -Fq '"&" @operator' languages/marmoset/highlights.scm
rg -Fq '"%" @operator' languages/marmoset/highlights.scm
rg -Fq '(fn_declaration' languages/marmoset/highlights.scm
rg -Fq '(lambda_expression' languages/marmoset/highlights.scm
rg -Fq '(derive_clause' languages/marmoset/highlights.scm
rg -Fq '(fn_declaration' languages/marmoset/outline.scm
rg -Fq 'target:' languages/marmoset/outline.scm
