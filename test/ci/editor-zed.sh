#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZED_DIR="$REPO_ROOT/tools/zed-marmoset"

cd "$ZED_DIR"
cargo check --locked

python3 - <<'PY'
import pathlib
import re
import tomllib

data = tomllib.loads(pathlib.Path("extension.toml").read_text())
grammar = data["grammars"]["marmoset"]

assert grammar["repository"] == "https://github.com/zlw/marmoset", grammar
assert grammar["path"] == "tools/tree-sitter-marmoset", grammar
assert re.fullmatch(r"[0-9a-f]{40}", grammar["rev"]), grammar
assert "commit" not in grammar, grammar
PY

search_fixed() {
  local needle="$1"
  local file="$2"
  if command -v rg >/dev/null 2>&1; then
    rg -Fq -- "$needle" "$file"
  else
    grep -Fq -- "$needle" "$file"
  fi
}

search_fixed 'repository = "https://github.com/zlw/marmoset"' extension.toml
search_fixed 'path = "tools/tree-sitter-marmoset"' extension.toml
search_fixed 'portable instead of depending on a machine-specific local `file://`' README.md
search_fixed 'remove that directory and reinstall the dev' README.md
search_fixed '"case" @keyword.conditional' languages/marmoset/highlights.scm
search_fixed '"override" @keyword.modifier' languages/marmoset/highlights.scm
search_fixed '"=>" @operator' languages/marmoset/highlights.scm
search_fixed '"&" @operator' languages/marmoset/highlights.scm
search_fixed '"%" @operator' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/highlights.scm
search_fixed '(lambda_expression' languages/marmoset/highlights.scm
search_fixed '(derive_clause' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/outline.scm
search_fixed 'target:' languages/marmoset/outline.scm
