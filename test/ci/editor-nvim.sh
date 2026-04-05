#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
NVIM_DIR="$REPO_ROOT/tools/nvim-marmoset"

required_files=(
  "ftdetect/marmoset.lua"
  "ftplugin/marmoset.lua"
  "lua/marmoset/init.lua"
  "queries/marmoset/folds.scm"
  "queries/marmoset/highlights.scm"
  "queries/marmoset/indents.scm"
  "queries/marmoset/textobjects.scm"
)

for rel_path in "${required_files[@]}"; do
  full_path="$NVIM_DIR/$rel_path"
  if [ ! -s "$full_path" ]; then
    echo "Missing or empty required file: $full_path" >&2
    exit 1
  fi
done

# Parse Lua files when luac is present; otherwise keep static file checks.
if command -v luac >/dev/null 2>&1; then
  luac -p "$NVIM_DIR/ftdetect/marmoset.lua"
  luac -p "$NVIM_DIR/ftplugin/marmoset.lua"
  luac -p "$NVIM_DIR/lua/marmoset/init.lua"
fi

if command -v rg >/dev/null 2>&1; then
  rg -Fq 'MARMOSET_ROOT' "$NVIM_DIR/lua/marmoset/init.lua"
  rg -Fq 'std/prelude.mr' "$NVIM_DIR/lua/marmoset/init.lua"
else
  grep -Fq 'MARMOSET_ROOT' "$NVIM_DIR/lua/marmoset/init.lua"
  grep -Fq 'std/prelude.mr' "$NVIM_DIR/lua/marmoset/init.lua"
fi
