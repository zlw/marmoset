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

search_fixed() {
  local needle="$1"
  shift
  if command -v rg >/dev/null 2>&1; then
    rg -Fq -- "$needle" "$@"
  else
    grep -Fq -- "$needle" "$@"
  fi
}

reject_fixed() {
  local needle="$1"
  shift
  if search_fixed "$needle" "$@"; then
    echo "Forbidden editor launcher fallback found: $needle" >&2
    exit 1
  fi
}

search_fixed 'MARMOSET_ROOT' "$NVIM_DIR/lua/marmoset/init.lua"
search_fixed 'return root .. "/marmoset"' "$NVIM_DIR/lua/marmoset/init.lua"
search_fixed 'MARMOSET_ROOT' "$NVIM_DIR/lsp/marmoset.lua"
search_fixed 'vim.env.MARMOSET_ROOT .. "/marmoset"' "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed 'std/prelude.mr' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed 'find_marmoset_root' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed 'resolve_lsp_cmd' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed 'cmd = { "marmoset", "lsp" }' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed 'opts.lsp.cmd' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed '_build/default/bin/main.exe' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
reject_fixed '_build/install/default/bin/marmoset' "$NVIM_DIR/lua/marmoset/init.lua" "$NVIM_DIR/lsp/marmoset.lua"
