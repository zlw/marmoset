#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VSCODE_DIR="$REPO_ROOT/tools/vscode-marmoset"

cd "$VSCODE_DIR"
npm install --no-package-lock
npm run compile
npm run check-grammar

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

search_fixed 'MARMOSET_ROOT' src/extension.ts
search_fixed 'process.env.MARMOSET_ROOT' src/extension.ts
search_fixed 'path.join(marmosetRoot, "marmoset")' src/extension.ts
search_fixed 'MARMOSET_ROOT is not set; set it to the Marmoset repo root' src/extension.ts
reject_fixed 'marmoset.lsp.path' package.json src/extension.ts
reject_fixed 'lsp.path' package.json src/extension.ts
reject_fixed 'resolveMarmosetRoot' src/extension.ts
reject_fixed 'ancestorDirs' src/extension.ts
reject_fixed 'std/prelude.mr' src/extension.ts
reject_fixed 'fs.existsSync' src/extension.ts
reject_fixed 'serverPath' src/extension.ts
reject_fixed '_build/default/bin/main.exe' src/extension.ts
reject_fixed '_build/install/default/bin/marmoset' src/extension.ts
