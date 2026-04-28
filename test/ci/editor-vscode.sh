#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VSCODE_DIR="$REPO_ROOT/tools/vscode-marmoset"

cd "$VSCODE_DIR"
npm install --no-package-lock
npm run compile
npm run check-grammar

if command -v rg >/dev/null 2>&1; then
  rg -Fq 'MARMOSET_ROOT' src/extension.ts
  rg -Fq 'path.join(root, "std", "prelude.mr")' src/extension.ts
  rg -Fq 'path.join(marmosetRoot, "marmoset")' src/extension.ts
  ! rg -Fq '_build/default/bin/main.exe' src/extension.ts
  ! rg -Fq '_build/install/default/bin/marmoset' src/extension.ts
else
  grep -Fq 'MARMOSET_ROOT' src/extension.ts
  grep -Fq 'path.join(root, "std", "prelude.mr")' src/extension.ts
  grep -Fq 'path.join(marmosetRoot, "marmoset")' src/extension.ts
  ! grep -Fq '_build/default/bin/main.exe' src/extension.ts
  ! grep -Fq '_build/install/default/bin/marmoset' src/extension.ts
fi
