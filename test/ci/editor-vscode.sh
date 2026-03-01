#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VSCODE_DIR="$REPO_ROOT/tools/vscode-marmoset"

cd "$VSCODE_DIR"
npm install --no-package-lock
npm run compile
