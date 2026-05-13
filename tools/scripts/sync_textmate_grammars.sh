#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SOURCE="$REPO_ROOT/tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json"

usage() {
  echo "Usage: $0 [--check]" >&2
}

case "${1:-}" in
  "")
    echo "VS Code is the only TextMate consumer; JetBrains uses a native highlighter." >&2
    ;;
  "--check")
    if [ ! -f "$SOURCE" ]; then
      echo "Missing VS Code TextMate grammar: $SOURCE" >&2
      exit 1
    fi
    if find "$REPO_ROOT/tools/jetbrains-marmoset" \
        -path "$REPO_ROOT/tools/jetbrains-marmoset/build" -prune -o \
        \( -path '*/textmate/*' -o -name '*TextMate*' \) -print | grep -q .; then
      echo "JetBrains plugin should not depend on TextMate resources." >&2
      exit 1
    fi
    ;;
  *)
    usage
    exit 1
    ;;
esac
