#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SOURCE="$REPO_ROOT/tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json"
TARGET="$REPO_ROOT/tools/jetbrains-marmoset/src/main/resources/textmate/syntaxes/marmoset.tmLanguage.json"

usage() {
  echo "Usage: $0 [--check]" >&2
}

case "${1:-}" in
  "")
    cp "$SOURCE" "$TARGET"
    ;;
  "--check")
    if ! cmp -s "$SOURCE" "$TARGET"; then
      echo "TextMate grammars are out of sync." >&2
      echo "Run: tools/scripts/sync_textmate_grammars.sh" >&2
      diff -u "$SOURCE" "$TARGET" || true
      exit 1
    fi
    ;;
  *)
    usage
    exit 1
    ;;
esac
