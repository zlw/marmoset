#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
CACHE_ROOT="$REPO_ROOT/tools/zed-marmoset/grammars"

usage() {
  cat <<'EOF'
Usage: sync-local-grammar-cache.sh [--repo-root PATH] [--cache-root PATH]

Copy the current working tree contents of tools/tree-sitter-marmoset into the
Zed dev extension grammar cache and remove the cached wasm so Zed can rebuild it.
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --repo-root)
      REPO_ROOT="$2"
      shift 2
      ;;
    --cache-root)
      CACHE_ROOT="$2"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 1
      ;;
  esac
done

if ! command -v rsync >/dev/null 2>&1; then
  echo "sync-local-grammar-cache.sh requires rsync" >&2
  exit 1
fi

SOURCE_DIR="$REPO_ROOT/tools/tree-sitter-marmoset"
CACHE_REPO="$CACHE_ROOT/marmoset"
DEST_DIR="$CACHE_REPO/tools/tree-sitter-marmoset"
WASM_PATH="$CACHE_ROOT/marmoset.wasm"

if [ ! -d "$SOURCE_DIR" ]; then
  echo "Missing grammar source directory: $SOURCE_DIR" >&2
  exit 1
fi

if [ ! -d "$CACHE_REPO/.git" ]; then
  echo "Missing Zed grammar cache at $CACHE_REPO" >&2
  echo "Install the dev extension once before syncing the local grammar cache." >&2
  exit 1
fi

mkdir -p "$DEST_DIR"
rsync -a --delete \
  --exclude '.git' \
  --exclude 'node_modules' \
  --exclude 'bindings/node' \
  --exclude 'build' \
  --exclude 'target' \
  "$SOURCE_DIR/" \
  "$DEST_DIR/"

rm -f "$WASM_PATH"

echo "Synced local grammar working tree into $DEST_DIR"
echo "Removed cached wasm at $WASM_PATH"
echo "Reload or reinstall the dev extension in Zed if the grammar does not refresh automatically."
