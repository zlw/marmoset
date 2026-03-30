#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
EXTENSION_DIR="$REPO_ROOT/tools/zed-marmoset"
RESET_CACHE=0

usage() {
  cat <<'EOF'
Usage: set-grammar-source.sh <pinned|local> [--reset-cache] [--repo-root PATH] [--extension-dir PATH]

Modes:
  pinned  Restore the checked-in portable manifest that fetches the grammar from GitHub.
  local   Point the grammar repository at the current local repo with a file:// URL and HEAD rev.

Options:
  --reset-cache      Remove the extension grammar cache after writing the manifest.
  --repo-root PATH   Override the repo root used for local mode.
  --extension-dir PATH
                     Override the extension directory that receives extension.toml.
EOF
}

if [ "$#" -lt 1 ]; then
  usage >&2
  exit 1
fi

MODE="$1"
shift

case "$MODE" in
  pinned|local) ;;
  *)
    usage >&2
    exit 1
    ;;
esac

while [ "$#" -gt 0 ]; do
  case "$1" in
    --reset-cache)
      RESET_CACHE=1
      shift
      ;;
    --repo-root)
      REPO_ROOT="$2"
      shift 2
      ;;
    --extension-dir)
      EXTENSION_DIR="$2"
      shift 2
      ;;
    *)
      usage >&2
      exit 1
      ;;
  esac
done

OUTPUT="$EXTENSION_DIR/extension.toml"
python3 "$SCRIPT_DIR/render_extension_manifest.py" --mode "$MODE" --repo-root "$REPO_ROOT" --output "$OUTPUT"

if [ "$RESET_CACHE" -eq 1 ] && [ -d "$EXTENSION_DIR/grammars" ]; then
  rm -rf "$EXTENSION_DIR/grammars"
fi

if [ "$MODE" = "local" ]; then
  REV="$(git -C "$REPO_ROOT" rev-parse HEAD)"
  echo "Wrote local file:// grammar manifest to $OUTPUT"
  echo "Grammar source: $REPO_ROOT"
  echo "Grammar revision: $REV"
  if [ "$RESET_CACHE" -eq 1 ]; then
    echo "Removed $EXTENSION_DIR/grammars to force Zed to rebuild the grammar cache."
  fi
  echo "For uncommitted grammar edits, run tools/zed-marmoset/scripts/sync-local-grammar-cache.sh after the dev extension is installed."
else
  echo "Restored pinned grammar manifest at $OUTPUT"
  if [ "$RESET_CACHE" -eq 1 ]; then
    echo "Removed $EXTENSION_DIR/grammars to force Zed to rebuild the pinned grammar cache."
  fi
fi
