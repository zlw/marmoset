#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
JETBRAINS_DIR="$REPO_ROOT/tools/jetbrains-marmoset"

cd "$JETBRAINS_DIR"
./gradlew --no-daemon check
