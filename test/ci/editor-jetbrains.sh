#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
JETBRAINS_DIR="$REPO_ROOT/tools/jetbrains-marmoset"

cd "$JETBRAINS_DIR"
./gradlew --no-daemon check

DESCRIPTOR="src/main/kotlin/com/github/zlw/marmoset/lsp/MarmosetLspServerDescriptor.kt"
PLUGIN_XML="src/main/resources/META-INF/plugin.xml"
TEXTMATE_XML="src/main/resources/META-INF/textmate.xml"
PATCHED_PLUGIN_XML="build/tmp/patchPluginXml/plugin.xml"

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

search_fixed 'System.getenv("MARMOSET_ROOT")' "$DESCRIPTOR"
search_fixed 'Path.of(marmosetRoot, "marmoset")' "$DESCRIPTOR"
search_fixed 'GeneralCommandLine(commandPath.toString(), "lsp")' "$DESCRIPTOR"
search_fixed 'MARMOSET_ROOT is not set; set it to the Marmoset repo root' "$DESCRIPTOR"
reject_fixed 'GeneralCommandLine("marmoset", "lsp")' "$DESCRIPTOR"
reject_fixed '_build/default/bin/main.exe' "$DESCRIPTOR"
reject_fixed '_build/install/default/bin/marmoset' "$DESCRIPTOR"
reject_fixed 'std/prelude.mr' "$DESCRIPTOR"

search_fixed '<depends optional="true" config-file="textmate.xml">org.jetbrains.plugins.textmate</depends>' "$PLUGIN_XML"
reject_fixed '<textmate.bundleProvider' "$PLUGIN_XML"
search_fixed '<textmate.bundleProvider' "$TEXTMATE_XML"
search_fixed 'since-build="252"' "$PATCHED_PLUGIN_XML"
reject_fixed 'until-build=' "$PATCHED_PLUGIN_XML"
