#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZED_DIR="$REPO_ROOT/tools/zed-marmoset"

cd "$ZED_DIR"
cargo test --locked

python3 - <<'PY'
import json
import pathlib
import re
import tomllib

data = tomllib.loads(pathlib.Path("extension.toml").read_text())
grammar = data["grammars"]["marmoset"]

assert grammar["repository"] == "https://github.com/zlw/marmoset", grammar
assert grammar["path"] == "tools/tree-sitter-marmoset", grammar
assert re.fullmatch(r"[0-9a-f]{40}", grammar["rev"]), grammar
assert "commit" not in grammar, grammar
assert data["languages"] == ["languages/marmoset"], data
assert data["lib"] == {"kind": "Rust", "version": "0.7.0"}, data

node_types = {
    item["type"]: item.get("fields", {})
    for item in json.loads(pathlib.Path("../tree-sitter-marmoset/src/node-types.json").read_text())
    if item.get("named")
}

grammar_json = json.loads(pathlib.Path("../tree-sitter-marmoset/src/grammar.json").read_text())
tokens = set()

def walk(value):
    if isinstance(value, dict):
        if value.get("type") == "STRING" and "value" in value:
            tokens.add(value["value"])
        for nested in value.values():
            walk(nested)
    elif isinstance(value, list):
        for nested in value:
            walk(nested)

walk(grammar_json)

ignored_query_nodes = {"_", "match", "eq", "not-eq", "any-of", "set!", "is?", "is-not?"}

for query_path in pathlib.Path("languages/marmoset").glob("*.scm"):
    text = re.sub(r";.*$", "", query_path.read_text(), flags=re.MULTILINE)
    unknown_nodes = sorted(
        {
            m.group(1)
            for m in re.finditer(r"\(([A-Za-z_][A-Za-z0-9_!?\-]*)", text)
            if m.group(1) not in ignored_query_nodes and m.group(1) not in node_types
        }
    )
    if unknown_nodes:
        raise AssertionError(f"{query_path}: unknown query nodes {unknown_nodes}")

    stack = []
    for line in text.splitlines():
        field_match = re.match(r"^\s+([A-Za-z_][A-Za-z0-9_]*):", line)
        if field_match and stack:
            field = field_match.group(1)
            node = stack[-1]
            if field not in node_types[node]:
                raise AssertionError(f"{query_path}: field {field!r} is invalid for node {node!r}")
        for match in re.finditer(r"\(([A-Za-z_][A-Za-z0-9_!?\-]*)", line):
            node = match.group(1)
            if node in node_types:
                stack.append(node)
        closes = line.count(")")
        while closes > 0 and stack:
            stack.pop()
            closes -= 1

    unknown_tokens = sorted(
        {
            m.group(1)
            for m in re.finditer(r'^\s*"([^"\\\\]+)"\s+@', text, flags=re.MULTILINE)
            if m.group(1) not in tokens
        }
    )
    if unknown_tokens:
        raise AssertionError(f"{query_path}: unknown query tokens {unknown_tokens}")
PY

search_fixed() {
  local needle="$1"
  local file="$2"
  if command -v rg >/dev/null 2>&1; then
    rg -Fq -- "$needle" "$file"
  else
    grep -Fq -- "$needle" "$file"
  fi
}

search_fixed 'repository = "https://github.com/zlw/marmoset"' extension.toml
search_fixed 'path = "tools/tree-sitter-marmoset"' extension.toml
search_fixed 'languages = ["languages/marmoset"]' extension.toml
search_fixed 'kind = "Rust"' extension.toml
search_fixed 'portable instead of depending on a machine-specific local `file://`' README.md
search_fixed 'set-grammar-source.sh local --reset-cache' README.md
search_fixed 'sync-local-grammar-cache.sh' README.md
search_fixed 'set-grammar-source.sh pinned' README.md
search_fixed 'remove that directory and reinstall the dev' README.md
search_fixed '"case" @keyword.conditional' languages/marmoset/highlights.scm
search_fixed '"shape" @keyword.type' languages/marmoset/highlights.scm
search_fixed '"override" @keyword.modifier' languages/marmoset/highlights.scm
search_fixed '"=>" @operator' languages/marmoset/highlights.scm
search_fixed '"&" @operator' languages/marmoset/highlights.scm
search_fixed '"%" @operator' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/highlights.scm
search_fixed '(lambda_expression' languages/marmoset/highlights.scm
search_fixed '(derive_clause' languages/marmoset/highlights.scm
search_fixed '(shape_definition' languages/marmoset/highlights.scm
search_fixed '(wrapper_type' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/outline.scm
search_fixed '(shape_definition' languages/marmoset/outline.scm
search_fixed '(type_definition' languages/marmoset/outline.scm
search_fixed 'target:' languages/marmoset/outline.scm
search_fixed 'expr_or_block' languages/marmoset/indents.scm
search_fixed 'constructor_type_body' languages/marmoset/indents.scm

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

python3 scripts/render_extension_manifest.py \
  --mode pinned \
  --repo-root "$REPO_ROOT" \
  --output "$TMP_DIR/pinned.toml"
cmp -s extension.toml "$TMP_DIR/pinned.toml"

python3 scripts/render_extension_manifest.py \
  --mode local \
  --repo-root "$REPO_ROOT" \
  --output "$TMP_DIR/local.toml"

python3 - <<'PY' "$TMP_DIR/local.toml" "$REPO_ROOT"
import pathlib
import re
import sys

manifest_path = pathlib.Path(sys.argv[1])
repo_root = pathlib.Path(sys.argv[2]).resolve()
text = manifest_path.read_text()

assert f'repository = "{repo_root.as_uri()}"' in text, text
assert 'path = "tools/tree-sitter-marmoset"' in text, text
assert re.search(r'rev = "[0-9a-f]{40}"', text), text
PY

"$ZED_DIR/scripts/set-grammar-source.sh" local \
  --repo-root "$REPO_ROOT" \
  --extension-dir "$TMP_DIR/extension"
search_fixed "repository = \"file://" "$TMP_DIR/extension/extension.toml"

python3 - <<'PY' "$TMP_DIR/repo"
import pathlib
import subprocess
import sys

repo_root = pathlib.Path(sys.argv[1])
(repo_root / "tools/tree-sitter-marmoset/src").mkdir(parents=True, exist_ok=True)
(repo_root / "tools/tree-sitter-marmoset/src/parser.c").write_text("new parser\n")
(repo_root / "tools/tree-sitter-marmoset/grammar.js").write_text("new grammar\n")
(repo_root / "tools/zed-marmoset/grammars/marmoset/tools/tree-sitter-marmoset").mkdir(parents=True, exist_ok=True)
(repo_root / "tools/zed-marmoset/grammars/marmoset/.git").mkdir(parents=True, exist_ok=True)
(repo_root / "tools/zed-marmoset/grammars/marmoset/tools/tree-sitter-marmoset/stale.txt").write_text("stale\n")
(repo_root / "tools/zed-marmoset/grammars/marmoset.wasm").write_text("wasm\n")
subprocess.run(["git", "init", str(repo_root)], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run(["git", "-C", str(repo_root), "config", "user.email", "test@example.com"], check=True)
subprocess.run(["git", "-C", str(repo_root), "config", "user.name", "Test"], check=True)
subprocess.run(["git", "-C", str(repo_root), "add", "."], check=True)
subprocess.run(["git", "-C", str(repo_root), "commit", "-m", "init"], check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
PY

"$ZED_DIR/scripts/sync-local-grammar-cache.sh" \
  --repo-root "$TMP_DIR/repo" \
  --cache-root "$TMP_DIR/repo/tools/zed-marmoset/grammars"

test -f "$TMP_DIR/repo/tools/zed-marmoset/grammars/marmoset/tools/tree-sitter-marmoset/src/parser.c"
test -f "$TMP_DIR/repo/tools/zed-marmoset/grammars/marmoset/tools/tree-sitter-marmoset/grammar.js"
test ! -e "$TMP_DIR/repo/tools/zed-marmoset/grammars/marmoset/tools/tree-sitter-marmoset/stale.txt"
test ! -e "$TMP_DIR/repo/tools/zed-marmoset/grammars/marmoset.wasm"
