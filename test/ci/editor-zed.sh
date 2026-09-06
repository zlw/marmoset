#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ZED_DIR="$REPO_ROOT/tools/zed-marmoset"
TREE_SITTER_DIR="$REPO_ROOT/tools/tree-sitter-marmoset"

search_fixed() {
  local needle="$1"
  local file="$2"
  if command -v rg >/dev/null 2>&1; then
    rg -Fq -- "$needle" "$file"
  else
    grep -Fq -- "$needle" "$file"
  fi
}

reject_fixed() {
  local needle="$1"
  local file="$2"
  if search_fixed "$needle" "$file"; then
    echo "Forbidden editor launcher fallback found: $needle" >&2
    exit 1
  fi
}

cd "$ZED_DIR"
cargo test --locked

search_fixed 'repo_root.join("marmoset")' src/marmoset.rs
reject_fixed '_build/default/bin/main.exe' src/marmoset.rs
reject_fixed '_build/install/default/bin/marmoset' src/marmoset.rs
reject_fixed 'std::env::split_paths' src/marmoset.rs
reject_fixed 'worktree.which' src/marmoset.rs
reject_fixed 'repo_dev_binary' src/marmoset.rs
reject_fixed 'launchable_binary' src/marmoset.rs
reject_fixed '.arg("--version")' src/marmoset.rs
reject_fixed 'not launchable' src/marmoset.rs

search_fixed 'repository = "https://github.com/zlw/marmoset"' extension.toml
search_fixed 'path = "tools/tree-sitter-marmoset"' extension.toml
search_fixed 'languages = ["languages/marmoset"]' extension.toml
search_fixed 'kind = "Rust"' extension.toml
search_fixed 'version = "0.7.0"' extension.toml

PINNED_REV="$(sed -n '/^\[grammars\.marmoset\]/,/^\[/s/^rev = "\(.*\)"/\1/p' extension.toml)"
if [[ ! "$PINNED_REV" =~ ^[0-9a-f]{40}$ ]]; then
  echo "Pinned grammar rev is not a full Git SHA: $PINNED_REV" >&2
  exit 1
fi

if sed -n '/^\[grammars\.marmoset\]/,/^\[/p' extension.toml | grep -Eq '^commit[[:space:]]*='; then
  echo "Zed grammar manifest must use rev, not commit" >&2
  exit 1
fi

python3 - <<'PY'
import json
import pathlib
import re

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

if ! git merge-base --is-ancestor "$PINNED_REV" HEAD; then
  echo "Pinned grammar rev is not reachable from HEAD: $PINNED_REV" >&2
  exit 1
fi

if ! git -C "$REPO_ROOT" show "${PINNED_REV}:tools/tree-sitter-marmoset/grammar.js" | grep -Fq "wrap_expression"; then
  echo "Pinned Zed grammar rev does not include bare wrap expression support: $PINNED_REV" >&2
  exit 1
fi

if ! git -C "$REPO_ROOT" show "${PINNED_REV}:tools/tree-sitter-marmoset/grammar.js" | grep -Fq '"~>"'; then
  echo "Pinned Zed grammar rev does not include effect-polymorphic arrow support: $PINNED_REV" >&2
  exit 1
fi

search_fixed 'portable instead of depending on a machine-specific local `file://`' README.md
search_fixed 'set-grammar-source.sh local --reset-cache' README.md
search_fixed 'sync-local-grammar-cache.sh' README.md
search_fixed 'set-grammar-source.sh pinned' README.md
search_fixed 'remove that directory and reinstall the dev' README.md
search_fixed '"case" @keyword.conditional' languages/marmoset/highlights.scm
search_fixed '"shape" @keyword.type' languages/marmoset/highlights.scm
search_fixed '"extern" @keyword.type' languages/marmoset/highlights.scm
search_fixed '"override" @keyword.modifier' languages/marmoset/highlights.scm
search_fixed '"wrap" @keyword' languages/marmoset/highlights.scm
search_fixed '"=>" @operator' languages/marmoset/highlights.scm
search_fixed '"~>" @operator' languages/marmoset/highlights.scm
search_fixed '"|>" @operator' languages/marmoset/highlights.scm
search_fixed '"&" @operator' languages/marmoset/highlights.scm
search_fixed '"%" @operator' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/highlights.scm
search_fixed '(lambda_expression' languages/marmoset/highlights.scm
search_fixed '(derive_clause' languages/marmoset/highlights.scm
search_fixed '(shape_definition' languages/marmoset/highlights.scm
search_fixed '(extern_type_definition' languages/marmoset/highlights.scm
search_fixed '(extern_block' languages/marmoset/highlights.scm
search_fixed '(extern_fn_signature' languages/marmoset/highlights.scm
search_fixed '(simple_wrap_target' languages/marmoset/highlights.scm
search_fixed '(qualified_wrap_target' languages/marmoset/highlights.scm
search_fixed '(wrapper_type' languages/marmoset/highlights.scm
search_fixed '(fn_declaration' languages/marmoset/outline.scm
search_fixed '(shape_definition' languages/marmoset/outline.scm
search_fixed '(extern_type_definition' languages/marmoset/outline.scm
search_fixed '(extern_fn_signature' languages/marmoset/outline.scm
search_fixed '(type_definition' languages/marmoset/outline.scm
search_fixed 'target:' languages/marmoset/outline.scm
search_fixed 'expr_or_block' languages/marmoset/indents.scm
search_fixed 'constructor_type_body' languages/marmoset/indents.scm
search_fixed 'extern_block' languages/marmoset/indents.scm

(
  cd "$TREE_SITTER_DIR"
  npm install --no-package-lock >/dev/null
  python3 - <<'PY' "$REPO_ROOT" "$TREE_SITTER_DIR"
import pathlib
import re
import subprocess
import sys
import tempfile
import textwrap

repo_root = pathlib.Path(sys.argv[1]).resolve()
tree_sitter_dir = pathlib.Path(sys.argv[2]).resolve()
zed_lang_dir = repo_root / "tools" / "zed-marmoset" / "languages" / "marmoset"

capture_re = re.compile(r"capture:\s+\d+\s+-\s+([^,]+), .*text: `(.*)`$")

def run_query(query_path: pathlib.Path, source_path: pathlib.Path):
    output = subprocess.check_output(
        ["npx", "tree-sitter", "query", "--captures", str(query_path), str(source_path)],
        cwd=tree_sitter_dir,
        text=True,
    )
    captures = []
    for line in output.splitlines():
        match = capture_re.search(line)
        if match:
            captures.append((match.group(1), match.group(2)))
    return captures

def assert_capture(captures, capture, text):
    if (capture, text) not in captures:
        raise AssertionError(f"missing capture {(capture, text)!r}")

highlights_query = zed_lang_dir / "highlights.scm"
outline_query = zed_lang_dir / "outline.scm"

with tempfile.TemporaryDirectory() as tmpdir:
    monkey_path = pathlib.Path(tmpdir) / "zed-monkey.mr"
    monkey_path.write_text(
        textwrap.dedent(
            """
            fn fibonacci(n: Int) -> Int = n

            fn print_book_name(book: Map[Str, Str], fallback: Str) => Unit = {
              puts(book["title"])
              puts(fallback)
            }

            type DecodeError = {
              Bad = "Bad decode",
            }

            type FileError = {
              InvalidData(DecodeError) = "Invalid file data",
            }

            fn decode() -> Result[Str, DecodeError] = Result.Failure(DecodeError.Bad)
            fn adapt() -> Result[Str, FileError] = decode() wrap FileError.InvalidData
            fn apply(f: (Int) ~> Int, x: Int) ~> Int = f(x)

            extern type File

            extern "std/file" as file_shim = {
              fn read(path: Str) -> Result[Bytes, FileReadError]
              fn write!(path: Str, bytes: Bytes) => Result[Unit, FileWriteError]
            }
            """
        ).strip()
        + "\n"
    )
    monkey_highlights = run_query(highlights_query, monkey_path)
    for expected in [
        ("function", "print_book_name"),
        ("variable.parameter", "book"),
        ("type.builtin", "Map"),
        ("operator", "=>"),
        ("operator", "~>"),
        ("function", "fibonacci"),
        ("keyword.type", "extern"),
        ("type", "File"),
        ("function", "read"),
        ("function", "write!"),
        ("variable.parameter", "bytes"),
        ("keyword", "wrap"),
        ("type", "FileError"),
        ("constructor", "InvalidData"),
    ]:
        assert_capture(monkey_highlights, *expected)

    module_highlights = run_query(
        highlights_query,
        repo_root
        / "test"
        / "fixtures"
        / "modules_codegen"
        / "mc04_namespace_values_and_nested_qualifiers"
        / "main.mr",
    )
    for expected in [
        ("keyword.import", "import"),
        ("type", "math"),
        ("type", "geometry"),
        ("function.method.call", "make_point"),
        ("type", "Point"),
        ("function.method.call", "distance"),
        ("type", "Show"),
        ("function.method.call", "show"),
        ("type", "Color"),
        ("constructor", "Red"),
    ]:
        assert_capture(module_highlights, *expected)

    records_path = pathlib.Path(tmpdir) / "zed-records.mr"
    records_path.write_text(
        textwrap.dedent(
            """
            import sum

            sum.sum(1, 2)
            """
        ).strip()
        + "\n"
    )
    records_highlights = run_query(highlights_query, records_path)
    for expected in [
        ("keyword.import", "import"),
        ("type", "sum"),
        ("function.method.call", "sum"),
    ]:
        assert_capture(records_highlights, *expected)

    upcase_path = pathlib.Path(tmpdir) / "zed-upcase.mr"
    upcase_path.write_text(
        textwrap.dedent(
            """
            type Option[a] = { Some(a), None }
            shape NamedDweller = { name: Str }
            trait JungleDweller[a] = { fn greet(self: a) -> Str }
            type Monkey = { name: Str }
            fn identity(x: Int) -> Int = x
            """
        ).strip()
        + "\n"
    )
    upcase_outline = run_query(outline_query, upcase_path)
    for expected in [
        ("name", "Option"),
        ("name", "NamedDweller"),
        ("name", "JungleDweller"),
        ("name", "Monkey"),
        ("name", "identity"),
    ]:
        assert_capture(upcase_outline, *expected)

    scenario_path = pathlib.Path(tmpdir) / "zed-scenario.mr"
    scenario_path.write_text(
        textwrap.dedent(
            """
            type Option[a] = {
              Some(a),
              None,
            }

            shape Named = {
              name: Str,
              age: Int,
            }

            trait Greeter[a] = {
              fn greet(self: a, prefix: Str) -> Str
            }

            type Monkey = {
              name: Str,
              age: Int,
            }

            impl Greeter[Monkey] = {
              fn greet(self: Monkey, prefix: Str) -> Str = prefix + self.name
            }

            impl Monkey = {
              fn rename(self: Monkey, next_name: Str) -> Monkey = self
            }

            let name = "Curious George"
            let promoted = { ...{ name: "George", age: 7 }, name: }
            let banner = promoted |> Greeter.greet("hi ")

            fn print_book_name(book: Map[Str, Str], fallback: Str) => Unit = {
              let title = book["title"]
              puts("#{title}#{fallback}")
            }
            """
        ).strip()
        + "\n"
    )

    scenario_highlights = run_query(highlights_query, scenario_path)
    for expected in [
        ("keyword.type", "shape"),
        ("constructor", "Some"),
        ("function.method", "rename"),
        ("variable.parameter", "next_name"),
        ("property", "age"),
        ("operator", "|>"),
        ("property", "name"),
        ("type.builtin", "Unit"),
    ]:
        assert_capture(scenario_highlights, *expected)

    scenario_outline = run_query(outline_query, scenario_path)
    for expected in [
        ("name", "Option"),
        ("name", "Named"),
        ("name", "Greeter"),
        ("name", "Greeter[Monkey]"),
        ("name", "rename"),
        ("name", "print_book_name"),
    ]:
        assert_capture(scenario_outline, *expected)
PY
)

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

python3 - <<'PY' "$TMP_DIR/pinned-ext"
import pathlib

ext = pathlib.Path(__import__("sys").argv[1])
(ext / "grammars/marmoset/.git").mkdir(parents=True, exist_ok=True)
(ext / "grammars/marmoset/.git/config").write_text(
    "[remote \"origin\"]\n\turl = file:///tmp/marmoset\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n"
)
(ext / "grammars/marmoset/stale.txt").write_text("stale\n")
PY

"$ZED_DIR/scripts/set-grammar-source.sh" pinned \
  --extension-dir "$TMP_DIR/pinned-ext"
test ! -e "$TMP_DIR/pinned-ext/grammars"

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
