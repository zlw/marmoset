#!/usr/bin/env python3

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


PINNED_REPOSITORY = "https://github.com/zlw/marmoset"
PINNED_REV = "4d213cc6552161ab90d15caa769074ff4354da7c"


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[3]


def git_head(repo_root: Path) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo_root), "rev-parse", "HEAD"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def manifest(grammar_repository: str, grammar_rev: str) -> str:
    return (
        'id = "marmoset"\n'
        'name = "Marmoset"\n'
        'version = "0.2.0"\n'
        "schema_version = 1\n"
        'authors = ["Marmoset Contributors"]\n'
        'description = "Marmoset language support for Zed — syntax highlighting, brackets, indentation, and code outline."\n'
        'repository = "https://github.com/zlw/marmoset"\n'
        'languages = ["languages/marmoset"]\n'
        "\n"
        "[lib]\n"
        'kind = "Rust"\n'
        'version = "0.7.0"\n'
        "\n"
        "[grammars.marmoset]\n"
        f'repository = "{grammar_repository}"\n'
        'path = "tools/tree-sitter-marmoset"\n'
        f'rev = "{grammar_rev}"\n'
        "\n"
        "[language_servers.marmoset-lsp]\n"
        'languages = ["Marmoset"]\n'
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Render the Zed extension manifest for pinned or local grammar mode.")
    parser.add_argument("--mode", choices=["pinned", "local"], required=True)
    parser.add_argument("--repo-root", type=Path, default=repo_root_from_script())
    parser.add_argument("--output", type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root.resolve()

    if args.mode == "pinned":
        rendered = manifest(PINNED_REPOSITORY, PINNED_REV)
    else:
        rendered = manifest(repo_root.as_uri(), git_head(repo_root))

    if args.output is None:
        sys.stdout.write(rendered)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
