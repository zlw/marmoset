# Marmoset for Zed

Syntax highlighting, bracket matching, auto-indentation, and code outline for [Marmoset](https://github.com/zlw/marmoset) `.mr` files in [Zed](https://zed.dev).

## Install

### Dev extension (local)

1. Open the command palette in Zed
2. Run **zed: install dev extension**
3. Select the `tools/zed-marmoset/` directory

The checked-in manifest fetches the grammar from the main Marmoset repository at
a pinned revision, with `path = "tools/tree-sitter-marmoset"`. That keeps dev
installs portable instead of depending on a machine-specific local `file://`
repository path.

If a previous failed install left a stale grammar checkout under
`tools/zed-marmoset/grammars/`, remove that directory and reinstall the dev
extension so Zed can clone the grammar again from a clean state.

### From source (after pushing)

Update the pinned grammar `rev` in `extension.toml` when the tree-sitter
grammar changes and you want the checked-in dev extension manifest to follow the
new revision.

## Features

- Syntax highlighting (keywords, types, literals, operators, comments, etc.)
- Bracket matching and rainbow brackets
- Auto-indentation for blocks, arrays, objects, match arms
- Code outline (let bindings, enums, traits, impls, methods, type aliases)

## Requirements

Depends on the `tree-sitter-marmoset` grammar in the main Marmoset repository.
