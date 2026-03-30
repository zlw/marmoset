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
repository path, but it is not a good local grammar iteration loop by itself.

If a previous failed install left a stale grammar checkout under
`tools/zed-marmoset/grammars/`, remove that directory and reinstall the dev
extension so Zed can clone the grammar again from a clean state.

### Local grammar mode

For fast local grammar testing in Zed:

1. Run `./tools/zed-marmoset/scripts/set-grammar-source.sh local --reset-cache`
2. Install or reinstall the dev extension from `tools/zed-marmoset/`
3. After uncommitted edits under `tools/tree-sitter-marmoset/`, run
   `./tools/zed-marmoset/scripts/sync-local-grammar-cache.sh`

Zed's grammar manifest still requires a repository URL plus a Git revision. The
local mode script switches the manifest to a `file://` repository rooted at the
current checkout and pins it to the current `HEAD`, so committed local changes
can be reinstalled without pushing to GitHub. The cache sync script fills the
remaining gap for uncommitted grammar edits by copying the working tree into
Zed's cached grammar checkout and clearing the cached `marmoset.wasm`.

Before committing or running CI, restore the portable manifest with:

`./tools/zed-marmoset/scripts/set-grammar-source.sh pinned`

### From source (after pushing)

Update the pinned grammar `rev` in `extension.toml` when the tree-sitter
grammar changes and you want the checked-in dev extension manifest to follow the
new revision.

## Features

- Syntax highlighting (keywords, types, literals, operators, comments, etc.)
- Bracket matching and rainbow brackets
- Auto-indentation for blocks, arrays, objects, match arms
- Code outline (let bindings, types, shapes, enums, traits, impls, methods)

## Requirements

Depends on the `tree-sitter-marmoset` grammar in the main Marmoset repository.
