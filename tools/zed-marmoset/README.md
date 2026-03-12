# Marmoset for Zed

Syntax highlighting, bracket matching, auto-indentation, and code outline for [Marmoset](https://github.com/zlw/marmoset) `.mr` files in [Zed](https://zed.dev).

## Install

### Dev extension (local)

1. Open the command palette in Zed
2. Run **zed: install dev extension**
3. Select the `tools/zed-marmoset/` directory

The dev extension reads the grammar from the local monorepo checkout:
`repository = "file://../.."`, `path = "tools/tree-sitter-marmoset"`, and
`rev = "HEAD"`. That keeps the extension pointed at the current repository
instead of a separately pushed grammar revision.

If a previous failed install left a stale grammar checkout under
`tools/zed-marmoset/grammars/`, remove that directory and reinstall the dev
extension so Zed can clone the grammar again from a clean state.

### From source (after pushing)

The extension manifest expects to be built from the monorepo checkout so the
grammar is available at `tools/tree-sitter-marmoset/`.

## Features

- Syntax highlighting (keywords, types, literals, operators, comments, etc.)
- Bracket matching and rainbow brackets
- Auto-indentation for blocks, arrays, objects, match arms
- Code outline (let bindings, enums, traits, impls, methods, type aliases)

## Requirements

Depends on the `tree-sitter-marmoset` grammar in the local monorepo checkout.
