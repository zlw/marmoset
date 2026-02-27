# Marmoset for Zed

Syntax highlighting, bracket matching, auto-indentation, and code outline for [Marmoset](https://github.com/zlwaterfield/marmoset-ml) `.mr` files in [Zed](https://zed.dev).

## Install

### Dev extension (local)

1. Open the command palette in Zed
2. Run **zed: install dev extension**
3. Select the `tools/zed-marmoset/` directory

### From source (after pushing)

Update `extension.toml` with the correct `rev` for your grammar commit, then install as a dev extension.

## Features

- Syntax highlighting (keywords, types, literals, operators, comments, etc.)
- Bracket matching and rainbow brackets
- Auto-indentation for blocks, arrays, objects, match arms
- Code outline (let bindings, enums, traits, impls, methods, type aliases)

## Requirements

Depends on the `tree-sitter-marmoset` grammar in the sibling directory.
