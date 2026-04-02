# Marmoset for VS Code

Syntax highlighting for [Marmoset](https://github.com/zlw/marmoset) `.mr` files in VS Code.

## Install

### From VSIX (local)

```bash
cd tools/vscode-marmoset
npm install -g @vscode/vsce
vsce package
code --install-extension marmoset-0.1.0.vsix
```

### Development

1. Open this directory in VS Code
2. Press **F5** to launch an Extension Development Host
3. Open any `.mr` file to see highlighting

## Features

- Full syntax highlighting via TextMate grammar
- Bracket matching and auto-closing (`{}`, `[]`, `()`, `""`)
- Code folding on brace-delimited blocks
- Comment toggling (`#`)
- Auto-indentation

## What's highlighted

Keywords (`let`, `fn`, `if`, `match`, `case`, `enum`, `shape`, `trait`, `impl`, `derive`, `type`, `return`, `override`, `is`), canonical builtin types (`Int`, `Str`, `Bool`, `Float`, `Unit`, `List`, `Map`), builtin functions (`puts`, `len`, `first`, `rest`, `push`), literals, operators, enum variants, shape fields, wrapper constructors, canonical `type` sums, type parameters, generic constraints, and more.
