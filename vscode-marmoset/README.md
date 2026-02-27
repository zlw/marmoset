# Marmoset for VS Code

Syntax highlighting for [Marmoset](https://github.com/zlwaterfield/marmoset-ml) `.mr` files in VS Code.

## Install

### From VSIX (local)

```bash
cd vscode-marmoset
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

Keywords (`let`, `fn`, `if`, `match`, `enum`, `trait`, `impl`, `derive`, `type`, `return`, `for`, `is`), builtin types (`int`, `string`, `bool`, `float`), builtin functions (`puts`, `len`, `first`, `rest`, `push`), literals (integers, floats, strings, booleans), operators, enum variants, type parameters, generic constraints, and more.
