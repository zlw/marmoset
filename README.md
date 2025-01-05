# Monkey Language but in Roc 🐵
* [Writing Interpreter in Go](https://interpreterbook.com/) / [Writing Compiler in Go](https://compilerbook.com/)'s Monkey Language but in [Roc](https://www.roc-lang.org/)
* [Marmoset](https://en.wikipedia.org/wiki/Marmoset) is a small monkey, sometimes called Pug Monkey, and I do like pugs 🙈

## Installation

First you need to install Roc, check either:
* [official instruction](https://www.roc-lang.org/docs/installation)
* [asdf plugin](https://github.com/dkuku/asdf-roc)

```sh
asdf plugin add roc https://github.com/dkuku/asdf-roc.git
asdf install roc nightly
```

Then you can clone this repository and run the following commands:

```sh
git clone git@github.com:zlw/marmoset.git
```

## Build

### Dev
faster compilation, slower runtime performance
```sh
make build
```

### Release
slower compilation, faster runtime performance
```sh
make release
```

## Run tests

```sh
make unit
```

## Progress

- [x] Lexer
- [x] Parser
- [ ] Evaluator
- [ ] Compiler

## Features

| Feature                | Interpreter | Compiler |
|------------------------|-------------|----------|
| Bindings               | ❌          | ❌       |
| Conditionals           | ❌          | ❌       |
| Strings                | ❌          | ❌       |
| Integers               | ❌          | ❌       |
| Arithmetic +-/*        | ❌          | ❌       |
| Arrays                 | ❌          | ❌       |
| Indexing               | ❌          | ❌       |
| Dictionaries           | ❌          | ❌       |
| Functions              | ❌          | ❌       |
| First class functions  | ❌          | ❌       |
| Higher order functions | ❌          | ❌       |
| Closures               | ❌          | ❌       |
| Recursion              | ❌          | ❌       |
| Built-In Functions     | ❌          | ❌       |
| Loops                  | ❌          | ❌       |
| Floats                 | ❌          | ❌       |
| Macros                 | ❌          | ❌       |

## TODO

- [ ] Propagate Paring errors instead of crashing
  - [ ] Return `Result`
    - [ ] `parseIdentifier`
    - [ ] `parseIntegerLiteral`
    - [ ] `parsePrefixExpression`
    - [ ] `parseInfixExpression`
    - [ ] `parseBoolean`
    - [ ] `parseGroupedExpression`
    - [ ] `parseIfExpression`
    - [ ] `parseBlockStatement`
    - [ ] `parseFunctionLiteral`
    - [ ] `parseFunctionParameters`
    - [ ] `parseCallExpression`
    - [ ] `parseCallArguments`
