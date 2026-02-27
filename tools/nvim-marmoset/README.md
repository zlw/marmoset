# Marmoset for Neovim

Tree-sitter syntax highlighting, code folding, indentation, and textobjects for [Marmoset](https://github.com/zlw/marmoset) `.mr` files in Neovim.

## Install

Requires [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter).

### lazy.nvim

```lua
{
  "zlw/marmoset",
  config = function()
    require("marmoset").setup()
    -- Then install the parser:
    -- :TSInstall marmoset
  end,
}
```

### packer.nvim

```lua
use {
  "zlw/marmoset",
  config = function()
    require("marmoset").setup()
  end,
}
```

After installing the plugin, run `:TSInstall marmoset` to download and compile the tree-sitter parser.

## Features

- **Syntax highlighting** via tree-sitter (keywords, types, functions, literals, operators, patterns, etc.)
- **Code folding** on blocks, enums, traits, impls, match expressions, functions
- **Indentation** via tree-sitter indent queries
- **Textobjects** via [nvim-treesitter-textobjects](https://github.com/nvim-treesitter/nvim-treesitter-textobjects):
  - `af`/`if` - function outer/inner
  - `ac`/`ic` - class outer/inner (enums, traits, impls)
  - `aa`/`ia` - parameter outer/inner
  - `ao`/`io` - conditional outer/inner

## Filetype

`.mr` files are automatically detected as `marmoset`. Comment string is set to `# %s` and indentation defaults to 2 spaces.
