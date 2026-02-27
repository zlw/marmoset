local M = {}

function M.setup(opts)
  opts = opts or {}

  local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
  parser_config.marmoset = {
    install_info = {
      url = "https://github.com/zlwaterfield/marmoset-ml",
      files = { "tools/tree-sitter-marmoset/src/parser.c" },
      branch = "main",
      generate_requires_npm = true,
    },
    filetype = "marmoset",
  }
end

return M
