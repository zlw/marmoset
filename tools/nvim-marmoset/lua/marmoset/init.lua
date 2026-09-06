local M = {}

M._config = {
  lsp = {
    enable = true,
  },
}

local function repo_binary_path(root)
  return root .. "/marmoset"
end

function M.setup(opts)
  opts = opts or {}

  -- Tree-sitter registration (optional — works without nvim-treesitter)
  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if ok then
    local parser_config = parsers.get_parser_configs()
    parser_config.marmoset = {
      install_info = {
        url = "https://github.com/zlw/marmoset",
        files = { "tools/tree-sitter-marmoset/src/parser.c" },
        branch = "main",
        generate_requires_npm = true,
      },
      filetype = "marmoset",
    }
  end

  -- LSP configuration
  if opts.lsp == false then
    M._config.lsp.enable = false
  elseif type(opts.lsp) == "table" then
    if opts.lsp.enable ~= nil then
      M._config.lsp.enable = opts.lsp.enable
    end
  end
end

function M._start_lsp()
  if not M._config.lsp.enable then
    return
  end

  local root_dir = vim.fs.root(0, { ".git", "dune-project", "Makefile" })
    or vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":h")
  local marmoset_root = vim.fn.environ().MARMOSET_ROOT

  if not marmoset_root or marmoset_root == "" then
    vim.notify(
      "MARMOSET_ROOT is not set; set it to the Marmoset repo root",
      vim.log.levels.ERROR
    )
    return
  end

  local cmd_env = vim.fn.environ()
  cmd_env.MARMOSET_ROOT = marmoset_root

  vim.lsp.start({
    name = "marmoset",
    cmd = { repo_binary_path(marmoset_root), "lsp" },
    cmd_env = cmd_env,
    root_dir = root_dir,
  })
end

return M
