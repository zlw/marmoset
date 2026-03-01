local M = {}

M._config = {
  lsp = {
    enable = true,
    cmd = { "marmoset", "lsp" },
  },
}

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
    if opts.lsp.cmd then
      M._config.lsp.cmd = opts.lsp.cmd
    end
  end
end

function M._start_lsp()
  if not M._config.lsp.enable then
    return
  end

  local cmd = M._config.lsp.cmd
  if vim.fn.executable(cmd[1]) ~= 1 then
    return
  end

  local root_dir = vim.fs.root(0, { ".git", "dune-project", "Makefile" })
    or vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":h")

  vim.lsp.start({
    name = "marmoset",
    cmd = cmd,
    root_dir = root_dir,
  })
end

return M
