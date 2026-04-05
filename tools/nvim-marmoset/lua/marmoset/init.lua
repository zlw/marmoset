local M = {}
local uv = vim.uv or vim.loop

M._config = {
  lsp = {
    enable = true,
    cmd = { "marmoset", "lsp" },
    cmd_env = {},
  },
}

local function path_exists(path)
  return uv.fs_stat(path) ~= nil
end

local function parent_dir(path)
  local parent = vim.fs.dirname(path)
  if not parent or parent == path then
    return nil
  end
  return parent
end

local function find_marmoset_root(start_path)
  local current = start_path

  while current do
    if path_exists(current .. "/std/prelude.mr") then
      return current
    end
    current = parent_dir(current)
  end

  return nil
end

local function repo_binary_candidates(root)
  return {
    root .. "/marmoset",
    root .. "/_build/default/bin/main.exe",
    root .. "/_build/install/default/bin/marmoset",
  }
end

local function resolve_lsp_cmd(base_cmd, marmoset_root)
  local cmd = vim.deepcopy(base_cmd)

  if marmoset_root and type(cmd) == "table" and cmd[1] == "marmoset" then
    for _, candidate in ipairs(repo_binary_candidates(marmoset_root)) do
      if vim.fn.executable(candidate) == 1 then
        cmd[1] = candidate
        break
      end
    end
  end

  return cmd
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
    if opts.lsp.cmd then
      M._config.lsp.cmd = opts.lsp.cmd
    end
    if opts.lsp.cmd_env then
      M._config.lsp.cmd_env = opts.lsp.cmd_env
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
  local marmoset_root = find_marmoset_root(root_dir)
  local cmd = resolve_lsp_cmd(M._config.lsp.cmd, marmoset_root)

  if vim.fn.executable(cmd[1]) ~= 1 then
    return
  end

  local cmd_env = vim.tbl_extend("force", vim.fn.environ(), M._config.lsp.cmd_env or {})
  if marmoset_root then
    cmd_env.MARMOSET_ROOT = marmoset_root
  end

  vim.lsp.start({
    name = "marmoset",
    cmd = cmd,
    cmd_env = cmd_env,
    root_dir = root_dir,
  })
end

return M
