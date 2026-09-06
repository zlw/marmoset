-- Neovim 0.11+ native LSP config (used with vim.lsp.enable("marmoset"))
assert(
  vim.env.MARMOSET_ROOT and vim.env.MARMOSET_ROOT ~= "",
  "MARMOSET_ROOT is not set; set it to the Marmoset repo root"
)

return {
  cmd = { vim.env.MARMOSET_ROOT .. "/marmoset", "lsp" },
  filetypes = { "marmoset" },
  root_markers = { ".git", "dune-project", "Makefile" },
}
