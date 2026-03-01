-- Neovim 0.11+ native LSP config (used with vim.lsp.enable("marmoset"))
return {
  cmd = { "marmoset", "lsp" },
  filetypes = { "marmoset" },
  root_markers = { ".git", "dune-project", "Makefile" },
}
