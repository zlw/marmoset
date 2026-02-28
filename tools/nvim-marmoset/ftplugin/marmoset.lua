-- Marmoset filetype settings
vim.bo.commentstring = "# %s"
vim.bo.shiftwidth = 2
vim.bo.tabstop = 2
vim.bo.expandtab = true
vim.bo.softtabstop = 2

-- Start LSP if configured
require("marmoset")._start_lsp()
