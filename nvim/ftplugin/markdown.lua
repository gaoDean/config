vim.api.nvim_buf_set_keymap(0, "n", "j", "gj", { noremap = true, silent = true })
vim.api.nvim_buf_set_keymap(0, "n", "k", "gk", { noremap = true, silent = true })
vim.opt_local.spell = true
vim.opt_local.breakindent = true
vim.opt_local.breakindentopt="shift:2"
vim.opt_local.relativenumber = false
vim.opt_local.number = false
vim.opt_local.signcolumn = "yes"
