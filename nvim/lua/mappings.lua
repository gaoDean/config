-- functions
function pdfview()
	local path = vim.api.nvim_buf_get_name(0)
	local fname = vim.fn.fnamemodify(path, ":t:r")
	local dir = vim.fn.fnamemodify(path, ":p:h")
	local outpath = ("%s/view/%s.pdf"):format(dir, fname)
	vim.fn.jobstart({ "zathura", outpath })
end

vim.keymap.set("n", "<leader>r", [[<cmd>w | !compiler "%"<cr> | <cmd>lua pdfview()<cr>]])
vim.keymap.set("n", "<leader>w", ":bn<cr>")
vim.keymap.set("n", "<leader>q", ":bp<cr>")
vim.keymap.set("n", "<leader>d", ":bd<cr>")
vim.keymap.set("n", "<leader>l", "<c-w>w")
vim.keymap.set("n", "<leader>h", "<c-w>W")
vim.keymap.set("n", "q:", "<nop>");
vim.keymap.set("v", "J", ":m '>+1<cr>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<cr>gv=gv")
