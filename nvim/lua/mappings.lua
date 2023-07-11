-- functions
function pdfview()
	local path = vim.api.nvim_buf_get_name(0)
	local fname = vim.fn.fnamemodify(path, ":t:r")
	local dir = vim.fn.fnamemodify(path, ":p:h")
	local outpath = ("%s/%s.pdf"):format(dir, fname)
	vim.fn.jobstart({ "zathura", outpath })
end

-- echo the current file name + the it's parent directory
function echo_filename()
  local path = vim.api.nvim_buf_get_name(0)
  local fname = vim.fn.fnamemodify(path, ":t:r")
  local dir = vim.fn.fnamemodify(path, ":p:h")
  vim.cmd("echo '" .. dir .. "/" .. fname .. "'")
end


-- echo the current file path relative to the git root
-- make it echo the full path if not in a git repo
function echo_git_root()
  local path = vim.api.nvim_buf_get_name(0)
  local git_path = vim.fn.systemlist("relative_path_git " .. path)[1]
  vim.cmd("echo '" .. git_path .. "'")
end

-- vim.keymap.set("n", "<leader>r", [[<cmd>w | !compiler "%"<cr> | <cmd>lua pdfview()<cr>]])
vim.keymap.set("n", "<leader>w",  ":bn<cr>")
vim.keymap.set("n", "<leader>q",  ":bp<cr>")
vim.keymap.set("n", "<leader>bk", ":bd<cr>")
vim.keymap.set("n", "<leader>b!", ":bd!<cr>")
vim.keymap.set({"n", "x", "v", "i"}, "<c-c>", "\"*y")
-- vim.keymap.set("n", "<leader>l", "<c-w>w")
-- vim.keymap.set("n", "<leader>h", "<c-w>W")
vim.keymap.set("n", "<leader>y", "gg\"*yG")
-- vim.keymap.set("n", "<leader>c", [[:!conv "%" pdf<cr>]])
-- vim.keymap.set("n", "<leader>p", [[:!openas "%" beamer full<cr>]])
-- vim.keymap.set("n", "<leader>ec", [[:!conv "%"]])
-- vim.keymap.set("n", "<leader>ep", [[:!openas "%"]])
vim.keymap.set("n", "q:", "<nop>");
vim.keymap.set("v", "J", ":m '>+1<cr>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<cr>gv=gv")
