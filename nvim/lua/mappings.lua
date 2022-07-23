-- mappings

	-- functions
	local job_id
	function pdfview()
		local path = vim.api.nvim_buf_get_name(0)
		local fname = vim.fn.fnamemodify(path, ":t:r")
		local dir = vim.fn.fnamemodify(path, ":p:h")
		local outpath = ("%s/view/%s.pdf"):format(dir, fname)
		job_id = vim.fn.jobstart({ "zathura", outpath})
	end

		function map(mode, keys, output)
			vim.api.nvim_set_keymap(mode, keys, output, { noremap = true, silent = true})
		end

		function nmap(keys, output)
			map("n", keys, output)
		end

		function imap(keys, output)
			map("i", keys, output)
		end

		function vmap(keys, output)
			map("v", keys, output)
		end
	-- , 1 2 3 4 5 a b c d e f g h i k l m n o p q r s t w x
	nmap("<leader>f", [[<cmd>lua require('func').gitf()<cr>]])
	nmap("<leader>x", [[<cmd>lua require('fzf-lua').grep()<cr>]])
	nmap("<leader>,", [[<cmd>lua require('fzf-lua').files()<cr>]])
	nmap("<leader>o", [[<cmd>lua require('fzf-lua').oldfiles()<cr>]])
	nmap("<leader>b", [[<cmd>vs ~/.bib<cr>]])
	nmap("<leader>r", [[<cmd>w | !compiler "%"<cr>]])
	nmap("<leader>t", [[<cmd>w | !compiler "%"<cr> | <cmd>lua pdfview()<cr>]])
	nmap("<leader>e", [[<cmd>lua MiniSessions.write(vim.fn.expand("%:t:r") .. ".vim")<cr>]])
	nmap("<leader>d", [[<cmd>lua MiniSessions.delete()<cr>]])
	nmap("<leader>u", [[<cmd>NnnPicker /Users/deangao/.vim/sessions<cr>]])
	nmap("<leader>i", [[<cmd>lua MiniStarter.open()<cr>]])
	nmap("<leader>s", [[<cmd>lua vim.fn.jobstop(job_id)<cr>]])
	nmap("<leader>a", [[<cmd>lua require('harpoon.mark').add_file()<cr>]])
	nmap("<leader>p", [[<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>]])
	nmap("<leader>1", [[<cmd>lua require('harpoon.ui').nav_file(1)<cr>]])
	nmap("<leader>2", [[<cmd>lua require('harpoon.ui').nav_file(2)<cr>]])
	nmap("<leader>3", [[<cmd>lua require('harpoon.ui').nav_file(3)<cr>]])
	nmap("<leader>4", [[<cmd>lua require('harpoon.ui').nav_file(4)<cr>]])
	nmap("<leader>5", [[<cmd>lua require('harpoon.ui').nav_file(5)<cr>]])
	nmap("<leader>n", [[<cmd>NnnPicker<cr>]])
	nmap("<leader>g", [[<cmd>Goyo<cr>]])

	nmap("<leader>c", "zc")
	nmap("<leader>m", "zo")
	nmap("<leader>k", "zM")
	nmap("<leader>w", ":bn<cr>")
	nmap("<leader>q", ":bp<cr>")
	nmap("<leader>l", "<c-w>w")
	nmap("<leader>h", "<c-w>W")

	imap("{<CR>", "{<CR>}<ESC>O" )
	imap("{;<CR>", "{<CR>};<ESC>O")

	vmap("J", ":m '>+1<cr>gv=gv")
	vmap("K", ":m '<-2<cr>gv=gv")
	nmap("<leader>j", "q:")
	nmap("q:", "<nop>");

