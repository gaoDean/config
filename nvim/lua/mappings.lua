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

	nmap("<leader>f", [[<cmd>lua require('func').gitf()<cr>]])
	nmap("<leader>x", [[<cmd>lua require('fzf-lua').grep()<cr>]])
	nmap("<leader>,", [[<cmd>lua require('fzf-lua').files()<cr>]])
	nmap("<leader>o", [[<cmd>lua require('fzf-lua').oldfiles()<cr>]])
	nmap("<leader>r", [[<cmd>w | !compiler "%"<cr>]])
	nmap("<leader>t", [[<cmd>w | !compiler "%"<cr> | <cmd>lua pdfview()<cr>]])
	nmap("<leader>s", [[<cmd>lua vim.fn.jobstop(job_id)<cr>]])
	nmap("<leader>n", [[<cmd>NnnPicker<cr>]])
	nmap("<leader>g", [[<cmd>ZenMode<cr>]])
	nmap("<leader>i", [[<cmd>Twilight<cr>]])

	nmap("<leader>c", "zc")

	nmap("<leader>m", "zo")
	nmap("<leader>k", "zM")
	nmap("<leader>w", ":bn<cr>")
	nmap("<leader>q", ":bp<cr>")
	nmap("<leader>l", "<c-w>w")
	nmap("<leader>h", "<c-w>W")


	imap("{<CR>", "{<CR>}<ESC>O" )
	imap("{;<CR>", "{<CR>};<ESC>O")
	imap("<c-z>", [[<cmd>put=strftime('%a %d %b %Y')<cr>%O Viewed ]])

	vmap("J", ":m '>+1<cr>gv=gv")
	vmap("K", ":m '<-2<cr>gv=gv")
	nmap("<leader>j", "q:")
	nmap("q:", "<nop>");
