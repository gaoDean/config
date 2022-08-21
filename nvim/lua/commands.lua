-- commands
	function au(evt, pat, cmd) -- (string|table), (string|table), (string)
		vim.api.nvim_create_autocmd(evt, { pattern = pat, command = cmd, })
	end
	au("BufWritePre", "*", "%s/\\s\\+$//e")
	au("BufWritePre", "*", "%s/\\n\\+\\%$//e")
	au("VimEnter", "*", [[if eval("@%") == "" | lua require("fzf-lua").oldfiles({ winopts = { hl = { border = "GruvboxBg0", } } })]])
	-- au("FileType", "nroff", [[:Goyo]])

	-- Return to last edit position when opening files
	au("BufReadPost", "*", [[
		 if line("'\"") > 0 && line("'\"") <= line("$") |
		   exe "normal! g`\"" |
		 endif
		]])

	au("User", "GoyoEnter", "Limelight")
	au("User", "GoyoLeave", "Limelight!")
	au("User", "GoyoLeave", [[if &filetype == "markdown" | source /Users/deangao/.config/nvim/syntax/markdown.vim | endif]])
	au("User", "GoyoLeave", [[if &filetype == "nroff" | source /Users/deangao/.config/nvim/syntax/nroff.vim | endif]])
