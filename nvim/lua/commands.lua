-- commands
	function au(evt, pat, cmd) -- (string|table), (string|table), (string)
		vim.api.nvim_create_autocmd(evt, { pattern = pat, command = cmd, })
	end
	au("BufWritePre", "*", "%s/\\s\\+$//e")
	au("BufWritePre", "*", "%s/\\n\\+\\%$//e")
	-- au("FileType", "nroff", [[:Goyo]])

	-- Return to last edit position when opening files
	au("BufReadPost", "*", [[
		 if line("'\"") > 0 && line("'\"") <= line("$") |
		   exe "normal! g`\"" |
		 endif
		]])

	au("BufWritePost", "bm-files,bm-dirs", "!shortcuts")
	-- au("FileType", "*", "set fo-=c fo-=o fo-=r fo+=j") -- buffer only option
	au("FileType", "*", "set fo=qlj") -- buffer only option
	au("FileType", "lua", "set fo+=r")
