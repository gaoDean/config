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

	au("VimEnter", "*.md", "ZenMode")
	au("BufWritePost", "bm-files,bm-dirs", "!shortcuts")
	-- au("FileType", "*", "set fo-=c fo-=o fo-=r fo+=j") -- buffer only option
	au("FileType", "*", "set fo=qlj") -- buffer only option
	au("FileType", "lua", "set fo+=r")
	au("FileType", "mail", "set fo+=ta fo-=l")
	au("FileType", "gitcommit", "set fo+=ta fo-=l")
	au("FileType", "gitsendemail", "set fo+=ta fo-=l")

	au("BufRead", "*.ch.md", "!termfontfamily Didot")
	au("BufLeave", "*.ch.md", "!termfontfamily reset")
