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

	au("User", "GoyoEnter", "Limelight")
	au("User", "GoyoLeave", "Limelight!")
	au("User", "GoyoLeave", [[if &filetype == "markdown" | source /Users/deangao/.config/nvim/syntax/markdown.vim | endif]])
	au("User", "GoyoLeave", [[if &filetype == "nroff" | source /Users/deangao/.config/nvim/syntax/nroff.vim | endif]])
	au("BufWritePost", "bm-files,bm-dirs", "!shortcuts")

	au("BufRead,BufNewFile", "[Nn]eomuttrc,.neomuttrc,.neomutt/*muttrc,.config/neomutt/*muttrc", "setfiletype neomuttrc")
	au("BufRead,BufNewFile", "neomutt-*-\\w\\+,neomutt[[:alnum:]_-]\\\\\\{6\\}", "setfiletype mail")
	au("BufRead,BufNewFile", "/tmp/neomutt*", "map ZZ :Goyo\\|x<CR>")
	au("BufRead,BufNewFile", "/tmp/neomutt*", "map ZQ :Goyo\\|q!<CR>")
	-- au("FileType", "*", "set fo-=c fo-=o fo-=r fo+=j") -- buffer only option
	au("FileType", "*", "set fo=qlj") -- buffer only option
	au("FileType", "lua", "set fo+=r")
