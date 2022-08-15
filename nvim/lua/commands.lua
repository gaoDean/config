-- commands
	function au(evt, pat, cmd) -- (string|table), (string|table), (string)
		vim.api.nvim_create_autocmd(evt, { pattern = pat, command = cmd, })
	end
	au("ColorScheme", "*", "source ~/nvim/syntax/nroff.vim")
	au("BufWritePre", "*", "%s/\\s\\+$//e")
	au("BufWritePre", "*", "%s/\\n\\+\\%$//e")
	au("BufEnter", "*.ms", "set ft=nroff")
	au("BufEnter", "*.ms", "nnoremap j gj")
	au("BufEnter", "*.ms", "nnoremap k gk")
	au("BufEnter", "*.ms", "set spell")
	au("BufEnter", "*.md", "nnoremap j gj")
	au("BufEnter", "*.md", "nnoremap k gk")
	au("BufEnter", "*.html", "nnoremap j gj")
	au("BufEnter", "*.html", "nnoremap k gk")
	au("BufEnter", "*.html", "setlocal tabstop=2 shiftwidth=2")
	au("BufEnter", "*.amber", "setlocal ft=amber tabstop=2 shiftwidth=2 list")
	au("BufEnter", "*.gcss", "setlocal ft=css expandtab shiftwidth=2 list")
	au("BufEnter", "bib", "set ft=rbib")
	au("FileType", "sh", "set ft=bash")
	au("FileType", "*", "setlocal formatoptions-=c formatoptions-=o formatoptions+=j")
	au("VimEnter", "*", [[if eval("@%") == "" | lua require("fzf-lua").oldfiles({ winopts = { hl = { border = "GruvboxBg0", } } })]])
	-- au("FileType", "nroff", [[:Goyo]])

	-- Return to last edit position when opening files
	au("BufReadPost", "*", [[
		 if line("'\"") > 0 && line("'\"") <= line("$") |
		   exe "normal! g`\"" |
		 endif
		]])
	au("FileType", "markdown", "setl comments=b:*,b:-,b:+,n:>")
	au("FileType", "markdown", "setl formatoptions+=r")
