-- let
	local let = vim.g
	let.mapleader = " "
	vim.cmd("colorscheme gruvbox")

	-- gruvbox
		let.gruvbox_italic = 1
		let.gruvbox_italicize_strings = 1
		let.gruvbox_bold = 1
		let.gruvbox_invert_selection = 0
	-- haskell
		let.haskell_enable_quantification = 1
		let.haskell_enable_recursivedo = 1
		let.haskell_enable_arrowsyntax = 1
		let.haskell_enable_pattern_synonyms = 1
		let.haskell_enable_typeroles = 1
		let.haskell_enable_static_pointers = 1
		let.haskell_backpack = 1
		let.haskell_classic_highlighting = 0
-- opt
	local set = vim.opt
	-- set.textwidth = 80
	set.autochdir = true
	set.history = 1000
	set.laststatus = 0
	set.linebreak = true
	set.modeline = false
	set.number = true
	set.path = ".,,**"
	set.relativenumber = true
	set.scrolloff = 1
	set.splitbelow = true
	set.splitright = true
	set.termguicolors = true
	set.undofile = true

	set.showmode = false
	set.ruler = false
	set.swapfile = false

	set.hlsearch = false
	set.ignorecase = true
	set.smartcase = true

	-- set.foldmethod = "indent"
	-- set.foldenable = false
	-- set.foldnestmax = 1
	-- set.foldopen = set.foldopen - { "block" }
	-- set.foldtext = "v"
	-- set.fillchars  =  { fold = " " }

	local tw = 4
	set.expandtab = false
	set.tabstop = tw
	set.shiftwidth = tw
	set.shiftround = true

-- commands
	function au(evt, pat, cmd) -- (string|table), (string|table), (string)
		vim.api.nvim_create_autocmd(evt, { pattern = pat, command = cmd, })
	end
	au("ColorScheme", "*", "source ~/nvim/syntax/nroff.vim")
	au("BufWritePre", "*", "%s/\\s\\+$//e")
	au("BufEnter", "*.ms", "set ft=nroff")
	au("BufEnter", "*.hs", "set expandtab")
	au("FileType", "*", "setlocal formatoptions-=c formatoptions-=r formatoptions-=o formatoptions+=j")
	au("VimEnter", "*", [[if eval("@%") == "" | lua require("fzf-lua").oldfiles({ winopts = { hl = { border = "GruvboxBg0", } } })]])
	-- au("FileType", "nroff", [[:Goyo]])

	-- Return to last edit position when opening files
	au("BufReadPost", "*", [[
		 if line("'\"") > 0 && line("'\"") <= line("$") |
		   exe "normal! g`\"" |
		 endif
		]])
