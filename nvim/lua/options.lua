-- let
	local let = vim.g
	let.mapleader = " "
	vim.cmd([[
	colorscheme gruvbox
	]])

	-- gruvbox
		let.gruvbox_italic = 1
		let.gruvbox_italicize_strings = 1
		let.gruvbox_bold = 1
		let.gruvbox_invert_selection = 0
-- opt
	local set = vim.opt
	-- set.textwidth = 80
	set.autochdir = true
	set.mouse="n"
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
	set.undodir = "/Users/deangao/.config/nvim/undo"
	set.undofile = true

	set.showmode = false
	set.ruler = false
	set.swapfile = false

	set.hlsearch = false
	set.ignorecase = true
	set.smartcase = true

	local tw = 4
	set.expandtab = false
	set.tabstop = tw
	set.shiftwidth = tw
	set.shiftround = true
