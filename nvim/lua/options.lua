local let = vim.g
	let.is_posix = 1
	let.mapleader = " "

	-- gruvbox
		-- let.gruvbox_italic = 1
		-- let.gruvbox_italicize_strings = 1
		-- let.gruvbox_bold = 1
		-- let.gruvbox_invert_selection = 0
		-- let.gruvbox_baby_keyword_style = "NONE"
		let.gruvbox_baby_use_original_palette = true
		let.gruvbox_baby_string_style = "italic"
		local colors = require("gruvbox-baby.colors").config()
		let.gruvbox_baby_color_overrides = {blue_gray = colors.light_blue}
		vim.g.gruvbox_baby_highlights = {
			TSFunction = {fg = colors.forest_green, style="bold"},
			TSFuncBuiltin = {fg = colors.light_blue},
			TSFuncMacro = {fg = colors.light_blue},
			TSMethod = {fg = colors.magenta, style="bold"},
			TSParameter = {fg = colors.soft_yellow, style="italic"},
		}
		-- vim.cmd([[ colorscheme gruvbox-baby ]])
-- opt
	local set = vim.opt
	-- set.textwidth = 80
	set.autochdir = true
	set.wrap = true
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

	set.expandtab = false
	set.tabstop = 4
	set.shiftwidth = 4
	set.shiftround = true
