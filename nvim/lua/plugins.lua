local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')
	Plug 'gruvbox-community/gruvbox'
	Plug 'junegunn/goyo.vim'
	Plug 'echasnovski/mini.nvim'
	Plug 'luukvbaal/nnn.nvim'
	Plug('ibhagwan/fzf-lua', {branch = 'main'})
vim.call('plug#end')

-- ========================================================= --
require("fzf-lua").setup({
	winopts = {
		preview = { hidden = "hidden" },
		width = 0.6,
		height = 0.6,
	},
	files = {
		cmd = vim.fn.getenv('FZF_DEFAULT_COMMAND'),
	},
	grep = {
		cmd = "rg --color=never --files --hidden --follow -g '!.git'"
	}
})

require('mini.comment').setup({})
require('mini.surround').setup({})
require("nnn").setup({
	picker = {
		cmd = "nnn -c",
		style = {
			width = 0.6,     -- percentage relative to terminal size when < 1, absolute otherwise
			height = 0.57,    -- ^
			xoffset = 0.57,   -- ^
			yoffset = 0.4,   -- ^
			border = "rounded",-- border decoration for example "rounded"(:h nvim_open_win)
		},
	},
	replace_netrw = "picker",
})
local header_art =
	[[
	╭╮╭┬─╮╭─╮┬  ┬┬╭┬╮
	│││├┤ │ │╰┐┌╯││││
	╯╰╯╰─╯╰─╯ ╰╯ ┴┴ ┴
	]]
