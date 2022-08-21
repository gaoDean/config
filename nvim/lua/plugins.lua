require "paq" {
	"savq/paq-nvim";
	"gruvbox-community/gruvbox";
	"junegunn/goyo.vim";
	"junegunn/limelight.vim";
	-- "gaoDean/autolist.nvim";
	"echasnovski/mini.nvim";
	"luukvbaal/nnn.nvim";
	{"ibhagwan/fzf-lua", branch = "main"};
}

-- ========================================================= --
require("autolist").setup({})
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
-- require('mini.surround').setup({})
require('mini.surround').setup({
    custom_surroundings = {
      ['('] = { output = { left = '( ', right = ' )' } },
      ['['] = { output = { left = '[ ', right = ' ]' } },
      ['{'] = { output = { left = '{ ', right = ' }' } },
      ['<'] = { output = { left = '< ', right = ' >' } },
    },
    mappings = {
      add = 'ys',
      delete = 'ds',
      find = '',
      find_left = '',
      highlight = '',
      replace = 'cs',
      update_n_lines = '',
    },
    search_method = 'cover_or_next',
  })
  -- Remap adding surrounding to Visual mode selection
  vim.api.nvim_del_keymap('x', 'ys')
  vim.api.nvim_set_keymap('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
  -- Make special mapping for "add surrounding for line"
  vim.api.nvim_set_keymap('n', 'yss', 'ys_', { noremap = false })

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
