require "paq" {
	"echasnovski/mini.nvim";
	-- "folke/twilight.nvim";
	"folke/zen-mode.nvim";
	"ibhagwan/fzf-lua";
	"luukvbaal/nnn.nvim";
	"savq/paq-nvim";
	"NvChad/nvim-colorizer.lua";
	"nvim-treesitter/nvim-treesitter";
  "nvim-treesitter/playground";
  "windwp/nvim-ts-autotag";
  "catppuccin/nvim";
  "numToStr/Fterm.nvim";
	-- "gaoDean/autolist.nvim";
}

-- ========================================================= --

require("catppuccin").setup({
  flavour = "macchiato" -- mocha, macchiato, frappe, latte
})
vim.api.nvim_command "colorscheme catppuccin"

require("autolist").setup({
	invert = {
		indent = true,
	},
  normal_mappings = {
    invert = { '<leader>i+[catch]' },
  }
})

require("zen-mode").setup({
	window = {
		backdrop = 1,
		width = .80, -- width will be 85% of the editor width
		options = {
			number = false, -- disable number column
			relativenumber = false, -- disable relative numbers
		},
	},
})

require('nvim-treesitter.configs').setup({
  indent = { enable = true },
  incremental_selection = { enable = true, },
  playground = { enable = true },
  autotag = { enable = true },
  ensure_installed = {
	  "svelte",
	  "javascript",
	  "html",
	  "lua",
	  "rust",
  },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
    disable = {"help"},
    additional_vim_regex_highlighting = false,
  },
})

require('colorizer').setup({
	filetypes = {
		'css',
		'javascript',
		'html',
    'svelte',
	},
	user_default_options = {
        css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        -- Available modes for `mode`: foreground, background,  virtualtext
        mode = "foreground", -- Set the display mode.
	},
})

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
