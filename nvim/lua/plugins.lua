local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
local plugins = {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "macchiato" -- mocha, macchiato, frappe, latte
      })
      vim.cmd([[colorscheme catppuccin]])
    end,
  },
  {
    "echasnovski/mini.nvim",
    config = function()
      require('mini.comment').setup()
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
      -- vim.api.nvim_set_keymap('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
      -- Make special mapping for "add surrounding for line"
      vim.api.nvim_set_keymap('n', 'yss', 'ys_', { noremap = false })
    end,
  },
	{
    "ibhagwan/fzf-lua",
		keys = {
			{ "<leader>f", "<cmd>FzfLua git_files<cr>" },
			{ "<leader>a", "<cmd>FzfLua grep_project<cr>" },
			{ "<leader>n", "<cmd>FzfLua files<cr>" },
			{ "<leader>o", "<cmd>FzfLua oldfiles<cr>" },
		},
		opts = {
			winopts = {
				preview = { hidden = "hidden" },
				width = 0.6,
				height = 0.6,
			},
			files = {
				cmd = vim.fn.getenv('FZF_DEFAULT_COMMAND'),
			},
			grep = {
				cmd = "rg --color=always --hidden --follow -g '!{.git,node_modules}/'"
			}
		},
  },
	{
    "luukvbaal/nnn.nvim",
		keys = {
			{"<leader>,", "<cmd>NnnPicker<cr>"}
		},
    opts = {
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
    },
  },
	{
    "NvChad/nvim-colorizer.lua",
    name = "colorizer",
    ft = {
      'css',
      'javascript',
      'html',
      'svelte',
			'conf',
			'lua'
    },
    opts = {
      filetypes = {
        'css',
        'javascript',
        'html',
        'svelte',
				'conf'
      },
      user_default_options = {
        css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        -- Available modes for `mode`: foreground, background,  virtualtext
        mode = "foreground", -- Set the display mode.
      },
    },
  },
	{
		"Wansmer/treesj",
		event = "VeryLazy",
		config = function()
			require("treesj").setup()
			vim.keymap.set("n", "<leader>t", "<cmd>TSJToggle<cr>")
		end,
	},
	{
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup({
        indent = { enable = true },
				endwise = { enable = true },
				incremental_selection = { enable = true, },
        playground = { enable = true },
        autotag = { enable = true },
        ensure_installed = { "lua" },
        sync_install = false,
        auto_install = true,
        highlight = {
          enable = true,
          disable = {"help"},
          additional_vim_regex_highlighting = false,
        },
      })
    end,
    dependencies = {
			{
				"windwp/nvim-ts-autotag",
				lazy = true,
			},
			{
				"RRethy/nvim-treesitter-endwise",
				lazy = true,
			},
    },
  },
  {
    "windwp/nvim-autopairs",
		priority = 2,
    opts = {
				check_ts = true,
				map_c_w = true,
		},
  },
	{
		"ggandor/leap.nvim",
		event = "VeryLazy",
		config = function()
			local leap = require('leap')
			leap.add_default_mappings()
			-- leap.opts.safe_labels = { "s", "t", "n", "m", "f", "u", "S", "T", "N", "M", "F", "U", "G", "L", "H" }
			leap.opts.safe_labels = {}
			leap.opts.labels = { "t", "n", "e", "r", "a", "s",
				"i", "o", "g", "p", "h", "d", "m", "f", "l", "u",
				"c", "v", "j", "k", "w", "q", "x", "b", "z", "y",
				"T", "N", "E", "R", "A",
				"S", "I", "O", "H", "D",
			}
			vim.api.nvim_set_hl(0, 'LeapLabelPrimary', { link = "Cursor" })
		end
	},
	{
		"folke/todo-comments.nvim",
		event = "VeryLazy",
		opts = {
			signs = false,
		},
		-- PERF: fully optimised
		-- HACK: hmm, this looks a bit funky
		-- TODO: what else?
		-- NOTE: adding a note
		-- FIX: this needs fixing
		-- WARNING: ???
	},
	{
		"gaoDean/autolist.nvim",
		dev = true,
		priority = 1,
		ft = {
			"markdown",
			"text",
			"tex",
			"plaintex",
		},
		config = function()
			local autolist = require("autolist")
			autolist.setup()
			autolist.create_mapping_hook("i", "<CR>", autolist.new)
			autolist.create_mapping_hook("i", "<Tab>", autolist.indent)
			autolist.create_mapping_hook("i", "<S-Tab>", autolist.indent, "<C-D>")
			autolist.create_mapping_hook("n", "o", autolist.new)
			autolist.create_mapping_hook("n", "O", autolist.new_before)
			autolist.create_mapping_hook("n", ">>", autolist.indent)
			autolist.create_mapping_hook("n", "<<", autolist.indent)
			autolist.create_mapping_hook("n", "<C-r>", autolist.force_recalculate)
			autolist.create_mapping_hook("n", "<leader>x", autolist.invert_entry, "")
			autolist.create_mapping_hook("n", "<leader>x", autolist.invert_entry, "")
			vim.api.nvim_create_autocmd("TextChanged", {
				pattern = "*",
				callback = function()
					vim.cmd.normal({autolist.force_recalculate(nil, nil), bang = false})
				end
			})
		end,
	},
	{
		"folke/zen-mode.nvim",
		keys = {
			{ "<leader>z", ":ZenMode<cr>"}
		},
		opts = {
			window = {
				backdrop = 1,
				width = 80,
				options = {
					number = false, -- disable number column
					relativenumber = false, -- disable relative numbers
				},
			},
		}
	},
}

require("lazy").setup(plugins, {
  dev = {
    path = "~/repos/rea",
  }
})
