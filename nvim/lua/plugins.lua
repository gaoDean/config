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

local function create_file_if_not_found(file_path)
  if vim.fn.filereadable(file_path) == 0 then
    -- Create the file here
    -- You can use the `vim.api.nvim_command` function to execute a Vim command to create the file
    vim.api.nvim_command('edit ' .. file_path)
  end
end

vim.opt.rtp:prepend(lazypath)
local plugins = {
  -- {
  --   "mcchrish/zenbones.nvim",
  --   dependencies = "rktjmp/lush.nvim",
  --   config = function()
  --     -- vim.cmd([[colorscheme zenbones]])
  --   end
  -- },
	{
	   "savq/melange-nvim",
		config = function()
			vim.cmd([[colorscheme melange]])
	   end
  },
  {
    "nvim-telescope/telescope.nvim",
    tag = "0.1.2",
    keys = {
      { "<leader>.", "<cmd>Telescope find_files<cr>" },
      { "<leader><leader>", "<cmd>Telescope git_files<cr>" },
      { "<leader>fr", "<cmd>Telescope oldfiles<cr>" },
      { "<leader>bb", "<cmd>Telescope buffers<cr>" },
      { "<leader>fs", "<cmd>Telescope git_status<cr>" },
      -- { "<leader>.", "<cmd>Telescope file_browser hidden=true path=%:p:h select_buffer=true<cr>" },
      { "<leader>ff", "<cmd>Telescope<cr>" },
    },
    config = function()
      local dropdown_config = require("telescope.themes").get_dropdown({
        borderchars = {
          { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
          prompt = {"─", "│", " ", "│", '┌', '┐', "│", "│"},
          results = {"─", "│", "─", "│", "├", "┤", "┘", "└"},
          preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
        },
        width = 0.8,
        previewer = false,
        prompt_title = false
      })
      require("telescope.themes").get_viy = function()
        return dropdown_config
      end
      local actions = require("telescope.actions")
      local fb_actions = require("telescope").extensions.file_browser.actions
      require("telescope").setup({
        extensions = {
          ["zf-native"] = {
            file = {
              enable = true,
              highlight_results = true,
              match_filename = true,
            },
            generic = {
              enable = true,
              highlight_results = true,
              match_filename = false,
            },
          },
          -- file_browser = {
          --   -- disables netrw and use telescope-file-browser in its place
          --   theme = "viy",
          --   mappings = {
          --     i = {
          --       ["<backspace>"] = fb_actions.goto_parent_dir,
          --       ["~"] = fb_actions.goto_home_dir,
          --       ["<c-w>"] = fb_actions.goto_parent_dir,
          --     }
          --   },
          -- },
        },
        defaults = {
          mappings = {
            i = {
              ["<esc>"] = actions.close,
            },
          },
        },
        pickers = {
          find_files = dropdown_config,
          oldfiles = dropdown_config,
          buffers = dropdown_config,
          git_files = dropdown_config,
          git_status = dropdown_config,
        }
      })
      require("telescope").load_extension("zf-native")
      require("telescope").load_extension("file_browser")
    end,
    dependencies = {
      'nvim-lua/plenary.nvim',
      "natecraddock/telescope-zf-native.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
    }
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        char = {
          enabled = false
        }
      }
    },
    keys = {
      {
        "s",
        mode = { "n", "x", "o" },
        function()
          require("flash").jump()
        end,
        desc = "Flash",
      },
      {
        "r",
        mode = "o",
        function()
          require("flash").remote()
        end,
        desc = "Remote Flash",
      },
    },
  },
  {
    "github/copilot.vim",
    event = "VeryLazy",
  },
  {
    "echasnovski/mini.nvim",
    event = "VeryLazy",
    config = function()
      require('mini.comment').setup({
        options = {
          custom_commentstring = function()
            return require('ts_context_commentstring.internal').calculate_commentstring() or vim.bo.commentstring
          end,
        },
      })
      require('mini.surround').setup({
        custom_surroundings = {
          ['('] = { output = { left = '( ', right = ' )' } },
          ['['] = { output = { left = '[ ', right = ' ]' } },
          ['{'] = { output = { left = '{ ', right = ' }' } },
          ['<'] = { output = { left = '< ', right = ' >' } },
          ['b'] = { output = { left = '( ', right = ' )' } },
          ['B'] = { output = { left = '{ ', right = ' }' } },
          ['s'] = { output = { left = '[ ', right = ' ]' } },
          ['t'] = { output = { left = '< ', right = ' >' } },
        },
        mappings = {
          add = 'yS',
          delete = 'dS',
          find = '',
          find_left = '',
          highlight = '',
          replace = 'cS',
          update_n_lines = '',
        },
        search_method = 'cover_or_next',
      })
      -- Remap adding surrounding to Visual mode selection
      vim.api.nvim_del_keymap('x', 'yS')
      vim.api.nvim_set_keymap('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
      -- Make special mapping for "add surrounding for line"
      vim.api.nvim_set_keymap('n', 'yss', 'ys_', { noremap = false })
    end,
  },
  {
    "luukvbaal/nnn.nvim",
    keys = {
      {"<leader>n", "<cmd>NnnPicker<cr>"}
    },
    opts = {
      picker = {
        cmd = "nnn -A -c",
        style = {
          width = 0.8,     -- percentage relative to terminal size when < 1, absolute otherwise
          height = 0.8,    -- ^
          -- xoffset = 0.57,   -- ^
          -- yoffset = 0.4,   -- ^
          border = "rounded",-- border decoration for example "rounded"(:h nvim_open_win)
        },
      },
      replace_netrw = "picker",
    },
  },
  {
    "NvChad/nvim-colorizer.lua",
    name = "colorizer",
    event = "VeryLazy",
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
    keys = {
      { "<leader>t", "<cmd>TSJToggle<cr>" }
    },
    opts = {
      max_join_length = 500,
    }
  },
  -- {
  --   "nvim-treesitter/playground",
  --   event = "VeryLazy",
  -- },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup({
        indent = { enable = true },
        endwise = { enable = true },
        context_commentstring = { enable = true },
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
      {
        "JoosepAlviste/nvim-ts-context-commentstring",
        lazy = true,
      },
    },
  },
  {
    "windwp/nvim-autopairs",
    event = "VeryLazy",
    priority = 2,
    opts = {
      check_ts = true,
      map_c_w = true,
      map_cr = true,
    },
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
    "gaoDean/fstatus.nvim",
    dev = true,
    event = "VeryLazy",
    opts = true
  },
  {
    "gaoDean/autolist.nvim",
    dev = true,
    priority = 1,
    ft = {
      "markdown",
      "text",
      "tex",
      "norg",
      "plaintex",
    },
    config = function()
      require("autolist").setup()
      vim.keymap.set("i", "<tab>", "<cmd>AutolistTab<cr>")
      vim.keymap.set("i", "<s-tab>", "<cmd>AutolistShiftTab<cr>")
      vim.keymap.set("i", "<CR>", function()
        -- run autolist-new-bullet after the <cr> of nvim-autopairs-cr
        -- timeout of 0ms delays enough for my computer but u might need to adjust
        local timeoutms = 0
        vim.loop.new_timer():start(timeoutms, 0, vim.schedule_wrap(function()
          require("autolist").new_bullet()
        end))

        return require("nvim-autopairs").autopairs_cr()
      end, { expr = true, noremap = true })
      vim.keymap.set("n", "o", "o<cmd>AutolistNewBullet<cr>")
      vim.keymap.set("n", "O", "O<cmd>AutolistNewBulletBefore<cr>")
      vim.keymap.set("n", "<CR>", "<cmd>AutolistToggleCheckbox<cr><CR>")
      vim.keymap.set("n", "<C-r>", "<cmd>AutolistRecalculate<cr>")


      -- cycle list types with dot-repeat
      vim.keymap.set("n", "<leader>cn", require("autolist").cycle_next_dr, { expr = true })
      vim.keymap.set("n", "<leader>cp", require("autolist").cycle_prev_dr, { expr = true })

      -- if you don't want dot-repeat
      -- vim.keymap.set("n", "<leader>cn", "<cmd>AutolistCycleNext<cr>")
      -- vim.keymap.set("n", "<leader>cp", "<cmd>AutolistCycleNext<cr>")

      -- functions to recalculate list on edit
      vim.keymap.set("n", ">>", ">><cmd>AutolistRecalculate<cr>")
      vim.keymap.set("n", "<<", "<<<cmd>AutolistRecalculate<cr>")
      vim.keymap.set("n", "dd", "dd<cmd>AutolistRecalculate<cr>")
      vim.keymap.set("v", "d", "d<cmd>AutolistRecalculate<cr>")
    end,
  },
  -- {
  --   "folke/zen-mode.nvim",
  --   keys = {
  --     { "<leader>z", ":ZenMode<cr>"}
  --   },
  --   opts = {
  --     window = {
  --       backdrop = 1,
  --       width = 0.7,
  --       options = {
  --         number = false, -- disable number column
  --         relativenumber = false, -- disable relative numbers
  --       },
  --     },
  --     plugins = {
  --       alacritty = {
  --         enabled = true,
  --         font = "32", -- font size
  --       },
  --     },
  --   }
  -- },
  {
    "keaising/im-select.nvim",
    event = "VeryLazy",
    opts = {
      default_im_select = "com.apple.keylayout.Australian",
      default_command = "/usr/local/bin/im-select"
    }
  },
  -- {
  --   dev = true,
  --   "jbyuki/nabla.nvim",
  --   keys = {
  --     { "<leader>b", ":lua require('nabla').popup()<CR>" },
  --   },
  --   ft = {
  --     "markdown",
  --     "latex",
  --     "text"
  --   },
  --   config = function()
  --     require("nabla").toggle_virt()
  --   end
  -- },
}

require("lazy").setup(plugins, {
  dev = {
    path = "~/repos/rea",
  }
})
