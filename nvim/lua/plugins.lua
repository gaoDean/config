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
require("lazy").setup({
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
      vim.api.nvim_set_keymap('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
      -- Make special mapping for "add surrounding for line"
      vim.api.nvim_set_keymap('n', 'yss', 'ys_', { noremap = false })
    end,
  },
	{
    "ibhagwan/fzf-lua",
    lazy = true,
    config = {
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
    },
  },
	{
    "luukvbaal/nnn.nvim",
    event = "VeryLazy",
    config = {
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
    },
    config = {
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
    },
  },
	{
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require('nvim-treesitter.configs').setup({
        indent = { enable = true },
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
      "windwp/nvim-ts-autotag",
    },
  },
  {
    "gaoDean/autolist.nvim",
    dev = false,
    lazy = true,
    config = function()
      require("autolist").setup()
      function create_mapping_hook(mode, mapping, hook, alias)
        vim.keymap.set(
          mode,
          mapping,
          function(motion)
            local keys = hook(motion, alias or mapping)
            if not keys then keys = "" end
            return keys
          end,
          { expr = true}
        )
      end

      create_mapping_hook("i", "<cr>", require("autolist").new)
      create_mapping_hook("i", "<tab>", require("autolist").indent)
      create_mapping_hook("i", "<s-tab>", require("autolist").indent, "<c-d>")
      create_mapping_hook("n", "dd", require("autolist").force_recalculate)
      create_mapping_hook("n", "o", require("autolist").new)
      create_mapping_hook("n", "O", require("autolist").new_before)
      create_mapping_hook("n", ">>", require("autolist").indent)
      create_mapping_hook("n", "<<", require("autolist").indent)
      create_mapping_hook("n", "<c-r>", require("autolist").force_recalculate)
      create_mapping_hook("n", "<leader>x", require("autolist").invert_entry, "")
    end,
  }
},
{
  dev = {
    path = "~/repos/rea",
  }
}
)

-- ========================================================= --
--
-- require("autolist").setup({})
-- function create_mapping_hook(mode, mapping, hook, alias)
--   vim.keymap.set(
--     mode,
--     mapping,
--     function(motion)
--       local keys = hook(motion, alias or mapping)
--       if not keys then keys = "" end
--       return keys
--     end,
--     { expr = true}
--   )
-- end
--
-- create_mapping_hook("i", "<cr>", require("autolist").new)
-- create_mapping_hook("i", "<tab>", require("autolist").indent)
-- create_mapping_hook("i", "<s-tab>", require("autolist").indent, "<c-d>")
-- create_mapping_hook("n", "dd", require("autolist").force_recalculate)
-- create_mapping_hook("n", "o", require("autolist").new)
-- create_mapping_hook("n", "O", require("autolist").new_before)
-- create_mapping_hook("n", ">>", require("autolist").indent)
-- create_mapping_hook("n", "<<", require("autolist").indent)
-- create_mapping_hook("n", "<c-r>", require("autolist").force_recalculate)
-- create_mapping_hook("n", "<leader>x", require("autolist").invert_entry, "")
