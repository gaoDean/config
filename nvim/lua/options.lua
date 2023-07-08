local let = vim.g
let.is_posix = 1
let.mapleader = " "

-- opt
local set = vim.opt
-- set.textwidth = 80
set.background = "light"
set.autochdir = true
set.wrap = true
set.mouse="n"
set.history = 1000
set.laststatus = 0
set.linebreak = true
set.modeline = false
set.signcolumn = "yes"
set.number = false
set.path = ".,,**"
set.relativenumber = false
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

set.expandtab = true
set.tabstop = 2
set.shiftwidth = 2
set.shiftround = true

-- vim.cmd([[colorscheme zellner]])
