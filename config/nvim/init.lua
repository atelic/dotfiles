-- Neovim Configuration with LazyVim
-- Managed via ~/dotfiles - symlinked to ~/.config/nvim
-- Focus: Speed + AI

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Leader key (must be before lazy)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- ===== Core Options (before plugins for speed) =====
local opt = vim.opt

-- Performance
opt.lazyredraw = true  -- Don't redraw during macros (faster)
opt.updatetime = 200   -- Faster completion
opt.timeoutlen = 300   -- Faster key sequences

-- UI
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.cursorline = true
opt.termguicolors = true
opt.showmode = false   -- Mode shown in statusline
opt.cmdheight = 1
opt.pumheight = 10     -- Popup menu height
opt.splitbelow = true
opt.splitright = true
opt.scrolloff = 8
opt.sidescrolloff = 8

-- Editing
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.smartindent = true
opt.wrap = false
opt.linebreak = true
opt.breakindent = true

-- Search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- Files
opt.backup = false
opt.writebackup = false
opt.swapfile = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("state") .. "/undo"

-- Clipboard
opt.clipboard = "unnamedplus"

-- Completion
opt.completeopt = "menu,menuone,noselect"

-- ===== Lazy.nvim Setup =====
require("lazy").setup({
  spec = {
    -- Import LazyVim and its plugins
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },
    
    -- ===== Language Support =====
    { import = "lazyvim.plugins.extras.lang.typescript" },
    { import = "lazyvim.plugins.extras.lang.json" },
    { import = "lazyvim.plugins.extras.lang.python" },
    { import = "lazyvim.plugins.extras.lang.rust" },
    { import = "lazyvim.plugins.extras.lang.go" },
    { import = "lazyvim.plugins.extras.lang.tailwind" },
    { import = "lazyvim.plugins.extras.lang.yaml" },
    { import = "lazyvim.plugins.extras.lang.docker" },
    { import = "lazyvim.plugins.extras.lang.markdown" },
    { import = "lazyvim.plugins.extras.lang.nix" },

    -- ===== AI Plugins =====
    -- GitHub Copilot (use Nix-provided Node)
    { import = "lazyvim.plugins.extras.ai.copilot" },
    { import = "lazyvim.plugins.extras.ai.copilot-chat" },
    {
      "zbirenbaum/copilot.lua",
      opts = {
        copilot_node_command = "/etc/profiles/per-user/ericbarbour/bin/node",
      },
    },
    
    -- Avante (Cursor-like AI experience)
    {
      "yetone/avante.nvim",
      event = "VeryLazy",
      lazy = false,
      version = false,
      opts = {
        provider = "claude",
        auto_suggestions_provider = "copilot",
        providers = {
          claude = {
            model = "claude-opus-4-5-20251101",
            extra_request_body = {
              max_tokens = 8192,
            },
          },
        },
        behaviour = {
          auto_suggestions = false, -- Use copilot for suggestions
          auto_set_keymaps = true,
        },
        mappings = {
          ask = "<leader>aa",
          edit = "<leader>ae",
          refresh = "<leader>ar",
          toggle = {
            default = "<leader>at",
            debug = "<leader>ad",
            hint = "<leader>ah",
          },
          diff = {
            ours = "co",
            theirs = "ct",
            both = "cb",
            cursor = "cc",
          },
        },
        hints = { enabled = true },
        windows = {
          position = "right",
          wrap = true,
          width = 40,
        },
      },
      build = "make",
      dependencies = {
        "nvim-treesitter/nvim-treesitter",
        "stevearc/dressing.nvim",
        "nvim-lua/plenary.nvim",
        "MunifTanjim/nui.nvim",
        "nvim-tree/nvim-web-devicons",
        "zbirenbaum/copilot.lua",
        {
          "HakonHarnes/img-clip.nvim",
          event = "VeryLazy",
          opts = {
            default = {
              embed_image_as_base64 = false,
              prompt_for_file_name = false,
              drag_and_drop = { insert_mode = true },
            },
          },
        },
        {
          "MeanderingProgrammer/render-markdown.nvim",
          opts = { file_types = { "markdown", "Avante" } },
          ft = { "markdown", "Avante" },
        },
      },
    },
    
    -- ===== Editor Enhancements =====
    { import = "lazyvim.plugins.extras.editor.mini-files" },
    { import = "lazyvim.plugins.extras.editor.illuminate" },
    
    -- ===== Coding =====
    { import = "lazyvim.plugins.extras.coding.mini-surround" },
    { import = "lazyvim.plugins.extras.coding.yanky" },

    -- ===== LSP =====
    { import = "lazyvim.plugins.extras.editor.inc-rename" },

    -- ===== Util =====
    { import = "lazyvim.plugins.extras.util.persistence" },

    -- ===== Formatting =====
    { import = "lazyvim.plugins.extras.formatting.prettier" },
    
    -- ===== UI =====
    { import = "lazyvim.plugins.extras.ui.mini-animate" },
    { import = "lazyvim.plugins.extras.ui.treesitter-context" },
    
    -- ===== Custom Overrides =====
    -- Faster startup - disable some heavy plugins
    { "folke/noice.nvim", enabled = true },
    { "rcarriga/nvim-notify", enabled = true },
    
    -- Theme
    {
      "catppuccin/nvim",
      name = "catppuccin",
      priority = 1000,
      opts = {
        flavour = "mocha",
        transparent_background = false,
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          telescope = true,
          treesitter = true,
          mini = true,
          native_lsp = {
            enabled = true,
            underlines = {
              errors = { "undercurl" },
              warnings = { "undercurl" },
            },
          },
        },
      },
    },
    
    -- Faster file navigation
    {
      "nvim-telescope/telescope.nvim",
      dependencies = {
        {
          "nvim-telescope/telescope-fzf-native.nvim",
          build = "make",
          config = function()
            require("telescope").load_extension("fzf")
          end,
        },
      },
      opts = {
        defaults = {
          file_ignore_patterns = {
            "node_modules",
            ".git/",
            "target/",
            "dist/",
            "build/",
            "__pycache__/",
            ".next/",
          },
        },
      },
    },
    
    -- Git signs in gutter
    {
      "lewis6991/gitsigns.nvim",
      opts = {
        current_line_blame = true,
        current_line_blame_opts = { delay = 500 },
      },
    },
    
    -- Which-key timeout
    {
      "folke/which-key.nvim",
      opts = {
        delay = 200,
      },
    },
    
    -- Lazygit integration
    {
      "kdheepak/lazygit.nvim",
      lazy = true,
      cmd = {
        "LazyGit",
        "LazyGitConfig",
        "LazyGitCurrentFile",
        "LazyGitFilter",
        "LazyGitFilterCurrentFile",
      },
      keys = {
        { "<leader>gg", "<cmd>LazyGit<cr>", desc = "LazyGit" },
      },
    },
  },
  
  defaults = {
    lazy = false,
    version = false,
  },
  
  install = { colorscheme = { "catppuccin", "tokyonight", "habamax" } },
  
  checker = { enabled = true, notify = false },
  
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
        "netrwPlugin",
        "matchit",
        "matchparen",
      },
    },
  },
})

-- ===== Colorscheme =====
vim.cmd.colorscheme("catppuccin")

-- ===== Custom Keymaps =====
local map = vim.keymap.set

-- Quick escape
map("i", "jk", "<Esc>", { desc = "Exit insert mode" })
map("i", "jj", "<Esc>", { desc = "Exit insert mode" })

-- Better up/down for wrapped lines
map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- Move lines
map("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move down" })
map("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move up" })
map("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move down" })
map("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move up" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Go to left window" })
map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window" })
map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window" })
map("n", "<C-l>", "<C-w>l", { desc = "Go to right window" })

-- Buffer navigation
map("n", "H", "<cmd>bprevious<cr>", { desc = "Previous buffer" })
map("n", "L", "<cmd>bnext<cr>", { desc = "Next buffer" })

-- Clear search highlight
map("n", "<Esc>", "<cmd>noh<cr><Esc>", { desc = "Clear hlsearch" })

-- Better indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

-- Save file
map({ "n", "i", "v" }, "<C-s>", "<cmd>w<cr><Esc>", { desc = "Save file" })
map("n", "<leader>w", "<cmd>w<cr>", { desc = "Save file" })

-- Quit
map("n", "<leader>q", "<cmd>q<cr>", { desc = "Quit" })
map("n", "<leader>Q", "<cmd>qa!<cr>", { desc = "Quit all" })

-- Center cursor after movements
map("n", "<C-d>", "<C-d>zz")
map("n", "<C-u>", "<C-u>zz")
map("n", "n", "nzzzv")
map("n", "N", "Nzzzv")

-- Splits
map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Split vertical" })
map("n", "<leader>sh", "<cmd>split<cr>", { desc = "Split horizontal" })

-- ===== AI Keymaps =====
-- Copilot
map("n", "<leader>cp", "<cmd>Copilot panel<cr>", { desc = "Copilot Panel" })

-- ===== Autocommands =====
local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Highlight on yank
autocmd("TextYankPost", {
  group = augroup("highlight_yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 200 })
  end,
})

-- Auto-resize splits
autocmd("VimResized", {
  group = augroup("resize_splits", { clear = true }),
  callback = function()
    vim.cmd("tabdo wincmd =")
  end,
})

-- Go to last location when opening buffer
autocmd("BufReadPost", {
  group = augroup("last_loc", { clear = true }),
  callback = function()
    local mark = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- Close some filetypes with q
autocmd("FileType", {
  group = augroup("close_with_q", { clear = true }),
  pattern = {
    "help",
    "lspinfo",
    "man",
    "notify",
    "qf",
    "query",
    "startuptime",
    "checkhealth",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
  end,
})

-- Auto-switch Node version when .nvmrc is present
local function load_nvmrc()
  local nvmrc = vim.fn.findfile(".nvmrc", vim.fn.getcwd() .. ";")
  if nvmrc == "" then return end

  local f = io.open(nvmrc, "r")
  if not f then return end
  local version = f:read("*l"):gsub("%s+", "")
  f:close()

  -- Resolve actual node path via nvm (must source shell function)
  local cmd = string.format("bash -c 'source $NVM_DIR/nvm.sh && nvm which %s 2>/dev/null'", version)
  local node_path = vim.fn.system(cmd):gsub("%s+", "")

  if vim.v.shell_error == 0 and node_path ~= "" then
    local node_dir = vim.fn.fnamemodify(node_path, ":h")
    -- Prepend to PATH (affects LSP servers, terminals, etc.)
    vim.env.PATH = node_dir .. ":" .. vim.env.PATH
    vim.notify("Node: " .. version, vim.log.levels.INFO)
  end
end

autocmd({ "DirChanged", "VimEnter" }, {
  group = augroup("nvm_auto", { clear = true }),
  callback = load_nvmrc,
})
