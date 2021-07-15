local g = vim.g
local o = vim.opt
local cmd = vim.cmd
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

g.mapleader = ','

-- Settings.
o.showcmd = false
o.ruler = false
o.hidden = true
o.showmatch = true
o.ignorecase = true
o.smartcase = true
o.backup = false
o.wb = false
o.swapfile = false
o.autoread = true

-- Editing.
o.tabstop = 2
o.softtabstop = 2
o.expandtab = true
o.shiftwidth=2
o.autoindent = true
o.wildmode = 'longest:list'
o.completeopt = 'menuone,noselect'
o.joinspaces = false
o.matchpairs:append('<:>')
o.spell = true
o.clipboard = 'unnamed'

-- UI.
o.hlsearch = true
o.incsearch = true
o.number = true
o.relativenumber = true
o.splitbelow = true
o.splitright = true
o.showmode = false

lspconfig = require'lspconfig'
lspconfig.gopls.setup {
  settings = {
    gopls = {
      gofumpt = true,
    },
  },
}
lspconfig.pyright.setup{}
lspconfig.rls.setup{}

-- cmd('autocmd BufWritePre * lua vim.lsp.buf.formatting_sync(nil, 1000)')

function goimports(timeout_ms)
  local context = { only = { "source.organizeImports" } }
  vim.validate { context = { context, "t", true } }

  local params = vim.lsp.util.make_range_params()
  params.context = context

  -- See the implementation of the textDocument/codeAction callback
  -- (lua/vim/lsp/handler.lua) for how to do this properly.
  local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, timeout_ms)
  if not result or next(result) == nil then return end
  local actions = result[1].result
  if not actions then return end
  local action = actions[1]

  -- textDocument/codeAction can return either Command[] or CodeAction[]. If it
  -- is a CodeAction, it can have either an edit, a command or both. Edits
  -- should be executed first.
  if action.edit or type(action.command) == "table" then
    if action.edit then
      vim.lsp.util.apply_workspace_edit(action.edit)
    end
    if type(action.command) == "table" then
      vim.lsp.buf.execute_command(action.command)
    end
  else
    vim.lsp.buf.execute_command(action)
  end
end

cmd('autocmd BufWritePre *.go lua goimports(1000)')

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = 'NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder',
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
    ultisnips = true;
    luasnip = true;
  };
}

-- Asterisk.
map('', '*', '<Plug>(asterisk-z*)')
map('', '#', '<Plug>(asterisk-z#)')
map('', 'g*', '<Plug>(asterisk-gz*)')
map('', 'g#', '<Plug>(asterisk-gz#)')
g['asterisk#keeppos'] = 1

-- Easy-align.
map('x', 'ga', '<Plug>(EasyAlign)')
map('n', 'ga', '<Plug>(EasyAlign)')

-- Go.
cmd('au FileType go set noexpandtab')
cmd('au FileType go set tw=120')

-- Python
cmd('au FileType python set tabstop=4')
cmd('au FileType python set shiftwidth=4')
cmd('au FileType python set tw=120')

-- Salt.
cmd('au! BufRead,BufNewFile *.sls     setfiletype yaml')
