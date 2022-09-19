local wezterm = require 'wezterm'
return {
  font = wezterm.font 'Berkeley Mono',
  font_size = 13.0,
  use_fancy_tab_bar = false,
  tab_bar_at_bottom = true,
  tab_max_width = 48,
  use_resize_increments = false,

  keys = {
    { 
      key = 'z',
      mods = 'CMD',
      action = wezterm.action.TogglePaneZoomState,
    },
    { 
      key = 'h',
      mods = 'CMD',
      action = wezterm.action.ActivatePaneDirection 'Left',
    },
    { 
      key = 'j',
      mods = 'CMD',
      action = wezterm.action.ActivatePaneDirection 'Down',
    },
    { 
      key = 'k',
      mods = 'CMD',
      action = wezterm.action.ActivatePaneDirection 'Up',
    },
    { 
      key = 'l',
      mods = 'CMD',
      action = wezterm.action.ActivatePaneDirection 'Right',
    },
    { 
      key = 'h',
      mods = 'CMD|SHIFT',
      action = wezterm.action.AdjustPaneSize {'Left', 1},
    },
    { 
      key = 'j',
      mods = 'CMD|SHIFT',
      action = wezterm.action.AdjustPaneSize {'Down', 1},
    },
    { 
      key = 'k',
      mods = 'CMD|SHIFT',
      action = wezterm.action.AdjustPaneSize {'Up', 1},
    },
    { 
      key = 'l',
      mods = 'CMD|SHIFT',
      action = wezterm.action.AdjustPaneSize {'Right', 1},
    },
    { 
      key = '%',
      mods = 'CMD|CTRL|SHIFT',
      action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    { 
      key = '"',
      mods = 'CMD|CTRL|SHIFT',
      action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
      key = '{',
      mods = 'CMD|CTRL|SHIFT',
      action = wezterm.action.MoveTabRelative(-1),
    },
    {
      key = '}',
      mods = 'CMD|CTRL|SHIFT',
      action = wezterm.action.MoveTabRelative(1),
    },
  },

  color_scheme = "Catppuccin Mocha",
  -- colors = {
  --   -- The default text color
  --   foreground = '#f7f1ff',
  --   -- The default background color
  --   background = '#222222',

  --   -- Overrides the cell background color when the current cell is occupied by the
  --   -- cursor and the cursor style is set to Block
  --   cursor_bg = '#f7f1ff',
  --   -- Overrides the text color when the current cell is occupied by the cursor
  --   cursor_fg = '#000000',
  --   -- Specifies the border color of the cursor when the cursor style is set to Block,
  --   -- or the color of the vertical or horizontal bar when the cursor style is set to
  --   -- Bar or Underline.
  --   cursor_border = '#52ad70',

  --   -- the foreground color of selected text
  --   selection_fg = '#363537',
  --   -- the background color of selected text
  --   selection_bg = '#f7f1ff',

  --   -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  --   scrollbar_thumb = '#222222',

  --   -- The color of the split lines between panes
  --   split = '#444444',

  --   ansi = {
  --     '#363537',
  --     '#fc618d',
  --     '#7bd88f',
  --     '#fce566',
  --     'fd9353',
  --     '#948ae3',
  --     '#5ad4e6',
  --     '#f7f1ff',
  --   },
  --   brights = {
  --     '#69676c',
  --     'fc618d',
  --     '#7bd88f',
  --     '#fce566',
  --     '#fd9353',
  --     '#948ae3',
  --     '#5ad4e6',
  --     '#f7f1ff',
  --   },

    -- Since: 20220319-142410-0fcdea07
    -- When the IME, a dead key or a leader key are being processed and are effectively
    -- holding input pending the result of input composition, change the cursor
    -- to this color to give a visual cue about the compose state.
    -- compose_cursor = 'orange',
  -- },
}
