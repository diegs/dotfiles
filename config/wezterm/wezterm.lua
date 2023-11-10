function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    -- return 'Wombat (Gogh)'
    -- return 'Tender (Gogh)'
    return 'Spacegray Eighties (Gogh)'
    -- return 'Arthur'
  else
    return 'Borland'
    -- return 'Catppuccin Latte'
    -- return 'Builtin Light'
  end
end

return {
  color_scheme = scheme_for_appearance(get_appearance()),
  font = wezterm.font 'SF Mono',
  font_size = 14.0,
  use_fancy_tab_bar = false,
  hide_tab_bar_if_only_one_tab = false,
  tab_bar_at_bottom = true,
  switch_to_last_active_tab_when_closing_tab = true,
  tab_max_width = 32,
  quit_when_all_windows_are_closed = false,
  audible_bell = "Disabled",
  initial_rows = 48,
  initial_cols = 140,
  use_resize_increments = true,
  leader = { key = "W", mods = "SHIFT|CMD" },
  keys = {
    { key = "{", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(-1) },
    { key = "}", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(1) },
    { key = "s", mods = "LEADER", action = wezterm.action.SplitVertical },
    { key = "v", mods = "LEADER", action = wezterm.action.SplitHorizontal },
    { key = "z", mods = "LEADER", action = wezterm.action.TogglePaneZoomState },
    { key = "h", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Left") },
    { key = "j", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Down") },
    { key = "k", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Up") },
    { key = "l", mods = "LEADER", action = wezterm.action.ActivatePaneDirection("Right") },
    { key = "h", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Left', 5 } },
    { key = "j", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Down', 5 } },
    { key = "k", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Up', 5 } },
    { key = "l", mods = "LEADER|SHIFT", action = wezterm.action.AdjustPaneSize { 'Right', 5 } },
  },
}
