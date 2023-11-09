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
  quit_when_all_windows_are_closed = false,
  audible_bell = "Disabled",
  initial_rows = 48,
  initial_cols = 140,
  keys = {
    { key= "{", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(-1) },
    { key= "}", mods = "SHIFT|CTRL|CMD", action = wezterm.action.MoveTabRelative(1) },
  }
}
