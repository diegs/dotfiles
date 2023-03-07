#!/usr/bin/env bash

set -eu -o pipefail

THEME=$(defaults read -g AppleInterfaceStyle || echo "Light")

if [[ "$THEME" == "Dark" ]]; then
  ln -sf ${HOME}/.config/helix/themes/dark.toml ${HOME}/.config/helix/themes/current.toml
else
  ln -sf ${HOME}/.config/helix/themes/light.toml ${HOME}/.config/helix/themes/current.toml
fi

pkill -SIGUSR1 hx
