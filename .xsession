#!/usr/bin/env bash
xsetroot -solid '#1d1f21'
xset +fp ~/.fonts/envypn
setxkbmap -layout us -option ctrl:nocaps
gnome-settings-daemon &
drive-sync &
/usr/bin/xcompmgr -a &
exec xmonad
