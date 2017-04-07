#!/usr/bin/env bash
xsetroot -solid '#1d1f21'
xautolock -time 5 -locker xlock &
# xset +fp ~/.fonts/envypn
# setxkbmap -layout us -option ctrl:nocaps
exec xmonad
