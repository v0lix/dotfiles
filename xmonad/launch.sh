#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q xmobar

# Wait until the processes have been shut down
while pgrep -u $UID -x xmobar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
xmobar ~/.xmonad/xmobarrc &
