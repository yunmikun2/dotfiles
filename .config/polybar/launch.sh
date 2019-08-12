#!/usr/bin/sh

killall -q pa-applet
killall -q nm-applet
killall -q polybar

while pgrep -x polybar >/dev/null; do sleep 1; done

polybar top &
nm-applet &
pa-applet &
# gbacklight
