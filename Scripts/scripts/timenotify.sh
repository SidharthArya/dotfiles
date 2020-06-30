#!/bin/sh

DOTHING="$(echo | dmenu -p Notify:)"
TIME="$(echo |dmenu -p Time:)"
sleep $(echo "$TIME" | bc)m && notify-send "$DOTHING";
