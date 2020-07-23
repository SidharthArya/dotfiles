#!/bin/bash

DOTHING="$(echo | rofi -dmenu -p Notify)"
TIME="$(echo |rofi -dmenu -p Time)"
if [[ $TIME == "t"* ]];
then
    TIME=$(echo $(date --date="${TIME#t}" +"%s") - $(date +"%s") | bc)
fi
sleep $(echo "$TIME*60" | bc) && notify-send "$DOTHING";
