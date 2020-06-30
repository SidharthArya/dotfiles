#!/bin/bash
 
DIR=$HOME/Downloads
 
# Get rid of old log file
rm /tmp/virus-scan.log 2> /dev/null
 
inotifywait -q -m -e close_write,moved_to --format '%w%f' $DIR | while read FILE
do
     # Have to check file length is nonzero otherwise commands may be repeated
     if [ -s $FILE ]; then
          date > $HOME/virus-scan.log
          clamscan $FILE >> $HOME/virus-scan.log
          kdialog --title "Virus scan of $FILE" --msgbox "$(cat /tmp/virus-scan.log)"
     fi
done
