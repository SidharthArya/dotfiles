#!/bin/sh
CURRENT_DIRECTORY="$(pwd)"
export IFS=$'\n'
for i in $(cat ~/.local/scripts/assets/repository);
do
    cd "$i";
    git add .
    git commit -m "Auto Commit: $(date)";
    git push
done
cd "$CURRENT_DIRECTORY"
