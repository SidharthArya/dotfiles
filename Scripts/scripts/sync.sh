#!/bin/sh
sh ~/.local/scripts/repository.sh
CURRENT_DIR="$(pwd)"
cd "$(cat ~/Documents/Sync/drive)";
drive push *;
cd "$CURRENT_DIR"
