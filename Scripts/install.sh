#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo "$DIR"
ln -s $REMOVE "$DIR/scripts" $HOME/.local/scripts
mkdir -p $HOME/.config/sxhkd
ln -s $REMOVE "$DIR/sxhkd" $HOME/.config/sxhkd/scripts
