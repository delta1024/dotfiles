#!/bin/zsh
export ZDOTDIR="$HOME/.config/zsh"
. "$HOME/.config/guix/active-profiles"
export $(dbus-launch)
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	sleep 1
startx
fi
