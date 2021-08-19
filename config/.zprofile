#!/bin/zsh
export ZDOTDIR="$HOME/.config/zsh"
source /etc/profile
. "$HOME/.config/guix/active-profiles"
xmodmap ~/.Xmodmap
export $(dbus-launch)
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	sleep 1
startx
fi
