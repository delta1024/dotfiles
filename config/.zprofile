#!/bin/zsh
export ZDOTDIR="$HOME/.config/zsh"
export $(dbus-launch)
#doas rc-service ntp-client start &
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	sleep 1
startx
fi
