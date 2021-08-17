#!/bin/zsh
export ZDOTDIR="$HOME/.config/zsh"
export $(dbus-launch)
export GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
#doas rc-service ntp-client start &
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	sleep 1
startx
fi
