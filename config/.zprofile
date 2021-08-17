#!/bin/zsh
export ZDOTDIR="$HOME/.config/zsh"
export $(dbus-launch)
export GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
export GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
#doas rc-service ntp-client start &
for i in $GUIX_EXTRA_PROFILES/*; do
  profile=$i/$(basename "$i")
  if [ -f "$profile"/etc/profile ]; then
    GUIX_PROFILE="$profile"
    . "$GUIX_PROFILE"/etc/profile
  fi
  unset profile
done
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	sleep 1
startx
fi
