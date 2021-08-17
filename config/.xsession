export ZDOTDIR="$HOME/.config/zsh"
export GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
dunst &
picom &
# sxhkd &
xclip &
/usr/libexec/polkit-mate-authentication-agent-1 &
$HOME/.scripts/wallpaper.sh draw
xmodmap ~/.Xmodmap 
emacs -mm -exwm --degub-init
