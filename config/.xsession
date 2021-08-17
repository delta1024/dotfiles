export ZDOTDIR="$HOME/.config/zsh"
. "$HOME/.config/guix/active-profiles"
dunst &
picom &
# sxhkd &
xclip &
/usr/libexec/polkit-mate-authentication-agent-1 &
$HOME/.scripts/wallpaper.sh draw
xmodmap ~/.Xmodmap 
emacs -mm -exwm --degub-init
