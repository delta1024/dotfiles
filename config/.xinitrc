dunst &
picom &
# sxhkd &
xclip &
# /usr/libexec/polkit-mate-authentication-agent-1 &
$HOME/.scripts/wallpaper.sh draw
xmodmap ~/.Xmodmap 
# mpd ~/.config/mpd/mpd.conf 2>/dev/null &
exec dbus-launch --exit-with-session emacs -mm -exwm --debug-init
