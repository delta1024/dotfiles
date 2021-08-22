dunst &
picom &
xclip &
#/usr/libexec/polkit-mate-authentication-agent-1 &
$HOME/.scripts/wallpaper.sh draw
exec dbus-launch --exit-with-session emacs -mm -exwm --debug-init
