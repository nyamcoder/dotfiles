#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

### wikis:
### https://wiki.archlinux.org/index.php/Xinitrc
### https://wiki.archlinux.org/index.php/Fonts#Older_applications
### https://wiki.archlinux.org/index.php/Fonts#Fonts_with_Xorg
### https://wiki.archlinux.org/index.php/Font_Configuration
### https://wiki.archlinux.org/index.php/Xresources

xset +fp $HOME/.fonts; xset fp rehash
xrdb -merge $HOME/.Xresources
xsetroot -cursor_name left_ptr 
#xcompmgr -nC &
setxkbmap -layout de
##!/bin/sh
 
# Start URxvt daemon so we can quickly open the other clients
##urxvtd -q -o -f [[ -x "/usr/bin/numlockx" ]] && numlockx & [[ -x "/usr/bin/unclutter" -a -z "`pidof unclutter`" ]] && unclutter -idle 5 -root&
##urxvtd -q -f -o&

#killall ibus-daemon
#ibus-daemon --xim &	# --xim für urxvt
stalonetray&
#procmeter3&
##terminal&
##urxvt -xrm "URxvt.session:"&

# http://wiki.ubuntuusers.de/rxvt-unicode#Im-Daemon-Modus
urxvtd -f
urxvtc -xrm "URxvt.perl-ext: " &	# für ohne tab

#(sleep 10 && stalonetray) &

#mpd &


## font-pfade:
## https://wiki.archlinux.org/index.php/Adding_fonts#Older_applications
for path in /usr/share/fonts/local/* $HOME/.fonts/*/* $HOME/.fonts/*/*/*; do xset +fp $path; xset fp rehash


#mpd $HOME/.mpdconf2 &

# Set Wallpaper with feh
#eval `cat $HOME/.fehbg` &
feh --bg-fill $HOME/.xmonad/wallpapers/saehae.jpg &


# Launch tray and statusbar
stalonetray -i 16 --max-width 48 --icon-gravity E --geometry 48x16-0+0 -bg '#2e3436' --sticky --skip-taskbar & 
$HOME/.bin/dzen.sh | dzen2 -e 'onstart=lower' -p -ta r -fn 'DotumChe-11' -bg '#2e3436' -fg '#babdb6' -h 16 -w 1632 &

#(sleep 5 && stalonetray) &

xscreensaver -nosplash &

xmobar &

#exec dbus-launch xmonad
#exec ck-launch-session dbus-launch ~/.cabal/bin/xmonad
exec dbus-launch ~/.cabal/bin/xmonad
