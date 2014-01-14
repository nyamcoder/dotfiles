




# Created by newuser
#
#   _________  _   _ ____   ____ 
#  |__  / ___|| | | |  _ \ / ___|
#    / /\___ \| |_| | |_) | |    
# _ / /_ ___) |  _  |  _ <| |___ 
#(_)____|____/|_| |_|_| \_\\____|
#


###	T T Y - F R A M E B U F F E R 
fbterm 2>/dev/null


###	Terminal-Fenstertitel anzeigen (X, multiplexer_div)
#[[ -t 1 ]] || return
#case $TERM in
#    *xterm*|*rxvt*|(dt|k|E|a)term)
#    preexec () {
#    #print -Pn "\e]2;[%l] [%n@%m] <$1> | ${COLUMNS}x${LINES}\a"
#    print -Pn "\e]2;%l [${COLUMNS}x${LINES}] :: $1\a"
#    }
#    ;;
#    screen*)
#        preexec () {
#        # this doesn't do anything... *wah*
#        #print -Pn "\e\"[screen] [%l] [%n@%m] <$1>\e\134"
#        print -Pn "\e\"$1\e\134"
#    }
#  ;;
#esac 


# 
#	zsh-grml-config: siehe  ~/.zshrc.local
source $HOME/.zshrc.local

###	F A R B E N
#source /home/ottugi/.scripts/live-command-coloring.sh
### postinstall von aur-- cw (colorwrapper) --> am Schluss von .zshrc anfügen

###	P A T H
export PATH="/usr/lib/ccache/bin/:$HOME/.bin/:$HOME/.cabal/bin:/usr/lib/cw:$PATH" 
