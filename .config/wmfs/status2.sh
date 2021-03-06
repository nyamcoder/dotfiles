### nach https://bbs.archlinux.org/viewtopic.php?pid=881204#p881204
# Paths
# -----
ICONPATH="$HOME/.config/icons-1"

# -------------
# Defining VARS
# -------------
        DATE=$(date "+%H:%M")
        SDA4=$(df -h /home | tail -1 | awk -F' ' '{print $4}')
        SDA2=$(df -h / | tail -1 | awk -F' ' '{print $3}')
        TEMP=$(cat /proc/acpi/thermal_zone/TZ01/temperature | awk '{print $2}')
        VOL=$(amixer get Master | awk '/Front\ Left:/ {print $5}' | sed -e 's/\[//g' -e 's/]//g')
        MEM=$(free -t -m | grep '^Total:' | awk '{print $3}')
        # CPU lines courtesy Procyon: https://bbs.archlinux.org/viewtopic.php?pid=661592
        CPU0=$(eval $(awk '/^cpu0 /{print "previdle=" $5 "; prevtotal=" $2+$3+$4+$5 }' /proc/stat); 
            sleep 0.4; 
            eval $(awk '/^cpu0 /{print "idle=" $5 "; total=" $2+$3+$4+$5 }' /proc/stat); 
            intervaltotal=$((total-${prevtotal:-0})); 
            echo "$((100*( (intervaltotal) - ($idle-${previdle:-0}) ) / (intervaltotal) ))")
#        CPU1=$(eval $(awk '/^cpu1 /{print "previdle=" $5 "; prevtotal=" $2+$3+$4+$5 }' /proc/stat); 
#            sleep 0.4; 
#            eval $(awk '/^cpu1 /{print "idle=" $5 "; total=" $2+$3+$4+$5 }' /proc/stat); 
#            intervaltotal=$((total-${prevtotal:-0})); 
#            echo "$((100*( (intervaltotal) - ($idle-${previdle:-0}) ) / (intervaltotal) ))")

# ---------
# The print 
# ---------
wmfs -s "
        \i[896;3;0;0;$ICONPATH/temp.png]\ 
        \s[910;12;#DCDCDC;$TEMP C]\ 
        \s[933;12;#8F8F8F;|]\ 
        \i[940;3;0;0;$ICONPATH/cpu.png]\ 
        $(wmfs-status-gauge 954 2 35 3 '#df0031' 1 '#333333' $CPU0) \
#        $(wmfs-status-gauge 954 8 35 3 '#df0031' 1 '#333333' $CPU1) \
        \s[996;12;#8F8F8F;|]\ 
        \i[1003;3;0;0;$ICONPATH/mem.png]\ 
        \s[1016;12;#DCDCDC;$MEM MB]\ 
        \s[1052;12;#8F8F8F;|]\ 
        \i[1060;3;0;0;$ICONPATH/hd.png]\ 
        \s[1073;12;#DCDCDC;/: $SDA2]\ 
        \s[1113;12;#DCDCDC;/home: $SDA4]\ 
        \s[1168;12;#8F8F8F;|]\ 
        \i[1177;3;0;0;$ICONPATH/spkr_01.png]\ 
        $(wmfs-status-gauge 1189 4 35 5 '#2c8cd7' 1 '#333333' $VOL) \
        \s[1230;12;#8F8F8F;|]\
        \s[1250;12;#DCDCDC;$DATE]\
        \i[1237;3;0;0;$ICONPATH/clock.png]\
        "
