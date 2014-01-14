#/bin/sh


#DATE
DATECMD="`date +%H:%M:%S`"
#DATECMD=`LANG=ko_KR.UTF8 date +"%y년%b%e일 (%a) %H:%M %Z"`
DATESEP="$SEPCOL" 
DATE="$DATECMD" 


### nur datum:
wmfs -s  "                \s[2000;30;#D4D4D4;$DATE]\ " ## ggf. nach draußen verschieben x;y 

