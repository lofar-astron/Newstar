$!#  dwarflogout.ssc
$!#  GvD 911203
$!#
$!#  Revisions:
$!#	WNB 930302	Make ssc
$!#
$!#	Save DWARF symbols
$!#
$ DELETE/SYMBOL/GLOBAL LOGOUT
$ NAME=F$SEARCH("SYS$LOGIN:LOGOUT.COM")
$ IF NAME.NES."" THEN @'NAME'
$ LOGOUT
