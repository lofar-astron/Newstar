#
#  Machine specific rules for Sun Solaris at ATNF (WNB 960610)
#
set LD_USER="-lsocket -lnsl"
set LD_USER="$LD_USER -L/usr/ucblib -lucb"

