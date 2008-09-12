#
#  Machine specific rules for Sun Solaris at NFRA (HjV 960618)
#
set LD_USER="-lsocket -lnsl"
set LD_USER="$LD_USER -L/usr/ucblib -lucb"

