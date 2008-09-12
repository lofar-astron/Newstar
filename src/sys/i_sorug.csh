#
#  Machine specific rules for Sun Solaris at RUG (HjV 961212)
#
set FC=/opt/SUNWspro/bin/f77
#set LD_X11=/usr/lib/X11/libX11.a
set LD_USER="-lsocket -lnsl"
set LD_USER="$LD_USER -L/usr/ucblib -lucb -L/usr/lib -ldl -lw"

