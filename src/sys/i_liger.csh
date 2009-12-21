#
#  Machine specific rules for Linux at NFRA (AxC 010130)
#
set CC="gcc-3.3"
set FFLAGS="-c -w -Wall -I$n_inc -fno-automatic -finit-local-zero -fugly-logint-fno-backslash -fdollar-ok -fno-second-underscore -m32"
set CFLAGS="-c -w -I$n_inc -m32"
set LD_X11="-L/usr/lib -lX11"
#set LD_USER="-L/usr/lib -lncursesw"



