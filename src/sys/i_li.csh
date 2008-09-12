#
#  Machine specific rules for Linux AxC 010130)
#

#
# Fortran compiler commands
#
set FC=g77
set FFLAGS="-c -w -Wall -I$n_inc -fno-automatic -finit-local-zero -fugly-logint -fno-backslash -fdollar-ok -fno-second-underscore"
set FFLAGS_D=-g
set FFLAGS_O=
set FFLAGS_X=
#
# C compiler commands
#
set CC=gcc
set CFLAGS="-c -w -I$n_inc"
set CFLAGS_D=-g
set CFLAGS_O=
#
# Assembler commands
#
set AS=as
set ASFLAGS=
#
# Linking (invoked through $FC)
#
set FFLAGS_L=" -fugly-logint -fno-backslash -fdollar-ok -fno-second-underscore"
#set LD_X11=/usr/X11/lib/libX11.a
set LD_X11="-L/usr/X11/lib -lX11"
#
# Data type on this machine
#
set Dattyp=6













