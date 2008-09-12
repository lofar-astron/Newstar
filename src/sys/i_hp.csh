#
#  Machine specific rules for HP Workstation (CMV 930524)
#
#  All possible settings should be in this file, so you can 
#  use it as a template for any other Unix machine.
#

#
# Fortran compiler commands
#
#set FC=f77
#set FFLAGS="-c -g +e +es +ppu -Nl50"
#set FFLAGS_D="-g"
#set FFLAGS_O="-O"
#set FFLAGS_X=""
set FC=g77
set FFLAGS="-c -w -Wall -I$n_inc -fno-automatic -finit-local-zero -fugly-logint -fno-backslash -fdollar-ok -fno-second-underscore"
set FFLAGS_D=-g
set FFLAGS_O=
set FFLAGS_X=
#
# C compiler commands
#
#set CC=cc
#set CFLAGS="-c -w"
#set CFLAGS_D="-g"
#set CFLAGS_O="-O"
#if (-d /usr/include/X11R5) then
#   set CFLAGS=( $CFLAGS -I/usr/include/X11R5 -I$n_inc)
#else
#   set CFLAGS=( $CFLAGS -I/usr/include/X11R4 -I$n_inc)
#endif
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
#set FFLAGS_L="-g +e +es +ppu"
set FFLAGS_L=" -fugly-logint -fno-backslash -fdollar-ok -fno-second-underscore"
set LD_X11="-L/usr/lib/X11 -lX11"#
# Data type on this machine
#
set Dattyp=8
