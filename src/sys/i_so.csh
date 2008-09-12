#
#  Machine specific rules for Sun Solaris (HjV 960618)
#

#
# Fortran compiler commands
#
set FC=f77
set FFLAGS="-c -w -xl -Nl50 "
set FFLAGS_D=-g
set FFLAGS_O=
set FFLAGS_X=
#
# C compiler commands
#
set CC=gcc
set CFLAGS="-c -w"
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
set FFLAGS_L="-g -xl -Qoption ld -m"
set LD_X11=-lX11
#
# Data type on this machine
#
set Dattyp=7

