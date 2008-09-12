#
#  Machine specific rules for Convex (CMV 930524)
#
#
# Fortran compiler commands
#
set FC=fc
set FFLAGS="-c -na -nw -vfc -sa -LST"
set FFLAGS_D="-g"
set FFLAGS_O="-O3"
set FFLAGS_X="-xr"
#
# C compiler commands
#
set CC=cc
set CFLAGS="-c -na -nw"
set CFLAGS_D="-g"
set CFLAGS_O="-O"
#
# Assembler commands
#
set AS=as
set ASFLAGS=
#
# Linking (invoked through $FC)
#
set FFLAGS_L="-g -sa -vfc -O3 -na -nw -M"
set LD_X11=-lX11
#
# Data type on this machine
#
set Dattyp=5
