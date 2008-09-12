#
#  Machine specific rules for Alliant (CMV 930524)
#
#
# Fortran compiler commands
#
set FC=fortran
set FFLAGS="-c -e -w"
set FFLAGS_D="-g"
set FFLAGS_O="-O -OM -alt"
set FFLAGS_X="-xref"
#
# C compiler commands
#
set CC=cc
set CFLAGS="-c -ce -w"
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
set FFLAGS_L="-g -M"
set LD_X11=-lX11
#
# Data type on this machine
#
set Dattyp=7


