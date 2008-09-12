#
#  Machine specific rules for DEC Workstation (CMV 930524)
#
#
# Fortran compiler commands
#
set FC=f77
set FFLAGS="-c -V -w -assume back"
set FFLAGS_D="-g"
set FFLAGS_O="-O1"
set FFLAGS_X="-xref"
#
# C compiler commands
#
set CC=cc
set CFLAGS="-c -w"
set CFLAGS_D="-g"
set CFLAGS_O="-O1"
#
# Assembler commands
#
set AS=as
set ASFLAGS=
#
# Linking (invoked through $FC)
#
set FFLAGS_L="-g -assume back -Wl,-M"
set LD_X11=-lX11
#
# Data type on this machine
#
set Dattyp=6

