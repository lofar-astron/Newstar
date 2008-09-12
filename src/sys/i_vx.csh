#
#  Machine specific rules for VMS machines (WNB 940315)
#

#
# Fortran compiler commands
#
set FC=fortran
set FFLAGS="/F77/EXTEND/I4/SHOW=(NODICT,NOINCL,MAP,NOPREP,NOSIN)"
set FFLAGS_D="/DEBUG/WARN=NOGEN"
set FFLAGS_O="/OPTIM"
set FFLAGS_X="/CROSS/FULL"
set FFLAGS_L="/F77/EXTEND/I4/SHOW=(NODICT,NOINCL,MAP,NOPREP,NOSIN)/DEBUG/OPTIM"
#
# C compiler commands
#
set CC=CC
set CFLAGS='/LIST/OPTIM/DEF="wn_vx__/name=as_is"'
set CFLAGS_D="/DEBUG"
set CFLAGS_O="/OPTIM"
#
# Assembler commands
#
set AS=MACRO
set ASFLAGS=
#
# Linking (invoked through $FC)
#
set FFLAGS_L="/F77/EXTEND/I4/SHOW=(NODICT,NOINCL,MAP,NOPREP,NOSIN)/DEBUG/OPTIM"
set LD_X11=
#
# Data type on this machine
#
set Dattyp=1

