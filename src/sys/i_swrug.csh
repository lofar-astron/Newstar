#
#  Machine specific rules for Sun Workstations at RUG (CMV 931006)
#
set FC=/usr/lang/SC1.0/f77
set FFLAGS="-c -w -xl -Nl50"
set FFLAGS_L="-g -xl -Qoption ld -M -Bstatic"
set LD_X11=/usr/openwin/lib/libX11.a
#
# Data type on this machine
#
set Dattyp=7

