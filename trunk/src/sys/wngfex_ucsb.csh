#! /bin/csh
#+wngfex_ucsb.csh
#
#  File with system dependent commands for Newstar site UCSB
#
#  Called by wngfex.csh with the command in $loa, the name of the 
#  input file in $lob and the name of the spool-file in $loc.
#  The name for a temporary spool file is in $loct.
#  The status of the command, if available, should be returned in statx.
#

if ($loa =~ sp* || $loa =~ la* || $loa =~ ps* || $loa =~ a3*) then  # spool all
   ln -s $lob $loct
   @ statx = { lpr $loct }
   set statx=1                              # make sure file kept
endif
#-

