#! /bin/csh
#+wngfex_irabo.csh
#
# Revision:
#
#  File with system dependent commands for Newstar site IRABO
#
#  Called by wngfex.csh with the command in $loa, the name of the 
#  input file in $lob and the name of the spool-file in $loc.
#  The name for a temporary spool file is in $loct.
#  The status of the command, if available, should be returned in statx.
#

if ($loa =~ sp*) then                           # spool
   ln -s $lob $loct
   @ statx = { lpr -Plpa0 $loct }
   set statx=1					# make sure file kept

else if ($loa =~ qm*) then			# spool QMS

else if ($loa =~ ps*) then			# spool PS (A4)
   ln -s $lob $loct
   @ statx = { print $loct }
   set statx=1					# make sure file kept

else if ($loa =~ a3*) then			# spool A3

else if ($loa =~ la*) then			# spool LA

endif
#-

