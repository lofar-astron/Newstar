#! /bin/csh
#+wngfex_ucb.csh
#
#  File with system dependent commands for Newstar site UCB
#
#  Called by wngfex.csh with the command in $loa, the name of the 
#  input file in $lob and the name of the spool-file in $loc.
#  The name for a temporary spool file is in $loct.
#  The status of the command, if available, should be returned in statx.
#

if ($loa =~ sp*) then				# spool
   ln -s $lob $loct
   @ statx = { enscript -r $loct }
   set statx=1					# make sure file kept
endif

if ($loa =~ ps* || $loa =~ a3*) then		# spool PS /A3
   ln -s $lob $loct
   @ statx = { lp $loct }
   set statx=1					# make sure file kept
endif
#-

