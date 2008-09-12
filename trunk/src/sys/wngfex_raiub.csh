#! /bin/csh
#+wngfex_raiub.csh
#	960802 HjV		Add Alpha/OSF1
#	971029 Helge Rottmann	Add Solaris stuff
#
#  File with system dependent commands for Newstar site RAIUB
#
#  Called by wngfex.csh with the command in $loa, the name of the 
#  input file in $lob and the name of the spool-file in $loc.
#  The name for a temporary spool file is in $loct.
#  The status of the command, if available, should be returned in statx.
#

if ($loa =~ sp*) then                           # spool
   ln -s $lob $loct
   if ("$n_arch" == "sw" ) then
      @ statx = { lpr $loct }
   else if ("$n_arch" == "da" ) then
      @ statx = { lpr -Plp $loct }
   endif

else if ($loa =~ qm*) then                      # spool QMS

else if ($loa =~ ps*) then                      # spool PS
   ln -s $lob $loct
   if ("$n_arch" == "sw" ) then
      @ statx = { lpr -Pps2 $loct }
   else if ("$n_arch" == "so" ) then
      @ statx = { lp -d ps2 -o nobanner $loct }
   else if ("$n_arch" == "da" ) then
      @ statx = { lpr -Pps2 $loct }
   endif
   set statx=1                                     # make sure file kept

else if ($loa =~ a3*) then                      # spool A3

else if ($loa =~ la*) then                      # spool LA

endif
#-
