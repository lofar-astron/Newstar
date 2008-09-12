#! /bin/csh
#+wngfex_nfra.csh
#
#  File with system dependent commands for Newstar site NFRA
#  Revision:
#	950130 HjV	Use $HOME in directories iso. ~ or ~/
#	950705 HjV	Add A0-plotter
#	970121 HjV	Use LPDEST (if set), else use PSPRINT
#			Add Solaris
#
#  Called by wngfex.csh with the command in $loa, the name of the 
#  input file in $lob and the name of the spool-file in $loc.
#  The name for a temporary spool file is in $loct.
#  The status of the command, if available, should be returned in statx.
#

if ($loa =~ sp*) then                           # spool
   @ statx = { ftp -n << _EOD_ }
     open rzmvx5
     user printvax printvax_90a
     put $lob lpa0:$loct                         # print
     close
     quit
_EOD_

else if ($loa =~ qm*) then                      # spool QMS
#   @ statx = { ftp -n << _EOD_ }
#     open rzmvx5
#     user printvax printvax_90a 
#     binary
#     put $lob $loct                            # print
#     close
#     quit
#_EOD_

else if ($loa =~ ps* || $loa =~ a3*) then       # spool PS / A3
   ln -s $lob $loct
   if ("$n_arch" == "sw" || "$n_arch" == "so" ) then
      if ($?LPDEST) then			# did user set LPDEST
         @ statx = { lp $loct }			# yes
      else
         @ statx = { lpr -Ppsprint $loct }	# no
      endif
#      @ statx = { lpr -Pqms860 $loct }
      set statx=1                              # make sure file kept
   else if ("$n_arch" == "hp") then
      if ($?LPDEST) then			# did user set LPDEST
         @ statx = { lp $loct }			# yes
      else
         @ statx = { lpr -dpsprint $loct }	# no
      endif
#      cp $loct $HOME/$loct
#      remsh rzmws0 'lpr -Pqms860 $HOME/'$loct
#      sleep 10
#      rm $HOME/$loct
#      set statx=1                              # make sure file kept
#   else if ("$n_arch" == "al") then
#      @ statx = { lpr -Ppmq $loct }
#      set statx=1                              # make sure file kept
   else
      set statx=1                              # make sure file kept
   endif

else if ($loa =~ a0*) then		       # spool A0
   ln -s $lob $loct
   if ("$n_arch" == "sw" || "$n_arch" == "so" ) then
      @ statx = { lpr -Pjet650 $loct }
      set statx=1                              # make sure file kept
   else if ("$n_arch" == "hp") then
      @ statx = { lpr -djet650 $loct }
      set statx=1                              # make sure file kept
   else
      set statx=1                              # make sure file kept
   endif

else if ($loa =~ la*) then                      # spool LA
   ln -s $lob $loct
   @ statx = { ftp -n << _EOD_ }
     open rzmvx4
     user printvax printvax_90a
     put $lob TXA4:$loct                       # print
     close
     quit
_EOD_

endif
#-

