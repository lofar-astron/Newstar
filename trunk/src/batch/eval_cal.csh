#!/bin/csh -f
#CMV 940225
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch eval_cal
#
#  This batch file will show the result of the selfcalibration
#
#  Dwarf symbols used:   
#     ROOTFILE  The name of the scanfile to use
#     FIELD     The sector in the scanfile to calibrate
#
#  Revisions:
#	CMV 940328	Created (does not yet work)
#

#
#  Preliminaries: get date, time, init Newstar, ...
#
source $n_batch/init_batch.csh

#
#  Show the channel, allow flagging
#

if (-e $ROOTFILE.FLA) then
   phistory "eval_cal: Flags file created"
   dwe nflag.put | tee -a $Tmpfile
   phistory "eval_cal: Flags applied to data"
   grep "^ #" $Tmpfile >>$ROOTFILE.status
   set Status="calibrate"     # Need to redo selfcalibration
else
   echo -n "What's the geneal quality of the data? "
   set Flag=($<)
   phistory "eval_cal: $Flag"
   echo -n "Do you want to inspect things in detail (y,n)? [n] "
   set Flag=($<)
   if ("$Flag" =~ [Yy]*) then
      set Status="detail"
   else
      set Status="ok"
   endif
endif
