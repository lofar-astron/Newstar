#!/bin/csh
#CMV 940224
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch  wsrt_nst  [cycleno_start cycleno_end]
#
#  If no cyclus numbers are given, they will be asked for.
#  Preferably, you should give a range which contains a single 
#  calibrator-source-calibrator group (casca)
#
#
#  This file contains the overall procedure, the details are in:
#       load_data.csh
#
#       self_cal.csh       Self-calibration of single calibrator
#       copy_cal.csh       Copy calibrator info to mapfile
#       make_map.csh       Make a map 
#
#       eval_cal.csh       Display calibrated data with NGIDS, allow flagging
#       eval_raw.csh       Display raw data with NGIDS, allow flagging
#       eval_map.csh       Display a map with NGIDS
#       plot_detail.csh    Make detailed plots using NPLOT
#
#
#  Revisions:
#	CMV 940224	Created
#

#
#  Preliminaries: get date, time, init Newstar, ...
#
set C_Version=1.0
set Rootfile="casca"
source $n_batch/init_batch.csh

echo "Welcome to the WSRT on-site analysis (v $C_Version)"

#
#  The off-line programs force us to work in ~/nst
#
cd ~/nst
echo "We are now in directory $cwd for Newstar processing."

#
#  Now we first get some data in a SCN file
#
source $n_batch/load_data.csh
if ("$Status" != "ok") exit

#
#  The next step is to calibrate the calibrators
#

foreach Field ( `awk '{ if ($2 == "Cal") print $1}' $Rootfile.status `)

  dwlet field=$Field

#
#  Iteratively do the selfcalibration
#
  set Status="calibrate"
  while ("$Status" == "calibrate") 

     source $n_batch/self_cal.csh

#
#  Evaluate the result, do manual flagging if necessary
#
     if ("$Status" == "ok") source $n_batch/eval_cal.csh

#
#  Plot details if necessary
#
     while ("$Status" == "detail") 
        source $n_batch/plot_detail.csh
     end

  end     

end

#
#  The calibrators are fine, now the sources
#

foreach Field ( `awk '{ if ($2 == "Src") print $1}' $Rootfile.status `)

   setenv FIELD $Field

#
#  Copy calibration data (if possible!)
#
   source $n_batch/copy_cal.csh

#
#  Evaluate the calibration if something could be copied
#
   if ("$Status" == "calibrated") then

      source $n_batch/eval_cal.csh

#
#  Or just look at the data if no calibrator available
#
   else

      source $n_batch/eval_raw.csh

   endif

#
#  Again we may want to plot the details
#
   while ("$Status" == "detail")
     source $n_batch/plot_detail.csh
   end

#
#  Finally make a map and display it. We start with channel 0
#   
   dwlet Channel=0
   source $n_batch/make_map.csh

#
#  Display the map
#   
   if ("$Status" == "ok") source $n_batch/eval_map.csh

#
#  We may want to make other channels
#
   while ("$Status" == "more") then
      source $n_batch/make_map.csh
      if ("$Status" == "ok") source $n_batch/eval_map.csh
   endif

end

#
#  That is it, tell them where the log-files can be found
#
log "General logging information is in $Logfile"
log "The project status is in $Rootfile.status, the project history,"
log "including your quality judgements is in $Rootfile.history"


