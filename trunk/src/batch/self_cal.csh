#!/bin/csh -f
#CMV 940225
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch self_cal
#
#  This batch file will self-calibrate a single calibrator.
#
#  Dwarf symbols used:   
#     ROOTFILE  The name of the scanfile to use
#     FIELD     The sector in the scanfile to calibrate
#
#  Revisions:
#	CMV 940328	Created
#

#
#  Preliminaries: get date, time, init Newstar, ...
#
source $n_batch/init_batch.csh

#
#  First flag on shadowing
#
dwe nflag.shadow 
phistory "self_cal shadow: \
 `awk '/# data XX/ {printf "flagged %s%% of %s uv-points",$8,$14}' NFLAG.LOG ` "

#
#  Use NCALIB to do the self-calibration on all channels
#
if (! $?FIELD)   setenv FIELD "000.000"

setenv NCHAN `awk '{ if ($1 == "'$FIELD'") print $8}' $ROOTFILE.status`
setenv SCHAN 1
setenv FCHAN 0
    
dwe ncalib.selfcal
if (`grep -c 'Pure redundancy used'` == 0) then
   phistory "self_cal calibrate: selfcalibration used"
else
   phistory "self_cal calibrate: pure redundancy used"
endif

#
#  Assume ok (even if no calibrator model, redundancy will work)
#    
set Status=ok
