#!/bin/csh -f
#CMV 940225
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch load_data [cycleno_start cycleno_end]
#
#  This batch file loads data into a SCN file
#
#  If no cyclus numbers are given, they will be asked for.
#  Preferably, you should give a range which contains a single 
#  calibrator-source-calibrator group (casca).
#
#
#  Revisions:
#	CMV 940225	Created
#	CMV 940328	Separate dista call for each cycleno.
#

#
#  Preliminaries: get date, time, init Newstar, ...
#
source $n_batch/init_batch.csh

#
#  Use findjm to get the range that is available
#
ksh <<_EOD_ >& $Tmpfile
. ~/.kshrc
findjm
exit
_EOD_

set Flag=`tail -1 $Tmpfile`
echo "Available cyclus numbers: $Flag[1] - $Flag[2] (seq.nr $Flag[3] - $Flag[4])"

#
#  Get the cycle number range to process: if no arguments given ask from user
#
set cstart="$1"
if ("$cstart" == "") then
   echo -n "Enter the first cyclus number to process [$Flag[1]]: "
   set cstart="$<"
   if ("$cstart" == "") set cstart=$Flag[1]
endif

set cstop="$2"
if ("$cstop" == "") then
   echo -n "Enter the last cyclus number to process [$Flag[2]]: "
   set cstop="$<"
   if ("$cstop" == "") set cstop=$Flag[2]
endif


log "Loading cyclus numbers $cstart - $cstop"

#
# Remove old files
#
set nonomatch
rm -f $ROOTFILE.* $Rootfile.*
unset nonomatch

#
# Load through dista, show limited set of output, save the rest
#
set current=$cstart
set label=1

if (-e $Tmpfile) rm -f $Tmpfile

while ($current <= $cstop)

  ksh <<_EOD_ >>$Tmpfile
. ~/.kshrc
dista
2
$Rootfile
$label
$current,$current

-1,-1
Y
exit
_EOD_

  set nonomatch
  if (-e $Rootfile.*$label) echo "Loaded $current"
  unset nonomatch

  @ current = $current + 1
  @ label   = $label   + 1
end

#
# Check wether we have a file, if so convert to SCN file
#
if (-e $Rootfile.000001) then
   log "Data loaded to disk, now converting to a SCAN-file"
   dwe nscan.load >> $Logfile
   rm -f $Rootfile.??????
endif

#
# If no scanfile, show errors and exit
#
if (! -e $Rootfile.SCN) then
   log "Error producing SCN file, the following information may help..."
   cat $Tmpfile
   set Status="No SCN file"
#
# else generate the project status
#
else
   rm -f $Tmpfile
   log "Created SCAN-file $Rootfile.SCN"
   echo "Project status for $ROOTFILE (created $C_Date)" >$ROOTFILE.status
   dwe nscan.overview | grep -v NSCAN >>$ROOTFILE.status
   set Status="ok"
endif

phistory "load_data ${cstart},${cstop}: $Status"

