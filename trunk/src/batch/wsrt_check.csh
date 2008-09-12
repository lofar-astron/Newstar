#!/bin/csh
#CMV 940224
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch  wsrt_nst  cyclusno
#
#  If no cyclus number is given, it will be asked for.
#
#  This procedure loads the data in a Scan file and selects one of 
#  several actions depending on the type of data:
#
#  Astronomical measurements:  make a raw I map of channel 0
#   if Mosaic:                 make a raw I and Q mosaic maps of channel 0
#   if broadband DCB:          make some plots of the data
#
#  This batch script just calls procedures that can also be 
#  invoked "by hand".
#
#       load_data.csh      Load data from circle-file
#       
#       make_map.csh       Make a map
#       make_mos.csh       Make mosaic map
#       plot_92cal.csh     Plot 92cm broadband (9A,8B,7C,6D Amp, phase)
#       plot_92src.csh     Plot 92cm broadband (9A,8B,7C,6D Cos)
#
#  Revisions:
#	CMV 940329	Created
#
#set echo
#
# Get the argument 
#
if ("$1" == "") then
   echo -n "Enter the cyclus number to load: "
   set $1=($<)
endif
set argv=( $1 $1 )   # Load just one frame
setenv ROOTFILE "c$1"

#
#  Preliminaries: get date, time, init Newstar, ...
#
set C_Version=1.0
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
#  Have a look at the data to see what we should do
#
setenv FIELD "000.000"

#
#  Get the obs.type and the number of fields
#
set Flag=(`awk '{ if ($1 == "'$FIELD'") print $2 " " $9}' $ROOTFILE.status `)   

#
#  If it's a source with just one pointing centre, make a channel 0 map
#
if ($Flag[1] == "Src" && $Flag[2] == 1) then

   setenv CHANNEL 0

   source $n_batch/make_map.csh

   if ("$Status" == "ok") then
      setenv PLOTTER PSP
      dwe nplot.plmap
   endif

#
#  If it's a mosaic, first make a mosaic map
#   
else if ($Flag[1] == "Src" && $Flag[2] > 1) then
   setenv CHANNEL 0

   source $n_batch/make_mos.csh

   if ("$Status" == "ok") then
      setenv PLOTTER PSP
      dwe nplot.plmos
   endif
endif

#
#  Remove all Newstar LOG and PLT files, any information should 
#  have been extracted or printed.
#
set nonomatch
'rm' -f *.LOG *.PLT
unset nonomatch

#
#  That is it, tell them where the log-files can be found
#
log "General logging information is in $Logfile"
log "The project status is in $ROOTFILE.status, the project history,"
log "is in $ROOTFILE.history"

