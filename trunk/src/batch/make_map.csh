#!/bin/csh -f
#CMV 940329
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch make_map
#
#  This batch file will make a map of a single channel
#
#  Dwarf symbols used:   
#     ROOTFILE  The name of the scanfile to use
#     FIELD     The sector in the scanfile to calibrate
#     CHANNEL   The channel specification for the map
#
#  Revisions:
#	CMV 940328	Created
#       CMV 940705      Optionally ask CHANNEL
#

#
#  Preliminaries: get date, time, init Newstar, ...
#
source $n_batch/init_batch.csh

if (! $?CHANNEL) then
   echo -n "Enter channel(s) to do: "
   set noglob
   set ans=($<)
   setenv CHANNEL $ans
   unset noglob
endif

if (! $?FIELD) then
   echo -n "Enter the sector(s) to do (eg 0.0): "
   set noglob
   set ans=($<)
   setenv FIELD $ans
   unset noglob
endif

#
#  Call nmap to make the map
#
log "Making map for Channel $CHANNEL"
dwe nmap.chmap >> $Logfile

#
#  Check exit status
#
if (-e $Rootfile.WMP  && `genaid size $Rootfile.WMP` > 512) then
   set Status="ok"
else
   set Status="Could not produce map"
endif

phistory "make_map $CHANNEL : $Status"
