#!/bin/csh -f
#CMV 940329
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Usage (make sure Newstar has been initialised):   
#
#      nbatch make_map
#
#  This batch file will make a map of a single channel for mosaic data
#
#  Dwarf symbols used:   
#     ROOTFILE  The name of the scanfile to use
#     FIELD     The sector in the scanfile to calibrate
#     CHANNEL   The channel specification for the map
#
#  Revisions:
#	CMV 940328	Created
#	CMV 940705	Optionally ask CHANNEL and FIELD
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
   echo -n "Enter the group.observation to do (eg 0.0): "
   set noglob
   set ans=($<)
   setenv FIELD $ans
   unset noglob
   setenv FIELD `echo $FIELD | awk -F. '{printf("%03d.%03d",$1,$2)}'`
endif

if (-e $ROOTFILE.status) then
  set Flag=( `awk '{ if ($1 == "'$FIELD'") print $9}' $ROOTFILE.status` )   
else
  echo -n "Enter the number of fields to do: "
  set Flag=($<)
endif
setenv NFIELD $Flag

#
#  Find the reference position
#
@ Flag = $NFIELD / 2
setenv MIDDLE $Flag
set Flag=(`dwe nscan.mosref | awk '/(1950)/ {if ($2 == "(1950)") printf "%s, ",$3; else printf "%s ",$2}' `)
if ("$Flag" == "") set Flag="40"
setenv MOSCENTR "$Flag"

#
#  Call nmap to make the map
#
log "Making mosaic maps"
if (-e ${Rootfile}_F.WMP) then
  'rm' -f ${Rootfile}_F.WMP
endif
dwe nmap.chmos >> $Logfile


#
#  Combine the mosaic fields
#
log "Combining mosaic maps in single field"
dwe nmap.moscom >> $Logfile

#
#  Check exit status
#
if (-e $Rootfile.WMP  && `genaid size $Rootfile.WMP` > 512) then
   'rm' -f ${Rootfile}_F.WMP
   set Status="ok"
else
   set Status="Could not produce map"
endif

phistory "make_map $CHANNEL : $Status"
