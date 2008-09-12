#!/bin/csh -f
#CMV 940225
#+
#  Batch file for on-site analysis of WSRT observations
#
#  Used by the other bachfile only
#
#  Revisions:
#	CMV 940225	Created
#       CMV 940329	Changed phistory, always include it
#	CMV 940617	Correct setenv ROOTFILE if asked

#
# Construct the date/time strings, get current version, define logfile
#
set Myname=`awk -F: '{ if ($1 == "'$USER'") print $5 }' /etc/passwd | awk -F, '{ print $1}' `
if ("$Myname" == "") set Myname=`whoami`

set dt = (`date`)
if ("$dt[3]" =~ [1-9]) set dt[3] = "0$dt[3]"                # day
set mc=( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)
foreach mm ( 01 02 03 04 05 06 07 08 09 10 11 12)
  if ("$dt[2]" == "$mc[$mm]") break                         # month
end
@ yy = $dt[$#dt] - 1900                                     # year
set mh=( `echo $dt[4] | tr -s ":" " "` )                    # hh mm ss
set C_Date="$yy$mm$dt[3]"                                   # date: yymmdd
set C_Time="$mh[1]$mh[2]"                                   # time: hhmm
unset dt mc mm yy mh

#
# Check wether we need to do more
#
if ($?batch_init) exit

#
# Check wether we can proceed
#
if (! $?n_root   || ! $?n_src    || ! $?n_site || ! $?n_hlp ) then
   echo " "
   echo "Getting upset: environment not setup"
   echo "First initialise Newstar and then try again"
   echo " "
   exit
endif

#
# Initialise Newstar aliases for this site
#
source "$n_src/sys/newstar_$n_site.csh"

#
# Get current version, define projectname
#
if (! $?C_Version) set C_Version=$C_Date
if (! $?ROOTFILE) then
   echo -n "Enter the name for this project: "
   set tmp=($<)
   setenv ROOTFILE $tmp
   unset tmp
endif
set Rootfile=(`echo $ROOTFILE | tr '[a-z]' '[A-Z]' `)

#
# Define logfile, tmpfile, aliases
#
set Logfile=$cwd/${ROOTFILE}_${C_Date}1.log
@ ii = 1
while (-e $Logfile)
  @ ii = $ii + 1
  set Logfile=$cwd/${ROOTFILE}_${C_Date}$ii.log
end
unset ii 

alias log      'echo \!* | tee -a $Logfile'
alias phistory 'echo $C_Date $C_Time - $Myname - \!* | tee -a $ROOTFILE.history'

#
# Set temporary file, tell what we are doing
#
set Name=$0; 
set Name=$Name:t
set Tmpfile=$cwd/${Name:r}$$.tmp
log "Running $Name for $n_site ($n_arch) on $HOST at $C_Date/$C_Time"
unset Name

#
# Get the dwarf settings for batch files
#
dwrestore $n_batch/profile.par /overwrite/nolog
if (-e profile.par) then
   dwrestore profile.par /overwrite/nolog
endif

#
# Flag that we initialised
#
set batch_init

#
# Set the exit status
#
set Status="ok"
