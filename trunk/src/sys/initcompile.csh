#!/bin/csh
#set echo
#+
#  initcompile.csh
#	CMV 930528	Created
#	CMV 931105	Added Myname
#	CMV 931223	Remove _Objectlist _Textlist
#	CMV 940218	Get version number
#	CMV 950116	Search for i_${n_site}.csh as well
#	WNB 950224	Added elm alias	
#	HjV 950828	Better test for elm alias
#
#  This script is invoked both by shadow.csh and update.csh to 
#  ensure the environment is set up and to make common initialisations.
#
#  $Files should contain the list of files (with intersparsed switches).
#
#-

#
# See if environment is defined
#
if (! $?n_root   || ! $?n_src    || ! $?n_inc  || ! $?n_lib || ! $?n_exe || \
    ! $?n_import || ! $?n_arch   || ! $?n_site || ! $?n_hlp ) then
   echo " "
   echo "Getting upset: environment not setup"
   echo "First initialise Newstar and then try again"
   echo " "
   exit
endif

if (! -d $n_src) then
   echo " "
   echo "You do not have a proper Master source tree...."
   echo "Your Master source tree should root at  $n_src"
   echo "This compiler script seems to be  $0"
   echo "Please verify your setup file (newstar_$n_site.csh)"
   exit
endif

#
# Make sure elm known
#
set a=(`which elm`)
if ($#a != 1) then
   alias elm \/usr/ucb/mail
else
   if (! -e $a[1]) alias elm \/usr/ucb/mail
endif
unset a
#
# Make sure pine known at nfra and wsrt
#
if (("$n_site" == nfra ) || ("$n_site" == wsrt )) then
    alias nsmail "pine -I ^X,y -subject "
endif

#
# Read the local compiler settings from various files
#
if (-e $n_src/sys/i_$n_arch.csh)         source $n_src/sys/i_$n_arch.csh
if (-e $n_src/sys/i_$n_site.csh)         source $n_src/sys/i_$n_site.csh
if (-e $n_src/sys/i_$n_arch$n_site.csh)  source $n_src/sys/i_$n_arch$n_site.csh
if ($?n_usrc) then
  if ($n_usrc != $n_src) then
    if (-e $n_usrc/sys/i_$n_arch.csh)        source $n_usrc/sys/i_$n_arch.csh
    if (-e $n_usrc/sys/i_$n_site.csh)        source $n_usrc/sys/i_$n_site.csh
    if (-e $n_usrc/sys/i_$n_arch$n_site.csh) source $n_usrc/sys/i_$n_arch$n_site.csh
  endif
endif
if (-e i_$n_arch.csh) source i_$n_arch.csh

#
# Construct the date/time strings, get current version, define logfile
#
set Myname=`awk -F: '{ if ($1 == "'$USER'") print $5 }' /etc/passwd`
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

set C_Version=$C_Date
if (-e $n_src/sys/version.idx) then
   set tmp=(`head -1 $n_src/sys/version.idx`)
   if ($#tmp > 2) then
      if ("$tmp[3]" != "") set C_Version=$tmp[3]
   endif
endif 

set Logfile=$n_src/upd${C_Date}${n_arch}.log
@ ii = 0
while (-e $Logfile)
  @ ii = $ii + 1
  set Logfile=$n_src/upd${C_Date}${n_arch}$ii.log
end
unset ii 

if ("`alias log`" == "") alias log echo

#
# Default some things to the current directory
#
if (! $?n_uexe) set n_uexe=$cwd
if (! $?n_ulib) set n_ulib=$cwd
if (! $?n_work) set n_work=$cwd
if (! $?n_usrc) set n_usrc=__undefined__

#
# For proper linking, it is quite essential that all files in $n_inc
# have been properly linked into $n_uinc. If there are no files there,
# just move to the master system $n_inc.
#
if (! $?n_uinc) set n_uinc=$n_inc
if ($n_uinc != $n_inc) then
   if (! -d $n_uinc || ! -e $n_uinc/WNG_DEF) then
      log "%%%%%% Warning; no WNG_DEF in "\$n_uinc
   endif
endif

#
# Set temporary file, tell what we are doing
#
set Name=$0; 
set Name=$Name:t
set Tmpfile=$n_work/${Name:r}$$.tmp
log "Running $Name for $n_site ($n_arch) on $HOST at $C_Date/$C_Time"
unset Name
