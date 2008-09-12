#!/bin/csh
#  Created from ncopy.ssc on Wed Sep 1 08:35:07 METDST 1993 at rzmws5
#
# ncopy.ssc
# wnb 910930
#
#  Revisions:
#	WNB 920913	Include NGCALC files; more options
#	WNB 921006	Make more general
#	WNB 921230	Make SSC
#	WNB 930901	Make sure aliases
#
# Copy and convert Newstar data from one machine to other
#
#	Uses environment: WNG_SITE
#
# Intro
#
	echo "Copy files from other machine (TCPIP) and convert them."
	echo "                    Give empty answers to exit gracefully."
	echo " "
	if (! $?ROOTDWARF) then
	  echo "No dwarf environment specified; change your .cshrc"
	  exit
	endif
	source $ROOTDWARF/dwarf_alias.sun	# get aliases
#
# Get data type
#
LA:
	echo -n "Type (model, scan, map, gcalc, mongo): "
	set type=$<
	set type=`echo $type | tr A-Z a-z`
	switch ($type)
	  case "":
		echo "Exit requested"
		echo ""
		exit
		breaksw
	  case mod*:
		set type=model
		breaksw
	  case s*:
		set type=scan
		breaksw
	  case ma*:
		set type=map
		breaksw
	  case mon*:
		set type=mongo
		breaksw
	  case g*:
		set type=gcalc
	  default:
		echo "Unknown type"
		echo " "
		goto LA
		breaksw
	endsw
#
# Get machine info
#
	echo -n "Input machine (eg RZMVX4): "
	set vmach=$<
	if ($vmach == "") goto LA
	set vmach=`echo $vmach | tr a-z A-Z`
#
# Get file info
#
	echo -n "$vmach user (eg GER): "
	set vus=$<
	if ($vus == "") goto LA
	echo -n "$vmach directory in $vmach format (eg USER1:[GER.DATA]): "
	set vdir="$<"
	if ("$vdir" == "") goto LA
LC:
	echo -n "$vmach filename (eg ngc1274_21cm.scn): "
	set vfil="$<"
	if ("$vfil" == "") goto LA
	set vfil=`echo $vfil | tr a-z A-Z`
LB:
	echo -n "Full directory or empty if current: "
	set adat="$<"
	if ("$adat" == "") set adat=$cwd
	if (!(-d $adat)) then
	  echo "Non-existant directory"
	  echo " "
	  goto LB
	endif
	set anod="$vfil"
	if ($anod == "") goto LA
	if (-e $adat/$anod) then
	  echo -n "Node exists. Delete it? (y,n) [n]: "
	  set ans=$<
	  switch ($ans)
	    case [yY]*:
		'rm' $adat/$anod
		echo "Node $adat/$anod deleted"
		breaksw
	    default:
		goto LC
		breaksw
	  endsw
	endif
#
# Transfer file
#
	echo " "
	echo "Transfering file. $vmach password may be asked for."
	echo " "
	if ($type == mongo) then
	  @ ans = { ftp -n << qqq }
	  open $vmach
	  user $vus
	  cd "$vdir"
	  get $vfil $adat/$anod
	  close
	  quit
qqq
	else
	  @ ans = { ftp -n << qqq }
	  open $vmach
	  user $vus
	  cd "$vdir"
	  binary
	  get $vfil $adat/$anod
	  close
	  quit
qqq
	endif
	if ((-e $adat/$anod)) then
	  echo "Transfer completed, starting conversion."
	  echo " "
	else
	  echo "Transfer not completed properly, abnormal termination."
	  echo " "
	  exit
	endif
#
# Conversion
#
	switch ($type)
	  case model:
		@ ans = { dwe nmodel/log=no >/dev/null <<qqq }
CVX
"@$adat/$anod"
QUIT
qqq
		breaksw
	  case map:
		@ ans = { dwe nmap/log=no >/dev/null <<qqq }
CVX
"@$adat/$anod"
QUIT
qqq
		breaksw
	  case scan:
		@ ans = { dwe nscan/log=no >/dev/null <<qqq }
CVX
"@$adat/$anod"
qqq
		breaksw
	  case gcalc:
		@ ans = { dwe ngcalc/log=no >/dev/null <<qqq }
CVX
"@$adat/$anod"
qqq
		breaksw
	  case mongo:
		breaksw
	  default:
		echo "Programming error, abnormally terminated."	
		echo " "
		breaksw
	endsw
#
# Ready
#
	if ($ans == 0) then
	  echo "Conversion of $adat/$anod finished."
	  echo " "
	else
	  echo "Errors in conversion of $adat/$anod."
	  echo " "
	endif
	exit
