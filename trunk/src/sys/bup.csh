#! /bin/csh -f
#+ bup.csh
#
#   HjV 931228  Created
#
# This is a script to update the Newstar programs in the background
#
#
# Uncomment the following line for testing purposes...
#set echo 
	echo " Use one of the following options:"
	set Oms=("nup b -u all" \
		 "nup b -u all -t:dsc"  \
		 "nup b -u all -t:^dsc/^exe/^pef/^psc/^pin"  \
		 "nup b -u all -t:exe/pef/psc/pin"  \
		 "Enter command yourself" )
	@ ii = 1
	while ($ii <= $#Oms)
	   echo $ii"." $Oms[$ii]
	   @ ii = $ii + 1
	end
	@ Opt = $#Oms
	echo -n "Which option (1-[$#Oms]): "
	set ans=($<); if ("$ans" != "") set Opt="$ans"

	if ($Opt != $#Oms) then
	   set Com = "$Oms[$Opt]"
	else
	   set Com="nup b -u all"
	   echo -n "Enter update command [$Com]: "
	   set ans=($<); if ("$ans" != "") set Com="$ans"
	endif
	( ( $Com ) | & nsmail "$n_site-$n_arch done: $Com" coolen@astron.nl )&
	echo "Started command: $Com"     
