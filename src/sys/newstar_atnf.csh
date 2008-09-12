#
#  Local startup for Newstar (HjV 931007)
#  Revision:
#	CMV 931201	Split off newstar_env.csh
#	HjV 940314	Change n_root
#	HjV 940516	Typo
#	WNB 940621	Some name changes
#	WNB 940624	Error in n_exe definition
#	WNB 950808	Change n_root for auto mount across network;
#			add raptor, remove ateles (CV)
#	WNB/HjV 951212	Add n_www
#	WNB 960627	Make so
#
#+
#       Institute:      Australian Telescope National Facility
#       Address:        P.O. Box 76
#                       Epping NSW2121
#			Australia
#       Contact person: Wim Brouw
#       Email address:  wbrouw@atnf.csiro.au
#       FTP-node(s):  	norma.atnf.csiro.au     (SO - norma)
#			robin.atnf.csiro.au     (VX - robin)
#			raptor.atnf.csiro.au	(SG - raptor)
#	Phone:		+(61)2.93724316
#-
#
#  Define the name of this site
#
setenv n_site    atnf
setenv n_install so
##/cv/vx
setenv n_hosts   norma
##,robin,raptor

#
#  Define the root of the Newstar directory tree
#
setenv n_root /nfs/code_norma/nstar
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh
#
#  Any non-standard environment settings should be made here
#
if ($HOSTTYPE =~ dec* || $HOSTTYPE =~ *mips*) then
   setenv n_exe /nfs/data_norma/nstar/sdw/dwarf
else
   setenv n_exe /nfs/data_norma/nstar/sso/dwarf
endif
#
#  Now do the general setup
#
source $n_root/src/sys/newstar_init.csh
#
#  Now we may wish to change anything we do not like
#
setenv n_import $n_root/test

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser

#
#  Mag tapes
#
if ($HOST == ateles) then
   setenv MAG0 "/dev/rmt8"
   setenv MAG1 "/dev/rmt16"
   setenv MAG2 "/dev/rmt0"
   setenv MAG3 "/dev/rmt9"
   setenv MAG4 "/dev/rmt17"
   setenv MAG5 "/dev/rmt1"
else if ($HOST == norma) then
else if ($?MACHINE_ARC) then
   if ("$MACHINE_ARC" == "dec") then
   else
      if ($HOST == carina) then
         setenv MAG9 "/dev/nrst0"
      endif
   endif
else
   if ($HOST == carina) then
      setenv MAG9 "/dev/nrst0"
   endif
endif
#
