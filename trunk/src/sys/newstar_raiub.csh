#
#  Local startup for Newstar (HjV 931007)
#	951212 HjV		Add n_www
#	960802 HjV		Add Alpha/OSF1
#	971029 Helge Rottmann	Add Solaris stuff
#
#+
#       Institute:      Radioastronomisches Institut Universitaet Bonn
#       Address:        Auf dem Huegel 71
#                       D-53121  Bonn
#			Germany
#
#	Sun-Workstation:
#	   Contact person: Helge Rottmann
#	   Email address:  rottmann@astro.uni-bonn.de
#	   FTP-node(s):    131.220.96.29	(aux29) 
#          FTP-node(s):    131.220.96.26        (sun150)
#	   Phone:          09-49-228733393
#
#	Dec-Alpha/OSF1:
#          Contact person: Peter Kalberla
#	   Email address:  pkalberla@astro.uni-bonn.de
#          FTP-node(s):    131.220.96.17	(soft1)
#          Phone:          09-49-228733645
#-
#
#  Define the name of this site
#
setenv n_site    raiub
  switch  ("`uname -a`")
  case OSF1*alpha:
     set MACHINE='alpha'
     breaksw
  case "SunOS*sun3*":
     set MACHINE='sun3'
     breaksw
  case "SunOS*4.1*sun4*":
     set MACHINE='sun4c'
     breaksw
  case "SunOS*5.*sun4*":
     set MACHINE='ssol2'
     breaksw
  case "ULTRIX*RISC":
     set MACHINE='risc'
     breaksw
  default:
     set MACHINE='unknown'
     breaksw
  endsw
if ($MACHINE == "sun4c" ) then
   setenv n_install sw
   setenv n_hosts   sun29
   setenv n_arch    sw
else if ($MACHINE == "ssol2" ) then
   setenv n_install so
   setenv n_hosts   sun150
   setenv n_arch    so
else if ($MACHINE == "alpha") then
   setenv n_install da 
   setenv n_hosts   aibn17
   setenv n_arch    da
endif
#
#  Define the root of the Newstar directory tree
#
if ("$n_arch" == "sw" ) then
   setenv n_root /aux29/dwingeloo/newstar
else if ("$n_arch" == "so" ) then
   setenv n_root /net/sun29/aux29/dwingeloo/newstar
else if ("$n_arch" == "da" ) then
   setenv n_root /soft1/newstar
endif
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh
#
#  Any non-standard environment settings should be made here
#
#
#  Now do the general setup
#
source $n_root/src/sys/newstar_init.csh
#
#  Now we may wish to change anything we do not like
#
if ($HOST == "sun29") then
   setenv MAG8 "/dev/nrst0"
else if ($HOST == "sun150") then
   setenv MAG8 "/dev/nrst/0"
else if ("$HOST" == "aibn20") then
   setenv MAG8 "/dev/nrmt0h"
else if ("$HOST" == "aibn23") then
   setenv MAG9 "/dev/nrmt0h"
endif
#
# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
if ("$n_arch" == "da" ) then
   setenv n_www netscape		# where xyz is your favorite www browser
endif
