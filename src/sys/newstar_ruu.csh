#
#  Local startup for Newstar (HjV 931007)
#
#  Revision:
#	CMV 940419 	Add n_doabp setting
#	HjV 940518	Change test for alpha
#	SJH 960605	Adapt things for new server ruunf3
#
#+
#       Institute:      Sterrenkundig Instituut Rijks Universiteit Utrecht
#       Address:        Princetonplein 5
#			postbus 80000
#                       3508 TA  Utrecht
#			Netherlands
#       Contact person: Sake J. Hogeveen
#       Email address:  hogeveen@fys.ruu.nl
#       FTP-node(s):    131.211.32.203	(da - ruunf3)
#       Phone:          030 - 2535227  (or 030 - 2535200)
#-
#
#  Define the name of this site
#
setenv n_site    ruu
setenv n_install dw/da
setenv n_hosts   ruunb6,ruunf3
setenv n_doabp   ok

#
#  Define the root of the Newstar directory tree
#
setenv n_root /strknd/ULTRIX/lib/newstar
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh
#
#  Any non-standard environment settings should be made here
#
if ($HOSTTYPE == "alpha") then
#
# The file system at Natk & Sterrek Utrecht has been configured such
# that nfs mounted disks are accessed by their genuine name from
# every workstation in the cluster --- Sake J. Hogeveen, 29/08/94.
#
#   setenv n_root /nfs/ruunb6/b6-usr2/ULTRIX/lib/newstar
   setenv n_root /strknd/ULTRIX/lib/newstar
#   setenv n_exe  /nfs/ruunb6/b6-usr2/OSF1/lib/newstar
   setenv n_exe  /strknd/OSF1/lib/newstar
endif
#
#  Now do the general setup
#
source $n_root/src/sys/newstar_init.csh
#
#  Now we may wish to change anything we do not like
#
if ($HOST == ruune7) then
   setenv MAG8 "/dev/nrmt0h"
endif
if ($HOST == ruunf3) then
   setenv MAG7 "/dev/nrmt2h"    # DAT 2
   setenv MAG8 "/dev/nrmt1h"	# DAT 1
   setenv MAG9 "/dev/nrmt0h"	# Exabyte
endif
#

