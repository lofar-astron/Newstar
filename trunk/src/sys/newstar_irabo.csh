#
#  Local startup for Newstar IRABO (HjV 950825)
#  Revision:
#	951212 HjV	Add n_www
#
#+
#       Institute:      Instituto di Radioastronomica - C.N.R.
#       Address:        via Gobetti 101
#			I-40129 Bologna
#                       Italy
#       Contact person: Danielle Dallacasa
#       Email address:  dallacasa@astbo1.bo.cnr.it
#       FTP-node(s):    terra.bo.cnr.it    (Alpha - terra)
#       Phone:          
#-
#
#  Define the name of this site
#
setenv n_site    irabo
setenv n_install da
setenv n_hosts   terra

#
#  Define the root of the Newstar directory tree
#
setenv n_root /soft/newstar
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
setenv MAG8 /dev/nrmt0
