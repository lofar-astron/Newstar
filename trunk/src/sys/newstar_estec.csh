#
#  Local startup for Newstar (HjV 941125)
#	951212 HjV	Add n_www
#
#+
#       Institute:      ESTEC
#       Address:        Keplerlaan 1
#                       2200 AG  NOORDWIJK
#			The Netherlands
#       Contact person: Lorraine Hanlon
#       Email address:  lhanlon@astro.estec.esa.nl
#       FTP-node(s):    astro.estec.esa.nl	(SW
#       Phone:          01719 - 83833
#-
#
#  Define the name of this site
#
setenv n_site    estec
setenv n_install sw
setenv n_hosts   arthur

#
#  Define the root of the Newstar directory tree
#
setenv n_root /usr7/newstar
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
setenv MAG8 "/dev/rst0"
setenv MAG9 "/dev/rst1"

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
