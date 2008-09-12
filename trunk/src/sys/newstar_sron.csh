#
#  Local startup for Newstar
#
#  Revision:
#	CMV 950105 	Created
#	HjV 951212	Add n_www
#
#+
#       Institute:      SRON Utrecht
#       Address:        Sorbonnelaan 2
#			Postbus 80000
#                       3584 CA  Utrecht
#			Netherlands
#       Contact person: Chiel Galama
#       Email address:  m.galama@sron.ruu.nl
#       FTP-node(s):    saturn.sron.ruu.nl (hp)
#       Phone:          030 - 535644
#-
#
#  Define the name of this site
#
setenv n_site    sron
setenv n_install hp
setenv n_hosts   saturn
#setenv n_doabp   ok

#
#  Define the root of the Newstar directory tree
#
if (-e /users/newstar/src) then
  setenv n_root /users/newstar
else
  setenv n_root /nfs/saturn/users/newstar
endif
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
setenv HOSTTYPE hp9000s700
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
setenv MAG8 "/dev/rmt/c201d3m"
#

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
