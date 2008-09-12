#
#  Local startup for Newstar UCB (HjV 931216)
#  Revision:
#	940914 HjV	Created
#	951212 HjV	Add n_www
#
#+
#       Institute:      University of California at Berkeley
#       Address:        Astronomy / Radio Astronomy
#                       601 Campbell Hall
#			University of California
#			Berkeley, CA94720
#			USA
#       Contact person: Dan Plonsey
#       Email address:  dplonsey@astro.berkeley.edu
#       FTP-node(s):    floris.berkeley.edu
#       Phone:          (510)642-3163
#-
#
#  Define the name of this site
#
setenv n_site    ucb
setenv n_install sw
setenv n_hosts   floris

#
#  Define the root of the Newstar directory tree
#
setenv n_root /hond/newstar
setenv OPENWINHOME /usr/openwin
setenv LD_LIBRARY_PATH /usr/ucblib:/opt/SUNWspro/lib:$OPENWINHOME/lib:/usr/local/lib
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
if ($HOSTTYPE =~ sun*) then
    setenv MAG0 /dev/rmt/0		# exabyte
    setenv MAG1 /dev/rmt/1		# exabyte
    setenv MAG2 /dev/rmt/2		# 9 track
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
