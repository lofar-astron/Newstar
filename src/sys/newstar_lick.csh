#
#  Local startup for Newstar (HjV 960611)
#
#  Revision:
#
#+
#       Institute:      Lick Observatory, University of California at St. Cruz
#       Address:        1156 High Street
#			95064  California
#			U.S.A.
#       Contact person: Arpad Szomoru
#       Email address:  arpad@kanchhi.ucolick.org
#       FTP-node(s):    128.114.23.54	(sw - kanchhi.ucolick.org)
#       Phone:          
#-
#
#  Define the name of this site
#
setenv n_site    lick
setenv n_install sw
setenv n_hosts   kanchhi

#
#  Define the root of the Newstar directory tree
#
setenv n_root /i/arpad/newstar
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
setenv MAG8 "/dev/rst1"			# 8mm tape drive
setenv MAG9 "/dev/sr0"			# CD-ROM
#
# Define your www browser
#   If you don't define it, xmosaic (part of newstar distibution) will be used
#   If you change the next line, please report it to newstar@astron.nl
#      else your changes will be lost with the next update.
#setenv n_www xyz                       # where xyz is your favorite www browser

