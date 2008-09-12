#
#  Local startup for Newstar (HjV 940614)
#  Revision:
#	951212 HjV	Add n_www
#
#+
#       Institute:      Beijing Astronomical Observatory
#       Address:        Beijing
#                       100080
#			China
#       Contact person: Peng Bo
#       Email address:  zhangxz@bepc2.ihep.ac.cn
#       FTP-node(s):    bao01.bao.ac.cn		(VAX - )
#			ibepc2.ihep.ac.cn	(VAX - )
#						(SW  - sun8)
#       Phone:          86-1 256 1265
#-
# Should login via VAX and do: SET HOST SUN8
# Yet (940623) it still does NOT work
#
#  Define the name of this site
#
setenv n_site    bao
setenv n_install sw
setenv n_hosts   sun8

#
#  Define the root of the Newstar directory tree
#
setenv n_root /home/newstar
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
   setenv MAG9 "/dev/rst0"		# 
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
