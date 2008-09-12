#
#  Local startup for Newstar (HjV 931007))
#	951212 HjV	Add n_www
#
#+
#       Institute:      ARECIBO Obersatory
#       Address:        P.O. Box 995, Arecibo
#                       Puerto Rico 00613 USA
#       Contact person: Tapasi Ghosh
#       Email address:  tghosh@naic.edu
#       FTP-node(s):    192.65.176.64	(SW  - nevis)
#			192.65.176.4	(SW  - aosun)
#       Phone:          (1)-809-878-2612
#-
#
#  Define the name of this site, installed architectures and hosts
#
setenv n_site    arecb
setenv n_install sw
setenv n_hosts   nevis

#
#  Define the root of the Newstar directory tree
#
setenv n_root /usr/local/newstar
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
   setenv MAG8 "/dev/nrst1"
   setenv MAG9 "/dev/nrst0"
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
