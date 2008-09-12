#
#  Local startup for Newstar (HjV - 950314)
#  Revision:
#	951212 HjV	Add n_www
#
#+
#       Institute:      California Institute for Technology
#       Address:        Caltech 105-24
#			PASADENA CA91125
#			USA
#       Contact person: Gautam Vasisht
#       Email address:  gv@astro.caltech.edu
#       FTP-node(s):    phobos.caltech.edu
#       Phone:          001-818-395-4987
#-
#
#  Define the name of this site
#
setenv n_site    calt
setenv n_install sw
setenv n_hosts   phobos

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
setenv n_exe $n_root/$n_arch/bin
#
#  Now do the general setup
#
source $n_root/src/sys/newstar_init.csh
#
#  Now we may wish to change anything we do not like
#
if ($HOSTTYPE =~ sun*) then
      setenv MAG0 "/dev/nrst0"		# Exabyte
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser

