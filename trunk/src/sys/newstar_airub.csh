#
#  Local startup for Newstar (HjV 951214)
#  Revision:
#
#+
#       Institute:      University of Bochum, Astronomical Institute
#       Address:        Universitaetsstrase 150
#			NA 7 Nord
#			44780 Bochum
#			Germany
#       Contact person: Goetz Golla
#       Email address:  golla@astro.ruhr-uni-bochum.de
#       FTP-node(s):    alpha3.astro.ruhr-uni-bochum.de	(DA  - alpha3)
#       Phone:          +49 234 700 2335
#-
#
#  Define the name of this site
#
setenv n_site    airub
setenv n_install da
setenv n_hosts   alpha4

#
#  Define the root of the Newstar directory tree
#
setenv n_root /opt/newstar
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
if ($HOST == alpha1 || $HOST == alpha3) then
    setenv MAG6 "/dev/EXABYTE"
    setenv MAG7 "/dev/nEXABYTE"
    setenv MAG8 "/dev/DAT"
    setenv MAG9 "/dev/nDAT"
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
