#
#  Local startup for Newstar (CMV 930922)
#  Revision:
#	940413 CMV	Created for UvA
#	951212 HjV	Add n_www
#+
#       Institute:      Sterrenkundig Instituut "Anton Pannekoek"
#       Address:        Kruislaan 403
#                       1098 SJ  Amsterdam
#			Netherlands
#       Contact person: Michiel Berger
#       Email address:  michielb@astro.uva.nl
#       FTP-node(s):    helios.astro.uva.nl (HP)
#       Phone:          020 - 5257482
#-
#
#  Define the name of this site
#
setenv n_site    uva
setenv n_install hp
setenv n_hosts   helios

#
#  Define the root of the Newstar directory tree
#
setenv n_root /sirius/u/michielb/newstar
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
if ($HOST == helios) then
   setenv MAG8 "/dev/rmt/c201d1m"
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser

#
#  Remote tape-units on the VAX (Optical Disks)
#
#setenv MAG4 //rzmvx4.astron.nl:1100/RZMVX4\$MUA0:
#setenv MAG5 //rzmvx4.astron.nl:1101/RZMVX4\$MUA1:
