#
#  Local startup for Newstar (CMV 930525)
#  Revision:
#	931007 HjV	Add MAG*-settings
#	931201 CMV	Split off newstar_env.csh
#	940209 CMV	Add tapeunit on Huygens
#	940613 HjV	Add tapeunit on Magellan
#	950130 HjV	Use $HOME in directories iso. ~ or ~/
#	951212 HjV	Add n_www
#	961212 HjV	Add Solaris
#	981119 HjV	Update for brahe (HPUX-10.20) and jeans (Solaris 2.6)
#	000308 AxC	Update for /Software/users/newstar
#
#+
#       Institute:      Kapteyn Instituut Rijks Universiteit Groningen
#       Address:        Kapteyn Instituut
#			Postbus 800
#			9700 AV  Groningen
#			Netherlands
#       Contact person: Wim Zwitser
#       Email address:  zwitser@astro.rug.nl
#       FTP-node(s):    129.125.6.131	(HP  - brahe)
#			129.125.6.228	(SO  - jeans)
#       Phone:          050 - 3634071
#-
#
#  Define the name of this site
#
setenv n_site    rug
setenv n_install hp/so
setenv n_hosts   brahe,jeans
setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/usr/ucblib"

#
#  Define the root of the Newstar directory tree
#
setenv n_root /Software/users/newstar
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

#Cannot handle /Software (capital in path gets lowercased).
setenv MODELB ~newstar/data/

setenv MAG0 "/dev/rmt/0l"
setenv MAG1 "/dev/rmt/0m"

alias scissor $n_src/sys/scissor.csh
setenv QED1 //www.astron.nl:8083          # The scissor server


# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
setenv n_www netscape			# where xyz is your favorite www browser
