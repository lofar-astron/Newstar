#
#  Local startup for Newstar (HjV 931007)
#
#+
#       Institute:      Westerbork Synthese Radio Telescope
#       Address:        Schattenberg 1
#                       9433 TA  Zwiggelte
#			Netherlands
#       Contact person: Teun Grit
#       Email address:  grit@astron.nl
#       FTP-node(s):    192.87.1.225	(HP  - wsrt00)
#       Phone:          05933 - 2421
#
#     Revision:
#	940711 CMV	Initialise wsrt environment as well
#	941109 CMV	Add scissor commands
#	951212 HjV	Add n_www
#	960329 JPH	n_www = netscape
#-
#
#  Define the name of this site
#
setenv n_site    wsrt
setenv n_install hp
setenv n_hosts   waw01

#
#  Define the root of the Newstar directory tree
#
setenv n_root /users/srt/nst
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

alias scissor $n_src/sys/scissor.csh
alias sctar   scissor tar
alias scship  scissor ship
alias scinit  scissor init
alias scmail  scissor mail
setenv QED1 //www.astron.nl:8083          # The scissor server

#
#  Now do the general setup
#
source $n_src/sys/init_wsrt.csh

#
#  Now we may wish to change anything we do not like
#
alias nsmail "pine -I ^X,y -subject "
if ($HOST =~ wsrt*) then
   setenv MAG8 "/dev/rmt/0m"
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
setenv n_www netscape 

