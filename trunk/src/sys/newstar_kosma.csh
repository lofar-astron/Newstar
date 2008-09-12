#
#  Local startup for Newstar (HjV 931007)
#	951212 HjV	Add n_www
#
#+
#       Institute:      I. Physikalisches Institut
#                       Koelner Observatorium fuer SubMillimeter-Astronomie
#       Address:        Zelpicher Strasse 77
#                       50937 Koeln
#                       Germany
#       Contact person: Uwe Corneliussen
#       Email address:  corneli@ph1.uni-koeln.de
#       FTP-node(s):    134.95.50.8        (HP  - apollo)
#       Phone:          +49 221 470 3558
#-
#
#  Define the name of this site
#
setenv n_site    kosma
setenv n_install hp
setenv n_hosts   apollo

#
#  Define the root of the Newstar directory tree
#
setenv n_root /utildsk/newstar
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
if ($HOST =~ apollo*) then
   setenv MAG8 "/dev/rmt/0m"
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
