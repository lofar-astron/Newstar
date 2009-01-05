#
#  Local startup for Newstar (HjV 931007)
#
#+
#       Institute:      Westerbork Synthese Radio TelescopeWNB Laptop
#       Address:        Schattenberg 1
#                       9433 TA  Zwiggelte
#			Netherlands
#       Contact person: Wim Brouw
#       Email address:  brouw@astron.nl
#       FTP-node(s):    
#       Phone:          050 3634067
#
#     Revision:
#	070831 WNB	Initialise 
#-
#
#  Define the name of this site
#
setenv n_site    wnbl
setenv n_install li
setenv n_hosts   debian

#
#  Define the root of the Newstar directory tree
#
setenv n_root /nstar
setenv n_hlp  $n_root/hlp

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
#  Now do the general setup
#
##source $n_src/sys/init_wsrt.csh

#
#  Now we may wish to change anything we do not like
#
##alias nsmail "pine -I ^X,y -subject "
setenv LPATH /lib:/usr/lib

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
setenv n_www firefox 

