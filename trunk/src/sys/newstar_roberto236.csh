#
#  Local startup for Newstar (HjV 931007)
#
#+
#       Institute:      Westerbork Synthese Radio TelescopeWNB Laptop
#       Address:        Schattenberg 1
#                       9433 TA  Zwiggelte
#			Netherlands
#       Contact person: Arthur Coolen
#       Email address:  coolen@astron.nl
#       FTP-node(s):    
#       Phone:          0521-595292
#
#     Revision:
#	091221 AxC	Initialise 
#       100103 WNB      Make newstar_ger208.csh
#       100104 WNB      Correct for absence 'standard' login
#       110307 AxC      Make newstar_roberto236.csh
#-
#
#  Define the name of this site
#
setenv n_site    roberto236
setenv n_install li
setenv n_hosts   dop236
setenv n_ftp     ftp.astron.nl

#
#  Define the root of the Newstar directory tree
#
# unset some variables if people started old system first
unsetenv n_root n_src n_exe n_lib n_hlp n_arch n_doc
unsetenv n_inc n_tst n_batch n_master n_remote
# set the standard NFRA HOSTTYPE
setenv HOSTTYPE linux
# O.K. set root now
setenv n_root /dop236_0/newstar
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

