#
#  Local startup for Newstar UCSB (HjV 931216)
#  Revision:
#	931216 HjV	Created
#	951212 HjV	Add n_www
#	960129 HjV	Add LD_LIBRARY_PATH
#
#+
#       Institute:      University of California at Santa Barbara
#       Address:        Physics Department
#			University of California
#                       Santa Barbara, CA  93106-9530
#			USA
#       Contact person: Robert Geller
#       Email address:  rhmg@chester.physics.ucsb.edu
#       FTP-node(s):    128.111.8.130  (SW - chester.physics.ucsb.edu)
#       Phone:          (805)893-8875
#-
#
#  Define the name of this site
#
setenv n_site    ucsb
setenv n_install sw
setenv n_hosts   chester

#
#  Define the root of the Newstar directory tree
#
setenv n_root /home/chester4/rhmg/newstar
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh
#
#  Any non-standard environment settings should be made here
#
if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH "$LD_LIBRARY_PATH":"/usr/lang/SC0.0"
   else
    setenv LD_LIBRARY_PATH "/usr/lang/SC0.0"
endif
#
#  Now do the general setup
#
source $n_root/src/sys/newstar_init.csh
#
#  Now we may wish to change anything we do not like
#
if ($HOSTTYPE =~ sun*) then
   if ($?TAPE) then
      setenv MAG8 $TAPE
   else
      setenv MAG8 /dev/rst0
   endif
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
