#
#  Local startup for Newstar Sterrenwacht Leiden (CMV 931214)
#  Revision:
#	931214 CMV	Created
#	940328 HjV	Test for n_root
#	950130 HjV	New contact person
#	950623 HjV	Change for new situation (add SUN)
#	951212 HjV	Add n_www
#       040304 AxC      1st linux 4 leiden try
#
#+
#       Institute:      Sterrenwacht Leiden
#       Address:        Huygens Lab
#			P.O. Box 9513
#                       2300 RA  Leiden
#			Netherlands
#       Contact person: Jeroen Stil
#       Email address:  stil@Strw.LeidenUniv.nl
#       FTP-node(s):    vecht.strw.LeidenUniv.nl    (SW - vecht)
#			strw.strw.LeidenUniv.nl     (SW - strw)
#			dollard.strw.LeidenUniv.nl  (HP - dollard)
#			schelde.strw.LeidenUniv.nl  (HP - schelde)
#			drech.strw.LeidenUniv.nl    (HP - drecht)
#       Phone:          071 - 275883
#-
#
#  Define the name of this site
#
setenv n_site    rul
setenv n_install li
setenv n_hosts   dolder

#
#  Define the root of the Newstar directory tree
#
setenv n_root /software/newstar
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
##if ($HOSTTYPE =~ hp*) then
##   if ($?TAPE) then
##      setenv MAG8 $TAPE
##   else
##      setenv MAG8 /dev/rmt/1m
##   endif
##endif

if ($HOST == vecht) then

else if ($HOST == schelde) then
    setenv MAG8 /dev/rmt/0mn
else if ($HOST == strw) then
    setenv MAG8 /dev/nrst0
else if ($HOST == drecht) then
    setenv MAG8 /dev/rmt/0mn
else if ($HOST == dollard) then
    setenv MAG9 /dev/rmt/2m
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
#setenv n_www xyz			# where xyz is your favorite www browser
