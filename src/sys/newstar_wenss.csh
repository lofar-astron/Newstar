#
#  Local startup for Newstar for WENSS (HjV 931223)
#  Revision:
#	940419 CMV	Add n_doabp setting
#	941109 CMV	Add scissor commands
#	950130 HjV	Use $HOME in directories iso. ~ or ~/
#	950329 HjV	Executable and libraries now local (on /user8)
#	950529 HjV	Make WENSS a separate site
#	951012 HjV	Add DAT-device for DAW13
#	951212 HjV	Add n_www
#
#+
#       Institute:      Netherlands Foundation for Research in Astronomy
#       Address:        WENSS-project
#			P.O. Box 2
#                       7990 AA  Dwingeloo
#			Netherlands
#       Contact person: Yuan Tang
#       Email address:  tang@astron.nl
#       FTP-node(s):    192.87.1.158	(HP  - daw08)
#       Phone:          05219 - 7244  Ext 249
#-
#
#  Define the name of this site
#
setenv n_site    wenss
setenv n_install hp
setenv n_hosts   daw08
setenv n_doabp   ok

#
#  Define the root of the Newstar directory tree
#
# unset some variables if people started old system first
unsetenv n_root n_src n_exe n_lib n_hlp n_arch n_doc 
unsetenv n_inc n_tst n_batch n_master n_remote
# O.K. set root now
setenv n_root /user8/newstar
#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh
#
#  Any non-standard environment settings should be made here
#
if ($HOSTTYPE =~ sun*) setenv LD_LIBRARY_PATH "/usr/openwin/lib:/usr/lib"
#
#  Print newstar news (when available, and only once!)
#
if (! $?DWARF_SYMBOLS && -e $n_root/import/newstar.news) cat $n_root/import/newstar.news
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
#  Now we may wish to change anything we do not like
#
if ($HOSTTYPE =~ sun*) then
   setenv MAG8 "/dev/rst1"		# 1 Giga DAT DDS
   setenv MAG9 "/dev/rst0"		# Exabyte
else if ($HOSTTYPE =~ hp*) then
   if ($HOST == rzmws4 || $HOST == rzmws5 || $HOST == daw08) then
	setenv MAG8 "/dev/rmt/0m"
   else if ($HOST == rzmws6 || $HOST == rzmws7) then
	setenv MAG8 "/dev/rmt/c201d3m"
   else if ($HOST == daw13) then
	setenv MAG8 "/dev/rmt/1m"
	setenv MAG9 "/dev/rmt/3m"
   endif
endif

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
setenv n_www netscape


#
#  Print on the line printer (some people are used to this command)
#
alias pvax      "wngfex sp \!* "

#
#  Not everybody has elm in his path
#
if ($n_arch == sw) then
   if ($USER == jph) alias elm \mail
endif

#
# Some tricky things for GIDS
#
if ($?DISPLAY) then
  setenv DEFAULT_DISPLAY $HOME/.gids-$DISPLAY
  if ($DISPLAY =~ rzmws5*) then   # Cannot use gipsy gids
    setenv gids_setup ~ger
  endif
endif

