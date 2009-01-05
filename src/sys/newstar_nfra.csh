#
#  Local startup for Newstar (CMV 930922)
#  Revision:
#	931007 HjV	Add MAG*-settings
#	931020 CMV	Add LD_LIBRARY_PATH (not everybody has this in .cshrc)
#	931110 HjV	Change device name MAG8 and type newstar.news
#	931201 CMV	Split off newstar_env.csh
#	940304 CMV	Add n_ftp to invoke copying to the ftp area
#	940315 HjV	Add nbug (was in newstar_init.csh before)
#	940419 CMV	Add n_doabp setting
#	940620 CMV	Separate filesystems for rzmws5,rzmws6,daw03
#	940628 CMV	Change directory for daw03, add rzmws7
#	940708 HjV	Add tape-unit MAG0 (1600 bpi) and 
#			MAG1 (6250 bpi) for rzmws0
#	940720 CMV 	Changed location of n_hlp
#	941019 HjV 	Changed location of n_hlp
#	941027 CMV	Added MAG3 for optical disk on daw03
#	941102 CMV	Added scissor commands
#	941109 CMV	Add scmail command
#	950130 HjV	Use $HOME in directories iso. ~ or ~/
#	950424 HjV	Add rzmws4
#	950614 HjV	Add n_l2h
#	950627 HjV      Add dat-device on DAW16
#       950830 ErDeul   Modified LD_LIBRARY_PATH to add to existing
#	951012 HjV	Add DAT-device for DAW13
#	951212 HjV	Add n_www
#	960618 HjV	Add Solaris (so, duw01); rzmws6 removed, add daw16
#	970829 HjV	Remove ws4, ws5, ws7 stuff
#			Add daw18 (only for compile/linking)
#
#+
#       Institute:      Netherlands Foundation for Research in Astronomy
#       Address:        P.O. Box 2
#                       7990 AA  Dwingeloo
#			Netherlands
#       Contact person: Arthur Coolen
#       Email address:  coolen@astron.nl
#       FTP-node(s):    192.87.1.150	(Solaris 2.6 - dus0)
#       Phone:          0521 - 595237
#-
#
#  Define the name of this site
#
setenv n_site    nfra
setenv n_install li
setenv n_hosts   dop64
setenv n_ftp     ftp.astron.nl
setenv n_doabp   ok
setenv _Merge    0

#
#  Define the root of the Newstar directory tree
#
unsetenv n_src n_exe   # If people started old system first
setenv n_root /dop64_0/newstar
setenv n_hlp  $n_root/hlp

#
#  Make sure we have the standard settings (HOSTTYPE etc)
#
source $n_root/src/sys/newstar_env.csh

#
#  Any non-standard environment settings should be made here
#
# Note that the /usr/lib path entry in the LD_LIBRARY_PATH is not required
# and will explicitly cuase problems with the SunOS 4.1.3 version and X11R6
# binaries.
#   COMMENT by E.R. Deul 07-09-95
#
if ($HOSTTYPE =~ sol*) then
   setenv n_l2h $n_root/latex2html
   if ($?LD_LIBRARY_PATH) then
      setenv LD_LIBRARY_PATH "$LD_LIBRARY_PATH":"/usr/openwin/lib"
   else
      setenv LD_LIBRARY_PATH "/usr/openwin/lib"
   endif
endif
if ($HOST == daw16) then
   if (-e /daw16_1/newstar/execute.exe)  setenv n_exe /daw16_1/newstar
else if ($HOST == daw03) then
   if (-e /usr/local/bin/newstar/execute.exe) \
                                       setenv n_exe /usr/local/bin/newstar
endif

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
alias nsmail "pine -I ^X,y -subject "
if ($HOST == rzmws0) then
   setenv MAG0 "/dev/rst12"		# 1600 bpi tape-unit
   setenv MAG1 "/dev/rst28"		# 6250 bpi tape-unit
endif
if ($HOSTTYPE =~ sun*) then
   setenv MAG8 "/dev/rst1"		# 1 Giga DAT DDS
   setenv MAG9 "/dev/rst0"		# Exabyte
else if ($HOST == daw08  || $HOST == daw16  || \
	 $HOST == daw03  || $HOST == duw01 ) then
   setenv MAG8 "/dev/rmt/0m"
else if ($HOST == daw13) then
   setenv MAG8 "/dev/rmt/1m"
   setenv MAG9 "/dev/rmt/3m"
else if ($HOST == duw00) then
   setenv MAG8 "/dev/rmt/1"
endif

setenv MAG7 disk:/cdrom/cdrom0

# Define your www browser
# If you don't define it, xmosaic (part of newstar distibution) will be used
setenv n_www netscape			# where xyz is your favorite www browser
#
#  Unit on daw03 and remote tape-units on the VAX (Optical Disks)
#
if ($HOST == daw03) then
  setenv MAG7 disk:/cdrom
  setenv MAG3 disk:/opt
  alias  odinit scinit 3
endif

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
endif








