#!/bin/csh -f
#
#	940314 HjV	When HOST contains dots, take first part.
#	940322 HjV	Make contents HOST, HOSTTYPE lowercase
#	960619 HjV	Add test for Solaris
#+
#  newstar_env.csh - make sure general settings have been made
#
#  This script sets HOSTTYPE, HOST and USER if they are not yet defined
#-
#
#  Define the name of the current host
#
if (! $?HOST) then
  if (-x /usr/local/bin/hostname) then
     setenv HOST `/usr/local/bin/hostname`
  else if (-x /usr/bin/hostname) then
     setenv HOST `/usr/bin/hostname`
  else if (-x /bin/hostname) then
     setenv HOST `/bin/hostname`
  else if (-x /usr/local/hostname) then
     setenv HOST `/usr/local/hostname`
  else if (-x /usr/ucb/hostname) then
     setenv HOST `/usr/ucb/hostname`
  else
     setenv HOST unknown
  endif
endif
#  remove part after dot
setenv HOST `echo $HOST | awk '{split($1,name,"."); {print name[1]} }' `
#  set to lowercase
setenv HOST `echo $HOST | tr '[A-Z]' '[a-z]' `
#
#  Define the hosttype (all these crazy paths taken from cshrc.csh@rug)
#
if (! $?HOSTTYPE ) then
  if ($?arch) then                              # Defined at the atnf
     setenv HOSTTYPE $arch
  else if (-x /usr/local/bin/arch) then
     setenv HOSTTYPE `/usr/local/bin/arch`
  else if (-x /usr/bin/arch) then
     setenv HOSTTYPE `/usr/bin/arch`
  else if (-x /bin/arch) then
     setenv HOSTTYPE `/bin/arch`
  else if (-x /usr/local/arch) then
     setenv HOSTTYPE `/usr/local/arch`
  else if (-x /usr/ucb/arch) then
     setenv HOSTTYPE `/usr/ucb/arch`
  else
     setenv HOSTTYPE unknown                   # it's a pity
  endif
endif
#  Test if this is a Solaris
if ($HOSTTYPE == "sun4" ) then
   set ver = `uname -r|awk -F. '{printf $1}'`
   if ( $ver == "5" ) setenv HOSTTYPE solaris2
   unset ver
endif
#
if ($HOSTTYPE =~ *linux*) then
   setenv HOSTTYPE linux
endif
alias arch echo $HOSTTYPE                      # Make sure we have an arch
#
#  Set username
#
if (! $?USER) then
   setenv USER `whoami`
endif
#
#  Make sure some domainname is given
#
#set flag=( `which domainname` )
#if (! -x "$flag") alias domainname echo $HOST
