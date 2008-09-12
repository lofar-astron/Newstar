#+
#  newstar_init.csh
#     CMV 930525
#
#  Revision
#	CMV 931115	Changed test for dw to *mips*
#	CMV 931201	Split off newstar_env.csh
#	CMV 940216	Add display of version.idx to nnews
#	CMV 940304	Change n_remote for anonymous ftp
#	HjV 940315	Remove nbug (now in newstar_nfra.csh)
#	HjV 940321	Change n_remote rzmws10.nfra.nl to 192.87.1.160
#	CMV 940329	Add $n_batch and alias nbatch
#	CMV 940414	Test if Symbol-file is corrupt
#	HjV 940516	Add N_ARCH check for CV and DA
#	HjV 940526	Add another N_ARCH check for DW (decstation)
#	CMV 940530	n_script: unset precmd (need proper prompt)
#	HjV 941017	Use ftp.nfra.nl iso. 192.87.1.160 for n_remote
#	CMV 941110	Also define n_remote if not Master account
#       HjV 950130      Use $HOME in directories iso. ~ or ~/
#	WNB 950224	Make sure non-existant dwarf symbol file replaced
#	WNB 950808	Add n_arch check for SGi
#	HjV 951212	Add n_www
#	JPH 960315	Add dwrec|p|n
#	HjV 960619	Add test for Solaris
#
#  General startup for newstar (CMV 930524)
#
#  This script will in general be sourced from a site specific startup
#  file. Before invoking this script, at least the following should 
#  have been set:
#
#      $n_root    path to the root of Newstar tree
#      $n_site    name for this site
#
#  Optionally, you may set
#
#      $n_arch    architecture (two letter codes)
#      $n_src     root of source tree
#      $n_inc     directory with (precompiled) include files
#      $n_hlp     directory with local hypertext network
#      $n_lib     directory with object libraries for $n_arch
#      $n_exe     directory with executables for $n_arch
#      $n_tst     directory with executables for $n_arch (testversions)
#      $n_www     your favorite WWW browser
# 
#  To set up a programming environment, at least the following should
#  have been set as well:
#
#      $n_uroot   path to the root of the User tree
#
#  This script assumes the following general settings have been made:
#      HOST HOSTTYPE
#
#  The script newstar_env.csh makes sure these exist
#
#
#
if (! $?n_root || ! $?n_site ) then
   echo " "
   echo "Cannot continue: n_root and n_site are not defined..."
   echo "Contact your Newstar manager..."
   echo " "
   exit
endif

#
#  Find the architecture (if not already defined in site specific script)
#
if (! $?n_arch) then
  if ($HOSTTYPE =~ sun*) then
     setenv n_arch sw
  else if ($HOSTTYPE =~ solaris*) then
     setenv n_arch so
  else if ($HOSTTYPE =~ hp*) then
     setenv n_arch hp
  else if ($HOSTTYPE == alliant) then
     setenv n_arch al
  else if ($HOSTTYPE =~ dec* || $HOSTTYPE =~ *mips*) then
     setenv n_arch dw
  else if ($HOSTTYPE =~ alpha) then
     setenv n_arch da
  else if ($HOSTTYPE =~ c2mp) then
     setenv n_arch cv
  else if ($HOSTTYPE =~ iris*) then
     setenv n_arch sg
  else if ($HOSTTYPE =~ linux*) then
     setenv n_arch li
  endif
endif

#
# Define the default tree (do not override any settings made by user
#   at the calling siter-dependent script, allowing e.g. $n_exe to 
#   reside on different file-systems for different architectures).
#
# $n_work is defined at the end of this file
#
if (! $?n_src)    setenv n_src    $n_root/src
if (! $?n_doc)    setenv n_doc    $n_src/doc
if (! $?n_inc)    setenv n_inc    $n_root/lib/inc
if (! $?n_lib)    setenv n_lib    $n_root/lib/$n_arch
if (! $?n_exe)    setenv n_exe    $n_root/exe/$n_arch
if (! $?n_hlp)    setenv n_hlp    $n_root/exe/html
if (! $?n_tst)    setenv n_tst    $n_root/tst/$n_arch
if (! $?n_import) setenv n_import $n_root/import
if (! $?n_batch)  setenv n_batch  $n_src/batch
if (! $?n_www)    setenv n_www    $n_exe/xmosaic.exe

#
#  Define all necessary aliases for general Newstar use.
#
if (! -e $n_exe/execute.exe ) then
   if (! $?DWARF_SYMBOLS) then
      echo " "
      echo "Newstar cannot yet run on this architecture..."
      echo "(no file "\$n_exe"/execute.exe exists)"
      echo " "
   endif
   setenv DWARF_SYMBOLS None
else
   alias nnews         'more $n_src/sys/version.idx $n_src/doc/nnews.hlp'
   alias ncopy         '$n_src/nscan/ncopy.csh'
   alias nbatch        '$n_batch/\!^.csh \!:2* '
   alias dwcalc        '$n_exe/calculate.exe \!* '
   alias dwcalculate   '$n_exe/calculate.exe \!* '
   alias dwc           '$n_exe/clear.exe \!* '
   alias dwclear       '$n_exe/clear.exe \!* '
   alias dwe           '$n_exe/execute.exe \!* '
   alias dwexe         '$n_exe/execute.exe \!* '
   alias exe           '$n_exe/execute.exe \!* '
   alias dwl           '$n_exe/let.exe \!* '
   alias dwlet         '$n_exe/let.exe \!* '
   alias dwr           '$n_exe/restore.exe \!* '
   alias dwrestore     '$n_exe/restore.exe \!* '
   alias dwrec 	       '$n_src/sys/dwrecord.csh m'
   alias dwren         '$n_src/sys/dwrecord.csh n'
   alias dwrep         '$n_src/sys/dwrecord.csh r'
   alias dwsa          '$n_exe/save.exe \!* '
   alias dwsave        '$n_exe/save.exe \!* '
   alias dws           '$n_exe/specify.exe \!* '
   alias dwspecify     '$n_exe/specify.exe \!* '
   alias dwv           '$n_exe/view.exe \!* '
   alias dwview        '$n_exe/view.exe \!* '
   alias bldppd        '$n_exe/sys_bldppd.exe \!* '
   alias prtppd        '$n_exe/sys_prtppd.exe \!* '
   alias prtunits      '$n_exe/prtunits.exe'
   alias wngfex        '$n_src/sys/wngfex.csh'
   alias genaid        '$n_exe/genaid.exe \!* '
   alias outdwarf   \
      'if ($?DWARF_SYMBOLS) "cp" $DWARF_SYMBOLS $HOME/SYMBOL_DIR/SAVSYMBOLS'
   alias outd          outdwarf
#
# Remove corrupt symbol-files
#
   if ($?DWARF_SYMBOLS) then
      if (-z $DWARF_SYMBOLS) then
         "rm" -f $DWARF_SYMBOLS
         unsetenv DWARF_SYMBOLS
      endif
   endif
#
# Make sure symbol file exist
#
   if ($?DWARF_SYMBOLS) then
      if (! -e $DWARF_SYMBOLS) then
         unsetenv DWARF_SYMBOLS
      endif
   endif
#
# Initialise the DWARF parameter interface, if not already done
#
   if (! $?DWARF_SYMBOLS) then
#
# Define DWARF_SYMBOLS, create symbols file, purge old files
#
     setenv DWARF_SYMBOLS $HOME/SYMBOL_DIR/SYMBOL.$$

     if (! -e $HOME/SYMBOL_DIR) then
       "mkdir" $HOME/SYMBOL_DIR
        echo "Created subdirectory $HOME/SYMBOL_DIR"
     endif

     if (-e $DWARF_SYMBOLS) then
        "rm" -f $DWARF_SYMBOLS
     endif

     if (-e $HOME/SYMBOL_DIR/SAVSYMBOLS) then
       "cp" $HOME/SYMBOL_DIR/SAVSYMBOLS $DWARF_SYMBOLS
        echo "Symbols restored from $HOME/SYMBOL_DIR/SAVSYMBOLS"
     else
       "touch" $DWARF_SYMBOLS
     endif
     chmod 644 $DWARF_SYMBOLS

     "find" $HOME/SYMBOL_DIR/SYMBOL.*  -atime +7 -exec "rm" "{}" ";"   >& /dev/null
#
# Set up Dwarf symbols for interactive session
#
     $n_exe/initdw.exe $$+INTERACTIVE
#
# Add some general symbols
#
     $n_exe/let.exe /NOLOG <<_endlet_
YES   = .TRUE.
YE    = .TRUE.
Y     = .TRUE.
NO    = .FALSE.
N     = .FALSE.
PI    = 3.141592653589793
PIRAD = 3.141592653589793 RAD

_endlet_
#
#  Set path to model database, this may be overridden by NGEN
#
     setenv MODELB     $n_src/data/
     setenv MODEL_PATH $n_src/data/

   endif
#
# To facilate the script utility, we may set a different prompt
#
   if ($?n_script) then
      unalias precmd
      set prompt="script> "
      alias \# 'echo \!* >/dev/null'
      dws dwarf /nomenu <<_EOD_
bell=on
#
_EOD_
   endif

endif

#
#  Set up the programmers environment (n_usrc, n_uinc, ...)
#
#  First decide wether we work in a user-system or in the Master
#  To work in a user system, the user should set $u_root 
#  previously to invoking this script. This will set up
#  a programming environment with respect to this directory
#
#  If the user is the owner of $n_root, the programming environment 
#  will be set to the Master system.
#
#  Regardless of the programming environment, the user may always
#  define $n_uexe as a user binary tree. By default, $n_uexe points 
#  to the test directory of the Master executable tree.
#
alias ndoc          $n_src/sys/document.csh 
alias nhyper        ndoc hyper
alias nscript       ndoc script

alias nsh           $n_src/sys/shadow.csh
alias nup           $n_src/sys/update.csh
alias nlink         nsh build -T:exe
alias ncomp         nsh build 
setenv NSTAR_DIR    "nscan nmap nplot ncopy wng dwarf "
setenv n_master     newstar@astron.nl
setenv n_remote     "ftp.astron.nl anonymous newstar/src"

alias nsmail "elm -s "

if (-o $n_src) then
   if (! $?n_work)   setenv n_work   $n_root/work/$n_arch
   setenv n_usrc  $n_src
   setenv n_uinc  $n_inc
   setenv n_ulib  $n_lib
   setenv n_uexe  $n_tst
   alias  nlink   nup build -T:exe 
   alias  ncomp   nup build 
   alias  spawn   '( \!* |& nsmail "\!*" newstar@astron.nl >/dev/null ) &'
else if ($?n_uroot) then
   if (! $?n_usrc)  setenv n_usrc  $n_uroot/src
   if (! $?n_uinc)  setenv n_uinc  $n_uroot/lib/inc
   if (! $?n_ulib)  setenv n_ulib  $n_uroot/lib/$n_arch
   if (! $?n_uexe)  setenv n_uexe  $n_uroot/exe/$n_arch
   if (! $?n_work)  setenv n_work  $n_uroot/work/$n_arch
endif
