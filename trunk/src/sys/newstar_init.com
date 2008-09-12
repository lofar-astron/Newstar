$!+
$!  newstar_init.com
$!     CMV 930525
$!
$!  Revision
$!	CMV 931115	Changed test for dw to *mips*
$!	CMV 931201	Split off newstar_env.csh
$!	CMV 940216	Add display of version.idx to nnews
$!	CMV 940304	Change n_remote for anonymous ftp
$!	WNB 940311	Make SSC
$!	HjV 940315      Remove nbug (now in newstar_nfra.csh)
$!	HjV 940321      Change n_remote rzmws10.nfra.nl to 192.87.1.160
$!	CMV 940329      Add $n_batch and alias nbatch
$!	CMV 940414      Test if Symbol-file is corrupt
$!	HjV 940516      Add N_ARCH check for CV and DA
$!	HjV 940526      Add another N_ARCH check for DW (decstation)
$!	CMV 940530      n_script: unset precmd (need proper prompt)
$!	WNB 940621	Make .com
$!
$!  General startup for newstar (CMV 930524)
$!
$!  This script will in general be sourced from a site specific startup
$!  file. Before invoking this script, at least the following should 
$!  have been set:
$!
$!      $n_root    path to the root of Newstar tree
$!      $n_site    name for this site
$!
$!  Optionally, you may set
$!
$!      $n_arch    architecture (two letter codes)
$!      $n_src     root of source tree
$!      $n_inc     directory with (precompiled) include files
$!      $n_hlp     directory with local hypertext network
$!      $n_lib     directory with object libraries for $n_arch
$!      $n_exe     directory with executables for $n_arch
$!      $n_tst     directory with executables for $n_arch (testversions)
$! 
$!  To set up a programming environment, at least the following should
$!  have been set as well:
$!
$!      $n_uroot   path to the root of the User tree
$!
$!  This script assumes the following general settings have been made:
$!      HOST HOSTTYPE
$!
$!  The script newstar_env.com makes sure these exist
$!
$!-
$!
$!  Init
$!
$ 	ECHO="WRITE SYS$OUTPUT"
$!
$!  Checks
$!
$	IF F$TRNLNM("N_ROOT") .EQS. "" .OR. F$TRNLNM("N_SITE").EQS. ""
$	THEN
$	   echo " "
$	   echo "Cannot continue: n_root and n_site are not defined..."
$	   echo "Contact your Newstar manager..."
$	   echo " "
$	   exit
$	ENDIF
$!
$!  Find the architecture (if not already defined in site specific script)
$!
$	IF F$TRNLNM("N_ARCH") .EQS. "" THEN -
		DEFINE/NOLOG N_ARCH VX
$!
$! Define the default tree (do not override any settings made by user
$!   at the calling siter-dependent script, allowing e.g. $n_exe to 
$!   reside on different file-systems for different architectures).
$!
$! $n_work is defined at the end of this file
$!
$	A=F$TRNLNM("N_ROOT")-"]"+"SRC.]"
$	IF F$TRNLNM("N_SRC") .EQS. "" THEN -
		DEFINE/NOLOG/TRANS=CONCEAL N_SRC 'A'
$	IF F$TRNLNM("N_INC") .EQS. "" THEN -
		DEFINE/NOLOG N_INC N_ROOT:[LIB.INC]
$	IF F$TRNLNM("N_LIB") .EQS. "" THEN -
		DEFINE/NOLOG N_LIB N_ROOT:[LIB.'F$TRNLNM("N_ARCH")']
$	IF F$TRNLNM("N_EXE") .EQS. "" THEN -
		DEFINE/NOLOG N_EXE N_ROOT:[EXE.'F$TRNLNM("N_ARCH")']
$	IF F$TRNLNM("N_HLP") .EQS. "" THEN -
		DEFINE/NOLOG N_HLP N_ROOT:[EXE.HTML]
$	IF F$TRNLNM("N_TST") .EQS. "" THEN -
		DEFINE/NOLOG N_TST N_ROOT:[TST.'F$TRNLNM("N_ARCH")']
$	IF F$TRNLNM("N_IMPORT") .EQS. "" THEN -
		DEFINE/NOLOG N_IMPORT N_ROOT:[IMPORT]
$	IF F$TRNLNM("N_BATCH") .EQS. "" THEN -
		DEFINE/NOLOG N_BATCH N_SRC:[BATCH]
$!
$!  Define all necessary aliases for general Newstar use.
$!
$	IF F$SEARCH("N_EXE:EXECUTE.EXE") .EQS. ""
$	THEN
$	  IF F$TRNLNM("DWARF_SYMBOLS") .EQS. ""
$	  THEN
$	    echo " "
$	    echo "Newstar cannot yet run on this architecture..."
$	    echo "(no file N_EXE:execute.exe exists)"
$	    echo " "
$	  ENDIF
$	  DEFINE/NOLOG DWARF_SYMBOLS "None"
$	ELSE
$	  nnews         =="TYPE/PAGE N_SRC:[SYS]version.idx,N_SRC:[doc]nnews.hlp"
$	  ncopy         =="@N_SRC:[nscan]NCOPY.COM"
$	  nbatch	=="@N_BATCH:"
$	  dwcalc*ulate	=="$n_exe:calculate.exe"
$	  dwc*lear	=="$n_exe:clear.exe"
$	  dwe*xecute	=="$n_exe:execute.exe"
$	  exe*cute	=="$n_exe:execute.exe"
$	  dwl*et	=="$n_exe:let.exe"
$	  dwr*estore	=="$n_exe:restore.exe"
$	  dwsa*ve	=="$n_exe:save.exe"
$	  dws*pecify	=="$n_exe:specify.exe"
$	  dwv*iew	=="$n_exe:view.exe"
$	  bldppd        =="$n_exe:sys_bldppd.exe"
$	  prtppd        =="$n_exe:sys_prtppd.exe"
$	  prtunits      =="$n_exe:prtunits.exe"
$	  wngfex        =="@n_src:[sys]wngfex.com"
$	  outd*warf	==   -
		"DWSAVE SYS$LOGIN:LOGIN.SAV"
$	ENDIF
$!
$! Remove corrupt symbol-files
$!
$!
$! Initialise the DWARF parameter interface, if not already done
$!
$!
$! Define DWARF_SYMBOLS, create symbols file, purge old files
$!
$!
$! Set up Dwarf symbols for interactive session
$!
$	TMP="$N_EXE:INITDW "
$	TMP 'F$STRING(F$GETJPI("","PROC_INDEX"))'+"INTERACTIVE"
$	IF F$SEARCH("SYS$LOGIN:LOGIN.SAV").NES.""
$	THEN
$	  DWRESTORE SYS$LOGIN:LOGIN.SAV
$	  echo "Symbols restored from SYS$LOGIN:LOGIN.SAV"
$	ENDIF
$!
$! Add some general symbols
$!
	Y*ES   ==".TRUE."
	N*O    ==".FALSE."
	PI    =="3.141592653589793"
	PIRAD =="3.141592653589793 RAD"
$!
$!  Set path to model database, this may be overridden by NGEN
$!
$	DEFINE/NOLOG MODELB N_SRC:[DATA]
$!
$! To facilate the script utility, we may set a different prompt
$!
$!
$!  Set up the programmers environment (n_usrc, n_uinc, ...)
$!
$!  First decide wether we work in a user-system or in the Master
$!  To work in a user system, the user should set $u_root 
$!  previously to invoking this script. This will set up
$!  a programming environment with respect to this directory
$!
$!  If the user is the owner of $n_root, the programming environment 
$!  will be set to the Master system.
$!
$!  Regardless of the programming environment, the user may always
$!  define $n_uexe as a user binary tree. By default, $n_uexe points 
$!  to the test directory of the Master executable tree.
$!
$	nup==		"$N_EXE:perl N_SRC:[SYS]update.pls"
$	nsh==		"$N_EXE:perl N_SRC:[SYS]shadow.pls"
$	ndoc==		"$N_EXE:perl N_SRC:[SYS]document.pls"
$	nhyper==	"''ndoc' hyper"
$	nscript==	"''ndoc' script"
$	DEFINE/NOLOG NSTAR_DIR    "nscan nmap nplot ncopy wng dwarf "
$	DEFINE/NOLOG n_master     "newstar@astron.nl"
$	IF F$TRNLNM("N_WORK") .EQS. "" THEN -
		DEFINE/NOLOG N_WORK N_ROOT:[WORK.'F$TRNLNM("N_ARCH")']
$	DEFINE/NOLOG/TRANS=CONCEAL  n_usrc 'F$TRNLNM("N_SRC")'  
$	DEFINE/NOLOG n_uinc  'F$TRNLNM("n_inc")'
$	DEFINE/NOLOG n_ulib  'F$TRNLNM("n_lib")'
$	DEFINE/NOLOG n_remote "192.87.1.160 anonymous newstar/src"
$	DEFINE/NOLOG n_uexe  'F$TRNLNM("n_tst")'
$	nlink==		"''nup' build -T:exe "
$	ncomp==		"''nup' build "
$	spawn==		""
$!
$!  Ready
$!
$	EXIT
