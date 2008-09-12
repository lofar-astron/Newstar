$!#  nredo.ssc
$!# 	WNB 921231
$!# 
$!#  Revisions:
$!#	WNB 930303	NSTAR_DIR added
$!#	WNB 930305	Make sure aliases
$!#	WNB 930514	Correct aliases
$!#	WNB 940124	Leave _TLB
$!# 
$!# 	Rebuild Newstar from current files/text library
$!#
$!#	Uses environment variables:
$!#		WNG			where to find wng-type sources
$!#		WNG_TYPE		machine (sw, dw, hp, al, cv etc)
$!#		WNG_OLBEXE		root of wng-type .olb, .ppd, .exe
$!#		WNG_SITE		site (nfra, atnf, rug ...)
$!#		EXEDWARF_UNIX		where to find/set DWARF .exe, .ppd
$!#		LIBDWARF		where to find DWARF .olb
$!#		NSTAR_DIR		N directories
$!#	and also possible:
$!#		WNG_EXE, WNG_OLB, WNG_TLB, WNG_ERR, WNG_LIS, WNG_LINK
$!#
$!#	Use as:
$!#		$WNG/nredo.sun c|l|cl [switches] ["dir1 dir2 ..."]	(Unix)
$!#		@WNG:NREDO c|l|cl [switches] [dir1,dir2,...]		(VAX)
$!#	c/l compile/link
$!#	switches e.g. -l1 (always at least - if dir given); -u is default
$!#	directories e.g. nscan (default all N-directories)
$!#
$!# Intro
$!#
$	SET NOON					!DISCARD ERRORS
$	ON CONTROL_Y THEN GOTO EXEX			!FINISH NEATLY
$	VER=F$VERIFY()					!FOR ^Y
$	PID=F$EXTRACT(4,4,F$GETJPI("","PID"))		!FOR TMP FILES
$	DEP=1						!FOR INDIRECT
$	LDAT=F$EXTRACT(2,2,F$CVTIM(,,"YEAR"))+F$CVTIM(,,"MONTH")+ -
		F$CVTIM(,,"DAY")			!VERSION
$	TELL="WRITE SYS$OUTPUT"				!FOR EASE OF USE
$ !
$	ASSIGN="ASSIGN"					!MAKE SURE PROPER USE
$	BACKUP="BACKUP"
$	COPY="COPY"
$	DELETE="DELETE"
$	DIR="DIRECTORY"
$	PURGE="PURGE"
$	SET="SET"
$	SHOW="SHOW"
$	TELL " "
$	TELL "Rebuilding Newstar."
$	TELL " "
$	TELL "A log will be made in the standard UPD''LDAT'.LOG"
$	TELL " "
$!#
$!# Check environment
$!#
$	IF F$TRNLNM("WNG") .EQS. "" .OR. F$TRNLNM("WNG_OLBEXE") .EQS. "" -
		.OR. F$TRNLNM("WNG_DIR") .EQS. "" -
		.OR. "''WNG_TYPE'" .EQS. "" .OR. "''WNG_SITE'" .EQS. "" -
		.OR. "''NSTAR_DIR'" .EQS. "" -
$	THEN
$	  TELL " Error: Must have logicals WNG, WNG_OLBEXE, WNG_DIR and globals"
$	  TELL "        WNG_TYPE, WNG_SITE, NSTAR_DIR defined"
$	  TELL "        You probably have not included the proper"
$	  TELL "        files in LOGIN.COM"
$	  GOTO EXEX
$	ENDIF
$	IF F$TRNLNM("EXEDWARF") .EQS. ""  .OR. -
			F$TRNLNM("LIBDWARF") .EQS. "" .OR. -
			F$TRNLNM("RUNDWARF") .EQS. ""
$	THEN
$	  TELL " Error:   Cannot do everything with EXEDWARF and/or"
$	  TELL "          LIBDWARF and/or RUNDWARF not defined"
$	  TELL "          You probably have not included the proper"
$	  TELL "          files in LOGIN.COM"
$	  GOTO EXEX
$	ENDIF
$	IF "''NXEC'" .EQS. ""
$	THEN
$	  TELL " Error:   You have no symbol for NXEC etc."
$	  TELL "          You probably have not included the proper"
$	  TELL "          files in LOGIN.COM"
$	  GOTO EXEX
$	ENDIF
$	IF "''WNG_EXE'" .EQS. "" THEN -
	      WNG_EXE=F$TRNLNM("WNG_OLBEXE")
$	IF "''WNG_OLB'" .EQS. "" THEN -
	      WNG_OLB=F$TRNLNM("WNG_OLBEXE")
$	WNG_TLB=""					! MAKE SURE
$	WNG_ERR=""
$	WNG_LIS=""
$	WNG_LINK=""
$	BLDDIR="''NSTAR_DIR'"				! Newstar directories
$	IF P3 .NES. "" THEN BLDDIR="''P3'"
$	L0=0						! MAKE DIRECTORIES
$ LP1:	L1=F$ELEMENT(L0,",",BLDDIR)
$	IF L1 .NES. ","
$	THEN
$	  IF F$PARSE("''WNG_EXE':[''L1']") .EQS. "" THEN - ! MAKE DIRECTORIES
		CREATE/DIR 'WNG_EXE'['L1']
$	  IF F$PARSE("''WNG_OLB':[''L1']") .EQS. "" THEN -
		CREATE/DIR 'WNG_OLB':['L1']
$	  L0=L0+1
$	  GOTO LP1
$	ENDIF
$!#
$!# Start
$!#
$	TELL "Running NREDO.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TELL " "
$!#
$!# Compiling
$!#
$ GCMP:
$	IF F$LOCATE("c",P1) .EQS. F$LENGTH(P1) .AND. -
		F$LOCATE("C",P1) .EQS. F$LENGTH(P1) THEN GOTO LINK !NO COMP
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	TELL "Compiling ..."
$	L0=0						! MAKE DIRECTORIES
$ LP2:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    TELL "... ''L1'"
$	    ASSIGN/NOLOG NL: SYS$OUTPUT
$	    NCOMP -U 'P2' %%%.GRP			! Compile groups
$	    DEASSIGN SYS$OUTPUT
$	    L0=L0+1
$	    GOTO LP2
$	  ENDIF
$!#
$!# Link all
$!#
$ LINK:
$	IF F$LOCATE("l",P1) .EQS. F$LENGTH(P1) .AND. -
		F$LOCATE("L",P1) .EQS. F$LENGTH(P1) THEN GOTO END !NO LINK
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	TELL "Linking Newstar system ..."
$	L0=0						! MAKE DIRECTORIES
$ LP6:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    TELL "... ''L1'"
$	    ASSIGN/NOLOG NL: SYS$OUTPUT
$	    NLINK -U 'P2' %%%.GRP			! LINK
$	    DEASSIGN SYS$OUTPUT
$	    L0=L0+1
$	    GOTO LP6
$	  ENDIF
$!#
$!# Ready
$!#
$ END:
$ 	TELL " "
$	TELL "Newstar rebuilt."
$	TELL " "
$!#
$!#  EXIT
$!#
$ EXEX:	SET ON
$ EXX1:
$	SET DEF WNG_DIR:[WNG.-]				!BACK TO NORMAL
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
