$!#  ntarz.ssc
$!# 	WNB 921117
$!# 
$!#  Revisions:
$!#	WNB 921218	Add SSC
$!#	WNB 921222	Add COM
$!#	WNB 921224	Make SSC
$!#	WNB 930303	NSTAR_DIR added
$!#	WNB 930305	Make sure aliases
$!#	WNB 930514	Correct aliases
$!#	HjV 930914	Typo
$!#	WNB 940124	Leave _TLB
$!# 
$!# 	Build Newstar export files
$!#
$!#	Uses environment variables:
$!#		WNG			where to find wng-type sources
$!#		WNG_TYPE		machine (sw, dw, hp, al, cv etc)
$!#		WNG_OLBEXE		root of wng-type .olb, .ppd, .exe
$!#		WNG_SITE		site (nfra, atnf, rug ...)
$!#		EXEDWARF_UNIX		where to find/set DWARF .exe, .ppd
$!#		LIBDWARF		where to find DWARF .olb
$!#	and also possible:
$!#		WNG_EXE, WNG_OLB, WNG_TLB, WNG_ERR, WNG_LIS, WNG_LINK
$!#
$!# Intro
$!#
$	SET NOON					!DISCARD ERRORS
$	ON CONTROL_Y THEN GOTO EXEX			!FINISH NEATLY
$	VER=F$VERIFY()					!FOR ^Y
$	PID=F$EXTRACT(4,4,F$GETJPI("","PID"))		!FOR TMP FILES
$	DEP=1						!FOR INDIRECT
$	LDAT=F$EXTRAC(2,2,F$CVTIM(,,"YEAR"))+F$CVTIM(,,"MONTH")+ -
		F$CVTIM(,,"DAY")			!VERSION
$	TELL="WRITE SYS$OUTPUT"				!FOR EASE OF USE
$	TLOG="WRITE LOG''PID'''DEP'"
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
$	TELL "Building Newstar backup files."
$	TELL " "
$	TELL "A log will be made in WNG_DIR:[WNG.-]NT''PID'''DEP'.LOG"
$	TELL "There should be about 50 Mbytes available,"
$	TELL "and it will probably take up to an hour."
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
$!# Get questions
$!#
$	OPEN/WRITE/ERROR=EXEX LOG'PID''DEP' WNG_DIR:[WNG.-]NT'PID''DEP'.LOG ! START LOG
$	TLOG "NU''PID'''DEP'.LOG"
$	TELL "Running NTARZ.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TLOG "Running NTARZ.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TELL "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TLOG "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TELL " "
$	TLOG " "
$	DOSRC="Y"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Do you want the source files? (Y|N) [''DOSRC']: " -
			SYS$COMMAND L0
$	TLOG "Do you want the source files? (Y|N) [''DOSRC']: ''L0'"
$	IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOSRC="N"
$	DOOLB="N"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Do you want the object libraries? (Y|N) [''DOOLB']: " -
			SYS$COMMAND L0
$	TLOG "Do you want the object libraries? (Y|N) [''DOOLB']: ''L0'"
$	IF "''L0'" .EQS. "y" .OR. "''L0'" .EQS. "Y" THEN DOOLB="Y"
$	DOEXE="N"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Do you want the executables? (Y|N) [''DOEXE']: " -
			SYS$COMMAND L0
$	TLOG "Do you want the executables? (Y|N) [''DOEXE']: ''L0'"
$	IF "''L0'" .EQS. "y" .OR. "''L0'" .EQS. "Y" THEN DOEXE="Y"
$!#
$!# Do sources
$!#
$ DSRC:
$ 	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOSRC
$	THEN
$	  TELL "Creating Newstar_src_VMS.''LDAT'"
$	  TLOG "Creating Newstar_src_VMS.''LDAT'"
$	  LFIL=""
$	  L0=0						! MAKE LIST
$ LP2:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    IF LFIL .NES. "" THEN LFIL=LFIL+","
$	    IF "''L1'" .EQS. "WNG"
$	    THEN
$	      COPY ['L1']*.TXT *
$	      LFIL=LFIL+"[]*.TXT,"
$	    ENDIF
$	    LFIL=LFIL+"[''L1']*.COM;0,*.SUN;0,*.TLB;0,*.HLB;0,*.TXT;0"
$	    L0=L0+1
$	    GOTO LP2
$	  ENDIF
$	  BACKUP/LIST=LOG'PID''DEP'.LOG 'LFIL' -
			WNG_DIR:[WNG.-]NEWSTAR_SRC_VMS.'LDAT'/SAVE
$	ENDIF
$!#
$!# Write Object libraries
$!#
$ DOLB:
$ 	SET DEF WNG_OLBEXE:[WNG.-]			! BASE DIRECTORY
$	IF DOOLB
$	THEN
$	  TELL "Creating Newstar_olb_VMS.''LDAT'"
$	  TLOG "Creating Newstar_olb_VMS.''LDAT'"
$	  LFIL=""
$	  L0=0						! MAKE LIST
$ LP3:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    IF LFIL .NES. "" THEN LFIL=LFIL+","
$	    LFIL=LFIL+"[''L1']*.OLB;0"
$	    IF L1 .EQS. "WNG" THEN LFIL=LFIL+",*.EXE;0"
$	    L0=L0+1
$	    GOTO LP3
$	  ENDIF
$	  BACKUP/LIST=LOG'PID''DEP' 'LFIL' -
			WNG_DIR:[WNG.-]NEWSTAR_OLB_VMS.'LDAT'/SAVE
$	ENDIF
$!#
$!# Write executables
$!#
$ DEXE:
$ 	SET DEF RUNDWARF				! BASE DIRECTORY
$	IF DOEXE
$	THEN
$	  TELL "Creating Newstar_exe_VMS.''LDAT'"
$	  TLOG "Creating Newstar_exe_VMS.''LDAT'"
$	  LFIL="*.EXE;0,[.EXE]*.EXE;0,*.PPD;0"
$	  BACKUP/LIST=LOG'PID''DEP'.LOG 'LFIL' -
			WNG_DIR:[WNG.-]NEWSTAR_EXE_VMS.'LDAT'/SAVE
$	ENDIF
$!#
$!# Cleanup
$!#
$ CLUP:
$!#
$!# Ready
$!#
$ END:
$ 	TELL " "
$ 	TLOG " "
$	TELL "The backup files can after restore in the directories:"
$	TLOG "The backup files can after restore in the directories:"
$	TELL "WNG_DIR[WNG.-] for src; WNG_OLBEXE[WNG.-] for olb;"
$	TLOG "WNG_DIR[WNG.-] for src; WNG_OLBEXE[WNG.-] for olb;"
$	TELL "and RUNDWARF for exe"
$	TLOG "and RUNDWARF for exe"
$	TELL "be used by @WNG:NBUILD to build Newstar"
$	TLOG "be used by @WNG:NBUILD to build Newstar"
$ 	TELL "Check log for errors"
$ 	TLOG "Check log for errors"
$	TELL " "
$	TLOG " "
$!#
$!#  EXIT
$!#
$ EXEX:	SET ON
$	CLOSE/ERROR=EXX1 LOG'PID''DEP			!MAKE SURE
$ EXX1:
$	SET DEF WNG_DIR:[WNG.-]				!BACK TO NORMAL
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
