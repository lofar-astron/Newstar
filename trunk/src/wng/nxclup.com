$!#  nxclup.ssc
$!# 	WNB 921209
$!# 
$!#  Revisions:
$!#	WNB 921224	Make SSC
$!#	WNB 930303	NSTAR_DIR added
$!#	WNB 930305	Make sure aliases
$!#	WNB 930630	Typo in VAX check symbols
$!#	WNB 940124	Leave _TLB
$!# 
$!# 	Cleanup Newstar directories
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
$ !
$	ASSIGN="ASSIGN"					!MAKE SURE PROPER USE
$	BACKUP="BACKUP"
$	COPY="COPY"
$	DELETE="DELETE"
$	DIR="DIRECTORY"
$	PURGE="PURGE"
$	SET="SET"
$	SHOW="SHOW"
$!#
$!# Check environment
$!#
$	IF F$TRNLNM("WNG") .EQS. "" .OR. F$TRNLNM("WNG_OLBEXE") .EQS. "" -
		.OR. F$TRNLNM("WNG_DIR") .EQS. "" -
		.OR. "''WNG_TYPE'" .EQS. "" .OR. "''WNG_SITE'" .EQS. "" -
		.OR. "''NSTAR_DIR'" .EQS. ""
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
$	DOTMP="Y"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Delete tmp, old, new, err? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOTMP="N"
$	DOLOG="Y"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Delete log, lis, map? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOLOG="N"
$	DOSRC="N"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Delete source files? (Y|N) [N]: " -
			SYS$COMMAND L0
$	IF "''L0'" .EQS. "y" .OR. "''L0'" .EQS. "Y" THEN DOSRC="Y"
$!#
$!# Cleanup
$!#
$ CLUP:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	TELL "Purging ..."
$	PURGE WNG_DIR:[*]*.*
$	IF DOTMP
$	THEN
$	  TELL "Deleting tmp files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP7:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.ERR") .NES. "" THEN DELETE *.ERR;*
$	    IF F$SEARCH("*.OLD") .NES. "" THEN DELETE *.OLD;*
$	    IF F$SEARCH("*.*OLD") .NES. "" THEN DELETE *.*OLD;*
$	    IF F$SEARCH("*.NEW") .NES. "" THEN DELETE *.NEW;*
$	    IF F$SEARCH("*.TMP") .NES. "" THEN DELETE *.TMP;*
$	    IF F$SEARCH("*.*TMP") .NES. "" THEN DELETE *.*TMP;*
$	    IF F$SEARCH("*.OBJ") .NES. "" THEN DELETE *.OBJ;*
$	    IF F$SEARCH("*.JOU") .NES. "" THEN DELETE *.JOU;*
$	    L0=L0+1
$	    GOTO LP7
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOLOG
$	THEN
$	  TELL "Deleting lis files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP8:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.LIS") .NES. "" THEN DELETE *.LIS;*
$	    IF F$SEARCH("*.LOG") .NES. "" THEN DELETE *.LOG;*
$	    IF F$SEARCH("*.MAP") .NES. "" THEN DELETE *.LOG;*
$	    IF F$SEARCH("''WNG_EXE':[''L1']*.MAP") .NES. "" THEN -
			DELETE WNG_EXE:['L1']*.MAP;*
$	    L0=L0+1
$	    GOTO LP8
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOSRC
$	THEN
$	  TELL "Deleting source files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP9:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.F%%") .NES. "" THEN DELETE *.F%%;*
$	    IF F$SEARCH("*.C%%") .NES. "" THEN DELETE/EXCLUDE=(*.COM) *.C%%;*
$	    IF F$SEARCH("*.M%%") .NES. "" THEN DELETE *.M%%;*
$	    IF F$SEARCH("*.PIN") .NES. "" THEN DELETE *.PIN;*
$	    L0=L0+1
$	    GOTO LP9
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$!#
$!# Ready
$!#
$ END:
$!#
$!#  EXIT
$!#
$ EXEX:	SET ON
$	SET DEF WNG_DIR:[WNG.-]				!BACK TO NORMAL
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
