$!#  nbuild.ssc
$!# 	WNB 921117
$!# 
$!#  Revisions:
$!#	WNB 921210	Change compilation of .def etc; delete c??
$!#	WNB 921222	Add SSC and nonomatch for HP
$!#	WNB 921224	Make SSC
$!#	WNB 930303	NSTAR_DIR added
$!#	WNB 930305	Make sure of aliases
$!#	WNB 930514	Correct aliases
$!#	WNB 930803	Add .dsf
$!#	WNB 940124	Leave _TLB
$!# 
$!# 	Build Newstar from standard export tar tape
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
$	TELL "Building Newstar."
$	TELL " "
$	TELL "A log will be made in WNG_DIR:[WNG.-]NB''PID'''DEP'.LOG"
$	TELL "There should be about 100 Mbytes available,"
$	TELL "and it will probably take a few hours."
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
$	OPEN/WRITE/ERROR=EXEX LOG'PID''DEP' WNG_DIR:[WNG.-]NB'PID''DEP'.LOG ! START LOG
$	TLOG "NU''PID'''DEP'.LOG"
$	TELL "Running NBUILD.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TLOG "Running NBUILD.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TELL "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TLOG "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TELL " "
$	TLOG " "
$	DOGDEF="Y"
$	IF F$SEARCH("WNG_DIR:[WNG]WNG.DEF") .NES. ""
$	THEN
$	  READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT=".def files seem to exist. Get them anyway? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	  TLOG ".def files seem to exist. Get them anyway? (Y|N) [Y]: ''L0'"
$	  IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOGDEF="N"
$	ENDIF
$	DOMDEF="Y"
$	IF F$TRNLNM("WNG_DEF") .NES. "" .AND. .NOT. DOGDEF
$	THEN
$	  READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT=".def files seem compiled. Compile them anyway? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	  TLOG ".def files seem compiled. Compile them anyway? (Y|N) [Y]: ''L0'"
$	  IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOMDEF="N"
$	ENDIF
$	DOGGRP="Y"
$	IF F$SEARCH("WNG_DIR:[WNG]WNG.GRP") .NES. ""
$	THEN
$	  READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT=".grp files seem to exist. Get them anyway? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	  TLOG ".grp files seem to exist. Get them anyway? (Y|N) [Y]: ''L0'"
$	  IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOGGRP="N"
$	ENDIF
$	DOCOMP="N"
$	IF F$SEARCH("WNG_OLBEXE:[WNG]WNLIB.OLB") .NES. ""
$	THEN
$	  READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Some compilation at least done. Compile all? (Y|N) [N]: " -
			SYS$COMMAND L0
$	  TLOG "Some compilation at least done. Compile all? (Y|N) [N]: ''L0'"
$	  IF "''L0'" .EQS. "y" .OR. "''L0'" .EQS. "Y" THEN DOCOMP="Y"
$	ELSE
$	  DOCOMP="Y"
$	ENDIF
$	DOLINK="Y"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Link all programs? (Y|N) [Y]: " -
			SYS$COMMAND L0
$	TLOG "Link all programs? (Y|N) [Y]: ''L0'"
$	IF "''L0'" .EQS. "n" .OR. "''L0'" .EQS. "N" THEN DOLINK="N"
$	TELL "You could clean up all unwanted files."
$	TELL "(0)For operation the following files should remain:"
$	TELL "	EXEDWARF:*.exe and *.ppd; RUNDWARF:*.exe; WNG_DIR:[*]*.COM"
$	TELL "(1)For easy updating the following files should also remain:"
$	TELL "	WNG_OLBEXE:[*]*.olb WNG_DIR:[*]*.tlb and *.def and *.inc"
$	TELL "		and *.grp;"
$	TELL "(2)To check programs all source files could remain"
$	TELL "(3)To check all listing files can remain"
$	TELL "I can remove for you including and above a specified level:"
$	TELL "(probably better to run with 4 first time, and rerun"
$	TELL "	nbuild later with all questions n and the proper level)"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Clean disk level (1|2|3|4) [3]: " -
			SYS$COMMAND L0
$	IF L0 .LT. 1 .OR. L0 .GT.4  THEN L0=3
$	TLOG "Clean disk level (1|2|3|4) [3]: ''L0'"
$	DOCLUP=L0
$!#
$!# Get and make .def .sun
$!#
$ GDEF:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOGDEF
$	THEN
$	  TELL "Getting .def ..."
$	  TLOG "Getting .def ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP2:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    NGET -A *.DEF *.INC *.PEF,*.DSF,*.SSC,*.COM	! GET .DEF
$	    L0=L0+1
$	    GOTO LP2
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$ MDEF:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOMDEF
$	THEN
$	  TELL "Compiling .def ..."
$	  TLOG "Compiling .def ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP3:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    NCOMP -U *.DEF *.INC *.PEF,*.DSF,*.SSC,*.COM ! COMPILE .DEF
$	    L0=L0+1
$	    GOTO LP3
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$!#
$!# Get groups
$!#
$ GGRP:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOGGRP
$	THEN
$	  TELL "Getting .grp ..."
$	  TLOG "Getting .grp ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP4:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    NGET -NZA *.GRP				! GET .GRP
$	    L0=L0+1
$	    GOTO LP4
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$!#
$!# Compile all
$!#
$ COMP:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOCOMP
$	THEN
$	  TELL "Compiling Newstar system ..."
$	  TLOG "Compiling Newstar system ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP5:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    NCOMP *.GRP					! COMPILE
$	    L0=L0+1
$	    GOTO LP5
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$!#
$!# Link all
$!#
$ LINK:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOLINK
$	THEN
$	  TELL "Linking Newstar system ..."
$	  TLOG "Linking Newstar system ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP6:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    NLINK -U *.GRP				! LINK
$	    NCOMP -U *.PIN
$	    NCOMP -U *.DSC
$	    NCOMP -U *.SSC
$	    L0=L0+1
$	    GOTO LP6
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$!#
$!# Cleanup
$!#
$ CLUP:
$	SET DEF WNG_DIR:[WNG.-]				! BASE DIRECTORY
$	IF DOCLUP .LT. 4
$	THEN
$	  TELL "Deleting listing files ..."
$	  TLOG "Deleting listing files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP7:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.LIS") .NES. "" THEN DELETE *.LIS;*
$	    IF F$SEARCH("*.ERR") .NES. "" THEN DELETE *.ERR;*
$	    IF F$SEARCH("*.OLD") .NES. "" THEN DELETE *.OLD;*
$	    IF F$SEARCH("*.NEW") .NES. "" THEN DELETE *.NEW;*
$	    IF F$SEARCH("*.LOG") .NES. "" THEN DELETE *.LOG;*
$	    IF F$SEARCH("*.TMP") .NES. "" THEN DELETE *.TMP;*
$	    IF F$SEARCH("*.OBJ") .NES. "" THEN DELETE *.OBJ;*
$	    IF F$SEARCH("*.JOU") .NES. "" THEN DELETE *.JOU;*
$	    IF F$SEARCH("''WNG_EXE':[''L1']*.MAP") .NES. "" THEN -
			DELETE WNG_EXE:['L1']*.MAP;*
$	    L0=L0+1
$	    GOTO LP7
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$	IF DOCLUP .LT. 3
$	THEN
$	  TELL "Deleting source files ..."
$	  TLOG "Deleting source files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP8:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.F%%") .NES. "" THEN DELETE *.F%%;*
$	    IF F$SEARCH("*.C%%") .NES. "" THEN DELETE/EXCLUDE=(*.COM) *.C%%;*
$	    IF F$SEARCH("*.M%%") .NES. "" THEN DELETE *.M%%;*
$	    IF F$SEARCH("*.PIN") .NES. "" THEN DELETE *.PIN;*
$	    L0=L0+1
$	    GOTO LP8
$	  ENDIF
$	  DEASSIGN SYS$OUTPUT
$	  DEASSIGN SYS$ERROR
$	ENDIF
$	IF DOCLUP .LT. 3
$	THEN
$	  TELL "Deleting object/include files ..."
$	  TLOG "Deleting object/include files ..."
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	  ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	  L0=0						! MAKE DIRECTORIES
$ LP9:	  L1=F$ELEMENT(L0,",",BLDDIR)
$	  IF L1 .NES. ","
$	  THEN
$	    SET DEF WNG_DIR:['L1']
$	    IF F$SEARCH("*.DEF") .NES. "" THEN DELETE *.DEF;*
$	    IF F$SEARCH("*.INC") .NES. "" THEN DELETE *.INC;*
$	    IF F$SEARCH("*.DSC") .NES. "" THEN DELETE *.DSC;*
$	    IF F$SEARCH("*.SSC") .NES. "" THEN DELETE *.SSC;*
$	    IF F$SEARCH("''WNG_OLB':[''L1']*.OLB") .NES. "" THEN -
			DELETE WNG_OLB:['L1']*.OLB;*
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
$ 	TELL " "
$ 	TLOG " "
$	TELL "Newstar built. Try if everything works by typing:"
$	TELL "	indwarf			(unless in LOGIN.COM)"
$	TELL "	dws ngen/nomenu"
$	TELL "	log=y"
$	TELL "				(empty line)"
$	TELL "and:"
$	TELL "	pvax WNG_DIR:[WNG.-]NB''PID'''DEP'.LOG"
$	TELL "If problem exist, rerun @WNG:NBUILD with all questions y"
$	TELL "To make a minimum backup to be able to rebuild the system,"
$	TELL "run @WNG:NTARZ"
$	TELL "Good luck"
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
