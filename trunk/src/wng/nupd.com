$!#  nupd.ssc
$!# 	WNB 921117
$!# 
$!#  Revisions:
$!#	WNB 921224	Make SSC
$!#	WNB 930303	Add NSTAR_DIR
$!#	WNB 930305	Make sure aliases
$!#	WNB 930514	Correct alias read
$!#	WNB 930725	Typo (continuation - in VAX)
$!#	WNB 930731	Typo (' missed - VAX)
$!#	WNB 930803	Add local mode
$!#	WNB 930818	Typo
$!#	WNB 930901	Remove logging from Unix: .dsc give segmentation
$!#				faults: too many files open?
$!#	WNB 930921	Typo
$!#	WNB 931124	Remove logging for password
$!#	WNB 940124	Leave _TLB
$!#
$!# 	Update Newstar system across network
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
$!#		WNG_NODE, WNG_NODEDIR, WNG_NODEUSER
$!#		WNG_EXE, WNG_OLB, WNG_TLB, WNG_ERR, WNG_LIS, WNG_LINK
$!#
$!# Intro
$!#
$	SET NOON					!DISCARD ERRORS
$	ON CONTROL_Y THEN GOTO EXEX			!FINISH NEATLY
$	VER=F$VERIFY()					!FOR ^Y
$	PID=F$EXTRACT(4,4,F$GETJPI("","PID"))		!FOR TMP FILES
$	DEP=1						!FOR INDIRECT
$	LOCAL=0						!FOR LOCAL USE
$	TELL="WRITE SYS$OUTPUT"				!FOR EASE OF USE
$	TLOG="WRITE LOG''PID'''DEP'"
$ !
$	ASSIGN="ASSIGN"					!MAKE SURE PROPER USE
$	COPY="COPY"
$	DELETE="DELETE"
$	DIR="DIRECTORY"
$	PURGE="PURGE"
$	SET="SET"
$	SHOW="SHOW"
$	TELL " "
$	TELL "Updating Newstar system."
$	TELL " "
$	TELL "A log will be made in WNG_DIR:[WNG.-]NU''PID'''DEP'.LOG"
$	TELL "There should be about 20 Mbytes available,"
$	TELL "and it will probably take up to an hour."
$	TELL " "
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
		CREATE/DIR 'WNG_EXE':['L1']
$	  IF F$PARSE("''WNG_OLB':[''L1']") .EQS. "" THEN -
		CREATE/DIR 'WNG_OLB':['L1']
$	  L0=L0+1
$	  GOTO LP1
$	ENDIF
$	WNG_NODE="rzmvx4.astron.nl"			! NODE INFO
$	WNG_NODEDIR="user5:[wnb]"
$	WNG_NODEUSER="printvax printvax_90a"
$!#
$!# Get questions
$!#
$	OPEN/WRITE/ERROR=EXEX LOG'PID''DEP' WNG_DIR:[WNG.-]NU'PID''DEP'.LOG ! START LOG
$	TLOG "NU''PID'''DEP'.LOG"
$	TELL "Running NUPD.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TLOG "Running NUPD.COM at ''WNG_SITE'(''WNG_TYPE')"
$	TELL "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TLOG "on ''F$GETSYI("NODENAME")' at ''F$TIME()'"
$	TELL " "
$	TLOG " "
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Remote update node (*=local)[''WNG_NODE']: " -
		SYS$COMMAND L0
$	TLOG "Remote update node (*=local)[''WNG_NODE']: ''L0'"
$	IF "''L0'" .EQS. "*"
$	THEN
$	  LOCAL=1
$	  GOTO VERS
$	ENDIF
$	IF "''L0'" .NES. "" THEN WNG_NODE="''L0'"
$	L1="''F$ELEMENT(0," ",WNG_NODEUSER)'"
$	L2="''F$ELEMENT(1," ",WNG_NODEUSER)'"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Remote update user [''L1']: " SYS$COMMAND L0
$	TLOG "Remote update user [''L1']: ''L0'"
$	SET TERM/NOECHO
$	IF "''L0'" .NES. "" THEN L1="''L0'"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Remote password [''L2']: " SYS$COMMAND L0
$	SET TERM/ECHO
$	TELL " "
$	TLOG "Remote password [''L2']: "
$	IF "''L0'" .NES. "" THEN L2="''L0'"
$	WNG_NODEUSER="''L1' ''L2'"
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Remote directory [''WNG_NODEDIR']: " SYS$COMMAND L0
$	TLOG "Remote directory [''WNG_NODEDIR']: ''L0'"
$	IF "''L0'" .NES. "" THEN WNG_NODEDIR="''L0'"
$ VERS:
$	READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Remote update version (YYMMDD) []: " SYS$COMMAND L0
$	TLOG "Remote update version (YYMMDD) []: ''L0'"
$	IF "''L0'" .EQS. "" THEN GOTO EXEX		! STOP
$	DOVERS="''L0'"
$!#
$!# Get update info
$!#
$ GINFO:
$ 	SET DEF WNG					! BASE DIRECTORY
$	IF LOCAL.EQ.1 THEN GOTO COMP
$ 	TELL "Getting update information ..."
$ 	TLOG "Getting update information ..."
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	L0=0						! MAKE DIRECTORIES
$ LP2:	L1=F$ELEMENT(L0,",",BLDDIR)
$	IF L1 .NES. ","
$	THEN
$	  SET DEF WNG_DIR:['L1']
$	  NNET -NZ 'L1''DOVERS'.GRP
$	  L0=L0+1
$	  GOTO LP2
$	ENDIF
$	DEASSIGN SYS$OUTPUT
$	DEASSIGN SYS$ERROR
$!#
$!# Get updated files
$!#
$ GFIL:
$ 	SET DEF WNG					! BASE DIRECTORY
$ 	TELL "Getting update files ..."
$ 	TLOG "Getting update files ..."
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	L0=0						! MAKE DIRECTORIES
$ LP3:	L1=F$ELEMENT(L0,",",BLDDIR)
$	IF L1 .NES. ","
$	THEN
$	  SET DEF WNG_DIR:['L1']
$	  IF F$SEARCH("''L1'''DOVERS'.GRP") .NES. "
$	  THEN
$	    NNET 'L1''DOVERS'.GRP
$	  ENDIF
$	  L0=L0+1
$	  GOTO LP3
$	ENDIF
$	DEASSIGN SYS$OUTPUT
$	DEASSIGN SYS$ERROR
$!#
$!# Compile files
$!#
$ COMP:
$ 	SET DEF WNG					! BASE DIRECTORY
$ 	TELL "Compiling updated files ..."
$ 	TLOG "Compiling updated files ..."
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	L0=0
$ LP4:	L1=F$ELEMENT(L0,",",BLDDIR)
$	IF L1 .NES. ","
$	THEN
$	  SET DEF WNG_DIR:['L1']
$	  IF F$SEARCH("''L1'''DOVERS'.GRP") .NES. ""
$	  THEN
$ 	    NCOMP -U 'L1''DOVERS'.GRP
$	  ENDIF
$	  L0=L0+1
$	  GOTO LP4
$	ENDIF
$	DEASSIGN SYS$OUTPUT
$	DEASSIGN SYS$ERROR
$!#
$!# Link
$!#
$ LINK:
$ 	SET DEF WNG					! BASE DIRECTORY
$ 	TELL "Linking updated programs ..."
$ 	TLOG "Linking updated programs ..."
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$OUTPUT
$	ASSIGN/NOLOG LOG'PID''DEP' SYS$ERROR
$	L0=0
$ LP5:	L1=F$ELEMENT(L0,",",BLDDIR)
$	IF L1 .NES. ","
$	THEN
$	  SET DEF WNG_DIR:['L1']
$	  IF F$SEARCH("''L1'''DOVERS'.GRP") .NES. ""
$	  THEN
$ 	    NLINK -U 'L1''DOVERS'.GRP
$	  ENDIF
$	  L0=L0+1
$	  GOTO LP5
$	ENDIF
$	DEASSIGN SYS$OUTPUT
$	DEASSIGN SYS$ERROR
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
$ 	TELL "Newstar updated for version ''DOVERS'"
$ 	TLOG "Newstar updated for version ''DOVERS'"
$ 	TELL "Check log for errors"
$ 	TLOG "Check log for errors"
$	TELL " "
$	TLOG " "
$!#
$!#  EXIT
$!#
$ EXEX:	SET ON
$	SET TERM/ECHO
$	DEASSIGN SYS$OUTPUT
$	CLOSE/ERROR=EXX1 LOG'PID''DEP'			!MAKE SURE
$ EXX1:
$	SET DEF WNG_DIR:[WNG]				!BACK TO NORMAL
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
