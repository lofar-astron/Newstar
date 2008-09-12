$!#  nxec.ssc
$!# 	WNB 920908
$!# 
$!#  Revisions:
$!#	WNB 920930	Additions, overhaul, speed up
$!#	WNB 921014	Add -a nnet
$!#	WNB 921015	Change setting of WNG_OLB WNG_EXE
$!#	WNB 921019	Auto nget for ncomp included
$!#	WNB 921104	Typo (" missing); default u_d; fnam check
$!#	WNB 921113	Do postponed ar
$!#	WNB 921116	Add G qualifier and allow multi-level .grp
$!#	WNB 921117	Add regular expressions
$!#	WNB 921130	Stop multiple ar; correct set echo; tr for HP
$!#	WNB 921208	Add date, list of directories; new update log
$!#	WNB 921209	Add logical link test; -a0 switch
$!#	WNB 921211	Add P qualifier
$!#	WNB 921216	Add ##
$!#	WNB 921230	Make SSC
$!#	WNB 930108	Add X11
$!#	JPH 930225	-qv<x> option to start echo/verify with execution
$!#			include $WNG_LINK in c-dir
$!#			comments
$!#	WNB 930301	Add aliases for # type; add +es for HP; add NSTAR_DIR
$!#	WNB 930325	Cater for different fold
$!#	WNB 930330	Add _ax.tlb; giplib.olb; pgplot.olb
$!#	WNB 930405	Add SPAWN for better control (VAX)
$!#	HjV 930416	Add path for include files for HP
$!#	WNB 930427	More general include path HP
$!#	WNB 930429	Add -lm for SUN; delete pgplot
$!#	HjV 930503	Add -lm for HP; remove -lm for SUN
$!#	WNB 930504	Problem linking NGCALC
$!#	CMV 930906	Switches for SUN in RUG
$!#	WNB 930921	Assume never WNG_OLB/WNG_EXE in VMS
$!#	WNB 930922	WNG_OLB/WNG_EXE recursive calls
$!#	WNB 931213	Proper X11 path
$!#	WNB 931217	Add NCOPY to NSTAR_DIR
$!#	WNB 940124	Add _stlb, _s1tlb
$!# 
$!# 	Compile, link, maintain routines. Use as:
$!# 	Unix:	$WNG/nxec.sun <type> [-<code>] ... [<name>,...] ... [-<code>] ...
$!#	VAX:	@WNG:NXEC ...
$!# 
$!# 		Type can be:	NCOMP		Compile
$!# 				NLINK		Link
$!# 				NGET		Get from text library
$!# 				NXREF		Make Fortran crossreference
$!# 				NDEL		Delete
$!#				NNET		Get across net
$!# 		See Help (a ? in a parameter) for details.
$!#
$!#	Uses environment variables:
$!#		WNG			where to find wng-type sources
$!#		WNG_TYPE		machine (sw, dw, hp, al, cv etc)
$!#		WNG_OLBEXE		root of wng-type .olb, .ppd, .exe
$!#		WNG_SITE		site (nfra, atnf, rug ...)
$!#		EXEDWARF_UNIX		where to find/set DWARF .exe, .ppd
$!#		LIBDWARF		where to find DWARF .olb
$!#	and also possible:
$!#		WNG_NODE		node name (number) of central node
$!#		WNG_NODEUSER		user (or user and  pw) at central node
$!#		WNG_NODEDIR		WNG-root dir at central node
$!#					(DV:[...] or /.../......)
$!#		NSTAR_DIR		Newstar directories
$!#       The following env. variables are optional, to be used to define
$!#  a specific target file system (foreign host or shadow tree). Defaults are
$!#  shown in parentheses,  subdir is the name of the source's subdirectory
$!#  under $WNG/.. .
$!#    WNG_EXE            (WNG_OLBEXE)            parent of .exe target
$!#    WNG_OLB            (WNG_OLBEXE)            parent of .olb target
$!#    WNG_TLB            current directory       .tlb target
$!#    WNG_ERR, WNG_LIS   current directory       .lis, .err target
$!#    WNG_LINK           current directory       target for .f soft links (must
$!#                                               be subdir of WNG_EXE, WNG_OLB
$!#						or WNG/..)
$!#
$!# Intro
$!#
$	SET NOON					!DISCARD ERRORS
$	ON CONTROL_Y THEN GOTO EXEX			!FINISH NEATLY
$	INTAT="X0"					!INTERACTIVE
$	VER=F$VERIFY()					!FOR ^Y
$	TELL="WRITE SYS$OUTPUT"				!FOR EASE OF USE
$	PID=F$EXTRACT(4,4,F$GETJPI("","PID"))		!FOR TMP FILES
$	DEP=1						!FOR INDIRECT
$	IF P1 .EQS. "X1" .OR. P1 .EQS. "X2"		!NON-INTERACTIVE
$	THEN
$	  INTAT="''P1'"					!SET VARIABLES
$	  PNAM=P2
$	  PCOD=P3
$	  DEP=P4+1
$	ENDIF
$	C_DATE=F$EXTRACT(2,2,F$CVTIM(,,"YEAR"))+F$CVTIM(,,"MONTH")+ -
		F$CVTIM(,,"DAY")			!DATE YYMMDD
$	C_TIME=F$CVTIM(,,"HOUR")+F$CVTIM(,,"MINUTE")+ -
		F$CVTIM(,,"SECOND")			!TIME HHMMSS
$	C_UPD="UPD''C_DATE'.LOG"			!LOG NAME
$	IF F$SEARCH(C_UPD) .EQS. ""			!CREATE LOG
$	THEN
$	  L1="None"
$	  L0=1
$ LP23:
$	  L3=F$SEARCH("UPD%%%%%%.LOG",L0)
$	  IF L3 .NES. ""
$	  THEN
$	    L1=L3
$	    GOTO LP23
$	  ENDIF
$	  OPEN/WRITE/ERROR=EXEX UP'PID''DEP' 'C_UPD'
$	  WRITE/ERROR=EXEX UP'PID''DEP' "! ''C_UPD'	(Previous: ''L1')"
$	  CLOSE/ERROR=EXEX UP'PID''DEP'
$	ENDIF
$	UTELL="WRITE UP''PID'''DEP'"
$	OPEN/APPEND/SHARE/ERROR=EXEX UP'PID''DEP' 'C_UPD' !OPEN LOG
$ !							!CURRENT DIRECTORY
$	CWD=F$PARSE(F$ENVIRONMENT("DEFAULT"),,,"DEVICE","NO_CONCEAL")+ -
		F$PARSE(F$ENVIRONMENT("DEFAULT"),,,"DIRECTORY","NO_CONCEAL")
$	L0=0						!GET TAIL DIRECTORY
$ LP1:	  L0=L0+1
$	  L1=F$ELEMENT(L0,"[",CWD)
$	  IF L1 .NES. "["
$	  THEN
$	    L2="["+L1
$	    GOTO LP1
$	  ENDIF
$	CWDT=L2
$	L0=0
$ LP2:	  L0=L0+1
$	  L1=F$ELEMENT(L0,".",L2)
$	  IF L1 .NES. "."
$	  THEN
$	    CWDT="["+L1
$	    GOTO LP2
$	  ENDIF
$ !
$	ASSIGN="ASSIGN"					!MAKE SURE PROPER USE
$	CONVERT="CONVERT"
$	COPY="COPY"
$	CREATE="CREATE"
$	DELETE="DELETE"
$	DIR="DIRECTORY"
$	FORTRAN="FORTRAN"
$	LIBRARY="LIBRARY"
$	LINK="LINK"
$	MACRO="MACRO"
$	PURGE="PURGE"
$	SET="SET"
$	SHOW="SHOW"
$	SPAWN="SPAWN"
$	SUBMIT="SUBMIT"
$!#
$!# Check environment
$!#
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  IF F$TRNLNM("WNG") .EQS. "" .OR. F$TRNLNM("WNG_OLBEXE") .EQS. "" -
		.OR. "''WNG_TYPE'" .EQS. "" .OR. "''WNG_SITE'" .EQS. ""
$	  THEN
$	    TELL " Error: Must have logicals WNG, WNG_OLBEXE and globals"
$	    TELL "        WNG_TYPE, WNG_SITE defined"
$	    UTELL " Error: Must have logicals WNG, WNG_OLBEXE and globals"
$	    UTELL "        WNG_TYPE, WNG_SITE defined"
$	    GOTO EXEX
$	  ENDIF
$	  IF F$TRNLNM("EXEDWARF") .EQS. ""  .OR. -
			F$TRNLNM("LIBDWARF") .EQS. "" .OR. -
			F$TRNLNM("RUNDWARF") .EQS. ""
$	  THEN
$	    TELL " Warning: Cannot do everything with EXEDWARF and/or"
$	    TELL "          LIBDWARF and/or RUNDWARF not defined"
$	    UTELL " Warning: Cannot do everything with EXEDWARF and/or"
$	    UTELL "          LIBDWARF and/or RUNDWARF not defined"
$	  ENDIF
$	  IF "''WNG_NODE'" .EQS. "" THEN WNG_NODE=""	!DEFINE
$	  IF "''WNG_NODEDIR'" .EQS. "" THEN WNG_NODEDIR=""	
$	  IF "''WNG_NODEUSER'" .EQS. "" THEN WNG_NODEUSER=""
$	  WNG_EXE=F$PARSE(F$TRNLNM("WNG_OLBEXE"),,,"DEVICE","NO_CONCEAL")+ -
		F$PARSE(F$TRNLNM("WNG_OLBEXE"),"''CWDT'",, -
			"DIRECTORY","NO_CONCEAL")
$	  WNG_OLB=F$PARSE(F$TRNLNM("WNG_OLBEXE"),,,"DEVICE","NO_CONCEAL")+ -
		F$PARSE(F$TRNLNM("WNG_OLBEXE"),"''CWDT'",, -
			"DIRECTORY","NO_CONCEAL")
$	  IF "''WNG_TLB'" .EQS. "" THEN -
		WNG_TLB="''CWD'"
$	  IF "''WNG_ERR'" .EQS. "" THEN -
		WNG_ERR="''CWD'"
$	  IF "''WNG_LIS'" .EQS. "" THEN -
		WNG_LIS="''CWD'"
$	  IF "''WNG_LINK'" .EQS. "" THEN -
		WNG_LINK="''CWD'"
$	  IF "''NSTAR_DIR'" .EQS. "" THEN -
		NSTAR_DIR="WNG,DWARF,NSCAN,NCOPY,NMAP,NPLOT"
$	  IF F$PARSE("''WNG_EXE'") .EQS. "" THEN -	! MAKE DIRECTORIES
		CREATE/DIR 'WNG_EXE'
$	  IF F$PARSE("''WNG_OLB'") .EQS. "" THEN -
		CREATE/DIR 'WNG_OLB'
$	  IF F$PARSE("''WNG_TLB'") .EQS. "" THEN -
		CREATE/DIR 'WNG_TLB'
$	  IF F$PARSE("''WNG_ERR'") .EQS. "" THEN -
		CREATE/DIR 'WNG_ERR'
$	  IF F$PARSE("''WNG_LIS'") .EQS. "" THEN -
		CREATE/DIR 'WNG_LIS'
$	  IF F$PARSE("''WNG_LINK'") .EQS. "" THEN -
		CREATE/DIR 'WNG_LINK'
$	  IF F$TRNLNM("WNG_TLD") .EQS. "" .AND. -	! NO LOGICAL DEFINITIONS
		F$SEARCH("WNG:NXLDEF.COM") .NES. "" THEN -
			@WNG:NXLDEF.COM			! GET LOGICAL LINKS
$	ENDIF
$	C_DIR="''NSTAR_DIR'"				!N DIRECTORIES
$	IF F$PARSE("[-.NCOPY]") .NES. "" .AND. -
		F$LOCATE("NCOPY",NSTAR_DIR) .EQ. F$LENGTH(NSTAR_DIR) THEN -
			C_DIR=C_DIR+",NCOPY"
$!#
$!# External environment
$!#
$	EXT="''WNG_TYPE'"				! MACHINE TYPE
$	LNK_DEF="WNG_OLBEXE:[NSCAN]WNLIB.OLB/LIB"+ -
		",WNG_OLBEXE:[NMAP]WNLIB.OLB/LIB"+ -
		",WNG_OLBEXE:[NPLOT]WNLIB.OLB/LIB"
$	IF F$SEARCH("WNG_OLBEXE:[WNG]GIPLIB.OLB") .NES. "" THEN -
		LNK_DEF=LNK_DEF+",WNG_OLBEXE:[WNG]GIPLIB.OLB"
$	LNK_DEF=LNK_DEF+ -
		",WNG_OLBEXE:[WNG]WNLIB.OLB/LIB"	!DEFAULT LIBRARIES
$	LNK_USE=""
$	IF F$TRNLNM("WNG_LDFILES") .NES. "" THEN -
		LNK_USE="''F$TRNLNM("WNG_LDFILES")'"
$	FORTRAN="FORTRAN"				!FORTRAN COMPILER
$	XFORT=""
$	LFORT="/F77/EXTEND/I4/SHOW=(NODICT,NOINCL,MAP,NOPREP,NOSIN)"+ -
		"/DEBUG/OPTIM"				!LINK FORTRAN
$	CEE=""						!C COMPILER
$	XCEE=""
$	ASSEM="MACRO"					!ASSEMBLER
$	XASSEM=""
$!#
$!# nxec environment
$!#
$	CHTP=".DIR,.ERR,.EXE,.HLB,.JOU,.LIS,.LOG,.LST,.MAP,.MLB,"+ -
		".NEW,.NPD,.OBJ,.OLB,.OLD,.PPD,.TLB,.TMP,"+ -
		".UDF,.ULB,"				!FILES TO SKIP
$	CODES="ABCDLOPSUXYZ"				!KNOWN CODES
$	CODEX="LU"					!EXTENDED CODES
$	QUAL ="BCFGIJLMOPV"				!KNOWN QUALIFIERS
$	QUALA="BCFIJLMOP"				!ADDITIVE QUALIFIERS
$	NCC_D="CDLOZ"					!DEFAULT CODES
$	NDC_D="ACLZ"
$	NGC_D="LZ"
$	NLC_D="SLZ"
$	NNC_D="Z"
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  L_D="WNLIB"					!DEFAULT LIBRARY
$	  U_D=" "					!DEFAULT DWARF UPDATE
$	  L0=0						!DEFINE ALL CODES
$ LP3:	    CD_'F$EXTRACT(L0,1,CODES)'="-"
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODES) THEN GOTO LP3
$	  L0=0						!DEFINE ALL QUALIFIERS
$ LP4:	    'F$EXTRACT(L0,1,QUAL)'Q_D=""
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(QUAL) THEN GOTO LP4
$	ELSE						!BATCH
$	  FG_CD=P5
$	  L0=0						!COPY CODES
$ LP5:	    CD_'F$EXTRACT(L0,1,CODES)'=F$EXTRACT(L0,1,FG_CD)
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODES) THEN GOTO LP5
$	  FG_CX="''P6'"
$	  L0=0						!EXTENDED CODES
$ LP6:	    'F$EXTRACT(L0,1,CODEX)'_D=F$ELEMENT(L0," ","''FG_CX'")
$	    IF 'F$EXTRACT(L0,1,CODEX)'_D .EQS. "-" THEN -
			'F$EXTRACT(L0,1,CODEX)'_D=""	!EMPTY
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODEX) THEN GOTO LP6
$	ENDIF
$!#
$!# Machine environment
$!#
$	DATTP=1
$	BQ_D="/NOLOG/NOPRINT/NOKEEP"			!BATCH DEFAULT
$	FQ_D="/F77/EXTEND/I4/SHOW=(NODICT,NOINCL,MAP,NOPREP,NOSIN)" !FORTRAN
$	CQ_D="/LIST/SHOW=(STAT,TRANS,SYM)/NOWARN"
$	IF INTAT .NES. "X0"				!RESET QUAL. FOR BATCH
$	THEN
$	  L0=0
$ LP7:	    'F$EXTRACT(L0,1,QUAL)'Q_D='F$EXTRACT(L0,1,QUAL)'Q_DX
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(QUAL) THEN GOTO LP7
$	ENDIF
$!# 
$!#  Get execution type
$!#
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  PNAM=""					!UNKNOWN
$	  L1=F$LENGTH(P1)				!LENGTH GIVEN
$	  IF L1 .LT. 2 THEN P1="Empty"			!ERROR IN TYPE
$	  L0=F$EDIT("''P1'","UPCASE")			!GIVEN TYPE
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NCOMPILE") THEN PNAM="NCOMP" !COMPILE
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NLINK") THEN PNAM="NLINK" !LINK
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NDELETE") THEN PNAM="NDEL" !DELETE
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NGET") THEN PNAM="NGET" !GET FROM LIBRARY
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NNET") THEN PNAM="NNET" !GET FROM NET
$	  IF  L0 .EQS. F$EXTRACT(0,L1,"NXREF") THEN PNAM="NXREF" !XREF
$	  IF PNAM .EQS. ""				!ERROR
$	  THEN
$	    @WNG:NHELP 1 "''P1'" "''EXT'"		!HELP TEXT
$	    GOTO EXEX
$	  ENDIF
$	  PCOD=F$EXTRACT(0,2,PNAM)			!PROGRAM CODE
$!# 
$!#  See if Help
$!# 
$ HLP:	  IF P2+P3+P4+P5+P6+P7+P8 .EQS. ""		!NO CODES
$	  THEN
$	    L0="-"+CODES+"Q("+QUAL+")"
$	    READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Codes (''L0') [?]: " SYS$COMMAND P2 !GET CODES
$	    IF P2 .EQS. "" .OR. P2-"?" .NES. P2
$	    THEN
$	      P2=""
$	      @WNG:NHELP 2 'PNAM'			!HELP
$	      GOTO HLP					!RETRY
$	    ENDIF
$	    P2="-"+P2					!MAKE SURE -
$	  ENDIF
$	ENDIF
$!#
$!# Read line
$!#
$	IF INTAT .EQS. "X2" THEN -			!OPEN FILE
		OPEN/READ/ERROR=EXEX FX2'PID''DEP' 'P7'
$ RLIN:	IF INTAT .EQS. "X2"				!READ LINE
$	THEN
$	  READ/ERROR=EXEX/END=EXEX FX2'PID''DEP' L1	!GET LINE
$	  IF F$EXTRACT(0,1,L1) .EQS. "#" THEN GOTO RLIN	!UNIX COMMAND
$	  IF F$EXTRACT(0,1,L1) .EQS. "$"
$	  THEN
$	    IF F$EXTRACT(0,2,L1) .EQS. "$$"		!CHECK IF CORRECT
$	    THEN
$	      IF F$EDIT(F$EXTRACT(0,5,L1),"UPCASE") .EQS. "$$''PCOD'$"
$	      THEN
$	        SPAWN/NOLOG 'F$EXTRACT(5,-1,L1)'	!DO COMMAND
$	      ENDIF
$	    ELSE
$	      SPAWN/NOLOG 'F$EXTRACT(1,-1,L1)'		!DO DCL COMMAND
$	    ENDIF
$	    GOTO RLIN
$	  ENDIF
$	  L1=F$EDIT(L1,"UPCASE,UNCOMMENT")		!GET FILE NAME
$	  L1=F$EDIT(L1,"TRIM")
$	  IF L1 .EQS. "" THEN GOTO RLIN			!EMPTY LINE
$	  L0=1						!SPLIT IN FIELDS
$ LP8:	    L0=L0+1
$	    P'L0'=F$ELEMENT(L0-2," ",L1)		!GET CODES ETC
$	    IF P'L0' .EQS. " " THEN P'L0'=""
$	    IF L0 .LT. 8 THEN GOTO LP8
$	ENDIF
$	IF INTAT .EQS. "X1"				!MAKE ARGUMENTS
$	THEN
$	  P2="''P7'"					!SET FILENAMES
$	  P3="''P8'"					!AND CODES
$	  L0=3						!SPLIT IN FIELDS
$ LP9:	    L0=L0+1
$	    P'L0'=""
$	    IF L0 .LT. 8 THEN GOTO LP9
$	ENDIF
$!#
$!# Get codes and filenames
$!#
$	A0=""						!NO CODES
$	FNAM=""						!NO NAMES
$	L0=1						!ARG. COUNT
$ LP10:	  L0=L0+1
$	  IF P'L0' .NES. ""				!NOT EMPTY
$	  THEN
$	    IF F$EXTRACT(0,1,P'L0') .EQS. "-" .OR. -	!CODE
		F$EXTRACT(0,1,P'L0') .EQS. "+" .OR. -
		F$EXTRACT(0,1,P'L0') .EQS. "<"
$	    THEN
$	      A0=A0+P'L0'				!SET CODE
$	    ELSE
$	      IF FNAM .NES. "" THEN FNAM=FNAM+","	!SET FILE NAME
$	      FNAM=FNAM+P'L0'
$	    ENDIF
$	  ENDIF
$ 	  IF L0 .LT. 8 THEN GOTO LP10			!MORE
$	IF INTAT .EQS. "X2" THEN FNAM=F$ELEMENT(0,",",FNAM) !LIMIT TO ONE NAME
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  A0="-"+'PCOD'C_D+"-"+F$TRNLNM("''PCOD'_COD")+"-"+A0 !ADD DEFAULTS
$	  UTELL "-----"
$	  UTELL "---- ''PNAM' ''A0' ''FNAM'"		!LOG WHAT
$	  UTELL "-----"
$	  GOSUB NXANAL					!ANALYZE CODES
$	ENDIF
$	IF INTAT .EQS. "X1" .AND. "''A0'" .NES. "" THEN - !ANALYZE EXTRA
		GOSUB NXANAL
$!#
$!#  Interpret codes
$!#
$	IF VQ_D .NES. "" .AND. -
		F$LOCATE("X",VQ_D) .EQ. F$LENGTH(VQ_D) .AND. -
		F$LOCATE("x",VQ_D) .EQ. F$LENGTH(VQ_D)
$	THEN
$	  VER=F$VERIFY(1)				!SET VERIFY
$	ELSE
$	  VER=F$VERIFY(0)				!NO VERIFY
$	ENDIF
$!#
$!# Network info
$!#
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  IF PNAM .EQS. "NNET"
$	  THEN
$	    IF CD_A .EQS. "0" .OR. "''WNG_NODE'" .EQS. "" !DO ASK
$	    THEN
$	      L0=""
$ LP20:	      IF "''L0'" .EQS. ""
$	      THEN
$	        READ/TIME=90/END=EXEX/ERROR=EXEX -
			/PROMPT="Node [''WNG_NODE']: " SYS$COMMAND L0
$	        IF "''L0'" .EQS. "" THEN L0="''WNG_NODE'"
$	        WNG_NODE="''L0'"
$	        GOTO LP20
$	      ENDIF
$	    ENDIF
$	    IF CD_A .EQS. "0" .OR. "''WNG_NODEDIR'" .EQS. "" !DO ASK
$	    THEN
$	      L0=""
$ LP21:	      IF "''L0'" .EQS. ""
$	      THEN
$	        READ/TIME=90/END=EXEX/ERROR=EXEX -
			/PROMPT="Remote base directory [''WNG_NODEDIR']: " -
			 SYS$COMMAND L0
$	        IF "''L0'" .EQS. "" THEN L0="''WNG_NODEDIR'"
$	        WNG_NODEDIR="''L0'"
$	        GOTO LP21
$	      ENDIF
$	    ENDIF
$	    IF CD_A .EQS. "0" .OR. "''WNG_NODEUSER'" .EQS. "" !DO ASK
$	    THEN
$	      L0=""
$ LP22:	      IF "''L0'" .EQS. ""
$	      THEN
$	        READ/TIME=90/END=EXEX/ERROR=EXEX -
			/PROMPT="Remote user [''WNG_NODEUSER']: " -
			 SYS$COMMAND L0
$	        IF "''L0'" .EQS. "" THEN L0="''WNG_NODEUSER'"
$	        WNG_NODEUSER="''L0'"
$	        GOTO LP22
$	      ENDIF
$	    ENDIF
$!#
$!#  Network password
$!#
$	    IF F$ELEMENT(1," ","''WNG_NODEUSER'") .EQS. " "
$	    THEN					!NO PASSWORD GIVEN
$	      SET TERM/NOECHO
$	      READ/TIME=90/END=EXEX/ERROR=EXEX -
			/PROMPT="Password: " SYS$COMMAND L0 !GET PASSWORD
$	      SET TERM/ECHO
$	      TELL " "
$	      WNG_NODEUSER="''WNG_NODEUSER' ''L0'"	!SET PASSWORD
$	    ENDIF
$	  ENDIF
$!#
$!# Compiler info
$!#
$	  IF CD_B .EQS. "1" THEN BQ_D=BQ_D+"/LOG=''PNAM'.LOG/PRINT" !BATCH LOG
$	  IF CD_D .NES. "-" THEN FQ_D=FQ_D+"/DEBUG/WARN=NOGEN" !DEBUG FORTRAN
$	  IF CD_D .NES. "-" THEN CQ_D=CQ_D+"/DEBUG"	!DEBUG C
$	  IF CD_D .NES. "-" THEN MQ_D=MQ_D+"/DEBUG"	!DEBUG MACRO
$	  IF CD_D .NES. "-" THEN LQ_D=LQ_D+"/DEBUG"	!LINK DEBUG
$	  IF CD_X .NES. "-" THEN FQ_D=FQ_D+"/CROSS"	!FORTRAN XREF
$	  IF CD_X .NES. "-" THEN CQ_D=CQ_D+"/CROSS"	!C XREF
$	  IF CD_X .NES. "-" THEN MQ_D=MQ_D+"/CROSS"	!MACRO XREF
$	  IF CD_X .NES. "-" THEN LQ_D=LQ_D+"/CROSS/FULL" !LINK XREF
$	  IF CD_O .NES. "-" THEN FQ_D=FQ_D+"/OPTIM"	!FORTRAN OPTIMIZE
$	  IF CD_O .EQS. "-" THEN FQ_D=FQ_D+"/NOOPTIM"	!FORTRAN NOOPTIMIZE
$	  IF CD_O .NES. "-" THEN CQ_D=CQ_D+"/OPTIM"	!C OPTIMIZE
$	  IF CD_O .EQS. "-" THEN CQ_D=CQ_D+"/NOOPTIM"	!C NOOPTIMIZE
$	ENDIF
$!#
$!#  Format qualifiers
$!#
$!#
$!#  Create libraries
$!#
$	IF CD_L .NES. "-"				!NEED LIBRARY
$	THEN
$	  IF F$SEARCH("''WNG_OLB'''L_D'.OLB") .EQS. "" THEN -
		LIBRARY/CREATE 'WNG_OLB''L_D'.OLB	!CREATE .OLB
$	  IF F$SEARCH("''WNG_TLB'''L_D'.TLB") .EQS. "" THEN -
		LIBRARY/CREATE/TEXT 'WNG_TLB''L_D'.TLB	!CREATE .TLB
$	  IF F$SEARCH("''WNG_TLB'''L_D'_AX.TLB") .EQS. "" THEN -
		LIBRARY/CREATE/TEXT 'WNG_TLB''L_D'_AX.TLB !CREATE .TLB
$	  IF F$SEARCH("''WNG_OLB'''L_D'.OLB") .EQS. "" .OR. -
		F$SEARCH("''WNG_TLB'''L_D'.TLB") .EQS. "" .OR. -
		F$SEARCH("''WNG_TLB'''L_D'_AX.TLB") .EQS. ""
$	  THEN
$	    TELL ""
$	    TELL "Illegal Library name. Probably illegal L<name> specified"
$	    UTELL "Illegal Library name. Probably illegal L<name> specified"
$	    TELL ""
$	    GOTO EXEX
$	  ENDIF
$	ENDIF
$!#
$!#  DWARF data
$!#
$	IF INTAT .EQS. "X0"				!INTERACTIVE
$	THEN
$	  IF CD_S .NES. "-" .AND. PCOD .EQS. "NL"
$	  THEN						!NEED DWARF
$	    IF F$TRNLNM("EXEDWARF") .EQS. "" .OR. -
		F$TRNLNM("LIBDWARF") .EQS. ""		!CANNOT DWARF
$	    THEN
$	      CD_S="-"					!NO SHARED DWARF
$	    ENDIF
$	  ENDIF
$!#
$!#  Get files if none
$!#
$ FIL:	  IF FNAM .EQS. ""				!NO FILENAMES
$	  THEN
$	    READ/TIME=90/END=EXEX/ERROR=EXEX -
		/PROMPT="Filename[,...] [?]: " SYS$COMMAND FNAM !GET NAMES
$	    IF FNAM .EQS. ""
$	    THEN
$	      @WNG:NHELP 3 'PNAM'			!SHOW HELP
$	      GOTO FIL
$	    ENDIF
$	  ENDIF
$	ENDIF
$!#
$!#  Do batch if asked
$!#
$	IF INTAT .EQS. "X0"				!PREPARE BATCH
$	THEN
$	  FG_CD=""					!PREPARE CODES
$	  FG_CX=""
$	  L0=0						!DEFINE ALL CODES
$ LP11:	    FG_CD=FG_CD+CD_'F$EXTRACT(L0,1,CODES)'
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODES) THEN GOTO LP11
$	  L0=0						!EXTENDED CODES
$ LP12:	    L1='F$EXTRACT(L0,1,CODEX)'_D
$	    IF L1 .EQS. "" THEN L1="-"
$	    IF FG_CX .NES. "" THEN FG_CX=FG_CX+" "
$	    FG_CX=FG_CX+L1
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODEX) THEN GOTO LP12
$	  L0=0						!EXPORT QUALIFIERS
$ LP13:	    'F$EXTRACT(L0,1,QUAL)'Q_DX='F$EXTRACT(L0,1,QUAL)'Q_D
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(QUAL) THEN GOTO LP13
$	  IF CD_B .NES. "-"				!BATCH ASKED
$	  THEN
$	    IF CD_B .EQS. "2"				!SPAWN
$	    THEN
$	      SPAWN/NOWAIT/OUTPUT=SPAWN.LOG -
		 @WNG:NXEC X1 'PNAM' 'PCOD' 'DEP' "''FG_CD'" "''FG_CX'" -
			"''FNAM'"			!SPAWN
$	      IF .NOT.$SEVERITY
$	      THEN
$	        TELL ""
$	        TELL "Cannot spawn the ''PNAM' task"
$	        UTELL "Cannot spawn the ''PNAM' task"
$	        TELL ""
$	      ENDIF
$	    ELSE
$	      L0=""
$	      CLOSE/ERROR=GB21 F'PID''DEP'		!BATCH
$ GB21:	      OPEN/WRITE/ERROR=GB22 F'PID''DEP' 'PNAM''PID''DEP'.TMP
$	      WRITE/ERROR=GB22 F'PID''DEP' "$ !'F$VERIFY(''F$VERIFY()')'"
$	      WRITE/ERROR=GB22 F'PID''DEP' -
			"$ SET DEFAULT ''F$ENVIRONMENT("DEFAULT")'" !SET
$	      L00=0					!SAVE QUALIFIERS
$ LP16:	        L01='F$EXTRACT(L00,1,QUAL)'Q_DX		!QUAL.
$	        WRITE/ERROR=GB22 F'PID''DEP'' -
			"$ ''F$EXTRACT(L00,1,QUAL)'Q_DX="+ -
			"""''L01'"""
$	        L00=L00+1
$	        IF L00 .LT. F$LENGTH(QUAL) THEN GOTO LP16
$	      WRITE/ERROR=GB22 F'PID''DEP' "$ @WNG:NXEC X1 ''PNAM' "+ -
			"''PCOD' ''DEP' ""''FG_CD'"" ""''FG_CX'"" "+ -
			"""''FNAM'"""			!DO
$	      CLOSE/ERROR=GB22 F'PID''DEP'
$	      L0="''PNAM'''PID'''DEP'.TMP"
$	      L0=F$SEARCH("''L0'")
$	      IF L0 .NES. "" THEN SUBMIT'BQ_D' 'L0'/DELETE !SUBMIT
$	      L1=$SEVERITY
$	      IF L1 .AND. L0 .NES. "" THEN GOTO EXEX	!SUBMITTED
$ GB22:	      IF L0 .NES. "" THEN DELETE 'L0'		!DELETE
$	      TELL ""
$	      TELL "Cannot submit the ''PNAM' task"
$	      UTELL "Cannot submit the ''PNAM' task"
$	      TELL ""
$	    ENDIF
$	    GOTO EXEX
$	  ENDIF
$	ENDIF
$	IF INTAT .EQS. "X2" .AND. "''A0'" .NES. ""	!LOCAL SWITCHES
$	THEN
$	  @WNG:NXEC X1 'PNAM' 'PCOD' 'DEP' "''FG_CD'" "''FG_CX'" -
			"''FNAM'" "''A0'"		!DO
$	  FNAM=""					!SET DONE
$	ENDIF
$!#
$!#  Execute
$!#
$	IF VQ_D .NES. "" .AND. -
		(F$LOCATE("X",VQ_D) .LT. F$LENGTH(VQ_D) .OR. -
		F$LOCATE("x",VQ_D) .LT. F$LENGTH(VQ_D))
$	THEN
$	  VER=F$VERIFY(1)				!SET VERIFY
$	ELSE
$	  VER=F$VERIFY(0)				!NO VERIFY
$	ENDIF
$	IF INTAT .NES. "X0" .AND. CD_Y .EQS. "-"	!BATCH EXECUTION
$	THEN
$	  B_AA="-"					!PREPARE OUTPUT
$	  B_AB="-"
$	  B_AC="-"
$	  L0=0						!SET QUALIFIERS
$ LP14:	    B_AA=B_AA+"Q"+F$EXTRACT(L0,1,QUAL)+"<"+ -
		'F$EXTRACT(L0,1,QUAL)'Q_D+">"
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(QUAL) THEN GOTO LP14
$	  L0=0						!SET CODES
$ LP15:	    L1=F$EXTRACT(L0,1,CODES)
$	    IF CD_'L1' .NES. "-"			!CODE SET
$	    THEN
$	      B_AB=B_AB+L1+CD_'L1'			!SET CODE
$	      IF CODEX-L1 .NES. CODEX			!EXTENDED
$	      THEN
$	        B_AC=B_AC+L1+CD_'L1'+"<"+'L1'_D+">"
$	      ENDIF
$	    ENDIF
$	    L0=L0+1
$	    IF L0 .LT. F$LENGTH(CODES) THEN GOTO LP15
$	  TELL " "
$	  TELL "Command: ''PNAM' ''B_AB'"
$	  TELL "         ''B_AC'"
$	  TELL "         ''B_AA'"
$	  TELL "         ''FNAM'"
$	  TELL " "
$	ENDIF
$!#
$!#  Do all files
$!#
$	L0=-1						!COUNT FILES
$ EXE1:	L0=L0+1						!NEXT FILE
$	L1=""						!NOT INDIRECT
$	LOB=F$ELEMENT(L0,",",FNAM)			!GET NAME
$	IF LOB .EQS. "" THEN GOTO EXE1			!EMPTY, NEXT
$	IF LOB .EQS. ","				!READY
$	THEN
$	  IF INTAT .EQS. "X2" THEN GOTO RLIN		!READ NEXT LINE
$	  GOTO EXEX					!REAL READY
$	ENDIF
$	IF F$EXTRACT(0,1,LOB) .EQS. "@"			!INDIRECT
$	THEN
$	  LOB=LOB-"@"					!DELETE @
$	  L1="@"					!SET INDIRECT
$	  LOB=F$PARSE(LOB,".GRP;0",,,"SYNTAX_ONLY")	!GET FULL FILE NAME
$	ELSE
$	  IF PCOD .EQS. "NL"
$	  THEN
$	    LOB=F$PARSE(LOB,".EXE;0",,,"SYNTAX_ONLY")	!GET FULL FILE NAME
$	  ELSE
$	    LOB=F$PARSE(LOB,".*;0",,,"SYNTAX_ONLY")	!GET FULL FILE NAME
$	  ENDIF
$	ENDIF
$	IF LOB .EQS. "" THEN GOTO EXE1			!FORMAT ERROR
$	L5=""						!CHECK SINGLE FILE
$	L6="N"						!CHECK SEEN
$ EXE4:	L3=F$SEARCH(LOB,L0)				!FIND FILE
$ EXE5:	IF L3 .EQS. "" .OR. -
		F$PARSE(L3,,,"NAME","SYNTAX_ONLY")+ -
			F$PARSE(L3,,,"TYPE","SYNTAX_ONLY") .EQS. -
		F$PARSE(L5,,,"NAME","SYNTAX_ONLY")+ -
			F$PARSE(L5,,,"TYPE","SYNTAX_ONLY") .OR. -
		(PCOD .EQS. "NG" .AND. LOB-"*"-"%" .NES. LOB) !NOT FOUND
$	THEN
$	  IF L6 .OR. L1 .NES. "" THEN GOTO EXE1		!BUT ONE DONE
$	  IF PCOD .EQS. "ND" .OR. PCOD .EQS. "NN" .OR. PCOD .EQS. "NC"
$	  THEN
$	    IF LOB-"*"-"%" .NES. LOB THEN GOTO EXE1	!NONE DEFINED
$	    L3=LOB					!TRY ANYWAY
$	  ENDIF
$	  IF PCOD .EQS. "NG"				!NGET
$	  THEN
$	    IF LOB-"*"-"%" .NES. LOB			!GET LIST FIRST
$	    THEN
$	      L00=F$PARSE(LOB,,,"NAME","SYNTAX_ONLY")+ -
			F$PARSE(LOB,,,"TYPE","SYNTAX_ONLY")
$	      SET MESSAGE /NOIDEN/NOFACIL/NOSEVER/NOTEXT
$	      ASSIGN/USER NL: SYS$OUTPUT
$	      LIBRARY/TEXT/LIST=L'PID''DEP'.TMP/ONLY=('L00') 'WNG_TLB''L_D' !LIST
$	      L00=$SEVERITY				!FOR CHECK
$	      SET MESSAGE /IDEN/FACIL/SEVER/TEXT
$	      IF .NOT.L00 .OR. F$SEARCH("L''PID'''DEP'.TMP") .EQS. "" !ERROR
$	      THEN
$	        IF F$SEARCH("L''PID'''DEP'.TMP") .NES. "" THEN -
$			DELETE L'PID''DEP'.TMP;*
$	      ELSE
$	        @WNG:NXEC X2 'PNAM' 'PCOD' 'DEP' "''L00'" "''FG_CX'" -
			"L''PID'''DEP'.TMP"		!DO AS LIST
$	      ENDIF
$	      GOTO EXE1					!CONTINUE
$	    ELSE
$	      L3=LOB					!DO ONE
$	    ENDIF
$	  ENDIF
$	  IF PCOD .EQS. "NL"				!NLINK
$	  THEN
$	    IF	F$PARSE(LOB,,,"TYPE","SYNTAX_ONLY") .EQS. ".EXE" .OR. -
		F$PARSE(LOB,,,"TYPE","SYNTAX_ONLY") .EQS. "."
$	    THEN
$	      L3=F$PARSE(LOB,,,"NAME","SYNTAX_ONLY")+".EXE" !LINK AT LEAST ONE
$	    ELSE
$	      GOTO EXE1					!NONE
$	    ENDIF
$	  ENDIF
$	  GOSUB NX1					!DO A FILE
$	  IF F$SEARCH("L''PID'''DEP'.TMP") .NES. "" THEN -
		DELETE/NOLOG L'PID''DEP'.TMP;*		!DELETE FOR NGET
$	  GOTO EXE1					!NEXT FILE SPEC.
$	ENDIF
$	L5=L3						!CHECK
$	L4=L1
$	IF L1 .EQS. "" .AND. F$PARSE(L3,,,"TYPE","SYNTAX_ONLY") .EQS. ".GRP" -
		.AND. CD_Z .NES. "" THEN L4="@"		!INDIR
$	IF PCOD .EQS. "NL" .AND. L4 .EQS. "" .AND. -
		F$PARSE(L3,,,"TYPE","SYNTAX_ONLY") .NES. ".EXE" -
		THEN GOTO EXE4				!SKIP FOR NLINK
$	IF PCOD .EQS. "NG" .AND. L4 .EQS. "" .AND. -
		LOB-"*"-"%" .NES. LOB			!GET LIST FIRST
$	THEN
$	  L3=""						!FORCE LIST
$	  GOTO EXE5					!DO LIST
$	ENDIF
$	L3="''L4'''L3'"					!MAKE FULL NAME
$	GOSUB NX1					!DO: TYPE, FILE
$	L6="Y"						!AT LEAST ONE DONE
$	GOTO EXE4					!NEXT FILE
$!#
$!# Routines
$!#
$ !
$ ! Analyze codes
$ !
$ NXANAL:
$ GC1A:	L1="Y"						!NO N SEEN
$ GC14:	IF A0 .EQS. "" THEN RETURN			!READY
$	L2=F$EXTRACT(0,1,A0)				!CODE
$	IF L2 .EQS. "<" THEN GOSUB GA0			!SKIP <>
$	IF L2 .EQS. "<" THEN GOTO GC10			!CONTINUE
$	A0=A0-L2					!DELETE CODE
$	IF L2 .EQS. "-" THEN GOTO GC1A			!SKIP -
$	L2=F$EDIT(L2,"UPCASE")
$	IF L2 .EQS. "N" .OR. L2 .EQS. "+"
$	THEN						!NEGATE
$	  L1="N"
$	  GOTO GC14
$	ENDIF
$	IF L2 .EQS. "Q" .AND. L1			!QUALIFIER
$	THEN
$	  L2=F$EXTRACT(0,1,A0)				!QUALIFIER
$	  A0=A0-L2
$	  L2=F$EDIT(L2,"UPCASE")
$	  IF QUAL-L2 .EQS. QUAL THEN GOTO GC1A		!UNKNOWN QUALIFIER
$	  GOSUB GA0					!GET QUAL. ARGUMENT
$	  IF QUALA-L2 .EQS. QUALA .AND. A1.NES. ""
$	  THEN
$	    'L2'Q_D="''A1'"				!NOT ADDITIVE
$	  ELSE
$	    'L2'Q_D='L2'Q_D+A1				!ADD QUALIFIER DATA
$	  ENDIF
$	  GOTO GC1A					!CONTINUE
$	ENDIF
$	IF CODES-L2 .EQS. CODES THEN GOTO GC1A		!UNKNOWN CODE
$	IF L1
$	THEN
$	  CD_'L2'="0"					!SET STANDARD CODE
$	ELSE
$	  CD_'L2'="-"					!SET NO
$	  GOTO GC1A
$	ENDIF
$	L0=F$EXTRACT(0,1,A0)				!POSSIBLE DIGIT
$	IF "0123456789"-L0 .NES. "0123456789"
$	THEN						!DIGIT
$	  CD_'L2'=L0					!SET DIGIT
$	  A0=A0-L0					!DELETE DIGIT FROM CODE
$	ENDIF
$	IF CODEX-L2 .EQS. CODEX THEN GOTO GC1A		!NOT EXTENDED CODE
$	GOSUB GA0					!GET EXTENDED ARG.
$	IF A1 .NES. "" THEN 'L2'_D=A1			!NEW SETTING
$	GOTO GC1A					!CONTINUE
$ !
$ ! Get <string>
$ !
$ GA0:	A1=""						!EMPTY RESULT
$	IF F$LENGTH("''A0'") .LT. 1 THEN RETURN		!NO VALUE
$	IF F$EXTRACT(0,1,A0) .EQS. " " THEN GOTO GA0	!SKIP INIT. SPACE
$	IF F$EXTRACT(0,1,A0) .NES. "<" THEN RETURN	!NO VALUE
$	L00=0						!COUNT <>
$	L01=0						!COUNT "
$ GA06:	IF A0.EQS. ""					!ERROR
$	THEN
$	  A1=""
$	  RETURN
$	ENDIF
$	L02=F$EXTRACT(0,1,A0)				!CHAR.
$	A0=A0-L02					!SKIP CHAR
$	IF L01 .NE. 0					!STRING
$	THEN
$	  IF "''L02'" .EQS. """" THEN L01=0		!RESET STRING
$	ELSE
$	  IF "''L02'" .EQS. "<"				!<
$	  THEN
$	    L00=L00+1					!COUNT <
$	    IF L00 .EQ. 1 THEN GOTO GA06		!NO SAVE
$	  ELSE
$	    IF "''L02'" .EQS. ">"
$	    THEN
$	      L00=L00-1					!COUNT >
$	      IF L00 .EQ 0 THEN RETURN			!READY
$	    ELSE
$	      IF "''L02'" .EQS. """" THEN L01=1		!INDICATE "
$	    ENDIF
$	  ENDIF
$	ENDIF
$	A1=A1+L02					!SAVE CHAR
$	GOTO GA06					!CONTINUE
$ !
$ ! NX1	Do a file.
$ !
$ NX1:	IF F$EXTRACT(0,1,L3) .EQS. "@" .AND. CD_Z .NES. "-" !IND.
$	THEN
$	  IF L3-"@" .EQS. GQ_DX THEN L3=L3-"@"		!STOP INFINITE LOOP
$	ENDIF
$	IF F$EXTRACT(0,1,L3) .EQS. "@" .AND. CD_Z .NES. "-" !IND.
$	THEN
$	  L3=L3-"@"					!FILE NAME
$	  IF F$SEARCH(L3) .EQS. ""			!NOT THERE
$	  THEN
$	    IF PCOD .NES. "NC" THEN RETURN		!CANNOT DO
$	    FNM=F$PARSE(L3,,,"NAME","SYNTAX_ONLY")	!NAME
$	    FTP=F$PARSE(L3,,,"TYPE","SYNTAX_ONLY")	!TYPE
$	    B0=15-F$LENGTH(FNM)-F$LENGTH(FTP)		!MESSAGE POINTER
$	    IF B0.LT.1 THEN B0=1
$	    MSGT=F$EXTRACT(0,B0,"                    ")	!TEXT
$	    B1="Done: "					!ASSUME OK
$	    @WNG:NGET "''L3'"				!TRY TO GET
$	    IF F$SEARCH(L3) .EQS. "" THEN RETURN	!NOT FOUND
$	  ENDIF
$	  L00=""					!ADD Y
$	  L01=0
$ LP17:	    IF F$EXTRACT(L01,1,CODES) .EQS. "Y"
$	    THEN
$	      L00=L00+"0"
$	    ELSE
$	      L00=L00+CD_'F$EXTRACT(L01,1,CODES)'
$	    ENDIF
$	    L01=L01+1
$	    IF L01 .LT. F$LENGTH(CODES) THEN GOTO LP17
$	  GQ_DX="''L3'"					!SAVE CURRENT NAME
$	  @WNG:NXEC X2 'PNAM' 'PCOD' 'DEP' "''L00'" "''FG_CX'" -
			"''L3'"				!DO .GRP
$	ELSE
$	  L3=L3-"@"					!SURE FILE NAME
$	  FNM=F$PARSE(L3,,,"NAME","SYNTAX_ONLY")	!NAME
$	  FTP=F$PARSE(L3,,,"TYPE","SYNTAX_ONLY")	!TYPE
$	  IF PCOD .NES. "NL"				!NO CHECK FOR NLINK
$	  THEN
$	    IF F$LENGTH(FTP).NE.4 THEN RETURN		!SKIP IF NOT 3 CHAR EXT.
$	    IF F$LOCATE(FTP+",",CHTP) .LT. F$LENGTH(CHTP) THEN RETURN !FORGET
$	    IF PQ_D .NES. ""				!PURE GIVEN
$	    THEN
$	      PQ_D=F$EDIT(PQ_D,"UPCASE")
$	      IF F$LOCATE(FTP-".",PQ_D) .EQ. F$LENGTH(PQ_D) THEN RETURN !NOT
$	    ENDIF
$	    B0=15-F$LENGTH(FNM)-F$LENGTH(FTP)		!MESSAGE POINTER
$	  ELSE
$	    B0=10-F$LENGTH(FNM)-F$LENGTH(FTP)		!MESSAGE POINTER
$	  ENDIF
$	  IF B0.LT.1 THEN B0=1
$	  IF PCOD .EQS. "NC"				!MAYBE NGET FIRST
$	  THEN
$	    MSGT=F$EXTRACT(0,B0,"                    ")	!TEXT
$	    B1="Done: "					!ASSUME OK
$	    IF F$SEARCH(L3) .EQS. "" THEN -		!NGET FIRST
		@WNG:NGET "''L3'"
$	    B0=15-F$LENGTH(FNM)-F$LENGTH(FTP)		!MESSAGE POINTER
$	  ENDIF
$	  IF B0.LT.1 THEN B0=1
$	  MSGT=F$EXTRACT(0,B0,"                    ")	!TEXT
$	  B1="Done: "					!ASSUME OK
$	  @WNG:'PNAM' "''L3'"				!DO
$	ENDIF
$	RETURN						!OK
$!#
$!#  EXIT
$!#
$ EXEX:	SET ON
$	IF INTAT .EQS. "X0" THEN SET TERM/ECHO		!MAKE SURE
$	CLOSE/ERROR=EXX1 F'PID''DEP'			!MAKE SURE
$ EXX1:	CLOSE/ERROR=EXX2 FX2'PID''DEP'			!MAKE SURE
$ EXX2:	CLOSE/ERROR=EXX3 UP'PID''DEP'
$ EXX3:	IF F$SEARCH("L''PID'''DEP'.TMP") .NES. "" THEN -
		DELETE L'PID''DEP'.TMP;*
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
