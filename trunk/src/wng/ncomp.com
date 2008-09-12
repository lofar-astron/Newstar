$!#  ncomp.ssc
$!# 	WNB 920908
$!# 
$!#  Revisions:
$!#	WNB 920917	Make shells executable
$!#	WNB 920922	Add l switch to ar
$!#			Add .fun type
$!#	WNB 921002	Overhaul
$!#	JPH 921009	Make $WNG_LINK work. - Remove all target files for .dsc
$!#			 and .for compilations
$!#	WNB 921012	Some typo's
$!#	WNB 921016	Wrong nxup called
$!#	WNB 921019	Add copy option
$!#	WNB 921021	Suppress listing message; error for .cee
$!#	WNB 921104	Error in ppd update
$!#	WNB 921113	Postpone ar
$!#			Change rm for ppd
$!#			Remove empty ngen.ppd, global.ppd
$!#	WNB 921113	Use newest wntab
$!#	WNB 921113	Correct back
$!#	WNB 921122	.err output voor .pin; delete .uin
$!#	WNB 921130	Change tr for HP
$!#	WNB 921202	Include .pef
$!#	WNB 921204	Cater for long HP tlbset
$!#	WNB 921208	Change update and log
$!#	WNB 921208	Change .def; include dwarf/.inc; create nxldef.sun
$!#	WNB 921209	Add -a1, -a2, -a4, -a8
$!#	WNB 921211	Add .PSC
$!#	WNB 921215	Typo
$!#	WNB 921215	Add FSC, CSC
$!#	WNB 921218	Add SSC; site wn_...__
$!#	HjV 921221	Delete -f chmod; typo goto ncERR; changed rm ?*
$!#	WNB 921222	Include nonomatch; typo SSC; streamline psc ssc
$!#	WNB 921230	Shorter expressions for Alliant; cater NXFOR.SSC
$!#	WNB 921230	Make SSC; add some messages
$!#       JPH 930224      Prefix ${cwd}/ to file name in ln -s commands
$!#	WNB 930303	Change to SYS_BLD (VAX)
$!#                       Copy NXLDEF to shadow directory at NCDF.
$!#	WNB 930308	Forgot to delete shadowtest error
$!#	WNB 930330	Add .A.. and .X..; wn_gipsy__, wn_pgplot__
$!#	WNB 930402	Make logical link from .inc to .h locally only
$!#	WNB 930413	Typo NCEXE label
$!#	WNB 930517	Remove pgplot; put objects in WNG_OLB
$!#	WNB 930802	Change WNTAB into WNTINC
$!#	WNB 930803	Add .dsf
$!#
$!#			Note: This file contains a series of sed commands. By
$!#				transferring by mail some characters can be
$!#				lost. Make especially sure about the [].
$!#				All "empty" ones contain <space><tab>, i.e.
$!#				[ 	]
$!#
$!# 	Compile routines in nxec system. Use as:
$!#
$!#		source $WNG/ncomp.sun		(Unix)
$!#		@WNG:NCOMP <file>		(VAX)
$!#
$!#	This file uses many local variables set in nxec, and the
$!#	environment variables:
$!#		WNG, EXEDWARF_UNIX, WNG_OLB, WNG_OLBEXE, WNG_LIS, WNG_ERR
$!#	and command files nxup, wngfex,
$!#	and programs wntinc, sys_bldppd
$!#
$!#  Compile a file.
$!#
$	ON ERROR THEN GOTO ERR
$	IF F$SEARCH(P1) .EQS. "" THEN GOTO ERR		!CANNOT DO
$	IF F$PARSE(P1,,,"DEVICE","SYNTAX_ONLY")+ -
		F$PARSE(P1,,,"DIRECTORY","SYNTAX_ONLY") .NES. -
		F$ENVIRONMENT("DEFAULT")		!NOT IN CURRENT DIR.
$	THEN
$	  COPY 'F$EXTRACT(0,F$LOCATE(";",P1),P1)' []	!COPY FILE
$	  IF F$SEARCH("''FNM'''FTP'") .EQS. "" THEN GOTO ERR
$	  MSGT=MSGT+ " copied"
$	ENDIF
$	PURGE/KEEP=2 'FNM''FTP'				!PURGE AS YOU GO
$!#
$!#  Change files (-An switch)
$!#
$	IF CD_A .NES. "-" .AND. CD_A .GT. 0		!SWITCH GIVEN
$	THEN
$	  IF F$LENGTH(FTP) .EQ. 4 .AND. -
		(F$EXTRACT(0,2,FTP) .EQS. ".F" .OR. -
		 FTP .EQS. ".DEF" .OR. (FTP .EQS. ".INC" .AND. -
		  CWDT .EQS. "[DWARF]"))		!FORTRAN
$	  THEN
$	    IF F$SEARCH("''FNM'.TMP") .NES. "" THEN -
			DELETE 'FNM'.TMP;*		!MAKE SURE
$	    CLOSE/ERROR=LB21 AF'PID''DEP'		!OPEN INPUT
$ LB21:	    CLOSE/ERROR=LB20 AT'PID''DEP'		!OPEN OUTPUT
$ LB20:	    OPEN/ERROR=ERR/READ AF'PID''DEP' 'FNM''FTP'
$	    OPEN/ERROR=ERR/WRITE AT'PID''DEP' 'FNM'.TMP
$ LP20:	      READ/ERROR=ERR/END=LP21 AF'PID''DEP' L0	!READ LINE
$	      IF F$EXTRACT(0,1,L0) .NES. " " .AND. -
			F$EXTRACT(0,1,L0) .NES. "	" THEN GOTO LP22 !NO
$	      L1=F$EDIT(L0,"UNCOMMENT,UPCASE,COLLAPSE")	!FOR EASY SEARCH
$	      IF F$EXTRACT(0,8,L1) .NES. "INCLUDE'" THEN GOTO LP23 !NOT INCLUDE
$	      IF 2*(CD_A/2)-CD_A .NE. 0			!LOOK FOR ()
$	      THEN
$	        IF F$EXTRACT(8,1,L1) .EQS. "("		!FOUND
$	        THEN
$		  L2=F$LOCATE(")",L0)
$		  L0=F$EDIT(F$EXTRACT(0,L2,L0),"UPCASE")+ -
			F$EXTRACT(L2,-1,L0)-"("-")"	!MAKE NEW
$	          L1=F$EDIT(L0,"UNCOMMENT,UPCASE,COLLAPSE") !FOR EASY SEARCH
$		ENDIF
$	      ENDIF
$	      IF CD_A-(4*(CD_A/4)) .GT. 1		!CHANGE DEF
$	      THEN
$		IF F$LOCATE(":",F$EXTRACT(8,-1,L1)) .LT. -
			F$LOCATE("'",F$EXTRACT(8,-1,L1)) !FOUND
$		THEN
$		  L2=F$LOCATE(":",L0)+1
$		  L3=F$LOCATE("'",L0)+1
$		  L4=F$LOCATE("'",F$EXTRACT(L2,-1,L0))
$		  L0=F$EDIT(F$EXTRACT(0,L3,L0),"UPCASE")+ -
			F$EDIT(F$EXTRACT(L2,L4,L0),"UPCASE")+ -
			F$EXTRACT(L2+L4,-1,L0)
$	          L1=F$EDIT(L0,"UNCOMMENT,UPCASE,COLLAPSE") !FOR EASY SEARCH
$		ENDIF
$		IF F$LOCATE(".DEF'",F$EXTRACT(8,-1,L1)) .LT. -
			F$LOCATE("'",F$EXTRACT(8,-1,L1)) !FOUND .
$		THEN
$		  L2=F$LOCATE(".",L0)
$		  L3=F$LOCATE("'",F$EXTRACT(L2,-1,L0))
$		  L0=F$EDIT(F$EXTRACT(0,L2,L0)+"_"+ -
			F$EXTRACT(L2+1,L3,L0),"UPCASE")+ -
			F$EXTRACT(L2+L3+1,-1,L0)
$	        ENDIF
$	      ENDIF
$	      GOTO LP22					!READY INCLUDE
$ LP23:	      IF CD_A-(8*(CD_A/8)) .GT. 3		!CHANGE *4 *8
$	      THEN
$		IF F$EXTRACT(0,6,L1) .EQS. "REAL*4" .OR. -
			F$EXTRACT(0,9,L1) .EQS. "INTEGER*4"
$		THEN
$		  L2=F$LOCATE("*",L0)
$		  L3=F$LOCATE("4",L0)
$		  L0=F$EXTRACT(0,L2,L0)+F$EXTRACT(L3+1,-1,L0)
$		ENDIF
$		IF F$EXTRACT(0,6,L1) .EQS. "REAL*8"
$		THEN
$		  L2=F$LOCATE("R",L0)
$		  IF F$LOCATE("r",L0) .LT. L2 THEN L2=F$LOCATE("r",L0)
$		  L3=F$LOCATE("8",L0)
$		  L0=F$EXTRACT(0,L2,L0)+"DOUBLE PRECISION"+ -
			F$EXTRACT(L3+1,-1,L0)
$		ENDIF
$		IF F$EXTRACT(0,9,L1) .EQS. "LOGICAL*1
$		THEN
$		  L2=F$LOCATE("L",L0)
$		  IF F$LOCATE("l",L0) .LT. L2 THEN L2=F$LOCATE("l",L0)
$		  L3=F$LOCATE("1",L0)
$		  L0=F$EXTRACT(0,L2,L0)+"BYTE"+ -
			F$EXTRACT(L3+1,-1,L0)
$		ENDIF
$	      ENDIF
$ LP22:	      WRITE/ERROR=ERR AT'PID''DEP' L0		!COPY
$	      GOTO LP20					!NEXT
$ LP21:	    CLOSE/ERROR=ERR AT'PID''DEP'		!CLOSE OUTPUT
$	    CLOSE/ERROR=ERR AF'PID''DEP'		!CLOSE INPUT
$	    COPY 'FNM'.TMP 'FNM''FTP'			!NEW VERSION
$	    DELETE 'FNM'.TMP;*
$	  ENDIF
$	  IF F$LENGTH(FTP) .EQ. 4 .AND. -
		((F$EXTRACT(0,2,FTP) .EQS. ".C" .AND. -
		 FTP .NES. ".COM") .OR. -
		 (FTP .EQS. ".INC" .AND. CWDT .NES. "[DWARF]")) .AND. -
		CD_A-(4*(CD_A/4)) .GT. 1		!C
$	  THEN
$	    IF F$SEARCH("''FNM'.TMP") .NES. "" THEN -
			DELETE 'FNM'.TMP;*		!MAKE SURE
$	    CLOSE/ERROR=LB31 AF'PID''DEP'		!OPEN INPUT
$ LB31:	    CLOSE/ERROR=LB30 AT'PID''DEP'		!OPEN OUTPUT
$ LB30:	    OPEN/ERROR=ERR/READ AF'PID''DEP' 'FNM''FTP'
$	    OPEN/ERROR=ERR/WRITE AT'PID''DEP' 'FNM'.TMP
$ LP30:	      READ/ERROR=ERR/END=LP31 AF'PID''DEP' L0	!READ LINE
$	      L1=F$EDIT(L0,"UNCOMMENT,COLLAPSE")	!FOR EASY SEARCH
$	      IF F$EXTRACT(0,9,L1) .EQS. "#include""" .AND. -
			F$LOCATE(".inc""",L1) .LT. F$LENGTH(L1) !DO .INC
$	      THEN
$		L2=F$LOCATE(".inc",L0)
$		L0=F$EXTRACT(0,L2,L0)+"_"+F$EXTRACT(L2+1,-1,L0)
$	      ENDIF
$	      WRITE/ERROR=ERR AT'PID''DEP' L0		!COPY
$	      GOTO LP30					!NEXT
$ LP31:	    CLOSE/ERROR=ERR AT'PID''DEP'		!CLOSE OUTPUT
$	    CLOSE/ERROR=ERR AF'PID''DEP'		!CLOSE INPUT
$	    COPY 'FNM'.TMP 'FNM''FTP'			!NEW VERSION
$	    DELETE 'FNM'.TMP;*
$	  ENDIF
$	ENDIF
$!#
$!# Compile
$!#
$	IF CD_C .NES. "-"				!COMPILE
$	THEN
$	  IF IQ_D .NES. "" THEN IQ_D=IQ_D+"+"		!INCLUDE FILES
$	  IF JQ_D .NES. "" THEN JQ_D=JQ_D+"+"		!INCLUDE FILES
$	  IF FTP .EQS. ".FSC" THEN GOSUB FSC		!FSC
$	  IF FTP .EQS. ".FOR" .OR. FTP .EQS. ".FVX" THEN GOSUB FOR !FORTRAN
$	  IF FTP .EQS. ".CSC" THEN GOSUB CSC		!CSC
$	  IF FTP .EQS. ".MVX" THEN GOSUB MAC		!MACRO
$	  IF FTP .EQS. ".SSC" THEN GOSUB SSC		!SSC
$	  IF FTP .EQS. ".HLP" THEN GOSUB HLP		!HELP
$	  IF FTP .EQS. ".DSC" THEN GOSUB DSC		!DSC
$	  IF FTP .EQS. ".DEF" .OR. FTP .EQS. ".PEF" .OR. -
		FTP .EQS. ".DSF" THEN GOSUB DEF		! DEF/PEF/DSF
$	  IF FTP .EQS. ".INC" THEN GOSUB DEF		!INC
$	  IF FTP .EQS. ".PSC" THEN GOSUB PSC		!PSC
$	  IF FTP .EQS. ".PIN" THEN GOSUB PIN		!PIN
$	  IF FTP .EQS. ".AVX" THEN GOSUB ALB		!SPECIAL OLB
$	  IF FTP .EQS. ".XVX" THEN GOSUB XEX		!SPECIAL EXE
$	ENDIF
$	GOSUB OTH					!ALL OTHERS
$	GOTO ERR1
$!#
$!#  Others
$!#
$ OTH:	IF CD_P .NES. "-" .AND. MSGT-"printed" .EQS. MSGT .AND. -
		F$EXTRACT(1,1,FTP) .NES. "A" .AND. -
		F$EXTRACT(1,1,FTP) .NES. "X"		!PRINT
$	THEN
$	  @WNG:WNGFEX "SP" 'FNM''FTP' 'FNM''FTP'
$	  MSGT=MSGT+" printed"
$	ENDIF
$	IF CD_L .EQS. "0"				!LIBRARY
$	THEN
$	  IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	  THEN
$	    LIBRARY/TEXT 'WNG_TLB''L_D'_AX.TLB 'P1'/MODULE='FNM''FTP' !SET LIB.
$	    MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'_AX.TLB]"
$	  ELSE
$	    LIBRARY/TEXT 'WNG_TLB''L_D'.TLB 'P1'/MODULE='FNM''FTP' !SET IN LIB.
$	    MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'.TLB]"
$	  ENDIF
$	ENDIF
$	RETURN
$!#
$!# Exit
$!#
$ ERR:	B1="Not:  "
$	CLOSE/ERROR=ERR6 NXFO'PID''DEP'
$ ERR6:	CLOSE/ERROR=ERR5 NXPO'PID''DEP'
$ ERR5:	CLOSE/ERROR=ERR4 AT'PID''DEP
$ ERR4:	CLOSE/ERROR=ERR3 AF'PID''DEP
$ ERR3:	CLOSE/ERROR=ERR2 NXL1'PID''DEP'
$ ERR2:	CLOSE/ERROR=ERR1 NXL'PID''DEP'			!MAKE SURE
$ ERR1:	TELL B1+FNM+FTP+MSGT
$	UTELL B1+FNM+FTP+MSGT
$ EXIT:	EXIT
$!#
$!#  Fortran
$!#
$ FOR:	FORTRAN/LIST='WNG_LIS''FNM'/OBJECT='FNM''FQ_D' 'JQ_D''FNM''FTP'
$ FOR1:	IF $STATUS/%X1000 .EQ. %X38 THEN GOTO ERR	!DCL WARNING
$	IF F$SEARCH("''FNM'.OBJ") .EQS. "" THEN GOTO ERR
$	MSGT=MSGT+" compiled"
$	IF CD_L .NES. "-"				!TO LIB.
$	THEN
$	  LIBRARY 'WNG_OLB''L_D'.OLB 'FNM'.OBJ
$	  MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'.OLB]"
$	ENDIF
$	IF CD_L .NES. "-"				!DELETE
$	THEN
$	  DELETE 'FNM'.OBJ;*
$	ELSE
$	  PURGE 'FNM'.OBJ
$	ENDIF
$	PURGE 'WNG_LIS''FNM'.LIS
$	IF CD_P .NES. "-"				!PRINT
$	  THEN
$	  @WNG:WNGFEX "SP" 'WNG_LIS''FNM'.LIS 'FNM''FTP'
$	  MSGT=MSGT+" printed"
$	ENDIF
$	RETURN
$!#
$!#  Macro
$!#
$ MAC:	MACRO/LIST='WNG_LIS''FNM'/OBJECT='FNM''MQ_D' 'IQ_D''FNM''FTP'
$	GOTO FOR1
$!#
$!#  C
$!#
$!#
$!#  Help
$!#
$ HLP:	IF F$SEARCH("''WNG_TLB'''FNM'.HLB") .EQS. "" THEN -
		LIBRARY/HELP/CREATE 'WNG_TLB''FNM'.HLB	!CREATE HELP LIB
$	IF F$SEARCH("''WNG_TLB'''FNM'.HLB") .EQS. "" THEN GOTO ERR
$	IF CD_L .EQS. "0"
$	THEN
$	  LIBRARY/HELP 'WNG_TLB''FNM'.HLB 'FNM''FTP'	!SET IN LIBRARY
$	  MSGT=MSGT+" [''FNM'.HLB]"
$	ENDIF
$	IF CD_U .NES. "-" .AND. F$TRNLNM("EXEDWARF") .NES. "" .AND. -
			F$TRNLNM("LIBDWARF") .NES. ""	!UPDATE
$	THEN
$	  @WNG:NXUP "15" "''U_D'" "''P1'" "''FNM'" ""	!UPDATE
$	  MSGT=MSGT+" updated(''U_D')"
$	ENDIF
$	RETURN
$!#
$!#  DSC
$!#
$ DSC:	IF F$SEARCH("WNG:WNTINC.EXE") .EQS. "" THEN GOTO ERR !CANNOT DO
$	WNT="$WNG:WNTINC"				!COMMAND TO DO
$	WNT 'FNM'
$	MSGT=MSGT+" compiled"
$	IF CD_P .NES. "-"				!PRINT
$	THEN
$	  @WNG:WNGFEX "SP" 'FNM'.LIS 'FNM'.LIS
$	  MSGT=MSGT+" printed"
$	ENDIF
$	DIR/VERSION=1/NOHEAD/NOTRAIL/COL=1/OUT='FNM''PID'.TMP -
		'FNM'.DEF,'FNM'_BD.FOR,'FNM'.INC,'FNM'_%.*
$ DSC1:
$	L00=""					!ADD YNZ
$	L01=0
$ LP17:	  IF F$EXTRACT(L01,1,CODES) .EQS. "Y"
$	  THEN
$	    L00=L00+"0"
$	  ELSE
$	    IF F$EXTRACT(L01,1,CODES) .EQS. "Z"
$	    THEN
$	      L00=L00+"-"
$	    ELSE
$	      L00=L00+CD_'F$EXTRACT(L01,1,CODES)'
$	    ENDIF
$	  ENDIF
$	  L01=L01+1
$	  IF L01 .LT. F$LENGTH(CODES) THEN GOTO LP17
$	@WNG:NXEC X2 'PNAM' 'PCOD' 'DEP' "''L00'" "''FG_CX'" -
			'FNM''PID'.TMP			!DO REST
$	IF F$SEARCH("''FNM'''PID'.TMP") .NES. "" THEN -
		DELETE 'FNM''PID'.TMP;*
$	RETURN
$!#
$!# CSC
$!#
$ CSC:
$	L1=""
$	IF F$SEARCH("WNG_OLBEXE:[WNG]GIPLIB.OLB") .NES. "" THEN -
		L1=L1+"/DEF="""wn_gipsy__"""
$	CC/LIST='WNG_LIS''FNM'/OBJECT='FNM''CQ_D'/DIAG='WNG_ERR''FNM'.ERR -
		/DEF="wn_vx__"/DEF="wn_''WNG_SITE'__"'L1' -
		 'FNM''FTP'				!COMP.
$	GOTO FOR1
$!#
$!# SSC
$!#
$ SSC:
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN -
		DELETE 'WNG_ERR''FNM'.ERR;*
$	L01=F$SEARCH("WNG:NXFOR.COM")			!SAVE COMMAND
$	OPEN/ERROR=ERR/WRITE NXFO'PID''DEP' 'FNM'.COM	!CREATE OUTPUT
$	@'L01' 'FNM''FTP' NXFO'PID''DEP' 'WNG_ERR''FNM'.ERR  VAX
$	CLOSE/ERROR=ERR NXFO'PID''DEP'
$	OPEN/ERROR=ERR/WRITE NXFO'PID''DEP' 'FNM'.SUN	!CREATE OUTPUT
$	@'L01' 'FNM''FTP' NXFO'PID''DEP' 'WNG_ERR''FNM'.ERR  UNIX
$	CLOSE/ERROR=ERR NXFO'PID''DEP'
$	IF F$SEARCH("''FNM'.COM") .NES. "" THEN PURGE/KEEP=2 'FNM'.COM
$	IF F$SEARCH("''FNM'.SUN") .NES. "" THEN PURGE 'FNM'.SUN
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN GOTO ERR !ERROR
$	MSGT=MSGT+" compiled"
$	DIR/VERSION=1/NOHEAD/NOTRAIL/COL=1/OUT='FNM''PID'.TMP -
		'FNM'.COM,'FNM'.SUN
$	GOTO DSC1
$!#
$!# FSC
$!#
$ FSC:
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN -
		DELETE 'WNG_ERR''FNM'.ERR;*
$	OPEN/ERROR=ERR/WRITE NXFO'PID''DEP' 'FNM'.FOR	!CREATE OUTPUT
$	@WNG:NXFOR 'FNM''FTP' NXFO'PID''DEP' 'WNG_ERR''FNM'.ERR
$	CLOSE/ERROR=ERR NXFO'PID''DEP'
$	IF F$SEARCH("''FNM'.FOR") .NES. "" THEN PURGE 'FNM'.FOR
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN GOTO ERR !ERROR
$	FORTRAN/LIST='WNG_LIS''FNM'/OBJECT='FNM''FQ_D' 'JQ_D''FNM'.FOR !COMP.
$	GOTO FOR1
$!#
$!#  PSC
$!#
$ PSC:
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN -
		DELETE 'WNG_ERR''FNM'.ERR;*
$	OPEN/ERROR=ERR/WRITE NXPO'PID''DEP' 'FNM'.PIN	!CREATE OUTPUT
$	@WNG:NXPIN 'FNM''FTP' NXPO'PID''DEP' 'WNG_ERR''FNM'.ERR
$	CLOSE/ERROR=ERR NXPO'PID''DEP'
$	IF F$SEARCH("''FNM'.PIN") .NES. "" THEN PURGE 'FNM'.PIN
$	IF F$SEARCH("''WNG_ERR'''FNM'.ERR") .NES. "" THEN GOTO ERR !ERROR
$	MSGT=MSGT+" compiled"
$	DIR/VERSION=1/NOHEAD/NOTRAIL/COL=1/OUT='FNM''PID'.TMP -
		'FNM'.PIN
$	GOTO DSC1
$!#
$!#  PIN
$!#
$ PIN:	L0="13"						!UPDATE CODE
$	L1="PPD"					!TYPE
$	L2="=(COMP)"					!LIST TYPE
$ PIN2:	L3="$RUNDWARF:SYS_BLD''L1'.EXE"			!PROGRAM
$	ASSIGN/USER 'WNG_ERR''FNM'.ERR SYS$OUTPUT
$	L3 'FNM'/LIST'L2'				!DO
$	IF F$SEARCH("''FNM'.LIS") .EQS. "" .OR. -
		F$SEARCH("''FNM'.''L1'") .EQS. "" THEN GOTO ERR
$	MSGT=MSGT+" compiled"
$	PURGE 'FNM'.LIS,'FNM'.'L1'
$	COPY 'FNM'.LIS 'WNG_LIS''FNM'.LIS
$	PURGE 'WNG_LIS''FNM'.LIS
$	COPY 'FNM'.'L1' 'WNG_EXE'
$	PURGE 'WNG_EXE''FNM'.'L1'
$	IF CD_U .NES. "-" .AND. F$TRNLNM("EXEDWARF") .NES. "" !UPDATE
$	THEN
$	  @WNG:NXUP "''L0'" "''U_D'" "''FNM'.''L1'" "''FNM'" ""
$	  MSGT=MSGT+" updated(''U_D')"
$	ENDIF
$	IF CD_P .NES. "-"				!PRINT
$	THEN
$	  @WNG:WNGFEX "SP" 'WNG_LIS''FNM'.LIS 'FNM''FTP'
$	  MSGT=MSGT+" printed"
$	ENDIF
$	RETURN
$!#
$!#  def
$!#
$ DEF:
$	LOA=""					!DO NOT
$	IF FTP .EQS. ".INC" .AND. CWDT .EQS. "[DWARF]" THEN -
			LOA="''FNM'"			!WHAT
$	IF FTP .EQS. ".INC" .AND. CWDT .NES. "[DWARF]" THEN -
			LOA="''FNM'_''FTP'"-"."
$	IF FTP .EQS. ".DEF" .OR. FTP .EQS. ".PEF" .OR. FTP .EQS. ".DSF" THEN -
			LOA="''FNM'_''FTP'"-"."
$	IF LOA .NES. ""
$	THEN
$	  IF CD_U .NES. "-"				!UPDATE
$	  THEN
$	    LOB="WNG_DIR:''CWDT'''FNM'''FTP'"		!FILE NAME
$	    ASSIGN/NOLOG "''LOB'" 'LOA'
$	    IF F$SEARCH("WNG:NXLDEF.COM") .EQS. ""
$	    THEN
$	      OPEN/ERROR=ERR/WRITE NXL'PID''DEP' WNG:NXLDEF.COM
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !01 NXLDEF.COM"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !02 WNB ''C_DATE'"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !03"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !04  Revisions: "
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !05	Automatic by NCOMP"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !06"
$	      WRITE/ERROR=ERR NXL'PID''DEP' -
			"$    !07	Logical names for all include files"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$    !08"
$	      WRITE/ERROR=ERR NXL'PID''DEP' "$   ASSIGN/NOLOG QQ WNG_TLD !Test"
$	      CLOSE/ERROR=ERR NXL'PID''DEP'
$	    ENDIF
$	    OPEN/ERROR=ERR/READ NXL'PID''DEP' WNG:NXLDEF.COM !INPUT
$	    OPEN/ERROR=ERR/WRITE NXL1'PID''DEP' NXL'PID''DEP'.TMP !OUTPUT
$ LP19:	      READ/ERROR=ERR/END=LP18 NXL'PID''DEP' L0	!READ LINE
$	      IF F$LOCATE(LOA,L0) .EQS. F$LENGTH(L0) THEN -
			WRITE/ERROR=ERR NXL1'PID''DEP' L0 !COPY
$	      GOTO LP19
$ LP18:     L0="$  ASSIGN/NOLOG "+""""+"''LOB'"+""""+" ''LOA' ! ''C_DATE'"
$	    WRITE/ERROR=ERR NXL1'PID''DEP' L0
$	    CLOSE/ERROR=ERR NXL'PID''DEP'
$	    CLOSE/ERROR=ERR NXL1'PID''DEP'
$	    SORT NXL'PID''DEP'.TMP WNG:NXLDEF.COM
$	    DELETE NXL'PID''DEP'.TMP;*
$	    PURGE WNG:NXLDEF.COM
$	    MSGT=MSGT+" updated(''U_D')"
$	  ELSE
$	    LOB="''FNM'''FTP'"				!FILE NAME
$	    ASSIGN/NOLOG "''LOB'" 'LOA'
$	  ENDIF
$	ENDIF
$	RETURN
$!#
$!#  Special olb
$!#
$ ALB:	COPY 'FNM''FTP' 'WNG_OLB''FNM'.OLB		!MAKE PROPER LIBRARY
$	PURGE 'WNG_OLB''FNM'.OLB
$	MSGT=MSGT+" compiled"
$	RETURN
$!#
$!#  Special exe
$!#
$ XEX:	COPY 'FNM''FTP' WNG:'FNM'.EXE			!MAKE PROPER EXE
$	PURGE WNG:'FNM'.EXE
$	MSGT=MSGT+" compiled"
$	RETURN
$!#
$!# Exit
$!#
