$!#  ndel.ssc
$!# 	WNB 920208
$!# 
$!#  Revisions:
$!#	WNB 920922	Add l switch to ar
$!#			Add .fun type
$!#	WNB 921002	Overhaul
$!#	WNB 921012	Add question
$!#	WNB 921016	Wrong nxup called
$!#	WNB 921113	Postpone ar
$!#	WNB 921122	Delete .uin
$!#	WNB 921130	Change tr for HP
$!#	WNB 921202	Include .pef
$!#	WNB 921204	Limit tlbdel for HP
$!#	WNB 921208	Limit update; log data
$!#	WNB 921209	Include include files; -a0 switch
$!#	WNB 921211	Add PSC
$!#	WNB 921215	Typo
$!#	WNB 921215	Add FSC, CSC, CUN
$!#	WNB 921218	Add SSC
$!#	HJV 921221	Delete -f in chmod
$!#	WNB 921222	Typo SSC; streamline psc ssc
$!#	WNB 921230	Add HP length; make SSC
$!#	JPH 930225	Typo. - Use grep -v to remove line from nxldef
$!#	WNB 930325	Cater for different fold
$!#	WNB 930330	Add .a.. and .x..
$!#	WNB 930405	Suppress some error messages (VAX)
$!#	WNB 930803	Add .dsf
$!#	WNB 940210	Typo FSC handling (vx__ iso vax__)
$!# 
$!# 	Delete routines in nxec system. Use as:
$!#
$!#		source $WNG/ndel.sun	(UNIX)
$!#		@WNG:NDEL <file>	(VAX)
$!#
$!#	The command file uses a lot of local nxec variables, and
$!#	environment variables: WNG, WNG_TLB, WNG_OLB, WNG_TYPE
$!#	command files: nxup
$!#
$!#  Delete a file.
$!#
$	ON ERROR THEN GOTO ERR
$	IF F$SEARCH("''FNM'''FTP'") .NES. "" THEN -
		PURGE/KEEP=2 'FNM''FTP'			!PURGE AS YOU GO
$	IF CD_A .EQS. "0"				!CONFIRM
$	THEN
$	  READ/TIME=90/END=EXIT/ERROR=EXIT -
		/PROMPT="Delete ''FNM'''FTP'? (Y N) [N]: " -
		SYS$COMMAND L0
$	  IF .NOT.L0 THEN GOTO ERR
$	ENDIF
$	MSGT=MSGT+" deleted"				!INDICATE DONE
$	IF CD_C .NES. "-"				!COMPILE
$	THEN
$	  IF FTP .EQS. ".FSC" THEN GOSUB FSC		!FSC
$	  IF FTP .EQS. ".FOR" .OR. FTP .EQS. ".FVX" THEN GOSUB FOR !FORTRAN
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
$	  IF FTP .EQS. ".EXE" THEN GOSUB EXF		!EXE
$	ENDIF
$	GOSUB OTH					!ALL OTHERS
$	GOTO ERR1
$!#
$!#  Others
$!#
$ OTH:	IF F$SEARCH("''FNM'''FTP'") .NES. "" THEN DELETE 'FNM''FTP';*
$	IF CD_L .EQS. "0"				!LIBRARY
$	THEN
$	  IF F$EXTRACT(1,1,FTP) .EQS. "A" .OR. -
		F$EXTRACT(1,1,FTP) .EQS. "X"
$	  THEN
$	    SET NOON
$	    ASSIGN/USER NL: SYS$OUTPUT
$	    ASSIGN/USER NL: SYS$ERROR
$	    LIBRARY/TEXT/DEL='FNM''FTP' 'WNG_TLB''L_D'_AX.TLB !DELETE IN LIBRARY
$	    SET ON
$	    MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'_AX.TLB]"
$	  ELSE
$	    SET NOON
$	    ASSIGN/USER NL: SYS$OUTPUT
$	    ASSIGN/USER NL: SYS$ERROR
$	    LIBRARY/TEXT/DEL='FNM''FTP' 'WNG_TLB''L_D'.TLB !DELETE IN LIBRARY
$	    SET ON
$	    MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'.TLB]"
$	  ENDIF
$	ENDIF
$	RETURN
$!#
$!# Ready
$!#
$ ERR:	B1="Not:  "
$ ERR1:	TELL B1+FNM+FTP+MSGT
$	UTELL B1+FNM+FTP+MSGT
$ EXIT:	EXIT
$!#
$!#  Fortran
$!#
$ FOR:
$	IF F$SEARCH("''FNM'.OBJ") .NES. "" THEN DELETE 'FNM'.OBJ;*
$	IF CD_L .NES. "-"
$	THEN
$	  SET NOON
$	  ASSIGN/USER NL: SYS$OUTPUT
$	  ASSIGN/USER NL: SYS$ERROR
$	  LIBRARY/DEL='FNM' 'WNG_OLB''L_D'
$	  SET ON
$	  MSGT=MSGT+" [''F$PARSE(L_D,,,"NAME","SYNTAX_ONLY")'.OLB]"
$	ENDIF
$	RETURN
$!#
$!# FSC
$!#
$ FSC:	IF F$SEARCH("''FNM'.FOR") .NES. "" THEN DELETE 'FNM'.FOR;*
$	RETURN
$!#
$!#  Macro
$!#
$ MAC:
$	GOTO FOR
$!#
$!#  C
$!#
$!#
$!#  SSC
$!#
$ SSC:
$	ASSIGN/USER NL: SYS$OUTPUT
$	ASSIGN/USER NL: SYS$ERROR
$	DIR/VERSION=1/NOHEAD/NOTRAIL/COL=1/OUT='FNM''PID'.TMP -
		'FNM'.COM,'FNM'.SUN
$	GOTO DSC1
$!#
$!#  Help
$!#
$ HLP:
$	IF CD_L .EQS. "0" .AND. F$SEARCH("''WNG_TLB'''FNM'.HLB") .NES. "" THEN -
		DELETE 'WNG_TLB''FNM'.HLB;*
$	MSGT=MSGT+" [''FNM'.HLB]"
$	IF CD_U .NES. "-" .AND. F$TRNLNM("LIBDWARF") .NES. "" .AND. -
		F$TRNLNM("EXEDWARF") .NES. ""		!UPDATE
$	THEN
$	  @WNG:NXUP "A15" "''U_D'" "''P1'" "''FNM'" ""
$	  MSGT=MSGT+" updated(''U_D')"
$	ENDIF
$	RETURN
$!#
$!#  DSC
$!#
$ DSC:
$	ASSIGN/USER NL: SYS$OUTPUT
$	ASSIGN/USER NL: SYS$ERROR
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
$!#  PSC
$!#
$ PSC:
$	ASSIGN/USER NL: SYS$OUTPUT
$	ASSIGN/USER NL: SYS$ERROR
$	DIR/VERSION=1/NOHEAD/NOTRAIL/COL=1/OUT='FNM''PID'.TMP -
		'FNM'.PIN
$	GOTO DSC1
$!#
$!#  PIN
$!#
$ PIN:	L0="A13"					!UPDATE CODE
$	L1="PPD"					!TYPE
$	IF F$SEARCH("''FNM'.''L1'") .NES. "" THEN DELETE 'FNM'.'L1';*
$	IF F$SEARCH("''WNG_OLB'''FNM'.''L1'") .NES. "" THEN -
			DELETE 'WNG_OLB''FNM'.'L1';*
$	IF CD_U .NES. "-" .AND. F$TRNLNM("EXEDWARF") .NES. "" !UPDATE
$	THEN
$	  @WNG:NXUP "''L0'" "''U_D'" "''P1'" "''FNM'" ""
$	  MSGT=MSGT+" updated(''U_D')"
$	ENDIF
$	RETURN
$!#
$!#  Special EXE
$!#
$ XEX:
$	IF F$SEARCH("''FNM'.''FTP'") .NES. "" THEN DELETE 'FNM'.'FTP';*
$	IF F$SEARCH("WNG:''FNM'.EXE") .NES. "" THEN -
			DELETE WNG:'FNM'.EXE;*
$	RETURN
$!#
$!#  Special OLB
$!#
$ ALB:
$	IF F$SEARCH("''FNM'.''FTP'") .NES. "" THEN DELETE 'FNM'.'FTP';*
$	IF F$SEARCH("''WNG_OLB'''FNM'.OLB") .NES. "" THEN -
			DELETE 'WNG_OLB''FNM'.OLB;*
$	RETURN
$!#
$!#  def
$!#
$ DEF:
$	IF CD_U .NES. "-"				!UPDATE
$	THEN
$	  LOB="WNG_DIR:''CWDT'''FNM'''FTP'"		!FILE NAME
$	  LOA=""					!DO NOT
$	  IF FTP .EQS. ".INC" .AND. CWDT .EQS. "[DWARF]" THEN -
			LOA="''FNM'"			!WHAT
$	  IF FTP .EQS. ".INC" .AND. CWDT .NES. "[DWARF]" THEN -
			LOA="''FNM'_''FTP'"-"."
$	  IF FTP .EQS. ".DEF" .OR. FTP .EQS. ".PEF" .OR. FTP .EQS. ".DSF" THEN -
			LOA="''FNM'_''FTP'"-"."
$	  IF LOA .NES. ""
$	  THEN
$	    IF F$TRNLNM("''LOA'") .NES. "" THEN DEASSIGN 'LOA'
$	    IF F$SEARCH("WNG:NXLDEF.COM") .NES. ""
$	    THEN
$	      OPEN/ERROR=ERR/READ NXL'PID''DEP' WNG:NXLDEF.COM !INPUT
$	      OPEN/ERROR=ERR/WRITE NXL1'PID''DEP' NXL'PID''DEP'.TMP !OUTPUT
$ LP19:	        READ/ERROR=ERR/END=LP18 NXL'PID''DEP' L0 !READ LINE
$	        IF F$LOCATE(LOA,L0) .EQS. F$LENGTH(L0) THEN -
			WRITE/ERROR=ERR NXL1'PID''DEP' L0 !COPY
$	        GOTO LP19
$ LP18:	      CLOSE/ERROR=ERR NXL'PID''DEP'
$	      CLOSE/ERROR=ERR NXL1'PID''DEP'
$	      SORT NXL'PID''DEP'.TMP WNG:NXLDEF.COM
$	      DELETE NXL'PID''DEP'.TMP;*
$	      PURGE WNG:NXLDEF.COM
$	    ENDIF
$	    MSGT=MSGT+" updated(''U_D')"
$	  ENDIF
$	ENDIF
$	RETURN
$!#
$!# EXE files
$!#
$ EXF:
$	IF F$SEARCH("''WNG_EXE'''FNM'''FTP'") .NES. "" THEN -
			DELETE 'WNG_EXE''FNM''FTP';*
$	IF CD_U .NES. "-" .AND. F$TRNLNM("EXEDWARF") .NES. "" !UPDATE
$	THEN
$	  @WNG:NXUP "A4" "''U_D'" "''P1'" "''FNM'" ""
$	  MSGT=MSGT+" updated(''U_D')"
$	ENDIF
$	RETURN
$!#
$!# Exit
$!#
$!
