$!#  nlink.ssc
$!# 	WNB 920127
$!# 
$!#  Revisions:
$!#	WNB 921002	Overhaul
$!#	WNB 921006	New date calculation
$!#	JPH 921009	Pre-delete .map end .exe
$!#	JPH 921015	Correct date calculation, include time
$!#			directory check, short rm sequence
$!#	WNB 921016	Typo in date, wrong nxup called, wrong wngfex called
$!#	HjV 921016	Combine changes 921015 and 921016
$!#	HjV 921019	Remove directory check
$!#	WNB 921113	Put WNTAB.EXE in correct place
$!#	WNB 921130	Change tr for HP
$!#	HjV 921203	Use IGETARG iso. GETARG for HP 
$!#	WNB 921208	Get date from nxec; check update possible; log data
$!#	WNB 921209	-a1 switch
$!#	WNB 921216	Correct wntab directory if -u
$!#	WNB 921222	Add test undefined
$!#	WNB 921230	Make SSC
$!#	WNB 930106	Add unresolved test
$!#       HjV 930226      For VAX: remove shared stuff
$!#                       put lnk_def also before lnk_use
$!#			UNIX: remove reference to old dwarflib
$!#	WNB 930517	Use possible objects
$!#	WNB 930802	Change into WNTINC use
$!#	WNB 931130	Add WNG library to Dwarf
$!#	HJV 940202	Add extra call to DWARF LIB for VAX
$!#       AXC 040127      Removed IGETARG exception for hp again
$!# 
$!#	Link programs in nxec system. Use as:
$!#
$!#		source $WNG/nlink.sun		(Unix)
$!#		@WNG:NLINK <file>		(VAX)
$!#
$!#	The command file uses many local nxec variables, and
$!#	environment variables: WNG, LIBDWARF, WNG_OLB, WNG_EXE
$!#	command files: nxec, nxup, wngfex
$!#
$!# Link file
$!#
$	DEP=F$ENVIRONMENT("DEPTH")
$	IF FTP .NES. ".EXE" THEN EXIT			!ONLY .EXE
$	ON ERROR THEN GOTO ERR
$	L0="''C_DATE'/''C_TIME'"			!VERSION YYMMDD/HHMMSS
$	CLOSE/ERROR=GEN1 F'PID''DEP'			!MAKE PROGRAM
$ GEN1:	OPEN/WRITE/ERROR=ERR F'PID''DEP' F'PID''DEP'.FVX
$	WRITE/ERROR=ERR F'PID''DEP' "	PROGRAM ''FNM'_EXE"
$	WRITE/ERROR=ERR F'PID''DEP' "	CHARACTER*80 CLSTR" !COMMAND LINE
$	WRITE/ERROR=ERR F'PID''DEP' "	CALL LIB$GET_FOREIGN(CLSTR)" !GET LINE
$	IF CD_A .NES. "1"				!STANDARD
$	THEN
$	  WRITE/ERROR=ERR F'PID''DEP' "	CALL WNGIN('"+"''FNM'"+"','"+ -
		"''L0'"+"',''DATTP')"			!CALL INIT
$	ELSE
$	  WRITE/ERROR=ERR F'PID''DEP' "	CALL WNGIN1('"+"''FNM'"+"','"+ -
		"''L0'"+"',''DATTP')"			!CALL INIT
$	ENDIF
$	WRITE/ERROR=ERR F'PID''DEP' "	CALL ''FNM'(CLSTR)" !CALL PROGRAM
$	WRITE/ERROR=ERR F'PID''DEP' "	CALL WNGEX"	!FINISH OFF
$	WRITE/ERROR=ERR F'PID''DEP' "	END"
$	CLOSE/ERROR=ERR F'PID''DEP'
$	SET NOON
$	ASSIGN/USER NL: SYS$OUTPUT			!FORGET DONE MESSAGE
$	FORTRAN/LIST='WNG_LIS'F'PID''DEP'/OBJECT=F'PID''DEP''LFORT' -
			F'PID''DEP'.FVX			!COMPILE
$	SET ON
$	IF F$SEARCH("F''PID'''DEP'.OBJ") .EQS. "" THEN GOTO ERR !NOT COMPILED
$	GOSUB LNK					!LINK
$	GOTO ERR2
$!#
$!# Ready
$!#
$ ERR:	B1="Not:  "
$ ERR2:	TELL B1+FNM+MSGT
$	UTELL B1+FNM+MSGT
$ EXIT:	CLOSE/ERROR=ERR1 F'PID''DEP'			!MAKE SURE
$ ERR1:	CLOSE/ERROR=ERR3 O'PID''DEP'
$ ERR3:	IF F$SEARCH("F''PID'''DEP'.FVX") .NES. "" THEN -
		DELETE F'PID''DEP'.FVX;*
$	IF F$SEARCH("F''PID'''DEP'.OBJ") .NES. "" THEN -
		DELETE F'PID''DEP'.OBJ;*
$	IF F$SEARCH("O''PID'''DEP'.OPT") .NES. "" THEN -
		DELETE O'PID''DEP'.OPT;*
$	IF F$SEARCH("''WNG_LIS'F''PID'''DEP'.LIS") .NES. "" THEN -
		DELETE 'WNG_LIS'F'PID''DEP'.LIS;*
$	EXIT
$!#
$!# Local subroutine
$!#
$ LNK:	CLOSE/ERROR=LNK1 O'PID''DEP'			!MAKE OPTIONS
$ LNK1:	OPEN/WRITE/ERROR=ERR O'PID''DEP' O'PID''DEP'.OPT
$	L2=""						!LIBRARIES
$	L0=1						!SEARCH LIST
$ LNK0:	L3=F$SEARCH("*.OBJ;0",L0)			!FIND OBJECTS
$	IF (L3 .NES. "")
$	THEN
$	  IF F$LOCATE("F''PID'''DEP'",L3) .EQ. F$LENGTH(L3)
$	  THEN
$	    IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	    L2=L3					!NEXT
$	  ENDIF
$	  GOTO LNK0					!MORE?
$	ENDIF
$	IF CD_L .NES. "-"
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2="''L_D'/LIB"				!STANDARD LIB
$	ENDIF
$	IF LNK_DEF .NES. ""
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2=LNK_DEF					!STANDARD LIBS
$	ENDIF
$	IF LNK_USE .NES. ""
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2=LNK_USE
$	ENDIF
$	IF OQ_D .NES. ""
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2=OQ_D					!USER LIBS
$	ENDIF
$	IF CD_S .NES. "-"				!DWARF WANTED
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2="LIBDWARF:WNLIB/LIB"			!DWARF LIB
$	ENDIF
$	IF LNK_DEF .NES. ""
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2=LNK_DEF					!STANDARD LIBS
$	ENDIF
$	IF CD_S .NES. "-"				!DWARF WANTED
$	THEN
$	  IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2+",-"
$	  L2="LIBDWARF:WNLIB/LIB"			!DWARF LIB
$	ENDIF
$	IF L2 .NES. "" THEN -
$			WRITE/ERROR=ERR O'PID''DEP' L2	!LAST LINE
$	CLOSE/ERROR=ERR O'PID''DEP'
$	ON WARNING THEN GOTO ERR
$	MSGT=MSGT+ "linked"
$	LINK/EXEC='WNG_EXE''FNM'.EXE/MAP='WNG_EXE''FNM'.MAP'LQ_D' -
		F'PID''DEP'.OBJ, -
		O'PID''DEP'.OPT/OPTION
$	IF F$SEARCH("''WNG_EXE'''FNM'.EXE") .EQS. "" .OR. -
		F$SEARCH("''WNG_EXE'''FNM'.MAP") .EQS. "" THEN GOTO ERR
$	ON ERROR THEN GOTO ERR
$	PURGE/NOLOG 'WNG_EXE''FNM'.EXE,'WNG_EXE''FNM'.MAP
$	IF FNM .EQS. "WNTINC"				!SAVE IN CORRECT PLACE
$	THEN
$	  COPY 'WNG_EXE''FNM'.EXE WNG:
$	  PURGE WNG:'FNM'.EXE
$	ELSE
$	  IF CD_U .NES. "-" .AND. F$TRNLNM("EXEDWARF") .NES. "" !UPDATE
$	  THEN
$	    @WNG:NXUP "4" "''U_D'" "''WNG_EXE'''FNM'.EXE" "''FNM'" ""
$	    MSGT=MSGT+" updated(''U_D')"
$	  ENDIF
$	ENDIF
$	IF CD_P .NES. "-"				!PRINT
$	THEN
$	  @WNG:WNGFEX "SP" 'WNG_EXE''FNM'.MAP 'FNM'.EXE
$	  MSGT=MSGT+" printed"
$	ENDIF
$	RETURN
