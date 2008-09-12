$ ! DWEXE.COM
$ !	WNB 910909
$ !
$ ! Revisions:
$ !	WNB 921006	More general
$ !
$ !	Special DWARF exe command
$ !		@DWEXE [-<code>...] ... [<name>] [-<code>...] ...
$ !
$ !		See Help (a ? in a parameter) for details.
$ !
$ ! Local definitions (to be changed at non-NFRA sites)
$ !		*: must contain the correct data
$ !		#: must point to a valid directory
$ !
$ !
$ ! General definitions
$ !
$	VER=F$VERIFY()					!FOR ^Y
$	TELL="WRITE SYS$OUTPUT"				!FOR EASE OF USE
$	SET NOON					!DISCARD ERRORS
$	ON CONTROL_Y THEN GOTO EXEX			!FINISH NEATLY
$ !
$	ASSIGN="ASSIGN"					!MAKE SURE PROPER USE
$	CONVERT="CONVERT"
$	COPY="COPY"
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
$ !
$ !
$	CODES="ABDHILRSTV"				!KNOWN CODES
$	L0=0						!DEFINE ALL CODES
$ LCD1:	CD_'F$EXTRACT(L0,1,CODES)'="0"
$	L0=L0+1
$	IF L0 .LT. F$LENGTH(CODES) THEN GOTO LCD1
$ !
$	FNAM=""						!NO NAMES
$ !
$ ! See if Help
$ !
$ HLP:	L0=P1+P2+P3+P4+P5+P6+P7+P8
$	IF L0-"?" .NES. L0 THEN GOTO HLP1
$ !
$ ! Get codes
$ !
$ COD:	GOSUB GC0					!GET CODES
$	B_COD=A0					!SAVE GIVEN CODES
$	GOSUB GC1					!ANALYZE CODES
$ !
$ ! Interpret codes
$ !
$	IF CD_V .EQS. "2" THEN VER=F$VERIFY(1)		!SET VERIFY
$	IF CD_V .NES. "2" THEN VER=F$VERIFY(0)
$ !
$ ! Get file names
$ !
$ FIL:	IF FNAM .NES. "" THEN GOTO EXE			!FILENAME PRESENT
$	TELL ""
$	TELL "No program name specified"
$	TELL ""
$	GOTO EXEX
$ !
$ ! Execute
$ !
$ EXE:
$ !
$ ! Help
$ !
$	IF (CD_H .EQS. "2") THEN GOTO HLP1
$ !
$ ! ast
$ !
$	IF (CD_R .EQS. "1") THEN CD_S="2"		!SAVE IF NORUN
$	A_COD=""
$	IF (CD_A .EQS. "1") THEN A_COD=A_COD+"/NOASK"
$	IF (CD_A .EQS. "2") THEN A_COD=A_COD+"/ASK"
$	IF (CD_S .EQS. "1") THEN A_COD=A_COD+"/NOSAVE"
$	IF (CD_S .EQS. "2") THEN A_COD=A_COD+"/SAVE"
$	IF (CD_T .EQS. "1") THEN A_COD=A_COD+"/NOTEST"
$	IF (CD_T .EQS. "2") THEN A_COD=A_COD+"/TEST"
$ !
$ !  Stream
$ !
$	IF (CD_B .NES. "0") THEN B_COD="$''CD_B'"
$	IF (CD_B .EQS. "0") THEN B_COD=""
$ !
$ !  Specify
$ !
$	IF (CD_L .EQS. "0" .AND. CD_R .EQS. "0" .AND. -
		CD_D .EQS."0" .AND. CD_I .EQS. "0") THEN GOTO NOSPEC
$	CE_L="LOG"
$	IF (CD_L .EQS. "1") THEN CE_L=CE_L+"=YES"
$	IF (CD_L .EQS. "2") THEN CE_L=CE_L+"=SPOOL"
$	CE_R="RUN"
$	IF (CD_R .EQS. "1") THEN CE_R=CE_R+"=NO"
$	IF (CD_R .EQS. "2") THEN CE_R=CE_R+"=YES"
$	CE_D="DATAB"
$	IF (CD_D .EQS. "1") THEN CE_D=CE_D+"="""""
$	CE_I="INFIX"
$	IF (CD_I .EQS. "1") THEN CE_I=CE_I+"="""""
$	OPEN/WRITE/ERROR=NOSPEC FILE0 TMPDWX.TMP
$	WRITE/ERROR=NOSP1 FILE0 "''CE_L'"
$	WRITE/ERROR=NOSP1 FILE0 "''CE_R'"
$	WRITE/ERROR=NOSP1 FILE0 "''CE_D'"
$	WRITE/ERROR=NOSP1 FILE0 "''CE_I'"
$ NOSP1: CLOSE/ERROR=NOSPEC FILE0
$	ASSIGN/NOLOG TMPDWX.TMP SYS$INPUT
$	ASSIGN/NOLOG NL: SYS$OUTPUT
$	SPECIFY 'FNAM''B_COD'/NOMENU
$	DEASSIGN SYS$INPUT
$	DEASSIGN SYS$OUTPUT
$ !
$ !  Do program
$ !
$ NOSPEC:
$	IF (F$SEARCH("TMPDWX.TMP") .NES. "") THEN DELETE TMPDWX.TMP;*
$	EXE 'FNAM''B_COD''A_COD'/INPUT=SYS$COMMAND
$	GOTO EXEX
$ !
$ ! Routines
$ !
$ ! Get codes and filenames
$ !
$ GC0:	A0=""						!NONE
$	L1=0						!ARG. COUNT
$	L2=8
$ GC02:	L1=L1+1
$ 	IF L1 .GT. L2 THEN RETURN			!ALL DONE
$	IF F$EXTRACT(0,1,P'L1') .EQS. "-" THEN GOTO GC01 !CODE
$	IF P'L1' .EQS. "" THEN GOTO GC02		!EMPTY
$	IF FNAM .NES. "" THEN FNAM=FNAM+","		!SET FILE NAME
$	FNAM=FNAM+P'L1'
$	GOTO GC02					!CONTINUE
$ GC01:	A0=A0+F$EXTRACT(1,-1,P'L1')			!SET CODE
$	GOTO GC02
$ !
$ ! Analyze codes
$ !
$ GC1:
$ GC10:	IF A0 .EQS. "" THEN RETURN			!READY
$	L1="Y"						!NO N SEEN
$ GC14:	L2=F$EXTRACT(0,1,A0)				!CODE
$	A0=A0-L2					!DELETE CODE
$	L2=F$EDIT(L2,"UPCASE")
$	IF L2 .EQS. "N" THEN GOTO GC11			!NEGATE
$	IF CODES-L2 .EQS. CODES THEN GOTO GC10		!UNKNOWN CODE
$	CD_'L2'="1"					!DELETE POSSIBLE CODE
$	IF L1 THEN CD_'L2'="2"				!SET STANDARD CODE
$	IF .NOT.L1 THEN GOTO GC10			!CONTINUE
$	L0=F$EXTRACT(0,1,A0)				!POSSIBLE DIGIT
$	IF "0123456789"-L0 .EQS. "0123456789" THEN GOTO GC13 !NO DIGIT
$	CD_'L2'=L0					!SET DIGIT
$	A0=A0-L0					!DELETE DIGIT FROM CODE
$ GC13:	GOTO GC10					!CONTINUE
$ GC11:	L1="N"						!SET NEGATE
$	GOTO GC14					!GET CODE
$ !
$ ! Exit (GOTO not GOSUB)
$ !
$ EXEX:	SET ON
$ 	L0=F$VERIFY(VER)				!RESET VERIFY
$	EXIT
$ !
$ ! Help
$ !
$ HLP1:	TELL ""
$	TELL "The DWEXE (or DWE) command has the format:"
$	TELL "      DWEXE [-<code>...] ... [<name>] [-<code>...] ..."
$	TELL "The code can be:"
$	TELL ""
$	TELL "a    na    /ASK        /NOASK"
$	TELL "h          help"
$	TELL "s    ns    /SAVE       /NOSAVE"
$	TELL "t    nt    /TEST       /NOTEST"
$	TELL "l    nl    LOG=spool   LOG=y"
$	TELL "r    nr    RUN=yes     RUN=n and /SAVE"
$	TELL "     nd                DATAB="""""
$	TELL "     ni                INFIX="""""
$	TELL "v    nv    verify      noverify"
$	TELL "b<digit>   stream (e.g. b5)"
$	TELL ""
$	GOTO EXEX
