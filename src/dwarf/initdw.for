C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_INITDW
C.Keywords:	Initializing, Program Parameters, Specify
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	Fortran
C.Environment:	Any
C.Comments:
C.Version:	911126 GvD - creation
C.Version:	920212 GvD - change calls to MSG-routines
C.Version:	940121 CMV - changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE INITDW
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Define initial DWARF parameters as <ident>+<ibmode>
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		[ident]+[ibmode]
C	- Qualifiers:
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGRAM
		PARAMETER (PROGRAM = 'INITDW')
C
	INTEGER		NRARG, PREQ
		PARAMETER (NRARG = 1)
		PARAMETER (PREQ  = CLI__PARAMETER+CLI__REQUIRED+CLI__DEFAULT)
	CHARACTER*10	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*14	PROMPT(NRARG)
	CHARACTER*15	DEFVAL(NRARG)
		DATA NAME   /'IDIB'/
		DATA ATTR   /PREQ/
		DATA PROMPT /'Ident/Ibmode'/
		DATA DEFVAL /'XYZ+INTERACTIVE'/
C
	INTEGER		SYMBOL_DEFINE
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		DWC_CTL_OPEN, DWC_CTL_FILL
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		SYMBOL_EXIT
C
	INTEGER		IS, LPS
	CHARACTER	IDIB*32
C
C
C
C					Initialize
C					- get DWARF control variables
C					- start messenger
C					- initialize command-line interpreter
C
	IS = DWC_CTL_OPEN ()
	IF (IAND(IS,1).EQ.0) IS = DWC_CTL_FILL ()
	IF (IAND(IS,1).NE.0) IS = MSG_INIT (PROGRAM,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get ident and ibmode
C
	IS = CLI_GET ('IDIB',IDIB,LPS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Define ident if given
C					(can be max 3 characters)
	I = INDEX (IDIB,'+')
	IF (I.EQ.0) I = 1+LPS
	IF (I.GT.1) THEN
	    IS = SYMBOL_DEFINE ('DWARF$0_IDENT', IDIB(:MIN(3,I-1)), 0)
	ENDIF
C
C					Define ibmode if given
	IF (I.LT.LPS) THEN
	    IS = SYMBOL_DEFINE ('DWARF$0_IBMODE', IDIB(I+1:LPS), 0)
	ENDIF
C
C					Assemble DWARF symbols
C					Terminate the program
C
	IS = DWC_CTL_FILL ()
	IS = SYMBOL_EXIT ()
 999	E_C = MSG_SET(IS,0)		! Exit code for WNGEX
	END
