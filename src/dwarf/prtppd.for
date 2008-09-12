C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_PRTPPD
C.Keywords:	PPD File, Print
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900407 FMO - recreation
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE SYS_PRTPPD
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Print a map of a PPD file
C.Returns:	Not applicable
C.Notes:
C	- The program will prompt for the name of the PPD file.
C	- If <name>/PRINT is answered, the listing will be printed and deleted.
C	- If <name> is answered, the listing will kept as file <name>.MLIS.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	PROGNAME
	INTEGER*4	NRARG
		PARAMETER (PROGNAME = 'PRTPPD')
		PARAMETER (NRARG = 2)
	CHARACTER*6	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*13	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'PPDNAM'       ,'PRINT'       /
		DATA ATTR   /CLI__REQUIRED  ,CLI__QUALIFIER/
		DATA PROMPT /'PPD-file name',' '           /
		DATA DEFVAL /' '            ,' '           /
C
	INTEGER*4	CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER*4	PPD_LIST
C
	CHARACTER	PPDNAM*80, DUM*1
	INTEGER*4	IS, LNAM, LDUM
C
C
C					Initialize
C					- start messenger
C					- initialize command-line interpreter
C
	IS = MSG_INIT (PROGNAME,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get and check the PPD name
C
	IS = CLI_GET ('PPDNAM',PPDNAM,LNAM)
	IF (LNAM.EQ.0) THEN
		IS = MSG_SET (PPD_NOIMAGE,0)
	ELSE IF (LNAM.GT.9) THEN
		IS = MSG_SET (PPD_IMTOOLONG,0)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get and interpret the PRINT qualifier
C
	IS = CLI_GET ('PRINT',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = PPD_LIST (PPDNAM(:LNAM),F_SP)
	ELSE
		IS = PPD_LIST (PPDNAM(:LNAM),F_YES)
	ENDIF
	IF (IAND(IS,1).EQ.0) IS = MSG_SET(IS,0)
C
C					Terminate the program
C
 999	E_C=MSG_SET(IS,0)		!Set exit code for WNGEX
	END
