C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_BLDPPD
C.Keywords:	PPD File, Build
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900426 FMO - recreation
C.Version:	920206 GvD - add former optional arguments to CLI_GET and MSG
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	010709 AXC - Linux port - init. E_C
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE SYS_BLDPPD
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Build a PPD file from a PIN file
C.Returns:	Not applicable
C.Notes:
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	LISTYPES, COMMA
		PARAMETER (LISTYPES = 'COMPILATION,MAP,REFERENCE')
		PARAMETER (COMMA    = ',')
C
	INTEGER*4	NRARG, PR, Q, QV, QVD
		PARAMETER (NRARG = 3)
		PARAMETER (PR    = CLI__PARAMETER+CLI__REQUIRED)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QV    = CLI__QUALIFIER+CLI__VALUE)
		PARAMETER (QVD   = CLI__QUALIFIER+CLI__VALUE+CLI__DEFAULT)
	CHARACTER*7	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*12	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'PROGNAM'     ,'LIST','PRINT'/
		DATA ATTR   / PR           , QVD  , Q     /
		DATA PROMPT /'Program name',' '   ,' '    /
		DATA DEFVAL /' '           ,'COMP',' '    /
C
	INTEGER*4	BPD_COMPILE, BPD_REF_WRITE, BPD_REF_LIST, PPD_LIST
	INTEGER*4	CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER*4	DWC_PROG_CHECK, GEN_LNM_DEFINE
	INTEGER*4	STR_COPY_U, STR_MATCH_L
C
	CHARACTER	PROGNAM*16, LIST*80, LISTYP*12, DUM*1
	INTEGER*4	LP, LD, LL, LT, LDUM
	INTEGER*4	IS, PRTFLAGS, PTR, MATCHNR
	LOGICAL*4	DO_CLIST, DO_MLIST, DO_RLIST, IS_GLOBAL
C
C
C					Initialize
C					- start messenger
C					- initialize command-line interpreter
C
	IS = MSG_INIT ('BLDPPD',F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get and check the program name
C
	IS = CLI_GET ('PROGNAM',PROGNAM,LP)
	IF (IAND(IS,1).NE.0)
	1		IS = DWC_PROG_CHECK (PROGNAM(:LP),LP,IS_GLOBAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Set list and print flags
C
	DO_CLIST = .FALSE.
	DO_MLIST = .FALSE.
	DO_RLIST = .FALSE.
	PRTFLAGS = F_YES
C
	IS = CLI_GET ('LIST',LIST,LL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		PTR = 1
		DO WHILE (PTR.LE.LL)
			LT = 0
			IS = STR_COPY_U (COMMA,LIST(:LL),PTR,LISTYP,LT)
			IS = STR_MATCH_L (LISTYP(:LT),LISTYPES,MATCHNR)
			IF (MATCHNR.EQ.1) THEN
				DO_CLIST = .TRUE.
			ELSE IF (MATCHNR.EQ.2) THEN
				DO_MLIST = .TRUE.
			ELSE IF (MATCHNR.EQ.3) THEN
				DO_RLIST = .TRUE.
			ELSE
				IS = MSG_SET (PPD_INVQUAVAL,0)
				GOTO 999
			ENDIF
			PTR = PTR+1
		ENDDO
		IS = CLI_GET ('PRINT',DUM,LDUM)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (IS.EQ.DWC_PRESENT)
	1		PRTFLAGS = F_SP
	ENDIF
C
C					Compile with UPDATE 
C
	IS = BPD_COMPILE (PROGNAM(:LP),DO_CLIST,PRTFLAGS,.TRUE.)
C
C					If compilation succesful:
C					- list the new PPD file
C					- create a new PPD cross-reference file
C					- list the references to the new PPD
C
	IF (IS.EQ.PPD_SUCCESS) THEN
		IF (DO_MLIST) IS = PPD_LIST (PROGNAM(:LP),PRTFLAGS)
		IF (IAND(IS,1).NE.0) IS = BPD_REF_WRITE (PROGNAM(:LP))
		IF (IAND(IS,1).NE.0 .AND.DO_RLIST)
	1			IS = BPD_REF_LIST (PROGNAM(:LP),PRTFLAGS)
	ENDIF
C
C
 999	E_C = MSG_SET(IS,0) 		!WNGEX exit code
	E_C = 0
	END













