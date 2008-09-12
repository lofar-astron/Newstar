C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_SPECIFY
C.Keywords:	Program Parameters, External Defaults, Specify
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	VAX-Fortran
C.Environment:	VAX
C.Comments:
C.Version:	900325 FMO - creation
C.Version:	900411 FMO - commented out LIB$DO_COMMAND
C.Version:	910814 FMO - add SYMBOL_EXIT call at the end
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE SPECIFY
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C.Purpose:	Define external defaults for program parameters
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		prog[$stream]		required
C	- Qualifiers:
C		/CLEAR			default: /NOCLEAR
C		/NOMENU			default: /MENU
C		/COPY=prog[$stream]	default: /NOCOPY
C		/TEST or /NOTEST	default: current DWARF control parm
C		/NOSUBSTITUTE		default: /SUBSTITUTE
C		/EXTERNAL		default: /NOEXTERNAL
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGRAM
		PARAMETER (PROGRAM = 'SPECIFY')
C
	INTEGER		NRARG, PREQ, Q, QDEF, QVAL
		PARAMETER (NRARG = 7)
		PARAMETER (PREQ  = CLI__PARAMETER+CLI__REQUIRED)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QDEF  = CLI__QUALIFIER+CLI__DEFAULT)
		PARAMETER (QVAL  = CLI__QUALIFIER+CLI__VALUE)
	CHARACTER*10	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*14	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'PROGSTRM',
	1		'CLEAR','MENU','COPY','TEST','SUBSTITUTE','EXTERNAL'/
		DATA ATTR   /PREQ,
	1		Q      ,QDEF  ,QVAL  ,Q     ,QDEF        ,Q         /
		DATA PROMPT /'Program$stream',6*' '/
		DATA DEFVAL /7*' '/
C
	INTEGER		SP_MENU, SP_NOMENU, SP_CLEAR, SP_COPY
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		DWC_TEST_PUT, DWC_SYM_SPLIT, DWC_PROG_PUT
	INTEGER		DWC_PROG_CHECK, DWC_STREAM_CHECK, DWC_STREAM_GET
	INTEGER		DWC_CTL_OPEN, DWC_CTL_FILL, DWC_IBMODE_INQ
	INTEGER		PPD_INIT, PPD_EXIT
	INTEGER		SYMBOL_EXIT
C
	CHARACTER	PROGSTRM*22, PROG*9, TMPSTRM*12, STREAM*12, KEY*16
	CHARACTER	INPUTPS*22, INPROG*9, INSTREAM*12, LINE*255, DUM*1
	INTEGER		IS, LPS, LP, LS, LK, LIPS, LIP, LIS, LL, LDUM
	LOGICAL		IS_GLOBAL, IS_DCL
	LOGICAL		DO_MENU, DO_LOCAL, DO_SUBST, DO_EXTERN
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
C					Get program and stream names
C
	IS = CLI_GET ('PROGSTRM',PROGSTRM,LPS)
	IF (IAND(IS,1).NE.0)
	1	IS = DWC_SYM_SPLIT (PROGSTRM(:LPS),PROG,LP,TMPSTRM,LS,KEY,LK)
	IF (IAND(IS,1).NE.0)
	1	IS = DWC_PROG_CHECK (PROG(:LP),LP,IS_GLOBAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LS.GT.0) THEN
		IS = DWC_STREAM_CHECK (TMPSTRM(:LS),STREAM,LS,IS_GLOBAL)
	ELSE
		IS = DWC_STREAM_GET (STREAM,LS,IS_GLOBAL)
	END IF	
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If the TEST qualifier is given,
C					enable or disable test mode accordingly
C
	IS = CLI_GET ('TEST',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.NE.DWC_ABSENT) IS = DWC_TEST_PUT (IS.EQ.DWC_PRESENT)
C
C					Get names of input program and stream
C
	IS = CLI_GET ('COPY',INPUTPS,LIPS)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LIPS.GT.0) THEN
		IS = DWC_SYM_SPLIT (INPUTPS(:LIPS),INPROG,LIP,
	1						TMPSTRM,LIS,KEY,LK)
		IF (IAND(IS,1).NE.0)
	1		IS = DWC_PROG_CHECK (INPROG(:LIP),LIP,IS_GLOBAL)
		IF (IAND(IS,1).NE.0 .AND. LIS.GT.0) THEN
			IS = DWC_STREAM_CHECK (TMPSTRM(:LIS),INSTREAM,LIS,
	1						IS_GLOBAL)
		ELSE
			IS = DWC_STREAM_GET (INSTREAM,LIS,IS_GLOBAL)
		END IF	
		IF (IAND(IS,1).EQ.0) GOTO 999
	END IF
C
C					/MENU is not allowed in batch mode
C
	IS = CLI_GET ('MENU',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_MENU = IS.NE.DWC_NEGATED
	IF (DO_MENU .AND. IAND(DWC_IBMODE_INQ('BATCH'),1).NE.0) THEN
		IS = MSG_SET (DWC_QUALBATCH,1)
		CALL WNCTXT(DWLOG,DWMSG,'/MENU')
		IS = DWC_SUCCESS
		GOTO 999
	END IF
C
C					Open the PPD file
C
	IS = PPD_INIT (PROG(:LP))
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C			Set program name to program being specified
C
	IS = DWC_PROG_PUT (PROG(:LP))
C
C					Check whether local defaults are
C					allowed if program is not 'GLOBAL'
C
	DO_LOCAL = PROG(:LP).NE.'GLOBAL'
C
C					Clear existing defaults if requested
C
	IS = CLI_GET ('CLEAR',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = SP_CLEAR (PROG(:LP),STREAM(:LS))
		IF (IAND(IS,1).EQ.0) GOTO 999
	END IF
C
C					Copy defaults if requested
C
	IF (LIPS.GT.0) THEN
		IS = SP_COPY (PROG(:LP),STREAM(:LS),INPROG(:LIP),INSTREAM(:LIS))
		IF (IAND(IS,1).EQ.0) GOTO 999
	END IF
C
	IS = CLI_GET ('SUBSTITUTE',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_SUBST = IS.NE.DWC_NEGATED
C
	IS = CLI_GET ('EXTERNAL',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_EXTERN = IS.EQ.DWC_PRESENT
C
C					Ask or accept default values
C
	IS_DCL = .FALSE.
	IF (DO_MENU) THEN
		IS = SP_MENU (PROG(:LP),STREAM(:LS),DO_LOCAL,DO_SUBST,DO_EXTERN)
	ELSE
		IS = SP_NOMENU (PROG(:LP),STREAM(:LS),DO_LOCAL,DO_SUBST,
	1							IS_DCL,LINE,LL)
	END IF
C
C					Wrap-up
C					- close the PPD file
C					- assemble DWARF symbols (if applicable)
C					- terminate the program
C
	IS = PPD_EXIT ()
	IF (PROG(:LP).EQ.'DWARF') IS = DWC_CTL_FILL ()
	IS = SYMBOL_EXIT ()
 999	E_C = MSG_SET(IS,0)		!WNGEX exit code
	END
