C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_DEF_CHECK
C.Keywords:	Program Parameters, Specify, Check Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION SP_DEF_CHECK (SYMBOL,DLEVEL,HELPSW,
	1				VALUE,VALOUT,LOUT,DO_SUBST)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) symbol name (for messages)
	INTEGER*4	DLEVEL		! (m) helplevel minus userlevel
	INTEGER*4	HELPSW		! (i) print control for help info
	CHARACTER*(*)	VALUE		! (i) input value string
	CHARACTER*(*)	VALOUT		! (o) output value string
	INTEGER*4	LOUT		! (o) significant length of VALOUT
	LOGICAL*4	DO_SUBST	! (i) substitution requested by SPECIFY
C
C.Purpose:	Process the value string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_KEYVAHELP	value was a help request
C	warning	DWC_SYMBOLCLR	/CLEAR was given as value
C	false status codes returned by referenced modules
C.Notes:
C	- HELPSW > 0 (for SPECIFIY/NOMENU): always print help on request
C	         = 0 (for SPECIFY/MENU): only print if helplevel < max level
C	- The input string may contain a value string and qualifiers.
C	- The /CLEAR qualifier (no value string allowed) causes the current
C	  SPECIFY default to be deleted.
C	- The /(NO)ASK qualifier is accepted and will be (part of) the
C	  default "value".
C	- The value string will be checked completely if (in that order):
C	  the IMMEDIATE-switch in the PPD file is set, or
C	  the qualifier /SUBSTITUTE is appended to the value string, or
C	  no /NOSUBSTITUTE was given on the value string or the SPECIFY command.
C	- DO_SUBST = .TRUE. unless SPECIFY/NOSUBSTITUTE was given.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STR_SUBST, DWC_SYM_SPLIT, DWC_HELP
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_RELEASE
	INTEGER*4	PV_BLK_DECODE, PV_BLK_ENCODE
	INTEGER*4	PPD_AMAS_GET
	INTEGER*4	STR_SIGLEN, STR_COPY
	INTEGER*4	SYMBOL_DELETE, MSG_SET  
C
	INTEGER*4	NRARG, EXPR, QUAL
		PARAMETER (NRARG = 4)
		PARAMETER (EXPR = CLI__EXPRESSION)
		PARAMETER (QUAL = CLI__QUALIFIER)
	CHARACTER*10	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR' ,'ASK' ,'SUBSTITUTE' ,'CLEAR'/
		DATA ATTR   / EXPR    , QUAL , QUAL        , QUAL  /
		DATA PROMPT /' '      ,' '   ,' '          ,' '    /
		DATA DEFVAL /' '      ,' '   ,' '          ,' '    /
C
	CHARACTER	WORK*255, VALSTR*255, PROG*16, STREAM*16, KEY*16
	CHARACTER	DUM*1
	INTEGER*4	IS, LW, LVAL, LD, LP, LS, LK, VALBLK(8)
	INTEGER*4	ERRPTR, COUNT, Q_ASK
	LOGICAL*4	DO_CHECK, SWSYM
	BYTE		DEFARR(1)				! dummy
C
C
C					Substitute symbols (unknown allowed)
C
	IS = DWC_SYM_SPLIT (SYMBOL,PROG,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	SWSYM = .FALSE.
	IS = DWC_STR_SUBST (VALUE,WORK,LW,STREAM(:LS),ERRPTR,.TRUE.,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
C					If help request:
C					- give help and return
C
	IS = DWC_HELP (WORK(:LW),HELPSW,DLEVEL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the string
C					- reset the Command-Line Interpreter
C					- parse the string
C					- extract the pure value string
C
	IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).NE.0) IS = CLI_PARSE (WORK(:LW))
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',VALSTR,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the ASK qualifier
C
	COUNT = 0
	IS = CLI_GET ('ASK',DUM,LD)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		Q_ASK = 1
		COUNT = COUNT+1
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		Q_ASK = -1
		COUNT = COUNT+1
	ELSE
		Q_ASK = 0
	ENDIF
C
C					Must the value must be checked ?
C
	DO_CHECK = .TRUE.				! yes by default
	IS = CLI_GET ('SUBSTITUTE',DUM,LD)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN			! explicitly requested
		IF (LVAL.EQ.0) GOTO 992			! (value required)
		COUNT = COUNT+1
	ELSE IF (IS.EQ.DWC_NEGATED) THEN		! explicitly suppressed
		IF (LVAL.EQ.0) GOTO 993			! (value required)
		DO_CHECK = .FALSE.
		COUNT = COUNT+1
	ELSE
		DO_CHECK = DO_SUBST			! follow SPECIFY
	ENDIF
C
	IF (IAND(PPD_AMAS_GET('IMMEDIATE'),1) .NE. 0) THEN ! PPD forces full check
		IF (.NOT.DO_CHECK) THEN
			IS = MSG_SET (DWC_IMMNOSUBS,0) ! issue info message
			DO_CHECK = .TRUE.
		ENDIF
	ENDIF
C
C					If /CLEAR was given:
C					- no other qualifiers allowed
C					- no value allowed
C					- clear the symbol
C					- return with 'cleared' status
C
	IS = CLI_GET ('CLEAR',DUM,LD)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		IF (COUNT.GT.0) GOTO 994
		IF (LVAL.GT.0) GOTO 995
		IS = SYMBOL_DELETE (SYMBOL,DWC__GLOBALSYM)
		IF (IAND(IS,1).NE.0) IS = DWC_SYMBOLCLR
		GOTO 999
	ENDIF
C
C					If the value must be checked and
C					unknown symbols were found:
C					- repeat substitution to trap the error
C					Otherwise:
C					- the proper VALSTR is already there
C
	IF (DO_CHECK .AND. SWSYM) THEN
		IS = DWC_STR_SUBST (VALUE,WORK,LW,STREAM(:LS),ERRPTR,
	1							.FALSE.,SWSYM)
		GOTO 991
	ENDIF
C
C					Allocate memory for the value block
C
	IS = PV_BLK_ALLOC (VALSTR(:LVAL),VALBLK)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Decode and check the value
C
	IS = PV_BLK_DECODE (VALSTR(:LVAL),VALBLK,STREAM(:LS),
	1			.NOT.DO_CHECK,SWSYM,.FALSE.,DEFARR,0)
	IF (IAND(IS,1).EQ.0) GOTO 996
C
C					If full value check has been done:
C					- convert back to value string and
C					  append ASK qualifier to string
C					  (substitute qual no longer important)
C					Otherwise:
C					- return the original value string
C
	IF (DO_CHECK) THEN
		IS = PV_BLK_ENCODE (VALBLK,VALOUT,LOUT)
		IF (IAND(IS,1).EQ.0) GOTO 996
		IF (Q_ASK.GT.0) THEN
			IS = STR_COPY (' /ASK',VALOUT,LOUT)
		ELSE IF (Q_ASK.LT.0) THEN
			IS = STR_COPY (' /NOASK',VALOUT,LOUT)
		ENDIF
	ELSE
		VALOUT = VALUE
		LOUT = STR_SIGLEN (VALUE)
	ENDIF
C
C					Release virtual memory and return
C
	IS = PV_BLK_RELEASE (VALBLK)
	SP_DEF_CHECK = DWC_SUCCESS
	RETURN
C
C
 991	SP_DEF_CHECK = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,'keyword '//KEY(:LK),ERRPTR,WORK(:LW))
	RETURN
 992	SP_DEF_CHECK = MSG_SET (DWC_MANDATVAL,1)
	CALL WNCTXT(DWLOG,DWMSG,'/SUBSTITUTE')
	RETURN
 993	SP_DEF_CHECK = MSG_SET (DWC_MANDATVAL,1)
	CALL WNCTXT(DWLOG,DWMSG,'/NOSUBSTITUTE')
	RETURN
 994	SP_DEF_CHECK = MSG_SET (DWC_MULTIQUAL,1)
	CALL WNCTXT(DWLOG,DWMSG,'/CLEAR')
	RETURN
 995	SP_DEF_CHECK = MSG_SET (DWC_NOVALALL,1)
	CALL WNCTXT(DWLOG,DWMSG,'/CLEAR')
	RETURN
 996	SP_DEF_CHECK = IS
	IS = PV_BLK_RELEASE (VALBLK)
	RETURN
 999	SP_DEF_CHECK = IS
	RETURN
	END
