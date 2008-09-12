C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_NOMENU
C.Keywords:	Program Parameters, External Defaults, Specify
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900412 FMO - new code
C.Version:	920206 GvD - add former optional arguments to DWC_INPUT
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION SP_NOMENU (PROGNAM,STREAM,DO_LOCAL,DO_SUBST,
	1						IS_DCL,LINE,LL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	CHARACTER*(*)	STREAM		! (i) stream name
	LOGICAL*4	DO_LOCAL	! (i) only prompt for local defaults ?
	LOGICAL*4	DO_SUBST	! (i) symbol substitution requested ?
	LOGICAL*4	IS_DCL		! (o) DCL command given ?
	CHARACTER*(*)	LINE		! (o) last input line given
	INTEGER*4	LL		! (o) significant length of LINE
C
C.Purpose:	Let the user define external defaults	
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- The user can enter value definitions in the format:
C		<keyword> = <value_string>
C	  where <keyword> is the user's name for a parameter in the current
C	  program, and where the value string must obey the same rules as the
C	  answers to DWARF prompts.
C	- The values will be checked and if they are correct, the
C	  corresponding DWARF symbols will be defined.
C	- The user can get help information on all parameters by entering
C	  one or more question marks. He can also request help for a single
C	  parameter by entering question marks as the value.
C	- An empty answer (just pressing RETURN) signals the end of input.
C	- An answer starting with '$' is interpreted as a DCL command line.
C	  In that case, the function returns with IS_DCL set to .TRUE. and the
C	  command line in LINE(:LL).
C	- If an answer is found to be wrong in any way, the relevant messages
C	  will be written and the routine expects the next input line.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, DOLLAR, WILD, EQUAL
		PARAMETER (BLANK  = ' ')
		PARAMETER (DOLLAR = '$')
		PARAMETER (WILD   = '*')
		PARAMETER (EQUAL  = '=')
C
	INTEGER*4	SP_DEF_CHECK
	INTEGER*4	DWC_SYM_BUILD, DWC_HELP, DWC_INPUT
	INTEGER*4	PPD_HELP, PPD_READ_U, PPD_SSTR_GET, PPD_IOCD_GET
	INTEGER*4	MSG_SET
	INTEGER*4	STR_COPY_U, STR_SIGLEN, SYMBOL_DEFINE
C
	CHARACTER	WORK*255, SYMBOL*40, KEY*16
	CHARACTER	SSTR*5, GROUP*9, IOCD*8
	INTEGER*4	LW, LSYM, LK, LS, LG, LI
	INTEGER*4	IS, LEVEL, PTR
C
C
C					Accept next value definition
C
 100	IS = DWC_INPUT (LINE,BLANK,LL,0,0)
	IF (IAND(IS,1).EQ.0 .OR. LL.EQ.0) GOTO 900	! end of input
	IF (LINE(1:1).EQ.DOLLAR) GOTO 990		! DCL command
C
C					Help requested ?
C
	IS = DWC_HELP (LINE(:LL),-1,LEVEL)		! returns nr of '?'
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_KEYVAHELP) THEN
		IS = PPD_HELP (WILD,.FALSE.,.TRUE.,.FALSE.,LEVEL,6)
		IF (IAND(IS,1).EQ.0) GOTO 999
		GOTO 100
	ENDIF
C
C					Extract keyword
C
	KEY = BLANK
	LK = 0
	PTR = 1
	IS = STR_COPY_U (EQUAL,LINE(:LL),PTR,KEY,LK)
	IF (LK.EQ.0 .OR. PTR.GE.LL) GOTO 991		! no key or value
	PTR = PTR+1					! skip equal sign
	IF (LINE(PTR:PTR).EQ.BLANK) PTR = PTR+1		! skip blank
C
C					Read parameter description from PPD
C					- read by user's name
C					- only accept input-type parms
C
	IS = PPD_READ_U (KEY)				! key maybe expanded !!
	LK = STR_SIGLEN (KEY)
	IF (IS.EQ.PPD_KEYAMBIG) GOTO 992		! ambiguous abbrev
	IF (IAND(IS,1).EQ.0) GOTO 993				! unknown parameter
	IS = PPD_IOCD_GET (IOCD,LI)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (INDEX('MI',IOCD(:1)).EQ.0) GOTO 994		! no input parameter
C
C					Compose the symbol name
C
	IS = DWC_SYM_BUILD (PROGNAM,STREAM,KEY(:LK),SYMBOL,LSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Interpret the value
C
	IS = SP_DEF_CHECK (SYMBOL(:LSYM),LEVEL,1,LINE(PTR:LL),
	1						WORK,LW,DO_SUBST)
	IF (IS.EQ.DWC_KEYVAHELP) GOTO 100		! help given
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Can we specify a local default ?
C
	IF (DO_LOCAL) THEN
		IS = PPD_SSTR_GET (SSTR,LS,GROUP,LG)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (INDEX(SSTR(:LS),'L').EQ.0) GOTO 995
	ENDIF
C
C					Define the symbol
C
	IS = SYMBOL_DEFINE (SYMBOL(:LSYM),WORK(:LW),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	GOTO 100
C
 991	IS = MSG_SET (DWC_SPECWRSYN,0)
	GOTO 100
 992	IS = MSG_SET (PPD_KEYAMBIG,1)
	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
	GOTO 100
 993	IS = MSG_SET (DWC_UNKKEYW,1)
	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK),BLANK,PROGNAM)
	GOTO 100
 994	IS = MSG_SET (DWC_UNKKEYW,1)
	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK),' input-',PROGNAM)
	GOTO 100
 995	IS = MSG_SET (DWC_NOLOCVAL,1)
	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
	GOTO 100
 999	GOTO 100
C
C
 990	IS_DCL = .TRUE.
 900	SP_NOMENU = DWC_SUCCESS
	RETURN
	END
 
