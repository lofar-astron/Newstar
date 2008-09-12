C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_INP
C.Keywords:	Program Parameters, Get Value, Input
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	910826 FMO - in case of non-terminal input (DEVCOD=3), abort
C			the program if a wrong answer is received
C.Version:	920206 GvD - add former optional arguments to CLI_GET, etc.
C.Version:	940120 CMV - Changed messenger
C		941019 JPH - prefix default with '|'line delimiter 
C.Version:	010709 AXC - linux port,tmpchar in string calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INP (DEFAULT,LDEF,DLEVEL,DEFARR,NRDEF,
	1					VALBLK,DO_NOASK,DO_SAVE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DEFAULT		! (i) default string
	INTEGER*4	LDEF		! (i) its length
	INTEGER*4	DLEVEL		! (m) help-level difference
	BYTE		DEFARR(*)	! (i) default array
	INTEGER*4	NRDEF		! (i) nr of values in DEFARR
	INTEGER*4	VALBLK(8)	! (o) value block descriptor
	LOGICAL*4	DO_NOASK	! (o) /NOASK given ?
	LOGICAL*4	DO_SAVE		! (o) /SAVE given ?
C
C.Purpose:	Get user input and convert it to a value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also if no value is given
C	warning	DWC_EOFCTRLZ	CTRL/Z or # answer
C	fatal	DWC_GETINPERR	error from DWC_INPUT
C	error	DWC_PARWRANS	parameter input file error (program aborted)
C	false status codes returned by referenced routines
C.Notes:
C	- VALBLK(1) = 0 means that the user just gave a <return>.
C-------------------------------------------------------------------------
C
	INTEGER*4	GP_INP_GET, GP_INP_PARSE, GP_INP_DECODE
	INTEGER*4	GP_CTL_RESET
	INTEGER		DWC_SYSIN_GET, MSG_SET
	INTEGER*4	DWC_PROG_GET, DWC_STREAM_GET, DWC_SYM_BUILD
	INTEGER*4	PPD_UNAM_GET
C
	CHARACTER*255	ANSWER, VALSTR
	CHARACTER*16	PROGNAM, STREAM, KEY, ASKKEY, SYMBOL*50
	INTEGER*4	IS, LANS, LVAL, LP, LS, LK, LKMIN, LASK, LSYM
	INTEGER		DEVCOD
	LOGICAL*4	PROTO
C
C
	IS = DWC_SYSIN_GET (DEVCOD)
	IF (IAND(IS,1).NE.0) IS = DWC_PROG_GET (PROGNAM,LP)
	IF (IAND(IS,1).NE.0) IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
	IF (IAND(IS,1).NE.0) IS = PPD_UNAM_GET (KEY,LK,LKMIN,PROTO)
	IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD
	1		(PROGNAM(:LP),STREAM(:LS),KEY(:LK),SYMBOL,LSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get input from the user
C
 100	DO_NOASK = .FALSE.
	DO_SAVE = .FALSE.
	IS = GP_INP_GET (SYMBOL(:LSYM),DEFAULT,LDEF,DLEVEL,DEVCOD,
	1						ANSWER,LANS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Parse the answer string
C					- substitute symbols
C					- if help request: increment help level,
C					  possibly display full PPD help info,
C					  and ask again (if possible)
C					- analyse any qualifiers
C
	IS = GP_INP_PARSE (ANSWER(:LANS),DLEVEL,VALSTR,LVAL,ASKKEY,LASK,
	1						DO_NOASK,DO_SAVE)
	IF (IAND(IS,1).EQ.0) THEN
	    IF (IS.EQ.DWC_KEYVAHELP) GOTO 100
	    IS = MSG_SET (DWC_PARWRANS,1)
	    CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
	    IF (DEVCOD.EQ.3) GOTO 991
	    GOTO 100
	ENDIF
C
C					Act upon /ASK=keyword and ask again
C
	IF (LASK.GT.0) THEN
	    IS = GP_CTL_RESET (ASKKEY(:LASK))
	    IF (IAND(IS,1).EQ.0) THEN
		IS = MSG_SET (DWC_PARWRANS,1)
		CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
		IF (DEVCOD.EQ.3) GOTO 991
		GOTO 100
	    ENDIF
	ENDIF
C
C					Convert value string to value block
C
	IF (LVAL.GT.0) THEN
	    IS = GP_INP_DECODE (VALSTR(:LVAL),STREAM(:LS),VALBLK,
	1						DEFARR,NRDEF)
	    IF (IAND(IS,1).EQ.0) THEN
		IS = MSG_SET (DWC_PARWRANS,1)
		CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
		IF (DEVCOD.EQ.3) GOTO 991
		GOTO 100
	    ENDIF
	ENDIF
C
	GP_INP = DWC_SUCCESS
	RETURN
C
 991	E_C = DWC_PARWRANS
	CALL WNCTXT(DWLOG,'Error in parameter input file')
	CALL WNGEX
 999	GP_INP = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INP_GET (SYMBOL,DEFAULT,LDEF,DLEVEL,DEVCOD,
	1						ANSWER,LANS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) full parameter name
	CHARACTER*(*)	DEFAULT		! (i) default set
	INTEGER*4	LDEF		! (i) significant length of DEFAULT
	INTEGER*4	DLEVEL		! (i) userlevel difference
	INTEGER		DEVCOD		!(i) input device code
	CHARACTER*(*)	ANSWER		! (o) value set(s) (and qualifiers)
	INTEGER*4	LANS		! (o) significant length of ANSWER
C
C.Purpose:	Get new value set(s) for a program parameter from the user
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_EOFCTRLZ	end-of-input (= end-of-loop)
C	fatal	DWC_GETINPERR	I/O error (message holded)
C	fatal	DWC_KEYWMISM	parameter input file error (program aborted)
C	false status codes returned by referenced routines
C.Notes:
C	- The format of the prompt string depends on the current userlevel,
C	  but if there is a default set, it will be included.
C
C	The user can give several answers:
C	- ? to get help (the prompt will be repeated);
C	- a value string with or without qualifiers /NOASK and /(NO)SAVELAST;
C	- CTRL/Z or # to signal the end of input;
C	- /ASK=keyword to force prompting for that program parameter.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, EQUAL, UNDERSC, WHITE, EQUALS
		PARAMETER (BLANK   = ' '  )
		PARAMETER (TAB     = '	')
		PARAMETER (EQUAL   = '='  )
		PARAMETER (UNDERSC = '_' )
		PARAMETER (WHITE   = BLANK//TAB)
		PARAMETER (EQUALS  = BLANK//EQUAL//BLANK)
	INTEGER*4	BELL
	LOGICAL*4	SWITCH
		PARAMETER (BELL   = 1      )	! sound bell if it is active
		PARAMETER (SWITCH = .FALSE.)	! dummy switch
C
	INTEGER*4	DWC_SYM_SPLIT
	INTEGER*4	DWC_PRCMODE_INQ, DWC_LEVEL_GET
	INTEGER*4	PPD_PROMPT, DWC_INPUT
	INTEGER*4	STR_COPY, STR_SIGLEN, STR_SKIP_W, STR_CHECK_ANUMX
	INTEGER		MSG_SET
C
	CHARACTER	PROMPT*255, WORK*20, PROGNAM*16, STREAM*16, KEY*16
	CHARACTER	TMP*80
	INTEGER*4	IS, LPR, LW, LP, LS, LK, PTR
	INTEGER*4	CURLEVEL, MAXLEVEL
C
C
C					Split the symbol name, get the type
C					of input device and the userlevel
C
	IS = DWC_SYM_SPLIT (SYMBOL,PROGNAM,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).NE.0) IS = DWC_LEVEL_GET (CURLEVEL,MAXLEVEL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Compose the prompt string
C					- full symbol name for subprocess
C					- keyword only for main process
C					- append possible default set
C
	IF (IAND(DWC_PRCMODE_INQ('SUBPROCESS'),1) .NE. 0) THEN
		IS = PPD_PROMPT (PROGNAM(:LP)//STREAM(:LS)//UNDERSC,
	1				CURLEVEL+DLEVEL,SWITCH,PROMPT,LPR)
	ELSE
		IS = PPD_PROMPT (BLANK,CURLEVEL+DLEVEL,SWITCH,PROMPT,LPR)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
	TMP=EQUALS//DEFAULT(:LDEF)
	IF (LDEF.GT.0) IS = STR_COPY (TMP,PROMPT,LPR)
C
C					Ask the user
C
 100	IS = DWC_INPUT (ANSWER,PROMPT(:LPR),LANS,DEVCOD,BELL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Handle a <keyword>=<value> answer
C					- check keyword and extract value
C					Keyword mismatch:
C					- if input from a parameter file,
C					  abort the program (no correction
C					  is possible)
C					- if input from a terminal (e.g.
C					  type-ahead), ask again
C
	PTR = INDEX (ANSWER(:LANS),EQUAL)
	IF (PTR.GT.0) THEN
		WORK = ANSWER(:PTR-1)
		LW = STR_SIGLEN (WORK)
		IF (LW.LE.LK .AND. IAND(STR_CHECK_ANUMX(WORK(:LW)),1).NE.0) THEN
			IF (WORK(:LW).NE.KEY(:LK)) THEN
				IF (DEVCOD.EQ.3) GOTO 991
				IS = MSG_SET (DWC_KEYWMISM,1)
				CALL 
	1			    WNCTXT(DWLOG,DWMSG,BLANK,WORK(:LW),KEY(:LK))
				GOTO 100
			ENDIF
			IS = STR_SKIP_W (WHITE//EQUAL,ANSWER(:LANS),PTR)
			ANSWER = ANSWER(PTR:LANS)
			LANS = LANS-PTR+1
		ENDIF
	ENDIF
C
C
	IF (ANSWER.EQ.'#') THEN
		GP_INP_GET = DWC_EOFCTRLZ
	ELSE
		GP_INP_GET = DWC_SUCCESS
	ENDIF
	RETURN
C
C
 991	E_C = MSG_SET(DWC_KEYWMISM,1)
	CALL WNCTXT(DWLOG,DWMSG,' in parameter input file',WORK(:LW),KEY(:LK))
	CALL WNGEX
 999	GP_INP_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INP_PARSE (ANSWER,DLEVEL,VALSTR,LVAL,
	1					ASKKEY,LASK,DO_NOASK,DO_SAVE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	ANSWER		! (i) value set(s) (and qualifiers)
	INTEGER*4	DLEVEL		! (m) userlevel difference
	CHARACTER*(*)	VALSTR		! (o) value set(s) (without qualifiers)
	INTEGER*4	LVAL		! (o) significant length of VALSTR
	CHARACTER*(*)	ASKKEY		! (o) parameter to be ASK-ed
	INTEGER*4	LASK		! (o) significant length of ASKKEY
	LOGICAL*4	DO_NOASK	! (o) /NOASK requested ?
	LOGICAL*4	DO_SAVE		! (o) SAVE requested ?
C
C.Purpose:	Parse the user's input string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_KEYVAHELP	help request is given
C	error status from referenced routines (messages holded)
C.Notes:
C	- substitute the symbols between apostrophes;
C	- give help if requested (question marks);
C	- interprete and remove the qualifiers from the string;
C-------------------------------------------------------------------------
C
	INTEGER*4	NRARG, EXPR, QVAL, Q
		PARAMETER (NRARG = 3)
		PARAMETER (EXPR  = CLI__EXPRESSION)
		PARAMETER (QVAL  = CLI__QUALIFIER+CLI__VALUE)
		PARAMETER (Q     = CLI__QUALIFIER)
C
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STREAM_GET, DWC_SAVE_INQ, DWC_STR_SUBST, DWC_HELP
	INTEGER		MSG_SET
C
	CHARACTER*8	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR','ASK','SAVELAST'/
		DATA ATTR   / EXPR   , QVAL, Q        /
		DATA PROMPT /' '     ,' '  ,' '       /
		DATA DEFVAL /' '     ,' '  ,' '       /
C
	CHARACTER	STREAM*16, DUM*1
	INTEGER*4	IS, LS, LD, ERRPTR
	LOGICAL*4	SWSYM
C
C
C					Substitute symbols
C					(no unknown symbols allowed)
C
	SWSYM = .FALSE.
	IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = DWC_STR_SUBST (ANSWER,VALSTR,LVAL,STREAM(:LS),ERRPTR,
	1						.FALSE.,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
C					If help request:
C					- increment help level with nr of
C					  question marks given (only for this
C					  GET_PARM unless /HOLD was added)
C					- print PPD help if helplevel > beginner
C					- return with warning status
C
	IS = DWC_HELP (VALSTR(:LVAL),0,DLEVEL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the string
C					- reset the Command-Line Interpreter
C					- parse the string
C					- extract the pure value string
C
	IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).NE.0) IS = CLI_PARSE (VALSTR(:LVAL))
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',VALSTR,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the qualifiers
C					- if /ASK=<keyword> is given: return
C					  (no value specification is allowed)
C					- otherwise: set NOASK and SAVE flags
C
	IS = CLI_GET ('ASK',ASKKEY,LASK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		IF (LVAL.GT.0) GOTO 992			! no value allowed
	ELSE
		DO_NOASK = IS.EQ.DWC_NEGATED
		IS = CLI_GET ('SAVELAST',DUM,LD)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_SAVE = IS.EQ.DWC_PRESENT .OR.
	1		 (IS.EQ.DWC_ABSENT .AND. IAND(DWC_SAVE_INQ(),1).NE.0)
	ENDIF
C
C
	GP_INP_PARSE = DWC_SUCCESS
	RETURN
C
 991	GP_INP_PARSE = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,' ',ERRPTR,VALSTR(:LVAL))
	RETURN
 992	GP_INP_PARSE = MSG_SET (DWC_NOVALALL,1)
	CALL WNCTXT(DWLOG,DWMSG,'ASK')
	RETURN
 999	GP_INP_PARSE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INP_DECODE (VALSTR,STREAM,VALBLK,
	1						DEFARR,NRDEF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	VALSTR		! (i) value string
	CHARACTER*(*)	STREAM		! (i) stream name (for substitution)
	INTEGER*4	VALBLK(8)	! (i) value block descriptor
	BYTE		DEFARR(*)	! (i) default array
	INTEGER*4	NRDEF		! (i) nr of values in DEFARR
C
C.Purpose:	Analyse the value string and convert to a value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_DECODE, PV_BLK_RELEASE
C
	INTEGER*4	IS
	LOGICAL*4	SWSYM
C
C
C					Check and convert the value to an array
C					- no unknown symbols allowed
C
	IS = PV_BLK_ALLOC (VALSTR,VALBLK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (VALBLK(1).NE.0) THEN
		SWSYM = .FALSE.
		IS = PV_BLK_DECODE (VALSTR,VALBLK,STREAM,.FALSE.,SWSYM,
	1						.FALSE.,DEFARR,NRDEF)
		IF (IAND(IS,1).EQ.0) GOTO 991
	ENDIF
C
	GP_INP_DECODE = DWC_SUCCESS
	RETURN
C
 991	GP_INP_DECODE = IS
	IS = PV_BLK_RELEASE (VALBLK)
	RETURN
C
 999	GP_INP_DECODE = IS
	RETURN
	END
