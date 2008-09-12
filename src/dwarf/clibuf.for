C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CLI_BUF
C.Keywords:	Command Line Interpreter, Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Symbolic names for argument attributes:
C	CLI__PARAMETER	CLI__QUALIFIER	CLI__EXPRESSION
C	CLI__OPTIONAL	CLI__DEFAULT	CLI__REQUIRED
C	CLI__NOVALUE	CLI__VALUE
C		Common variables used:
C	INTEGER*4	CLI$NRARG			! nr of arguments
C	INTEGER*4	CLI$NRPAR			! nr of parameters
C	INTEGER*4	CLI$NREXP			! nr of expressions
C	INTEGER*4	CLI$NRQUA			! nr of qualifiers
C	CHARACTER	CLI$NAME(CLI__MXNR)*(CLI__LNAM)	! names (upper case)
C	INTEGER*4	CLI$ATTR(CLI__MXNR)		! attributes
C	INTEGER*4	CLI$STAT(CLI__MXNR)		! status codes
C	INTEGER*4	CLI$IDPRO(CLI__MXNR)		! prompt string ID's
C	INTEGER*4	CLI$IDDEF(CLI__MXNR)		! default value ID's
C	INTEGER*4	CLI$IDVAL(CLI__MXNR)		! value string ID's
C		where CLI__MXNR = 20 and CLI__LNAM = 16
C
C.Version:	900420 FMO - recreation
C.Version:	910913 FMO - allow up to 20 arguments (was 10)
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940209 CMV - Print message for unknown qualifier in CLI.FOR
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
	INTEGER*4	NRARG		! (i) nr of arguments
	CHARACTER*(*)	NAME(*)		! (i) names (case insenstive)
	INTEGER*4	ATTR(*)		! (i) attributes
	CHARACTER*(*)	PROMPT(*)	! (i) prompt strings
	CHARACTER*(*)	DEFVAL(*)	! (i) default values
C
C.Purpose:	Initialize the CLI buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_CLISYNTAX	any error -> ABORT
C.Notes:
C	- The command-line syntax, as described in the notes of CLI_INIT, is
C	  checked and stored in the CLI buffer.
C	- Any error must be a programming error and leads to program abort.
C	- The argument status is initialized at DWC_PRESENT for all qualifiers
C	  with the CLI__DEFAULT attribute and for all parameters with a default
C	  value, and at DWC_ABSENT for all other arguments.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	CLI_STR_INIT, CLI_STR_PUT
	INTEGER*4	STR_SIGLEN, STR_UPCASE
	INTEGER		MSG_SET
C
	INTEGER*4	IS, LN, LP, LD
	LOGICAL*4	VALUE, DEFAULT, REQUIRED
C
C
	IF (NRARG.LT.0 .OR. NRARG.GT.CLI__MXNR) THEN
		IS = 4
		CALL WNCTXT(DWLOG,'Invalid number of arguments: !SJ',NRARG)
		GOTO 999
	ENDIF
C
C					Initialize the buffers
C
	CLI$NRARG = NRARG
	CLI$NRPAR = 0
	CLI$NREXP = 0
	CLI$NRQUA = 0
	DO I = 1,CLI__MXNR
		CLI$NAME(I) = BLANK
		CLI$ATTR(I) = 0
		CLI$STAT(I) = DWC_ABSENT
		CLI$IDPRO(I) = 0
		CLI$IDDEF(I) = 0
		CLI$IDVAL(I) = 0
	ENDDO
	IS = CLI_STR_INIT ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Check the command-line syntax
C					and fill the buffers
C
	DO I = 1,NRARG
C
		LN = STR_SIGLEN (NAME(I))
		IF (LN.EQ.0) THEN
			IS = 4
			CALL WNCTXT(DWLOG,'Argument !SJ has no name',I)
			GOTO 999
		ENDIF
		CLI$NAME(I) = NAME(I)
		IS = STR_UPCASE (CLI$NAME(I))
		LP = STR_SIGLEN (PROMPT(I))
		LD = STR_SIGLEN (DEFVAL(I))
C
		VALUE    = IAND(ATTR(I),CLI__VALUE).NE.0
		REQUIRED = IAND(ATTR(I),CLI__REQUIRED).NE.0
		DEFAULT  = .NOT.REQUIRED .AND. IAND(ATTR(I),CLI__DEFAULT).NE.0
C
C					Qualifier definition
C
		IF (IAND(ATTR(I),CLI__QUALIFIER).NE.0) THEN
			CLI$ATTR(I) = CLI__QUALIFIER
			CLI$NRQUA = CLI$NRQUA+1
			IF (VALUE) THEN
				CLI$ATTR(I) = CLI$ATTR(I)+CLI__VALUE
				IF (LD.GT.0) THEN
				    IS = CLI_STR_PUT (CLI$IDDEF(I),DEFVAL(I),LD)
				ELSE IF (DEFAULT) THEN
				    IS = 4
				    CALL WNCTXT(DWLOG,'Qualifier !AS: '//
	1			    'default value required',4,0,NAME(I))
				ENDIF
			ELSE
				IF (LD.GT.0) THEN
				    IS = 4
				    CALL WNCTXT(DWLOG,'Qualifier !AS: '//
	1			    'no default value allowed',NAME(I))
				ENDIF
			ENDIF
			IF (DEFAULT) THEN
				CLI$ATTR(I) = CLI$ATTR(I)+CLI__DEFAULT
				CLI$STAT(I) = DWC_PRESENT
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Parameter or expression
C
		ELSE
			IF (IAND(ATTR(I),CLI__EXPRESSION).NE.0) THEN
				IF (I.NE.1) THEN
				    IS = 4
				    CALL WNCTXT(DWLOG,'Expression !AS: '//
	1			    'must be the first argument',NAME(I))
				ENDIF
				CLI$ATTR(I) = CLI__EXPRESSION
				CLI$NREXP = 1
				CLI$NRPAR = 1
			ELSE
				IF (CLI$NREXP.GT.0) THEN
				    IS = 4
				    CALL WNCTXT(DWLOG,'Parameter !AS: '//
	1			    'not allowed after expression',NAME(I))
				ELSE IF (CLI$NRQUA.GT.0) THEN
				    IS = 4
				    CALL WNCTXT(DWLOG,'Parameter !AS: '//
	1			    'must precede qualifiers',NAME(I))
				ENDIF
				CLI$ATTR(I) = CLI__PARAMETER
				CLI$NRPAR = CLI$NRPAR+1
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
C
C					- if a default value is given:
C					  put it into the value buffer
C					  and set status to PRESENT
C
			IF (LD.GT.0) THEN
				CLI$STAT(I) = DWC_PRESENT
				IS = CLI_STR_PUT (CLI$IDVAL(I),DEFVAL(I),LD)
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		ENDIF
C
C					For required arguments:
C					- the name will be used as prompt
C					  if no prompt string was provided
C
		IF (REQUIRED) THEN
			CLI$ATTR(I) = CLI$ATTR(I)+CLI__REQUIRED
			IF (LP.GT.0) THEN
				IS = CLI_STR_PUT (CLI$IDPRO(I),PROMPT(I),LP)
			ELSE
				IS = CLI_STR_PUT (CLI$IDPRO(I),NAME(I),LN)
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
C
	ENDDO
C
	CLI_BUF_INIT = DWC_SUCCESS
	RETURN
C
C					Fatal error found: abort program
C
 999	CLI_BUF_INIT = MSG_SET (DWC_CLISYNTAX,0)
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_PUTPAR (NR,VAL,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
	INTEGER*4	NR		! (i) parameter nr
	CHARACTER*(*)	VAL	 	! (i) value
	INTEGER*4	LVAL		! (i) significant length of the value
C
C.Purpose:	Store or clear the parameter value and set its status
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CLIBUFERR	error report left in message buffer
C.Notes:
C	- If LVAL > 0, the value will be stored and the status will be set to
C	  DWC_PRESENT.
C	- Otherwise, any old value will be cleared and the status will be
C	  set to DWC_ABSENT.
C-------------------------------------------------------------------------
C
	INTEGER*4	CLI_STR_PUT
	INTEGER		MSG_SET
C
	INTEGER*4	IS
C
C
	IF (NR.GT.CLI$NRPAR) THEN
		IS = MSG_SET (DWC_CLIPARUNK,1)
		CALL WNCTXT(DWLOG,DWMSG,NR)
		GOTO 999
	ENDIF
C
	IS = CLI_STR_PUT (CLI$IDVAL(NR),VAL,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LVAL.GT.0) THEN
		CLI$STAT(NR) = DWC_PRESENT
	ELSE
		CLI$STAT(NR) = DWC_ABSENT
	ENDIF
C
C
	CLI_BUF_PUTPAR = DWC_SUCCESS
	RETURN
C
 999	CLI_BUF_PUTPAR = DWC_CLIBUFERR
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_PUTQUAL (KEY,VAL,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
	CHARACTER*(*)	KEY	 	! (i) qualifier key (case insensitive)
	CHARACTER*(*)	VAL	 	! (i) value
	INTEGER*4	LVAL		! (i) significant length of the value
C
C.Purpose:	Store or clear the value of a qualifier and set its status
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CLIBUFERR	error report left in message buffer
C.Notes:
C	- If KEY is a unique abbreviation of a qualifier name, the status will
C	  be set to DWC_PRESENT. If LVAL > 0, VAL(:LVAL) will be stored.
C	- If KEY is 'NO' followed by an (abbreviated) qualifier name, the
C	  status will be set to DWC_NEGATED.
C	- In both cases any old value will be cleared.
C-------------------------------------------------------------------------
C
	INTEGER*4	CLI_STR_PUT, STR_MATCH_A, STR_UPCASE
	INTEGER		MSG_SET
C
	CHARACTER	UPKEY*(CLI__LNAM+2)
	INTEGER*4	IS, NR
C
C
	UPKEY = KEY
	IS = STR_UPCASE (UPKEY)
C
C					KEY = name :
C					- qualifier present
C					- check whether a value is allowed
C					- store or clear the value
C
	IS = STR_MATCH_A (UPKEY,CLI$NRQUA,CLI$NAME(CLI$NRPAR+1),NR)
	IF (IAND(IS,1).NE.0) THEN
		NR = CLI$NRPAR+NR
		IF (IAND(CLI$ATTR(NR),CLI__VALUE).NE.0) THEN
			IF (LVAL.LE.0 .AND. CLI$IDDEF(NR).EQ.0) THEN
				IS = MSG_SET(DWC_QUALNOVAL,1)
			ELSE
				IS = CLI_STR_PUT (CLI$IDVAL(NR),VAL,LVAL)
			ENDIF
		ELSE IF (LVAL.GT.0) THEN
			IS = MSG_SET (DWC_QUALVALNA,1)
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 999
		CLI$STAT(NR) = DWC_PRESENT
C
C					KEY = NOname:
C					- qualifier negated
C					- no value allowed
C					- clear the value
C
	ELSE
	    IF (UPKEY(:2).EQ.'NO') IS = STR_MATCH_A (UPKEY(3:),
	1				CLI$NRQUA,CLI$NAME(CLI$NRPAR+1),NR)
	    IF (IAND(IS,1).NE.0) THEN
		NR = CLI$NRPAR+NR
		IF (LVAL.LE.0) THEN
			IS = CLI_STR_PUT (CLI$IDVAL(NR),VAL,LVAL)
		ELSE
			IS = MSG_SET (DWC_QUALVALNA,1)
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 999
		CLI$STAT(NR) = DWC_NEGATED
C
C					Otherwise:
C					- invalid key
C
	    ELSE
		IF (NR.GT.0) IS = MSG_SET (DWC_AMBQUAL,1)
	        IF (NR.LE.0) IS = MSG_SET (DWC_UNKQUAL,1)
		GOTO 999
	    ENDIF
	ENDIF
C
C
	CLI_BUF_PUTQUAL = DWC_SUCCESS
	RETURN
C
 999	CLI_BUF_PUTQUAL = DWC_CLIBUFERR
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_GET (NAME,VAL,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
C
	CHARACTER*(*)	NAME	 	! (i) argument name (case insensitive)
	CHARACTER*(*)	VAL	 	! (o) value
	INTEGER*4	LVAL		! (o) significant length of the value
C
C.Purpose:	Get status and value of an argument from the buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_PRESENT	argument is present
C	success	DWC_NEGATED	qualifier is negated
C	success	DWC_ABSENT	optional argument is absent
C	success	DWC_REQUIRED	required argument is absent
C	error	DWC_CLIBUFERR	error report left in message buffer
C.Notes:
C	- NAME can be a uniquely abbreviated parameter or qualifier name.
C	- If the argument is present, any value (maybe the default value) will
C	  be returned in VAL. LVAL = 0 means that no value is available.
C	- If the argument is absent but required, the status DWC_REQUIRED will
C	  be returned and the prompt string and its length will be returned in
C	  VAL and LVAL.
C	- In all other cases VAL will be blank and LVAL = 0.
C	- Parameter (default) values are used only once: after each GET for a
C	  parameter, its value will be removed from the buffer and the status
C	  in the buffer will be set to DWC_ABSENT. Qualifier values and
C	  status's are not modified by GET.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	CLI_STR_GET, CLI_STR_PUT
	INTEGER		STR_UPCASE, STR_MATCH_A, MSG_SET
C
	CHARACTER	UPNAME*(CLI__LNAM)
	INTEGER*4	STAT, IS, NR
C
C
C					Initialize output arguments
C
	VAL = BLANK
	LVAL = 0
C
C					Check the argument name
C
	UPNAME = NAME
	IS = STR_UPCASE (UPNAME)
	IS = STR_MATCH_A (UPNAME,CLI$NRARG,CLI$NAME,NR)
	IF (IAND(IS,1).EQ.0) THEN
		IF (NR.GT.0) IS = MSG_SET (DWC_CLINAMAMB,1)
		IF (NR.EQ.0) IS = MSG_SET (DWC_CLINAMUNK,1)
		IF (NR.GE.0) CALL WNCTXT(DWLOG,DWMSG,NAME)
		GOTO 999
	ENDIF
C
C					Fill status and value arguments
C
	STAT = CLI$STAT(NR)
	IF (STAT.EQ.DWC_PRESENT) THEN
		IF (CLI$IDVAL(NR).GT.0) THEN
			IS = CLI_STR_GET (CLI$IDVAL(NR),VAL,LVAL)
		ELSE IF (CLI$IDDEF(NR).GT.0) THEN
			IS = CLI_STR_GET (CLI$IDDEF(NR),VAL,LVAL)
		ENDIF
	ELSE IF (STAT.EQ.DWC_ABSENT) THEN
		IF (IAND(CLI$ATTR(NR),CLI__REQUIRED).NE.0) THEN
			STAT = DWC_REQUIRED
			IS = CLI_STR_GET (CLI$IDPRO(NR),VAL,LVAL)
		ENDIF
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Parameter and expression values
C					are only used once:
C					- set buffer status to absent
C					- clear the value in the buffer
C
	IF (NR.LE.CLI$NRPAR .AND. STAT.EQ.DWC_PRESENT) THEN
		CLI$STAT(NR) = DWC_ABSENT
		IS = CLI_STR_PUT (CLI$IDVAL(NR),BLANK,0)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	CLI_BUF_GET = STAT
	RETURN
C
 999	CLI_BUF_GET = DWC_CLIBUFERR
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_GETCOM (COMMAND,LCOM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
	CHARACTER*(*)	COMMAND	 	! (o) full command string
	INTEGER*4	LCOM		! (o) significant length of the string
C
C.Purpose:	Get the complete argument string from the buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- All present arguments and negated qualifiers will be assembled.
C	- Qualifiers are given with their full names preceded by '/' or '/NO').
C	- Qualifier values are added with a '=' sign.
C	- If the output string is too short the command will be truncated.
C	- Since parameter value disappear from the buffer with a CLI_BUF_GET,
C	  CLI_BUF_GETCOM only gives the complete argument string when it is
C	  called before any parameter GET.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, SLASH, EQUAL
		PARAMETER (BLANK = ' ')
		PARAMETER (SLASH = '/')
		PARAMETER (EQUAL = '=')
C
	INTEGER*4	CLI_STR_GET, STR_COPY, STR_SIGLEN
C
	CHARACTER	QKEY*(CLI__LNAM+3), VAL*255
	INTEGER*4	IS, LK, LV, NR
C
C
	COMMAND = BLANK
	LCOM = 0
C
C					Assemble present parameters
C
	DO NR = 1,CLI$NRPAR
		IF (CLI$STAT(NR).EQ.DWC_PRESENT) THEN
			IS = CLI_STR_GET (CLI$IDVAL(NR),VAL,LV)
			IS = STR_COPY (VAL(:LV)//BLANK,COMMAND,LCOM)
		ENDIF
	ENDDO
C
C					Assemble present/negated qualifiers
C
	DO NR = CLI$NRPAR+1,CLI$NRARG
		IF (CLI$STAT(NR).EQ.DWC_PRESENT) THEN
			QKEY = SLASH//CLI$NAME(NR)
			LK = STR_SIGLEN (QKEY)
			IF (CLI$IDVAL(NR).GT.0) THEN
				IS = CLI_STR_GET (CLI$IDVAL(NR),VAL,LV)
				IS = STR_COPY (QKEY(:LK)//EQUAL//VAL(:LV),
	1							COMMAND,LCOM)
			ELSE IF (CLI$IDDEF(NR).GT.0) THEN
				IS = CLI_STR_GET (CLI$IDDEF(NR),VAL,LV)
				IS = STR_COPY (QKEY(:LK)//EQUAL//VAL(:LV),
	1							COMMAND,LCOM)
			ELSE
				IS = STR_COPY (QKEY(:LK),COMMAND,LCOM)
			ENDIF
		ELSE IF (CLI$STAT(NR).EQ.DWC_NEGATED) THEN
			QKEY = SLASH//'NO'//CLI$NAME(NR)
			LK = STR_SIGLEN (QKEY)
			IS = STR_COPY (QKEY(:LK),COMMAND,LCOM)
		ENDIF
	ENDDO
C
	CLI_BUF_GETCOM = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_BUF_GETNRS (NRPAR,NREXP,NRQUA)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CLI_1_DEF'
C
	INTEGER*4	NRPAR		! (o) nr of parameters
	INTEGER*4	NREXP		! (o) nr of expressions
	INTEGER*4	NRQUA		! (o) nr of qualifiers
C
C.Purpose:	Get the numbers of defined arguments
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	NRPAR = CLI$NRPAR
	NREXP = CLI$NREXP
	NRQUA = CLI$NRQUA
C
	CLI_BUF_GETNRS = DWC_SUCCESS
	RETURN
	END
