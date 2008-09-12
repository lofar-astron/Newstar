C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CLI
C.Keywords:	DWARF, Command Line Interpreter
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		DWARF COMMAND LINE INTERPRETER (CLI)
C		------------------------------------
C
C	The CLI facility is used by all DWARF-system programs to analyse the
C	arguments in the primary command line, and by DWARF's program-parameter
C	interface to preprocess all its input strings.
C
C	Syntax definition
C	-----------------
C	A program or routine must start a sequence of CLI operations with a 
C	call to CLI_INIT or CLI_RESET to clear the internal buffers and to 
C	inform the facility about the number, names and attributes of the 
C	arguments to be expected. When CLI_INIT is called for the first time, 
C	it fetches any arguments given in the primary command line and stores 
C	them. The syntax can be redefined at any time, but all previous 
C	information will then be lost.
C
C	CLI input
C	---------
C	The CLI usually acquires input when it needs it (see CLI_INIT and 
C	CLI_GET) and analyses it via internal calls to CLI_PARSE. In addition, 
C	any routine can send its own input lines to the CLI by explicitly
C	calling CLI_PARSE.
C
C	CLI output
C	----------
C	The CLI is questioned about arguments via calls to CLI_GET. When 
C	CLI_GET finds that a required argument is missing, it will prompt for 
C	additional input (provided that the standard input device is a 
C	terminal), append the answer to the current command line, analyse the 
C	new line, and return the requested information.
C
C.Version:	900426 FMO - recreation
C.Version:	910913 FMO - allow up to 20 arguments (was 10)
C.Version:	910930 GvD - allow values starting with a slash
C.Version:	911022 GvD - fixed little bug when unknown qualifier is given
C.Version:	920206 GvD - no optional arguments anymore in CLI_GET
C.Version:	930312 FMO - LINE buffer 255 --> 1024
C.Version:      930923 HjV - LINE buffer  in CLI_GET 80 --> 255
C.Version:	940209 CMV - Print message for unknown qualifier here
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_INIT (NR,NAME,ATTR,PROMPT,DEFVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NR		! (i) nr of arguments
	CHARACTER*(*)	NAME(*)		! (i) names (case insensitive)
	INTEGER*4	ATTR(*)		! (i) attributes
	CHARACTER*(*)	PROMPT(*)	! (i) prompt strings
	CHARACTER*(*)	DEFVAL(*)	! (i) default values
C
C.Purpose:	Initialise the Command Line Interpreter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes from referenced routines
C.Notes:
C	- The command-line syntax is defined by means of the input arrays.
C	- When the program calls CLI_INIT for the first time, the routine reads
C	  any arguments given with the command that started up the program.
C
C 	- NR gives the number of arguments to be defined (up to 20).
C
C	- NAME defines the names of the arguments. A name cannot be empty
C	  or longer than 16 characters (trailing blanks and tabs are ignored)
C	  and will be converted to upper case for internal use.
C
C	- ATTR describes the arguments as the sum of the 3 basic attributes,
C	  "type", "presence" and "value", for which symbolic names are defined 
C	  in the Fortran include module 'CONSTANTS_1_DEF':
C
C	  "Type" can be either
C		CLI__PARAMETER (default), CLI__EXPRESSION, or CLI__QUALIFIER.
C	  Only the first element in the definition arrays can be an expression-
C	  type parameter, and no other parameters are allowed in that case.
C	  All parameter arguments must preceed the qualifier arguments in the
C	  definition arrays.
C
C 	  "Presence" can be either
C		CLI__OPTIONAL (default), CLI__REQUIRED, or CLI__DEFAULT.
C	  A default qualifier is assumed to be present until it is negated via
C	  /NOname. A parameter is assumed to be present by default when a 
C	  default value has been defined. Any parameter value disappears as
C	  soon as it has been fetched via a CLI_GET call; from then on, the
C	  parameter is either optional or required.
C
C	  "Value" (only used for qualifiers) can be either
C		CLI__NOVALUE (default), or CLI__VALUE.
C	  For a qualifier with the VALUE attribute a value must always be
C	  available when the qualifier is present (as opposed to negated).
C
C	- PROMPT defines prompt strings for the arguments (trailing blanks and
C	  tabs are ignored). If no prompt string is defined for a required
C	  argument, its name will be used as such.
C
C	- DEFVAL defines default values for the arguments (trailing blanks and
C	  tabs are ignored). A default value is:
C	  	required for qualifiers with both CLI__VALUE and CLI__DEFAULT,
C	  	not allowed for qualifiers with CLI__NOVALUE, and
C	  	allowed in all other cases.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	CLI_PARSE, CLI_BUF_INIT
	INTEGER*4	GEN_GETFOR
C
	CHARACTER*1024	LINE
	INTEGER*4	IS, LL
C
	LOGICAL*4	FIRST
		SAVE FIRST
		DATA FIRST /.TRUE./
C
C
C					Initialize the CLI buffer
C
	IS = CLI_BUF_INIT (NR,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get foreign command and parse it
C					- don't ask if none found
C
	IF (FIRST) THEN
		FIRST = .FALSE.
		IS = GEN_GETFOR (BLANK,LINE,LL)
		IF (IAND(IS,1).NE.0 .AND. LL.GT.0) IS = CLI_PARSE (LINE(:LL))
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
	CLI_INIT = DWC_SUCCESS
	RETURN
C
 999	CLI_INIT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_RESET (NR,NAME,ATTR,PROMPT,DEFVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NR		! (i) nr of arguments
	CHARACTER*(*)	NAME(*)		! (i) names (case insensitive)
	INTEGER*4	ATTR(*)		! (i) attributes
	CHARACTER*(*)	PROMPT(*)	! (i) prompt strings
	CHARACTER*(*)	DEFVAL(*)	! (i) default values
C
C.Purpose:	Initialise the Command Line Interpreter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes from referenced routines
C.Notes:
C	- The command-line syntax is defined as in CLI_INIT.
C	- Unlike CLI_INIT, this routine does not read the command line that 
C	  started up the program.
C-------------------------------------------------------------------------
C
	INTEGER*4	CLI_BUF_INIT
C
	CLI_RESET = CLI_BUF_INIT (NR,NAME,ATTR,PROMPT,DEFVAL)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_GET (NAME,VALUE,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (i) argument name
	CHARACTER*(*)	VALUE		! (o) argument value
	INTEGER*4	LVAL		! (o) significant length of VALUE
C
C.Purpose:	Get the status and value of an argument
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for truncated command line
C	success	DWC_PRESENT	argument is present
C	success	DWC_ABSENT	optional argument is absent
C	success	DWC_NEGATED	qualifier is negated
C	error	DWC_NOPARCOM	required argument is absent
C	false status codes from referenced routines
C.Notes:
C	- All arguments are optional.
C
C	If NAME is an argument name:
C	- NAME is case insensitive and may be a unique abbreviation of a name.
C	- The current status and value of that argument will be returned.
C	- LVAL = 0 indicates that no value is available (optional argument is
C	  absent, qualifier is negated or does not have the value attribute).
C	- If the argument is required but absent and the standard-input device
C	  is a terminal, the user will be prompted for input, the answer will
C	  be appended to the current command line, the new line will be parsed,
C	  and the routine will loop back until the argument is specified. If
C	  the input device is not a terminal, the routine returns with the
C	  error status DWC_NOPARVAL.
C 	- If CLI_GET is called for a parameter, that parameter becomes absent.
C	  CLI_GET only changes the presence of a qualifier, if the additional 
C	  input line contained that qualifier.
C
C	If NAME is blank (or empty):
C	- The current command line will be returned in VALUE.
C	- All present parameter values are listed, separated by blanks and 
C	  followed by all present or negated qualifiers in the format /name,
C	  /NOname or /name=value', where name is the full name.
C	- If the output string is too short, the command line will be truncated.
C	- Since parameter values are used only once, the complete command line 
C	  will only be returned if no CLI_GET (parameter) has been done yet.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	CLI_BUF_GET, CLI_PARSE, CLI_BUF_GETCOM
	INTEGER		DWC_INPUT, GEN_TERMSW, MSG_SET
C
	CHARACTER*255	LINE
	INTEGER*4	IS,LL
C
C
C					Get complete command line
C
	IF (NAME.EQ.BLANK) THEN
		CLI_GET = CLI_BUF_GETCOM (VALUE,LVAL)
		RETURN
	ENDIF
C
C					Get status and value of argument
C
 100	IS = CLI_BUF_GET (NAME,VALUE,LVAL)
C
C					If a required argument is absent:
C					- ask user on terminal
C					- parse the answer
C					- try again
C
	IF (IS.EQ.DWC_REQUIRED) THEN
		IF (IAND(GEN_TERMSW('SYS$INPUT'),1) .NE. 0) THEN
			IS = DWC_INPUT (LINE,VALUE(:LVAL),LL,0,0)
			IF (IAND(IS,1).NE.0) IS = CLI_PARSE (LINE)
			IF (IAND(IS,1).NE.0) GOTO 100
		ELSE
			IS = MSG_SET (DWC_NOPARCOM,0)
		ENDIF
	ENDIF
C
	CLI_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_PARSE (LINE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (i) command line
C
C.Purpose:	Analyse the command line and store the arguments
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CLIBUFERR	one or more syntax errors were found
C.Notes:
C	- The command line consists of a series of parameter and qualifier
C	  fields. These argument fields can be separated by any number of white
C	  characters (i.e. blanks and tabs). The only requirement is, that each
C	  parameter field must be preceded by at least one white, unless it is 
C	  the first field in the line.
C	- When CLI_PARSE finds a syntax error, it leaves a message in the
C	  message buffer, but completes the analysis of the line.
C
C	Parameter fields:
C	- Any field that does not start with a slash (/) is a parameter field.
C	  Parameters are identified by their sequence number in the line.
C	- For normal parameters (type attribute CLI__PARAMETER) the field ends
C	  before the first white or slash and may contain all other ASCII 
C	  characters.
C	- For parameters with the CLI__EXPRESSION attribute, the field can even 
C	  contain these delimiters. The end of an expression is marked by a 
C	  slash preceded by a blank, unless that combination is part of a 
C	  parenthesized ((...)) or quoted ("...") substring.
C	  
C	Qualifier fields:
C	- A qualifier field has the format /KEY, /NOKEY, or /KEY=VALUE, where 
C	  KEY identifies the qualifier. The last form is only allowed for
C	  qualifiers with the CLI__VALUE attribute. Both the slash and the
C	  equal sign may be surrounded by any number of whites.
C	- The keyword field ends before the first white, slash or equal sign.
C	  KEY must be a known qualifier name (case blind) or the unique 
C	  abbreviation of such a name.
C	- The value field in principle ends before the first white or slash,
C	  but CLI__PARSE accepts parenthesized ((...)) and quoted ("...")
C	  values, in which case it removes the delimiters and all whites from 
C	  the value.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, WHITE, SLASH
		PARAMETER (BLANK = ' ')
		PARAMETER (TAB   = '	')
		PARAMETER (WHITE = BLANK//TAB)
		PARAMETER (SLASH = '/')
C
	INTEGER*4	CLI_PARSE_P, CLI_PARSE_Q, CLI_PARSE_X
	INTEGER*4	CLI_BUF_GETNRS, CLI_BUF_PUTPAR, CLI_BUF_PUTQUAL
	INTEGER*4	STR_SIGLEN, STR_SKIP_W
C
	CHARACTER*255	KEY, VAL
	INTEGER*4	LL, LK, LV, PTR, PTRSAV
	INTEGER*4	IS, PCNT, NRPAR, NREXP, NRQUA
	LOGICAL*4	ERROR
C
C
	IS = CLI_BUF_GETNRS (NRPAR,NREXP,NRQUA)
	ERROR = .FALSE.
	LL = STR_SIGLEN (LINE)
	PTR = 1
	PCNT = 0
	IS = STR_SKIP_W (WHITE,LINE(:LL),PTR)
C
C					Loop through the command string
C
	DO WHILE ((.NOT.ERROR) .AND. (PTR.LE.LL))
		IF (LINE(PTR:PTR).NE.SLASH) THEN
C
C					Extract and store the parameter
C
			IF (NREXP.GT.0) THEN
				IS = CLI_PARSE_X (LINE(:LL),PTR,VAL,LV)
			ELSE
				IS = CLI_PARSE_P (LINE(:LL),PTR,VAL,LV)
			ENDIF
			PCNT = PCNT+1
			IS = CLI_BUF_PUTPAR (PCNT,VAL,LV)
		ELSE
C
C					Extract and store the qualifier
C					If it is not a qualifier, treat it
C					as a value if expressions are allowed.
C					First remove messages from _PARSE_Q.
C
			PTRSAV = PTR
			IS = CLI_PARSE_Q (LINE(:LL),PTR,KEY,LK,VAL,LV)
			IS = CLI_BUF_PUTQUAL (KEY(:LK),VAL,LV)
			IF ((IAND(IS,1).EQ.0) .AND. (NREXP.GT.0)) THEN
				PTR = PTRSAV
				IS = CLI_PARSE_X (LINE(:LL),PTR,VAL,LV)
				PCNT = PCNT+1
				IS = CLI_BUF_PUTPAR (PCNT,VAL,LV)
			ELSE IF (IAND(IS,1).EQ.0) THEN
			   	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
			ENDIF
		ENDIF
C
		IF (IAND(IS,1).EQ.0) ERROR = .TRUE.
		IS = STR_SKIP_W (WHITE,LINE(:LL),PTR)
	ENDDO
C
	IF (ERROR) THEN
		CLI_PARSE = DWC_CLIBUFERR
	ELSE
		CLI_PARSE = DWC_SUCCESS
	ENDIF
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_PARSE_P (LINE,PTR,VAL,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (i) command line
	INTEGER*4	PTR		! (m) start of parameter -> end+1
	CHARACTER*(*)	VAL		! (o) parameter value
	INTEGER*4	LV		! (o) length of VAL
C
C.Purpose:	Extract a parameter field from the command line
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	see CLI_PARSE
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, WHITE, SLASH, P_END
		PARAMETER (BLANK = ' ')
		PARAMETER (TAB   = '	')
		PARAMETER (WHITE = BLANK//TAB)
		PARAMETER (SLASH = '/')
		PARAMETER (P_END = WHITE//SLASH)
C
	INTEGER*4	STR_COPY_U
C
	INTEGER*4	IS
C
C
	LV = 0
	IS = STR_COPY_U (P_END,LINE,PTR,VAL,LV)
C
	CLI_PARSE_P = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_PARSE_X (LINE,PTR,VAL,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (i) command line
	INTEGER*4	PTR		! (m) start of expression -> end+1
	CHARACTER*(*)	VAL		! (o) expression
	INTEGER*4	LV		! (o) length of VAL
C
C.Purpose:	Extract an expression from the command line
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	see CLI_PARSE
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, SLASH, QUOTE, OPNPAR, CLOPAR
		PARAMETER (BLANK  = ' ')
		PARAMETER (SLASH  = '/')
		PARAMETER (QUOTE  = '"')
		PARAMETER (OPNPAR = '(')
		PARAMETER (CLOPAR = ')')
C
	INTEGER*4	STR_COPY_U, STR_COPY
C
	INTEGER*4	IS, LL, DEPTH
	LOGICAL*4	EXP_END
C
C
	VAL = BLANK
	LV = 0
	LL = LEN(LINE)
	DEPTH = 0
C
C					Copy until qualifier or end-of-line
C					- copy quoted substrings
C					- keep track of the subexpression 
C					  depth, i.e. nr of '(' - nr of ')' 
C					- copy subexpressions
C					- stop at BLANK//SLASH at depth 0
C
	IS = STR_COPY_U (SLASH//QUOTE//OPNPAR//CLOPAR,LINE,PTR,VAL,LV)
	EXP_END = PTR.GT.LL
	IF (.NOT.EXP_END .AND. PTR.GT.1) THEN
		EXP_END = LINE(PTR-1:PTR).EQ.BLANK//SLASH
	ENDIF
	DO WHILE (.NOT.EXP_END)
		IF (LINE(PTR:PTR).EQ.QUOTE) THEN
			IS = STR_COPY (QUOTE,VAL,LV)
			PTR = PTR+1
			IS = STR_COPY_U (QUOTE,LINE,PTR,VAL,LV)
		ELSE IF (LINE(PTR:PTR).EQ.OPNPAR) THEN
			DEPTH = DEPTH+1
		ELSE IF (LINE(PTR:PTR).EQ.CLOPAR) THEN
			DEPTH = DEPTH-1
		ENDIF
		IS = STR_COPY (LINE(PTR:PTR),VAL,LV)
		PTR = PTR+1
		IF (DEPTH.EQ.0) THEN
			IS = STR_COPY_U (QUOTE//OPNPAR//CLOPAR//SLASH,LINE,PTR,
	1								VAL,LV)
			EXP_END = PTR.GT.LL
			IF (.NOT.EXP_END .AND. PTR.GT.1) THEN
				EXP_END = LINE(PTR-1:PTR).EQ.BLANK//SLASH
			ENDIF
		ELSE
			IS = STR_COPY_U (QUOTE//OPNPAR//CLOPAR,LINE,PTR,VAL,LV)
			EXP_END = PTR.GT.LL
		ENDIF
	ENDDO
	IF (LV.GT.0 .AND. VAL(LV:LV).EQ.BLANK) LV = LV-1
C
C					Return
C
	CLI_PARSE_X = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_PARSE_Q (LINE,PTR,KEY,LK,VAL,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (i) command line
	INTEGER*4	PTR		! (m) start of qualifier -> end+1
	CHARACTER*(*)	KEY		! (o) qualifier key
	INTEGER*4	LK		! (o) length of KEY
	CHARACTER*(*)	VAL		! (o) qualifier value
	INTEGER*4	LV		! (o) length of VAL
C
C.Purpose:	Extract a qualifier field from the command line
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	see CLI_PARSE
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, WHITE, SLASH, EQUAL, QUOTE, OPPAR, CLPAR
		PARAMETER (BLANK = ' ')
		PARAMETER (TAB   = '	')
		PARAMETER (WHITE = BLANK//TAB)
		PARAMETER (SLASH = '/')
		PARAMETER (EQUAL = '=')
		PARAMETER (QUOTE = '"')
		PARAMETER (OPPAR = '(')
		PARAMETER (CLPAR = ')')
C
	CHARACTER*(*)	Q_END, K_END, V_START
	CHARACTER*1	V_END(2)
		PARAMETER (Q_END   = WHITE//SLASH)	! end qualifier
		PARAMETER (K_END   = Q_END//EQUAL)	! end qual key
		PARAMETER (V_START = QUOTE//OPPAR)	! start delimited value
		DATA       V_END    /QUOTE, CLPAR/	! end delimited value
C
	INTEGER*4	STR_SKIP_W, STR_COPY_U
C
	INTEGER*4	LL, IS, VTYP
C
C
	LL = LEN(LINE)
	LK = 0
	LV = 0
C
C					Extract qualifier key
C					- first, skip slash and whites
C
	PTR = PTR+1
	IS = STR_SKIP_W (WHITE,LINE,PTR)
	IS = STR_COPY_U (K_END,LINE,PTR,KEY,LK)
C
C					Skip to next token
C
	IS = STR_SKIP_W (WHITE,LINE,PTR)
	IF (PTR.LE.LL .AND. LINE(PTR:PTR).EQ.EQUAL) THEN
C
C					Extract qualifier value
C					- first, skip equal sign and whites
C					- determine type of value
C					- delimited values are extracted
C					  without delimiters and whites
C
		PTR = PTR+1
		IS = STR_SKIP_W (WHITE,LINE,PTR)
		VTYP = 0
		IF (PTR.LE.LL) VTYP = INDEX(V_START,LINE(PTR:PTR))
		IF (VTYP.EQ.0) THEN
			IS = STR_COPY_U (Q_END,LINE,PTR,VAL,LV)
		ELSE
			PTR = PTR+1
			DO WHILE (PTR.LE.LL .AND. LINE(PTR:PTR).NE.V_END(VTYP))
				IS = STR_SKIP_W (WHITE,LINE,PTR)
				IS = STR_COPY_U (WHITE//V_END(VTYP),
	1						LINE,PTR,VAL,LV)
			ENDDO
			PTR = PTR+1
		ENDIF
	ENDIF
C
	CLI_PARSE_Q = DWC_SUCCESS
	RETURN
	END
