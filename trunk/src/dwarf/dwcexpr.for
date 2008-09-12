C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_EXPR
C.Keywords:	DWARF Expression
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C			- use argument PAR iso. I in error message in _SUBX
C.Version:	010709 AXC - linux port - exatr real*8R2
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy routine
C-------------------------------------------------------------------------
C
	DWC_EXPR = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_SOLVE (STRING,STREAM,FACUNIT,UNITSTR,
	1					ANSWER,PTR,CHKSW,SWSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) expression
	CHARACTER*(*)	STREAM		! (i) stream name for symbol names
	REAL*8		FACUNIT		! (i) conversion factor (-> def unit)
	CHARACTER*(*)	UNITSTR		! (i) possible unit codes
	REAL*8		ANSWER		! (o) result
	INTEGER*4	PTR		! (o) position of possible error
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
C
C.Purpose:	Check the syntax of an expression and solve it
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_TOODEENES	subexpressions too deeply nested
C	error	DWC_TOOMANYNR	too many numbers in subexpression
C	error	DWC_NOTAFTOP	nothing found after last operator
C	error	DWC_UNBPAREN	unbalanced parentheses
C	false status codes returned by referenced routines
C.Notes:
C	- Parentheses can be used to create subexpressions for changing the
C	  normal arithmetical priorities. Up to 10 levels are allowed, with in
C	  each level up to 10 numbers.
C	- Unary operators ('+', '-' and .NOT.) create a new subexpression which
C	  ends before the next binary operator with the same or lower priority
C	  than the unary operator.
C	- The numbers and operators in each subexpression are kept in arrays.
C	  When the end of the subexpression is encountered (end of unary reach,
C	  closing parenthesis ')', or end-of-expression), it will be evaluated
C	  and the result will be stored in the level above.
C	- The calculation is done in real*8.
C	- It's possible to use symbols in the expression. They will be
C	  substituted in the string with parentheses around them to ensure
C	  the correct arithmetical order.
C	  As a consequence, STRING might change.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, OPPAR
	INTEGER*4	MAXLEV, MAXNNR
		PARAMETER (BLANK  = ' ')
		PARAMETER (OPPAR  = '(')
		PARAMETER (MAXLEV = 20)		! max depth of subexpressions
		PARAMETER (MAXNNR = 100)	! max nr of numbers in expr
C
	INTEGER*4	DWC_EXPR_FUNC, DWC_EXPR_NR, DWC_EXPR_OPER
	INTEGER*4	DWC_EXPR_SUBX, DWC_EXPR_SUBST, DWC_EXPR_UNIT
	INTEGER*4	STR_SIGLEN, STR_CHECK_ALPH, STR_SKIP_W
	INTEGER		MSG_SET
C
	INTEGER*4	LEV, UNARY, TOTNNR
	INTEGER*4	UNOP(MAXLEV), NNR(MAXLEV)
	INTEGER*4	FUNC(MAXLEV), NPAR(MAXLEV), STN(MAXLEV)
	INTEGER*4	OPER(MAXNNR)
	REAL*8		NR(MAXNNR)
	REAL*8		NUM
	INTEGER*4	IS, LSTR, FUN, PAR, NSUBST
	LOGICAL*4	FOUND_OP, START_SUB, END_SUB, END_STRING
C
C
	ANSWER = 0
	LSTR = STR_SIGLEN (STRING)
	PTR = 1
C
	TOTNNR = 0				! total nr of numbers
	LEV = 1					! current subexpression level
	NNR(LEV) = 0				! nr of numbers (and op codes)
	STN(LEV) = 1				! start of first number in NR
	FUNC(LEV) = 0				! function code
	UNOP(LEV) = 0				! unary operator code
	NSUBST = 0				! nr of symbol substitutions
C
C
	FOUND_OP = .TRUE.
	DO WHILE (PTR.LE.LSTR)
	    FUN = 0
	    UNARY = 0
C
C					Start of subexpression ?
C
	    IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR)	! skip blanks
	    START_SUB = STRING(PTR:PTR).EQ.OPPAR
C
C					Unary operator ?
C
	    IF (.NOT.START_SUB) THEN
		IF (STRING(PTR:PTR).EQ.'+') THEN
		    UNARY = 4
		ELSE IF (STRING(PTR:PTR).EQ.'-') THEN
		    UNARY = 5
		ELSE IF (STRING(PTR:PTR+4).EQ.'.NOT.') THEN
		    UNARY = 16
		    PTR = PTR+4
		ENDIF
		START_SUB = UNARY.NE.0
	    ENDIF
C
C					Function or symbol name?
C					- if function: determine code
C					  and nr of possible parameters
C					- if symbol name: substitute
C					  (put value between parentheses)
C
	    IF (.NOT.START_SUB) THEN
		FOUND_OP = .FALSE.
		IF (IAND(STR_CHECK_ALPH(STRING(PTR:PTR)),1) .NE. 0) THEN
		    IS = DWC_EXPR_FUNC (STRING,LSTR,PTR,FUN,PAR)
		    IF (IAND(IS,1).EQ.0) GOTO 999		! unknown function
		    IF (FUN.LT.0) THEN			! symbol name
			IS = DWC_EXPR_SUBST (STRING,LSTR,PTR,-FUN,STREAM,
	1						CHKSW,SWSYM,NSUBST)
			IF (IAND(IS,1).EQ.0) GOTO 999		! substitution error
		    ENDIF
		    START_SUB = .TRUE.
		ENDIF
	    ENDIF
C
C					If start subexpression:
C					- go 1 level deeper
C
	    IF (START_SUB) THEN
		LEV = LEV+1
		IF (LEV.GT.MAXLEV) GOTO 991		! too deep a level
		NNR(LEV) = 0				! nr of REAL*8 numbers
		STN(LEV) = TOTNNR+1			! start of 1st nr in NR
		FUNC(LEV) = FUN				! function code
		NPAR(LEV) = PAR				! nr of parameters
		UNOP(LEV) = UNARY			! unary operator
		PTR = PTR+1				! 1-st pos in subexpr
C
C					Otherwise:
C					- extract number
C
	    ELSE
		IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR) ! skip blanks
		IS = DWC_EXPR_NR (STRING,LSTR,PTR,NUM)
		IF (IAND(IS,1).EQ.0) GOTO 999			! extraction error
 200		NNR(LEV) = NNR(LEV)+1			! make room for number
		TOTNNR   = TOTNNR  +1
		IF (TOTNNR.GT.MAXNNR) GOTO 992		! too many numbers
C
C					- extract unit and scale number
C					- store number in array
C
		IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR) ! skip blanks
		IS = DWC_EXPR_UNIT (STRING,LSTR,PTR,NUM,FACUNIT,UNITSTR)
		IF (IAND(IS,1).EQ.0) GOTO 999			! scaling error
		NR(TOTNNR) = NUM
C
C					- extract and store operator
C
		IS = DWC_EXPR_OPER (STRING,LSTR,PTR,OPER(TOTNNR),
	1							UNOP(LEV),LEV)
		IF (IAND(IS,1).EQ.0) GOTO 999			! syntax error
		END_STRING = OPER(TOTNNR).EQ.0
		END_SUB = OPER(TOTNNR).EQ.-1
C
C					If end of subexpression:
C					- evaluate it
C					- go 1 level up
C
		IF (END_SUB) THEN
		    IS = DWC_EXPR_SUBX (NR(STN(LEV)),OPER(STN(LEV)),UNOP(LEV),
	1			NNR(LEV),FUNC(LEV),NPAR(LEV),FACUNIT,NUM,SWSYM)
		    IF (IAND(IS,1).EQ.0) GOTO 999		! evaluation error
		    TOTNNR = TOTNNR-NNR(LEV)
		    LEV = LEV-1
		    GOTO 200				! fill in the nr
		ENDIF
		IF (.NOT.END_STRING) FOUND_OP = .TRUE.	! operator found
	    ENDIF
	ENDDO
C
C					Evaluate whole expression
C
	IF (FOUND_OP) GOTO 993				! ends with operator
	IF (LEV.NE.1) GOTO 994				! unbalanced parenth's
	IS = DWC_EXPR_SUBX (NR,OPER,UNOP,NNR,FUNC,NPAR,FACUNIT,
	1						ANSWER,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	DWC_EXPR_SOLVE = DWC_SUCCESS
	PTR = 0
	RETURN
C
 991	DWC_EXPR_SOLVE = MSG_SET (DWC_TOODEENES,1)
	CALL WNCTXT(DWLOG,DWMSG,MAXLEV)
	RETURN
C
 992	DWC_EXPR_SOLVE = MSG_SET (DWC_TOOMANYNR,1)
	CALL WNCTXT(DWLOG,DWMSG,MAXNNR)
	RETURN
C
 993	DWC_EXPR_SOLVE = MSG_SET (DWC_NOTAFTOP,0)
	RETURN
C
 994	DWC_EXPR_SOLVE = MSG_SET (DWC_UNBPAREN,0)
	RETURN
C
 999	DWC_EXPR_SOLVE = IS		! error reported by referenced routine
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_FUNC (STRING,LSTR,PTR,FUNC,NRPAR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) expression
	INTEGER*4	LSTR		! (i) significant length of STRING
	INTEGER*4	PTR		! (m) pointer (see notes)
	INTEGER*4	FUNC		! (o) function code or -length name
	INTEGER*4	NRPAR		! (o) nr of required parameters
C
C.Purpose:	Determine the function used in the expression
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_UNKFUNC	unknown function
C.Notes:
C	- If on input PTR points to a valid function name, it will on output
C	  point to the opening parenthesis.
C	- If PTR points to a symbol name (an extended-alphanumeric substring
C	  not followed by an opening parenthesis), PTR is not changed and
C	  -FUNC will be the length of the symbol name.
C
C	- Valid functions:
C	code  name  nrpar	code  name  nrpar	code  name  nrpar
C
C	  1   MIN   -32		  2   MAX   -32		  3   SIN     1
C	  4   COS     1		  5   TAN     1		  6   ASIN    1
C	  7   ACOS    1		  8   ATAN    1		  9   ATAN2   2
C	 10   ABS     1		 11   EXP     1		 12   LOG     1
C	 13   LOG10   1		 14   SQRT    1		 15   TRUNC   1
C	 16   ROUND   1		 17   MOD     2		 18   SIGN    1
C
C	   NRPAR > 0 gives the exact nr of parameters the user must give,
C	  -NRPAR > 0 gives the maximum nr of parameters the user can give.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, OPPAR, ANUMX
	INTEGER*4	NRFUNC
		PARAMETER (BLANK = ' ')
		PARAMETER (OPPAR = '(')
		PARAMETER (ANUMX = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_')
		PARAMETER (NRFUNC = 18)			! nr of functions
C
	CHARACTER*5	FNAM(NRFUNC)
	INTEGER*4	PAR(NRFUNC)
		DATA FNAM /
	1		'MIN'  ,'MAX'  ,'SIN'  ,'COS'  ,'TAN'  ,'ASIN' ,
	2		'ACOS' ,'ATAN' ,'ATAN2','ABS'  ,'EXP'  ,'LOG'  ,
	3		'LOG10','SQRT' ,'TRUNC','ROUND','MOD'  ,'SIGN' /
		DATA PAR /
	1		-32    ,-32    ,1      ,1      ,1      ,1      ,
	2		1      ,1      ,2      ,1      ,1      ,1      ,
	3		1      ,1      ,1      ,1      ,2      ,1      /
C
	INTEGER		MSG_SET, STR_SKIP_W, STR_MATCH_A
C
	INTEGER*4	IS, START, LNAME
C
C
C					Skip to the end of the name
C					and skip possible blanks
C
	START = PTR
	LNAME = STR_SKIP_W (ANUMX,STRING(:LSTR),PTR)
	IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR)
C
C					If function name:
C					- return code and nr of parameters
C					Otherwise (symbol name):
C					- restore pointer and return length
C
	IF (PTR.LE.LSTR .AND. STRING(PTR:PTR).EQ.OPPAR) THEN
		IS = STR_MATCH_A (STRING(START:PTR-1),NRFUNC,FNAM,FUNC)
		IF (IS.NE.1) GOTO 999			! unknown function
		NRPAR = PAR(FUNC)
	ELSE
		PTR = START
		FUNC = -LNAME
	ENDIF
C
C
	DWC_EXPR_FUNC = DWC_SUCCESS
	RETURN
C
 999	DWC_EXPR_FUNC = MSG_SET (DWC_UNKFUNC,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_SUBST (STRING,LSTR,PTR,LNAM,
	1					STREAM,CHKSW,SWSYM,NRSUBS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) expression
	INTEGER*4	LSTR		! (m) significant length of expression
	INTEGER*4	PTR		! (i) start of symbol name
	INTEGER*4	LNAM		! (i) length of symbol name
	CHARACTER*(*)	STREAM		! (i) stream name
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) were there unknown symbols ?
	INTEGER*4	NRSUBS		! (m) nr of substitutions performed
C
C.Purpose:	Replace a symbol in a expression by its value
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_MUTUALSUB	more than 25 substitutions (probably looping)
C	error	DWC_SYMNOTDEF	unknown symbol (only for CHKSW = .TRUE.)
C	error	DWC_STRTOOSHO	STRING is too short to contain the symbol value
C.Notes:
C	- The significant length of the expression will be updated.
C	- The value will be placed between parentheses to keep the correct
C	  arithmetical order.
C	- To avoid substitution looping, there is a maximum of 25 substitutions.
C	- An unknown symbol will be substituted by '(1)'.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	OPPAR, CLPAR
	INTEGER*4	MAXSUBS
		PARAMETER (OPPAR = '(')
		PARAMETER (CLPAR = ')')
		PARAMETER (MAXSUBS = 25)
C
	INTEGER*4	DWC_SYM_EXPAND, DWC_SYM_TRANSL
	INTEGER		STR_COPY, MSG_SET
C
	CHARACTER*255	VALUE, WORK
	INTEGER*4	IS, LVAL, LW
C
C
C					Check nr of substitutions
C
	IF (NRSUBS.GE.MAXSUBS) GOTO 991
	NRSUBS = NRSUBS+1
C
C					Translate the symbol
C					- expand symbol name (if needed)
C
	IS = DWC_SYM_EXPAND (STRING(PTR:PTR+LNAM-1),STREAM,WORK,LW)
	IF (IAND(IS,1).NE.0)
	1	IS = DWC_SYM_TRANSL (WORK(:LW),VALUE,LVAL,CHKSW,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Replace the symbol name by the value
C					- put the value between parentheses
C
	LW = 0
	IS = STR_COPY (STRING(PTR+LNAM:LSTR),WORK,LW)	! save rest of string
	LSTR = PTR-1					! keep first part
	IS = STR_COPY (OPPAR//VALUE(:LVAL)//CLPAR//	! append value
	1		WORK(:LW),STRING,LSTR)		!  and rest string
	IF (IS.LT.0) GOTO 990				! string overflow
C
C
	DWC_EXPR_SUBST = DWC_SUCCESS
	RETURN
C
 990	DWC_EXPR_SUBST = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STRING))
	RETURN
C
 991	DWC_EXPR_SUBST = MSG_SET (DWC_MUTUALSUB,0)
	RETURN
C
 999	DWC_EXPR_SUBST = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_NR (STRING,LSTR,PTR,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) expression
	INTEGER*4	LSTR		! (i) significant length of STRING
	INTEGER*4	PTR		! (m) pointer (start -> end of nr)
	REAL*8		NR		! (o) number
C
C.Purpose:	Read a number from the expression and decode it
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_INVNONR	invalid number or no number
C.Notes:
C	- The number can be in decimal (default), octal or hexadecimal radix
C	  indicated by the prefixes by %D, %O and %X respectively. An octal or
C	  hexadecimal number must be followed by a blank or an operator (incl.
C	  comma and open-parenthesis).
C	- Real numbers can only be given in decimal radix. Exponential D- or
C	  E-notation can be used, but the exponent can contain only 1 unary
C	  sign. A decimal point is not required.
C	- Decimal numbers can be given as DD:MM:SS, where DD, MM and SS are
C	  real numbers. The total value is DD+MM/60+SS/3600. By default all
C	  three numbers are zero, but at least one of them must be given.
C	  Significant colons must be given too of course.
C	  Eg.:	::5.3 means 0:0:5.3
C	- When a valid number has been found, the PTR will point to the next
C	  character in the expression.
C-------------------------------------------------------------------------
C
	LOGICAL*4	DECIMAL, OCTAL, HEXA
	CHARACTER*(*)	RADLIST, BLANK, EXPON, SIGNS, COLON
		PARAMETER (DECIMAL = 1)
		PARAMETER (OCTAL   = 2)
		PARAMETER (HEXA    = 3)
		PARAMETER (RADLIST = 'DOX')
		PARAMETER (BLANK   = ' ')
		PARAMETER (EXPON   = 'DE')
		PARAMETER (SIGNS   = '+-')
		PARAMETER (COLON   = ':')
C
	INTEGER*4	DWC_EXPR_OPER_M
	INTEGER*4	STR_CHECK_ALPH, STR_SKIP_U, STR_READ_D
	INTEGER		MSG_SET
C
	INTEGER*4	LENG, ST, END, COLPOS, NRCOL, INTNR
	INTEGER*4	IS, RADIX
	REAL*8		NRDMS
	LOGICAL*4	END_OF_NR, IS_EXPON
C
C
C					Logical value ?
C
	IF (STRING(PTR:PTR+5).EQ.'.TRUE.') THEN
		NR = -1
		PTR = PTR+6
		GOTO 900
	ELSE IF (STRING(PTR:PTR+6).EQ.'.FALSE.') THEN
		NR = 0
		PTR = PTR+7
		GOTO 900
	ENDIF
C
C					Determine the radix
C					- default decimal
C
	ST = PTR
	RADIX = DECIMAL
	IF (STRING(ST:ST).EQ.'%') THEN
		RADIX = INDEX (RADLIST,STRING(ST+1:ST+1))
		IF (RADIX.EQ.0) GOTO 999		! invalid radix
		ST = PTR+2				! skip radix prefix
	ENDIF
C
C					Get the end of the number
C
	END_OF_NR = .FALSE.
	IS_EXPON = .FALSE.
	I = ST						! start of number
	DO WHILE (.NOT.END_OF_NR)
C
C					- end of string or blank ?
C
		IF (I.GT.LSTR .OR. STRING(I:I).EQ.BLANK) THEN
			END_OF_NR = .TRUE.
C
C					- start of unit code ?
C
		ELSE IF (RADIX.EQ.DECIMAL .AND. .NOT.IS_EXPON
	1		.AND. INDEX(EXPON,STRING(I:I)).NE.0) THEN
			IF (IAND(STR_CHECK_ALPH(STRING(I+1:I+1)),1) .NE. 0) THEN
				END_OF_NR = .TRUE.	! unit found
			ELSE
				IS_EXPON = .TRUE.	! exponent found
				I = I+1			! skip E or D
				IF (INDEX(SIGNS,STRING(I:I)).NE.0)
	1				I = I+1		! skip sign
			ENDIF
C
C					- alphabetic character ?
C
		ELSE IF (RADIX.EQ.DECIMAL
	1	        .AND. IAND(STR_CHECK_ALPH(STRING(I:I)),1) .NE. 0) THEN
			END_OF_NR = .TRUE.
C
C					- operator symbol ?
C
		ELSE IF (IAND(DWC_EXPR_OPER_M(STRING,LSTR,I),1) .NE. 0) THEN
			END_OF_NR = .TRUE.
C
C					COLON indicates a new part of the nr,
C					so an exponent is possible again
C
		ELSE IF (STRING(I:I).EQ.COLON) THEN
			IS_EXPON = .FALSE.
			I = I+1					! skip colon
C
C					Otherwise: stil part of nr
C
		ELSE
			I = I+1					! skip char
		ENDIF
	ENDDO
C
C					Check the nr
C
	END = I-1
	LENG = END-ST+1
	IF (LENG.EQ.0) GOTO 999				! no nr
	IF (RADIX.EQ.OCTAL) THEN
		IF (LENG.GT.12) GOTO 999		! too long
		READ (STRING(ST:END),'(O12)',ERR=999) INTNR
		NR = INTNR
	ELSE IF (RADIX.EQ.HEXA) THEN
		IF (LENG.GT.8) GOTO 999			! too long
		READ (STRING(ST:END),'(Z8)',ERR=999) INTNR
		NR = INTNR
	ELSE				! DECIMAL
C
		NRDMS = 0
		NRCOL = 0
		DO WHILE (ST.LE.END+1)
		    IF (NRCOL.GT.2) GOTO 999		! more than 2 colons
		    COLPOS = ST
		    IS = STR_SKIP_U (COLON,STRING(:END),COLPOS) ! find colon
		    IF (IS.GT.0) THEN
			IS = STR_READ_D (STRING(ST:COLPOS-1),NR) ! decode nr
			IF (IAND(IS,1).EQ.0) GOTO 999
			NRDMS = NRDMS+NR*60D0**-NRCOL	! convert to DDMMSS
		    ENDIF
		    ST = COLPOS+1			! skip past colon
		    NRCOL = NRCOL+1			! increment colon count
		ENDDO
		IF (LENG.LT.NRCOL) GOTO 999		! only colons, no nr's
		NR = NRDMS
	ENDIF
	PTR = END+1
C
 900	DWC_EXPR_NR = DWC_SUCCESS
	RETURN
C
 999	DWC_EXPR_NR = MSG_SET (DWC_INVNONR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_OPER (STRING,LSTR,PTR,OPER,
	1						UNOPER,DEPTH)
C	          ENTRY  DWC_EXPR_OPER_M (STRING,LSTR,PTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	DWC_EXPR_OPER_M	! just match
C
	CHARACTER*(*)	STRING		! (i) expression
	INTEGER*4	LSTR		! (i) significant length of STRING
	INTEGER*4	PTR		! (m) pointer (start -> end of oper)
	INTEGER*4	OPER		! (o) operator code
C				1=**	2=*	3=/	4=+	5=-
C				6=.EQ.	7=.NE.	8=.LT.	9=.LE.	10=.GT.
C				11=.GE.	12=.AND. 13=.OR. 14=.XOR. 15=.EQV.
C				0=end-of-string
C				-1=)
C				-2=,
	INTEGER*4	UNOPER		! (i) unary operator code
C				4=+	5=-	16=.NOT.
	INTEGER*4	DEPTH		! (i) Subexpression level
C				on level 1 ) and , are not allowed
C
C.Purpose:	Extract the binary operator from the expression
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	0		no operator match (DWC_EXPR_OPER_M only)
C	error	DWC_UNBPAREN	unbalanced parentheses
C	error	DWC_INVOPER	invalid operator
C.Notes:
C	- Closing parenthesis (')'), comma and end-of-string are detected
C	  and are treated as pseudo operators. The first two are not allowed
C	  on DEPTH = 1.
C	- The function also checks for the end of the reach of a unary
C	  operator (i.e., the arithmetical priority of the binary operator
C	  is equal to or less than the priority of the unary operator). If
C	  that is found, the code for the pseudo operator ')' will be returned.
C	- If an operator has been found, PTR will point to the first position
C	  behind the operator.
C
C	The priority order of the operators is:
C	high	**
C	 |	*	/
C	 |	+	-
C	 |	EQ	NE	LT	LE	GT	GE
C	 |	NOT
C	 |	AND
C	 v	OR
C	low	XOR	EQV
C-------------------------------------------------------------------------
C
	INTEGER*4	MSG_SET
C
C					Possible binary operators
C					(unaries are catched in _SOLVE;
C					.NOT. has the code 16)
C
	CHARACTER*5	OPARR(-2:15)
	INTEGER*4	LENOP(-2:15), PRIO(-2:16)
		DATA OPARR /
	1		','	,')'	,' '	,
	2		'**'	,'*'	,'/'	,'+'	,'-'	,
	3		'.EQ.'	,'.NE.'	,'.LT.'	,'.LE.'	,'.GT.'	,
	4		'.GE.'	,'.AND.','.OR.'	,'.XOR.','.EQV.'/
		DATA LENOP /
	1		1	,1	,5	,
	2		2	,1	,1	,1	,1	,
	3		4	,4	,4	,4	,4	,
	4		4	,5	,4	,5	,5	/
		DATA PRIO /
	1		100	,100	,100	,
	2		1	,2	,2	,3	,3	,
	3		4	,4	,4	,4	,4	,
	4		4	,6	,7	,8	,8	,5/
C
C
C
C					Find the operator
C
	IF (PTR.GT.LSTR) THEN
		OPER = 0				! end of string
	ELSE
		DO OPER = -2,15
		    IF (STRING(PTR:PTR+LENOP(OPER)-1).EQ.OPARR(OPER)) GOTO 100
		ENDDO
		GOTO 999				! invalid operator
 100		CONTINUE
	ENDIF
C
C					Check
C
	IF (UNOPER.GT.0 .AND. PRIO(OPER).GE.PRIO(UNOPER)) THEN
		OPER = -1				! end of unary range
	ELSE IF (OPER.EQ.0) THEN			! end of string
	ELSE IF (DEPTH.EQ.1 .AND. OPER.LT.0) THEN
		IF (OPER.EQ.-1) GOTO 998		! unbalanced parenth's
		IF (OPER.EQ.-2) GOTO 999		! invalid operator
	ELSE
		PTR = PTR+LENOP(OPER)			! shift pointer
	ENDIF
C
C
	DWC_EXPR_OPER = DWC_SUCCESS
	RETURN
C
 998	DWC_EXPR_OPER = MSG_SET (DWC_UNBPAREN,0)
	RETURN
C
 999	DWC_EXPR_OPER = MSG_SET (DWC_INVOPER,1)
	CALL WNCTXT(DWLOG,DWMSG,STRING(1:2))
	RETURN
C
C	=====================
	ENTRY DWC_EXPR_OPER_M (STRING,LSTR,PTR)
C	=====================
C
	DO J = -2,15
		IF (STRING(PTR:PTR+LENOP(J)-1).EQ.OPARR(J)) GOTO 800
	ENDDO
	DWC_EXPR_OPER_M = 0				! no operator
	RETURN
C
 800	DWC_EXPR_OPER_M = DWC_SUCCESS			! operator found
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_UNIT (STRING,LSTR,PTR,NR,
	1						FACUNIT,UNITSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
	CHARACTER*(*)	STRING		! (i) expression
	INTEGER*4	LSTR		! (i) significant length of STRING
	INTEGER*4	PTR		! (m) pointer (start -> end of unit)
	REAL*8		NR		! (m) nr to be scaled
	REAL*8		FACUNIT		! (i) conversion factor
	CHARACTER*(*)	UNITSTR		! (i) string with possible unit codes
C
C.Purpose:	Scale the nr if the expression contains a valid unit
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	error	DWC_INVUNIT	unit not allowed
C	false status code returned by READ_UNIT
C.Notes:
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, ALPHA
		PARAMETER (BLANK = ' ')
		PARAMETER (ALPHA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
C
	INTEGER*4	STR_SKIP_W, STR_MATCH_L, READ_UNIT
	INTEGER		MSG_SET
C
	INTEGER*4	IS, ST, MATCHNR
	REAL*8		FACTOR
	CHARACTER*16	WORK
C
C
C					Is a unit code given ?
C					- if so: determine its length
C					  and get the scale factor
C
	ST = PTR
	IS = STR_SKIP_W (ALPHA,STRING(:LSTR),PTR)
	IF (IS.EQ.0) GOTO 900				! no unit
C
	IS = STR_MATCH_L (STRING(ST:PTR-1),UNITSTR,MATCHNR)
	IF (IAND(IS,1).EQ.0) GOTO 999				! invalid code
	IS = READ_UNIT (STRING(ST:PTR-1),WORK,FACTOR)
	IF (IAND(IS,1).EQ.0) GOTO 998				! error
C
C					Scale the nr and
C					update the pointer
C
	FACTOR = FACUNIT/FACTOR
	NR = NR*FACTOR
	IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR)
C
C
 900	DWC_EXPR_UNIT = DWC_SUCCESS
	RETURN
C
 998	DWC_EXPR_UNIT = IS
	PTR = ST
	RETURN
C
 999	DWC_EXPR_UNIT = MSG_SET (DWC_INVUNIT,1)
	CALL WNCTXT(DWLOG,DWMSG,STRING(ST:PTR-1))
	PTR = ST
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_SUBX (NR,OPER,UNOPER,NNR,FUNC,NRPAR,
	1						FACUNIT,NUM,SWSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
	REAL*8		NR(*)		! (m) table with numbers
	INTEGER*4	OPER(*)		! (m) table with operator codes
	INTEGER*4	UNOPER		! (i) unary operator code
	INTEGER*4	NNR		! (i) nr of numbers in NR
	INTEGER*4	FUNC		! (i) function code (0 = no function)
	INTEGER*4	NRPAR		! (i) nr of parms required for function
	REAL*8		FACUNIT		! (i) factor (default unit to radians)
	REAL*8		NUM		! (o) result
	LOGICAL*4	SWSYM		! (i) were unknown symbols present ?
C
C.Purpose:	Evaluate a subexpression
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	DWC_SUCCESS
C	DWC_UNDEFEXP	undefined exponentiation
C	DWC_DIVBYZERO	divide by zero
C	DWC_TOOMANARG	too many function-arguments
C	DWC_TOOLITARG	not enough function-arguments
C	DWC_INVFUNARG	invalid function-argument
C.Notes:
C	- OPER(I) is an operator for NR(I) and NR(I+1).
C	- If unknown symbols were found, these have been substituted by (1).
C	  In that case only syntax errors are significant and data errors
C	  are ignored.
C	- First the arithmetic is done according to the normal arithmetical
C	  priority-rules.
C	- Then the function is evaluated if one is given. The argument or
C	  result of a trigonometric function is converted from, respectively
C	  to, the default unit.
C	- Finally the unary operator is taken if one is given.
C-------------------------------------------------------------------------
C
	INTEGER*4	DWC_EXPR_LOGI, MSG_SET, READ_UNIT
	INTEGER*4	MOVE_BLD, MOVE_BLJ
C
	INTEGER*4	SUB, PAR
	INTEGER*4	EXPON, IS
	REAL*8		FACTOR, DIFF, R2
	COMPLEX*16	COMPL
	CHARACTER*16 	WORK
C
C
C					Get nr of numbers/operators
C					Get conversion factor for radians
C
	SUB = NNR
	IS = READ_UNIT ('RAD',WORK,FACTOR)
	IF (IAND(IS,1).EQ.0) GOTO 999
	FACTOR = FACTOR/FACUNIT				! factor for gonio fu's
C
C					Perform all exponentiations (backwards)
C					- negative exp can only be integer
C
	I = SUB-1
	DO WHILE (I.GT.0)
	    IF (OPER(I).EQ.1) THEN
		IF (NR(I).EQ.0) THEN			! zero exp
			NR(I+1) = 0
		ELSE IF (NR(I).LT.0) THEN		! negative exp
			EXPON = NINT (NR(I+1))
			IF (ABS(EXPON-NR(I+1)).GT.1.E-4) THEN
				IF (.NOT.SWSYM) GOTO 991 ! not integer
				EXPON = 1
			ENDIF
			NR(I+1) = NR(I)**EXPON
		ELSE					! positive exp
			NR(I+1) = NR(I)**NR(I+1)
		ENDIF
		IS = MOVE_BLD (NR(I+1),NR(I),SUB-I)	! shift tables
		IS = MOVE_BLJ (OPER(I+1),OPER(I),SUB-I)
		SUB = SUB-1
	    ELSE
		I = I-1
	    ENDIF
	ENDDO
C
C					Perform all multiplications/divisions
C
	I = 1
	DO WHILE (I.LT.SUB)
	    IF (OPER(I).EQ.2 .OR. OPER(I).EQ.3) THEN
		IF (OPER(I).EQ.2) THEN
			NR(I+1) = NR(I)*NR(I+1)		! multiply
		ELSE
			IF (NR(I+1).EQ.0) THEN		! avoid zero-divide
				IF (.NOT.SWSYM) GOTO 992
				NR(I+1) = 1
			ENDIF
			NR(I+1) = NR(I)/NR(I+1)		! divide
		ENDIF
		IS = MOVE_BLD (NR(I+1),NR(I),SUB-I)	! shift tables
		IS = MOVE_BLJ (OPER(I+1),OPER(I),SUB-I)
		SUB = SUB-1
	    ELSE
		I = I+1
	    ENDIF
	ENDDO
C
C					Perform all additions/subtractions
C
	I = 1
	DO WHILE (I.LT.SUB)
	    IF (OPER(I).EQ.4 .OR. OPER(I).EQ.5) THEN
		IF (OPER(I).EQ.4) THEN
			NR(I+1) = NR(I)+NR(I+1)		! add
		ELSE
			NR(I+1) = NR(I)-NR(I+1)		! subtract
		ENDIF
		IS = MOVE_BLD (NR(I+1),NR(I),SUB-I)	! shift tables
		IS = MOVE_BLJ (OPER(I+1),OPER(I),SUB-I)
		SUB = SUB-1
	    ELSE
		I = I+1
	    ENDIF
	ENDDO
C
C					Perform all compares with
C					accuracy of 8 decimals
C
	I = 1
	DO WHILE (I.LT.SUB)
	    IF (OPER(I).GE.6. AND. OPER(I).LE.11) THEN
		DIFF = NR(I)-NR(I+1)
		IF (ABS(DIFF).LT.1.E-8) DIFF = 0
		IF (DIFF.EQ.0) THEN
		   R1=1
		ELSE
		   R1=0
		END IF
		IF (DIFF.LT.0) THEN
		   R2=1
		ELSE
		   R2=0
		END IF 
		IF (OPER(I).EQ.6) THEN
			NR(I+1) = R1 !DIFF.EQ.0
		ELSE IF (OPER(I).EQ.7) THEN
			NR(I+1) = 1-R1 !DIFF.NE.0
		ELSE IF (OPER(I).EQ.8) THEN
			NR(I+1) = R2 !DIFF.LT.0
		ELSE IF (OPER(I).EQ.9) THEN
			NR(I+1) = MAX(1,R1+R2) !DIFF.LE.0
		ELSE IF (OPER(I).EQ.10) THEN
			NR(I+1) = 1-R1*R2 !DIFF.GT.0
		ELSE
			NR(I+1) = 1-R2 !DIFF.GE.0
		ENDIF
		IS = MOVE_BLD (NR(I+1),NR(I),SUB-I)	! shift tables
		IS = MOVE_BLJ (OPER(I+1),OPER(I),SUB-I)
		SUB = SUB-1
	    ELSE
		I = I+1
	    ENDIF
	ENDDO
C
C					Perform all logical operations
C
	IS = DWC_EXPR_LOGI (NR,OPER,SUB,12,12,SWSYM)	! .AND.'s
	IF (IAND(IS,1).NE.0) IS = DWC_EXPR_LOGI (NR,OPER,SUB,13,13,SWSYM) ! .OR.'s
	IF (IAND(IS,1).NE.0) IS = DWC_EXPR_LOGI (NR,OPER,SUB,14,15,SWSYM) ! .XOR. and .EQV.'s
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Calculate the function value
C					- first check nr of parameters
C					- if there is no function, there may be
C					  1 (REAL) or 2 (COMPLEX) numbers
C
	PAR = -1
	IF (FUNC.GT.0) PAR = NRPAR
	IF (SUB.GT.ABS(PAR)) GOTO 993			! too many parms
	IF (PAR.GT.0 .AND .SUB.LT.PAR) GOTO 994		! too few parms
C
	IF (FUNC.EQ.1) THEN				! MIN
		NUM = NR(1)
		DO I = 2,SUB
			NUM = MIN (NUM,NR(I))
		ENDDO
C
	ELSE IF (FUNC.EQ.2) THEN			! MAX
		NUM = NR(1)
		DO I = 2,SUB
			NUM = MAX (NUM,NR(I))
		ENDDO
C
	ELSE IF (FUNC.EQ.3) THEN			! SIN
		NUM = SIN (NR(1)*FACTOR)
C
	ELSE IF (FUNC.EQ.4) THEN			! COS
		NUM = COS (NR(1)*FACTOR)
C
	ELSE IF (FUNC.EQ.5) THEN			! TAN
		NUM = TAN (NR(1)*FACTOR)
C
	ELSE IF (FUNC.EQ.6) THEN			! ASIN
		IF (ABS(NR(1)).GT.1) GOTO 995
		NUM = ASIN (NR(1))/FACTOR
C
	ELSE IF (FUNC.EQ.7) THEN			! ACOS
		IF (ABS(NR(1)).GT.1) GOTO 995
		NUM = ACOS (NR(1))/FACTOR
C
	ELSE IF (FUNC.EQ.8) THEN			! ATAN
		NUM = ATAN (NR(1))/FACTOR
C
	ELSE IF (FUNC.EQ.9) THEN			! ATAN2
		IF (NR(1).EQ.0.AND.NR(2).EQ.0) GOTO 995
		NUM = ATAN2 (NR(1),NR(2))/FACTOR
C
	ELSE IF (FUNC.EQ.10) THEN			! ABS
		NUM = ABS (NR(1))
C
	ELSE IF (FUNC.EQ.11) THEN
		NUM = EXP (NR(1))			! EXP
C
	ELSE IF (FUNC.EQ.12) THEN			! LOG
		IF (NR(1).LE.0) GOTO 995
		NUM = LOG (NR(1))
C
	ELSE IF (FUNC.EQ.13) THEN			! LOG10
		IF (NR(1).LE.0) GOTO 995
		NUM = LOG10 (NR(1))
C
	ELSE IF (FUNC.EQ.14) THEN			! SQRT
		IF (NR(1).LT.0) GOTO 995
		NUM = SQRT (NR(1))
C
	ELSE IF (FUNC.EQ.15) THEN			! TRUNC
		IF (NR(1).GE.2147483647.5
	1	.OR.NR(1).LE.-2147483648.5) GOTO 995
		NUM = INT (NR(1))
C
	ELSE IF (FUNC.EQ.16) THEN			! ROUND
		IF (NR(1).GE.2147483647.5
	1	.OR.NR(1).LE.-2147483648.5) GOTO 995
		NUM = NINT (NR(1))
C
	ELSE IF (FUNC.EQ.17) THEN			! MOD
		NUM = MOD (NR(1),NR(2))
C
	ELSE IF (FUNC.EQ.18) THEN			! SIGN
		IF (NR(1).LT.-1.E-8) THEN
			NUM = -1
		ELSE IF (NR(1).GT.1.E-8) THEN
			NUM = 1
		ELSE
			NUM = 0
		ENDIF
C
	ELSE IF (SUB.EQ.1) THEN				! normal REAL nr
		NUM = NR(1)
C
	ELSE						! complex nr
		COMPL = CMPLX (NR(1),NR(2))
		NUM = ABS (COMPL)
	ENDIF
C
C					Unary operator (ignore +)
C
	IF (UNOPER.EQ.5) THEN				! -
		NUM = -NUM
	ELSE IF (UNOPER.EQ.16) THEN			! .NOT.
		NR(2) = NUM
		OPER(1) = UNOPER
		SUB = 2
		IS = DWC_EXPR_LOGI (NR,OPER,SUB,16,16,SWSYM)
		IF (IAND(IS,1).EQ.0) GOTO 999
		NUM = NR(1)
	ENDIF
C
C
	DWC_EXPR_SUBX = DWC_SUCCESS
	RETURN
C
 991	DWC_EXPR_SUBX = MSG_SET (DWC_UNDEFEXP,0)
	RETURN
C
 992	DWC_EXPR_SUBX = MSG_SET (DWC_DIVBYZERO,0)
	RETURN
C
 993	DWC_EXPR_SUBX = MSG_SET (DWC_TOOMANARG,1)
	CALL WNCTXT(DWLOG,DWMSG,ABS(PAR))
	RETURN
C
 994	DWC_EXPR_SUBX = MSG_SET (DWC_TOOLITARG,1)
	CALL WNCTXT(DWLOG,DWMSG,PAR)
	RETURN
C
 995	IF (SWSYM) THEN
		NUM = 1
		DWC_EXPR_SUBX = DWC_SUCCESS
	ELSE
		DWC_EXPR_SUBX = MSG_SET (DWC_INVFUNARG,0)
	ENDIF
	RETURN
C
 999	DWC_EXPR_SUBX = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPR_LOGI (NR,OPER,SUB,TYPE1,TYPE2,SWSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	REAL*8		NR(*)		! (m) table with numbers
	INTEGER*4	OPER(*)		! (m) table with operator codes
C				12=.AND.	13=.OR.
C				14=.XOR.	15=.EQV.
C				16=.NOT.
C				OPER(I) is an operator for NR(I) and NR(I+1)
C				for .NOT. only NR(I+1)
	INTEGER*4	SUB		! (m) nr of numbers in NR
	INTEGER*4	TYPE1		! (i) demanded operator 1
	INTEGER*4	TYPE2		! (i) demanded operator 2
	LOGICAL*4	SWSYM		! (i) were unknown symbols found ?
C				If so, they have been substituted by (1); then
C				only syntax errors are significant
C
C.Purpose:
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_INTOVERFL	integer overflow during conversion
C	false status codes returned by referenced routines
C.Notes:
C	This function solves the demanded logical operations in an
C	expression.
C	Max. 2 operation-types can be solved.
C	By different calls to this function, the user can use its own
C	arithmetical priorities.
C	The values and operator-codes must reside in a table.
C	The function will adjust the tables to the left, if an
C	operation is done.
C	The needed values will be rounded to integer*4 to make logical
C	operations possible.
C-------------------------------------------------------------------------
C
	INTEGER*4	GEN_CVT_D_NR, MOVE_BLD, MOVE_BLJ
	INTEGER		MSG_SET
C
	INTEGER*4	IS
C
C
	I = 1
	DO WHILE (I.LT.SUB)
C
C					Convert to INTEGER*4
C
		IF (OPER(I).EQ.TYPE1 .OR. OPER(I).EQ.TYPE2) THEN
			IS = GEN_CVT_D_NR ('J',NR(I+1),J2)
			IF (IAND(IS,7).NE.1) THEN
				IF (.NOT.SWSYM) GOTO 999
			ENDIF
			IF (OPER(I).NE.16) THEN
				IS = GEN_CVT_D_NR ('J',NR(I),J1)
				IF (IAND(IS,7).NE.1) THEN
					IF (.NOT.SWSYM) GOTO 999
				ENDIF
			ENDIF
C
C					Do logical operation
C
			IF (OPER(I).EQ.12) THEN
				J2 = IAND (J1,J2)
			ELSE IF (OPER(I).EQ.13) THEN
				J2 = IOR (J1,J2)
			ELSE IF (OPER(I).EQ.14) THEN
				J2 = IEOR (J1,J2)
			ELSE IF (OPER(I).EQ.15) THEN
				J2 = NOT (IEOR (J1,J2))
			ELSE IF (OPER(I).EQ.16) THEN
				J2 = NOT (J2)
			ENDIF
C
C					Convert back to REAL*8 and
C					adjust the table to the left
C
			NR(I+1) = J2
			IS = MOVE_BLD (NR(I+1),NR(I),SUB-I)
			IS = MOVE_BLJ (OPER(I+1),OPER(I),SUB-I)
			SUB = SUB-1
		ELSE
			I = I+1
		ENDIF
	ENDDO
C
C
	DWC_EXPR_LOGI = DWC_SUCCESS
	RETURN
C
 999	IF (IAND(IS,1).EQ.0) THEN
		DWC_EXPR_LOGI = IS
	ELSE
		DWC_EXPR_LOGI = MSG_SET (DWC_INTOVERFL,1)
		CALL WNCTXT(DWLOG,DWMSG,'J')
	ENDIF
	RETURN
	END
