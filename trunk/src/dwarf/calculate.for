C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_CALCULATE
C.Keywords:	Calculator
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Any
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	911022 GvD - print sign when negative 0:MM:SS
C			suppress bell when showing answers
C.Version:	920206 GvD - add former optional arguments to CLI_GET/DWC_INPUT
C.Version:	920512 GvD - adapted completely to UNIX
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940118 CMV - used WNCFOP, WNCALN i.s.o. DWARF stuff
C.Version:	940119 CMV - removed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE CALCULATE
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Calculate expressions typed in by the user
C.Returns:	Not applicable
C.Notes:
C	- It is possible to define a symbol with the calculated value.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	PROGNAME, BLANK, EXPERR
		PARAMETER (PROGNAME = 'CALCULATE')
		PARAMETER (BLANK    = ' ')
		PARAMETER (EXPERR   = 'Error at or near position !SJ '//
	1		'in value-string:!/   !AS')
C
	INTEGER*4	NRARG, Q, QVAL
		PARAMETER (NRARG = 7)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QVAL  = CLI__QUALIFIER+CLI__VALUE)
	CHARACTER*10	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*12	DEFVAL(NRARG)
		DATA NAME   /'EXPRESSION'
	1			,'TYPE','RADIX','STREAM','LIST','UNIT','LOG'/
		DATA ATTR   /CLI__EXPRESSION
	1			,QVAL  ,QVAL   ,QVAL    ,Q     ,QVAL  ,Q/
		DATA PROMPT /NRARG*' '/
		DATA DEFVAL /NRARG*' '/
C
	INTEGER*4	CALCUL_DEF, CALCUL_QUAL
	INTEGER*4	DWC_CTL_OPEN, DWC_INPUT
	INTEGER*4	DWC_TSTSYM, DWC_STR_SUBST, DWC_EXPCAL
	INTEGER*4	CLI_INIT, CLI_GET, CLI_PARSE
	INTEGER*4	GEN_CVT_D_NR, GEN_CVT_NR_D, GEN_CVT_NR_L
	INTEGER*4	SYMBOL_DEFINE
	INTEGER*4	STR_COPY_U, STR_SKIP_U, STR_SKIP_W
	INTEGER		WNCAL0
C
	CHARACTER*255	LINE, COMMAND, INPUT, EXPRESSION, UNITSTR, RESULT
	CHARACTER	STREAM*12, SYMBOL*16, DATTYP*1, INTTYP*1, FORM*5
	INTEGER*4	LL, LC, LI, LX, LU, LR, LSYM
	INTEGER*4	IS, TMP, PTR, DD, MM, RADIX, LOGID
	LOGICAL*4	DO_LIST, DO_LOG, IS_DCL
	REAL*8		VALUE, R8, SS
	BYTE		BHULP(16)
	LOGICAL*4	SWSYM, SWSIGN
		DATA LOGID  /0/
		DATA IS_DCL /.FALSE./
C
C
C					Initialize
C					- get DWARF control variables
C					- initialize command-line interpreter
C
	IS = DWC_CTL_OPEN ()				! ignore false return
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					Get and interpret initial command line
C					- no expression is allowed
C					- set new argument attributes and
C					  default values, using the qualifiers
C					  supplemented with program defaults
C
	IS = CLI_GET (BLANK,COMMAND,LC)
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('EXPRESSION',EXPRESSION,LX)
	IF (IAND(IS,1).NE.0 .AND. LX.GT.0)
	1	CALL WNCTXT(F_T,'Sorry, you cannot give '//
	1		'an expression on the commandline')
	IF (IAND(IS,1).NE.0) IS = CALCUL_DEF (ATTR,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					Ask input line and save it
C					- stop if end-of-input is given
C					  (CTRL/Z,'#' or ' ')
C
 100	TMP = DWC_INPUT (INPUT,'Expression',LI,0,0)
	IF (IAND(TMP,1).EQ.0 .OR. LI.EQ.0) GOTO 900
C
C					Substitute apostrophed symbols
C
	TMP = DWC_STR_SUBST (INPUT(:LI),LINE,LL,BLANK,PTR,.FALSE.,SWSYM)
	IF (IAND(TMP,1).EQ.0) THEN
		CALL WNCTXT(F_T,EXPERR,PTR,INPUT(:LI))
		GOTO 100
	ENDIF
C
C					Process help request
C
	IF (LINE(:LL).EQ.'?') THEN
	   CALL WNCTXT(F_T,'No on-line help available')
	   GOTO 100
	ENDIF
C
C					Interpret the input line
C					- reset the command-line interpreter
C					- parse the line
C					- process the qualifiers
C					- get the expression
C
	TMP = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(TMP,1).NE.0) TMP = CLI_PARSE (LINE(:LL))
	IF (IAND(TMP,1).NE.0)
	1	TMP = CALCUL_QUAL (DATTYP,RADIX,STREAM,DO_LIST,DO_LOG,UNITSTR)
	IF (IAND(TMP,1).NE.0) TMP = CLI_GET ('EXPRESSION',LINE,LL)
	IF (IAND(TMP,1).EQ.0) THEN
		IS = TMP
		GOTO 100
	ENDIF
C
C					If format is: <name> = <expression>
C					- a symbol must be defined
C					- isolate and check the symbol name
C					- determine start of expression
C
	PTR = 1
	LSYM = 0
	TMP = STR_COPY_U ('=',LINE(:LL),PTR,SYMBOL,LSYM)
	IF (PTR.GT.LL) THEN
		LSYM = 0
		PTR = 1
	ELSE
		IF (LSYM.GT.0) LSYM = WNCAL0(SYMBOL(:LSYM))
		IF (LSYM.GT.0) THEN
			TMP = DWC_TSTSYM (SYMBOL(:LSYM))
			IF (IAND(TMP,1).EQ.0) THEN
				IS = TMP
				GOTO 100
			ENDIF
		ENDIF
		PTR = PTR+1
		TMP = STR_SKIP_W (BLANK,LINE(:LL),PTR)
	ENDIF
	EXPRESSION = LINE(PTR:LL)
	LX = LL-PTR+1
C
C					If 'UNIT=?' is given:
C					- spawn PRTUNITS.COM and ask again
C
	IF (LSYM.GT.0 .AND. EXPRESSION(:LX).EQ.'?') THEN
		IF (INDEX('UNITS',SYMBOL(:LSYM)).NE.1) THEN
		   CALL WNCTXT(F_T,'HELP is only possible for UNITS')
		ELSE
		   CALL PRTUNITS()
		ENDIF
		GOTO 100
	ENDIF
C
C					If the expression is quoted:
C					- remove the quotes
C					- don't evaluate the expression
C
	IF (EXPRESSION(1:1).EQ.'"' .AND. EXPRESSION(LX:LX).EQ.'"') THEN
		RESULT = EXPRESSION(2:LX-1)
		LR = LX-2
C		
C					Check and evaluate the expression
C					- internal datatype: 'I' i.s.o. 'L'
C					- evaluate to REAL*8 value
C					- convert to proper datatype
C
	ELSE
		SWSYM = .FALSE.
		INTTYP = DATTYP
		IF (INTTYP.EQ.'L') INTTYP = 'I'
		TMP = DWC_EXPCAL (EXPRESSION,UNITSTR,STREAM,.FALSE.,SWSYM,
	1							VALUE,PTR)
		IF (IAND(TMP,1).EQ.0) THEN
		   CALL WNCTXT(F_T,'Syntax error in the expression')
		   GOTO 100
		ENDIF
		TMP = GEN_CVT_D_NR (INTTYP,VALUE,BHULP)
		IF (TMP.NE.GEN_SUCCESS) THEN
		   IF (IAND(TMP,1).NE.0) 
	1	       CALL WNCTXT(F_T,'Integer overflow during '//
	1		   'conversion to !AS data type',INTTYP)
		   CALL WNCTXT(F_T,EXPERR,PTR,EXPRESSION)
		   GOTO 100
		ENDIF
C
C					Process the answer
C					- isolate the default unitcode
C
		PTR = 1
		TMP = STR_SKIP_U (',',UNITSTR,PTR)
		LU = WNCAL0(UNITSTR(:PTR-1))
C
C					- normal conversion to ASCII string
C					- convert logicals first from integer
C					  to logical value
C
		IF (UNITSTR(:LU).NE.'DMS' .AND. UNITSTR(:LU).NE.'HMS') THEN
			IF (DATTYP.EQ.'R') THEN
				FORM = '!E'
			ELSE IF (DATTYP.EQ.'D' .OR. DATTYP.EQ.'L') THEN
				FORM = '!'//DATTYP
			ELSE
				IF (RADIX.EQ.1) THEN
					FORM = '!S'//DATTYP
				ELSE IF (RADIX.EQ.2) THEN
					FORM = '%O!O'//DATTYP
				ELSE IF (RADIX.EQ.3) THEN
					FORM = '%X!X'//DATTYP
				ENDIF
			ENDIF
			IF (DATTYP.EQ.'L') TMP = GEN_CVT_NR_L (INTTYP,BHULP)
			CALL WNCTXS(RESULT,FORM,BHULP)
			LR=WNCAL0(RESULT)
C
C					- convert to DD:MM:SS.SSSSSS string
C
		ELSE
			TMP = GEN_CVT_NR_D (INTTYP,BHULP,R8)	! -> REAL*8
			SWSIGN = R8.LT.0
			IF (SWSIGN) R8 = -R8
			DD = INT(R8)				! unsigned DD
			R8 = (R8-DD)*60
			MM = INT(R8)				! unsigned MM
			SS = (R8-MM)*60				! SS.SSSSSS
			IF (SWSIGN) THEN
			   CALL WNCTXS(RESULT,'-!SJ:!SJ:!D.6',
	1				DD,MM,SS)
			ELSE
			   CALL WNCTXS(RESULT,'!SJ:!SJ:!D.6',
	1				DD,MM,SS)
			ENDIF
			LR=WNCAL0(RESULT)
		ENDIF
	ENDIF
C
C					Output the result
C					- type if /LIST
C					- define the symbol
C					- log if /LOG
C					Symbol assignments are prefixed with
C					'$'; all other lines are prefixed with
C					'!'. The log file can later be executed
C					as a command file.
C
	IF (DO_LIST) THEN
	   IF (LSYM.GT.0) THEN
	      CALL WNCTXT(F_T,'!5C!AS = !AS',
	1			SYMBOL(:LSYM),RESULT(:LR))
	   ELSE
	      CALL WNCTXT(F_T,'!5C    = !AS',RESULT(:LR))
	   END IF
	END IF
C
	IF (LSYM.GT.0) TMP = SYMBOL_DEFINE (SYMBOL(:LSYM),
	1			RESULT(:LR),DWC__GLOBALSYM)
C
	IF (DO_LOG) THEN
	   LOGID=1				!Flag logging occurred
	   CALL WNCTXT(F_P,'$!! Expression: !AS',INPUT(:LI))
	   IF (LSYM.GT.0) THEN
	      CALL WNCTXT(F_P,'$!_!AS :== !AS',
	1		SYMBOL(:LSYM),RESULT(:LR))
	   ELSE
	      CALL WNCTXT(F_P,'$!!      = !AS',RESULT(:LR))
	   ENDIF
	ENDIF
C
	GOTO 100
C
900	CONTINUE
990	IF (LOGID.EQ.0) LOGCD=F_NO		!Throw away if empty
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE PRTUNITS()
C
	INCLUDE	'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	READ_UNITG_ALL, READ_UNITG, READ_UNIT
	INTEGER*4	STR_COPY_U
	INTEGER		WNCALN
C
	CHARACTER	GLIST*80, ULIST*255, GROUP*10, UNIT*8, DUM*1
	INTEGER*4	IS, LGL, LUL, PTR, PTR2, LG, LU, LDUM
	INTEGER*4	PRTID /0/, PRTFLAGS, MSGFLAGS
	REAL*8		FACTOR
C
	CALL WNCTXT(F_TP,'Currently recognized DWARF units:')
	CALL WNCTXT(F_TP,'!2/!5CGroup !15CUnit !25CFactor')
C
C					Write to print file
C
	IS = READ_UNITG_ALL (GLIST)		! get all groups
	IF (IAND(IS,1).EQ.0) GOTO 999
	LGL = WNCALN(GLIST)
	PTR = 1
	DO WHILE (PTR.LE.LGL)
	   LG = 0
	   IS = STR_COPY_U (',',GLIST(:LGL),PTR,GROUP,LG) ! next group
	   IS = READ_UNITG (GROUP(:LG),ULIST)		! get all units
	   IF (IAND(IS,1).EQ.0) GOTO 999
	   LUL = WNCALN(ULIST)
	   PTR2 = 1
	   LU = 0
	   IS = STR_COPY_U (',',ULIST(:LUL),PTR2,UNIT,LU) ! first unit
	   IS = READ_UNIT (UNIT(:LU),GROUP,FACTOR)		! get factor
	   IF (IAND(IS,1).EQ.0) GOTO 999
	   CALL WNCTXT(F_TP,'!/!5C!AS !15C!AS !25C!D',
	1			  GROUP(:LG),UNIT(:LU),FACTOR)
	   PTR2 = PTR2+1
	   DO WHILE (PTR2.LE.LUL)				! do all units
	      LU = 0
	      IS = STR_COPY_U (',',ULIST(:LUL),PTR2,UNIT,LU)
	      IS = READ_UNIT (UNIT(:LU),GROUP,FACTOR)
	      IF (IAND(IS,1).EQ.0) GOTO 999
	      CALL WNCTXT(F_TP,'!15C!AS !25C!D',UNIT(:LU),FACTOR)
	      PTR2 = PTR2+1
	   ENDDO
	   PTR = PTR+1
	ENDDO
	RETURN
C
 999	CALL WNCTXT(F_TP,'Error reading units...')
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CALCUL_QUAL (DATTYP,RADIX,STREAM,
	1				DO_LIST,DO_LOG,UNITSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DATTYP		! (o) data type
	INTEGER*4	RADIX		! (o) data-radix
	CHARACTER*(*)	STREAM		! (o) stream name
	LOGICAL*4	DO_LIST		! (o) list ?
	LOGICAL*4	DO_LOG		! (o) log ?
	CHARACTER*(*)	UNITSTR		! (o) string with possible unitcodes
C
C.Purpose:	Check/convert the qualifiers of the program CALCULATE
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CALINVTYP	invalid datatype
C	error	DWC_CALINVRAD	invalid radix
C	error	DWC_STRINVNR	invalid stream name
C	.false. status codes from CLI or READ_UNIT routines
C.Notes:
C	- If a qualifier is negated, the program default will be used.
C	- If a unitcode is given, all possible unitcodes of the same group will
C	  be appended (comma-separated list). The given unitcode will be used
C	  as the default by the EXP-routines.
C	- If an error is detected, a message will be written.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	DWC_STREAM_CHECK, DWC_STREAM_GET, CLI_GET
	INTEGER*4	READ_UNIT, READ_UNITG
C
	REAL*8    	FACTOR
	CHARACTER	VAL*80, GROUP*16, DUM*1
	INTEGER*4	IS, LS, LVAL, LDUM, TYPNR
	CHARACTER	RADLIST*3, TYPLIST*6
		DATA RADLIST /'DOX'/		! decimal, octal, hexadecimal
		DATA TYPLIST /'BIJLRD'/		! I1, I2, I4, L2, R4, R8
C
C
C
C					Output radix
C					- ignore '%' in qualifier value
C					- set to decimal if /NORADIX
C
	IS = CLI_GET ('RADIX',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		IF (VAL(1:1).EQ.'%') THEN
			RADIX = INDEX (RADLIST,VAL(2:2))
		ELSE
			RADIX = INDEX (RADLIST,VAL(1:1))
		ENDIF
		IF (RADIX.EQ.0) CALL WNCTXT(F_T,
	1	'!AS is an invalid radix-option (D, O or X is valid)',
	2	VAL(:LVAL))
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		RADIX = 1
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					Data type
C					- real*8 if /NOTYPE
C					- integer for non-decimal radix
C					  (type J i.s.o. non-integer types)
C
	IS = CLI_GET ('TYPE',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		TYPNR = INDEX (TYPLIST,VAL(1:1))
		IF (TYPNR.EQ.0) THEN
			CALL WNCTXT(F_T,
	1	'!AS is an invalid datatype (B,I,J,L,R,D are valid)',
	2	VAL(:LVAL))
			GOTO 990
		ENDIF
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		TYPNR = 6
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
	IF (RADIX.GT.1) TYPNR = MIN(3,TYPNR)
	DATTYP = TYPLIST(TYPNR:TYPNR)
C
C					Stream name
C					- check syntax and prefix '$' if needed
C					- use DWARF$STREAM if /NOSTREAM
C
	IS = CLI_GET ('STREAM',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = DWC_STREAM_CHECK (VAL(:LVAL),STREAM,LS,.FALSE.)
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					List flag
C
	IS = CLI_GET ('LIST',DUM,LDUM)
	DO_LIST = IS.EQ.DWC_PRESENT
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					Log flag
C
	IS = CLI_GET ('LOG',DUM,LDUM)
	DO_LOG = IS.EQ.DWC_PRESENT
	IF (IAND(IS,1).EQ.0) GOTO 990
C
C					Unit code
C					- no unit if /NOUNIT
C					- append all unit codes in the group
C
	IS = CLI_GET ('UNIT',VAL,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 990
	IF (IS.EQ.DWC_NEGATED) THEN
		VAL = '1'
		LVAL = 1
	ENDIF
	IS = READ_UNIT (VAL(:LVAL),GROUP,FACTOR)
	IF (IAND(IS,1).NE.0) THEN
		UNITSTR = VAL(:LVAL)//','
		IS = READ_UNITG (GROUP,UNITSTR(LVAL+2:))
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
C
	CALCUL_QUAL = DWC_SUCCESS
	RETURN
C
 990	CALCUL_QUAL = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CALCUL_DEF (ATTR,DEFVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	ATTR(*)		! (m) argument attributes
	CHARACTER*(*)	DEFVAL(*)	! (m) default values
C
C.Purpose:	Modify the default syntax for input to the program CALCULATE
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CALINVTYP	invalid datatype
C	error	DWC_CALINVRAD	invalid radix
C	error	DWC_STRINVNR	invalid stream name
C	.false. status codes from CLI or READ_UNIT routines
C.Notes:
C	- All qualifiers with a value (TYPE, RADIX, STREAM and UNIT) will be
C	  set to present-by-default. If the qualifier is present on the initial
C	  command line, the associated value will be the default. Otherwise,
C	  the program default will be used ('D', 'D', DWARF$STREAM and '1').
C	- If an error is detected, a message will be written.
C-------------------------------------------------------------------------
C
	INTEGER*4	DWC_STREAM_CHECK, DWC_STREAM_GET, CLI_GET
	INTEGER*4	READ_UNIT
C
	REAL*8    	FACTOR
	CHARACTER	VAL*80, STREAM*12, UNIT*8, GROUP*16, DUM*1
	INTEGER*4	IS, LVAL, LDUM, LS, RADIX, TYPE
C
	CHARACTER	RADLIST*3, TYPLIST*6
		DATA RADLIST /'DOX'/		! decimal, octal, hexadecimal
		DATA TYPLIST /'BIJLRD'/		! I1, I2, I4, L2, R4, R8
C
C
C
C					Output radix
C					- default: decimal
C					- ignore '%' in qualifier value
C
	RADIX = 1
	IS = CLI_GET ('RADIX',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		IF (VAL(1:1).EQ.'%') THEN
			RADIX = INDEX (RADLIST,VAL(2:2))
		ELSE
			RADIX = INDEX (RADLIST,VAL(1:1))
		ENDIF
		IF (RADIX.EQ.0) CALL WNCTXT(F_T,
	1	'!AS is an invalid radix-option (D, O or X is valid)',
	2	VAL(:LVAL))
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
	ATTR(3) = CLI__QUALIFIER+CLI__DEFAULT+CLI__VALUE
	DEFVAL(3) = RADLIST(RADIX:RADIX)
C
C					Data type
C					- default: REAL*8
C					- integer for non-decimal radix
C					  (type J i.s.o. non-integer types)
C
	TYPE = 6
	IS = CLI_GET ('TYPE',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		TYPE = INDEX (TYPLIST,VAL(1:1))
		IF (TYPE.EQ.0) CALL WNCTXT(F_T,
	1	'!AS is an invalid datatype (B,I,J,L,R,D are valid)',
	2	VAL(:LVAL))
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
	IF (RADIX.GT.1) TYPE = MIN(3,TYPE)
	ATTR(2) = CLI__QUALIFIER+CLI__DEFAULT+CLI__VALUE
	DEFVAL(2) = TYPLIST(TYPE:TYPE)
C
C					Stream name
C					- default: DWARF$STREAM
C					- check syntax and prefix '$'
C
	IS = CLI_GET ('STREAM',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = DWC_STREAM_CHECK (VAL(:LVAL),STREAM,LS,.FALSE.)
	ELSE
		IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
	ATTR(4) = CLI__QUALIFIER+CLI__DEFAULT+CLI__VALUE
	DEFVAL(4) = STREAM(:LS)
C
C					List flag
C					- default: list the results
C
	ATTR(5) = CLI__QUALIFIER+CLI__DEFAULT
	IS = CLI_GET ('LIST',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 990
	IF (IS.EQ.DWC_NEGATED) ATTR(5) = CLI__QUALIFIER
C
C					Log flag
C					- default: don't log the results
C
	ATTR(7) = CLI__QUALIFIER
	IS = CLI_GET ('LOG',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 990
	IF (IS.EQ.DWC_PRESENT) ATTR(7) = CLI__QUALIFIER+CLI__DEFAULT
C
C					Unit code
C					- default: no unit
C
	UNIT = '1'
	IS = CLI_GET ('UNIT',VAL,LVAL)
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = READ_UNIT (VAL(:LVAL),GROUP,FACTOR)
		IF (IAND(IS,1).NE.0) UNIT = VAL(:LVAL)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 990
	ATTR(6) = CLI__QUALIFIER+CLI__DEFAULT+CLI__VALUE
	DEFVAL(6) = UNIT
C
	CALCUL_DEF = DWC_SUCCESS
	RETURN
C
 990	CALCUL_DEF = IS
	RETURN
	END
