C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PV_SET
C.Keywords:	Parameter Values, Sets
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900421 FMO - recreation
C.Version:	900831 FMO - split PV_SET_TOBY
C.Version:	911227 GvD - use symbolic names PARM__ in PV_SET_ENCODE
C			handle PARM__EOF
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920508 GvD - convert logical values to correct format
C.Version:	930413 WNB - cater for < .5 increment if integer
C------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy function
C-------------------------------------------------------------------------
C
	PV_SET = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_ENCODE (DTYPE,LARR,NRARR,ARRAY,TOBY,
	1							STRING,LSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D,L,C)
	INTEGER*4	LARR		! (i) length of each value (bytes)
	INTEGER*4	NRARR		! (i) nr of values in the set
	BYTE		ARRAY(LARR,*)	! (i) value set
	LOGICAL*4	TOBY		! (i) in TOBY format ?
	CHARACTER*(*)	STRING		! (o) value string
	INTEGER*4	LSTR		! (o) significant length of STRING
C
C.Purpose:	Convert a value set to a value string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	string is truncated
C	false status codes returned by referenced modules
C.Notes:
C	- NRARR = 0 indicates a "null" set; it will be converted to a double
C	  quote ("").
C	- NRARR = -1 indicates a "wild" set; it will be converted to an
C	  asterisk (*).
C	- NRARR = -2 indicates an "eof" set; it will be converted to a
C	  num-sign (#).
C	- The values in the string will be separated by comma's.
C	- If the output string is too short, the offending value (and its
C	  delimiter) is not put out.
C	Character (data type 'C'):
C	- Undefined values (starting with UNDEF_C) will be converted to empty
C	  string fields e.g. ,,).
C	- If a value is not extended alphanumeric (uppercase characters,
C	  dollar and underscore), it will be enclosed in quotes.
C	- Embedded quotes will be converted to double quotes.
C	- White values (only blanks and tabs) will be converted to single
C	  quoted blanks.
C	Numerical (data types L,B,I,J,R,D):
C	- Undefined values (UNDEF_<type>) will be converted to empty string
C	  fields e.g. ,,).
C	- For sets in TOBY format TO and BY will be inserted where necessary:
C	  TO is needed if the start and end values are not equal;
C	  BY is needed if the increment is not equal to 1.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, NULLVAL, WILDVAL, EOFVAL, VALDELIM, TO, BY
		PARAMETER (BLANK    = ' ' )
		PARAMETER (NULLVAL  = '""')
		PARAMETER (WILDVAL  = '*' )
		PARAMETER (EOFVAL   = '#')
		PARAMETER (VALDELIM = ',' )
		PARAMETER (TO       = ' TO ')
		PARAMETER (BY       = ' BY ')
C
	INTEGER*4	PV_VAL_ENCODE
	INTEGER*4	STR_COPY, GEN_CVT_NR_D, MSG_SET  
C
	INTEGER*4	IS, LADD, VALNR
	REAL*8		TBS, TBE, TBI
C
C
	STRING = BLANK
	LSTR = 0
C
C					"null" set
C
	IF (NRARR.EQ.PARM__NULL) THEN
		IS = STR_COPY (NULLVAL,STRING,LSTR)
		IF (IS.LT.0) GOTO 991
C
C					"wild" set
C
	ELSE IF (NRARR.EQ.PARM__WILD) THEN
		IS = STR_COPY (WILDVAL,STRING,LSTR)
		IF (IS.LT.0) GOTO 991
C
C					"eof" set
C
	ELSE IF (NRARR.EQ.PARM__EOF) THEN
		IS = STR_COPY (EOFVAL,STRING,LSTR)
		IF (IS.LT.0) GOTO 991
C
C					Normal character or numerical set
C					
C
	ELSE IF (.NOT.TOBY) THEN
	 DO VALNR = 1,NRARR
		IS = PV_VAL_ENCODE (DTYPE,LARR,ARRAY(1,VALNR),
	1						STRING(LSTR+1:),LADD)
		IF (IAND(IS,1).EQ.0) GOTO 999
		LSTR = LSTR+LADD
		IF (VALNR.NE.NRARR) THEN
			IS = STR_COPY (VALDELIM,STRING,LSTR)
			IF (IS.LT.0) GOTO 991
		ENDIF
	 ENDDO
C
C					Numerical set in TOBY format
C					- NRARR is a multiple of 3
C
C
	ELSE
C
C					- write TOBY set only if
C					  the start value is defined
C
	    DO VALNR = 1,NRARR,3
		IS = GEN_CVT_NR_D (DTYPE,ARRAY(1,VALNR),TBS)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (TBS.NE.UNDEF_D) THEN			! write start
		    IS = PV_VAL_ENCODE (DTYPE,LARR,ARRAY(1,VALNR),
	1						STRING(LSTR+1:),LADD)
		    IF (IAND(IS,1).EQ.0) GOTO 999
		    LSTR = LSTR+LADD
C
C					- write end value only if
C					  |end-start| >= |increment|
 
		    IS = GEN_CVT_NR_D (DTYPE,ARRAY(1,VALNR+1),TBE)
		    IF (IAND(IS,1).NE.0)
	1		IS = GEN_CVT_NR_D (DTYPE,ARRAY(1,VALNR+2),TBI)
		    IF (IAND(IS,1).EQ.0) GOTO 999
		    IF (ABS(TBE-TBS).GE.ABS(TBI)) THEN
			IS = STR_COPY (TO,STRING,LSTR)		! write ' TO '
			IF (IS.LT.0) GOTO 991			!  and end val
			IS = PV_VAL_ENCODE (DTYPE,LARR,ARRAY(1,VALNR+1),
	1						STRING(LSTR+1:),LADD)
			IF (IAND(IS,1).EQ.0) GOTO 999
			LSTR = LSTR+LADD
C
C					- write increment only if
C					  the increment is not 1
C
			IF (TBI.NE.1) THEN
			    IS = STR_COPY (BY,STRING,LSTR)	! write ' BY '
			    IF (IS.LT.0) GOTO 991		!  and incremnt
			    IS = PV_VAL_ENCODE (DTYPE,LARR,ARRAY(1,VALNR+2),
	1					STRING(LSTR+1:),LADD)
			    IF (IAND(IS,1).EQ.0) GOTO 999
			    LSTR = LSTR+LADD
			ENDIF
		    ENDIF
		ENDIF
		IF (VALNR+3.LE.NRARR) THEN
			IS = STR_COPY (VALDELIM,STRING,LSTR)
			IF (IS.LT.0) GOTO 991
		ENDIF
	    ENDDO
	ENDIF
C
	PV_SET_ENCODE = DWC_SUCCESS
	RETURN
 991	PV_SET_ENCODE = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STRING))
	RETURN
 999	PV_SET_ENCODE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_DECODE (STRING,DTYPE,LARR,NRARR,ARRAY,
	1		NR,SWDEF,STREAM,CHKSW,SWSYM,SWDV,TOBY,DEFARR,NRDEF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) value string (single set)
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D,L,C)
	INTEGER*4	LARR		! (i) length of each value (bytes)
	INTEGER*4	NRARR		! (i) nr of values in ARRAY
	BYTE		ARRAY(LARR,*)	! (o) output array
	INTEGER*4	NR		! (o) nr of used elements in set
	LOGICAL*4	SWDEF(*)	! (o) corresp. value given by user ?
	CHARACTER*(*)	STREAM		! (i) stream name (for substitution)
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
	LOGICAL*4	SWDV		! (i) default value string ?
	LOGICAL*4	TOBY		! (i) ARRAY in TOBY format ?
	BYTE		DEFARR(LARR,*)	! (i) default array
	INTEGER*4	NRDEF		! (i) nr of values in DEFARR
C
C.Purpose:	Convert a value set to a value (and switch) array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_WILDNOTAL	wildcard values (*) not allowed
C	error	DWC_NULLNOTAL	null values ("") not allowed
C	error	DWC_NOVALDEF	no value given and no default available
C	error	DWC_EXPERRMSG	expression error
C.Notes:
C	- Values that are not given by the user will be replaced by the
C	  corresponding default values (if present), unless the parameter
C	  has the UNDEFINED attribute. For scalars and arrays only explicitly
C	  undefined values will be defaulted; for vectors all undefined values
C	  will be defaulted. E.g. for a 5-valued parameter:
C		1,,2,	->	1,def,2,def		(array)
C		1,,2,	->	1,def,2,def,def		(vector)
C	- Numerical values can be given as numerical expressions containing
C	  with most Fortran intrinsic functions and with symbol-names. Such
C	  expressions will be evaluated after symbol substitution.
C	- Logical values are treated as integers (B,I,J depending on LARR).
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, EOFVAL, WILDVAL, NULLVAL
		PARAMETER (BLANK = ' ')
		PARAMETER (EOFVAL = '#')
		PARAMETER (WILDVAL = '*')
		PARAMETER (NULLVAL = '""')
C
	INTEGER*4	PV_SET_DECODE_C, PV_SET_DECODE_N
	INTEGER*4	PPD_AMAS_GET
	INTEGER*4	MSG_SET  , MOVE_BLB
C
	INTEGER*4	IS, ERRPTR
C
C
C					Decode special value set
C
	IF (STRING.EQ.WILDVAL) THEN
		IF (IAND(PPD_AMAS_GET('WILD_CARDS'),1) .EQ. 0) GOTO 991
		NR = PARM__WILD
	ELSE IF (STRING.EQ.NULLVAL) THEN
		IF (IAND(PPD_AMAS_GET('NULL_VALUES'),1) .EQ. 0) GOTO 992
		NR = PARM__NULL
	ELSE IF (STRING.EQ.EOFVAL) THEN
		NR = PARM__EOF
C
C					Decode normal set
C
	ELSE
		IF (DTYPE.EQ.'C') THEN
			IS = PV_SET_DECODE_C (STRING,
	1				LARR,NRARR,ARRAY,NR,SWDEF,ERRPTR)
		ELSE
			IS = PV_SET_DECODE_N (STRING,STREAM,CHKSW,SWSYM,DTYPE,
	1				LARR,NRARR,TOBY,ARRAY,NR,SWDEF,ERRPTR)
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 993
C
C					For a vector all values will be returned
C
		IF (IAND(PPD_AMAS_GET('VECTOR'),1).NE.0 .AND. NR.LT.NRARR)
	1		NR = NRARR
C
C					Replace undefined values by defaults
C					- unless the undefined attribute is set
C					- for vectors: also replace trailing
C					  undefined values
C
		IF (IAND(PPD_AMAS_GET('UNDEFINED_VALUES'),1) .EQ. 0) THEN
		    DO J = 1,NR
			IF (.NOT.SWDEF(J)) THEN
			    IF (J.GT.NRDEF) GOTO 994
			    IS = MOVE_BLB (DEFARR(1,J),ARRAY(1,J),LARR)
			ENDIF
		    ENDDO
		ENDIF
C
C					If the string is a default value:
C					- set all define switches to undefined
C
		IF (SWDV) THEN
		    DO J = 1,NRARR
			SWDEF(J) = .FALSE.
		    ENDDO
		ENDIF
	ENDIF
C
C
	PV_SET_DECODE = DWC_SUCCESS
	RETURN
 991	PV_SET_DECODE = MSG_SET (DWC_WILDNOTAL,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK)
	RETURN
 992	PV_SET_DECODE = MSG_SET (DWC_NULLNOTAL,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK)
	RETURN
 993	PV_SET_DECODE = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK,ERRPTR,STRING)
	RETURN
 994	IF (TOBY) J = 1+J/3
	IS = MSG_SET (DWC_NOVALDEF,1)
	CALL WNCTXT(DWLOG,DWMSG,J)
	PV_SET_DECODE = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK,1,STRING)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_DECODE_C (STRING,LARR,NRARR,ARRAY,
	1						NRVAL,SWDEF,ERRPTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) input character string
	INTEGER*4	LARR		! (i) maximum length of a value
	INTEGER*4	NRARR		! (i) maximum nr of values
	BYTE		ARRAY(LARR,*)	! (o) output array
	INTEGER*4	NRVAL		! (o) nr of used values
	LOGICAL*4	SWDEF(*)	! (o) value defined ?
	INTEGER*4	ERRPTR		! (o) position of error in input string
C
C.Purpose:	Decode a character-type value set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	work string is too short
C	error	DWC_TOOMANYEL	too many elements given
C	error	DWC_TOOMANCHR	too many characters for an element
C	false status code returned by referenced routine
C.Notes:
C	- Comma's outside quoted substrings are treated as value separators.
C	- Quotes that delimit quoted substrings are not outputted, but double
C	  quotes inside a quoted substring are replaced by a single quote.
C	  E.g.	AB"cd"EF 	==>	ABcdEF
C		"AB""CD"	==>	AB"CD
C		""""		==>	"
C	- Undefined values are empty values in the string (e.g. ,,); they are
C	  counted as used values.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	QUOTE, COMMA, BLANK
		PARAMETER (QUOTE = '"')
		PARAMETER (COMMA = ',')
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	STR_COPY_U, STR_COPY
	INTEGER*4	MSG_SET  , MOVE_BLB
C
	CHARACTER*255	WORK
	INTEGER*4	IS, LW, LSTR, PTR
	LOGICAL*4	END_VAL, IS_QUOTED
C
C
C					Parse the string
C					- go to LSTR+1 to catch trailing comma
C
	LSTR = LEN (STRING)
	NRVAL = 0
	PTR = 1
	ERRPTR = PTR
	WORK = BLANK
	LW = 0
	DO WHILE (PTR.LE.LSTR+1)
C
C					Copy until end-of-value or
C					start of quoted substring
C
	    IS = STR_COPY_U (QUOTE//COMMA,STRING,PTR,WORK,LW)
	    IF (IS.LT.0) GOTO 993
	    END_VAL = PTR.GT.LSTR .OR. STRING(PTR:PTR).EQ.COMMA
C
C					If quoted substring:
C					- loop until end of quoted substring
C					- skip start quote
C					- copy until quote or end-of-string
C					- if double quote: copy only one
C					- otherwise: end of quoted substring
C
	    IF (.NOT.END_VAL) THEN
		IS_QUOTED = .TRUE.
		DO WHILE (IS_QUOTED .AND. .NOT.END_VAL)
		    PTR = PTR+1
		    IS = STR_COPY_U (QUOTE,STRING,PTR,WORK,LW)
		    IF (IS.LT.0) GOTO 993
		    PTR = PTR+1
		    IF (PTR.GT.LSTR .OR. STRING(PTR:PTR).EQ.COMMA) THEN
			END_VAL = .TRUE.
		    ELSE IF (STRING(PTR:PTR).EQ.QUOTE) THEN
			IS = STR_COPY (QUOTE,WORK,LW)
			IF (IS.LT.0) GOTO 993
		    ELSE
			IS_QUOTED = .FALSE.
		    ENDIF
		ENDDO
	    ENDIF
C
C					If end-of-value:
C					- check length and nr of value
C					- move the value into the array
C					- set or clear the value-defined switch
C					- prepare for next value
C
	    IF (END_VAL) THEN
		IF (LW.GT.LARR) GOTO 991		! too long a value
		IF (NRVAL.GE.NRARR) GOTO 992		! too many values
		NRVAL = NRVAL+1
		IS = MOVE_BLB (%REF(WORK),ARRAY(1,NRVAL),LARR)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LW.GT.0) THEN
		    SWDEF(NRVAL) = .TRUE.
		ELSE
		    ARRAY(1,NRVAL) = UNDEF_B
		    SWDEF(NRVAL) = .FALSE.
		ENDIF
		PTR = PTR+1
		ERRPTR = PTR
		WORK = BLANK
		LW = 0
	    ENDIF
	ENDDO
C
C					Fill out the value and switch arrays
C
	DO J = NRVAL+1,NRARR
		IS = MOVE_BLB (%REF(WORK),ARRAY(1,J),LARR)
		IF (IAND(IS,1).EQ.0) GOTO 999
		ARRAY(1,J) = UNDEF_B
		SWDEF(J) = .FALSE.
	ENDDO
C
C
	ERRPTR = 0
	PV_SET_DECODE_C = DWC_SUCCESS
	RETURN
 991	PV_SET_DECODE_C = MSG_SET (DWC_TOOMANCHR,1)
	CALL WNCTXT(DWLOG,DWMSG,LARR)
	RETURN
 992	PV_SET_DECODE_C = MSG_SET (DWC_TOOMANYEL,1)
	CALL WNCTXT(DWLOG,DWMSG,NRARR)
	RETURN
 993	PV_SET_DECODE_C = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(WORK))
	RETURN
 999	PV_SET_DECODE_C = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_DECODE_N (STRING,STREAM,CHKSW,SWSYM,
	1			DTYPE,LARR,NRARR,TOBY,ARRAY,NRVAL,SWDEF,ERRPTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) string with values
	CHARACTER*(*)	STREAM		! (i) stream name (symbol substitution)
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D,L)
	INTEGER*4	LARR		! (i) length of a value
	INTEGER*4	NRARR		! (i) maximum nr of values
	LOGICAL*4	TOBY		! (i) set in TOBY format ?
	BYTE		ARRAY(LARR,*)	! (o) output array
	INTEGER*4	NRVAL		! (o) nr of used values
	LOGICAL*4	SWDEF(*)	! (o) value defined ?
	INTEGER*4	ERRPTR		! (o) position of error in string
C
C.Purpose:	Decode a numerical value set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	work string is too short
C	error	DWC_UNBPAREN	unbalanced parentheses
C	error	DWC_TSNOTALL	TO/BY not allowed
C	error	DWC_STEPNOTAL	BY not allowed in this position
C	error	DWC_TONOTALL	TO not allowed in this position
C	error	DWC_STEPSIGN	BY-value has wrong sign
C	error	DWC_STEPISZER	BY-value is zero
C	error	DWC_TOOMANYEL	too many numbers given
C	error	DWC_INTOVERFL	integer overflow during conversion
C	false status code returned by referenced routines
C.Notes:
C	- The expression string is split into single expressions, these are
C	  evaluated, and put into the array in the wanted format.
C	- Logical values are treated as integers (B,I,J depending on LARR).
C	- The values/expressions must be separated by comma's. By means of TO/BY
C	  the user can give start,end,increment if that is allowed by the PPD
C	  file. E.g.:	1 TO 10 BY 2,20 TO 100 BY 5
C	- The words TO and BY must be preceded and followed by a blank.
C	  The increment cannot be 0 and must have the right sign.
C	- In some situations (SPECIFY and BLDPPD) symbol values may not be known
C	  yet. In that case, CHKSW = .TRUE. and any unknown symbol name is
C	  substituted by 1 (then, only the syntax will be checked).
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	COMMA, BLANK, OPPAR, CLPAR, TODELIM, BYDELIM
		PARAMETER (COMMA = ',')
		PARAMETER (BLANK = ' ')
		PARAMETER (OPPAR = '(')
		PARAMETER (CLPAR = ')')
		PARAMETER (TODELIM = ' TO ')
		PARAMETER (BYDELIM = ' BY ')
C
	INTEGER*4	PV_VAL_DECODE_N, GEN_CVT_D_NR, GEN_CVT_NR_L
	INTEGER*4	GEN_CVT_NR_D		! 930413
	INTEGER*4	STR_COPY_U, STR_COPY
	INTEGER*4	MSG_SET  
C
	CHARACTER	WORK*255, DTYP2*1
	INTEGER*4	IS, PTR, LSTR, LW
	INTEGER*4	DEPTH, NRIE, SW, SAVPTR(3)
	LOGICAL*4	IS_PARENT, IS_ENDVAL
	REAL*8		SIE(3)
C
C
	LSTR = LEN (STRING)
	NRVAL = 0
C
C					Treat logical as integer values
C
	DTYP2 = DTYPE
	IF (DTYPE.EQ.'L') THEN
		IF (LARR.EQ.1) THEN
			DTYP2 = 'B'
		ELSE IF (LARR.EQ.2) THEN
			DTYP2 = 'I'
		ELSE
			DTYP2 = 'J'
		ENDIF
	ENDIF
C
C					Initialize the array
C					- with undefined values
C					- set all switches to undefined
C
	DO I = 1,NRARR
		IS = GEN_CVT_D_NR (DTYP2,UNDEF_D,ARRAY(1,I))
		IF (IAND(IS,1).EQ.0) GOTO 999
		SWDEF(I) = .FALSE.
	ENDDO
C
C					Initialize pointers and switches
C
	SW = 1					! start value expected
	SAVPTR(1) = 1				! its position
	PTR = 1					! pointer in the string
	NRIE = 1				! only start
	IF (TOBY) NRIE = 3			! start,increment,end
	WORK = BLANK
	LW = 0
C
C					Parse the string
C					- copy to a work string, because
C					  VAL_DECODE_N might modify the string
C
	DO WHILE (PTR.LE.LSTR+1)
	    IS = STR_COPY_U (COMMA//OPPAR//CLPAR//BLANK,STRING,PTR,
	1							WORK,LW)
	    IF (IS.LT.0) GOTO 990			! work string too short
	    IS_ENDVAL = PTR.GT.LSTR .OR. STRING(PTR:PTR).EQ.COMMA
C
C					If subexpression (parenthesized):
C					- copy until end of subexpression
C
	    IF (.NOT.IS_ENDVAL) THEN
		IF (STRING(PTR:PTR).EQ.CLPAR) GOTO 991	! unbalanced parentheses
		IF (STRING(PTR:PTR).EQ.OPPAR) THEN
		    DEPTH = 1
		    IS_PARENT = .TRUE.
		    DO WHILE (IS_PARENT)
			IS = STR_COPY (STRING(PTR:PTR),WORK,LW)
			PTR = PTR+1
			IS = STR_COPY_U (OPPAR//CLPAR,STRING,PTR,WORK,LW)
			IF (IS.LT.0) GOTO 990		! work string too short
			IF (PTR.GT.LSTR) GOTO 991	! unbalanced parentheses
			IF (STRING(PTR:PTR).EQ.CLPAR) THEN
			    DEPTH = DEPTH-1
			    IF (DEPTH.LT.0) GOTO 991	! unbalanced parentheses
			    IF (DEPTH.EQ.0) THEN
				IS_PARENT = .FALSE.
				IS = STR_COPY (STRING(PTR:PTR),WORK,LW)
				IF (IS.LT.0) GOTO 990	! work string too short
				PTR = PTR+1
			    ENDIF
			ELSE
			    DEPTH = DEPTH+1
			ENDIF
		    ENDDO
C
C					TO ?
C
		ELSE IF (STRING(PTR:PTR+3).EQ.TODELIM) THEN
		    IF (.NOT.TOBY) GOTO 992		! TOBY not allowed
		    IF (SW.NE.1) GOTO 993		! no start val expected
		    IS = PV_VAL_DECODE_N (WORK,STREAM,CHKSW,SWSYM,SIE(1))
		    IF (IAND(IS,1).EQ.0) GOTO 995		! decode error
		    PTR = PTR+3				! skip delimiter
		    SW = 2				! expect end value
		    SAVPTR(2) = PTR			! its start position
		    WORK = BLANK
		    LW = 0				! clear work string
C
C					BY ?
C
		ELSE IF (STRING(PTR:PTR+3).EQ.BYDELIM) THEN
		    IF (NRIE.EQ.1) GOTO 992		! TOBY not allowed
		    IF (SW.NE.2) GOTO 994		! no end val expected
		    IS = PV_VAL_DECODE_N (WORK,STREAM,CHKSW,SWSYM,SIE(2))
		    IF (IAND(IS,1).EQ.0) GOTO 995		! decode error
		    PTR = PTR+3
		    SW = 3				! expect increment
		    SAVPTR(3) = PTR
		    WORK = BLANK
		    LW = 0				! clear work string
C
C					Just a blank
C
		ELSE
		    IS = STR_COPY (STRING(PTR:PTR),WORK,LW)
		    IF (IS.LT.0) GOTO 990		! work string too short
		    PTR = PTR+1				! copy it
		ENDIF
C
C					End of value found
C
	    ELSE
		IF (SW.EQ.1) THEN			! start value expected
		    IF (LW.EQ.0 .OR. WORK(:LW).EQ.BLANK) THEN
			SIE(1) = UNDEF_D
		    ELSE
			IS = PV_VAL_DECODE_N (WORK,STREAM,CHKSW,SWSYM,SIE(1))
			IF (IAND(IS,1).EQ.0) GOTO 995		! decode error
			SIE(2) = SIE(1)
			SIE(3) = 1
		    ENDIF
		ELSE					! end or increment exp.
		    IS = PV_VAL_DECODE_N (WORK,STREAM,CHKSW,SWSYM,SIE(SW))
		    IF (IAND(IS,1).EQ.0) GOTO 995		! decode error
		    IF (SW.EQ.2) SIE(3) = 1
		ENDIF
C
C					If value defined:
C					- check/modify increment
C					- put (sub)value(s) in array
C
		IF (SIE(1).NE.UNDEF_D) THEN
		    IF (SIE(2).EQ.SIE(1)) THEN
CC930413		SIE(3) = SIE(1)
			SIE(3) = 1			! 930413
			IF (SIE(3).EQ.0) SIE(3) = 1
		    ELSE IF ((SIE(2)-SIE(1))*SIE(3).LT.0) THEN
			IF (.NOT.SWSYM) GOTO 996	! wrong increment sign
		    ENDIF
		    IF (NRVAL.LT.NRARR) THEN		! 930413 CVT BACK/FOR
C							! to check low int value
			IS = GEN_CVT_D_NR (DTYP2,SIE(3),ARRAY(1,NRVAL+1))
			IS = GEN_CVT_NR_D (DTYP2,ARRAY(1,NRVAL+1),SIE(3))
		    END IF
		    IF (SIE(3).EQ.0) THEN
			IF (.NOT.SWSYM) GOTO 997	! zero increment
		    ENDIF
		    DO I = 1,NRIE
			NRVAL = NRVAL+1
			IF (NRVAL.GT.NRARR) GOTO 998	! too many values
			SWDEF(NRVAL) = .TRUE.
			IS = GEN_CVT_D_NR (DTYP2,SIE(I),ARRAY(1,NRVAL))
			IF (IAND(IS,7).NE.1) THEN	! no success
				IF (.NOT.SWSYM) GOTO 999 ! conversion error
			ENDIF
			IF (DTYPE.EQ.'L') THEN
			    IS = GEN_CVT_NR_L (DTYP2,ARRAY(1,NRVAL))
			    IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		    ENDDO
C
C					If undefined value:
C					- only check the nr of values
C
		ELSE
		    DO I = 1,NRIE
			NRVAL = NRVAL+1
			IF (NRVAL.GT.NRARR) GOTO 998	! too many values
		    ENDDO
		ENDIF
C
C					Save pointers and switch
C
		PTR = PTR+1
		IF (PTR.LE.LSTR+1) THEN			! comma found: more
		    SW = 1				! expect start value
		    SAVPTR(1) = PTR			! its start position
		    WORK = BLANK
		    LW = 0				! clear work string
		ENDIF
	    ENDIF
	ENDDO
C
C
	PV_SET_DECODE_N = DWC_SUCCESS
	ERRPTR = 0
	RETURN
 990	PV_SET_DECODE_N = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(WORK))
	ERRPTR = 0			! work string too short
	RETURN
 991	PV_SET_DECODE_N = MSG_SET (DWC_UNBPAREN,0)
	ERRPTR = PTR			! position of offending/missing ')'
	RETURN
 992	PV_SET_DECODE_N = MSG_SET (DWC_TSNOTALL,0)
	ERRPTR = PTR			! position of offending ' TO ' or ' BY '
	RETURN
 993	PV_SET_DECODE_N = MSG_SET (DWC_TONOTALL,0)
	ERRPTR = PTR			! position of offending ' TO '
	RETURN
 994	PV_SET_DECODE_N = MSG_SET (DWC_STEPNOTAL,0)
	ERRPTR = PTR			! position of offending ' BY '
	RETURN
 995	PV_SET_DECODE_N = IS		! error in PV_VAL_DECODE_N
	ERRPTR = SAVPTR(SW)		! position of offending value
	RETURN
 996	PV_SET_DECODE_N = MSG_SET (DWC_STEPSIGN,0)
	ERRPTR = SAVPTR(3)		! position of offending increment value
	RETURN
 997	PV_SET_DECODE_N = MSG_SET (DWC_STEPISZER,0)
	ERRPTR = SAVPTR(3)		! position of offending increment value
	RETURN
 998	PV_SET_DECODE_N = MSG_SET (DWC_TOOMANYEL,0,NRARR/NRIE)
	ERRPTR = SAVPTR(I)		! position of offending value
	RETURN
 999	IF (IAND(IS,1).EQ.0) THEN
		PV_SET_DECODE_N = IS		! conversion error
	ELSE
		PV_SET_DECODE_N = MSG_SET (DWC_INTOVERFL,1)
	CALL WNCTXT(DWLOG,DWMSG,DTYP2)
	ENDIF
	ERRPTR = SAVPTR(I)		! position of offending value
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_READ (DTYPE,LARR,NRARR,ARRAY,VALNR,
	1							COUNT,VALUE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D,L,C)
	INTEGER*4	LARR		! (i) length of value (in bytes)
	INTEGER*4	NRARR		! (i) nr of values in the set
	BYTE		ARRAY(LARR,*)	! (i) value set
	INTEGER*4	VALNR		! (m) sequence nr of value in set
	INTEGER*4	COUNT		! (m) counter for TO/BY arrays
	BYTE		VALUE(*)	! (o) next value
C
C.Purpose:	Get the next value from a value set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning		0	end of set reached
C	false status codes returned by referenced routines
C.Notes:
C	- NRARR > 0 required (no "null" or "wild" sets allowed).
C	- VALNR must be 0 for the first time; it will be reset to 0
C	  when the end of the set is reached.
C	- Logical values will be treated as integers (B,I,J depending on LARR).
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PV_SET_TOBY_C, PV_SET_TOBY_X
	INTEGER*4	PPD_AMAS_GET
	INTEGER*4	MOVE_BLB
C
	INTEGER*4	IS, MAXCOUNT
C
C
C
C					Character-type value set:
C					- return the next defined value
C
	IF (DTYPE.EQ.'C') THEN
		IF (VALNR.LT.0) VALNR = 0		! first time
 100		VALNR = VALNR+1				! next value
		IF (VALNR.GT.NRARR) GOTO 990		! end of set
		IF (ARRAY(1,VALNR).EQ.UNDEF_B) GOTO 100	! undefined, try next
		IS = MOVE_BLB (ARRAY(1,VALNR),VALUE,LARR)
C
C					Logical value or numerical vector
C					- return the first or next value
C
	ELSE IF (DTYPE.EQ.'L'
	1 .OR. IAND(PPD_AMAS_GET('VECTOR'),1) .NE. 0) THEN
		IF (VALNR.LT.0) VALNR = 0		! first time
		VALNR = VALNR+1				! next value
		IF (VALNR.GT.NRARR) GOTO 990		! end of set
		IS = MOVE_BLB (ARRAY(1,VALNR),VALUE,LARR)
C
C					Numerical scalar or array
C					- start with the first TOBY set
C					  or continue in current one
C
	ELSE
		IF (VALNR.LE.0) THEN			! first time:
			VALNR = 1			! first TOBY set
			COUNT = 0			! first value
		ELSE
			COUNT = COUNT+1			! next value
		ENDIF
C
C					- count the values in the TOBY set
C					- if the TOBY set is undefined or
C					  exhausted, continue with the next one
C
		IS = PV_SET_TOBY_C (DTYPE,LARR,ARRAY(1,VALNR),MAXCOUNT)
		DO WHILE (COUNT.GT.MAXCOUNT)		! TOBY set exhausted:
			VALNR = VALNR+3			! next TOBY set
			IF (VALNR.GT.NRARR) GOTO 990	! end of set
			COUNT = 0			! first value
			IS = PV_SET_TOBY_C (DTYPE,LARR,ARRAY(1,VALNR),MAXCOUNT)
		ENDDO
C
C					- extract the value from the TOBY set
C
		IS = PV_SET_TOBY_X (DTYPE,LARR,ARRAY(1,VALNR),COUNT,VALUE)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
	PV_SET_READ = DWC_SUCCESS
	RETURN
 990	PV_SET_READ = 0
	VALNR = 0
	RETURN
 999	PV_SET_READ = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_TOBY_C (DTYPE,LARR,ARRAY,MAXCOUNT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D)
	INTEGER*4	LARR		! (i) length of value (in bytes)
	BYTE		ARRAY(*)	! (i) TOBY set
	INTEGER*4	MAXCOUNT	! (o) maximum of TOBY counter
C
C.Purpose:	Count the values in a TOBY set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CALINVTYP	invalid data type
C	false status code returned by referenced routine
C.Notes:
C	- MAXCOUNT+1 is the nr of values defined.
C	- The set is undefined (MAXCOUNT = -1) if the start value is undefined.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	BYTE		BVAL(8), BSET(24)
	INTEGER*2	IVAL, ISET(3)
	INTEGER*4	JVAL, JSET(3), IS
	REAL*4		RVAL, RSET(3)
	REAL*8		DVAL, DSET(3)
		EQUIVALENCE (BVAL,IVAL,JVAL,RVAL,DVAL)
		EQUIVALENCE (BSET,ISET,JSET,RSET,DSET)
C
C
	MAXCOUNT = -1					! assume no values
	IS = MOVE_BLB (ARRAY,BSET,3*LARR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (DTYPE.EQ.'B') THEN
		IF (BSET(1).NE.UNDEF_B) MAXCOUNT = (BSET(2)-BSET(1))/BSET(3)
	ELSE IF (DTYPE.EQ.'I') THEN
		IF (ISET(1).NE.UNDEF_I) MAXCOUNT = (ISET(2)-ISET(1))/ISET(3)
	ELSE IF (DTYPE.EQ.'J') THEN
		IF (JSET(1).NE.UNDEF_J) MAXCOUNT = (JSET(2)-JSET(1))/JSET(3)
	ELSE IF (DTYPE.EQ.'R') THEN
		IF (RSET(1).NE.UNDEF_R) MAXCOUNT =
	1				(RSET(2)-RSET(1)+.01*RSET(3))/RSET(3)
	ELSE IF (DTYPE.EQ.'D') THEN
		IF (DSET(1).NE.UNDEF_D) MAXCOUNT =
	1				(DSET(2)-DSET(1)+.01*DSET(3))/DSET(3)
	ELSE
		GOTO 992				! invalid data type
	ENDIF
C
C
	PV_SET_TOBY_C = DWC_SUCCESS
	RETURN
 992	PV_SET_TOBY_C = MSG_SET (DWC_CALINVTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DTYPE)
	RETURN
 999	PV_SET_TOBY_C = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_SET_TOBY_X (DTYPE,LARR,ARRAY,COUNT,VALUE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D)
	INTEGER*4	LARR		! (i) length of value (in bytes)
	BYTE		ARRAY(*)	! (i) TOBY set
	INTEGER*4	COUNT		! (i) TOBY counter for value
	BYTE		VALUE(*)	! (o) extracted value
C
C.Purpose:	Extract a value from the TOBY set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CALINVTYP	invalid data type
C	error		2	invalid COUNT
C	false status code returned by referenced routine
C.Notes:
C	- COUNT must be in the range [0,MAXCOUNT].
C	- MAXCOUNT+1 is the nr of values defined in the TOBY set.
C	- The set is undefined (MAXCOUNT = -1) if the start value is undefined.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MOVE_BLB, MSG_SET
C
	BYTE		BVAL(8), BSET(24)
	INTEGER*2	IVAL, ISET(3)
	INTEGER*4	JVAL, JSET(3), IS, MAXCOUNT
	REAL*4		RVAL, RSET(3)
	REAL*8		DVAL, DSET(3)
		EQUIVALENCE (BVAL,IVAL,JVAL,RVAL,DVAL)
		EQUIVALENCE (BSET,ISET,JSET,RSET,DSET)
C
C
	IF (COUNT.LT.0) GOTO 991			! invalid count
	IS = MOVE_BLB (ARRAY,BSET,3*LARR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (DTYPE.EQ.'B') THEN
		IF (BSET(1).NE.UNDEF_B) MAXCOUNT = (BSET(2)-BSET(1))/BSET(3)
		IF (COUNT.GT.MAXCOUNT) GOTO 991
		BVAL(1) = BSET(1)+COUNT*BSET(3)
	ELSE IF (DTYPE.EQ.'I') THEN
		IF (ISET(1).NE.UNDEF_I) MAXCOUNT = (ISET(2)-ISET(1))/ISET(3)
		IF (COUNT.GT.MAXCOUNT) GOTO 991
		IVAL = ISET(1)+COUNT*ISET(3)
	ELSE IF (DTYPE.EQ.'J') THEN
		IF (JSET(1).NE.UNDEF_J) MAXCOUNT = (JSET(2)-JSET(1))/JSET(3)
		IF (COUNT.GT.MAXCOUNT) GOTO 991
		JVAL = JSET(1)+COUNT*JSET(3)
	ELSE IF (DTYPE.EQ.'R') THEN
		IF (RSET(1).NE.UNDEF_R) MAXCOUNT =
	1				(RSET(2)-RSET(1)+.01*RSET(3))/RSET(3)
		IF (COUNT.GT.MAXCOUNT) GOTO 991
		RVAL = RSET(1)+COUNT*RSET(3)
	ELSE IF (DTYPE.EQ.'D') THEN
		IF (DSET(1).NE.UNDEF_D) MAXCOUNT =
	1				(DSET(2)-DSET(1)+.01*DSET(3))/DSET(3)
		IF (COUNT.GT.MAXCOUNT) GOTO 991
		DVAL = DSET(1)+COUNT*DSET(3)
	ELSE
		GOTO 992				! invalid data type
	ENDIF
C
	IS = MOVE_BLB (BVAL,VALUE,LARR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PV_SET_TOBY_X = DWC_SUCCESS
	RETURN
 991	PV_SET_TOBY_X = 2
	CALL WNCTXT(DWLOG,'Invalid TOBY count !SJ',COUNT)
	RETURN
 992	PV_SET_TOBY_X = MSG_SET (DWC_CALINVTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DTYPE)
	RETURN
 999	PV_SET_TOBY_X = IS
	RETURN
	END
