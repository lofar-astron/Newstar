C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PV_VAL
C.Keywords:	Parameter Values, Single Value
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900420 FMO - recreation
C.Version:	910730 FMO - corrected STR_FAO arg in ENCODE_N
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940119 CMV - use WNCTXS i.s.o. STR_FAO
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_VAL ()
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
	PV_VAL = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_VAL_ENCODE (DTYPE,LVAL,VALUE,STRING,LSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type code (B,I,J,R,D,L,C)
	INTEGER*4	LVAL		! (i) length of value (in bytes)
	BYTE		VALUE(*)	! (i) value
	CHARACTER*(*)	STRING		! (o) value string
	INTEGER*4	LSTR		! (o) significant length of STRING
C
C.Purpose:	Convert a single parameter value to a standard value string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	string is truncated
C	error	DWC_CALINVTYP	invalid data type
C	error	2		encoding error in STR_FAO (-> asterisks)
C	false code returned by referenced routine
C.Notes:
C	Logical (data type L):
C	- The output string will be 'YES' or 'NO'.
C	Character (data type C):
C	- An undefined value (starting with UNDEF_C) will be converted to an
C	  empty string (LSTR = 0).
C	- If the value is not extended alphanumeric (uppercase characters,
C	  dollar and underscore), the string will be enclosed in quotes.
C	- Embedded quotes will be converted to double quotes.
C	- A white value (only blanks and tabs) will be converted to a single
C	  quoted blank.
C	Numerical (data types B,I,J,R,D):
C	- An undefined value (UNDEF_<type>) will be converted to an empty
C	  string (LSTR = 0).
C	- Leading blanks, non-significant zero's and a trailing decimal point
C	  are removed from the output string.
C	- If the PPD file gives a unit for the parameter, it will be appended
C	  to the output string, preceeded by a blank.
C	- For unit codes DMS and HMS the number is converted to the format
C	  DD:MM:SS.SS or HH:MM:SS.SS, where SS is rounded to 3 or 6 decimals
C	  for single or double precision reals.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PV_VAL_ENCODE_C, PV_VAL_ENCODE_N
	INTEGER*4	PPD_USTR_GET
	INTEGER*4	MOVE_BLB
C
	CHARACTER	WORK*255
	INTEGER*4	IS, LW
C
C
	IF (DTYPE.EQ.'C') THEN
		IS = MOVE_BLB (VALUE,%REF(WORK),LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS = PV_VAL_ENCODE_C (WORK(:LVAL),STRING,LSTR)
	ELSE
		IS = PPD_USTR_GET (WORK,LW)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS = PV_VAL_ENCODE_N (DTYPE,LVAL,VALUE,WORK(:LW),STRING,LSTR)
	ENDIF
C
	PV_VAL_ENCODE = DWC_SUCCESS
	RETURN
 999	PV_VAL_ENCODE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_VAL_ENCODE_C (VALUE,STRING,LSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	VALUE		! (i) value
	CHARACTER*(*)	STRING		! (o) value string
	INTEGER*4	LSTR		! (o) significant length of STRING
C
C.Purpose:	Convert a character-type value to a standard value string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	string is truncated
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, QUOTE, QUOTE2, BLANKVAL
		PARAMETER (BLANK    = ' ' )
		PARAMETER (QUOTE    = '"' )
		PARAMETER (QUOTE2   = QUOTE//QUOTE)
		PARAMETER (BLANKVAL = QUOTE//BLANK//QUOTE)
C
	INTEGER*4	STR_COPY, STR_COPY_U, STR_CHECK_ANUMX
	INTEGER*4	MSG_SET  
	INTEGER		WNCAL0
C
	INTEGER*4	LVAL, NCOPY, PTR
C
C
	STRING = BLANK
	LSTR = 0
	LVAL = WNCAL0(VALUE)
C
C					Blank value -> quoted blank
C
	IF (LVAL.EQ.0) THEN
		NCOPY = STR_COPY (BLANKVAL,STRING,LSTR)
C
C					Undefined value -> empty string
C
	ELSE IF (VALUE(1:1).EQ.UNDEF_C) THEN
		NCOPY = 0
C
C					Extended alpha-numeric -> straight copy
C
	ELSE IF (IAND(STR_CHECK_ANUMX(VALUE(:LVAL)),1) .NE. 0) THEN
		NCOPY = STR_COPY (VALUE(:LVAL),STRING,LSTR)
C
C					Otherwise:
C					- enclose in quotes
C					- double embedded quotes
C
	ELSE
		PTR = 1
		NCOPY = STR_COPY (QUOTE,STRING,LSTR)
		DO WHILE (PTR.LE.LVAL)
			NCOPY = STR_COPY_U (QUOTE,VALUE(:LVAL),PTR,STRING,LSTR)
			IF (PTR.LE.LVAL) THEN
				NCOPY = STR_COPY (QUOTE2,STRING,LSTR)
				PTR = PTR+1
			ENDIF
		ENDDO
		NCOPY = STR_COPY (QUOTE,STRING,LSTR)
	ENDIF
	IF (NCOPY.LT.0) GOTO 999				! truncated
C
	PV_VAL_ENCODE_C = DWC_SUCCESS
	RETURN
 999	PV_VAL_ENCODE_C = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STRING))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_VAL_ENCODE_N (DTYPE,LVAL,VALUE,UNIT,
	1						STRING,LSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type (B,I,J,R,D,L)
	INTEGER*4	LVAL		! (i) length of value (in bytes)
	BYTE		VALUE(*)	! (i) value
	CHARACTER*(*)	UNIT		! (i) unit code
	CHARACTER*(*)	STRING		! (o) value string
	INTEGER*4	LSTR		! (o) significant length of STRING
C
C.Purpose:	Convert a numerical-type value to a standard value string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_CALINVTYP	invalid data type
C	error	2		encoding error in STR_FAO (-> asterisks)
C	false code returned by referenced routine
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, ASTER
		PARAMETER (BLANK = ' ')
		PARAMETER (ASTER = '*')
C
	INTEGER		WNCAL0
C
	INTEGER*4	MOVE_BLB, MSG_SET
C
	BYTE		B(8)
	INTEGER*2	II
	INTEGER*4	LU, IS, DD, MM
	REAL*4		R
	REAL*8		D, SS
	LOGICAL*1	LL1
	LOGICAL*2	L2
	LOGICAL*4	L4
		EQUIVALENCE (B,II,J,R,D,LL1,L2,L4)
	LOGICAL*4	LSW, IS_DMS
C
C
	STRING = BLANK
	LSTR = 0
	LU = LEN (UNIT)
	IS_DMS = UNIT.EQ.'DMS' .OR. UNIT.EQ.'HMS'
	IS = MOVE_BLB (VALUE,B,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (DTYPE.EQ.'L') THEN
	    IF (LVAL.EQ.1) THEN
		LSW = LL1
	    ELSE IF (LVAL.EQ.2) THEN
		LSW = L2
	    ELSE
		LSW = L4
	    ENDIF
		CALL WNCTXS(STRING,'!L',LSW)
C
	ELSE IF (DTYPE.EQ.'B') THEN
		IF (B(1).EQ.UNDEF_B) THEN
		ELSE IF (IS_DMS) THEN
			CALL WNCTXS(STRING,'!SB:0:0 !AS',B(1),UNIT)
		ELSE IF (LU.GT.0) THEN
			CALL WNCTXS(STRING,'!SB !AS',,B(1),UNIT)
		ELSE
			CALL WNCTXS(STRING,'!SB',B(1))
		ENDIF
C
	ELSE IF (DTYPE.EQ.'I') THEN
		IF (II.EQ.UNDEF_I) THEN
		ELSE IF (IS_DMS) THEN
			CALL WNCTXS(STRING,'!SI:0:0 !AS',II,UNIT)
		ELSE IF (LU.GT.0) THEN
			CALL WNCTXS(STRING,'!SI !AS',II,UNIT)
		ELSE
			CALL WNCTXS(STRING,'!SI',II)
		ENDIF
C
	ELSE IF (DTYPE.EQ.'J') THEN
		IF (J.EQ.UNDEF_J) THEN
		ELSE IF (IS_DMS) THEN
			CALL WNCTXS(STRING,'!SJ:0:0 !AS',J,UNIT)
		ELSE IF (LU.GT.0) THEN
			CALL WNCTXS(STRING,'!SJ !AS',J,UNIT)
		ELSE
			CALL WNCTXS(STRING,'!SJ',J)
		ENDIF
C
	ELSE IF (DTYPE.EQ.'R') THEN
		IF (R.EQ.UNDEF_R) THEN
		ELSE IF (IS_DMS) THEN
			DD = INT(R)			! signed DD or HH
			R = ABS(R-DD)*60		! decimal minutes
			MM = INT(R)			! unsigned MM
			SS = ABS(R-MM)*60		! decimal seconds
			CALL WNCTXS(STRING,'!SJ:!SJ:!D.3 !AS',
	1				DD,MM,SS,UNIT)
		ELSE IF (LU.GT.0) THEN
			CALL WNCTXS(STRING,'!E !AS',R,UNIT)
		ELSE
			CALL WNCTXS(STRING,'!E',R)
		ENDIF
C
	ELSE IF (DTYPE.EQ.'D') THEN
		IF (D.EQ.UNDEF_D) THEN
		ELSE IF (IS_DMS) THEN
			DD = INT(D)			! signed DD or HH
			D = ABS(D-DD)*60		! decimal minutes
			MM = INT(D)			! unsigned MM
			SS = ABS(D-MM)*60		! decimal seconds
			CALL WNCTXS(STRING,'!SJ:!SJ:!D.6 !AS',
	1				DD,MM,SS,UNIT)
		ELSE IF (LU.GT.0) THEN
			CALL WNCTXS(STRING,'!D !AS',D,UNIT)
		ELSE
			CALL WNCTXS(STRING,'!D',D)
		ENDIF
	ELSE
		GOTO 991				! invalid data type
	ENDIF
	LSTR=WNCAL0(STRING)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (STRING(1:1).EQ.ASTER) GOTO 992
C
	PV_VAL_ENCODE_N = DWC_SUCCESS
	RETURN
 991	PV_VAL_ENCODE_N = MSG_SET (DWC_CALINVTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DTYPE)
	RETURN
 992	PV_VAL_ENCODE_N = 2
	CALL WNCTXT(DWLOG,'Encoding error in STR_FAO')
	RETURN
 999	PV_VAL_ENCODE_N = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_VAL_DECODE_N (STRING,STREAM,CHKSW,
	1						SWSYM,VALUE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) expression
	CHARACTER*(*)	STREAM		! (i) stream name (symbol substitution)
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
	REAL*8		VALUE		! (o) value
C
C.Purpose:	Evaluate an expression to a REAL*8 value
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_EXPERRMSG	evaluation error
C	false status codes returned by referenced routines
C.Notes:
C	- STRING will be modified if symbols are present.
C	- If a unit is defined in the PPD file, the result of the evaluation
C	  will be expressed in that unit. Otherwise degrees are used for
C	  trigonometric functions.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, VALDELIM, DEFCODE
		PARAMETER (BLANK    = ' ')
		PARAMETER (VALDELIM = ',')
		PARAMETER (DEFCODE  = 'DEG')
C
	INTEGER*4	PPD_USTR_GET
	INTEGER*4	DWC_EXPR_SOLVE
	INTEGER*4	READ_UNIT, READ_UNITG, MSG_SET  
C
	INTEGER		WNCAL0
C
	CHARACTER	UNITLIST*100, DEFUNIT*10, GROUP*10
	INTEGER*4	IS, LU, ERRPTR
	REAL*8		FACTOR
C
C
C					Build the list of possible units:
C					- if a unit is given in the PPD file,
C					  start with that unit code and
C					  append all units in the same group
C					- otherwise, only 'DEG'
C
	IS = PPD_USTR_GET (DEFUNIT,LU)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LU.EQ.0 .OR. DEFUNIT(:LU).EQ.'1') THEN
		DEFUNIT = DEFCODE
		UNITLIST = DEFCODE
		IS = READ_UNIT (DEFUNIT,GROUP,FACTOR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ELSE
		IS = READ_UNIT (DEFUNIT(:LU),GROUP,FACTOR)
		IF (IAND(IS,1).EQ.0) GOTO 999
		UNITLIST = DEFUNIT(:LU)//VALDELIM
		IS = READ_UNITG (GROUP,UNITLIST(LU+2:))
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
	LU = WNCAL0(UNITLIST)
C
C					Evaluate the expression
C
	IS = DWC_EXPR_SOLVE (STRING,STREAM,FACTOR,UNITLIST(:LU),VALUE,
	1						ERRPTR,CHKSW,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
	PV_VAL_DECODE_N = DWC_SUCCESS
	RETURN
 991	PV_VAL_DECODE_N = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK,ERRPTR,STRING)
	RETURN
 999	PV_VAL_DECODE_N = IS
	RETURN
	END
