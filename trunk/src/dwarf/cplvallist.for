C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_VALLIST
C.Keywords:	Compiler Utility, Value List
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C		941212 JPH - COMMA --> DELIM
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_VALLIST (LIST,TYPE,LVAL,ARRAY,LARR,NRVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LIST		! (i) value list to be decoded
	CHARACTER*(*)	TYPE		! (i) data-type code
	INTEGER*4	LVAL		! (i) length of a decoded single value
	BYTE		ARRAY(*)	! (o) array with decoded values
	INTEGER*4	LARR		! (i) max length of array (bytes)
	INTEGER*4	NRVAL		! (o) nr of values in the list
C
C.Purpose:	Convert a comma-separated list of values to a value array
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	error	CPL_VALLISINV	Error report left in message buffer
C.Notes:
C	- Leading and trailing blanks and tabs in list elements are ignored.
C	- Valid data-type codes are B (byte), I (integer*2), J (integer*4),
C	  R (real*4), D (real*8), L (logical) and C (character).
C	- LVAL gives the maximum length for C values, the length for L values,
C	  and is ignored for the other types (B,I,J,R,D).
C	- Valid L values are 'FALSE', 'TRUE', 'NO' ,'YES' or abbreviations.
C	- Empty elements (e.g. ',,') are converted to undef_ values, but empty
C	  L values are converted to .FALSE.
C	- In case of errors a zero-filled array will be returned (NRVAL=0).
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, COMMA, LOGLIST, TYPLIST, DELIM
		PARAMETER (BLANK   = ' ')
		PARAMETER (TAB     = '	')
		PARAMETER (COMMA   = ',')
		PARAMETER (LOGLIST = 'TRUE,FALSE,YES,NO')
		PARAMETER (TYPLIST = 'BIJRDLC')
		PARAMETER (DELIM = ', ;|[]:' )
C
	INTEGER*4	STR_SIGLEN, STR_MATCH_L, STR_COPY_U, STR_SKIP_W
	INTEGER*4	STR_READ_B, STR_READ_I, STR_READ_J
	INTEGER*4	STR_READ_R, STR_READ_D
	INTEGER*4	CLEAR_BLB, MOVE_BLB
	INTEGER*4	MSG_SET
C
	CHARACTER*80	VALUE
	INTEGER*4	LL, LV
	CHARACTER*80	C
	BYTE		B(80)
	INTEGER*2	II
	REAL*4		R
	REAL*8		D
	LOGICAL*4	L
		EQUIVALENCE (C,B,II,J,R,D,L)
	INTEGER*4	IS, PTR, MATCH, ARRPTR
C
C
C					Initialize
C
	IF (INDEX(TYPLIST,TYPE).EQ.0) GOTO 991
	NRVAL = 0
	IS = CLEAR_BLB (ARRAY,LARR)
	ARRPTR = 0
	LL = STR_SIGLEN (LIST)
C
C					Isolate next value
C					- LL+1 to take care of trailing comma
C
	PTR = 1
	DO WHILE (PTR.LE.LL+1)
		LV = 0
		IS = STR_SKIP_W (BLANK//TAB,LIST(:LL),PTR)
		IS = STR_COPY_U (DELIM,LIST,PTR,VALUE,LV)
!!		IS = STR_COPY_U (COMMA,LIST,PTR,VALUE,LV)
		IF (LV.GT.0) LV = STR_SIGLEN (VALUE(:LV))
C
C					Decode/check the value
C
		IF (LV.GT.0) THEN
			IF (TYPE.EQ.'B') THEN
				IS = STR_READ_B (VALUE(:LV),B(1))
			ELSE IF (TYPE.EQ.'I') THEN
				IS = STR_READ_I (VALUE(:LV),II)
			ELSE IF (TYPE.EQ.'J') THEN
				IS = STR_READ_J (VALUE(:LV),J)
			ELSE IF (TYPE.EQ.'R') THEN
				IS = STR_READ_R (VALUE(:LV),R)
			ELSE IF (TYPE.EQ.'D') THEN
				IS = STR_READ_D (VALUE(:LV),D)
			ELSE IF (TYPE.EQ.'L') THEN
				IS = STR_MATCH_L (VALUE(:LV),LOGLIST,MATCH)
				IF (IAND(IS,1).NE.0) 
	1				L = MATCH.EQ.1 .OR. MATCH.EQ.3
			ELSE
				IS = LV.LE.LVAL
				IF (IAND(IS,1).NE.0) C = VALUE(:LV)
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Undefined value
C
		ELSE
			IF (TYPE.EQ.'B') THEN
				B(1) = UNDEF_B
			ELSE IF (TYPE.EQ.'I') THEN
				II = UNDEF_I
			ELSE IF (TYPE.EQ.'J') THEN
				J = UNDEF_J
			ELSE IF (TYPE.EQ.'R') THEN
				R = UNDEF_R
			ELSE IF (TYPE.EQ.'D') THEN
				D = UNDEF_D
			ELSE IF (TYPE.EQ.'L') THEN
				L = .FALSE.
			ELSE
				C = UNDEF_C
			ENDIF
		ENDIF
		IF (ARRPTR+LVAL.GT.LARR) GOTO 992
		IS = MOVE_BLB (B,ARRAY(ARRPTR+1),LVAL)
		ARRPTR = ARRPTR+LVAL
		NRVAL = NRVAL+1
		PTR = PTR+1
	ENDDO
C
C					Return
C
	CPL_VALLIST = CPL_SUCCESS
	RETURN
C
 991	IS = MSG_SET (CPL_DATTYPINV,1)
	CALL WNCTXT(DWLOG,DWMSG,TYPE)
	GOTO 999
 992	IS = MSG_SET (CPL_ARROVRFLO,0)
 999	NRVAL = 0
	IS = CLEAR_BLB (ARRAY,LARR)
	CPL_VALLIST = MSG_SET (CPL_VALLISINV,1)
	CALL WNCTXT(DWLOG,DWMSG,LIST)
	RETURN
	END
