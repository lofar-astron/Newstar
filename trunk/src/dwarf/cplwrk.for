C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_WRK
C.Keywords:	Compiler Utility, Work Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Parameters:
C	INTEGER*4	CPL__WRKLMAX	! size of value buffer (bytes)
C	INTEGER*4	CPL__WRKNMAX	! max nr of fields
C		Common variables used:
C	CHARACTER*(*)	CPL$WRKBUF	! (m) buffer with field values
C	INTEGER*4	CPL$WRKLBUF	! (m) signif length of buffer
C	INTEGER*4	CPL$WRKSTART(i)	! (m) start of value i in buffer
C	INTEGER*4	CPL$WRKEND(i)	! (m) end of value i in buffer
C	INTEGER*4	CPL$WRKLNR(i)	! (m) source-line nr of field i
C	INTEGER*4	CPL$WRKLNR(0)	! (m) nr of last source-line read
C
C.Version:	900415 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C--------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_WRK_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
C.Purpose:	Initialize the work buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C.Notes:
C--------------------------------------------------------------------------
C
C
C
C
	CPL$WRKLBUF = 0
	DO I = 1,CPL__WRKNMAX
		CPL$WRKSTART(I) = UNDEF_J
		CPL$WRKEND(I) = UNDEF_J
		CPL$WRKLNR(I) = UNDEF_J
	ENDDO
C
	CPL_WRK_INIT = CPL_SUCCESS
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_WRK_PUTLNR (FIELDNR,LINENR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	FIELDNR		! (i) field nr
	INTEGER*4	LINENR		! (i) source-line nr
C
C.Purpose:	Store the source-line number for a field in the work buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_FLDNRINV	Field nr out of range
C.Notes:
C	CPL$WRKLNR(0) is used for the nr of the last source-line of a group
C--------------------------------------------------------------------------
C
	INTEGER		MSG_SET
C
C
	IF (FIELDNR.LT.0 .OR. FIELDNR.GT.CPL__WRKNMAX) GOTO 999
	CPL$WRKLNR(FIELDNR) = LINENR
C
	CPL_WRK_PUTLNR = CPL_SUCCESS
	RETURN
C
 999	CPL_WRK_PUTLNR = MSG_SET (CPL_FLDNRINV,1)
	CALL WNCTXT(DWLOG,DWMSG,FIELDNR)
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_WRK_PUTVAL (FIELDNR,VALUE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	FIELDNR		! (i) field nr
	CHARACTER*(*)	VALUE		! (i) value
C
C.Purpose:	Put the field value (character format) in the work buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_FLDNRINV	Field nr out of range
C	fatal	CPL_WRKFUL	Buffer overflow
C.Notes:
C	Trailing blanks and tabs in the value are ignored. If the value is
C	empty, nothing will be stored and the success status is returned.
C--------------------------------------------------------------------------
C
	INTEGER*4	STR_SIGLEN, STR_COPY, MSG_SET
C
	INTEGER*4	IS, LVAL
C
C
	IF (FIELDNR.LE.0 .OR. FIELDNR.GT.CPL__WRKNMAX) GOTO 999
C
	LVAL = STR_SIGLEN (VALUE)
	IF (LVAL.GT.0) THEN
		CPL$WRKSTART(FIELDNR) = CPL$WRKLBUF+1
		IS = STR_COPY (VALUE(:LVAL),CPL$WRKBUF,CPL$WRKLBUF)
		CPL$WRKEND(FIELDNR) = CPL$WRKLBUF
		IF (IS.LT.0) GOTO 998
	ENDIF
C
	CPL_WRK_PUTVAL = CPL_SUCCESS
	RETURN
C
 998	CPL_WRK_PUTVAL = MSG_SET (CPL_WRKFUL,0)
	RETURN
C
 999	CPL_WRK_PUTVAL = MSG_SET (CPL_FLDNRINV,1)
	CALL WNCTXT(DWLOG,DWMSG,FIELDNR)
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_WRK_GET (FIELDNR,LINENR,VALUE,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'CPL_2_DEF'
C
	INTEGER*4	FIELDNR		! (i) field nr
	INTEGER*4	LINENR		! (o) source-line nr of the field
	CHARACTER*(*)	VALUE		! (o) value of the field
	INTEGER*4	LVAL		! (o) its significant length
C
C.Purpose:	Get line nr and field value (char format) from the work buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_FLDNRINV	Field nr out of range
C	error	CPL_STROVRFLO	Output string too short
C.Notes:
C	- If no line nr was stored for the field, the nr of the last source-
C	  line read will be returned.
C	- If no field value was stored, LVAL = 0 will be returned.
C	- If VALUE is longer than the value stored, it will be padded with
C	  blanks; if it is shorter, the value will be truncated.
C--------------------------------------------------------------------------
C
	INTEGER*4	STR_COPY, MSG_SET
C
	INTEGER*4	IS, WST, WED
C
C
	LINENR = 0
	VALUE = ' '
	LVAL = 0
	IF (FIELDNR.LT.0 .OR. FIELDNR.GT.CPL__WRKNMAX) GOTO 999
C
	LINENR = CPL$WRKLNR(FIELDNR)
	IF (FIELDNR.GT.0) THEN
		IF (LINENR.EQ.UNDEF_J) LINENR = CPL$WRKLNR(0)
		WST = CPL$WRKSTART(FIELDNR)
		IF (WST.NE.UNDEF_J) THEN
			WED = CPL$WRKEND(FIELDNR)
			IS = STR_COPY (CPL$WRKBUF(WST:WED),VALUE,LVAL)
			IF (IS.LT.0) GOTO 998
		ENDIF
	ENDIF
C
	CPL_WRK_GET = CPL_SUCCESS
	RETURN
C
 998	CPL_WRK_GET = MSG_SET (CPL_STROVRFLO,0)
	RETURN
C
 999	CPL_WRK_GET = MSG_SET (CPL_FLDNRINV,1)
	CALL WNCTXT(DWLOG,DWMSG,FIELDNR)
	RETURN
	END
