C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_FAO
C.Keywords:	PPD File, Formatted ASCII Output
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	940119 CMV - use wnctxs i.s.o. STR_FAO
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_FAO (ARRAY,LARR,DTYPE,PLEN,STRING,LSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (i) value array
	INTEGER*4	LARR		! (i) nr of bytes in the array
	CHARACTER*1	DTYPE		! (i) datatype (B,I,J,R,D,C)
	INTEGER*4	PLEN		! (i) nr of bytes per value
	CHARACTER*(*)	STRING		! (o) output string
	INTEGER*4	LSTR		! (o) its significant length
C
C.Purpose:	Encode an array of values into a string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_TYPEINV	invalid data type
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	In the output string the values are separated by a comma and a blank.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	NTYPES
	CHARACTER*(*)	BLANK, COMMA, TYPES
		PARAMETER (BLANK  = ' ')
		PARAMETER (COMMA  = ',')
		PARAMETER (NTYPES = 6  )
		PARAMETER (TYPES  = 'BIJRDC')
	CHARACTER*5	FMT(NTYPES)
		DATA FMT /'!#SB','!#SI','!#SJ','!#E7','!#D16',' '/
C
	INTEGER*4	MOVE_BLB, MSG_SET  
	INTEGER		WNCALN
C
	INTEGER*4	IS, PTR, NVALS
C
C
	STRING = BLANK
	LSTR = 0
C
	PTR = INDEX (TYPES,DTYPE)
	IF (PTR.EQ.0) THEN
		IS = PPD_TYPEINV
		GOTO 999
	ENDIF
C
	IF (DTYPE.NE.'C') THEN
		NVALS = LARR/PLEN
		CALL WNCTXS(STRING,FMT(PTR),NVALS,ARRAY)
		LSTR=WNCALN(STRING)
	ELSE
		PTR = 1
		DO WHILE (PTR.LE.LARR)
			IF (LSTR+PLEN+2.GT.LEN(STRING)) THEN
				IS = PPD_STRTOOSML
				GOTO 999
			ENDIF
			IS = MOVE_BLB (ARRAY(PTR),%REF(STRING(LSTR+1:)),PLEN)
			LSTR = WNCALN(STRING(:LSTR+PLEN))+2
			STRING (LSTR-1:LSTR) = COMMA//BLANK
			PTR = PTR+PLEN
		ENDDO
		LSTR = LSTR-2
	ENDIF
C
	PPD_FAO = PPD_SUCCESS
	RETURN
C
 999	PPD_FAO = MSG_SET (IS,0)
	RETURN
	END
