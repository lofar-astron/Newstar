C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_MIN
C.Keywords:	PPD File, Parameter Minimum/Maximum Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$MNOFF	! (m) offset of minimum values in descr
C	INTEGER*4	PPDPD$MNLEN	! (m) length of minimum values
C	INTEGER*4	PPDPD$MXOFF	! (m) offset of maximum values in descr
C	INTEGER*4	PPDPD$MXLEN	! (m) length of maximum values
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MIN_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed minimum values
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the minimum values for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_MMNOVAL	min-check requested, but no values
C	error	PPD_MMNOCHK	values given, but no min-check requested
C	error	PPD_MMINV	invalid value found
C	error	PPD_VCINVNVL	vector: invalid nr of values
C	error	PPD_NVCINVNVL	scalar or array: only 1 value allowed
C.Notes:
C	- The minimum values are stored in the variable-length part of the
C	  current parameter description. Its offset w.r.t. the start of the
C	  description and its significant length are stored in the fixed part
C	  (fields PPDPD$MNOFF and PPDPD$MNLEN).
C	- If no minimum check is requested, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	LBUF
	CHARACTER*(*)	EMPTVAL
		PARAMETER (LBUF    = 256 )
		PARAMETER (EMPTVAL = '[]')
C
	INTEGER*4	PPD_DTYPE_GET, PPD_DTYPE_XGET
	INTEGER*4	PPD_NVAL_GET, PPD_NVAL_XGET
	INTEGER*4	PPD_CMAS_GET, PPD_AMAS_GET, CPL_VALLIST
	INTEGER*4	STR_SIGLEN, MOVE_BLB
C
	BYTE		BUF(LBUF)
	CHARACTER*1	DTYPE, XDTYPE
	INTEGER*4	PLEN, XPLEN, NVAL, XNVAL, MNVAL, MXVAL
	INTEGER*4	IS, LSTR, LARR, COUNT
C
C
	PPDPD$MNOFF = UNDEF_J
	PPDPD$MNLEN = 0
	LSTR = STR_SIGLEN (STRING)
C
	IF (IAND(PPD_CMAS_GET('MINIMUM'),1) .NE. 0) THEN
C
C					If input as a character-string list:
C					- minimum values must be given
C					- decode the list into an array
C					- check the nr of values
C					- store the array in the description
C
		IF (DO_CHECK) THEN
			IF (LSTR.EQ.0 .OR. STRING(:LSTR).EQ.EMPTVAL) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			IS = PPD_DTYPE_GET (DTYPE,PLEN)
			IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
			IS = CPL_VALLIST (STRING(:LSTR),DTYPE,PLEN,
	1							BUF,LBUF,COUNT)
			IF (IAND(IS,1).EQ.0) THEN
				IS = PPD_MMINV
				GOTO 999
			ENDIF
C
			IF (IAND(PPD_AMAS_GET('VECTOR'),1) .NE. 0) THEN
				IF (COUNT.NE.NVAL) IS = PPD_VCINVNVL
			ELSE
				IF (COUNT.NE.1) IS = PPD_NVCINVNVL
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
C
			PPDPD$MNOFF = PPDPD$LENG
			PPDPD$MNLEN = PLEN*COUNT
			PPDPD$LENG = PPDPD$LENG+PPDPD$MNLEN
			IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
			IS = MOVE_BLB (BUF,PPDPD_(PPDPD$MNOFF+1),PLEN*COUNT)
C
C					If input as an array:
C					- array cannot be empty
C					- check datatype, value length
C					  and nr of values
C					- store the array in the description
C
		ELSE
			LARR = LEN (STRING)
			IF (LARR.EQ.0) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			IS = PPD_DTYPE_GET (DTYPE,PLEN)
			IS = PPD_DTYPE_XGET (XDTYPE,XPLEN)
			IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
			IS = PPD_NVAL_XGET (XNVAL,MNVAL,MXVAL)
			IF (DTYPE.NE.XDTYPE .OR.
	1		    PLEN .NE.XPLEN  .OR.
	2		    NVAL .NE.XNVAL) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			PPDPD$MNOFF = PPDPD$LENG
			PPDPD$MNLEN = LARR
			PPDPD$LENG = PPDPD$LENG+LARR
			IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
			IS = MOVE_BLB (%REF(STRING),PPDPD_(PPDPD$MNOFF+1),LARR)
		ENDIF
C
	ELSE IF (DO_CHECK) THEN
		IF (LSTR.GT.0. AND. STRING(:LSTR).NE.EMPTVAL) THEN
			IS = PPD_MMNOCHK
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_MIN_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_MIN_PUT = IS
	RETURN
C
 9999	PPD_MIN_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MIN_GET (ARRAY,MAXL,LARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (o) minimum values
	INTEGER*4	MAXL		! (i) size of array
	INTEGER*4	LARR		! (o) significant size of array
C
C.Purpose:	Get the minimum values from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output array too small
C.Notes:
C	- If no minimum values are given, LARR = 0 will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS
C
C
C					Get the array
C
	LARR = 0
	IF (PPDPD$MNOFF.NE.UNDEF_J) THEN
		IF (PPDPD$MNLEN.GT.MAXL) GOTO 999
		LARR = PPDPD$MNLEN
		IS = MOVE_BLB (PPDPD_(PPDPD$MNOFF+1),ARRAY,LARR)
	ENDIF
C
	PPD_MIN_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_MIN_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MIN_XGET (ARRAY,MAXL,LARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (o) minimum values
	INTEGER*4	MAXL		! (i) size of array
	INTEGER*4	LARR		! (o) nr of bytes in array
C
C.Purpose:	Get the minimum values for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C.Notes:
C	- The values are fetched directly from the mapped PPD file using
C	  the offset and length given in the current parameter description.
C	- Use XGET i.s.o. GET when the variable-length part of the current
C	  description contains data for another parameter (e.g. the COPY
C	  parameter in BLDPPD).
C	- If no minimum values are given, LARR = 0 will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	LARR = 0
C
C					Make sure that the PPD file is mapped
C					and that a parameter has been selected
C
	IS = PPD_STAT_INQ (MAPB,ADDR,HLPB)
	IF (MAPB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ELSE IF (ADDR.EQ.0) THEN
		IS = PPD_NOCURENTR
		GOTO 999
	ENDIF
C
C					Get the array
C
	IF (PPDPD$MNOFF.NE.UNDEF_J) THEN
		IF (PPDPD$MNLEN.LE.MAXL) THEN
			LARR = PPDPD$MNLEN
			ADDR = ADDR+PPDPD$MNOFF+1
			IS = MOVE_BLB (A_B(ADDR-A_OB),ARRAY,LARR)
		ELSE
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_MIN_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_MIN_XGET = MSG_SET (IS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MAX_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed maximum values
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the maximum values for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_MMNOVAL	max-check requested, but no values
C	error	PPD_MMNOCHK	values given, but no max-check requested
C	error	PPD_MMINV	invalid value found
C	error	PPD_VCINVNVL	vector: invalid nr of values
C	error	PPD_NVCINVNVL	scalar or array: only 1 value allowed
C.Notes:
C	- The maximum values are stored in the variable-length part of the
C	  current parameter description. Its offset w.r.t. the start of the
C	  description and its significant length are stored in the fixed part
C	  (fields PPDPD$MXOFF and PPDPD$MXLEN).
C	- If no maximum check is requested, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	LBUF
	CHARACTER*(*)	EMPTVAL
		PARAMETER (LBUF    = 256 )
		PARAMETER (EMPTVAL = '[]')
C
	INTEGER*4	PPD_DTYPE_GET, PPD_DTYPE_XGET
	INTEGER*4	PPD_NVAL_GET, PPD_NVAL_XGET
	INTEGER*4	PPD_CMAS_GET, PPD_AMAS_GET, CPL_VALLIST
	INTEGER*4	STR_SIGLEN, MOVE_BLB
C
	BYTE		BUF(LBUF)
	CHARACTER*1	DTYPE, XDTYPE
	INTEGER*4	PLEN, XPLEN, NVAL, XNVAL, MNVAL, MXVAL
	INTEGER*4	IS, LSTR, LARR, COUNT
C
C
	PPDPD$MXOFF = UNDEF_J
	PPDPD$MXLEN = 0
	LSTR = STR_SIGLEN (STRING)
C
	IF (IAND(PPD_CMAS_GET('MAXIMUM'),1) .NE. 0) THEN
C
C					If input as a character-string list:
C					- maximum values must be given
C					- decode the list into an array
C					- check the nr of values
C					- store the array in the description
C
		IF (DO_CHECK) THEN
			IF (LSTR.EQ.0 .OR. STRING(:LSTR).EQ.EMPTVAL) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			IS = PPD_DTYPE_GET (DTYPE,PLEN)
			IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
			IS = CPL_VALLIST (STRING(:LSTR),DTYPE,PLEN,
	1							BUF,LBUF,COUNT)
			IF (IAND(IS,1).EQ.0) THEN
				IS = PPD_MMINV
				GOTO 999
			ENDIF
C
			IF (IAND(PPD_AMAS_GET('VECTOR'),1) .NE. 0) THEN
				IF (COUNT.NE.NVAL) IS = PPD_VCINVNVL
			ELSE
				IF (COUNT.NE.1) IS = PPD_NVCINVNVL
			ENDIF
			IF (IAND(IS,1).EQ.0) GOTO 999
C
			PPDPD$MXOFF = PPDPD$LENG
			PPDPD$MXLEN = PLEN*COUNT
			PPDPD$LENG = PPDPD$LENG+PPDPD$MXLEN
			IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
			IS = MOVE_BLB (BUF,PPDPD_(PPDPD$MXOFF+1),PLEN*COUNT)
C
C					If input as an array:
C					- array cannot be empty
C					- check datatype, value length
C					  and nr of values
C					- store the array in the description
C
		ELSE
			LARR = LEN (STRING)
			IF (LARR.EQ.0) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			IS = PPD_DTYPE_GET (DTYPE,PLEN)
			IS = PPD_DTYPE_XGET (XDTYPE,XPLEN)
			IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
			IS = PPD_NVAL_XGET (XNVAL,MNVAL,MXVAL)
			IF (DTYPE.NE.XDTYPE .OR.
	1		    PLEN .NE.XPLEN  .OR.
	2		    NVAL .NE.XNVAL) THEN
				IS = PPD_MMNOVAL
				GOTO 999
			ENDIF
C
			PPDPD$MXOFF = PPDPD$LENG
			PPDPD$MXLEN = LARR
			PPDPD$LENG = PPDPD$LENG+LARR
			IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
			IS = MOVE_BLB (%REF(STRING),PPDPD_(PPDPD$MXOFF+1),LARR)
		ENDIF
C
	ELSE IF (DO_CHECK) THEN
		IF (LSTR.GT.0. AND. STRING(:LSTR).NE.EMPTVAL) THEN
			IS = PPD_MMNOCHK
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_MAX_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_MAX_PUT = IS
	RETURN
C
 9999	PPD_MAX_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MAX_GET (ARRAY,MAXL,LARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (o) maximum values
	INTEGER*4	MAXL		! (i) size of array
	INTEGER*4	LARR		! (o) significant size of array
C
C.Purpose:	Get the maximum values from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	otuput array too small
C.Notes:
C	- If no maximum values are given, LARR = 0 will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS
C
C
C					Get the array
C
	LARR = 0
	IF (PPDPD$MXOFF.NE.UNDEF_J) THEN
		IF (PPDPD$MXLEN.GT.MAXL) GOTO 999
		LARR = PPDPD$MXLEN
		IS = MOVE_BLB (PPDPD_(PPDPD$MXOFF+1),ARRAY,LARR)
	ENDIF
C
	PPD_MAX_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_MAX_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MAX_XGET (ARRAY,MAXL,LARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (o) maximum values
	INTEGER*4	MAXL		! (i) size of array
	INTEGER*4	LARR		! (o) nr of bytes in array
C
C.Purpose:	Get the maximum values for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C.Notes:
C	- The values are fetched directly from the mapped PPD file using
C	  the offset and length given in the current parameter description.
C	- Use XGET i.s.o. GET when the variable-length part of the current
C	  description contains data for another parameter (e.g. the COPY
C	  parameter in BLDPPD).
C	- If no maximum values are given, LARR = 0 will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	LARR = 0
C
C					Make sure that the PPD file is mapped
C					and that a parameter has been selected
C
	IS = PPD_STAT_INQ (MAPB,ADDR,HLPB)
	IF (MAPB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ELSE IF (ADDR.EQ.0) THEN
		IS = PPD_NOCURENTR
		GOTO 999
	ENDIF
C
C					Get the array
C
	IF (PPDPD$MXOFF.NE.UNDEF_J) THEN
		IF (PPDPD$MXLEN.LE.MAXL) THEN
			LARR = PPDPD$MXLEN
			ADDR = ADDR+PPDPD$MXOFF+1
			IS = MOVE_BLB (A_B(ADDR-A_OB),ARRAY,LARR)
		ELSE
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_MAX_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_MAX_XGET = MSG_SET (IS,0)
	RETURN
	END
