C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_NVAL
C.Keywords:	PPD File, Number of Values per Set
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPDPD$NVAL	! (m) nr of values per set
C	INTEGER*4	PPDPD$MNVAL	! (m) minimum nr of values per set
C	INTEGER*4	PPDPD$MXVAL	! (m) maximum nr of values per set
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_NVAL_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed nr
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Decode, check and store the nr of values per set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOTPOSINT	must be a positive integer
C	error	PPD_NVLINVVEC	vector attribute only allowed if nr > 1
C	error	PPD_NVLINVCHK	order checks only allowed if nr > 1
C.Notes:
C	- The nr will be stored in the current parameter description
C	  (field PPDPD$NVAL).
C	- If an invalid nr is given, the default (1) will be used.
C	- If DO_CHECK is off, only the consistency with attributes and checks
C	  is tested.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL
	INTEGER*4	DEFNVAL
		PARAMETER (EMPTVAL = '[]')
		PARAMETER (DEFNVAL =  1  )
C
	INTEGER*4	PPD_AMAS_GET, PPD_CMAS_GET
	INTEGER*4	STR_SIGLEN, STR_READ_J
C
	INTEGER*4	IS, LS
	INTEGER*4	NVAL
C
C
C					Decode and check
C
	IF (DO_CHECK) THEN
		PPDPD$NVAL = DEFNVAL
		LS = STR_SIGLEN (STRING)
		IF (LS.GT.0 .AND. STRING(:LS).NE.EMPTVAL) THEN
			IS = STR_READ_J (STRING(:LS),NVAL)
			IF (IAND(IS,1).EQ.0 .OR. NVAL.LE.0) THEN
				IS = PPD_NOTPOSINT
				GOTO 999
			ENDIF
			PPDPD$NVAL = NVAL
		ENDIF
	ENDIF
C
C					Check against attributes and checks
C
	IF (PPDPD$NVAL.EQ.1) THEN
		IF (IAND(PPD_AMAS_GET ('VECTOR'),1) .NE. 0) THEN
			IS = PPD_NVLINVVEC
			GOTO 999
		ENDIF
		IF (IAND(PPD_CMAS_GET ('ASCENDING'     ),1) .NE. 0 .OR.
	1	    IAND(PPD_CMAS_GET ('DESCENDING'    ),1) .NE. 0 .OR.
	2	    IAND(PPD_CMAS_GET ('NON_ASCENDING' ),1) .NE. 0 .OR.
	3	    IAND(PPD_CMAS_GET ('NON_DESCENDING'),1) .NE. 0) THEN
			IS = PPD_NVLINVCHK
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_NVAL_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_NVAL_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MNVAL_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed nr
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Decode, check and store the minimum nr of values per set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOTPOSINT	must be a positive integer
C	error	PPD_MNVALINV	minimum nr > nr
C.Notes:
C	- The nr will be stored in the current parameter description
C	  (field PPDPD$MNVAL).
C	- If an invalid nr is given, the default (1) will be used.
C	- If DO_CHECK is off, only the consistency with the nr of values will
C	  be tested.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL
	INTEGER*4	DEFMNVAL
		PARAMETER (EMPTVAL  = '[]')
		PARAMETER (DEFMNVAL =  1  )
C
	INTEGER*4	STR_SIGLEN, STR_READ_J
C
	INTEGER*4	IS, LS
	INTEGER*4	MNVAL
C
C
C					Decode and check
C
	IF (DO_CHECK) THEN
		PPDPD$MNVAL = DEFMNVAL
		LS = STR_SIGLEN (STRING)
		IF (LS.GT.0 .AND. STRING(:LS).NE.EMPTVAL) THEN
			IS = STR_READ_J (STRING(:LS),MNVAL)
			IF (IAND(IS,1).EQ.0 .OR. MNVAL.LE.0) THEN
				IS = PPD_NOTPOSINT
				GOTO 999
			ENDIF
			PPDPD$MNVAL = MNVAL
		ENDIF
	ENDIF
C
C					Check against nr of values
C
	IF (PPDPD$MNVAL.GT.PPDPD$NVAL) THEN
		PPDPD$MNVAL = DEFMNVAL
		IS = PPD_MNVALINV
		GOTO 999
	ENDIF
C
	PPD_MNVAL_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_MNVAL_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_MXVAL_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed nr
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Decode, check and store the maximum nr of values per set
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOTPOSINT	must be a positive integer
C	error	PPD_MXVALINV	maximum nr > nr or < minimum nr
C.Notes:
C	- The nr will be stored in the current parameter description
C	  (field PPDPD$MXVAL).
C	- If an invalid nr is given, the default (PPDPD$NVAL) will be used.
C	- If DO_CHECK is off, only the consistency with the (minimum) nr of
C	  values will be tested.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL
		PARAMETER (EMPTVAL  = '[]')
C
	INTEGER*4	STR_SIGLEN, STR_READ_J
C
	INTEGER*4	IS, LS
	INTEGER*4	MXVAL
C
C
C					Decode and check
C
	IF (DO_CHECK) THEN
		PPDPD$MXVAL = PPDPD$NVAL
		LS = STR_SIGLEN (STRING)
		IF (LS.GT.0 .AND. STRING(:LS).NE.EMPTVAL) THEN
			IS = STR_READ_J (STRING(:LS),MXVAL)
			IF (IAND(IS,1).EQ.0 .OR. MXVAL.LE.0) THEN
				IS = PPD_NOTPOSINT
				GOTO 999
			ENDIF
			PPDPD$MXVAL = MXVAL
		ENDIF
	ENDIF
C
C					Check against (minimum) nr of values
C
	IF (PPDPD$MXVAL.GT.PPDPD$NVAL
	1 .OR. PPDPD$MXVAL.LT.PPDPD$MNVAL) THEN
		PPDPD$MXVAL = PPDPD$NVAL
		IS = PPD_MXVALINV
		GOTO 999
	ENDIF
C
	PPD_MXVAL_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_MXVAL_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NVAL		! (o) nr of values per set
	INTEGER*4	MNVAL		! (o) minimum nr of values per set
	INTEGER*4	MXVAL		! (o) maximum nr of values per set
C
C.Purpose:	Get the nr of values per set from the current param description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	NVAL = PPDPD$NVAL
	MNVAL = PPDPD$MNVAL
	MXVAL = PPDPD$MXVAL
	PPD_NVAL_GET = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_NVAL_XGET (NVAL,MNVAL,MXVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NVAL		! (o) nr of values per set
	INTEGER*4	MNVAL		! (o) minimum nr of values per set
	INTEGER*4	MXVAL		! (o) maximum nr of values per set
C
C.Purpose:	Get the nr of values per set for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C.Notes:
C	- The fields are taken directly from the mapped PPD file using the
C	  start address of the description of the current parameter.
C	- Use XGET i.s.o. GET when the description array PPDPD_ contains
C	  data for another parameter.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	NVAL = 0
	MNVAL = 0
	MXVAL = 0
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
C					Get the fields
C
	IS = MOVE_BLJ (A_B(ADDR+PPDPD_NVAL-A_OB),NVAL,1)
	IS = MOVE_BLJ (A_B(ADDR+PPDPD_MNVAL-A_OB),MNVAL,1)
	IS = MOVE_BLJ (A_B(ADDR+PPDPD_MXVAL-A_OB),MXVAL,1)
C
	PPD_NVAL_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_NVAL_XGET = MSG_SET (IS,0)
	RETURN
	END
