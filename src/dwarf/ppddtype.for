C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_DTYPE
C.Keywords:	PPD File, Parameter Datatype and Length
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*1	PPDPD$DTYPE	! (m) datatype
C	INTEGER*4	PPDPD$PLEN	! (m) length in bytes
C
C.Version:	900415 FMO - recreation
C.Version:	911024 GvD - test if length <=255
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DTYPE_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C 
C
	CHARACTER*(*)	STRING		! (i) proposed code
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Check and store the datatype code for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_TYPENOT	datatype not given
C	error	PPD_TYPEINV	invalid code
C	error	PPD_TYPCHKINV	datatype inconsistent with checks
C.Notes:
C	- The datatype is required and will be stored in the current parameter
C	  description (field PPDPD$DTYPE).
C	- If an invalid code is given, the default ('J') will be used.
C	- If DO_CHECK is off, only the consistency with the checks is looked at.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL, DEFDTYPE, TYPES
		PARAMETER (EMPTVAL  = '[]')
		PARAMETER (DEFDTYPE = 'J' )
		PARAMETER (TYPES    = 'BIJRDLC')
C
	INTEGER*4	PPD_CMAS_GET, STR_SIGLEN
C
	INTEGER*4	IS, LS
C
C
C					Check the code
C
	IF (DO_CHECK) THEN
		PPDPD$DTYPE = DEFDTYPE
		LS = STR_SIGLEN (STRING)
		IF (LS.EQ.0 .OR. STRING(:LS).EQ.EMPTVAL) THEN
			IS = PPD_TYPENOT
			GOTO 999
		ELSE IF (LS.GT.1 .OR. INDEX(TYPES,STRING(:1)).EQ.0) THEN
			IS = PPD_TYPEINV
			GOTO 999
		ENDIF
		PPDPD$DTYPE = STRING(:1)
	ENDIF
C
C					Check against the requested checks
C
	IF (PPDPD$DTYPE.EQ.'L' .AND.
	1	(IAND(PPD_CMAS_GET ('MINIMUM'       ),1) .NE. 0 .OR.
	2	 IAND(PPD_CMAS_GET ('MAXIMUM'       ),1) .NE. 0 .OR.
	3	 IAND(PPD_CMAS_GET ('ASCENDING'     ),1) .NE. 0 .OR.
	4	 IAND(PPD_CMAS_GET ('DESCENDING'    ),1) .NE. 0 .OR.
	5	 IAND(PPD_CMAS_GET ('NON_ASCENDING' ),1) .NE. 0 .OR.
	6	 IAND(PPD_CMAS_GET ('NON_DESCENDING'),1) .NE. 0)) THEN
			IS = PPD_TYPCHKINV
			GOTO 999
	ELSE IF (PPDPD$DTYPE.NE.'C' .AND.
	1	(IAND(PPD_CMAS_GET ('ALPHABETIC'    ),1) .NE. 0 .OR.
	2	 IAND(PPD_CMAS_GET ('NUMERIC'       ),1) .NE. 0 .OR.
	3	 IAND(PPD_CMAS_GET ('ANUMERIC'      ),1) .NE. 0 .OR.
	4	 IAND(PPD_CMAS_GET ('OPTIONS'       ),1) .NE. 0 .OR.
	5	 IAND(PPD_CMAS_GET ('ABBREV_OPTIONS'),1) .NE. 0 .OR.
	6	 IAND(PPD_CMAS_GET ('NODE'          ),1) .NE. 0)) THEN
			IS = PPD_TYPCHKINV
			GOTO 999
	ENDIF
C
	PPD_DTYPE_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_DTYPE_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PLEN_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed length
	LOGICAL*4	DO_CHECK	! (i) check string syntax ?
C
C.Purpose:	Decode, check and store the length for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_LENGTHNOT	length not given for character type
C	error	PPD_NOTPOSINT	must be a positive integer
C	error   PPD_LENGTHLON	length too long
C	error	PPD_LENGTHINV	invalid length for given datatype
C.Notes:
C	- The datatype will be stored in the current parameter description
C	  (field PPDPD$PLEN).
C	- If an invalid length is given, the default for the datatype will
C	  be used.
C	- If DO_CHECK is off, only the match with datatype with be checked.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNR
	CHARACTER*(*)	EMPTVAL, TYPES
		PARAMETER (MAXNR   = 7)
		PARAMETER (EMPTVAL = '[]')
		PARAMETER (TYPES   = 'BIJRDLC')
	INTEGER*4	DEFPLEN(MAXNR)
		DATA DEFPLEN /1,2,4,4,8,1,1/
C
	INTEGER*4	STR_SIGLEN, STR_READ_J
C
	INTEGER*4	IS, LS, NR
	INTEGER*4	PLEN
C
	NR = INDEX(TYPES,PPDPD$DTYPE)
C
C					Decode and check
C
	IF (DO_CHECK) THEN
		LS = STR_SIGLEN (STRING)
		IF (LS.EQ.0 .OR. STRING(:LS).EQ.EMPTVAL) THEN
			IF (PPDPD$DTYPE.EQ.'C') THEN
				IS = PPD_LENGTHNOT
				GOTO 999
			ENDIF
			PPDPD$PLEN = DEFPLEN(NR)
		ELSE
			IS = STR_READ_J (STRING(:LS),PLEN)
			IF (IAND(IS,1).EQ.0 .OR. PLEN.LE.0) THEN
				IS = PPD_NOTPOSINT
				GOTO 999
			ELSE IF (PLEN.GT.255) THEN
				IS = PPD_LENGTHLON
				GOTO 999
			ENDIF
			PPDPD$PLEN = PLEN
		ENDIF
	ENDIF
C
C					Check match with datatype
C
	IF (PPDPD$DTYPE.NE.'C' .AND. PPDPD$DTYPE.NE.'L'
	1	               .AND. PPDPD$PLEN.NE.DEFPLEN(NR)) THEN
		IS = PPD_LENGTHINV
		GOTO 999
	ENDIF
C
	PPD_PLEN_PUT = PPD_SUCCESS
	RETURN
C
 999	PPDPD$PLEN = DEFPLEN(NR)
	PPD_PLEN_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DTYPE_GET (CODE,PLEN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	CODE		! (o) datatype code
	INTEGER*4	PLEN		! (o) parameter length in bytes
C
C.Purpose:	Get the datatype and length from the current param description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
C
	CODE = PPDPD$DTYPE
	PLEN = PPDPD$PLEN
C
	PPD_DTYPE_GET = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DTYPE_XGET (CODE,PLEN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	CODE		! (o) datatype code
	INTEGER*4	PLEN		! (o) parameter length in bytes
C
C.Purpose:	Get the datatype and length for the current parameter
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
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	CODE = ' '
	PLEN = 0
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
	IS = MOVE_BLB (A_B(ADDR+PPDPD_DTYPE-A_OB),%REF(CODE),1)
	IS = MOVE_BLJ (A_B(ADDR+PPDPD_PLEN-A_OB),PLEN,1)
C
	PPD_DTYPE_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_DTYPE_XGET = MSG_SET (IS,0)
	RETURN
	END
