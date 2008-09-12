C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_NSETS
C.Keywords:	PPD File, Maximum Number of Sets
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPDPD$NSETS	! (m) max nr of sets
C
C.Version:	900415 FMO - recreation
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_NSETS_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed nr
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Decode, check and store the maximum nr of value sets
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOTPOSINT	must be a positive integer
C.Notes:
C	- The nr will be stored in the current parameter description
C	  (field PPDPD$NSETS).
C	- If an invalid nr is given, the default (LARGEST_I) will be used.
C	- If DO_CHECK is off, the function just returns with PPD_SUCCESS.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL
	INTEGER*4	DEFNSETS
		PARAMETER (EMPTVAL  = '[]')
		PARAMETER (DEFNSETS = LARGEST_J)
C
	INTEGER*4	STR_SIGLEN, STR_READ_J
C
	INTEGER*4	IS, LS
	INTEGER*4	NSETS
C
C
C					Decode and check
C
	IF (DO_CHECK) THEN
		PPDPD$NSETS = DEFNSETS
		LS = STR_SIGLEN (STRING)
		IF (LS.GT.0 .AND. STRING(:LS).NE.EMPTVAL) THEN
			IS = STR_READ_J (STRING(:LS),NSETS)
			IF (IAND(IS,1).EQ.0 .OR. NSETS.LE.0) GOTO 999
			PPDPD$NSETS = NSETS
		ENDIF
	ENDIF
C
	PPD_NSETS_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_NSETS_PUT = PPD_NOTPOSINT
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_NSETS_GET (NSETS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NSETS		! (o) max nr of value sets
C
C.Purpose:	Get the max nr of sets from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	NSETS = PPDPD$NSETS
	PPD_NSETS_GET = PPD_SUCCESS
	RETURN
	END
