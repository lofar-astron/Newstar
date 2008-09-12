C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_CHECK
C.Keywords:	PPD File, Check Parameter Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	920413 GvD - define SWARR as log*4 iso. log*1
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK (ARR,NVSPEC,NVRES,SWARR,NSETS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARR(*)		! (m) value array to be checked
	INTEGER*4	NVSPEC(*)	! (i) nr of specified values per set
	INTEGER*4	NVRES		! (i) nr of reserved values in each set
	LOGICAL*4	SWARR(*)	! (i) "value defined" switch array
	INTEGER*4	NSETS		! (i) nr of value-sets in ARR
C
C.Purpose:	Check the array of values for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_ERRMINCHK	error in check against minimum value
C	error	PPD_ERRMAXCHK	error in check against maximum value
C	error	PPD_NUMSETMAX	number of value-sets exceeds maximum
C	error	PPD_NUMVALMIN	number of values less than minimum
C	error	PPD_NUMVALMAX	number of values exceeds maximum
C	error	PPD_VALLSSMIN	value less than minimum
C	error	PPD_VALEXCMAX	value exceeds maximum
C	error	PPD_STRNOTALP	string not alphabetic
C	error	PPD_STRNOTNUM	string not numeric
C	error	PPD_STRNOTAN	string not alpha-numeric
C	error	DBD_BADNODE	bad node name
C	false status returned by referenced routines
C.Notes:
C	- ARR(PLEN,NVRES,NSETS) overlays the value array VALARR(NVRES,NSETS),
C	  of datatype DTYPE and a value length of PLEN bytes.
C	- NVSPEC(NSETS)
C	- SWARR(NVRES,NSETS) tells for each element in VALARR whether or not
C	  the value was specified. This array has significance for vectors only.
C	- For scalars and numeric arrays, VALARR is always in TO/BY format:
C	  triplets of values (start, end, step).
C	- For vectors and character arrays, VALARR just contains normal values.
C								
C	The following checks are performed:
C
C	Number of value sets
C	- For arrays and vectors, NSETS should not exceed the maximum nr of sets
C	  allowed for the parameter (field PPDPD$NSETS in the PPD file).
C	- For scalars, the total nr of values in VALARR should not exceed the
C	  maximum nr of sets. The nr of values is counted via the routine
C	  PV_SET_TOBY_C.
C
C	Number of values per set
C	- For vectors, NVSPEC must be within limits (fields PPDPD$MNVAL and
C	  PPDPD$MXVAL).
C	- For arrays, the nr of values per set should not exceed PPDPD$MXVAL.
C	  The nr of values is counted via the routine PV_SET_TOBY_C.
C
C	Checks on individual values
C	- The PPD field PPDPD$CMAS tells which checks have to be performed.
C	- If the abbreviated_option check is requested and the value is a valid
C	  abbreviation, it will be replaced by the full option.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	SIZE
		PARAMETER (SIZE = 256)
C
	INTEGER*4	PPD_AMAS_GET	,PPD_CMAS_GET	,PPD_DTYPE_GET
	INTEGER*4	PPD_NVAL_GET	,PPD_NSETS_GET	,PPD_OPSTR_MATCH
	INTEGER*4	PPD_MIN_GET	,PPD_MAX_GET
	INTEGER*4	PPD_CHECK_ASC	,PPD_CHECK_DES	,PPD_CHECK_NAS
	INTEGER*4	PPD_CHECK_NDE	,PPD_CHECK_NODE	,PPD_FAO
	INTEGER*4	PV_SET_TOBY_C
	INTEGER*4	STR_SIGLEN
	INTEGER*4	STR_CHECK_ALPH	,STR_CHECK_NUM	,STR_CHECK_ANUM
	INTEGER*4	MSG_SET  	,MOVE_BLB	,BLB_COMPARE
C
	CHARACTER	DTYPE*1, STRING*(SIZE)
	BYTE		MIN(SIZE), MAX(SIZE)
	INTEGER*4	LSTR, LMIN, LMAX
	INTEGER*4	PLEN, NVALS, MINNVALS, MAXNVALS, MAXNSETS
	INTEGER*4	IS, SNR, VNR, SPTR, VPTR, MPTR, PTR
	INTEGER*4	CNT, NSCALARS
	LOGICAL*4	TOBY, VECTOR, SCALAR
	LOGICAL*4	CHMIN, CHMAX, CHASC, CHDES, CHNAS, CHNDE
	LOGICAL*4	CHALP, CHNUM, CHANM, CHOPT, CHOPS, CHNOD
C
C
C					Get data from the parameter description
C					- datatype and parameter length
C					- skip checking of logical parms
C
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (DTYPE.EQ.'L') GOTO 900
C
C					- max nr of sets
C					- (min/max) nr of values per set
C
	IS = PPD_NSETS_GET (MAXNSETS)
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NVALS,MINNVALS,MAXNVALS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					- parameter attributes
C
	VECTOR = IAND(PPD_AMAS_GET ('VECTOR'),1) .NE. 0
	TOBY = .NOT.VECTOR .AND. DTYPE.NE.'C'
	SCALAR = NVALS.EQ.1
C
C					- checks to be performed
C
	CHMIN = IAND(PPD_CMAS_GET ('MINIMUM')       ,1) .NE. 0
	CHMAX = IAND(PPD_CMAS_GET ('MAXIMUM')       ,1) .NE. 0
	CHASC = IAND(PPD_CMAS_GET ('ASCENDING')     ,1) .NE. 0
	CHDES = IAND(PPD_CMAS_GET ('DESCENDING')    ,1) .NE. 0
	CHNAS = IAND(PPD_CMAS_GET ('NON_ASCENDING') ,1) .NE. 0
	CHNDE = IAND(PPD_CMAS_GET ('NON_DESCENDING'),1) .NE. 0
	CHALP = IAND(PPD_CMAS_GET ('ALPHABETIC')    ,1) .NE. 0
	CHNUM = IAND(PPD_CMAS_GET ('NUMERIC')       ,1) .NE. 0
	CHANM = IAND(PPD_CMAS_GET ('ANUMERIC')      ,1) .NE. 0
	CHOPT = IAND(PPD_CMAS_GET ('OPTIONS')       ,1) .NE. 0
	CHOPS = IAND(PPD_CMAS_GET ('ABBREV_OPTIONS'),1) .NE. 0
	CHNOD = IAND(PPD_CMAS_GET ('NODE')          ,1) .NE. 0
C
C					- (arrays of) min and max values
C
	IF (CHMIN) THEN
		IS = PPD_MIN_GET (MIN,SIZE,LMIN)
		IF (IAND(IS,1).EQ.0) THEN
			IS = MSG_SET (PPD_ERRMINCHK,0)
			GOTO 999
		ENDIF
	ENDIF
	IF (CHMAX) THEN
		IS = PPD_MAX_GET (MAX,SIZE,LMAX)
		IF (IAND(IS,1).EQ.0) THEN
			IS = MSG_SET (PPD_ERRMAXCHK,0)
			GOTO 999
		ENDIF
	ENDIF
C
C					Check number of sets
C
	IF (.NOT.SCALAR .AND. NSETS.GT.MAXNSETS) THEN
		IS = MSG_SET (PPD_NUMSETMAX,1)
		CALL WNCTXT(DWLOG,DWMSG,NSETS,MAXNSETS)
		GOTO 999
	ENDIF
C
C					+----------------------+
C					| Checks per value set |
C					+----------------------+
C
C					- skip sets without specified values
C					- SPTR: start index of set in ARR
C
	NSCALARS = 0
	DO SNR = 1,NSETS
	 IF (NVSPEC(SNR).GT.0) THEN
		SPTR = (SNR-1)*PLEN*NVRES+1
C
C					Count the nr of values in the set
C					- vector: only default values is OK
C					- array
C					- scalar: just count all values in
C					  all sets (-> NSCALARS)
C
		NVALS = 0
		IF (VECTOR) THEN
			PTR = (SNR-1)*NVRES+1
			DO VNR = 1,NVSPEC(SNR)
				IF (SWARR(PTR)) NVALS = NVALS+1
				PTR = PTR+1
			ENDDO
			IF (NVALS.EQ.0)	NVALS = MAXNVALS
		ELSE IF (DTYPE.EQ.'C') THEN			! no TO/BY
			DO PTR = SPTR,SPTR+(NVSPEC(SNR)-1)*PLEN,PLEN
				IF (ARR(PTR).NE.UNDEF_B) NVALS = NVALS+1
			ENDDO
			IF (SCALAR) NSCALARS = NSCALARS+NVALS
		ELSE						! TO/BY form
			DO PTR = SPTR,SPTR+(NVSPEC(SNR)-1)*PLEN,3*PLEN
				IS = PV_SET_TOBY_C (DTYPE,PLEN,ARR(PTR),CNT)
				IF (IAND(IS,1).EQ.0) GOTO 999
				NVALS = NVALS+CNT+1
			ENDDO
			IF (SCALAR) NSCALARS = NSCALARS+NVALS
		ENDIF
C
C					Check the nr of values
C
		IF (.NOT.SCALAR) THEN
			IF (NVALS.LT.MINNVALS) THEN
				IS = MSG_SET (PPD_NUMVALMIN,1)
				CALL WNCTXT(DWLOG,DWMSG,NVALS,MINNVALS)
				GOTO 999
			ELSE IF (NVALS.GT.MAXNVALS) THEN
				IS = MSG_SET (PPD_NUMVALMAX,1)
				CALL WNCTXT(DWLOG,DWMSG,NVALS,MAXNVALS)
				GOTO 999
			ENDIF
		ENDIF
C
C					Check the order of the values
C
		IF (CHASC) THEN
			IS = PPD_CHECK_ASC (ARR(SPTR),NVSPEC(SNR),DTYPE,PLEN,TOBY)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ELSE IF (CHNDE) THEN
			IS = PPD_CHECK_NDE (ARR(SPTR),NVSPEC(SNR),DTYPE,PLEN,TOBY)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ELSE IF (CHNAS) THEN
			IS = PPD_CHECK_NAS (ARR(SPTR),NVSPEC(SNR),DTYPE,PLEN,TOBY)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ELSE IF (CHDES) THEN
			IS = PPD_CHECK_DES (ARR(SPTR),NVSPEC(SNR),DTYPE,PLEN,TOBY)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
C
C					+------------------+
C					| Checks per value |
C					+------------------+
C
C					- don't check TO/BY step values
C					- VPTR: start index of value in ARR
C					- MPTR: start index of value in MIN/MAX
C
		MPTR = 1
		DO VNR = 1,NVSPEC(SNR)
		 IF (.NOT.TOBY .OR. MOD(VNR,3).NE.0) THEN
			VPTR = SPTR+(VNR-1)*PLEN
			IF (VECTOR) MPTR = 1+(VNR-1)*PLEN
C
C					Check against minimum/maximum
C
			IF (CHMIN) THEN
				IS = BLB_COMPARE (ARR(VPTR),MIN(MPTR),DTYPE,
	1								PLEN)
				IF (IS.LT.1) THEN
					IS = PPD_FAO (MIN(MPTR),PLEN,DTYPE,
	1						PLEN,STRING,LSTR)
					IS = MSG_SET (PPD_VALLSSMIN,0)
					CALL WNCTXT(DWLOG,DWMSG,STRING(:LSTR))
					GOTO 999
				ENDIF
			ENDIF
			IF (CHMAX) THEN
				IS = BLB_COMPARE (ARR(VPTR),MAX(MPTR),DTYPE,
	1								PLEN)
				IF (IS.GT.1) THEN
					IS = PPD_FAO (MAX(MPTR),PLEN,
	1						DTYPE,PLEN,STRING,LSTR)
					IS = MSG_SET (PPD_VALEXCMAX,0)
					CALL WNCTXT(DWLOG,DWMSG,STRING(:LSTR))
					GOTO 999
				ENDIF
			ENDIF
C
C					Check character-string type
C					- alphabetic, numeric or alphanumeric ?
C
			IF (CHALP) THEN
				IS = MOVE_BLB (ARR(VPTR),%REF(STRING),PLEN)
				LSTR = STR_SIGLEN (STRING(:PLEN))
				IS = STR_CHECK_ALPH (STRING(:LSTR))
				IF (IAND(IS,1).EQ.0) THEN
					IS = MSG_SET (PPD_STRNOTALP,0)
					GOTO 999
				ENDIF
			ELSE IF (CHNUM) THEN
				IS = MOVE_BLB (ARR(VPTR),%REF(STRING),PLEN)
				LSTR = STR_SIGLEN (STRING(:PLEN))
				IS = STR_CHECK_NUM (STRING(:LSTR))
				IF (IAND(IS,1).EQ.0) THEN
					IS = MSG_SET (PPD_STRNOTNUM,0)
					GOTO 999
				ENDIF
			ELSE IF (CHANM) THEN
				IS = MOVE_BLB (ARR(VPTR),%REF(STRING),PLEN)
				LSTR = STR_SIGLEN (STRING(:PLEN))
				IS = STR_CHECK_ANUM (STRING(:LSTR))
				IF (IAND(IS,1).EQ.0) THEN
					IS = MSG_SET (PPD_STRNOTAN,0)
					GOTO 999
				ENDIF
			ENDIF
C
C					Check whether valid option
C					- return full option in ARR
C
			IF (CHOPT.OR.CHOPS) THEN
				IS = PPD_OPSTR_MATCH (ARR(VPTR),PLEN,CHOPS)
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
C
C					Check whether valid node name
C
			IF (CHNOD) THEN
				IS = MOVE_BLB (ARR(VPTR),%REF(STRING),PLEN)
				LSTR = STR_SIGLEN (STRING(:PLEN))
				IS = PPD_CHECK_NODE (STRING(:LSTR))
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		 ENDIF
		ENDDO
	 ENDIF
 	ENDDO
C
C					For scalars:
C					check the total nr of values in the
C					array against the max nr of sets
C
	IF (SCALAR .AND. NSCALARS.GT.MAXNSETS) THEN
		IS = MSG_SET (PPD_NUMVALMAX,1)
	CALL WNCTXT(DWLOG,DWMSG,NSCALARS,MAXNSETS)
	ENDIF
C
 900	PPD_CHECK = PPD_SUCCESS
	RETURN
C
 999	PPD_CHECK = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK_ASC
	1				(ARRAY,NELEM,DTYPE,PLEN,TOBY_FORM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PLEN		! (i) nr of bytes per element
	BYTE		ARRAY(PLEN,*)	! (i) array
	INTEGER*4	NELEM		! (i) number of elements in array
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D,C)
	LOGICAL*4	TOBY_FORM	! (i) TO/BY format ?
C
C.Purpose:	Check whether the array elements are in ascending order
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_ARRNOTASC	array not in ascending order
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	BLB_COMPARE, BLB_COMPAR1, MSG_SET  
C
	INTEGER*4	IS
C
C
	IF (.NOT.TOBY_FORM) THEN
		DO I = 2,NELEM
		    IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-1),DTYPE,PLEN)
		    IF (IS.LE.1) GOTO 999
		ENDDO
C
C					TO/BY format (start,end,step values):
C					- start > preceding end
C					- end >= start (= only for special step)
C					- special: step = end, or step = 1
C
	ELSE
		DO I = 1,NELEM,3
		    IF (I.GT.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-2),DTYPE,PLEN)
			IF (IS.LE.1) GOTO 999
		    ENDIF
C
		    IS = BLB_COMPARE (ARRAY(1,I+1),ARRAY(1,I),DTYPE,PLEN)
		    IF (IS.LT.1) GOTO 999
C
		    IF (IS.EQ.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I+2),ARRAY(1,I+1),DTYPE,PLEN)
			IF (IS.NE.1) IS = BLB_COMPAR1 (ARRAY(1,I+2),DTYPE,PLEN)
 			IF (IS.NE.1) GOTO 999
		    ENDIF
		ENDDO
	ENDIF
C
	PPD_CHECK_ASC = PPD_SUCCESS
	RETURN
C
 999	PPD_CHECK_ASC = MSG_SET (PPD_ARRNOTASC,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK_NDE
	1				(ARRAY,NELEM,DTYPE,PLEN,TOBY_FORM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PLEN		! (i) nr of bytes per element
	BYTE		ARRAY(PLEN,*)	! (i) array
	INTEGER*4	NELEM		! (i) number of elements in array
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D,C)
	LOGICAL*4	TOBY_FORM	! (i) TO/BY format ?
C
C.Purpose:	Check whether the array elements are in non-descending order
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_ARRNOTNDE	array not in non-descending order
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	BLB_COMPARE, BLB_COMPAR1, MSG_SET  
C
	INTEGER*4	IS
C
C
	IF (.NOT.TOBY_FORM) THEN
		DO I = 2,NELEM
		    IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-1),DTYPE,PLEN)
		    IF (IS.LT.1) GOTO 999
		ENDDO
C
C					TO/BY format (start,end,step values):
C					- start >= preceding end
C					- end >= start (= only for special step)
C					- special: step = end, or step = 1
C
	ELSE
		DO I = 1,NELEM,3
		    IF (I.GT.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-2),DTYPE,PLEN)
			IF (IS.LT.1) GOTO 999
		    ENDIF
C
		    IS = BLB_COMPARE (ARRAY(1,I+1),ARRAY(1,I),DTYPE,PLEN)
		    IF (IS.LT.1) GOTO 999
C
		    IF (IS.EQ.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I+2),ARRAY(1,I+1),DTYPE,PLEN)
			IF (IS.NE.1) IS = BLB_COMPAR1 (ARRAY(1,I+2),DTYPE,PLEN)
 			IF (IS.NE.1) GOTO 999
		    ENDIF
		ENDDO
	ENDIF
C
	PPD_CHECK_NDE = PPD_SUCCESS
	RETURN
C
 999	PPD_CHECK_NDE = MSG_SET (PPD_ARRNOTNDE,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK_DES
	1				(ARRAY,NELEM,DTYPE,PLEN,TOBY_FORM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PLEN		! (i) nr of bytes per element
	BYTE		ARRAY(PLEN,*)	! (i) array
	INTEGER*4	NELEM		! (i) number of elements in array
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D,C)
	LOGICAL*4	TOBY_FORM	! (i) TO/BY format ?
C
C.Purpose:	Check whether the array elements are in descending order
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_ARRNOTDES	array not in descending order
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	BLB_COMPARE, BLB_COMPAR1, MSG_SET  
C
	INTEGER*4	IS
C
C
	IF (.NOT.TOBY_FORM) THEN
		DO I = 2,NELEM
		    IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-1),DTYPE,PLEN)
		    IF (IS.GE.1) GOTO 999
		ENDDO
C
C					TO/BY format (start,end,step values):
C					- start < preceding end
C					- end <= start (= only for special step)
C					- special: step = end, or step = 1
C
	ELSE
		DO I = 1,NELEM,3
		    IF (I.GT.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-2),DTYPE,PLEN)
			IF (IS.GE.1) GOTO 999
		    ENDIF
C
		    IS = BLB_COMPARE (ARRAY(1,I+1),ARRAY(1,I),DTYPE,PLEN)
		    IF (IS.GT.1) GOTO 999
C
		    IF (IS.EQ.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I+2),ARRAY(1,I+1),DTYPE,PLEN)
			IF (IS.NE.1) IS = BLB_COMPAR1 (ARRAY(1,I+2),DTYPE,PLEN)
 			IF (IS.NE.1) GOTO 999
		    ENDIF
		ENDDO
	ENDIF
C
	PPD_CHECK_DES = PPD_SUCCESS
	RETURN
C
 999	PPD_CHECK_DES = MSG_SET (PPD_ARRNOTDES,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK_NAS
	1				(ARRAY,NELEM,DTYPE,PLEN,TOBY_FORM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PLEN		! (i) nr of bytes per element
	BYTE		ARRAY(PLEN,*)	! (i) array
	INTEGER*4	NELEM		! (i) number of elements in array
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D,C)
	LOGICAL*4	TOBY_FORM	! (i) TO/BY format ?
C
C.Purpose:	Check whether the array elements are in non-ascending order
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_ARRNOTNAS	array not in non-ascending order
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	BLB_COMPARE, BLB_COMPAR1, MSG_SET  
C
	INTEGER*4	IS
C
C
	IF (.NOT.TOBY_FORM) THEN
		DO I = 2,NELEM
		    IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-1),DTYPE,PLEN)
		    IF (IS.GT.1) GOTO 999
		ENDDO
C
C					TO/BY format (start,end,step values):
C					- start <= preceding end
C					- end <= start (= only for special step)
C					- special: step = end, or step = 1
C
	ELSE
		DO I = 1,NELEM,3
		    IF (I.GT.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I),ARRAY(1,I-2),DTYPE,PLEN)
			IF (IS.GT.1) GOTO 999
		    ENDIF
C
		    IS = BLB_COMPARE (ARRAY(1,I+1),ARRAY(1,I),DTYPE,PLEN)
		    IF (IS.GT.1) GOTO 999
C
		    IF (IS.EQ.1) THEN
			IS = BLB_COMPARE (ARRAY(1,I+2),ARRAY(1,I+1),DTYPE,PLEN)
			IF (IS.NE.1) IS = BLB_COMPAR1 (ARRAY(1,I+2),DTYPE,PLEN)
 			IF (IS.NE.1) GOTO 999
		    ENDIF
		ENDDO
	ENDIF
C
	PPD_CHECK_NAS = PPD_SUCCESS
	RETURN
C
 999	PPD_CHECK_NAS = MSG_SET (PPD_ARRNOTNAS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CHECK_NODE (NODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (i) full node name
C
C.Purpose:	Check node name syntax
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	DBD_NAMTOLNG	junction name longer than 8 characters
C	error	DBD_NAMTOMNY	more than 16 junction names
C	error	DBD_BADNODE	syntax error
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MAXNR, MAXLEN
		PARAMETER (MAXNR = 16)
		PARAMETER (MAXLEN = 8)
	CHARACTER*(*)	DELIM, NULLNODE
		PARAMETER (DELIM = '.')
		PARAMETER (NULLNODE = '0')
C
	INTEGER*4	PPD_AMAS_GET
	INTEGER*4	STR_SIGLEN, STR_SKIP_U, MSG_SET  
C
	INTEGER*4	IS, LN, NR, NSKIP, PTR
C
C
	NR = 0
	LN = STR_SIGLEN (NODE)
	IF (LN.GT.0) THEN
		PTR = 1
		NSKIP = STR_SKIP_U (DELIM,NODE(:LN),PTR)
		IF (NSKIP.GT.MAXLEN) GOTO 991
		IF (NSKIP.NE.1 .OR. NODE(1:1).NE.NULLNODE) NR = 1
		PTR = PTR+1
		DO WHILE (PTR.LE.LN)
			NR = NR+1
			IF (NR.GT.MAXNR) GOTO 992
			NSKIP = STR_SKIP_U (DELIM,NODE(:LN),PTR)
			IF (NSKIP.GT.MAXLEN) GOTO 991
			PTR = PTR+1
		ENDDO
	ENDIF
	IF (NR.EQ.0
	1 .AND. IAND(PPD_AMAS_GET('NULL_NODE'),1) .EQ. 0) GOTO 999
C
	PPD_CHECK_NODE = PPD_SUCCESS
	RETURN
C
 991	IS = MSG_SET (DBD_NAMTOLNG,1)
	CALL WNCTXT(DWLOG,DWMSG,NODE(PTR-NSKIP:PTR-1))
	GOTO 999
 992	IS = MSG_SET (DBD_NAMTOMNY,1)
	CALL WNCTXT(DWLOG,DWMSG,MAXNR)
 999	PPD_CHECK_NODE = MSG_SET (DBD_BADNODE,0)
	RETURN
	END
