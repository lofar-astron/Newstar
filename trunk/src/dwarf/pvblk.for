C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PV_BLK
C.Keywords:	Parameter Values, Block
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		The fields in the value block descriptor are:
C	VALBLK(1) = length of the block in bytes
C	VALBLK(2) = address of the memory block
C	          = address of the array with the nr of used values per set
C	VALBLK(3) = address of the value array
C	VALBLK(4) = address of the array with the value-defined switches
C	VALBLK(5) = nr of sets in the block
C	VALBLK(6) = nr of reserved values per set
C	VALBLK(7) = nr of bytes per value
C	VALBLK(8) = OR-ed flags (1 = scalar parm, 2 = TOBY format)
C
C.Version:	900416 FMO - recreation
C.Version:	900830 FMO - corrected wrong TOBY switch in READ
C.Version:	910730 FMO - corrected scalar special set handling in READ
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920413 GvD - use log*4 iso, log*1 for switches-array (VALBLK(4))
C			put switches in front of values for alignment purposes
C.Version:	920508 GvD - do not allow TOBY for logical data
C.Version:	940117 CMV - indirect adressing, use WNGGVM i.s.o. GEN_GETVM
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source-module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy function
C-------------------------------------------------------------------------
C
	PV_BLK = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK_ALLOC (VALSTR,VALBLK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	VALSTR		! (m) value string
	INTEGER*4	VALBLK(8)	! (o) value block descriptor
C
C.Purpose:	Allocate a value block to accommodate the value sets
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also if the string is blank
C	false status codes returned by referenced routines
C.Notes:
C	- Sets are delimited by semicolons (outside quoted substrings).
C	- For scalars, commas outside quoted substrings and subexpressions
C	  (parenthesized substrings) are converted to semicolons.
C	- If VALSTR is blank or an error occurs, VALBLK will be cleared.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	QUOTE, SCOLON, COMMA, OPNPAR, CLOPAR, DELIM
		PARAMETER (QUOTE  = '"')
		PARAMETER (SCOLON = ';')
		PARAMETER (COMMA  = ',')
		PARAMETER (OPNPAR = '(')
		PARAMETER (CLOPAR = ')')
		PARAMETER (DELIM  = QUOTE//SCOLON//COMMA//OPNPAR//CLOPAR)
	INTEGER*4	SCALAR_BIT, TOBY_BIT
		PARAMETER (SCALAR_BIT = 0)
		PARAMETER (TOBY_BIT   = 1)
C
	INTEGER*4	PPD_DTYPE_GET, PPD_NVAL_GET, PPD_AMAS_GET
	INTEGER*4	STR_SIGLEN, STR_SKIP_U
	INTEGER		MSG_SET, CLEAR_BLJ
	LOGICAL		WNGGVM
C
	CHARACTER*1	DTYPE
	INTEGER*4	LVAL, NRVPS, MNVPS, MXVPS, FLAGS
	INTEGER*4	IS, LSTR, PTR, DEPTH, NRSETS
C
C
C					Clear the block descriptor
C					- if blank value string: return
C
	IS = CLEAR_BLJ (VALBLK,8)
	IF (IAND(IS,1).EQ.0) GOTO 999
	LSTR = STR_SIGLEN (VALSTR)
	IF (LSTR.EQ.0) GOTO 900
C
C					Get info from PPD file
C
	IS = PPD_DTYPE_GET (DTYPE,LVAL)			! length of a value
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NRVPS,MNVPS,MXVPS)	! nr of values per set
	IF (IAND(IS,1).EQ.0) GOTO 999
	FLAGS = 0
	IF (NRVPS.EQ.1)					! scalar type
	1	FLAGS = IBSET (FLAGS,SCALAR_BIT)
	IF (DTYPE.NE.'C' .AND. DTYPE.NE.'L'
	1   .AND. IAND(PPD_AMAS_GET('VECTOR'),1).EQ.0)	! TOBY format
	1	FLAGS = IBSET (FLAGS,TOBY_BIT)
C
C					For scalar value string:
C					- skip through quoted substrings
C					- count nr of semicolons
C					- keep track of the subexpression
C					  depth, i.e. nr of '(' - nr of ')'
C					- skip through subexpressions
C					- convert commas to semicolons
C
	NRSETS = 1
	PTR = 1
	IF (BTEST(FLAGS,SCALAR_BIT)) THEN
		DEPTH = 0
		IS = STR_SKIP_U (DELIM,VALSTR(:LSTR),PTR)
		DO WHILE (PTR.LE.LSTR)
			IF (VALSTR(PTR:PTR).EQ.QUOTE) THEN
				PTR = PTR+1
				IS = STR_SKIP_U (QUOTE,VALSTR(:LSTR),PTR)
			ELSE IF (VALSTR(PTR:PTR).EQ.SCOLON) THEN
				NRSETS = NRSETS+1
			ELSE IF (VALSTR(PTR:PTR).EQ.OPNPAR) THEN
				DEPTH = DEPTH+1
			ELSE IF (VALSTR(PTR:PTR).EQ.CLOPAR) THEN
				DEPTH = DEPTH-1
			ELSE IF (DEPTH.EQ.0) THEN
				VALSTR(PTR:PTR) = SCOLON
				NRSETS = NRSETS+1
			ENDIF
			PTR = PTR+1
			IS = STR_SKIP_U (DELIM,VALSTR(:LSTR),PTR)
		ENDDO
C
C					For non-scalar value string:
C					- skip through quoted substrings
C					- count nr of semicolons
C
	ELSE
		IS = STR_SKIP_U (QUOTE//SCOLON,VALSTR(:LSTR),PTR)
		DO WHILE (PTR.LE.LSTR)
			IF (VALSTR(PTR:PTR).EQ.QUOTE) THEN
				PTR = PTR+1
				IS = STR_SKIP_U (QUOTE,VALSTR(:LSTR),PTR)
			ELSE
				NRSETS = NRSETS+1
			ENDIF
			PTR = PTR+1
			IS = STR_SKIP_U (QUOTE//SCOLON,VALSTR(:LSTR),PTR)
		ENDDO
	ENDIF
C
C					Allocate memory for the value block
C					and fill the descriptor array
C
	IS=DWC_SUCCESS
	IF (BTEST(FLAGS,TOBY_BIT)) NRVPS = 3*NRVPS	! res nr vals/set
	VALBLK(1) = NRSETS*(4+NRVPS*(LVAL+4))		! length value block
	IF (.NOT.WNGGVM(VALBLK(1),VALBLK(2))) THEN	! get VM address
	   IS=MSG_SET(DWC_PPDNOVIRT,1)
	   CALL WNCTXT(DWLOG,DWMSG,VALBLK(1))
	   GOTO 997
	END IF
	IF (IAND(IS,1).EQ.0) GOTO 998
	VALBLK(4) = VALBLK(2)+NRSETS*4			! address switch array
	VALBLK(3) = VALBLK(4)+NRSETS*NRVPS*4		! address value array
	VALBLK(5) = NRSETS				! nr sets
	VALBLK(6) = NRVPS
	VALBLK(7) = LVAL
	VALBLK(8) = FLAGS
C
C
 900	PV_BLK_ALLOC = DWC_SUCCESS
	RETURN
 998	PV_BLK_ALLOC = MSG_SET (IS,0)
 997	IS = CLEAR_BLJ (VALBLK,8)
	RETURN
 999	PV_BLK_ALLOC = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK_RELEASE (VALBLK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	VALBLK(8)	! (m) value block descriptor
C
C.Purpose:	Release the value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code from referenced routine
C.Notes:
C	- The value block descriptor will always be cleared.
C-------------------------------------------------------------------------
C
	INTEGER		CLEAR_BLJ, MSG_SET
	LOGICAL		WNGFVM
C
	INTEGER*4	IS
C
C
	IF (VALBLK(1).NE.0.AND.VALBLK(2).NE.0) THEN
		IF (.NOT.WNGFVM(VALBLK(1),VALBLK(2))) THEN
	 	   IS = DWC_PPDFRVIRT
C		type*,'Yeergh'
		   IS = MSG_SET (IS,0)
		END IF
	ENDIF
	IS = CLEAR_BLJ (VALBLK,8)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PV_BLK_RELEASE = DWC_SUCCESS
	RETURN
 999	PV_BLK_RELEASE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK_READ (VALBLK,SETNR,VALNR,COUNT,ARROUT,
	1					NROUT,SWDEF,STRING,LSTR)
C
	INCLUDE 'WNG_DEF'			! For indirect addressing
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	VALBLK(8)	! (i) value block descriptor
	INTEGER*4	SETNR		! (m) nr of current set
	INTEGER*4	VALNR		! (m) nr of value in current set
	INTEGER*4	COUNT		! (m) counter in set for TO/BY arrays
	BYTE		ARROUT(*)	! (o) next value set
	INTEGER*4	NROUT		! (o) nr of values in ARROUT
	LOGICAL*4	SWDEF		! (i) is ARRAY a default value ?
	CHARACTER*(*)	STRING		! (o) next value set as a string
	INTEGER*4	LSTR		! (o) length of value string
C
C.Purpose:	Get the next value set from a value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning		0	end of block is reached (always for zero block)
C	false status codes returned by referenced routines
C.Notes:
C	- The value set will be returned both as a string and as an array.
C	- The output string can be in TOBY format.
C	- The output array is in normal format (converted from TOBY when
C	  necessary) unless it concerns a default value set.
C	- NROUT can be 0 ("null" set) or -1 ("wild" set); in those cases
C	  the contents of ARROUT are undetermined.
C	- The routine will maintain the pointers SETNR, VALNR and COUNT.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
	INTEGER*4	SCALAR_BIT, TOBY_BIT
		PARAMETER (BLANK      = ' ')
		PARAMETER (SCALAR_BIT = 0)
		PARAMETER (TOBY_BIT   = 1)
C
	INTEGER*4	PV_SET_READ, PV_SET_ENCODE, PV_VAL_ENCODE
	INTEGER*4	PPD_DTYPE_GET
	INTEGER*4	MOVE_BLB, MOVE_BLJ
C
	CHARACTER*1	DTYPE
	INTEGER*4	IS, ANVPS, AVAL, PLEN, NR
	LOGICAL*4	TOBY
C
C
C
	STRING = BLANK
	LSTR = 0
	IF (VALBLK(2).EQ.0) GOTO 990
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					SCALAR
C					======
C
	IF (BTEST(VALBLK(8),SCALAR_BIT)) THEN
	    NROUT = UNDEF_J
	    DO WHILE (NROUT.EQ.UNDEF_J)
C
C					If there is a current set:
C					- special set: set exhausted
C					- else: extract the next value
C					- if found: ready
C					- else: set exhausted
C
		IF (SETNR.GT.0) THEN
		    ANVPS = VALBLK(2)+(SETNR-1)*4-A_OB
		    AVAL = VALBLK(3)+(SETNR-1)*VALBLK(6)*VALBLK(7)-A_OB
		    IS = MOVE_BLJ (A_B(ANVPS),NR,1)
		    IF (NR.GT.0) THEN
			IS = PV_SET_READ (DTYPE,VALBLK(7),A_B(ANVPS),
	1				A_B(AVAL),VALNR,COUNT,ARROUT)
			IF (IAND(IS,1).NE.0) THEN
			    NROUT = 1
			ELSE IF (IS.NE.0) THEN
			    GOTO 999
			ENDIF
		    ENDIF
		ENDIF
C
C					If first time or exhausted set:
C					- move to the next set
C					- if this is a special set: ready
C
		IF (NROUT.EQ.UNDEF_J) THEN
		    SETNR = SETNR+1
		    IF (SETNR.GT.VALBLK(5)) GOTO 990
		    VALNR = 0
		    ANVPS = VALBLK(2)+(SETNR-1)*4-A_OB
		    IS = MOVE_BLJ (A_B(ANVPS),NROUT,1)
		    IF (IAND(IS,1).EQ.0) GOTO 999
		    IF (NROUT.GT.0) NROUT = UNDEF_J
		ENDIF
	    ENDDO
C
C					Convert the value set to a string
C					- for normal sets: use VAL_ENCODE
C					  to suppress TOBY output format
C
	    IF (NROUT.EQ.1) THEN
		IS = PV_VAL_ENCODE (DTYPE,VALBLK(7),ARROUT,STRING,LSTR)
	    ELSE
		TOBY = BTEST (VALBLK(8),TOBY_BIT)
		IS = PV_SET_ENCODE (DTYPE,VALBLK(7),NROUT,ARROUT,TOBY,
	1						STRING,LSTR)
	    ENDIF
	    IF (IAND(IS,1).EQ.0) GOTO 999
C
C					ARRAY OR VECTOR
C					===============
C
	ELSE
C
C					Get the first or next value set
C					and convert it to a string
C
	    SETNR = SETNR+1
	    IF (SETNR.GT.VALBLK(5)) GOTO 990
	    VALNR = 0
	    ANVPS = VALBLK(2)+(SETNR-1)*4-A_OB
	    AVAL = VALBLK(3)+(SETNR-1)*VALBLK(6)*VALBLK(7)-A_OB
	    IS = MOVE_BLJ (A_B(ANVPS),NROUT,1)
	    TOBY = BTEST (VALBLK(8),TOBY_BIT)
	    IS = PV_SET_ENCODE (DTYPE,VALBLK(7),NROUT,A_B(AVAL),TOBY,
	1						STRING,LSTR)
	    IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If it is not a special value set:
C					- copy it to the output array (possibly
C					  after conversion from TOBY format)
C
	    IF (NROUT.GT.0) THEN
		IF (.NOT.BTEST(VALBLK(8),TOBY_BIT) .OR. SWDEF) THEN
		    IS = MOVE_BLB (A_B(AVAL),ARROUT,VALBLK(7)*NROUT)
		    IF (IAND(IS,1).EQ.0) GOTO 999
		ELSE
		    NROUT = 0
		    IS = 1
		    DO WHILE (IAND(IS,1).NE.0)
			IS = PV_SET_READ (DTYPE,VALBLK(7),A_B(ANVPS),
	1					A_B(AVAL),VALNR,COUNT,
	2					ARROUT(NROUT*VALBLK(7)+1))
			IF (IAND(IS,1).NE.0) NROUT = NROUT+1
		    ENDDO
		    IF (IS.NE.0) GOTO 999
		ENDIF
	    ENDIF
	ENDIF
C
	PV_BLK_READ = DWC_SUCCESS
	RETURN
 990	PV_BLK_READ = 0
	RETURN
 999	PV_BLK_READ = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK_ENCODE (VALBLK,STRING,LSTR)
C
	INCLUDE 'WNG_DEF'			! For indirect addressing
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	VALBLK(8)	! (i) value block descriptor
	CHARACTER*(*)	STRING		! (o) value string
	INTEGER*4	LSTR		! (o) significant length of STRING
C
C.Purpose:	Convert a value block to a standard value string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for empty block
C	warning	DWC_STRTOOSHO	output string has been truncated
C	false status codes returned by referenced routines
C.Notes:
C	- The value string is created according to DWCL syntax rules.
C	- The values in a set will be separated by comma's, and sets will be
C	  separated by semicolons.
C	- LSTR = 0 for an empty block.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, SETDELIM
	INTEGER*4	TOBY_BIT
		PARAMETER (BLANK    = ' ')
		PARAMETER (SETDELIM = ';')
		PARAMETER (TOBY_BIT = 1)
C
	INTEGER*4	PV_SET_ENCODE
	INTEGER*4	PPD_DTYPE_GET
	INTEGER		MSG_SET, STR_COPY
C
	CHARACTER*1	DTYPE
	INTEGER*4	IS, LADD, SETNR, ANVPS, AVAL, PLEN
	LOGICAL*4	TOBY
C
C
C					Convert set for set
C
	STRING = BLANK
	LSTR = 0
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	ANVPS = VALBLK(2)-A_OB
	AVAL = VALBLK(3)-A_OB
	TOBY = BTEST (VALBLK(8),TOBY_BIT)
	DO SETNR = 1,VALBLK(5)
		IS = PV_SET_ENCODE (DTYPE,VALBLK(7),A_B(ANVPS),
	1		A_B(AVAL),TOBY,STRING(LSTR+1:),LADD)
		IF (IAND(IS,1).EQ.0) GOTO 999
		LSTR = LSTR+LADD
		IF (SETNR.NE.VALBLK(5)) THEN
			IS = STR_COPY (SETDELIM,STRING,LSTR)
			IF (IS.LT.0) GOTO 990
		ENDIF
		ANVPS = ANVPS+LB_J
		AVAL = AVAL+VALBLK(6)*VALBLK(7)
	ENDDO
C
	PV_BLK_ENCODE = DWC_SUCCESS
	RETURN
 990	PV_BLK_ENCODE = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STRING))
	RETURN
 999	PV_BLK_ENCODE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_BLK_DECODE (STRING,VALBLK,STREAM,CHKSW,
	1					SWSYM,SWDV,DEFARR,NRDEF)
C
	INCLUDE 'WNG_DEF'			! For indirect addressing
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) value string
	INTEGER*4	VALBLK(8)	! (i) value block descriptor
	CHARACTER*(*)	STREAM		! (i) stream name (for substitution)
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (o) unknown symbols found ?
	LOGICAL*4	SWDV		! (i) default value string ?
	BYTE		DEFARR(*) 	! (i) default set
	INTEGER*4	NRDEF		! (i) nr of values in DEFARR
C
C.Purpose:	Convert a value string to a value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for empty string
C	error	DWC_CHKERRMSG	error in value checking
C	false status codes returned by referenced modules
C.Notes:
C	- VALBLK must have been created via PV_BLK_ALLOC (STRING,VALBLK).
C	- Value sets are delimited by semicolons (outside quoted substrings).
C	- Values within a set are delimited by commas.
C	- Numerical values can be given as expressions; these will be evaluated
C	  after substitution of any symbol names in the expression.
C	- Values that are not given by the user will be replaced by the
C	  corresponding default values (if present), unless the parameter
C	  has the UNDEFINED attribute. For scalars and arrays only explicitly
C	  undefined values will be defaulted; for vectors all undefined values
C	  will be defaulted. E.g. for a 5-valued parameter:
C		1,,2,	->	1,def,2,def		(array)
C		1,,2,	->	1,def,2,def,def		(vector)
C	- If the parameter is a node name, relative names (with dots and minus
C	  signs) will be expanded using the current node name. A trailing colon
C	  in the node specification is interpreted as a directive to set the
C	  current node. If it is given, all subsequent relative node names in
C	  the value block will be expanded using the new current node. DWARF's
C	  current node will not yet be changed.
C	- If no unknown symbols were found (SWSYM = .FALSE.), the values in
C	  the block will be checked against the PPD requirements.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, QUOTE, SETDELIM
	INTEGER*4	TOBY_BIT
		PARAMETER (BLANK = ' ')
		PARAMETER (QUOTE = '"')
		PARAMETER (SETDELIM = ';')
		PARAMETER (TOBY_BIT = 1)
C
	INTEGER*4	PV_SET_DECODE
	INTEGER*4	DWC_NODE_EXPAND_A
	INTEGER*4	PPD_DTYPE_GET, PPD_CMAS_GET, PPD_CHECK
	INTEGER*4	STR_SKIP_U
	INTEGER		MSG_SET
C
	CHARACTER	DTYPE*1
	INTEGER*4	IS, LSTR, PTR, START, ANVPS, AVAL, ASW, PLEN
	LOGICAL*4	TOBY
C
C
	LSTR = LEN (STRING)
	IF (LSTR.EQ.0) GOTO 900					! no value
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	ANVPS = VALBLK(2)-A_OB
	AVAL = VALBLK(3) -A_OB
	ASW = VALBLK(4)  -A_OB
	TOBY = BTEST (VALBLK(8),TOBY_BIT)
C
C					Decode set by set
C					- isolate the next set
C					- allow for a trailing set delimiter
C
	START = 1
	PTR = 1
	DO WHILE (PTR.LE.LSTR+1)
		IS = STR_SKIP_U (QUOTE//SETDELIM,STRING(:LSTR),PTR)
C
C					- start of quoted substring: skip
C					  through the whole substring
C
		IF (PTR.LE.LSTR .AND. STRING(PTR:PTR).EQ.QUOTE) THEN
			PTR = PTR+1
			IS = STR_SKIP_U (QUOTE,STRING(:LSTR),PTR)
			PTR = PTR+1
			IF (PTR.GT.LSTR+1) PTR = LSTR+1		! no end quote
C
C					- set complete: decode it, clear the
C					  buffer, and go for the next set
C
		ELSE
			IS = PV_SET_DECODE (STRING(START:PTR-1),
	1			DTYPE,VALBLK(7),VALBLK(6),A_B(AVAL),
	2			A_B(ANVPS),A_B(ASW),STREAM,CHKSW,SWSYM,
	3			SWDV,TOBY,DEFARR,NRDEF)
			IF (IAND(IS,1).EQ.0) GOTO 999
			PTR = PTR+1
			START = PTR
			ANVPS = ANVPS+LB_J
			AVAL = AVAL+VALBLK(6)*VALBLK(7)
			ASW = ASW+VALBLK(6)*LB_J
		ENDIF
	ENDDO
C
C					If node name: expand all names in block
C
	IF (IAND(PPD_CMAS_GET('NODE'),1) .NE. 0) THEN
		ANVPS = VALBLK(2)-A_OB
		AVAL = VALBLK(3) -A_OB
		ASW = VALBLK(4)  -A_OB		
		IS = DWC_NODE_EXPAND_A (VALBLK(5),VALBLK(6),VALBLK(7),
	1				A_B(AVAL),A_B(ANVPS))
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					Check the values
C
	IF (.NOT.SWSYM) THEN
		ANVPS = VALBLK(2)-A_OB
		AVAL = VALBLK(3) -A_OB
		ASW = VALBLK(4)  -A_OB		
		IS = PPD_CHECK (A_B(AVAL),A_B(ANVPS),VALBLK(6),
	1				A_B(ASW),VALBLK(5))
		IF (IAND(IS,1).EQ.0) GOTO 991
	ENDIF
C
 900	PV_BLK_DECODE = DWC_SUCCESS
	RETURN
 991	PV_BLK_DECODE = MSG_SET (DWC_CHKERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,BLANK,STRING)
	RETURN
 999	PV_BLK_DECODE = IS
	RETURN
	END

