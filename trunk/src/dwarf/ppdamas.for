C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_AMAS
C.Keywords:	PPD File, Parameter Attributes
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPDPD$AMAS	! (m) attribute mask
C
C.Version:	900415 FMO - recreation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_AMAS_PUT (LIST,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LIST		! (i) list of proposed attributes
	LOGICAL*4	DO_CHECK	! (i) check internal consistency ?
C
C.Purpose:	Check and store the attributes for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_CHATINV	invalid attribute name
C	error	PPD_CHATNUNI	ambiguously abbreviated attribute name
C	error	PPD_UNDONLVEC	undefined values only allowed for vectors
C	error	PPD_NNDNOTNOD	null-node check invalid (no node attribute)
C.Notes:
C	- The list should be a comma-separated list of valid attribute names.
C	  It will be converted to a bitmask and stored in the current
C	  parameter description (field PPDPD$AMAS).
C	- If an invalid attribute name or conflicting attributes are given,
C	  the default mask (no attributes at all) will be stored.
C	- If DO_CHECK is off, only the compatibility with the requested checks
C	  will be checked.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	COMMA, EMPTVAL
		PARAMETER (COMMA   = ',' )
		PARAMETER (EMPTVAL = '[]')
C
	INTEGER*4	MAXNR, DEFMASK
		PARAMETER (MAXNR   = 11)
		PARAMETER (DEFMASK = 0 )
	CHARACTER*16	NAMES(MAXNR)
		DATA NAMES /
	1		'LOOP',		'VECTOR',
	2		'WILD_CARDS',	'IMMEDIATE',
	3		'ASK',		'UNDEFINED_VALUES',
	4		'TEST',		'PUT_GLOBAL',
	5		'DYNAMIC',	'NULL_VALUES',
	6		'NULL_NODE' /
C
	INTEGER*4	PPD_CMAS_GET
	INTEGER*4	STR_SIGLEN, STR_MATCH_A, STR_COPY_U
C
	CHARACTER*16	NAME
	INTEGER*4	IS, LL, LN, PTR, NR
C
C
C					Check and convert name by name
C					- extract the next name
C					- check it
C					- set the corresponding bit in the mask
C
	IF (DO_CHECK) THEN
		PPDPD$AMAS = DEFMASK
		LL = STR_SIGLEN (LIST)
		IF (LL.GT.0 .AND. LIST(:LL).NE.EMPTVAL) THEN
			PTR = 1
			DO WHILE (PTR.LE.LL)
				LN = 0
				IS = STR_COPY_U (COMMA,LIST(:LL),PTR,NAME,LN)
				IS = STR_MATCH_A (NAME(:LN),MAXNR,NAMES,NR)
				IF (IAND(IS,1).EQ.0) THEN
					IF (IS.EQ.0) THEN
						IS = PPD_CHATNUNI
					ELSE
						IS = PPD_CHATINV
					ENDIF
					PPDPD$AMAS = DEFMASK
					GOTO 999
				ENDIF
				PPDPD$AMAS = IBSET (PPDPD$AMAS,NR-1)
				PTR = PTR+1
			ENDDO
		ENDIF
C
C					Check internal consistency
C					- 'UNDEFINED_VALUES' only if 'VECTOR'
C
		IF (BTEST(PPDPD$AMAS,5) .AND. .NOT.BTEST(PPDPD$AMAS,1)) THEN
			IS = PPD_UNDONLVEC
			PPDPD$AMAS = DEFMASK
			GOTO 999
		ENDIF
	ENDIF
C
C					Check consistency with checks
C					- 'NULL_NODE' only if 'NODE' check
C
	IF (BTEST(PPDPD$AMAS,10)
	1 .AND. IAND(PPD_CMAS_GET('NODE'),1).EQ.0) THEN
		IS = PPD_NNDNOTNOD
		GOTO 999
	ENDIF
C
C
	PPD_AMAS_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_AMAS_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_AMAS_GET (NAME)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (i) (abbrev) name of attribute
C
C.Purpose:	Check whether the named attribute is present
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C.Notes:
C	A false status code is also returned if the attribute name is not
C	correct.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNR
		PARAMETER (MAXNR   = 11)
	CHARACTER*16	NAMES(MAXNR)
		DATA NAMES /
	1		'LOOP',		'VECTOR',
	2		'WILD_CARDS',	'IMMEDIATE',
	3		'ASK',		'UNDEFINED_VALUES',
	4		'TEST',		'PUT_GLOBAL',
	5		'DYNAMIC',	'NULL_VALUES',
	6		'NULL_NODE' /
C
	INTEGER*4	STR_MATCH_A
C
	INTEGER*4	IS, NR
C
C
	IS = STR_MATCH_A (NAME,MAXNR,NAMES,NR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (.NOT.BTEST (PPDPD$AMAS,NR-1)) IS = 0
C
 999	PPD_AMAS_GET = IS
	RETURN
	END
