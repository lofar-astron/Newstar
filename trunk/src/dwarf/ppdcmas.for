C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_CMAS
C.Keywords:	PPD File, Parameter Checks
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPDPD$CMAS	! (m) check mask
C
C.Version:	900415 FMO - recreation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CMAS_PUT (LIST,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LIST		! (i) list of proposed checks
	LOGICAL*4	DO_CHECK	! (i) check internal consistency ?
C
C.Purpose:	Check and store the checks for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_CHATINV	invalid check name
C	error	PPD_CHATNUNI	ambiguously abbreviated check name
C	error	PPD_MUTEXCLCH	invalid combination of checks
C.Notes:
C	- The list should be a comma-separated list of valid check names.
C	  It will be converted to a bitmask and stored in the current
C	  parameter description (field PPDPD$CMAS).
C	- If an invalid check name or conflicting checks are given,
C	  the default mask (no checks at all) will be stored.
C	- If DO_CHECK is off, the function just returns with PPD_SUCCESS.
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
		PARAMETER (MAXNR   = 12)
		PARAMETER (DEFMASK = 0 )
	CHARACTER*16	NAMES(MAXNR)
		DATA NAMES /
	1		'MINIMUM',	'MAXIMUM',
	2		'ASCENDING',	'DESCENDING',
	3		'ALPHABETIC',	'NUMERIC',
	4		'ANUMERIC',	'OPTIONS',
	5		'ABBREV_OPTIONS','NODE',
	6		'NON_ASCENDING'	,'NON_DESCENDING' /
C
	INTEGER*4	STR_SIGLEN, STR_MATCH_A, STR_COPY_U
C
	CHARACTER*16	NAME
	INTEGER*4	IS, LL, LN, PTR, NR, MASK
C
C
C					Check and convert name by name
C					- extract the next name
C					- check it
C					- set the corresponding bit in the mask
C
	IF (DO_CHECK) THEN
		MASK = DEFMASK
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
					GOTO 999
				ENDIF
				MASK = IBSET (MASK,NR-1)
				PTR = PTR+1
			ENDDO
		ENDIF
C
C					Check internal consistency
C
		IF ((BTEST(MASK,7) .AND. BTEST(MASK,8)) .OR.
	1	    (BTEST(MASK,4) .AND. BTEST(MASK,5)) .OR.
	2	    (BTEST(MASK,4) .AND. BTEST(MASK,6)) .OR.
	3	    (BTEST(MASK,5) .AND. BTEST(MASK,6)) .OR.
	4	    (BTEST(MASK,2) .AND. BTEST(MASK,3)) .OR.
	5	    (BTEST(MASK,2) .AND. BTEST(MASK,10)) .OR.
	6	    (BTEST(MASK,2) .AND. BTEST(MASK,11)) .OR.
	7	    (BTEST(MASK,3) .AND. BTEST(MASK,10)) .OR.
	8	    (BTEST(MASK,3) .AND. BTEST(MASK,11)) .OR.
	9	    (BTEST(MASK,10) .AND. BTEST(MASK,11))) THEN
			IS = PPD_MUTEXCLCH
			GOTO 999
		ENDIF
		PPDPD$CMAS = MASK
	ENDIF
C
	PPD_CMAS_PUT = PPD_SUCCESS
	RETURN
C
 999	PPDPD$CMAS = DEFMASK
	PPD_CMAS_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_CMAS_GET (NAME)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (i) (abbrev) name of check
C
C.Purpose:	Check whether the named check must be performed
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C.Notes:
C	A false status code is also returned if the check name is not correct.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNR
		PARAMETER (MAXNR   = 12)
	CHARACTER*16	NAMES(MAXNR)
		DATA NAMES /
	1		'MINIMUM',	'MAXIMUM',
	2		'ASCENDING',	'DESCENDING',
	3		'ALPHABETIC',	'NUMERIC',
	4		'ANUMERIC',	'OPTIONS',
	5		'ABBREV_OPTIONS','NODE',
	6		'NON_ASCENDING'	,'NON_DESCENDING' /
C
	INTEGER*4	STR_MATCH_A
C
	INTEGER*4	IS, NR
C
C
	IS = STR_MATCH_A (NAME,MAXNR,NAMES,NR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (.NOT.BTEST (PPDPD$CMAS,NR-1)) IS = 0
C
 999	PPD_CMAS_GET = IS
	RETURN
	END
