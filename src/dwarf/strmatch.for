C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_MATCH
C.Keywords:	String, Match
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	880313 FMO - creation
C.Version:	881017 FMO - ignore leading blanks and tabs in all strings
C.Version:	881228 FMO - allow empty array
C.Version:	890612 FMO - ignore leading/trailing CR and LF 's
C.Version:	910805 FMO - allow wilcards (*) in STRING, add STR_MATCH
C		941019 JPH - COMMA --> COMMA//';|' for oprions formatting
C		941212 JPH - Add '/[]' delimiters; COMMA --> DELIM
C		010709 AXC - linux port - Parameter
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION STR_MATCH (STR1,STR2)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STR1		!(i) string to be matched
	CHARACTER*(*)	STR2		!(i) string to be matched with
C
C.Purpose:	Check whether two strings match
C.Returns:	Status code (.TRUE. if unique match, .FALSE. if not)
C	success	1	full match (with or without wildcards in STR2)
C	info	3	abbreviated match (only if no wildcards in STR2)
C	error	2	no match
C.Notes:
C	- STR1 cannot contain wildcards (*). If STR2 contains wildcards, only
C	  a complete match will be recognised: i.e. all non-wild substrings of
C	  STR2 must be present in STR1 and in the right order, and all the
C	  remaining substrings in STR1 must correspond with a wildcard in STR2.
C	- Trailing blanks and tabs are ignored in both strings. An empty string
C	  never matches.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	WILD
		PARAMETER (WILD = '*')
C
	INTEGER		STR_SIGLEN, STR_SKIP_U, STR_SKIP_W, STR_SKIP_PAST
C
	INTEGER		IS, LL1, L2, LS, P1, P2, S1, S2
	LOGICAL		WILD_END
C
C
	LL1 = STR_SIGLEN (STR1)				!length of STR1
	IF (LL1.EQ.0) GOTO 999				!zero: no match
	L2 = STR_SIGLEN (STR2)				!length of STR2
	IF (L2.EQ.0) GOTO 999				!zero: no match
C
C					If no wildcard in STR2:
C
	IF (INDEX(STR2(:L2),WILD).EQ.0) THEN
	    IF (LL1.GT.L2) GOTO 999			!no match
	    IF (STR1(:LL1).NE.STR2(:LL1)) GOTO 999
	    IF (LL1.EQ.L2) THEN
		STR_MATCH = 1				!full match
	    ELSE
		STR_MATCH = 3				!abbreviated match
	    END IF
C
C					Otherwise:
C					- check start of STR2 for wildcards
C
	ELSE
	    P1 = 1					!pointer in STR1
	    P2 = 1					!pointer in STR2
	    IS = STR_SKIP_W (WILD,STR2(:L2),P2)		!skip leading wildcards
	    IF (P2.GT.L2) THEN				!completely wild STR2
		STR_MATCH = 1				! STR1 always matches
		RETURN
	    END IF
C
C					- check next non-wild substring
C
	    DO WHILE (P2.LE.L2)
		S2 = P2					!start of subs in STR2
		LS = STR_SKIP_U (WILD,STR2(:L2),P2)	!extract subs from STR2
		S1 = STR_SKIP_PAST (STR2(S2:P2-1),STR1(:LL1),P1)!find it in STR1
		IF (S2.EQ.1) THEN			!non-wild STR2 start
		    IF (S1.NE.1) GOTO 999		! not equal: no match
		ELSE					!internal non-wild subs
		    IF (S1.EQ.0) GOTO 999		! no match in STR1
		END IF
		IF (P2.LE.L2) THEN			!wildcard in STR2
		    IS = STR_SKIP_W (WILD,STR2(:L2),P2)	! skip wildcards
		    WILD_END = P2.GT.L2			! wild end of STR2 ?
		END IF
	    END DO
	    IF (.NOT.WILD_END .AND.
	1       STR1(LL1-LS+1:LL1).NE.STR2(S2:S2+LS-1)) goto 999
	    STR_MATCH = 1
	END IF
C
	RETURN
C
 999	STR_MATCH = 2
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION STR_MATCH_A (STRING,NARR,ARRAY,MATCHNR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) string to be matched
	INTEGER		NARR		! (i) number of match strings
	CHARACTER*(*)	ARRAY(*)	! (i) array with match strings
	INTEGER		MATCHNR		! (o) nr of matching string
C
C.Purpose:	Find the character-array element that matches a string
C.Returns:	Status code (.TRUE. if unique match, .FALSE. if not)
C	success	1	full match (with or without wildcards in ARRAY)
C	info	3	unique abbreviated match (no wildcards in ARRAY)
C	warning	0	more than one abbreviated match (no wildcards in ARRAY)
C	error	2	no match (MATCHNR = 0)
C.Notes:
C	- STRING cannot contain wildcards (*), array elements can.
C	- Abbreviated matches are only recognised if ARRAY does not contain
C	  wildcards.
C	- In the case of multiple abbreviated or wildcard matches, MATCHNR will
C	  contain the nr of the first match.
C	- Leading and trailing blanks, tabs, LF's and CR's are ignored in all
C	  strings. An empty string never matches.
C-------------------------------------------------------------------------
C
	INTEGER		STR_SIGLEN, STR_SKIP_W, STR_MATCH
C
	CHARACTER*1	BLANK, TAB, LF, CR, WILD
	CHARACTER*4	WHITE
		PARAMETER (BLANK = ' ')
		PARAMETER (WILD = '*')
C
	INTEGER		LL1, P1, P2
	INTEGER		STATUS, NSKIP, IS
C
C
	TAB   = CHAR(9)
	LF    = CHAR(10)
	CR    = CHAR(13)
	WHITE = BLANK//TAB//LF//CR
C
	MATCHNR = 0					!no match
	STATUS = 2
C
C					Ignore leading and trailing whites
C
	P1 = 1						!start of string
	LL1 = STR_SIGLEN (STRING)			!end of string
	NSKIP = STR_SKIP_W (WHITE,STRING(:LL1),P1)	!skip leading whites
	IF (P1.GT.LL1) GOTO 900				!empty: no match
C
C					Check whether ARRAY contains wildcards
C
	DO I = 1,NARR
		IF (INDEX(ARRAY(I),WILD).NE.0) GOTO 100
	END DO
C
C					If ARRAY does not contain wildcards:
C					- search through array until a full
C					  match is found, or until the end
C
	DO I = 1,NARR
		P2 = 1					!start of element
		NSKIP = STR_SKIP_W (WHITE,ARRAY(I),P2)	!skip leading whites
		IS = STR_MATCH (STRING(P1:LL1),ARRAY(I)(P2:)) !check match
		IF (IS.EQ.2) THEN			!no match:
			CONTINUE			! continue
		ELSE IF (IS.EQ.1) THEN			!full match:
			STATUS = 1			! set "full" status
			MATCHNR = I			! set matchnr
			GOTO 900			! break
		ELSE IF (MATCHNR.EQ.0) THEN		!first short match:
			STATUS = 3			! set "abbrev" status
			MATCHNR = I			! set matchnr
		ELSE					!duplicate short match:
			STATUS = 0			! set "duplic" status
CCC			MATCHNR = MATCHNR		! keep first matchnr
		END IF
	END DO
	GOTO 900
C
C					If ARRAY does contain wildcards:
C					- search through array until a
C					  match is found, or until the end
C
 100	DO I = 1,NARR
		P2 = 1					!start of element
		NSKIP = STR_SKIP_W (WHITE,ARRAY(I),P2)	!skip leading whites
		IS = STR_MATCH (STRING(P1:LL1),ARRAY(I)(P2:)) !check match
		IF (IS.EQ.1) THEN			!match:
			STATUS = 1			! set "full" status
			MATCHNR = I			! set matchnr
			GOTO 900			! break
		END IF
	END DO
C
 900	STR_MATCH_A = STATUS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION STR_MATCH_L (STRING,LIST,MATCHNR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) string to be matched
	CHARACTER*(*)	LIST		! (i) list with match strings
	INTEGER		MATCHNR		! (o) nr of matching string
C
C.Purpose:	Find the field in a comma-separated list that matches a string
C.Returns:	Status code (.TRUE. if unique match, .FALSE. if not)
C	success	1	full match (with or without wildcards in ARRAY)
C	info	3	unique abbreviated match (no wildcards in ARRAY)
C	warning	0	more than one abbreviated match (no wildcards in ARRAY)
C	error	2	no match (MATCHNR = 0)
C.Notes:
C	- STRING cannot contain wildcards (*), list elements can.
C	- Abbreviated matches are only recognised if LIST does not contain
C	  wildcards.
C	- In the case of multiple abbreviated or wildcard matches, MATCHNR will
C	  contain the nr of the first match.
C	- Leading and trailing blanks, tabs, LF's and CR's are ignored in all
C	  strings. An empty string never matches.
C-------------------------------------------------------------------------
C
	INTEGER		STR_SIGLEN, STR_SKIP_W, STR_SKIP_U, STR_MATCH
C
	CHARACTER*(*)	BLANK, COMMA, WILD, DELIM
	CHARACTER	TAB*1, LF*1, CR*1, WHITE*4
		PARAMETER (BLANK = ' ')
		PARAMETER (COMMA = ',')
		PARAMETER (WILD  = '*')
		PARAMETER (DELIM = ',;|[]:' )
C
	INTEGER		LL, LL1, P1, P2
	INTEGER		STATUS, IS, PTR, NSKIP
C
C
	TAB   = CHAR(9)
	LF    = CHAR(10)
	CR    = CHAR(13)
	WHITE = BLANK//TAB//LF//CR
C
	MATCHNR = 0					!no match
	STATUS = 2
C
C					Ignore leading and trailing whites
C
	P1 = 1						!start of string
	LL1 = STR_SIGLEN (STRING)			!end of string
	NSKIP = STR_SKIP_W (WHITE,STRING(:LL1),P1)	!skip leading whites
	IF (P1.GT.LL1) GOTO 900				!empty: no match
	LL = STR_SIGLEN (LIST)				!length of list
C
C					Check whether LIST contains wildcards
C
	IF (INDEX(LIST(:LL),WILD).NE.0) GOTO 100
C
C					If LIST does not contain wildcards:
C					- search through list until a full
C					  match is found, or until the end
C
	I = 1						!element nr
	PTR = 1						!list pointer
	DO WHILE (PTR.LE.LL)
	    NSKIP = STR_SKIP_W (WHITE,LIST(:LL),PTR)	!skip leading whites
	    P2 = PTR					!start of element
!!	    NSKIP = STR_SKIP_U (COMMA//';|/[]',LIST(:LL),PTR)
							!find end of element
	    NSKIP = STR_SKIP_U (DELIM,LIST(:LL),PTR)	!find end of element
	    IF (PTR.GT.P2) THEN				!non-empty element
		IS = STR_MATCH (STRING(P1:LL1),LIST(P2:PTR-1)) !check match
		IF (IS.EQ.2) THEN			!no match:
			CONTINUE			! continue
		ELSE IF (IS.EQ.1) THEN			!full match:
			STATUS = 1			! set "full" status
			MATCHNR = I			! set matchnr
			GOTO 900			! break
		ELSE IF (MATCHNR.EQ.0) THEN		!first short match:
			STATUS = 3			! set "abbrev" status
			MATCHNR = I			! set matchnr
		ELSE					!duplicate short match:
			STATUS = 0			! set "duplic" status
CCC			MATCHNR = MATCHNR		! keep first matchnr
		END IF
	    END IF
	    I = I+1					!increment element nr
	    PTR = PTR+1 				!skip list separator
	END DO
	GOTO 900
C
C					If LISTdoes contain wildcards:
C					- search through LIST until a
C					  match is found, or until the end
C
 100	I = 1						!element nr
	PTR = 1						!list pointer
	DO WHILE (PTR.LE.LL)
	    NSKIP = STR_SKIP_W (WHITE,LIST(:LL),PTR)	!skip leading whites
	    P2 = PTR					!start of element
	    NSKIP = STR_SKIP_U (DELIM,LIST(:LL),PTR)	!find end of element
!!	    NSKIP = STR_SKIP_U (COMMA,LIST(:LL),PTR)	!find end of element
	    IF (PTR.GT.P2) THEN				!non-empty element
		IS = STR_MATCH (STRING(P1:LL1),LIST(P2:PTR-1)) !check match
		IF (IS.EQ.1) THEN			!match:
			STATUS = 1			! set "full" status
			MATCHNR = I			! set matchnr
			GOTO 900			! break
		END IF
	    END IF
	    I = I+1					!increment element nr
	    PTR = PTR+1 				!skip list separator
	END DO
C
 900	STR_MATCH_L = STATUS
	RETURN
	END
