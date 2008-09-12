C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_SKIP
C.Keywords:	String, Skip characters
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	880303 FMO - creation
C.Version:	910805 FMO - add STR_SKIP_PAST
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION STR_SKIP_U (MATCH,STRING,PTR)
C	INTEGER ENTRY    STR_SKIP_W (MATCH,STRING,PTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER		STR_SKIP_W
C
	CHARACTER*(*)	MATCH		! (i) match characters
	CHARACTER*(*)	STRING		! (i) string
	INTEGER		PTR		! (m) current position in string
C
C.Purpose:	Skip through a string until/while the character matches
C.Returns:	Number of characters skipped (0 if none)
C.Notes:
C	- Skipping also stops at the end of the string. Conceptually, the
C	  string is preceded and followed by an infinite number of non-skip
C	  characters:
C	- The end of the string corresponds to PTR=LEN(STRING)+1).
C	- If at the start PTR < 1 or PTR > LEN(STRING), the function just
C	  returns a function value 0 and leaves PTR as it is.
C-------------------------------------------------------------------------
C
	INTEGER		COUNT
C
C Skip until
C
	COUNT = 0
	IF (PTR.GT.0 .AND. PTR.LE.LEN(STRING)) THEN
		DO PTR = PTR,LEN(STRING)
			IF (INDEX(MATCH,STRING(PTR:PTR)).NE.0) GOTO 100
			COUNT = COUNT+1
		END DO
 100		CONTINUE
	ENDIF
	STR_SKIP_U = COUNT
	RETURN
C
C
C Skip while
C
	ENTRY STR_SKIP_W (MATCH,STRING,PTR)
C
	COUNT = 0
	IF (PTR.GT.0 .AND. PTR.LE.LEN(STRING)) THEN
		DO PTR = PTR,LEN(STRING)
			IF (INDEX(MATCH,STRING(PTR:PTR)).EQ.0) GOTO 200
			COUNT = COUNT+1
		END DO
 200		CONTINUE
	ENDIF
	STR_SKIP_W = COUNT
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION STR_SKIP_PAST (SUBSTR,STRING,PTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SUBSTR		! (i) substring
	CHARACTER*(*)	STRING		! (i) string
	INTEGER		PTR		! (m) current position in string
C
C.Purpose:	Skip past a substring in a string
C.Returns:	Start position of the substring in the string (or zero)
C.Notes:
C	- If PTR > LEN(STRING) or if the substring is not present in
C	  STRING(PTR:), PTR will be left unchanged and a zero function value
C	  will be returned.
C	- Otherwise, PTR will be set to the first position after the substring
C	  and the start position of the substring will be returned.
C	- If the substring is empty, PTR will be left unchanged and will be
C	  returned as the value value.
C-------------------------------------------------------------------------
C
C
C
	PTR = MAX(PTR,1)
	IF (PTR.GT.LEN(STRING)) THEN
		STR_SKIP_PAST = 0
	ELSE IF (LEN(SUBSTR).EQ.0) THEN
		STR_SKIP_PAST = PTR
	ELSE
		I = INDEX (STRING(PTR:),SUBSTR)
		IF (I.EQ.0) THEN
			STR_SKIP_PAST = 0
		ELSE
			STR_SKIP_PAST = PTR+I-1
			PTR = PTR+I-1+LEN(SUBSTR)
		END IF
	END IF
	RETURN
	END
